use gimli::{AttributeValue, DebuggingInformationEntry, UnitOffset, constants};

use crate::dwarf::{DwarfError, Slice, UnitRef};
use crate::{
    MemberOffset, PointerType, PrimitiveType, PrimitiveTypeEncoding, StructureMember,
    StructureType, Type, TypeDefType, TypeSize, ValueSize,
};

pub fn parse_type<'d>(
    unit: UnitRef<'d, '_>,
    offset: UnitOffset,
) -> Result<Option<Type<'d>>, DwarfError> {
    let entry = unit.unit.entry(offset)?;

    Ok(match entry.tag {
        constants::DW_TAG_base_type => parse_base_type(unit, entry).map(Type::Primitive),
        // Pointers may be nested within `DW_TAG_restrict_type`, `DW_TAG_const_type` etc.
        constants::DW_TAG_pointer_type
        | constants::DW_TAG_reference_type
        | constants::DW_TAG_rvalue_reference_type => {
            parse_pointer_type(unit, entry).map(Type::Pointer)
        }
        constants::DW_TAG_typedef => parse_typedef(unit, entry).map(Type::TypeDef),
        constants::DW_TAG_structure_type => {
            parse_structure_type(unit, entry)?.map(Type::StructureType)
        }
        _ => None,
    })
}

fn parse_base_type<'d>(
    unit: UnitRef<'d, '_>,
    entry: DebuggingInformationEntry<Slice<'d>>,
) -> Option<PrimitiveType<'d>> {
    let mut name = None;
    let mut encoding = None;
    let mut byte_size = None;
    let mut bit_size = None;
    let mut data_bit_offset = None;

    for attr in entry.attrs {
        match attr.name() {
            constants::DW_AT_name => {
                // May also need to check `DW_AT_specification` and `DW_AT_abstract_origin` for the name.
                name = unit.string_value(attr.value());
            }
            constants::DW_AT_byte_size => byte_size = attr.udata_value(),
            constants::DW_AT_bit_size => bit_size = attr.udata_value(),
            constants::DW_AT_data_bit_offset => data_bit_offset = attr.udata_value(),
            constants::DW_AT_encoding => {
                use AttributeValue::Encoding;
                encoding = Some(match attr.value() {
                    Encoding(constants::DW_ATE_boolean) => PrimitiveTypeEncoding::Boolean,
                    Encoding(constants::DW_ATE_address) => PrimitiveTypeEncoding::Address,
                    Encoding(constants::DW_ATE_signed) => PrimitiveTypeEncoding::SignedInt,
                    Encoding(constants::DW_ATE_unsigned) => PrimitiveTypeEncoding::UnsignedInt,
                    Encoding(constants::DW_ATE_signed_char) => PrimitiveTypeEncoding::SignedChar,
                    Encoding(constants::DW_ATE_unsigned_char) => {
                        PrimitiveTypeEncoding::UnsignedChar
                    }
                    Encoding(constants::DW_ATE_float) => PrimitiveTypeEncoding::Float,
                    Encoding(constants::DW_ATE_complex_float) => {
                        PrimitiveTypeEncoding::ComplexFloat
                    }
                    _ => continue,
                });
            }
            _ => {}
        }
    }

    let (size, value_size) = match (byte_size, bit_size) {
        (Some(byte_size), Some(bit_size)) => (
            TypeSize::Bytes(byte_size),
            Some(ValueSize {
                size: bit_size,
                offset: data_bit_offset.unwrap_or(0),
            }),
        ),
        (Some(byte_size), None) => (TypeSize::Bytes(byte_size), None),
        (None, Some(bit_size)) => (TypeSize::Bits(bit_size), None),
        (None, None) => return None,
    };

    Some(PrimitiveType {
        name,
        encoding,
        size,
        value_size,
    })
}

fn parse_pointer_type<'d>(
    unit: UnitRef<'d, '_>,
    entry: DebuggingInformationEntry<Slice<'d>>,
) -> Option<PointerType> {
    let mut pointee = None;
    let mut size = None;

    for attr in entry.attrs {
        match attr.name() {
            // Return `None` if `DW_AT_type` is present but cannot be resolved, rather than
            // treating the pointer as `void*`.
            constants::DW_AT_type => pointee = Some(unit.to_type_ref(attr)?),
            constants::DW_AT_byte_size => {
                size = attr.udata_value().map(TypeSize::Bytes);
            }
            constants::DW_AT_bit_size => {
                size = attr.udata_value().map(TypeSize::Bits);
            }
            _ => {}
        }
    }

    Some(PointerType {
        pointee: pointee.map(Into::into),
        size: size.unwrap_or_else(|| TypeSize::Bytes(unit.unit.encoding().address_size.into())),
    })
}

fn parse_typedef<'d>(
    unit: UnitRef<'d, '_>,
    entry: DebuggingInformationEntry<Slice<'d>>,
) -> Option<TypeDefType<'d>> {
    let mut name = None;
    let mut ty = None;

    for attr in entry.attrs {
        match attr.name() {
            constants::DW_AT_name => {
                name = unit.string_value(attr.value());
            }
            constants::DW_AT_type => ty = Some(unit.to_type_ref(attr)?),
            _ => {}
        }
    }

    Some(TypeDefType {
        name: name?,
        ty: ty.map(Into::into),
    })
}

fn parse_structure_type<'d>(
    unit: UnitRef<'d, '_>,
    entry: DebuggingInformationEntry<Slice<'d>>,
) -> Result<Option<StructureType<'d>>, DwarfError> {
    let offset = entry.offset();

    let mut name = None;
    let mut size = None;
    let mut members = Vec::new();

    for attr in entry.attrs {
        match attr.name() {
            constants::DW_AT_name => {
                name = unit.string_value(attr.value());
            }
            constants::DW_AT_byte_size => {
                size = attr.udata_value().map(TypeSize::Bytes);
            }
            constants::DW_AT_bit_size => {
                size = attr.udata_value().map(TypeSize::Bits);
            }
            _ => {}
        }
    }

    let mut tree = unit.unit.entries_tree(Some(offset))?;
    let root = tree.root()?;
    let mut children = root.children();

    while let Some(child) = children.next()? {
        let member = child.entry();
        if member.tag != constants::DW_TAG_member {
            continue;
        }

        let mut name = None;
        let mut ty = None;
        let mut offset = None;

        for &attr in &member.attrs {
            match attr.name() {
                constants::DW_AT_name => {
                    name = unit.string_value(attr.value());
                }
                constants::DW_AT_type => {
                    ty = unit.to_type_ref(attr).map(Into::into);
                }
                constants::DW_AT_data_member_location => {
                    offset = Some(attr.udata_value().map(MemberOffset::Bytes));
                }
                constants::DW_AT_data_bit_offset => {
                    offset = Some(attr.udata_value().map(MemberOffset::Bits));
                }
                _ => {}
            }
        }

        let offset = match offset {
            Some(Some(offset)) => offset,
            // DWARF 5 §5.7.6 - The offset attributes may be missing if the member is at the beginning.
            None => MemberOffset::Bytes(0),
            // A valid member must also have a valid offset.
            Some(None) => continue,
        };

        // A valid member must always have a type.
        let Some(ty) = ty else {
            continue;
        };

        members.push(StructureMember { name, ty, offset });
    }

    Ok(Some(StructureType {
        name: name,
        size,
        members,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SymbolMap;
    use crate::dwarf::{DwarfInfo, DwarfUnitTable, Endian};
    use crate::{ObjectKind, Type};
    use gimli::read::{DebugAbbrev, DebugInfo, Dwarf};
    use symbolic_common::CpuFamily;

    #[test]
    fn test_type_sizes() {
        let debug_abbrev = [
            1, 0x11, 0, 0, 0, 2, 0x24, 0, 0x0b, 0x0f, 0, 0, 3, 0x24, 0, 0x0d, 0x0f, 0, 0, 4, 0x24,
            0, 0x0b, 0x0f, 0x0d, 0x0f, 0x6b, 0x0f, 0, 0, 5, 0x24, 0, 0x0b, 0x0f, 0x0d, 0x0f, 0, 0,
            6, 0x13, 0, 0x0b, 0x0f, 0, 0, 7, 0x13, 0, 0x0d, 0x0f, 0, 0, 8, 0x0f, 0, 0, 0, 9, 0x0f,
            0, 0x0b, 0x0f, 0, 0, 0,
        ];
        let types = [2, 4, 3, 39, 4, 4, 1, 7, 5, 4, 1, 6, 8, 7, 32, 8, 9, 4, 0];
        let mut debug_info = (types.len() as u32 + 9).to_le_bytes().to_vec();
        debug_info.extend_from_slice(&[5, 0, 1, 8, 0, 0, 0, 0]);
        debug_info.extend_from_slice(&[1]);
        debug_info.extend_from_slice(&types);

        let debug_abbrev = DebugAbbrev::new(&debug_abbrev, Endian::Little);
        let debug_info = DebugInfo::new(&debug_info, Endian::Little);
        let mut dwarf =
            Dwarf::load(|_| Ok::<_, gimli::Error>(Slice::new(&[], Endian::Little))).unwrap();
        dwarf.debug_abbrev = debug_abbrev;
        dwarf.debug_info = debug_info;
        let dwarf = DwarfInfo {
            info: DwarfUnitTable::from_iter(dwarf.units()).unwrap(),
            types: DwarfUnitTable::from_iter(dwarf.type_units()).unwrap(),
            inner: dwarf,
            symbol_map: SymbolMap::new(),
            address_offset: 0,
            kind: ObjectKind::Debug,
            cpu_family: CpuFamily::Amd64,
        };
        let unit = dwarf.info.get_unit(0, &dwarf.inner).unwrap().unwrap();
        let unit = UnitRef { info: &dwarf, unit };
        let mut entries = unit.unit.entries();
        entries.next_dfs().unwrap();
        let mut offsets = Vec::new();
        while let Some(entry) = entries.next_dfs().unwrap() {
            offsets.push(entry.offset());
        }

        let actual =
            offsets
                .into_iter()
                .map(|offset| match parse_type(unit, offset).unwrap().unwrap() {
                    Type::Primitive(ty) => (ty.size, ty.value_size),
                    Type::StructureType(ty) => (ty.size.unwrap(), None),
                    Type::Pointer(ty) => (ty.size, None),
                    Type::TypeDef(_) => unreachable!(),
                });
        let expected = [
            (TypeSize::Bytes(4), None),
            (TypeSize::Bits(39), None),
            (TypeSize::Bytes(4), Some(ValueSize { size: 1, offset: 7 })),
            (TypeSize::Bytes(4), Some(ValueSize { size: 1, offset: 0 })),
            (TypeSize::Bytes(8), None),
            (TypeSize::Bits(32), None),
            (TypeSize::Bytes(8), None),
            (TypeSize::Bytes(4), None),
        ];
        let actual = actual.collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
