#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    BigInt,
    U32,
}

impl ToString for ValueType {
    fn to_string(&self) -> String {
        match self {
            ValueType::U32 => "U32",
            ValueType::BigInt => "BigInt",
        }
        .to_string()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum SizeOption{
    Single(usize),
    Multiple(Vec<(usize, usize)>) // The first value indicates the cmp_id, the second the size
}

impl ToString for SizeOption {
    fn to_string(&self) -> String {
        return match self {
            SizeOption::Single(size) => format!("{}", size),
            SizeOption::Multiple(sizes) => {
                let sizes_str = sizes.iter()
                    .map(|(first, second)| format!("({}, {})", first, second))
                    .collect::<Vec<_>>()
                    .join(", ");
                return format!("[{}]", sizes_str);
            }
        };
    }
}


#[derive(Clone, PartialEq, Eq)]
pub struct InstrContext {
    pub size: SizeOption,
}

impl ToString for InstrContext {
    fn to_string(&self) -> String {
        return format!("size: {}", self.size.to_string());
    }
}
