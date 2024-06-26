use lz_fnv::Fnv1a;
use std::collections::BTreeMap;

pub struct IODef {
    pub code: usize,
    pub offset: usize,
    pub lengths: Vec<usize>,
}

// It is an array that contains (name, start position, size)

pub struct SignalInfo{
    pub name: String,
    pub size: usize,
    pub start: usize,
}

pub struct BusInfo{
    pub name: String,
    pub size: usize,
    pub start: usize,
    pub bus_id: usize
}

pub enum WireInfo{
    Signal(SignalInfo),
    Bus(BusInfo)
}

pub type InputList = Vec<WireInfo>;
pub type TemplateList = Vec<String>;
pub struct InfoParallel{
    pub name: String,
    pub is_parallel: bool,
    pub is_not_parallel: bool,
}
pub type TemplateListParallel = Vec<InfoParallel>;
pub type SignalList = Vec<usize>;
pub type InputOutputList = Vec<IODef>;
pub type TemplateInstanceIOMap = BTreeMap<usize, InputOutputList>;
pub type MessageList = Vec<String>;

pub fn hasher(value: &str) -> u64 {
    use lz_fnv::FnvHasher;
    let mut fnv_hasher: Fnv1a<u64> = Fnv1a::with_key(14695981039346656037);
    fnv_hasher.write(value.as_bytes());
    fnv_hasher.finish()
}
