
use super::types::*;
use super::wasm::*;


/// FR context, stores references to FR functions
#[derive(Clone)]
pub struct FRContext {
    pub raw_copy: CircomFunctionRef,
    pub is_true: CircomFunctionRef,
    pub to_int: CircomFunctionRef,

    pub copy: CircomFunctionRef,
    pub copyn: CircomFunctionRef,
    pub add: CircomFunctionRef,
    pub sub: CircomFunctionRef,
    pub mul: CircomFunctionRef,
    pub div: CircomFunctionRef,
    pub pow: CircomFunctionRef,
    pub idiv: CircomFunctionRef,
    pub mod_: CircomFunctionRef,
    pub shl: CircomFunctionRef,
    pub shr: CircomFunctionRef,
    pub leq: CircomFunctionRef,
    pub geq: CircomFunctionRef,
    pub lt: CircomFunctionRef,
    pub gt: CircomFunctionRef,
    pub eq: CircomFunctionRef,
    pub neq: CircomFunctionRef,
    pub lor: CircomFunctionRef,
    pub land: CircomFunctionRef,
    pub bor: CircomFunctionRef,
    pub band: CircomFunctionRef,
    pub bxor: CircomFunctionRef,
    pub neg: CircomFunctionRef,
    pub lnot: CircomFunctionRef,
    pub bnot: CircomFunctionRef
}

impl FRContext {
    /// Creates new context
    pub fn new() -> FRContext {
        let fr_raw_copy_type = CircomFunctionType::new(
            vec![CircomValueType::WASM(WASMType::I64)],
            vec![CircomValueType::FR]);
        let fr_raw_copy = CircomFunctionRef::new("Fr_rawCopyS2L".to_string(), fr_raw_copy_type);

        let fr_is_true_type = CircomFunctionType::new(
            vec![CircomValueType::FR],
            vec![CircomValueType::WASM(WASMType::I32)]
        );
        let fr_is_true = CircomFunctionRef::new("Fr_isTrue".to_string(), fr_is_true_type);

        let fr_to_int_type = CircomFunctionType::new(
            vec![CircomValueType::FR],
            vec![CircomValueType::WASM(WASMType::I32)]
        );
        let fr_to_int = CircomFunctionRef::new("Fr_toInt".to_string(), fr_to_int_type);

        let copyn_type = CircomFunctionType::new(
            vec![CircomValueType::FR, CircomValueType::WASM(WASMType::I32)],
            vec![CircomValueType::FR]
        );
        let copyn = CircomFunctionRef::new("Fr_copyn".to_string(), copyn_type);

        return FRContext {
            raw_copy: fr_raw_copy,
            is_true: fr_is_true,
            to_int: fr_to_int,

            copy: Self::create_fr_func_ref("Fr_copy", 1, 1),
            copyn: copyn,
            add: Self::create_fr_func_ref("Fr_add", 2, 1),
            sub: Self::create_fr_func_ref("Fr_sub", 2, 1),
            mul: Self::create_fr_func_ref("Fr_mul", 2, 1),
            div: Self::create_fr_func_ref("Fr_div", 2, 1),
            pow: Self::create_fr_func_ref("Fr_pow", 2, 1),
            idiv: Self::create_fr_func_ref("Fr_idiv", 2, 1),
            mod_: Self::create_fr_func_ref("Fr_mod", 2, 1),
            shl: Self::create_fr_func_ref("Fr_shl", 2, 1),
            shr: Self::create_fr_func_ref("Fr_shr", 2, 1),
            leq: Self::create_fr_func_ref("Fr_leq", 2, 1),
            geq: Self::create_fr_func_ref("Fr_geq", 2, 1),
            lt: Self::create_fr_func_ref("Fr_lt", 2, 1),
            gt: Self::create_fr_func_ref("Fr_gt", 2, 1),
            eq: Self::create_fr_func_ref("Fr_eq", 2, 1),
            neq: Self::create_fr_func_ref("Fr_neq", 2, 1),
            lor: Self::create_fr_func_ref("Fr_lor", 2, 1),
            land: Self::create_fr_func_ref("Fr_land", 2, 1),
            bor: Self::create_fr_func_ref("Fr_bor", 2, 1),
            band: Self::create_fr_func_ref("Fr_band", 2, 1),
            bxor: Self::create_fr_func_ref("Fr_bxor", 2, 1),
            neg: Self::create_fr_func_ref("Fr_neg", 2, 1),
            lnot: Self::create_fr_func_ref("Fr_lnot", 2, 1),
            bnot: Self::create_fr_func_ref("Fr_bnot", 2, 1)
        };
    }

    /// Creates Fr function type with specified number of parameters and results
    fn create_fr_func_type(n_pars: usize, n_res: usize) -> CircomFunctionType {
        let params: Vec<_> = std::iter::repeat([CircomValueType::FR])
            .flatten()
            .take(n_pars)
            .collect();
        let ret_types: Vec<_> = std::iter::repeat([CircomValueType::FR])
            .flatten()
            .take(n_res)
            .collect();
        return CircomFunctionType::new(params, ret_types);
    }

    /// Creates Fr function reference
    fn create_fr_func_ref(name: &str, n_pars: usize, n_res: usize) -> CircomFunctionRef {
        let func_type = Self::create_fr_func_type(n_pars, n_res);
        return CircomFunctionRef::new(name.to_string(), func_type);
    }
}
