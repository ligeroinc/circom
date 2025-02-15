
use super::types::*;
use super::wasm::*;


/// Stores references to Ligetron functions
pub struct LigetronContext {
    pub print: WASMFunctionRef,
    pub print_str: WASMFunctionRef,
    pub dump_memory: WASMFunctionRef,
    pub assert_one: WASMFunctionRef,

    pub fp256_init: CircomFunctionRef,
    pub fp256_clear: CircomFunctionRef,
    pub fp256_set_ui: CircomFunctionRef,
    pub fp256_get_ui: CircomFunctionRef,
    pub fp256_from_hex: CircomFunctionRef,
    pub fp256_set_str: CircomFunctionRef,
    pub fp256_set_fp256: CircomFunctionRef,
    pub fp256_set_fp256_with_witness: CircomFunctionRef,
    pub fp256_set_fp256_raw: CircomFunctionRef,
    pub fp256_create_witness: CircomFunctionRef,
    pub fp256_print: CircomFunctionRef,

    pub fp256_addmod: CircomFunctionRef,
    pub fp256_submod: CircomFunctionRef,
    pub fp256_mulmod: CircomFunctionRef,
    pub fp256_divmod: CircomFunctionRef,
    pub fp256_addmod_raw: CircomFunctionRef,
    pub fp256_submod_raw: CircomFunctionRef,
    pub fp256_mulmod_raw: CircomFunctionRef,
    pub fp256_divmod_raw: CircomFunctionRef,

    pub fp256_assert_equal: CircomFunctionRef,
    pub fp256_eqz: CircomFunctionRef,

    pub fp256_pow: CircomFunctionRef,
    pub fp256_idiv: CircomFunctionRef,
    pub fp256_mod: CircomFunctionRef,
    pub fp256_neg: CircomFunctionRef,

    pub fp256_pow_raw: CircomFunctionRef,
    pub fp256_idiv_raw: CircomFunctionRef,
    pub fp256_mod_raw: CircomFunctionRef,
    pub fp256_neg_raw: CircomFunctionRef,

    pub fp256_shl: CircomFunctionRef,
    pub fp256_shr: CircomFunctionRef,

    pub fp256_shl_raw: CircomFunctionRef,
    pub fp256_shr_raw: CircomFunctionRef,

    pub fp256_bor: CircomFunctionRef,
    pub fp256_band: CircomFunctionRef,
    pub fp256_bxor: CircomFunctionRef,
    pub fp256_bnot: CircomFunctionRef,

    pub fp256_bor_raw: CircomFunctionRef,
    pub fp256_band_raw: CircomFunctionRef,
    pub fp256_bxor_raw: CircomFunctionRef,
    pub fp256_bnot_raw: CircomFunctionRef,

    pub fp256_bit_decompose_n: CircomFunctionRef,
    pub fp256_bit_extract: CircomFunctionRef,
    pub fp256_bit_extract_raw: CircomFunctionRef,

    pub fp256_leq: CircomFunctionRef,
    pub fp256_geq: CircomFunctionRef,
    pub fp256_lt: CircomFunctionRef,
    pub fp256_gt: CircomFunctionRef,
    pub fp256_eq: CircomFunctionRef,
    pub fp256_neq: CircomFunctionRef,

    pub fp256_leq_raw: CircomFunctionRef,
    pub fp256_geq_raw: CircomFunctionRef,
    pub fp256_lt_raw: CircomFunctionRef,
    pub fp256_gt_raw: CircomFunctionRef,
    pub fp256_eq_raw: CircomFunctionRef,
    pub fp256_neq_raw: CircomFunctionRef,

    pub fp256_lor: CircomFunctionRef,
    pub fp256_land: CircomFunctionRef,
    pub fp256_lnot: CircomFunctionRef,

    pub fp256_lor_raw: CircomFunctionRef,
    pub fp256_land_raw: CircomFunctionRef,
    pub fp256_lnot_raw: CircomFunctionRef,
}

impl LigetronContext {
    /// Creates new Ligetrong context making imports in specified WASM module
    pub fn new(module: &mut WASMModule) -> LigetronContext {
        let ligetron_print_type = WASMFunctionType::new().with_params(&[WASMType::I64]);
        let ligetron_print = module.import_function("print",
                                                    ligetron_print_type,
                                                    "env",
                                                    "print");

        let ligetron_print_str_type = WASMFunctionType::new().with_params(&[WASMType::PTR, WASMType::I32]);
        let ligetron_print_str = module.import_function("print_str",
                                                        ligetron_print_str_type,
                                                        "env",
                                                        "print_str");

        let ligetron_dump_memory_type = WASMFunctionType::new().with_params(&[WASMType::PTR, WASMType::I32]);
        let ligetron_dump_memory = module.import_function("dump_memory",
                                                          ligetron_dump_memory_type,
                                                          "env",
                                                          "dump_memory");

        let ligetron_assert_one_type = WASMFunctionType::new().with_params(&[WASMType::I32]);
        let ligetron_assert_one = module.import_function("assert_one",
                                                         ligetron_assert_one_type,
                                                         "env",
                                                         "assert_one");

        let fp256_init_type = CircomFunctionType::new(vec![], vec![CircomValueType::FR]);
        let fp256_init = Self::import_function(module, "fp256_init", fp256_init_type);

        let fp256_clear_type = CircomFunctionType::new(vec![], vec![CircomValueType::FR]);
        let fp256_clear = Self::import_function(module, "fp256_clear", fp256_clear_type);

        let fp256_set_ui_type = CircomFunctionType::new(vec![CircomValueType::WASM(WASMType::I64)],
                                                        vec![CircomValueType::FR]);
        let fp256_set_ui = Self::import_function(module, "fp256_set_ui", fp256_set_ui_type);

        let fp256_get_ui_type = CircomFunctionType::new(vec![CircomValueType::FR],
                                                        vec![CircomValueType::WASM(WASMType::I64)]);
        let fp256_get_ui = Self::import_function(module, "fp256_get_ui", fp256_get_ui_type);

        let fp256_from_hex_params = vec![CircomValueType::WASM(WASMType::PTR),
                                         CircomValueType::WASM(WASMType::I32)];
        let fp256_from_hex_type = CircomFunctionType::new(fp256_from_hex_params,
                                                          vec![CircomValueType::FR]);
        let fp256_from_hex = Self::import_function(module, "fp256_from_hex", fp256_from_hex_type);

        let fp256_set_str_params = vec![CircomValueType::WASM(WASMType::PTR),
                                         CircomValueType::WASM(WASMType::I32)];
        let fp256_set_str_ret_types = vec![CircomValueType::FR,
                                           CircomValueType::WASM(WASMType::I32)];
        let fp256_set_str_type = CircomFunctionType::new(fp256_set_str_params,
                                                         fp256_set_str_ret_types);
        let fp256_set_str = Self::import_function(module, "fp256_set_str", fp256_set_str_type);

        let fp256_set_fp256_type = CircomFunctionType::new(vec![CircomValueType::FR],
                                                           vec![CircomValueType::FR]);
        let fp256_set_fp256 = Self::import_function(module, "fp256_set_fp256", fp256_set_fp256_type.clone());
        let fp256_set_fp256_raw = Self::import_function(module, "fp256_set_fp256_raw", fp256_set_fp256_type.clone());
        let fp256_set_fp256_with_witness = Self::import_function(module, "fp256_set_fp256_with_witness", fp256_set_fp256_type);

        let fp256_create_witness_type = CircomFunctionType::new(vec![CircomValueType::FR], vec![]);
        let fp256_create_witness = Self::import_function(module, "fp256_create_witness", fp256_create_witness_type);

        let fp256_print_type = CircomFunctionType::new(vec![], vec![CircomValueType::FR]);
        let fp256_print = Self::import_function(module, "fp256_print", fp256_print_type);

        let fp256_uop_type = CircomFunctionType::new(vec![CircomValueType::FR],
                                                     vec![CircomValueType::FR]);
        let fp256_bop_type = CircomFunctionType::new(vec![CircomValueType::FR, CircomValueType::FR],
                                                    vec![CircomValueType::FR]);

        let fp256_addmod = Self::import_function(module, "fp256_addmod", fp256_bop_type.clone());
        let fp256_submod = Self::import_function(module, "fp256_submod", fp256_bop_type.clone());
        let fp256_mulmod = Self::import_function(module, "fp256_mulmod", fp256_bop_type.clone());
        let fp256_divmod = Self::import_function(module, "fp256_divmod", fp256_bop_type.clone());

        let fp256_addmod_raw = Self::import_function(module, "fp256_addmod_raw", fp256_bop_type.clone());
        let fp256_submod_raw = Self::import_function(module, "fp256_submod_raw", fp256_bop_type.clone());
        let fp256_mulmod_raw = Self::import_function(module, "fp256_mulmod_raw", fp256_bop_type.clone());
        let fp256_divmod_raw = Self::import_function(module, "fp256_divmod_raw", fp256_bop_type.clone());

        let fp256_assert_equal_type = CircomFunctionType::new(vec![CircomValueType::FR,
                                                                   CircomValueType::FR],
                                                              vec![]);
        let fp256_assert_equal = Self::import_function(module, "fp256_assert_equal", fp256_assert_equal_type);

        let fp256_eqz_type = CircomFunctionType::new(vec![CircomValueType::FR],
                                                     vec![CircomValueType::WASM(WASMType::I32)]);
        let fp256_eqz = Self::import_function(module, "fp256_eqz", fp256_eqz_type);

        let fp256_pow = Self::import_function(module, "fp256_powmod", fp256_bop_type.clone());
        let fp256_idiv = Self::import_function(module, "fp256_idivmod", fp256_bop_type.clone());
        let fp256_mod = Self::import_function(module, "fp256_mod", fp256_bop_type.clone());
        let fp256_neg = Self::import_function(module, "fp256_negmod", fp256_uop_type.clone());

        let fp256_pow_raw = Self::import_function(module, "fp256_powmod_raw", fp256_bop_type.clone());
        let fp256_idiv_raw = Self::import_function(module, "fp256_idivmod_raw", fp256_bop_type.clone());
        let fp256_mod_raw = Self::import_function(module, "fp256_mod_raw", fp256_bop_type.clone());
        let fp256_neg_raw = Self::import_function(module, "fp256_negmod_raw", fp256_uop_type.clone());

        let fp256_shl = Self::import_function(module, "fp256_shlmod", fp256_bop_type.clone());
        let fp256_shr = Self::import_function(module, "fp256_shrmod", fp256_bop_type.clone());

        let fp256_shl_raw = Self::import_function(module, "fp256_shlmod_raw", fp256_bop_type.clone());
        let fp256_shr_raw = Self::import_function(module, "fp256_shrmod_raw", fp256_bop_type.clone());

        let fp256_bor = Self::import_function(module, "fp256_bormod", fp256_bop_type.clone());
        let fp256_band = Self::import_function(module, "fp256_bandmod", fp256_bop_type.clone());
        let fp256_bxor = Self::import_function(module, "fp256_bxormod", fp256_bop_type.clone());
        let fp256_bnot = Self::import_function(module, "fp256_bnotmod", fp256_uop_type.clone());

        let fp256_bor_raw = Self::import_function(module, "fp256_bormod_raw", fp256_bop_type.clone());
        let fp256_band_raw = Self::import_function(module, "fp256_bandmod_raw", fp256_bop_type.clone());
        let fp256_bxor_raw = Self::import_function(module, "fp256_bxormod_raw", fp256_bop_type.clone());
        let fp256_bnot_raw = Self::import_function(module, "fp256_bnotmod_raw", fp256_uop_type.clone());

        let fp256_bit_decompose_n_type = CircomFunctionType::new(vec![CircomValueType::FR,
                                                                      CircomValueType::FR,
                                                                      CircomValueType::WASM_I32()],
                                                                 vec![]);
        let fp256_bit_decompose_n = Self::import_function(module,
                                                          "fp256_bit_decompose_n",
                                                          fp256_bit_decompose_n_type);

        let fp256_bit_extract_type = CircomFunctionType::new(vec![CircomValueType::FR,
                                                                  CircomValueType::WASM_I32()],
                                                             vec![CircomValueType::FR]);
        let fp256_bit_extract = Self::import_function(module,
                                                      "fp256_bit_extract",
                                                      fp256_bit_extract_type.clone());
        let fp256_bit_extract_raw = Self::import_function(module,
                                                          "fp256_bit_extract_raw",
                                                          fp256_bit_extract_type);

        let fp256_leq = Self::import_function(module, "fp256_leq", fp256_bop_type.clone());
        let fp256_geq = Self::import_function(module, "fp256_geq", fp256_bop_type.clone());
        let fp256_lt = Self::import_function(module, "fp256_lt", fp256_bop_type.clone());
        let fp256_gt = Self::import_function(module, "fp256_gt", fp256_bop_type.clone());
        let fp256_eq = Self::import_function(module, "fp256_eq", fp256_bop_type.clone());
        let fp256_neq = Self::import_function(module, "fp256_neq", fp256_bop_type.clone());

        let fp256_leq_raw = Self::import_function(module, "fp256_leq_raw", fp256_bop_type.clone());
        let fp256_geq_raw = Self::import_function(module, "fp256_geq_raw", fp256_bop_type.clone());
        let fp256_lt_raw = Self::import_function(module, "fp256_lt_raw", fp256_bop_type.clone());
        let fp256_gt_raw = Self::import_function(module, "fp256_gt_raw", fp256_bop_type.clone());
        let fp256_eq_raw = Self::import_function(module, "fp256_eq_raw", fp256_bop_type.clone());
        let fp256_neq_raw = Self::import_function(module, "fp256_neq_raw", fp256_bop_type.clone());

        let fp256_lor = Self::import_function(module, "fp256_lor", fp256_bop_type.clone());
        let fp256_land = Self::import_function(module, "fp256_land", fp256_bop_type.clone());
        let fp256_lnot = Self::import_function(module, "fp256_lnot", fp256_uop_type.clone());

        let fp256_lor_raw = Self::import_function(module, "fp256_lor_raw", fp256_bop_type.clone());
        let fp256_land_raw = Self::import_function(module, "fp256_land_raw", fp256_bop_type.clone());
        let fp256_lnot_raw = Self::import_function(module, "fp256_lnot_raw", fp256_uop_type.clone());

        return LigetronContext {
            print: ligetron_print,
            print_str: ligetron_print_str,
            dump_memory: ligetron_dump_memory,
            assert_one: ligetron_assert_one,

            fp256_init: fp256_init,
            fp256_clear: fp256_clear,
            fp256_set_ui: fp256_set_ui,
            fp256_get_ui: fp256_get_ui,
            fp256_from_hex: fp256_from_hex,
            fp256_set_str: fp256_set_str,
            fp256_set_fp256: fp256_set_fp256,
            fp256_set_fp256_raw: fp256_set_fp256_raw,
            fp256_set_fp256_with_witness: fp256_set_fp256_with_witness,
            fp256_create_witness: fp256_create_witness,
            fp256_print: fp256_print,

            fp256_addmod: fp256_addmod,
            fp256_submod: fp256_submod,
            fp256_mulmod: fp256_mulmod,
            fp256_divmod: fp256_divmod,

            fp256_addmod_raw: fp256_addmod_raw,
            fp256_submod_raw: fp256_submod_raw,
            fp256_mulmod_raw: fp256_mulmod_raw,
            fp256_divmod_raw: fp256_divmod_raw,

            fp256_assert_equal: fp256_assert_equal,
            fp256_eqz: fp256_eqz,


            fp256_pow: fp256_pow,
            fp256_idiv: fp256_idiv,
            fp256_mod: fp256_mod,
            fp256_neg: fp256_neg,

            fp256_pow_raw: fp256_pow_raw,
            fp256_idiv_raw: fp256_idiv_raw,
            fp256_mod_raw: fp256_mod_raw,
            fp256_neg_raw: fp256_neg_raw,

            fp256_shl: fp256_shl,
            fp256_shr: fp256_shr,

            fp256_shl_raw: fp256_shl_raw,
            fp256_shr_raw: fp256_shr_raw,

            fp256_bor: fp256_bor,
            fp256_band: fp256_band,
            fp256_bxor: fp256_bxor,
            fp256_bnot: fp256_bnot,

            fp256_bor_raw: fp256_bor_raw,
            fp256_band_raw: fp256_band_raw,
            fp256_bxor_raw: fp256_bxor_raw,
            fp256_bnot_raw: fp256_bnot_raw,

            fp256_bit_decompose_n: fp256_bit_decompose_n,
            fp256_bit_extract: fp256_bit_extract,
            fp256_bit_extract_raw: fp256_bit_extract_raw,

            fp256_leq: fp256_leq,
            fp256_geq: fp256_geq,
            fp256_lt: fp256_lt,
            fp256_gt: fp256_gt,
            fp256_eq: fp256_eq,
            fp256_neq: fp256_neq,

            fp256_leq_raw: fp256_leq_raw,
            fp256_geq_raw: fp256_geq_raw,
            fp256_lt_raw: fp256_lt_raw,
            fp256_gt_raw: fp256_gt_raw,
            fp256_eq_raw: fp256_eq_raw,
            fp256_neq_raw: fp256_neq_raw,

            fp256_lor: fp256_lor,
            fp256_land: fp256_land,
            fp256_lnot: fp256_lnot,

            fp256_lor_raw: fp256_lor_raw,
            fp256_land_raw: fp256_land_raw,
            fp256_lnot_raw: fp256_lnot_raw
        };
    }

    /// Imports ligetron function
    fn import_function(module: &mut WASMModule,
                       name: &str,
                       tp: CircomFunctionType) -> CircomFunctionRef {
        module.import_function(&name, tp.to_wasm(), "ligetron", &name);
        return CircomFunctionRef::new(name.to_string(), tp);
    }
}
