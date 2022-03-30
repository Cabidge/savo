use inkwell::{
    context::Context,
    module::Module,
    values::{
        FloatValue,
        IntValue,
    },
    AddressSpace,
};

pub(super) fn build<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    let f64_type = ctx.f64_type();
    let i8_type = ctx.i8_type();
    let str_type = i8_type.ptr_type(AddressSpace::Generic);
    let void_type = ctx.void_type();

    let builder = ctx.create_builder();

    // -- putchar
    let putchar_fn_type = void_type.fn_type(&[i8_type.into()], false);
    let putchar_fn = module.add_function("putchar", putchar_fn_type, None);

    // -- putfc
    {
        let putfc_fn_type = void_type.fn_type(&[f64_type.into()], false);
        let putfc_fn = module.add_function("putfc", putfc_fn_type, None);

        let putfc_block = ctx.append_basic_block(putfc_fn, "entry");

        builder.position_at_end(putfc_block);

        let float = putfc_fn.get_first_param()
            .unwrap()
            .into_float_value();

        let ch: IntValue<'ctx> = builder.build_float_to_unsigned_int(
            float,
            i8_type.into(),
            "fl2ch",
        );

        builder.build_call(putchar_fn, &[ch.into()], "");

        builder.build_return(None);
    }

    // -- getchar
    let getchar_fn_type = i8_type.fn_type(&[], false);
    let getchar_fn = module.add_function("getchar", getchar_fn_type, None);

    // -- getfc
    {
        let getfc_fn_type = f64_type.fn_type(&[], false);
        let getfc_fn = module.add_function("getfc", getfc_fn_type, None);

        let getfc_block = ctx.append_basic_block(getfc_fn, "entry");

        builder.position_at_end(getfc_block);

        let ch = builder
            .build_call(getchar_fn, &[], "")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();

        let fl: FloatValue<'ctx> = builder.build_unsigned_int_to_float(
            ch,
            f64_type.into(),
            "ch2fl",
        );

        builder.build_return(Some(&fl));
    }

    // -- printf
    let printf_fn_type = void_type.fn_type(&[str_type.into()], true);
    module.add_function("printf", printf_fn_type, None);

    // Create format string
    let temp_fn_type = void_type.fn_type(&[], false);
    let temp_fn = module.add_function("?temp?", temp_fn_type, None);
    let temp_block = ctx.append_basic_block(temp_fn, "entry");

    builder.position_at_end(temp_block);

    builder.build_return(None);

    builder.build_global_string_ptr("%.16g\n", ".floatfmt").as_pointer_value();

    unsafe { temp_fn.delete() } // I can't figure out a fucking way to do this better
}