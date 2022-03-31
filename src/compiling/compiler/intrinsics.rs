use inkwell::{
    context::Context,
    module::Module,
    values::{
        FloatValue,
        IntValue,
    },
    AddressSpace,
};

use std::include_bytes;
use std::io::Write;

pub(super) fn write_intrinsics() -> tempfile::NamedTempFile {
    let mut file = tempfile::NamedTempFile::new().expect("Error creating intrinsics file");
    let bytes = include_bytes!("../../libintrinsics.a");

    file.write(bytes).expect("Error writing to intrinsics file");

    file
}

pub(super) fn build<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    let f64_type = ctx.f64_type();
    let void_type = ctx.void_type();

    // -- putfc
    let putfc_fn_type = void_type.fn_type(&[f64_type.into()], false);
    module.add_function("putfc", putfc_fn_type, None);

    // -- getfc
    let getfc_fn_type = f64_type.fn_type(&[], false);
    module.add_function("getfc", getfc_fn_type, None);

    // -- dumpf
    let dumpf_fn_type = void_type.fn_type(&[f64_type.into()], false);
    module.add_function("dumpf", dumpf_fn_type, None);
}