use inkwell::{
    context::Context,
    module::Module,
    AddressSpace,
};

use std::include_bytes;
use std::io::Write;

pub(super) fn write_intrinsics() -> tempfile::NamedTempFile {
    let mut file = tempfile::NamedTempFile::new().expect("Error creating intrinsics file");
    let bytes = include_bytes!("../../intrinsics.o");

    file.write(bytes).expect("Error writing to intrinsics file");

    file
}

pub(super) fn build<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    let f64_type = ctx.f64_type();
    let void_type = ctx.void_type();

    let void_f64_fn_type = void_type.fn_type(&[f64_type.into()], false);

    // -- dumpf
    module.add_function("dumpf", void_f64_fn_type, None);

    // -- putfc
    module.add_function("putfc", void_f64_fn_type, None);

    // -- getfc
    let getfc_fn_type = f64_type.fn_type(&[], false);
    module.add_function("getfc", getfc_fn_type, None);

    // -- pow
    let pow_fn_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);
    module.add_function("pow", pow_fn_type, None);

    // -- Deque functions
    {
        let ptr_type = ctx.bool_type().ptr_type(AddressSpace::Generic);

        let new_deque_fn_type = ptr_type.fn_type(&[], false);
        module.add_function("newDeque", new_deque_fn_type, None);

        let del_deque_fn_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("delDeque", del_deque_fn_type, None);

        let f64_ptr_fn_type = f64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("sizeOfQueue", f64_ptr_fn_type, None);
        module.add_function("peekDeque", f64_ptr_fn_type, None);
        module.add_function("peekHeadDeque", f64_ptr_fn_type, None);
        module.add_function("popDeque", f64_ptr_fn_type, None);
        module.add_function("popHeadDeque", f64_ptr_fn_type, None);

        let push_deque_fn_type = void_type.fn_type(&[ptr_type.into(), f64_type.into()], false);
        module.add_function("pushDeque", push_deque_fn_type, None);
    }
}