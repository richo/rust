// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//! Declare various LLVM values.
//!
//! Prefer using functions and methods from this module rather than calling LLVM
//! functions directly. These functions do some additional work to ensure we do
//! the right thing given the preconceptions of trans.
//!
//! Some useful guidelines:
//!
//! * Use declare_* family of methods if you are declaring, but are not
//!   interested in defining the ValueRef they return.
//! * Use define_* family of methods when you might be defining the ValueRef.
//! * When in doubt, define.
use llvm::{self, ValueRef};
use middle::ty::{self, ClosureTyper};
use syntax::abi;
use trans::attributes;
use trans::base;
use trans::common::{self, ExprId};
use trans::callee;
use trans::context::CrateContext;
use trans::monomorphize;
use trans::type_::Type;
use trans::type_of;

use std::ffi::CString;
use libc::c_uint;


/// Declare a global value.
///
/// If there’s a value with the same name already declared, the function will
/// return its ValueRef instead.
pub fn declare_global(ccx: &CrateContext, name: &str, ty: Type) -> llvm::ValueRef {
    debug!("declare_global(name={:?})", name);
    let namebuf = CString::new(name).unwrap_or_else(|_|{
        ccx.sess().bug(&format!("name {:?} contains an interior null byte", name))
    });
    unsafe {
        llvm::LLVMGetOrInsertGlobal(ccx.llmod(), namebuf.as_ptr(), ty.to_ref())
    }
}


/// Declare a function.
///
/// For rust functions use `declare_rust_fn` instead.
///
/// If there’s a value with the same name already declared, the function will
/// update the declaration and return existing ValueRef instead.
pub fn declare_fn(ccx: &CrateContext, name: &str, callconv: llvm::CallConv,
                  ty: Type, output: ty::FnOutput) -> ValueRef {
    warn!("declare_fn(name={:?})", name);
    let namebuf = CString::new(name).unwrap_or_else(|_|{
        ccx.sess().bug(&format!("name {:?} contains an interior null byte", name))
    });
    let llfn = unsafe {
        llvm::LLVMGetOrInsertFunction(ccx.llmod(), namebuf.as_ptr(), ty.to_ref())
    };
    warn!("Got fn: {:?}", llfn);

    llvm::SetFunctionCallConv(llfn, callconv);
    if name == "rust_eh_personality" {
        return llfn
    }

    // The exception handling personality function.
    //
    // If our compilation unit has the `eh_personality` lang item somewhere
    // within it, then we just need to translate that. Otherwise, we're
    // building an rlib which will depend on some upstream implementation of
    // this function, so we just codegen a generic reference to it. We don't
    // specify any of the types for the function, we just make it a symbol
    // that LLVM can later use.
    //
    // Note that MSVC is a little special here in that we don't use the
    // `eh_personality` lang item at all. Currently LLVM has support for
    // both Dwarf and SEH unwind mechanisms for MSVC targets and uses the
    // *name of the personality function* to decide what kind of unwind side
    // tables/landing pads to emit. It looks like Dwarf is used by default,
    // injecting a dependency on the `_Unwind_Resume` symbol for resuming
    // an "exception", but for MSVC we want to force SEH. This means that we
    // can't actually have the personality function be our standard
    // `rust_eh_personality` function, but rather we wired it up to the
    // CRT's custom `__C_specific_handler` personality funciton, which
    // forces LLVM to consider landing pads as "landing pads for SEH".
    let target = &ccx.sess().target.target;

    let llpersonality = match ccx.tcx().lang_items.eh_personality() {
        // TODO(richo) We don't obviously have a function context to lookup yet
        // Some(def_id) if !target.options.is_like_msvc => {
        //     callee::trans_fn_ref(ccx, def_id, ExprId(0),
        //                          ccx.fcx.param_substs).val
        // }
        _ => {
            let mut personality = ccx.eh_personality().borrow_mut();
            match *personality {
                Some(llpersonality) => {
                    warn!("Got a personality");
                    llpersonality
                },
                None => {
                    let name = if target.options.is_like_msvc {
                        "__C_specific_handler"
                    } else {
                        "rust_eh_personality"
                    };
                    warn!("Looking up a personality");
                    let fty = Type::variadic_func(&[], &Type::i32(ccx));
                    warn!("Setup fty");
                    let f = declare_cfn(ccx, name, fty, ccx.tcx().types.i32);
                    warn!("Declared a fn");
                    *personality = Some(f);
                    f
                }
            }
        }
    };

    warn!("Personality fn: {:?}", llpersonality);

    llvm::SetFunctionPersonalityFn(llfn, llpersonality);
    // Function addresses in Rust are never significant, allowing functions to
    // be merged.
    llvm::SetUnnamedAddr(llfn, true);

    if output == ty::FnDiverging {
        llvm::SetFunctionAttribute(llfn, llvm::Attribute::NoReturn);
    }

    if ccx.tcx().sess.opts.cg.no_redzone
        .unwrap_or(ccx.tcx().sess.target.target.options.disable_redzone) {
        llvm::SetFunctionAttribute(llfn, llvm::Attribute::NoRedZone)
    }

    if ccx.is_split_stack_supported() && !ccx.sess().opts.cg.no_stack_check {
        attributes::split_stack(llfn, true);
    }
    llfn
}


/// Declare a C ABI function.
///
/// Only use this for foreign function ABIs and glue. For Rust functions use
/// `declare_rust_fn` instead.
///
/// If there’s a value with the same name already declared, the function will
/// update the declaration and return existing ValueRef instead.
pub fn declare_cfn(ccx: &CrateContext, name: &str, fn_type: Type,
                   output: ty::Ty) -> ValueRef {
    warn!("Entered declare_cfn");
    declare_fn(ccx, name, llvm::CCallConv, fn_type, ty::FnConverging(output))
}


/// Declare a Rust function.
///
/// If there’s a value with the same name already declared, the function will
/// update the declaration and return existing ValueRef instead.
pub fn declare_rust_fn<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, name: &str,
                                 fn_type: ty::Ty<'tcx>) -> ValueRef {
    debug!("declare_rust_fn(name={:?}, fn_type={:?})", name,
           fn_type);
    let fn_type = monomorphize::normalize_associated_type(ccx.tcx(), &fn_type);
    debug!("declare_rust_fn (after normalised associated types) fn_type={:?}",
           fn_type);

    let function_type; // placeholder so that the memory ownership works out ok
    let (sig, abi, env) = match fn_type.sty {
        ty::TyBareFn(_, ref f) => {
            (&f.sig, f.abi, None)
        }
        ty::TyClosure(closure_did, substs) => {
            let typer = common::NormalizingClosureTyper::new(ccx.tcx());
            function_type = typer.closure_type(closure_did, substs);
            let self_type = base::self_type_for_closure(ccx, closure_did, fn_type);
            let llenvironment_type = type_of::type_of_explicit_arg(ccx, self_type);
            debug!("declare_rust_fn function_type={:?} self_type={:?}",
                   function_type, self_type);
            (&function_type.sig, abi::RustCall, Some(llenvironment_type))
        }
        _ => ccx.sess().bug("expected closure or fn")
    };

    let sig = ty::Binder(ty::erase_late_bound_regions(ccx.tcx(), sig));
    debug!("declare_rust_fn (after region erasure) sig={:?}", sig);
    let llfty = type_of::type_of_rust_fn(ccx, env, &sig, abi);
    debug!("declare_rust_fn llfty={}", ccx.tn().type_to_string(llfty));

    // it is ok to directly access sig.0.output because we erased all
    // late-bound-regions above
    let llfn = declare_fn(ccx, name, llvm::CCallConv, llfty, sig.0.output);
    attributes::from_fn_type(ccx, fn_type).apply_llfn(llfn);
    llfn
}


/// Declare a Rust function with internal linkage.
///
/// If there’s a value with the same name already declared, the function will
/// update the declaration and return existing ValueRef instead.
pub fn declare_internal_rust_fn<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, name: &str,
                                          fn_type: ty::Ty<'tcx>) -> ValueRef {
    let llfn = declare_rust_fn(ccx, name, fn_type);
    llvm::SetLinkage(llfn, llvm::InternalLinkage);
    llfn
}


/// Declare a global with an intention to define it.
///
/// Use this function when you intend to define a global. This function will
/// return None if the name already has a definition associated with it. In that
/// case an error should be reported to the user, because it usually happens due
/// to user’s fault (e.g. misuse of #[no_mangle] or #[export_name] attributes).
pub fn define_global(ccx: &CrateContext, name: &str, ty: Type) -> Option<ValueRef> {
    if get_defined_value(ccx, name).is_some() {
        None
    } else {
        Some(declare_global(ccx, name, ty))
    }
}


/// Declare a function with an intention to define it.
///
/// For rust functions use `define_rust_fn` instead.
///
/// Use this function when you intend to define a function. This function will
/// return None if the name already has a definition associated with it. In that
/// case an error should be reported to the user, because it usually happens due
/// to user’s fault (e.g. misuse of #[no_mangle] or #[export_name] attributes).
pub fn define_fn(ccx: &CrateContext, name: &str, callconv: llvm::CallConv, fn_type: Type,
                 output: ty::FnOutput) -> Option<ValueRef> {
    if get_defined_value(ccx, name).is_some() {
        None
    } else {
        Some(declare_fn(ccx, name, callconv, fn_type, output))
    }
}


/// Declare a C ABI function with an intention to define it.
///
/// Use this function when you intend to define a function. This function will
/// return None if the name already has a definition associated with it. In that
/// case an error should be reported to the user, because it usually happens due
/// to user’s fault (e.g. misuse of #[no_mangle] or #[export_name] attributes).
///
/// Only use this for foreign function ABIs and glue. For Rust functions use
/// `declare_rust_fn` instead.
pub fn define_cfn(ccx: &CrateContext, name: &str, fn_type: Type,
                  output: ty::Ty) -> Option<ValueRef> {
    if get_defined_value(ccx, name).is_some() {
        None
    } else {
        Some(declare_cfn(ccx, name, fn_type, output))
    }
}


/// Declare a Rust function with an intention to define it.
///
/// Use this function when you intend to define a function. This function will
/// return None if the name already has a definition associated with it. In that
/// case an error should be reported to the user, because it usually happens due
/// to user’s fault (e.g. misuse of #[no_mangle] or #[export_name] attributes).
pub fn define_rust_fn<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, name: &str,
                                fn_type: ty::Ty<'tcx>) -> Option<ValueRef> {
    if get_defined_value(ccx, name).is_some() {
        None
    } else {
        Some(declare_rust_fn(ccx, name, fn_type))
    }
}


/// Declare a Rust function with an intention to define it.
///
/// Use this function when you intend to define a function. This function will
/// return None if the name already has a definition associated with it. In that
/// case an error should be reported to the user, because it usually happens due
/// to user’s fault (e.g. misuse of #[no_mangle] or #[export_name] attributes).
pub fn define_internal_rust_fn<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, name: &str,
                                         fn_type: ty::Ty<'tcx>) -> Option<ValueRef> {
    if get_defined_value(ccx, name).is_some() {
        None
    } else {
        Some(declare_internal_rust_fn(ccx, name, fn_type))
    }
}


/// Get defined or externally defined (AvailableExternally linkage) value by name.
fn get_defined_value(ccx: &CrateContext, name: &str) -> Option<ValueRef> {
    debug!("get_defined_value(name={:?})", name);
    let namebuf = CString::new(name).unwrap_or_else(|_|{
        ccx.sess().bug(&format!("name {:?} contains an interior null byte", name))
    });
    let val = unsafe { llvm::LLVMGetNamedValue(ccx.llmod(), namebuf.as_ptr()) };
    if val.is_null() {
        debug!("get_defined_value: {:?} value is null", name);
        None
    } else {
        let (declaration, aext_link) = unsafe {
            let linkage = llvm::LLVMGetLinkage(val);
            (llvm::LLVMIsDeclaration(val) != 0,
             linkage == llvm::AvailableExternallyLinkage as c_uint)
        };
        debug!("get_defined_value: found {:?} value (declaration: {}, \
                aext_link: {})", name, declaration, aext_link);
        if !declaration || aext_link {
            Some(val)
        } else {
            None
        }
    }
}
