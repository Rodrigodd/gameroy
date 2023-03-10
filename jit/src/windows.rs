use std::mem::{align_of, size_of};

use dynasmrt::mmap::MutableBuffer;
use windows_sys::Win32::System::Diagnostics::Debug::{
    RtlAddFunctionTable, IMAGE_RUNTIME_FUNCTION_ENTRY, IMAGE_RUNTIME_FUNCTION_ENTRY_0,
};

/// See: https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170#unwind-operation-code
const UWOP_PUSH_NONVOL: u8 = 0;

// See: https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170#operation-info
const RBX: u8 = 3;
const RPB: u8 = 5;
const R12: u8 = 12;

/// See: https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170#struct-unwind_code
#[allow(dead_code)]
#[repr(C)]
struct UnwindCode {
    /// Offset in prolog where the operation occurs.
    offset_in_prolog: u8,
    /// Bit Fields:
    /// - Unwind operation code: 4
    /// - Operation_info: 4
    code_info: u8,
}

/// See: https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170#struct-unwind_info
#[repr(C, align(4))]
struct UnwindInfo {
    /// Bit Fields:
    /// - Version: 3
    /// - Flags: 5
    version_flags: u8,
    /// Size of prolog. Used by the Unwind Procedure to see if the code is in the middle of the
    /// prolog, and need special handling.
    ///
    /// See Unwind procedure 3.a: https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-170#unwind-procedure
    size_of_prolog: u8,
    /// Size of `unwind_codes_array`
    count_of_unwind_code: u8,
    /// Bit Fields:
    /// - Frame Register: 4
    /// - Frame Register offset (scaled): 4
    frame_register_frame_register_offset: u8,
    /// An array of items explaning the effect of the prolog in non-volatile registers. Windows docs
    /// say that the size of the array must be even for alignment purposes, but I only thing this is
    /// necessary if the next optional field don't have alignment of 4 bytes.
    unwind_codes_array: [UnwindCode; 3],
    // Following here, can be a Exceptional Handler, or a Chained Unwind Info, but I am not using
    // neither. The docs are not clear if they must exist, but the example in "Unwind data
    // definitions in C" indicates that they are optional.
}

/// Returns the smallest value greater or equal to x, with the given alignment.
fn align(x: usize, alignment: usize) -> usize {
    debug_assert!(alignment.is_power_of_two());
    (x - 1 + alignment) & alignment.wrapping_neg()
}

/// Writes the given code to a `MutableBuffer`, including its Unwind Information, and also register
/// this information using `RtlAddFunctionTable`.
// TODO: Must also returns a handle that allows calling `RtlDeleteFunctionTable`.
pub fn to_mutable_buffer_with_unwin_info(
    code: Vec<u8>,
    prolog_len: u8,
    push_r12_offset: u8,
    push_rbx_offset: u8,
    push_rbp_offset: u8,
) -> MutableBuffer {
    let code_offset = 0;
    let unwind_info_offset = align(
        code_offset + code.len(),
        align_of::<IMAGE_RUNTIME_FUNCTION_ENTRY>(),
    );
    let function_table_offset = align(
        unwind_info_offset + size_of::<IMAGE_RUNTIME_FUNCTION_ENTRY>(),
        align_of::<IMAGE_RUNTIME_FUNCTION_ENTRY>(),
    );
    let len = function_table_offset + size_of::<IMAGE_RUNTIME_FUNCTION_ENTRY>();

    let mut buffer = MutableBuffer::new(len).unwrap();
    buffer.set_len(len);
    buffer[..code.len()].copy_from_slice(code.as_slice());

    let dyn_base = buffer.as_ptr() as u64;

    let unwind_info = buffer[unwind_info_offset..].as_mut_ptr() as *mut UnwindInfo;
    unsafe {
        std::ptr::write(
            unwind_info,
            UnwindInfo {
                version_flags: 1 | (0 << 3) as u8,
                size_of_prolog: prolog_len,
                count_of_unwind_code: 3,
                frame_register_frame_register_offset: 0,
                // the unwind code must be in reverse order. Read 3.b) of Unwind Procedure.
                unwind_codes_array: [
                    UnwindCode {
                        offset_in_prolog: push_r12_offset,
                        code_info: UWOP_PUSH_NONVOL | (R12 << 4),
                    },
                    UnwindCode {
                        offset_in_prolog: push_rbx_offset,
                        code_info: UWOP_PUSH_NONVOL | (RBX << 4),
                    },
                    UnwindCode {
                        offset_in_prolog: push_rbp_offset,
                        code_info: UWOP_PUSH_NONVOL | (RPB << 4),
                    },
                ],
            },
        );
    }

    let function_table =
        buffer[function_table_offset..].as_mut_ptr() as *mut IMAGE_RUNTIME_FUNCTION_ENTRY;
    unsafe {
        std::ptr::write(
            function_table,
            IMAGE_RUNTIME_FUNCTION_ENTRY {
                BeginAddress: 0,
                EndAddress: code.len() as u32,
                Anonymous: IMAGE_RUNTIME_FUNCTION_ENTRY_0 {
                    UnwindInfoAddress: unwind_info_offset as u32,
                },
            },
        );
    }

    if unsafe { RtlAddFunctionTable(function_table, 1, dyn_base) } == 0 {
        eprintln!("Registering Function Table failed!!");
    }
    buffer
}
