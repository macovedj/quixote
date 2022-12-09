const std = @import("std");
const mem = std.mem;

// static int bc_read_error_end(BCReaderState *s)
// {
//     if (!s->error_state) {
//         JS_ThrowSyntaxError(s->ctx, "read after the end of the buffer");
//     }
//     return s->error_state = -1;
// }
fn bc_read_error_end(s: BCReaderState) c_int {
  // if (!s.error_state) {
  //   JS_ThrowSyntaxError(s.ctx, "read after the end of the buffer");
  // }
  s.error_state = -1;
  return s.error_state;
}

// static int bc_get_u8(BCReaderState *s, uint8_t *pval)
// {
//     if (unlikely(s->buf_end - s->ptr < 1)) {
//         *pval = 0; /* avoid warning */
//         return bc_read_error_end(s);
//     }
//     *pval = *s->ptr++;
//     return 0;
// }
fn bc_get_u8(s: *BCReaderState, pval: *u8) c_int {
  if (s.buf_end - s.ptr < 1) {
    pval.* = 0;
    return bc_read_error_end(s);
  }
  s.ptr += 1;
  pval.* = s.ptr;
  return 0;
}

//  /* allow function/module */
const JS_READ_OBJ_BYTECODE = 1 << 0;
// /* avoid duplicating 'buf' data */
const JS_READ_OBJ_ROM_DATA = 1 << 1;
// /* allow SharedArrayBuffer */
const JS_READ_OBJ_SAB = 1 << 2;
const JS_READ_OBJ_REFERENCE = 1 << 3;
// typedef struct JSParseState {
//     JSContext *ctx;
//     int last_line_num;  /* line number of last token */
//     int line_num;       /* line number of current offset */
//     const char *filename;
//     JSToken token;
//     BOOL got_lf; /* true if got line feed before the current token */
//     const uint8_t *last_ptr;
//     const uint8_t *buf_ptr;
//     const uint8_t *buf_end;

//     /* current function code */
//     JSFunctionDef *cur_func;
//     BOOL is_module; /* parsing a module */
//     BOOL allow_html_comments;
//     BOOL ext_json; /* true if accepting JSON superset */
// } JSParseState;

// /* 'input' must be zero terminated i.e. input[input_len] = '\0'. */
// static JSValue __JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
//                                  const char *input, size_t input_len,
//                                  const char *filename, int flags, int scope_idx)
// {
//     JSParseState s1, *s = &s1;
//     int err, js_mode, eval_type;
//     JSValue fun_obj, ret_val;
//     JSStackFrame *sf;
//     JSVarRef **var_refs;
//     JSFunctionBytecode *b;
//     JSFunctionDef *fd;
//     JSModuleDef *m;

//     js_parse_init(ctx, s, input, input_len, filename);
//     skip_shebang(s);

//     eval_type = flags & JS_EVAL_TYPE_MASK;
//     m = NULL;
//     if (eval_type == JS_EVAL_TYPE_DIRECT) {
//         JSObject *p;
//         sf = ctx->rt->current_stack_frame;
//         assert(sf != NULL);
//         assert(JS_VALUE_GET_TAG(sf->cur_func) == JS_TAG_OBJECT);
//         p = JS_VALUE_GET_OBJ(sf->cur_func);
//         assert(js_class_has_bytecode(p->class_id));
//         b = p->u.func.function_bytecode;
//         var_refs = p->u.func.var_refs;
//         js_mode = b->js_mode;
//     } else {
//         sf = NULL;
//         b = NULL;
//         var_refs = NULL;
//         js_mode = 0;
//         if (flags & JS_EVAL_FLAG_STRICT)
//             js_mode |= JS_MODE_STRICT;
//         if (flags & JS_EVAL_FLAG_STRIP)
//             js_mode |= JS_MODE_STRIP;
//         if (eval_type == JS_EVAL_TYPE_MODULE) {
//             JSAtom module_name = JS_NewAtom(ctx, filename);
//             if (module_name == JS_ATOM_NULL)
//                 return JS_EXCEPTION;
//             m = js_new_module_def(ctx, module_name);
//             if (!m)
//                 return JS_EXCEPTION;
//             js_mode |= JS_MODE_STRICT;
//         }
//     }
//     fd = js_new_function_def(ctx, NULL, TRUE, FALSE, filename, 1);
//     if (!fd)
//         goto fail1;
//     s->cur_func = fd;
//     fd->eval_type = eval_type;
//     fd->has_this_binding = (eval_type != JS_EVAL_TYPE_DIRECT);
//     fd->backtrace_barrier = ((flags & JS_EVAL_FLAG_BACKTRACE_BARRIER) != 0);
//     if (eval_type == JS_EVAL_TYPE_DIRECT) {
//         fd->new_target_allowed = b->new_target_allowed;
//         fd->super_call_allowed = b->super_call_allowed;
//         fd->super_allowed = b->super_allowed;
//         fd->arguments_allowed = b->arguments_allowed;
//     } else {
//         fd->new_target_allowed = FALSE;
//         fd->super_call_allowed = FALSE;
//         fd->super_allowed = FALSE;
//         fd->arguments_allowed = TRUE;
//     }
//     fd->js_mode = js_mode;
//     fd->func_name = JS_DupAtom(ctx, JS_ATOM__eval_);
//     if (b) {
//         if (add_closure_variables(ctx, fd, b, scope_idx))
//             goto fail;
//     }
//     fd->module = m;
//     s->is_module = (m != NULL);
//     s->allow_html_comments = !s->is_module;

//     push_scope(s); /* body scope */
//     fd->body_scope = fd->scope_level;
    
//     err = js_parse_program(s);
//     if (err) {
//     fail:
//         free_token(s, &s->token);
//         js_free_function_def(ctx, fd);
//         goto fail1;
//     }

//     /* create the function object and all the enclosed functions */
//     fun_obj = js_create_function(ctx, fd);
//     if (JS_IsException(fun_obj))
//         goto fail1;
//     /* Could add a flag to avoid resolution if necessary */
//     if (m) {
//         m->func_obj = fun_obj;
//         if (js_resolve_module(ctx, m) < 0)
//             goto fail1;
//         fun_obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_MODULE, m));
//     }
//     if (flags & JS_EVAL_FLAG_COMPILE_ONLY) {
//         ret_val = fun_obj;
//     } else {
//         ret_val = JS_EvalFunctionInternal(ctx, fun_obj, this_obj, var_refs, sf);
//     }
//     return ret_val;
//  fail1:
//     /* XXX: should free all the unresolved dependencies */
//     if (m)
//         js_free_module_def(ctx, m);
//     return JS_EXCEPTION;
// }


// const JSParseState = struct {
//   JSContext *ctx;
// } 

const JSContext = struct {};

const JSAtom = u32;

const BCReaderState = struct {
    // JSContext *ctx;
    ctx: *JSContext = undefined,
    // const uint8_t *buf_start, *ptr, *buf_end;
    buf_start: *u8,
    ptr: *u8,
    buf_end: *u8,
    // u32 first_atom;
    first_atom: u32,
    // uint32_t idx_to_atom_count;
    // JSAtom *idx_to_atom;
    idx_to_atom_count: *JSAtom,
    // int error_state;
    error_state: i32,
    // BOOL allow_sab : 8;
    // allow_sab: bool
    // BOOL allow_bytecode : 8;
    // BOOL is_rom_data : 8;
    // BOOL allow_reference : 8;
    // /* object references */
    // JSObject **objects;
    // int objects_count;
    // int objects_size;
    
// #ifdef DUMP_READ_OBJECT
//     const uint8_t *ptr_last;
//     int level;
// #endif
};

// static void js_parse_init(JSContext *ctx, JSParseState *s,
//                           const char *input, size_t input_len,
//                           const char *filename)
// {
//     memset(s, 0, sizeof(*s));
//     s->ctx = ctx;
//     s->filename = filename;
//     s->line_num = 1;
//     s->buf_ptr = (const uint8_t *)input;
//     s->buf_end = s->buf_ptr + input_len;
//     s->token.val = ' ';
//     s->token.line_num = 1;
// }
// fn js_parse_init(ctx: *JSContext, s *JSParseState) {
//   for (s[0..(@sizeOf(s))]) |*b| b.* = 0;
// }

// JSValue JS_ReadObject(JSContext *ctx, const uint8_t *buf, size_t buf_len,
//                        int flags)
// {
//     BCReaderState ss, *s = &ss;
//     JSValue obj;

//     ctx->binary_object_count += 1;
//     ctx->binary_object_size += buf_len;

//     memset(s, 0, sizeof(*s));
//     s->ctx = ctx;
//     s->buf_start = buf;
//     s->buf_end = buf + buf_len;
//     s->ptr = buf;
//     s->allow_bytecode = ((flags & JS_READ_OBJ_BYTECODE) != 0);
//     s->is_rom_data = ((flags & JS_READ_OBJ_ROM_DATA) != 0);
//     s->allow_sab = ((flags & JS_READ_OBJ_SAB) != 0);
//     s->allow_reference = ((flags & JS_READ_OBJ_REFERENCE) != 0);
//     if (s->allow_bytecode)
//         s->first_atom = JS_ATOM_END;
//     else
//         s->first_atom = 1;
//     if (JS_ReadObjectAtoms(s)) {
//         obj = JS_EXCEPTION;
//     } else {
//         obj = JS_ReadObjectRec(s);
//     }
//     bc_reader_free(s);
//     return obj;
// }

const Atoms = enum {
  JS_ATOM_END
};
const JSValue = struct {};

fn JS_ReadObject(ctx: *JSContext, buf: *u8, buf_len: usize, flags: u8) JSValue {
  const s = mem.zeroInit(BCReaderState);
  const obj = mem.zeroInit(JSValue);
  s.ctx = ctx;
  s.buf_start = buf;
  s.buf_end = buf + buf_len;
  s.ptr = buf;
  s.allow_bytecode = ((flags & JS_READ_OBJ_BYTECODE) != 0);
  s.is_rom_data = ((flags & JS_READ_OBJ_ROM_DATA) != 0);
  s.allow_sab = ((flags & JS_READ_OBJ_SAB) != 0);
  s.allow_reference = ((flags & JS_READ_OBJ_REFERENCE) != 0);
  if (s.allow_bytecode) {
    s.first_atom = Atoms.JS_ATOM_END;
  }
  return obj;
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}

test "simple test" {
    // var list = std.ArrayList(i32).init(std.testing.allocator);
    // defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    // try list.append(42);
    // try std.testing.expectEqual(@as(i32, 42), list.pop());
}
