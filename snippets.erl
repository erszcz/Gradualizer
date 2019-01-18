dbg:stop_clear().
code:add_path("/Users/erszcz/work/erszcz/tracer").
%dbg:tracer().
my_tracer:start().
dbg:p(all, call).
%dbg:tpl(typechecker, type_check_forms, x).
%dbg:tpl(typechecker, type_check_function, x).
%dbg:tpl(typechecker, check_clauses_fun, x).
%dbg:tpl(typechecker, check_clauses, x).
%dbg:tpl(typechecker, check_clauses_union, x).
%dbg:tpl(typechecker, check_clauses_intersect, x).
%dbg:tpl(typechecker, check_clause, x).
%dbg:tpl(typechecker, add_types_pats, x).
%dbg:tpl(typechecker, type_check_block_in, x).
%dbg:tpl(typechecker, refine_clause_arg_tys, x).
%dbg:tpl(typechecker, union_var_binds, x).
dbg:tpl(typechecker, type_check_expr, x).
dbg:tpl(typechecker, type_check_expr_in, x).
dbg:tpl(typechecker, do_type_check_expr_in, x).
%dbg:tpl(typechecker, normalize, x).

dbg:tpl(typechecker, expect_record_type, x).
dbg:tpl(typechecker, type_check_fields, x).
