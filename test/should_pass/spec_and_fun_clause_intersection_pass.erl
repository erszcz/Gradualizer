-module(spec_and_fun_clause_intersection_pass).

-spec foo(bar) -> bar;
         (list()) -> baz | qux.
foo(bar) -> bar;
foo([]) -> baz;
foo([a]) -> qux.
