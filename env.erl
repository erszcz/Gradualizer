-module(env).

-export_type([t1/0]).

-type t1() :: {} | t1().
