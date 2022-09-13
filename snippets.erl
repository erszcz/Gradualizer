{_,_,{_,[{_,_,[EmptyBin],[],_}]}} = merl:quote("fun (<<>>) -> ok end"),
EmptyBin.
EmptyBinTy = gradualizer_bin:compute_type(EmptyBin).

{_,_,{_,[{_,_,[Bin],[],_}]}} = merl:quote("fun (<<Rest/bytes>>) -> ok end"),
Bin.
BinTy = gradualizer_bin:compute_type(Bin).

%% NonEmpty aka MinimumLen
{_,_,{_,[{_,_,[NonEmptyBin],[],_}]}} = merl:quote("fun (<<A:8, Rest/bytes>>) -> ok end"),
NonEmptyBin.
NonEmptyBinTy = gradualizer_bin:compute_type(NonEmptyBin).

{_,_,{_,[{_,_,[FixedLenBin],[],_}]}} = merl:quote("fun (<<_:16>>) -> ok end"),
FixedLenBin.
FixedLenBinTy = gradualizer_bin:compute_type(FixedLenBin).

{_,_,{_,[{_,_,[NonOctalBin],[],_}]}} = merl:quote("fun (<<_:10>>) -> ok end"),
NonOctalBin.
NonOctalBinTy = gradualizer_bin:compute_type(NonOctalBin).

code:add_path("test").
application:ensure_all_started(gradualizer).
Type = fun (T) -> typelib:remove_pos(typelib:parse_type(T)) end.
Normalize = fun (T) -> typechecker:normalize(Type("gradualizer_type:abstract_type()"),
                                             test_lib:create_env([])) end.
PPNormalized = fun (T) -> io:format("~ts\n", [typelib:pp_type(Normalize(T))]) end.

PPNormalized("gradualizer_type:abstract_type()").


code:add_path("./_build/test/lib/gradualizer/test/property_test/").
Type = fun (T) -> typelib:remove_pos(typelib:parse_type(T)) end.
%Type1 = Type("{}").
Type1 = Type("
             #{t2(t1()) | {} | t1() => <<_:$P, _:_*2>>,
            t2(type_variable :: (type_variable :: t2($Y..43))) =>
                t2(tuple())}
").
Type2 = Type("
             type_variable :: t2(t1() | t2(<<_:1, _:_*4>>) | 5..11 | $f)
").
%Type2 = Type("t2(t2(t1())) | t2(t1()) | any_atom | A1").
TEnv = "
-type t1() :: {}.
-type t2(A1) :: t2(t2(t1())) | t2(t1()) | any_atom | A1.
".
Env = test_lib:create_env(TEnv, []).
%Env = test_lib:create_env([]).
typechecker:glb(Type1, Type2, Env).

TyPair = {
  {type,0,map,
   [{type,0,map_field_assoc,
     [{atom,0,key},{user_type,0,recursive_t,[]}]}]},
  {type,0,map,
   [{type,0,map_field_assoc,
     [{atom,0,key},
      {type,0,map,
       [{type,0,map_field_assoc,
         [{atom,0,key},{user_type,0,recursive_t,[]}]}]}]}]}
 }.

A = #{{{atom,0,key},{atom,0,key}} => true,
      {{type,0,map,
        [{type,0,map_field_assoc,
          [{atom,0,key},{user_type,0,recursive_t,[]}]}]},
       {type,0,map,
        [{type,0,map_field_assoc,
          [{atom,0,key},
           {type,0,map,
            [{type,0,map_field_assoc,
              [{atom,0,key},
               {user_type,0,recursive_t,[]}]}]}]}]}} =>
      true,
      {{type,0,map_field_assoc,
        [{atom,0,key},
         {type,0,map,
          [{type,0,map_field_assoc,
            [{atom,0,key},{user_type,0,recursive_t,[]}]}]}]},
       {type,0,map_field_assoc,
        [{atom,0,key},
         {type,0,map,
          [{type,0,map_field_assoc,
            [{atom,0,key},
             {type,0,map,
              [{type,0,map_field_assoc,
                [{atom,0,key},
                 {user_type,0,recursive_t,[]}]}]}]}]}]}} =>
      true}.

Ty1 = {type,0,union,
       [{type,0,binary,[{integer,0,0},{integer,0,0}]},
        {user_type,0,t2,
         [{type,0,union,
           [{user_type,0,t1,[]},
            {type,0,nil,[]},
            {user_type,0,t1,[]},
            {type,0,binary,[{integer,0,6},{char,0,107}]}]}]},
        {char,0,115},
        {type,0,union,
         [{user_type,0,t2,
           [{user_type,0,t2,[{type,0,record,[{atom,0,r2}]}]}]},
          {type,0,'fun',[]},
          {type,0,nonempty_binary,[]}]}]}.

Ty2 = {type,0,union,
       [{type,0,nil,[]},
        {user_type,0,t2,
         [{user_type,0,t2,
           [{type,0,'fun',
             [{type,0,any},{user_type,0,t1,[]}]}]}]},
        {user_type,0,t1,[]},
        {user_type,0,t2,[{type,0,tuple,any}]}]}.

TEnv = "
-type t1() :: {}.
-type t2(A1) :: t2(t2(t1())) | t2(t1()) | any_atom | A1.
".
Env = test_lib:create_env(TEnv, []).
typechecker:glb(Ty1, Ty2, Env).

Forms = [{attribute,0,module,module_name},
         {attribute,0,record,
          {r1,[{record_field,0,{atom,0,f1}},{record_field,0,{atom,0,f2}}]}},
         {attribute,0,record,{r2,[]}},
         {attribute,0,record,{r3,[{record_field,0,{atom,0,f1}}]}},
         {attribute,0,export,[{f1,1},{f1,2},{f2,0}]},
         {function,0,f1,1,
          [{clause,0,
            [{integer,0,5}],
            [[{integer,0,3},{string,0,[]}]],
            [{atom,0,false},
             {'if',0,[{clause,0,[],[],[{string,0,"P"},{integer,0,1}]}]}]}]},
         {function,0,f1,2,
          [{clause,0,
            [{char,0,80},{integer,0,572}],
            [[{integer,0,7},{integer,0,0}],
             [{integer,0,15},{op,0,'/',{atom,0,false},{atom,0,thing}}]],
            [{integer,0,0}]}]},
         {function,0,f2,0,
          [{clause,0,[],[],
            [{lc,0,
              {string,0,"cCXF"},
              [{atom,0,fact},
               {b_generate,0,
                {bin,0,
                 [{bin_element,0,
                   {op,0,'-',
                    {integer,0,0},
                    {op,0,'rem',{integer,0,3},{integer,0,1}}},
                   {integer,0,4},
                   [integer,{unit,7},unsigned,big]},
                  {bin_element,0,
                   {integer,0,8},
                   {integer,0,3},
                   [integer,{unit,8},signed,native]}]},
                {bin,0,
                 [{bin_element,0,
                   {integer,0,0},
                   {float,0,1.0910950854252375},
                   [unsigned]},
                  {bin_element,0,
                   {atom,0,true},
                   {atom,0,story},
                   [bits]}]}}]},
             {nil,0}]}]}].
Opts = [].
typechecker:type_check_forms(Forms, Opts).


{{user_type,0,t2,[{user_type,0,t2,[{type,0,map,any}]}]},
 {type,0,map,
  [{type,0,map_field_assoc,
    [{atom,0,any_atom},
     {user_type,0,t2,[{type,0,nil,[]}]}]}]}}

Env = test_lib:create_env("-type t2(A1) :: t2(t2(t2(t2(map())))) | A1.", []).
Ty1 = {user_type,0,t2,[{user_type,0,t2,[{user_type,0,t2,[{user_type,0,t2,[{user_type,0,t2,[{type,0,map,any}]}]}]}]}]}.
Ty2 = {type,0,map,[{type,0,map_field_assoc,[{atom,0,any_atom},{user_type,0,t2,[{type,0,nil,[]}]}]}]}.
typechecker:subtype(Ty1, Ty2, Env).

EnvFile = "/Users/erszcz/work/erszcz/gradualizer/test/should_pass/minimised_abstract_type_should_pass.erl".
application:ensure_all_started(gradualizer).
gradualizer_db:import_erl_files([EnvFile]).
rr(typechecker).
Env = test_lib:create_env_from_file(EnvFile, []).
Type = fun (T) -> typelib:remove_pos(typelib:parse_type(T)) end.
AbsTy = Type("type()").
N1 = typechecker:normalize(AbsTy, Env).
N2 = typechecker:normalize(N1, Env).
N3 = typechecker:normalize(N2, Env).
N4 = typechecker:normalize(N3, Env).
N5 = typechecker:normalize(N4, Env).
N6 = typechecker:normalize(N5, Env).
N7 = typechecker:normalize(N6, Env).
N8 = typechecker:normalize(N7, Env).
[N1 == N2, N2 == N3, N3 == N4, N4 == N5, N5 == N6, N6 == N7, N7 == N8].

application:ensure_all_started(gradualizer).
TEnv = "
-type t1() :: {}.
-type t2(A1) :: t2(type_variable) | A1.
".
Env = test_lib:create_env(TEnv, []).
{T1, T2} = {{user_type,0,t2,[{user_type,0,t2,[{var,0,type_variable}]}]},{type,0,union,[{user_type,0,t2,[{user_type,0,t1,[]}]},{user_type,0,t2,[{type,0,union,[{user_type,0,t1,[]},{var,0,type_variable},{user_type,0,t1,[]}]}]},{type,0,'fun',[{type,0,product,[{user_type,0,t1,[]}]},{type,0,range,[{integer,0,4},{integer,0,3}]}]}]}}.
Check = fun (T) ->
        N1 = typechecker:normalize(T, Env),
        N2 = typechecker:normalize(N1, Env),
        N3 = typechecker:normalize(N2, Env),
        N4 = typechecker:normalize(N3, Env),
        N5 = typechecker:normalize(N4, Env),
        N6 = typechecker:normalize(N5, Env),
        N7 = typechecker:normalize(N6, Env),
        N8 = typechecker:normalize(N7, Env),
        [N1 == N2, N2 == N3, N3 == N4, N4 == N5, N5 == N6, N6 == N7, N7 == N8]
end.
Check(T1).
Check(T2).
typechecker:subtype(T1, T2, Env).
