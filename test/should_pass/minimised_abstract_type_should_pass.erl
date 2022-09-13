-module(minimised_abstract_type_should_pass).

%% This is an example taken from Gradualizer code itself exposed by running 'make gradualize'.

-type abstract_type() :: af_annotated_type()
                       | af_atom()
                       | af_user_defined_type().

-type af_annotated_type() ::
        {'ann_type', anno(), [af_anno() | abstract_type()]}. % [Var, Type]

-type af_variable() :: {'var', anno(), atom()}. % | af_anon_variable()

-type af_anno() :: af_variable().

-type af_integer() :: {'integer', anno(), non_neg_integer()}.

-type af_character() :: {'char', anno(), char()}.

-type af_unary_op(T) :: {'op', anno(), unary_op(), T}.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

-type af_binary_op(T) :: {'op', anno(), binary_op(), T, T}.

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/='.

-type af_singleton_integer_type() :: af_integer()
                                   | af_character()
                                   | af_unary_op(af_singleton_integer_type())
                                   | af_binary_op(af_singleton_integer_type()).

-type af_atom() :: af_lit_atom(atom()).

-type af_lit_atom(A) :: {'atom', anno(), A}.

-type type_name() :: atom().

-type anno() :: erl_anno:anno().

-type af_user_defined_type() ::
        {'user_type', anno(), type_name(),  [abstract_type()]}.

-type type() :: abstract_type().

-spec annotate_user_type() -> type().
annotate_user_type() ->
    annotate_user_type_().

-spec annotate_user_type_() -> type().
annotate_user_type_() ->
    %% Annotate local user-defined type.
    {user_type, erl_anno:new(0), asd, []}.
