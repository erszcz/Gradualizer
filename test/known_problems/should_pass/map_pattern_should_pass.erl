-module(map_pattern_should_pass).

-export([f/1]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> boolean().
f(#{bepa := Bepa}) ->
    Bepa.
