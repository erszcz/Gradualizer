-module(poly1).

%%% good
%-spec f([integer()]) -> [integer()].
%f(L) ->
%    lists:map(fun (I) -> I * 2 end, L).

%% bad
-spec g([integer()]) -> [atom()].
g(L) ->
    lists:map(fun (I) -> I * 2 end, L).

%%% bad
%-spec h([string()]) -> [integer()].
%h(L) ->
%    map(fun (I) -> I * 2 end, L).

%%% false negative, i.e. there's an undetected bug here
%-spec i([binary() | integer()]) -> [integer()].
%i(L) ->
%    map(fun
%            (I) when is_integer(I) -> I * 2;
%            (B) when is_list(B) -> list_to_integer(B)
%        end, L).

%%% false negative, i.e. there's an undetected bug here
%-spec j([binary() | integer()]) -> [integer()].
%j(L) ->
%    map(fun
%            (I) when is_integer(I) -> I * 2
%        end, L).

%%% good
%-spec k([binary() | integer()]) -> [integer()].
%k(L) ->
%    map(fun
%            (I) when is_integer(I) -> I * 2;
%            (B) when is_binary(B) -> binary_to_integer(B)
%        end, L).
