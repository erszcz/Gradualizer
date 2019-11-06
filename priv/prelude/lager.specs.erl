-module(lager).

-spec lager:debug(string(), list()) -> ok.
-spec lager:info(string(), list()) -> ok.
-spec lager:warning(string(), list()) -> ok.
-spec lager:error(string(), list()) -> ok.
-spec lager:critical(string(), list()) -> ok.
