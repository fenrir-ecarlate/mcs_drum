-module(test_tools).

-export([
    decorate_test/3,
    timing/4]).

-include_lib("eunit/include/eunit.hrl").


%% ===========================================================================
%% API.
%% ===========================================================================

% Take a test of the form { "test description", test-asserts } and return
% { "test description function_name", test-asserts }
decorate_test(T, M, F) ->
    {Name, Assert} = T(fun M:F/1),
    {Name ++ " (" ++ atom_to_list(F) ++ ")", Assert}.

% Execute function M:F(A) N times and return a tuple containing timing
% information.
% WARNING: timing varies a lot! Use at least 100 iterations!
% Based on https://erlangcentral.org/wiki/index.php/Measuring_Function_Execution_Time
timing(M, F, A, N) when N > 0 ->
    garbage_collect(),
    L = timing_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    {{unit, usec}, {runs, Length}, {min, Min}, {max, Max}, {med, Med}, {avg,
        Avg}}.


%% ===========================================================================
%% Internals.
%% ===========================================================================

timing_loop(_M, _F, _A, 0, List) ->
    List;
timing_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    timing_loop(M, F, A, N - 1, [T|List]).


%% ===========================================================================
%% Tests
%% ===========================================================================

