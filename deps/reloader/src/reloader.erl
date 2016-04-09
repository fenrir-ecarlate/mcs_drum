%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

% Start/stop API.
-export([start/0, start/1, start_link/0, start_link/1, stop/0]).

% Reloader-specific API.
-export([list_changed/0, scan/0]).

% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {last, tref}).

%%-define(DBG(Fmt), io:format(Fmt)).
%%-define(DBG(Fmt, Args), io:format(Fmt, Args)).
-define(DBG(Fmt), ok).
-define(DBG(Fmt, Args), ok).


%% ===================================================================
%% Start/stop API.
%% ===================================================================

%% @doc Start the reloader with default poll interval (1 second).
start() ->
    start(1).

%% @doc Start the reloader with the spefied poll interval in seconds.
%% Entry point from the command-line: `erl -run reloader start N`.
start([N|_]) ->
    start(list_to_integer(N));
%% @doc Start the reloader with the spefied poll interval in seconds.
start(PollInterval) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [PollInterval], []).


%% @doc Start the reloader with default poll interval (1 second).
start_link() ->
    start_link(1).

%% @doc Start the reloader with the spefied poll interval in seconds.
start_link(PollInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PollInterval], []).

%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).


%% ===================================================================
%% Reloader-specific API.
%% ===================================================================

%% @doc Return a list of beam modules that have changed.
list_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @doc Trigger scan.
scan() ->
    ?MODULE ! doit.


%% ===================================================================
%% gen_server callbacks.
%% ===================================================================

%% @spec init([]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([PollInterval]) ->
    ?DBG("init ~p~n", [PollInterval]),
    {ok, TRef} = timer:send_interval(timer:seconds(PollInterval), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions.
%% ===================================================================

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
            false
    end.

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    ?DBG("reloader: doit ~p ~p ~n", [From, To]),
    [case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
             reload(Module);
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             io:format("Error reading ~s's file info: ~p~n",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
    io:format("reloader: reloading ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            io:format(" ok.~n"),
            case erlang:function_exported(Module, test, 0) of
                true ->
                    io:format(" - Calling ~p:test() ...", [Module]),
                    case catch Module:test() of
                        ok ->
                            io:format(" ok.~n"),
                            reload;
                        Reason ->
                            io:format(" fail: ~p.~n", [Reason]),
                            reload_but_test_failed
                    end;
                false ->
                    reload
            end;
        {error, Reason} ->
            io:format(" fail: ~p.~n", [Reason]),
            error
    end.

stamp() ->
    erlang:localtime().
