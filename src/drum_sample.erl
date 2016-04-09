%%%-------------------------------------------------------------------
%%% @author Domingues Pedrosa João Miguel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2016 08:56
%%%-------------------------------------------------------------------
-module(drum_sample).
-author("fenrir").

%% API
-export([render_file/1,decode_file/1]).
-ifdef(TEST).
-export([
  binary_to_string/1,
  parse_header/1,
  parse_tracks/1,
  parse_measure/1,
  render/3,
  render_tracks/2]).
-endif.

%%=========================================
%% API
%%=========================================
render_file(File) ->
  Res = decode_file(File),
  case Res of
    {ok, Version, Tempo, Tracks} ->
      {ok, render(Version, Tempo, Tracks)};
    _ ->Res
  end.

decode_file(File) ->
  Res=file:read_file(File),
  case Res of
    {ok, Bin} ->
      {_,Version,Tempo,Rest} = parse_header(Bin),
      {_, Tracks} = parse_tracks(Rest),
      {ok, Version, Tempo, Tracks};
    _ -> Res
  end.

%%=========================================
%% Internal
%%=========================================
-define(MAGIC, "SPLICE").
-define(VERSION, "0.808-alpha").

binary_to_string(Bin) ->
  lists:filter(fun(E) -> E =/= 0 end, binary_to_list(Bin)).

%%------------------

parse_header(Bin) ->
  case Bin of
    <<?MAGIC, Length:64, Payload:Length/binary, _/binary>> ->
      <<HW_version:32/binary, Tempo:32/little-float, Rest/binary>> = Payload,
      {ok, binary_to_string(HW_version), Tempo, Rest};
    _ -> {error, parse_header, Bin}
   end.

%%------------------

parse_tracks(Bin) ->
  {ok,parse_tracks(Bin,[])}.

parse_tracks(<<>>,Acc) ->
  lists:reverse(Acc);

parse_tracks(<<TrackN:8, NameLen:32, Instrument:NameLen/binary, Measure:16/binary, Rest/binary>>,Acc) ->
  parse_tracks(Rest,[{TrackN, binary_to_list(Instrument), parse_measure(Measure)}|Acc]).

%%------------------

parse_measure(Bin) ->
  parse_measure(Bin,[]).

parse_measure(<<>>,Acc) ->
  lists:reverse(Acc);

parse_measure(<<Quarter:4/binary, Rest/binary>>, Acc) ->
  case Quarter of
    <<A:8,B:8,C:8,D:8>> when A < 2, B < 2, C < 2, D < 2 ->
      parse_measure(Rest, [binary_to_list(Quarter)|Acc]);
    _ -> {parse_measure, bad_value, Quarter}
  end.

%%------------------

render(Version, Tempo, Tracks) ->
  io_lib:format("Saved with HW Version: ~s~nTempo: ~s~n~s",
    [Version,render_float(Tempo),render_tracks(Tracks, find_padding(Tracks))]).

%%------------------

render_tracks(Tracks,Padding) ->
  MaxLenInst = lists:foldl(
    fun({_, Instrument, _},Max) ->
      case length(Instrument) > Max of
        true -> length(Instrument);
        false -> Max
      end
    end,
    0,Tracks),

  MaxTrackN = lists:foldl(
    fun({TrackN, _, _},Max) ->
      case TrackN > Max of
        true -> TrackN;
        false -> Max
      end
    end,
    0,Tracks),

  render_tracks(Tracks,
    Padding + MaxLenInst + length(integer_to_list(MaxTrackN))
    ,[]).

render_tracks([],_B,Acc) ->
  lists:reverse(Acc);

render_tracks([{TrackN, Instrument, Measure}|Rest],Padding, Acc) ->
  render_tracks(Rest,Padding,
    [io_lib:format("(~p) ~s ~s~s~n",
    [TrackN,Instrument,lists:duplicate(Padding-length(Instrument)-length(integer_to_list(TrackN)),32),render_quarter(Measure)])|Acc]).

%%------------------
render_quarter(Measure) ->
  render_quarter(Measure,[$|]).

render_quarter([],Acc) ->
  lists:reverse(Acc);

render_quarter([Quarter|Rest], Acc) ->
  render_quarter(
    Rest,
    [[case E of 1 -> "x"; 0-> "-" end || E <- Quarter]++"|" | Acc]).

%-------------------
render_float(Float) ->
  case Float - trunc(Float) > 0 of
    true  -> float_to_list(Float, [{decimals,3},compact]);
    false -> float_to_list(Float, [{decimals,0}])
  end.

%-------------------
find_padding([]) ->
  0;

find_padding([{_, Instrument, _}|TTracks]) ->
  case Instrument of
    %% Check that the first character is capitalized
    [First|_] when First >= $A, First =< $Z -> 2;
    _ -> find_padding(TTracks)
  end.