%%%%-------------------------------------------------------------------
%%% @author Martin "Trin" Kudlvasr <martin.kudlvasr@uxvibe.com>
%%% @copyright (C) 2015, Martin Kudlvasr
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2015 10:27
%%%-------------------------------------------------------------------
-module(deeper_ejson).
-author("martin.kudlvasr@uxvibe.com").

%% API

-export([ map/3
        , get/2
        , set/3
        , keys_to_binaries/1
        , from_deep_map/1
        ]).

%% maps function F to all values in EJson matching Path
%%
%% we have reached the Path end, apply function F here
map(F, []               , Value)                    ->
  F(Value)
;
%% we have reached a list in EJson, but path is integer => apply to indexed item in list
map(F, [Index|Path]     , List ) when is_list(List) and is_integer(Index) ->
  Value0 = lists:nth(Index, List),
  Value1 = map(F, Path, Value0),
  lists:append([ lists:sublist(List, Index-1)
               , [ Value1 ]
               , lists:nthtail(Index, List) ]);
%% we have reached a split/list in EJson, apply to all items in list
map(F, Path             , List ) when is_list(List) ->
  [ map(F, Path, Value) || Value <- List ];
%% we have reached EJson object, try to go 1 level deeper
map(F, [Breadcrumb0|Tail], {KVs0})                    ->
  case lists:keysearch(Breadcrumb0, 1, KVs0) of
    {value, {Breadcrumb0, Value0}} ->
      Value1 = map(F, Tail, Value0),
      {lists:keyreplace(Breadcrumb0, 1, KVs0, {Breadcrumb0, Value1})};
    false ->
      {KVs0}

  end
;
map(_F, _Path, EJson) -> EJson.

get([]               , Value)                   -> [Value]
;
%% we have reached a list in EJson, but path is integer => apply to indexed item in list
get([Index|Path]     , List ) when is_list(List) and is_integer(Index) ->
  Value = lists:nth(Index, List),
  get(Path, Value)
;
get(Path             , List) when is_list(List) ->
  lists:append([ get(Path, Value) || Value <- List])
;
get([Breadcrumb0|Tail], {KVs0})                  ->
  %% keys can be either atom or binary, breadcrumb is always binary
  %% lager:debug("Diving into: ~p", [Breadcrumb]),
  case lists:keysearch(Breadcrumb0, 1, KVs0) of
    {value, {Breadcrumb0, Value0}} ->
      get(Tail, Value0);
    false ->
      [] %% Path is not applicable to this part of EJson => return nothing
  end
;
get(_Path, _Value) ->
  [] %% Path is not applicable to this part of EJson => return nothing
.

set(NewValue, Path, EJson) ->
  map(fun(_) -> NewValue end, Path, EJson).

keys_to_binaries(List) when is_list(List) ->
  [ keys_to_binaries(Value) || Value <- List ]
;
keys_to_binaries({EJson}) ->
  {[  { erlang:list_to_binary(erlang:atom_to_list(Key))
      , keys_to_binaries(Value)}
   || {Key, Value} <- EJson ]}
;
keys_to_binaries(Value) ->
  Value.

from_deep_map(Map) when is_map(Map) ->
  {maps:to_list( maps:map( fun(_K,V) -> from_deep_map(V) end, Map))};
from_deep_map(List) when is_list(List) ->
  [ from_deep_map(Term) || Term <- List ];
from_deep_map(Term) -> Term.