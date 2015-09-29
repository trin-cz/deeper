%%%-------------------------------------------------------------------
%%% @author Martin "Trin" Kudlvasr <martin.kudlvasr@uxvibe.com>
%%% @copyright (C) 2015, Martin Kudlvasr
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2015 10:27
%%%-------------------------------------------------------------------
-module(deeper_maps).
-author("martin.kudlvasr@uxvibe.com").

-export([ map/3
        , get/2
        , set/3
        , keys_to_binaries/1
        ]).

%% maps function F to all values in Map matching Path
%%
%% we have reached the Path end, apply function F here
map(F, [], Value) ->
  F(Value)
;
%% we have reached a list in Map, but path is integer => apply to indexed item in list
map(F, [Index|Path], List) when is_list(List) and is_integer(Index) ->
  Value0 = lists:nth(Index, List),
  Value1 = map(F, Path, Value0),
  lists:append([ lists:sublist(List, Index-1)
               , [Value1]
               , lists:nthtail(Index, List) ])
;
%% we have reached a split/list in Map, apply to all items in list
map(F, Path       , List ) when is_list(List) ->
  [ map(F, Path, Value) || Value <- List ]
;
%% we have reached Map object, try to go 1 level deeper
map(F, [Breadcrumb|Tail], Map) when is_map(Map) ->
  case maps:find(Breadcrumb, Map) of
    error ->
      case Tail of
        [] -> maps:put(Breadcrumb, F(undefined), Map); %% last segment of path, if the F is setter, it works
        _  -> Map %% Path not applicable to this part of Map => do nothing
      end;
    {ok, Value0} ->
       Value1 = map(F, Tail, Value0),
       maps:update(Breadcrumb, Value1, Map)
  end
;
map(_, _Path, Value) ->
  Value %% path not applicable to this part of Map => return the original
.

get([]               , Value)                    ->
  [Value]
;
get([Index|Path]     , List ) when is_list(List) and is_integer(Index) ->
  Value = lists:nth(Index, List),
  get(Path, Value)
;
%% we have reached a split/list in Map, get all items in list
get(Path             , List ) when is_list(List) ->
  lists:append([ get(Path, Value) || Value <- List ])
;
get([Breadcrumb|Tail], Map  ) when is_map(Map)   ->
  case maps:find(Breadcrumb, Map) of
    error       ->
      [];
    {ok, Value} ->
      get(Tail, Value)
  end
;
get(_Path, _Value) ->
  [] %% path not applicable to this part of deep map
.

set(NewValue, Path, Map) when (is_map(Map) or is_list(Map)) and is_list(Path) ->
  map(fun(_) -> NewValue end, Path, Map).

keys_to_binaries(List) when is_list(List) ->
  [ keys_to_binaries(Value) || Value <- List ]
;
keys_to_binaries(Map) when is_map(Map) ->
  maps:from_list([  { erlang:list_to_binary(erlang:atom_to_list(Key))
                    , keys_to_binaries(Value)}
                 || {Key, Value} <- maps:to_list(Map) ])
;
keys_to_binaries(Value) ->
  Value.

