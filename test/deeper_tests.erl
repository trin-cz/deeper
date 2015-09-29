%%%%-------------------------------------------------------------------
%%% @author Martin "Trin" Kudlvasr <martin.kudlvasr@uxvibe.com>
%%% @copyright (C) 2015, Martin Kudlvasr
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2015 10:27
%%%-------------------------------------------------------------------
-module(deeper_tests).
-author("martin.kudlvasr@uxvibe.com").

-include_lib("eunit/include/eunit.hrl").

maps1_test_() ->
  Orig_A = #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
  Orig_B = #{<<"a">> =>
             [ undefined
             , #{ <<"c">> => 3 }]},
  [ ?_assertEqual( deeper_maps:keys_to_binaries( #{ a => 1, b => 2, c => 3 })
                 , Orig_A )
  , ?_assertEqual( deeper_maps:get([<<"a">>,1,<<"c">>], Orig_B)
                 , [])
  , ?_assertEqual( deeper_maps:get([<<"a">>,2,<<"c">>], Orig_B)
                 , [3])
  , ?_assertEqual( deeper_maps:set(5, [<<"a">>,1,<<"c">>], Orig_B)
                 , #{<<"a">> => [ undefined , #{ <<"c">> => 3 }]})
  , ?_assertEqual( deeper_maps:set(5, [<<"a">>,2,<<"c">>], Orig_B)
                 , #{<<"a">> => [ undefined , #{ <<"c">> => 5 }]})
  , ?_assertEqual( deeper_maps:map(fun(X) -> X+1 end, [<<"a">>,1,<<"c">>], Orig_B)
                 , #{<<"a">> => [ undefined , #{ <<"c">> => 3 }]})
  , ?_assertEqual( deeper_maps:map(fun(X) -> X+1 end, [<<"a">>,2,<<"c">>], Orig_B)
                 , #{<<"a">> => [ undefined , #{ <<"c">> => 4 }]})
  ].

ejson1_test_() ->
  Orig_A = {[ {<<"a">>,1}, {<<"b">>, 2}, {<<"c">>, 3} ]},
  Orig_B = {[ { <<"a">>
              , [ <<"undefined">>
                , {[{ <<"c">>, 3 }]}]}]},
  [ ?_assertEqual( deeper_ejson:keys_to_binaries( {[{a,1}, {b,2}, {c,3}]})
                 , Orig_A )
  , ?_assertEqual( deeper_ejson:get([<<"a">>,1,<<"c">>], Orig_B)
                 , [])
  , ?_assertEqual( deeper_ejson:get([<<"a">>,2,<<"c">>], Orig_B)
                 , [3])
  , ?_assertEqual( deeper_ejson:set(5, [<<"a">>,1,<<"c">>], Orig_B)
                 , {[{<<"a">>, [ <<"undefined">>, {[{<<"c">>, 3}]}]}]})
  , ?_assertEqual( deeper_ejson:set(5, [<<"a">>,2,<<"c">>], Orig_B)
                 , {[{<<"a">>, [ <<"undefined">>, {[{<<"c">>, 5}]}]}]})
  , ?_assertEqual( deeper_ejson:map(fun(X) -> X+1 end, [<<"a">>,1,<<"c">>], Orig_B)
                 , {[{<<"a">>, [ <<"undefined">>, {[{<<"c">>, 3}]}]}]})
  , ?_assertEqual( deeper_ejson:map(fun(X) -> X+1 end, [<<"a">>,2,<<"c">>], Orig_B)
                 , {[{<<"a">>, [ <<"undefined">>, {[{<<"c">>, 4}]}]}]})
  ].
