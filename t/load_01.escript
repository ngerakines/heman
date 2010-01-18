#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name loader@127.0.0.1 -pa ../heman ./ebin -boot start_sasl

-include_lib("heman/include/heman.hrl").

main(_) ->
    [application:start(X) || X <- [sasl, mnesia, crypto, heman]],

    heman:rule_set({<<"harvest">>, <<"grain_harvested">>}, increase),
    heman:rule_set({<<"harvest">>, <<"rice_harvested">>}, increase),

    heman:health_set(<<"harvest">>, 1, <<"grain_harvested">>, [
        {{hours, 6, sum}, {under, 500}, {decrease, 10}},
        {{hours, 6, sum}, {under, 200}, {decrease, 50}},
        {{hours, 6, sum}, {over, 2000}, {increase, 10}},
        {{hours, 6, sum}, {over, 5000}, {increase, 30}}
    ]),
    heman:health_set(<<"harvest">>, 2, <<"rice_harvested">>, [
        {{hours, 24, sum}, {under, 500}, {decrease, 10}},
        {{hours, 24, sum}, {under, 200}, {decrease, 50}},
        {{hours, 24, sum}, {over, 2000}, {increase, 10}},
        {{hours, 24, sum}, {over, 5000}, {increase, 30}}
    ]),

    erlmc:start(),
    Pid1 = spawn_link(fun() -> write_loop(<<"harvest:grain_harvested">>, 5, 2) end),
    Pid2 = spawn_link(fun() -> write_loop(<<"harvest:rice_harvested">>, 25, 2) end),

    status_loop().

write_loop(Key, Range, Sleep) ->
    receive
        stop -> ok
        after 0 ->
            Data = <<(random:uniform(Range))>>,
            erlmc:set(Key, Data),
            timer:sleep(random:uniform(Sleep) * 1000),
            write_loop(Key, Range, Sleep)
    end.

status_loop() ->
    timer:sleep(1000 * 5),
    status_loop().

