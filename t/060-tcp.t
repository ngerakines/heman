#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../heman ./ebin -boot start_sasl -sasl errlog_type error

-include_lib("heman/include/heman.hrl").

main(_) ->
    etap:plan(15),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        etap:is(heman:rule_set({<<"harvest">>, <<"grain_harvested">>}, increase), ok, "rule 1 created"),
        etap:is(heman:rule_set({<<"harvest">>, <<"rice_harvested">>}, increase), ok, "rule 2 created"),
        Rules = [
            {rule,{<<"harvest">>,<<"grain_harvested">>},increase, undefined},
            {rule,{<<"harvest">>,<<"rice_harvested">>},increase, undefined}
        ],
        etap:is(lists:sort(heman:rule_get()), lists:sort(Rules), "rules exist"),
        ok
    end)(),
    
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

    etap:is(erlmc:start(), ok, "erlmc connect to default memcached server ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<1>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<3>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<2>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<4>>), <<>>, "set ok"),
    {H, M, _} = time(),
    etap:is(lists:sort(heman:stat_get()), [
        {stat,{date(),{H,M,0},<<"harvest">>,<<"grain_harvested">>}, {date(),{H,M,0}},<<"harvest">>,<<"grain_harvested">>,10}
    ], "stats exist"),
    etap:is(erlmc:stats(), [{{"localhost",11211},[{harvest,"30"}]}], "stats ok"),
    etap:is(erlmc:quit(), [{{"localhost",11211},[true]}], "quit ok"),

    ok.
