#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../heman ./ebin -boot start_sasl -sasl errlog_type error

-include_lib("heman/include/heman.hrl").

main(_) ->
    etap:plan(14),
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

    etap:is(erlmc:start(), ok, "erlmc connect to default memcached server ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<1>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<3>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<2>>), <<>>, "set ok"),
    etap:is(erlmc:set(<<"harvest:grain_harvested">>, <<4>>), <<>>, "set ok"),
    etap:is(erlmc:stats(), [{{"localhost",11211}, []}], "stats ok"),
    etap:is(erlmc:quit(), [{{"localhost",11211},[true]}], "quit ok"),

    ok.
