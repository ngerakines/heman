#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../heman ./ebin -boot start_sasl -config heman -sasl errlog_type error

-include_lib("heman/include/heman.hrl").

main(_) ->
    etap:plan(36),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"action">>}, increase), ok, "rule 1 created"),
        etap:is(heman:add_rule({<<"movies">>, <<"comedy">>}, increase), ok, "rule 2 created"),
        Rules = [
            {rule,{<<"movies">>,<<"action">>},increase},
            {rule,{<<"movies">>,<<"comedy">>},increase}
        ],
        etap:is(heman:rules(), Rules, "rules exist"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:set(<<"movies">>, <<"action">>, 1), ok, "rule 1 executed"),
        etap:is(heman:set(<<"movies">>, <<"comedy">>, 1), ok, "rule 2 executed"),
        etap:is(heman:set(<<"movies">>, <<"comedy">>, 1), ok, "rule 2 executed (again)"),
        Stats = [{<<"movies">>, <<"comedy">>, 2}, {<<"movies">>, <<"action">>, 1}],
        Results = [
            {stat,{date(),time(),<<"movies">>,<<"action">>},{date(),time()},<<"movies">>,<<"action">>,1},
            {stat,{date(),time(),<<"movies">>,<<"comedy">>},{date(),time()},<<"movies">>,<<"comedy">>,2}
        ],
        etap:is(lists:sort(heman:stats()), lists:sort(Results), "stats exist"),
        etap:is((hd(heman:get(<<"movies">>, <<"comedy">>)))#stat.value, 2, "getting value of rule 2"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:set(<<"movies">>, <<"drama">>, 4), ok, "new stat, rule 3"),
        etap:is(heman:set(<<"movies">>, <<"drama">>, 1), ok, "new stat, rule 3"),
        etap:is((hd(heman:get(<<"movies">>, <<"drama">>)))#stat.value, 5, "getting value of rule 3"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"crime">>}, larger), ok, "rule 4 created"),
        etap:is(heman:set(<<"movies">>, <<"crime">>, 5), ok, "executing against rule 4"),
        etap:is(heman:set(<<"movies">>, <<"crime">>, 3), ok, "executing against rule 4"),
        etap:is((hd(heman:get(<<"movies">>, <<"crime">>)))#stat.value, 5, "getting value of rule 4"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"documentary">>}, smaller), ok, "rule 5 created"),
        etap:is(heman:set(<<"movies">>, <<"documentary">>, 3), ok, "executing against rule 5"),
        etap:is(heman:set(<<"movies">>, <<"documentary">>, 5), ok, "executing against rule 5"),
        etap:is((hd(heman:get(<<"movies">>, <<"documentary">>)))#stat.value, 3, "getting value of rule 5"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"western">>}, replace), ok, "rule 6 created"),
        etap:is(heman:set(<<"movies">>, <<"western">>, 1), ok, "executing against rule 6"),
        etap:is(heman:set(<<"movies">>, <<"western">>, 9), ok, "executing against rule 6"),
        etap:is(heman:set(<<"movies">>, <<"western">>, 7), ok, "executing against rule 6"),
        etap:is((hd(heman:get(<<"movies">>, <<"western">>)))#stat.value, 7, "getting value of rule 6"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"horror">>}, fun(A,B) -> A*B end), ok, "rule 7 created"),
        etap:is(heman:set("movies", <<"horror">>, 2), ok, "executing against rule 7"),
        etap:is(heman:set(<<"movies">>, "horror", 3), ok, "executing against rule 7"),
        etap:is((hd(heman:get(<<"movies">>, <<"horror">>)))#stat.value, 6, "getting value of rule 7"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"animation">>}, {erlang, max}), ok, "rule 8 created"),
        etap:is(heman:set(<<"movies">>, <<"animation">>, 3), ok, "executing against rule 8"),
        etap:is(heman:set(<<"movies">>, <<"animation">>, 5), ok, "executing against rule 8"),
        etap:is((hd(heman:get(<<"movies">>, <<"animation">>)))#stat.value, 5, "getting value of rule 8"),
        ok
    end)(),

    pg2:get_closest_pid(heman_db) ! marco,

    etap:end_tests().
