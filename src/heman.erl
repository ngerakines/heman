%% Copyright (c) 2009 Nick Gerakiens <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(heman).
-behaviour(application).

%% exports: Application behvior, supervisor
-export([start/2, stop/1, start_phase/3, init/1]).
%% exports: build, misc
-export([env_key/1, env_key/2, build_rel/0, reload/0]).
%% exports: interface
-export([set/3, get/2, add_rule/2, rules/0, stats/0, health/1]).
-export([add_health_rule/4, health_rules/1]).

-include("heman.hrl").

%% NKG: Before starting, it's a good idea to net_adm:world(), pg2:which_groups() and global:sync()
start(_, _) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) ->  ok.

start_phase(mnesia, _, _) ->
    mnesia:create_schema([node()]),
    mnesia:create_table(rule, [{record_name, rule}, {attributes, record_info(fields, rule)}, {index, []}]),
    mnesia:create_table(stat, [{record_name, stat}, {attributes, record_info(fields, stat)}, {index, [fordate, namespace, key]}]),
    mnesia:create_table(health, [{record_name, health}, {attributes, record_info(fields, health)}, {index, [namespace]}]),
    mnesia:wait_for_tables([rule, stat], 5000),
    ok.

init(_) ->
    Rules = [#rule{ key = Key, rule = Rule} || {Key, Rule} <- env_key(rules, [])],
    {ok, {{one_for_one, 10, 10}, [
        {heman_db, {heman_db, start, [Rules]}, permanent, 5000, worker, [heman_db]},
        {heman_web, {heman_web, start, []}, permanent, 5000, worker, [heman_web]}
    ]}}.

env_key(Key) -> env_key(Key, undefined).
env_key(Key, Default) ->
    case application:get_env(heman, Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

build_rel() ->
    Apps = [kernel, stdlib, sasl, crypto, inets],
    {ok, FD} = file:open("heman.rel", [write]),
    RelInfo = {release,
        {"heman", "0.0.1"},
        get_app_version(erts), 
            [get_app_version(AppName) || AppName <- Apps] ++ [
            {heman, "0.0.1"}
        ]
    },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("heman", [local]),
    ok.

reload() ->
    Modules = [
        heman,
        heman_db,
        heman_web
    ],
    [begin
        case code:soft_purge(X) of
            true -> ok;
            false -> code:purge(X)
        end,
        code:load_abs("./ebin/" ++ atom_to_list(X))
    end || X <- Modules].

get_app_version(AppName) ->
    case code:lib_dir(AppName) of
        {error, bad_name} -> exit({bad_name, AppName});
        Dir ->
            case lists:reverse(string:tokens(Dir, "-")) of
                [Vsn|_] -> {AppName, Vsn};
                _ -> exit({failed_to_tokenize, Dir})
            end
    end.

set(N, K, V) when is_list(N) -> set(list_to_binary(N), K, V);
set(N, K, V) when is_list(K) -> set(N, list_to_binary(K), V);
set(Namespace, Key, Value) ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', {set, Namespace, Key, Value}, 5000) of
        {ok, ok} -> ok;
        Other -> Other
    end.

get(Namespace, Key) ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', {get, Namespace, Key}, 5000) of
        {ok, Stats} -> Stats;
        Other -> Other
    end.

add_rule(Key, Rule) ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', {add_rule, Key, Rule}, 5000) of
        {ok, ok} -> ok;
        Other -> Other
    end.

rules() ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', rules, 5000) of
        {ok, Results} -> Results;
        Other -> Other
    end.

stats() ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', stats, 5000) of
        {ok, Results} -> Results;
        Other -> Other
    end.

add_health_rule(Namespace, Priority, Key, Rules) ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', {health, {set, Namespace, Priority, Key, Rules}}, 5000) of
        {ok, ok} -> ok;
        Other -> Other
    end.

health_rules(Namespace) ->
    case gen:call(pg2:get_closest_pid(heman_db), '$heman_db_server', {health, {get, Namespace}}, 5000) of
        {ok, Results} -> Results;
        Other -> Other
    end.

health(Namespace) ->
    health_iter(Namespace, health_rules(Namespace), 50).

health_iter(_, [], Acc) -> Acc;
health_iter(Namespace, [Rule | Rules], Acc) ->
    Data = get(Namespace, Rule#health.key),
    NewAcc = health_rule_iter(Namespace, Rule#health.rules, Data, Acc),
    health_iter(Namespace, Rules, NewAcc).

%% What data do you need?
%% What is the condition?
%% What is the result?
health_rule_iter(_, [], _, Acc) -> Acc;
health_rule_iter(Health, [Rule | Rules], Data, Acc) ->
    {DRule, CRule, RRule} = Rule,
    case result(DRule, CRule, RRule, Data) of
        {stop, N} -> Acc + N;
        continue -> health_rule_iter(Health, Rules, Data, Acc)
    end.

%% TODO: Abstrack the data augment code into a different function.
result({hours, Hours, sum}, CRule, RRule, Data) ->
    StopDate = calendar:datetime_to_gregorian_seconds({date(), time()}) - (60 * 60 * Hours),
    SegmentedData = lists:filter(
        fun(X) -> calendar:datetime_to_gregorian_seconds(X#stat.fordate) >= StopDate end,
        Data
    ),
    Sum = lists:foldl(fun(X, Acc) -> Acc + X#stat.value end, 0, SegmentedData),
    case CRule of
        {over, Amount} when Sum > Amount -> apply_rule(RRule);
        {under, Amount} when Sum < Amount -> apply_rule(RRule);
        _ -> continue
    end.

apply_rule({increase, N}) -> {stop, N};
apply_rule({decrease, N}) -> {stop, -N}.
