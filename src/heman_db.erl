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
-module(heman_db).

-include("heman.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% exports: supervisor
-export([start/0, init/1, server_loop/0]).

start() ->
    proc_lib:start(heman_db, init, [self()]).

init(Parent) ->
    pg2:create(heman_db),
    pg2:join(heman_db, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    heman_db:server_loop().

%% TODO: Cleanup the receive handle, the patterns aren't organized.
server_loop() ->
    receive
        {'$heman_db_server', From, {health, {get, Namespace}}} ->
            Rules = mnesia:activity(transaction, fun() ->
                qlc:e( qlc:q([R || R <- mnesia:table(health), R#health.namespace == Namespace ]) )
            end),
            SortedRules = lists:sort(
                fun(A, B) -> A#health.priority < B#health.priority end,
                Rules
            ),
            gen:reply(From, SortedRules);
        {'$heman_db_server', From, {health, {set, Namespace, Priority, Key, Rules}}} ->
            mnesia:transaction(fun() -> mnesia:write(#health{
                pkey = {Namespace, Key},
                namespace = Namespace,
                priority = Priority,
                key = Key,
                rules = Rules
            }) end),
            gen:reply(From, ok);
        {'$heman_db_server', From, rules} ->
            Rules = mnesia:activity(transaction, fun() -> qlc:e( qlc:q([R || R <- mnesia:table(rule) ]) ) end),
            gen:reply(From, Rules);
        {'$heman_db_server', From, stats} ->
            Stats = mnesia:activity(transaction, fun() -> qlc:e( qlc:q([R || R <- mnesia:table(stat) ]) ) end),
            gen:reply(From, Stats);
        {'$heman_db_server', From, {add_rule, Key, Rule}} ->
            mnesia:transaction(fun() ->
                mnesia:write(#rule{
                    key = Key,
                    rule = Rule
                })
            end),
            gen:reply(From, ok);
        {'$heman_db_server', From, {set, Namespace, Key, Value}} ->
            %% Create a composite key based on date/time, namespace and key name
            %% NKG: Hash it? term_to_binary/1?
            DBKey = {date(), time(), Namespace, Key},
            mnesia:transaction(fun() ->
                %% Determine if an entry exists
                case mnesia:wread({stat, DBKey}) of
                    [OldStat] ->
                        NewValue = bump_data(mnesia:wread({rule, {Namespace, Key}}), DBKey, OldStat#stat.value, Value),
                        mnesia:write(OldStat#stat{ value = NewValue });
                    [] ->
                        %% if not, insert and move on
                        mnesia:write(#stat{
                            pkey = DBKey,
                            fordate = {date(), time()},
                            namespace = Namespace,
                            key = Key,
                            value = Value
                        })
                end
            end),
            gen:reply(From, ok);
        {'$heman_db_server', From, {get, Namespace, Key}} ->
            Stats = mnesia:activity(transaction, fun() ->
                qlc:e( qlc:q([R || R <- mnesia:table(stat), R#stat.namespace == Namespace, R#stat.key == Key ]) )
            end),
            gen:reply(From, Stats);
        Other ->
            error_logger:warning_report({?MODULE, ?LINE, unexpected_message, Other})
    end,
    heman_db:server_loop().

bump_data(Rules, {_, _, Namespace, Key}, OldValue, NewValue) ->
    case lists:keysearch({Namespace, Key}, 2, Rules) of
        false ->
            OldValue + NewValue;
        {value, Rule} when is_record(Rule, rule) ->
            case Rule#rule.rule of
                larger -> case NewValue > OldValue of true -> NewValue; _ -> OldValue end;
                smaller -> case NewValue < OldValue of true -> NewValue; _ -> OldValue end;
                replace -> NewValue;
                {M, F} ->
                    apply(M, F, [OldValue, NewValue]);
                Fun when is_function(Fun) ->
                    Fun(OldValue, NewValue);
                _ -> OldValue + NewValue
            end
    end.
