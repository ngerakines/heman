-module(heman_db).

-include("heman.hrl").

%% exports: supervisor
-export([start/1, init/2, server_loop/1]).
%% export: functionality
-export([set/3]).

start(Rules) ->
    proc_lib:start(heman_db, init, [self(), Rules]).

init(Parent, Rules) when is_list(Rules) ->
    pg2:create(Name),
    pg2:join(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    heman_db:server_loop(#state{ config = Rules }).

server_loop(State) ->
    receive
        {'$heman_db_server', From, {set, Namespace, Key, Value}} ->
            %% Create a composite key based on date/time, namespace and key name
            %% NKG: Hash it? term_to_binary/1?
            DBKey = {date(), time(), Namespace, Key},
            %% Determine if an entry exists
            case ets:lookup(statmaster_db_table, DBKey) of
                [] ->
                    %% if not, insert and move on
                    ets:insert(statmaster_db_table, {DBKey, Value});
                [{DBKey, OldValue}] ->
                    NewValue = bump_data(State, DBKey, OldValue, Value),
                    ets:insert(statmaster_db_table, {DBKey, NewValue});
                _ -> no_op
            end,
            gen:reply(From, ok);
        {'$heman_db_server', From, {get, Namespace, Key}} ->
            Results = ets:lookup(statmaster_db_table, DBKey)
            gen:reply(From, Results);
        Other ->
            error_logger:warning_report({?MODULE, ?LINE, unexpected_message, Other})
    end,
    heman_db:server_loop(State).

bump_data(State, {_, _, Namespace, Key}, OldValue, NewValue) ->
    case lists:keysearch({Namespace, Key}, 2, State#state.config) of
        false ->
            OldValue + NewValue;
        {value, {{Namespace, Key}, Rule}} ->
            case Rule of
                replace -> NewValue;
                increase -> OldValue + NewValue;
                {M, F} ->
                    apply(M, F, [OldValue, NewValue]);
                Fun ->
                    Fun(OldValue, NewValue)
            end
    end.

set(N, K, V) when is_list(N) -> set(list_to_binary(N), K, V);
set(N, K, V) when is_list(K) -> set(N, list_to_binary(K), V);
set(N, K, V) when is_list(V) -> set(N, K,list_to_binary(V));
set(Namespace, Key, Value) ->
    gen:call(global:whereis_name(heman_db), '$heman_db_server', {set, Namespace, Key, Value}, 5000).

get(Name, Key) ->
    gen:call(global:whereis_name(heman_db), '$heman_db_server', {get, Namespace, Key}, 5000).
