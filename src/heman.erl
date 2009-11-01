-module(heman).

-behaviour(application).

%% exports: Application behvior
-export([start/2, stop/1, init/1]).
%% exports: build, misc
-export([env_key/1, env_key/2, build_rel/0, reload/0]).

-include("heman.hrl").

%% NKG: Before starting, it's a good idea to net_adm:world(), pg2:which_groups() and global:sync()
start(_, _) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) ->  ok.

start_phase(mnesia, _, _) ->
    mnesia:create_schema([node()]),
    case mnesia:system_info(tables) of
        [schema] ->
            mnesia:create_table(rules, [{record_name, user}, {attributes, record_info(fields, user)}, {index, [key]}]),
            mnesia:create_table(stats, [{attributes, record_info(fields, user)}, {index, []}]),
            ok;
        _ ->
            ok
    end,
    mnesia:wait_for_tables([user, day], 5000),
    ok;

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

