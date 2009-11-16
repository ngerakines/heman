-module(heman_tcp).

-behavior(gen_server).

-export([start/0, start/1, start/2, acceptor_loop/1, loop/1]).
-export([init/1, code_change/3, handle_cast/2, terminate/2, handle_info/2, handle_call/3]).
-export([split_key/1]).

-record(state, {port, ip = any, listen = null, acceptor = null, backlog = 30}).
-define(IDLE_TIMEOUT, 30000).
-define(MAX_HEADERS, 1000).

start() ->
    start(any, 11211).

start(Port) ->
    start(any, Port).

start(Ip, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{ ip = Ip, port = Port}, []).

init(State) ->
    process_flag(trap_exit, true),
    Opts = [
        binary, 
        {reuseaddr, true},
        {packet, 0},
        {backlog, State#state.backlog},
        {recbuf, 8192},
        {active, false},
        {nodelay, true},
        inet, {ip, State#state.ip}
    ],
    %% TODO: Handle priviliaged ports.
    gen_tcp_listen(Opts, State).

gen_tcp_listen(Opts, State) ->
    case gen_tcp:listen(State#state.port, Opts) of
        {ok, Listen} ->
            {ok, ListenPort} = inet:port(Listen),
            {ok, new_acceptor(State#state{ listen = Listen, port = ListenPort })};
        {error, Reason} ->
            {stop, Reason}
    end.

code_change(_OldVsn, State, _Extra) -> State.

handle_cast(stop, State) -> {stop, normal, State};

handle_cast({accepted, _Pid}, State) ->
    {noreply, new_acceptor(State)};

handle_cast(_Message, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

handle_info(_Message, State) -> {noreply, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

new_acceptor(State = #state{listen = Listen}) ->
    Pid = proc_lib:spawn_link(?MODULE, acceptor_loop, [{self(), Listen}]),
    State#state{acceptor = Pid}.

acceptor_loop({Server, Listen}) ->
    case (catch gen_tcp:accept(Listen)) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            loop(Socket);
        {error, closed} -> exit({error, closed});
        Other ->
            error_logger:error_report({?MODULE, ?LINE, accept_fail, Other}),
            exit({error, accept_failed})
    end.

loop(Socket) ->
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Data} ->
            case parse_input(Socket, Data) of
                quit ->
                    gen_tcp:close(Socket),
                    exit(normal);
                stat ->
                    loop(Socket);
                {set, Namespace, Key, Value} ->
                    heman:stat_set(Namespace, Key, Value),
                    loop(Socket)
            end;
        {error, _} = Err ->
            error_logger:error_report({?MODULE, ?LINE, recv_error, Err}),
            ok
    end.

parse_input(Socket, <<16#80:8, Opcode:8, KeySize:16, ExtrasSize:8, 16#00:8, 16#00:16, BodySize:32, 16#00:32, 16#00:64, Body:BodySize/binary>>) ->
    <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>> = Body,
    case Opcode of
        16#01 ->
            ResponseBody = <<Extras:ExtrasSize/binary, Key:KeySize/binary, <<>>/binary>>,
            Response = <<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8, 16#00:8, 16#00:16, (size(ResponseBody)):32, 16#00:32, 16#00:64>>,
            gen_tcp:send(Socket, Response),
            gen_tcp:send(Socket, ResponseBody),
            {Namespace, Name} = split_key(Key),
            <<IntValue/integer>> = Value,
            {set, Namespace, Name, IntValue};
        16#10 ->
            lists:foreach(
                fun({K, V}) ->
                    ResponseBody = <<Extras:ExtrasSize/binary, K:(size(K))/binary, V/binary>>,
                    Response = <<16#81:8, Opcode:8, (size(K)):16, ExtrasSize:8, 16#00:8, 16#00:16, (size(ResponseBody)):32, 16#00:32, 16#00:64>>,
                    gen_tcp:send(Socket, Response),
                    gen_tcp:send(Socket, ResponseBody)
                end,
                [{X, heman:health(X)} || X <- heman:namespaces()]
            ),
            ResponseBody = <<Extras:ExtrasSize/binary, Key:KeySize/binary, <<>>/binary>>,
            Response = <<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8, 16#00:8, 16#00:16, (size(ResponseBody)):32, 16#00:32, 16#00:64>>,
            gen_tcp:send(Socket, Response),
            gen_tcp:send(Socket, ResponseBody),
            stat;
        16#07 ->
            ResponseBody = <<Extras:ExtrasSize/binary, Key:KeySize/binary, <<>>/binary>>,
            Response = <<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8, 16#00:8, 16#00:16, (size(ResponseBody)):32, 16#00:32, 16#00:64>>,
            gen_tcp:send(Socket, Response),
            gen_tcp:send(Socket, ResponseBody),
            quit
    end;
parse_input(_, Data) ->
    error_logger:error_report({?MODULE, ?LINE, bad_data, Data}),
    quit.

split_key(Key) -> split_key(Key, <<>>).
split_key(<<58, Binary/binary>>, B) -> {B, Binary};
split_key(<<A, Binary/binary>>, B) -> split_key(Binary, <<B/binary, A>>).
