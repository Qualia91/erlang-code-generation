# erlang-code-generation

Download here: https://marketplace.visualstudio.com/items?itemName=bocdev.erlang-code-generation

This is an Extension for Visual Studio Code to generate some boilerplate code for you Erlang projects. Its currently under development.

# Features

Currently Supported Generation:
- [Comments](#comments)
    - [Header](#header)
    - [Section](#section)
    - [Function](#function)
- [Module](#modules)
    - [Gen Server](#gen-server)
	- [Gen State Machine](#gen-state-machine)
    - [Supervisor](#supervisor)
    - [Header](#header)
    - [Empty](#empty)
    - [CT](#ct)
    - [Poolboy Worker](#poolboy-worker)
    - [Cowboy Websocket Handler](#cowboy-websocket-handler)
    - [Cowboy REST Handler](#cowboy-rest-handler)
    - [Lager Handler](#lager-handler)
- [Snippets](#snippets)
    - [Case](#case)
    - [Receive](#receive)
    - [Try/Catch](#try-catch)
    - [Eunit](#eunit)
    - [Poolboy Specs](#poolboy-specs)
    - [Cowboy Web Supervisor](#cowboy-web-supervisor)
    - [Worker Child Spec](#worker-child-spec)
    - [Supervisor Child Spec](#supervisor-child-spec)

    Snippets
Case
Receive
Try/Catch
Eunit
Poolboy Specs

# Docs

## Comments
### Header
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc <MODULE_COMMENT_TITLE>
%%%
%%% <MODULE_COMMENT>
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------
```
### Section
```erlang
%%%=============================================================================
%%% <SECTION_TITLE>
%%%=============================================================================
```

### Function
```erlang
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
```

## Modules
### Gen Server
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([]) ->
	LoopState = #loop_state{}, 
	{ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(_Request, _From, LoopState) ->
	Reply = ok,
	{reply, Reply, LoopState}.


-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast(_Msg, LoopState) ->
	{noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(_Info, LoopState) ->
	{noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
	ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
	{ok, LoopState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Gen State Machine
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen State Machine
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).
-behaviour(gen_statem).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
	start_link/0,
	stop/0
]).

%% Gen State Machine Callbacks
-export([
	init/1,
	code_change/4,
	callback_mode/0,
	terminate/3
]).

%% State transitions
-export([
	example_state/3,
	next_example_state/3,
	handle_event/3
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
	gen_statem:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?SERVER).	

%%%=============================================================================
%%% Gen State Machine Callbacks
%%%=============================================================================

init([]) ->
	State = start_state,
	Data = data, 
	{ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
	
callback_mode() -> 
	state_functions.

%%%=============================================================================
%%% State transitions
%%%=============================================================================

example_state({call,From}, action, Data) ->
    {next_state, next_example_state, Data, [{reply,From,next_example_state}]};
example_state(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

next_example_state({call,From}, action, Data) ->
	{next_state, example_state, Data, [{reply,From,example_state}]};
next_example_state(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

%%-----------------------------------------------------------------------------
%% @doc
%% Handle events common to all states 
%% @end
%%-----------------------------------------------------------------------------
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
	?assertEqual(true, true).

-endif.
```
### Supervisor
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).
-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal
%%%=============================================================================
```
### Header
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

%%%=============================================================================
%%% Global Record Definitions
%%%=============================================================================

%% example record
-record(example_record, {}).
-type example_record() :: example_record.
```
### Empty
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### CT
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-define(SERVER, ?MODULE).

-export([
	all/0, 
	init_per_suite/1, 
	init_per_testcase/2, 
	end_per_testcase/2, 
	end_per_suite/1
]).

-export([
    example_test/1
]).

%%%=============================================================================
%%% CT Functions
%%%=============================================================================

all() -> 
    [example_test].

init_per_suite(Config) ->
    Config.

init_per_testcase(_TestName, Config) ->
    Config.
 
end_per_testcase(_TestName, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%%%=============================================================================
%%% Tests
%%%=============================================================================

example_test(_Config) ->
	?assertEqual(true, true, <<"Example Comment">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================
```
### Poolboy Worker
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).
-behaviour(poolboy_worker).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/1]).

%% Callbacks
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2, 
	code_change/3
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(map()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(map()) -> {ok, loop_state()}.
init(Args) ->
	process_flag(trap_exit, true),
	{ok, #loop_state{}}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(_Request, _From, LoopState) ->
	Reply = ok,
	{reply, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast(_Msg, LoopState) ->
	{noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(_Info, LoopState) ->
	{noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
	ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
	{ok, LoopState}.

%%%=============================================================================
%%% Internal
%%%=============================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
	?assertEqual(true, true).

-endif.		
```
### Cowboy Websocket Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Cowboy websocket handler.
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% Websocket Callbacks
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
	ping_interval :: integer()
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% Websocket Callbacks
%%%=============================================================================

init(Req, [{ping_interval, Interval}]) ->
    {cowboy_websocket, Req, #loop_state{
        ping_interval = Interval
    }}.

websocket_init(LoopState = #loop_state{ping_interval = Interval}) ->
	lager:debug("Websocket init, pinging every ~p ms~n", [Interval]),
	erlang:start_timer(Interval, self(), ping),
	{reply, {binary, <<"ping">>}, LoopState}.

websocket_handle({_, <<"pong">>}, LoopState = #loop_state{ping_interval = Interval}) ->
	erlang:start_timer(Interval, self(), ping),
	{ok, LoopState};
websocket_handle(Msg, LoopState) ->
	lager:debug("Unhandled websocket_handle: ~p~n", [Msg]),
	{ok, LoopState}.

websocket_info({timeout, _Ref, ping}, LoopState) ->
	{reply, {text, <<"ping">>}, LoopState};
websocket_info(Info, State) ->
	lager:debug("Unknown websocket_info: ~p", [Info]),
	{ok, State}.

terminate(_Reason, _Req, _LoopState) ->
	lager:debug("Websocket connection terminated"),
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.	
```
### Cowboy REST Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Cowboy REST handler.
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% Websocket Callbacks
-export([
    init/2,
	allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
	
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% REST Callbacks
%%%=============================================================================

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

%%-----------------------------------------------------------------------------
%% @doc
%% REST methods supported by handler
%% @end
%%-----------------------------------------------------------------------------
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Define handler functions for PUT?POST type calls, with parameters if needed.
%% @end
%%-----------------------------------------------------------------------------
content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, []}, get_function},
		{{<<"application">>, <<"json">>, []}, get_function},
		{{<<"text">>, <<"plain">>, []}, get_function}
	], Req, State}.

%%-----------------------------------------------------------------------------
%% @doc 
%% Define handler functions for GET and HEAD calls, with parameters if needed.
%% @end
%%-----------------------------------------------------------------------------
content_types_accepted(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, []}, put_function},
		{{<<"application">>, <<"json">>, []}, put_function},
		{{<<"text">>, <<"plain">>, []}, put_function}
	], Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_function(Req, State) ->
	{<<"">>, Req, State}.

put_function(Req, State) ->
	Req1 = cowboy_req:reply(200, #{}, <<"Response">>, Req),
    {stop, Req1, State}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Lager Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Lager Handler.
%%% @author <USER_NAME>
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author(<USER_NAME>).
-behaviour(gen_event).

-include_lib("lager/include/lager.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-define(SERVER, ?MODULE).

%% Websocket Callbacks
-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type lager_msg_metadata() :: [tuple()].
-type binary_proplist() :: [{binary(), binary()}].

%% Loop state
-record(state, {
	level :: integer()
}).
-type state() :: state.

%%%=============================================================================
%%% Behaviour Impl
%%%=============================================================================

-spec init(list()) -> {ok, state()}.
init([Level, RetryTimes, RetryInterval, Token]) ->
    State = #state{
                    level = lager_util:level_to_num(Level)
                  },
    {ok, State}.

-spec handle_call(get_loglevel | set_loglevel, state()) -> {ok, state()}.
handle_call(get_loglevel, #state{ level = Level } = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{ level = lager_util:level_to_num(Level) }};
handle_call(_Request, State) ->
    {ok, ok, State}.
   
-spec handle_event({log, any()}, state()) -> {ok, state()}.
handle_event({log, Message}, #state{level=Level} = State) ->
	ok;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
	?assertEqual(true, true).

-endif.
```
## Snippets
### Case
```erlang
case VAR of
	_ ->
		ok
end,
```
### Receive
```erlang
receive
	_ ->
		ok
after
	Timeout ->
		ok
end,
```
### Try Catch
```erlang
try THIS of
	_ ->
		ok
catch
	ERROR_TYPE:ERROR ->
		ok
end,
```
### Eunit
```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Poolboy Specs
```erlang
{ok, Pools} = application:get_env(<APPLICATION_NAME>, pools),
PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
	WorkerImpl = proplists:get_value(worker_impl, WorkerArgs),
	PoolArgs = [{name, {local, Name}},
				{worker_module, WorkerImpl}] ++ SizeArgs,
	poolboy:child_spec(Name, PoolArgs, WorkerArgs)
end, Pools),
```
### Cowboy Web Supervisor
```erlang
Dispatch = cowboy_router:compile([
	{'_', [
		{"/endpoint", endpoint, [{stats_interval, 10000}]}
	]}
]),
{ok, _} = cowboy:start_clear(
	http,
	[
		{port, PORT}
	],
	#{env=>#{dispatch=>Dispatch}}
),
```
### Worker Child Spec
```erlang
#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker
}
```
### Supervisor Child Spec
```erlang
#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => supervisor
}
```
