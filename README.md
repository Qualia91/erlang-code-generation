# Erlang Quality of Life Improvements

Download here: https://marketplace.visualstudio.com/items?itemName=bocdev.erlang-code-generation

This is an Extension for Visual Studio Code to make using erlang in vscode just better. Its currently under development. Feel free to create pull requests for anything you think would be useful!

# Features
- [Side bar](#side-bar)
- [Code Generation & Snippets](#code-generation-and-snippets)
## Side bar
Version 1 introduces the project explorer, which gives you a top level view of your project and all the files within. Selecting a module or one of the items within one (definitions, macros, records, types, exported functions, imported function, tests) will take you to the module and the line number the sub item is found out. Pressing the pen icon on module items will copy it to your clipboard so you can use said item.

![Side](https://github.com/qualia91/erlang-code-generation/blob/master/images/side_bar_image.png?raw=true)

## Eunit testing
By pressing the testing icon to the right of a module in the side panel, it will run the eunit tests in the module and produce the output to an ouput channel called "Eunit Tests".

## Code Generation and Snippets:
- [Comments](#comments)
    - [Header](#header)
    - [Section](#section)
    - [Function](#function)

- [Modules](#modules)
	- [Supervisor](#supervisor)
	- [Empty](#empty)
	- [Gen Server](#gen-server)
	- [Gen State Machine](#gen-state-machine)
	- [Header](#header)
	- [CT](#ct)
	- [EScript](#escript)
	- [Lager Handler](#lager-handler)
	- [Poolboy Worker](#poolboy-worker)
	- [Cowboy Rest Handler](#cowboy-rest-handler)
	- [Cowboy Websocket Handler](#cowboy-websocket-handler)

- [Snippets](#snippets)
	- [print to console: log](#print-to-console)
	- [receive: rec](#receive)
	- [receive with after: reca](#receive-with-after)
	- [case: case](#case)
	- [if: if](#if)
	- [Try: try](#try)
	- [inline try/catch: ?](#inline-try/catch)
	- [eunit tests: eunit](#eunit-tests)
	- [poolboy specs: pools](#poolboy-specs)
	- [cowboy web supervisor: cows](#cowboy-web-supervisor)
	- [worker child specs: works](#worker-child-specs)
	- [supervisor child specs: sups](#supervisor-child-specs)
	- [comment: section: comsec](#comment-section)
	- [comment: function: funsec](#comment-function)


# Docs
## Comments
### Header
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% 
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
```
### Section
```erlang
%%%=============================================================================
%%% 
%%%=============================================================================
```
### Function
```erlang
%%-----------------------------------------------------------------------------
%% @doc
%% 
%% @end
%%-----------------------------------------------------------------------------
```
## Modules
### Supervisor
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Supervisor built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
-behaviour(supervisor).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
	start_link/0
]).

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
%%%  Callbacks
%%%=============================================================================

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Empty
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Empty Module built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([

]).

-define(SERVER, ?MODULE).

%% example record
-record(example_record, {

}).
-type example_record() :: example_record.

%%%=============================================================================
%%% API
%%%=============================================================================

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Gen Server
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen Server built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([start_link/0]).

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

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([]) ->
	LoopState = #loop_state{}, 
	{ok, LoopState}.

handle_call(_Request, _From, LoopState) ->
	Reply = ok,
	{reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
	{noreply, LoopState}.

handle_info(_Info, LoopState) ->
	{noreply, LoopState}.

terminate(_Reason, _LoopState) ->
	ok.

code_change(_OldVsn, LoopState, _Extra) ->
	{ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Gen State Machine
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen State Machine built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
-behaviour(gen_statem).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

-define(SERVER, ?MODULE).

%% External API
-export([
	start_link/0,
	stop/0,
    example_action/0
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

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
	gen_statem:start({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok;
stop() ->
    gen_statem:stop(?SERVER).	

-spec example_action() -> term();
example_action() ->
    gen_statem:call(?SERVER, example_action).

%%%=============================================================================
%%% Gen State Machine Callbacks
%%%=============================================================================

init([]) ->
	State = example_state,
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

example_state({call,From}, example_action, Data) ->
    {next_state, next_example_state, Data, [{reply,From,next_example_state}]};
example_state(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

next_example_state({call,From}, example_action, Data) ->
	{next_state, example_state, Data, [{reply,From,example_state}]};
next_example_state(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

%%-----------------------------------------------------------------------------
%% @doc
%% Handle events common to all states.
%% @end
%%-----------------------------------------------------------------------------
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Header
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Header built from template
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================

%% example record
-record(example_record, {

}).
-type example_record() :: example_record.
```
### CT
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Common Tests built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%=============================================================================
%%% Export and Defs
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
```
### EScript
```erlang
#!/opt/homebrew/opt/erlang@24/bin/escript
%% -*- erlang -*-

%%%=============================================================================
%%% EScript Functions
%%%=============================================================================

main(Inputs) ->
    try
        io:format("Inputs: ~p\n", [Inputs])
    catch
        Error:Reason ->
            io:format("Error: ~p, Reason: ~p\n", [Error, Reason]),
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: <MODULE_NAME> <INPUT>\n"),
    halt(1).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
```
### Lager Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Lager Handler built from template
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
-behaviour(gen_event).

%%%=============================================================================
%%% Export and Defs
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

%% Loop state
-record(state, {
    level :: integer()
}).
-type state() :: state.

%%%=============================================================================
%%% Lager Handler Callbacks
%%%=============================================================================

init([Level, _RetryTimes, _RetryInterval, _Token]) ->
    State = #state{
        level = lager_util:level_to_num(Level)
    },
    {ok, State}.

handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{ level = lager_util:level_to_num(Level) }};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, _Message}, _State) ->
    ok;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Poolboy Worker
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Poolboy Worker built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").
-behaviour(poolboy_worker).

%%%=============================================================================
%%% Export and Defs
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
%%% Callbacks
%%%=============================================================================

init(_Args) ->
	process_flag(trap_exit, true),
	{ok, #loop_state{}}.

handle_call(_Request, _From, LoopState) ->
	Reply = ok,
	{reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
	{noreply, LoopState}.

handle_info(_Info, LoopState) ->
	{noreply, LoopState}.

terminate(_Reason, _LoopState) ->
	ok.

code_change(_OldVsn, LoopState, _Extra) ->
	{ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Cowboy Rest Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Cowboy Rest Handler built from template.
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% Websocket Callbacks
-export([
    init/2,
	allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2
]).

%% Websocket Handlers
-export([
    get_function/2,
	put_function/2
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
	
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% Cowboy Rest Handler Callbacks
%%%=============================================================================

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

%%-----------------------------------------------------------------------------
%% @doc
%% REST methods supported by handler.
%% @end
%%-----------------------------------------------------------------------------
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Define handler functions for GET and HEAD calls, with parameters if needed.
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
%% Define handler functions for PUT?POST type calls, with parameters if needed.
%% @end
%%-----------------------------------------------------------------------------
content_types_accepted(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, []}, put_function},
		{{<<"application">>, <<"json">>, []}, put_function},
		{{<<"text">>, <<"plain">>, []}, put_function}
	], Req, State}.

%%%=============================================================================
%%% Handlers
%%%=============================================================================

get_function(Req, State) ->
	{<<"">>, Req, State}.

put_function(Req, State) ->
	Req1 = cowboy_req:reply(200, #{}, <<"Response">>, Req),
    {stop, Req1, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```
### Cowboy Websocket Handler
```erlang
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Websockets Handler made from template
%%% @author <USER_NAME>
%%% @end
%%%-----------------------------------------------------------------------------

-module(<MODULE_NAME>).
-author("<USER_NAME>").

%%%=============================================================================
%%% Export and Defs
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
%%% Websocket Handler Callbacks
%%%=============================================================================

init(Req, [{ping_interval, Interval}]) ->
    {cowboy_websocket, Req, #loop_state{
        ping_interval = Interval
    }}.

websocket_init(LoopState = #loop_state{ping_interval = Interval}) ->
    erlang:start_timer(Interval, self(), ping),
    {reply, {binary, <<"ping">>}, LoopState}.

websocket_handle({_, <<"pong">>}, LoopState = #loop_state{ping_interval = Interval}) ->
    erlang:start_timer(Interval, self(), ping),
    {ok, LoopState};
websocket_handle(_Msg, LoopState) ->
    {ok, LoopState}.

websocket_info({timeout, _Ref, ping}, LoopState) ->
    {reply, {text, <<"ping">>}, LoopState};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _LoopState) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
```

## Snippets
### print to console
#### *Description: Log output to console*
#### *Command: log*
```erlang
io:format("~p~n", [$1]);
$2
```
### receive
#### *Description: receive block*
#### *Command: rec*
```erlang
receive
	$1 ->
		$2
end$3
```
### receive with after
#### *Description: receive block with after*
#### *Command: reca*
```erlang
receive
	$1 ->
		$2
after
	$3 ->
		$4
end$5
```
### case
#### *Description: case block*
#### *Command: case*
```erlang
case $1 of
	$2 ->
		$3
end$4
```
### if
#### *Description: if block*
#### *Command: if*
```erlang
if
	$1 ->
		$2
	true ->
		$3
end$4
```
### Try
#### *Description: try .. catch block*
#### *Command: try*
```erlang
try $1 of
	$2 ->
		$3
catch
	$4 ->
		$5
end$6
```
### inline try/catch
#### *Description: inline try .. catch block*
#### *Command: ?*
```erlang
try $1 catch $2 -> $4 end$5
```
### eunit tests
#### *Description: eunit tests section*
#### *Command: eunit*
```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

$1_test() ->
	$2.

-endif.
$3
```
### poolboy specs
#### *Description: poolboy worker specs*
#### *Command: pools*
```erlang
{ok, Pools} = application:get_env($1, pools),
PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
	WorkerImpl = proplists:get_value(worker_impl, WorkerArgs),
	PoolArgs = [{name, {local, Name}},
	{worker_module, WorkerImpl}] ++ SizeArgs,
	poolboy:child_spec(Name, PoolArgs, WorkerArgs)
end, Pools),
$2
```
### cowboy web supervisor
#### *Description: cowboy web supervisor*
#### *Command: cows*
```erlang
Dispatch = cowboy_router:compile([
	{'_', [
		{"/endpoint", endpoint, [{stats_interval, 10000}]}
	]}
]),
{ok, _} = cowboy:start_clear(
	http,
	[
		{port, $1}
	],
	#{env=>#{dispatch=>Dispatch}}
),
$2
```
### worker child specs
#### *Description: worker child specs*
#### *Command: works*
```erlang
#{
	id => $1,
	start => {$2, start_link, [$3]},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker
}
$4
```
### supervisor child specs
#### *Description: worker child specs*
#### *Command: sups*
```erlang
#{
	id => $1,
	start => {$2, start_link, [$3]},
	restart => permanent,
	shutdown => brutal_kill,
	type => supervisor
}
$4
```
### comment: section
#### *Description: comment: section*
#### *Command: comsec*
```erlang
%%%=============================================================================
%%% $1
%%%=============================================================================
$2
```
### comment: function
#### *Description: comment: function*
#### *Command: funsec*
```erlang
%%------------------------------------------------------------------------------
%% @doc
%% $1
%% @end
%%------------------------------------------------------------------------------
$2
```
