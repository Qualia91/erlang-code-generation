# erlang-code-generation README

This is an Extension for Visual Studio Code to generate some boilerplate code for you Erlang projects. Its currently under development.

# Features

Currently Supported Generation:
- [Comments](#comments)
    - [Header](#header)
    - [Section](#section)
    - [Function](#function)
- [Module](#modules)
    - [Gen Server](#gen-server)
    - [Supervisor](#supervisor)
    - [Header](#header)
    - [Empty](#empty)
    - [CT](#ct)
    - [Poolboy Worker](#poolboy-worker)
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