import { userInfo } from 'os';
import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	
	console.log('Congratulations, your extension "erlang-code-generation" is now active!');

	let code = vscode.commands.registerCommand('erlang-code-generation.code-gen', () => {
		if (isFileOk()) {
			createCodeQuickPickBox([
				"Case", 
				"Receive", 
				"Try/Catch", 
				"Eunit", 
				"Poolboy Specs", 
				"Cowboy Web Supervisor", 
				"Worker Child Spec", 
				"Supervisor Child Spec"
			], "Select the code snippet you wish to generate");
		};
	});

	let comment = vscode.commands.registerCommand('erlang-code-generation.comment-gen', () => {
		if (isFileOk()) {
			createCommentQuickPickBox([
				"Header", 
				"Section", 
				"Function"
			], "Select the comment template you wish to generate");
		};
	});

	let module = vscode.commands.registerCommand('erlang-code-generation.module-gen', () => {
		if (isFileOk()) {
			createModuleQuickPickBox([
				"Gen Server", 
				"Gen State Machine", 
				"Supervisor", 
				"Header", 
				"Empty", 
				"CT", 
				"Poolboy Worker", 
				"Cowboy Websocket Handler",
				"Cowboy REST Handler",
				"Lager Handler"
			], "Select the module behavior you wish to implement");
		};
	});

	context.subscriptions.push(code);
	context.subscriptions.push(comment);
	context.subscriptions.push(module);
}

// this method is called when your extension is deactivated
export function deactivate() {}

function isFileOk():boolean {
	// check file is open
	var editor = vscode.window.activeTextEditor;
	if (!editor) {
		vscode.window.showErrorMessage("No file open");
		return false;
	}

	// check we have an erlang file open
	if (!(editor.document.fileName.endsWith(".erl") || editor.document.fileName.endsWith(".hrl"))) {
		vscode.window.showErrorMessage("File ".concat(editor.document.fileName).concat(" is not an erlang (.erl or .hrl) file"));
		return false;
	}

	return true;
}

function createCodeQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: false, placeHolder: pickableTitle})
		.then(item => {

			if (item !== undefined) {
				let editor = vscode.window.activeTextEditor;
				if (editor !== undefined) {
					if (editor !== undefined) {
						insertText(editor, 
							createCodeSnippet(editor, item),
							editor.selection.start);
					}
				}
			}

		});
}

function createModuleQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: false, placeHolder: pickableTitle})
		.then(item => {

			if (item !== undefined) {
				let editor = vscode.window.activeTextEditor;
				if (editor !== undefined) {
					if (editor !== undefined) {
						var fileNameSplit = editor.document.fileName.replace(".erl", "").replace(".hrl", "").split("\\");
						var fileName = fileNameSplit[fileNameSplit.length - 1];
						insertText(editor, 
							createModule(editor, item)
								.replace(/<MODULE_NAME>/g, fileName)
								.replace(/<USER_NAME>/g, userInfo().username),
							editor.selection.start);
					}
				}
			}

		});
}

function createCommentQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: true, placeHolder: pickableTitle})
		.then(items => {

			if (items !== undefined) {
				items.reverse().forEach(item => {
					let editor = vscode.window.activeTextEditor;
					if (editor !== undefined) {
						var fileNameSplit = editor.document.fileName.replace(".erl", "").replace(".hrl", "").split("\\");
						var fileName = fileNameSplit[fileNameSplit.length - 1];
						var commentPositions = getCommentPositions(editor, item);
						commentPositions.forEach(position => {
							if (editor !== undefined) {
								insertText(editor, 
									createComment(editor, item)
									.replace(/<MODULE_NAME>/g, fileName)
									.replace(/<USER_NAME>/g, userInfo().username),
								position);
							}
						});
					}
				});
			}

		});
}

function insertText(editor:vscode.TextEditor, text:string, pos:vscode.Position) {
	var snippet = new vscode.SnippetString(text);
	editor.insertSnippet(snippet, editor.selection.end);
}

function createComment(editor:vscode.TextEditor, item:string):string {
	switch (item) {
		case "Header":
			return `
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
`;
		case "Section":
			return `
%%%=============================================================================
%%% <SECTION_TITLE>
%%%=============================================================================
`;
		case "Function":
			return `
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
`;
		default:
			console.log("No such comment!");
			return "";
	}
}

function getCommentPositions(editor:vscode.TextEditor, item:string):vscode.Position[] {
	switch (item) {
		case "Header":
			return [editor.selection.start];
		case "Section":
			return [editor.selection.start];
		case "Function":
			return [editor.selection.start];
		default:
			console.log("No such comment!");
			return [];
	}
}

function createModule(editor:vscode.TextEditor, item:string):string {
	switch (item) {
		case "Cowboy Websocket Handler":
			return `%%%-----------------------------------------------------------------------------
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
`;
		case "Lager Handler":
	return `%%%-----------------------------------------------------------------------------
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
`;
		case "Cowboy REST Handler":
			return `%%%-----------------------------------------------------------------------------
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
`;
		case "Gen Server":
			return `%%%-----------------------------------------------------------------------------
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

%%%=============================================================================
%%% Exports and Definitions
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

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
`;
		case "Gen State Machine":
	return `%%%-----------------------------------------------------------------------------
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

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
	gen_statem:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?SERVER).	

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
`;
		case "Supervisor":
			return `%%%-----------------------------------------------------------------------------
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
`;
		case "Header":
return `%%%-----------------------------------------------------------------------------
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

`;
		case "CT":
			return `%%%-----------------------------------------------------------------------------
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

`;
		case "Poolboy Worker":
			return `%%%-----------------------------------------------------------------------------
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

-include_lib("eunit/include/eunit.hrl").

example_test() ->
	?assertEqual(true, true).

-endif.			
`;
		default:
			return `%%%-----------------------------------------------------------------------------
%%% @doc
%%%
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

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
`;
	}
}

function createCodeSnippet(editor:vscode.TextEditor, item:string):string {
	switch (item) {
		case "Case":
			return `case VAR of
	_ ->
		ok
end,`;
		case "Worker Child Spec":
	return `#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker
}`;
		case "Supervisor Child Spec":
	return `#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => supervisor
}`;
		case "Cowboy Web Supervisor":
	return `Dispatch = cowboy_router:compile([
	{'_', [
		{"/", endpoint, [{ping_interval, 10000}]}
	]}
]),
{ok, _} = cowboy:start_clear(
	http,
	[
		{port, PORT}
	],
	#{env=>#{dispatch=>Dispatch}}
),`;
		case "Try/Catch":
	return `try THIS of
	_ ->
		ok
catch
	ERROR_TYPE:ERROR ->
		ok
end,`;
		case "Receive":
	return `receive
	Case ->
		ok
after
	Timeout ->
		ok
end,`;
		case "Poolboy Specs":
			return `{ok, Pools} = application:get_env(<APPLICATION_NAME>, pools),
PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
	WorkerImpl = proplists:get_value(worker_impl, WorkerArgs),
	PoolArgs = [{name, {local, Name}},
				{worker_module, WorkerImpl}] ++ SizeArgs,
	poolboy:child_spec(Name, PoolArgs, WorkerArgs)
end, Pools),`;
		default:
			return `
%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
`;
	}
}