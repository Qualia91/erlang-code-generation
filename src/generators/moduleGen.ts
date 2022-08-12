import * as vscode from 'vscode';
import { userInfo } from 'os';
import * as fs from 'fs';

import * as utils from './../generic/utils';

//=============================================================================
// External Functions
//=============================================================================

export function createModuleQuickPickBox(pickableNames:string[], pickableTitle:string) {
    vscode.window.showQuickPick(pickableNames, {canPickMany: false, placeHolder: pickableTitle})
        .then(item => {

            if (item !== undefined) {
                let editor = vscode.window.activeTextEditor;
                if (editor !== undefined) {
                    if (editor !== undefined) {
                        var fileNameSplit = editor.document.fileName.replace(".erl", "").replace(".hrl", "").split("\\");
                        var fileName = fileNameSplit[fileNameSplit.length - 1];
                        utils.insertText(editor, 
                            createModule(editor, item)
                                .replace(/<MODULE_NAME>/g, fileName)
                                .replace(/<USER_NAME>/g, userInfo().username),
                            editor.selection.start);
                    }
                }
            }

        }
    );
};

//=============================================================================
// Internal Functions
//=============================================================================

function generateModuleTemplate(moduleTemplateFileName:string) : string {
    var moduleTemplate = fs.readFileSync(__dirname + '/../templates/' + moduleTemplateFileName,'utf8');
    var headerTemplate = fs.readFileSync(__dirname + '/../templates/header.template','utf8');
    var sectionCommentTemplate = fs.readFileSync(__dirname + '/../templates/sectionComment.template','utf8');
    var testsTemplate = fs.readFileSync(__dirname + '/../templates/eunitTests.template','utf8');

    moduleTemplate = moduleTemplate.replace(/<EUNIT_TESTS>(.*)?/g, testsTemplate);
    moduleTemplate = replaceStructureInTemplate(/<HEADER>(.*)?/, /<HEADER_TITLE>(.*)?/, moduleTemplate, headerTemplate, "Generated from " + moduleTemplateFileName);
    moduleTemplate = replaceStructureInTemplate(/<SECTION_COMMENT>(.*)?/, /<SECTION_TITLE>(.*)?/, moduleTemplate, sectionCommentTemplate, "");

    return moduleTemplate;
}

function replaceStructureInTemplate(structureGetter:RegExp, varInStructureGetter:RegExp, template:string, structureTemplate:string, defaultStructureInput:string) : string {
    var arr;
    while (arr = structureGetter.exec(template)) {
        var updatedStructureTemplate = structureTemplate.replace(varInStructureGetter, arr[1]);
        template = template.replace(structureGetter, updatedStructureTemplate);
    }
    return template;
}

function replaceInTemplate(template:string, rx:RegExp, defaultReplace:string) {
    var arr = rx.exec(template);
    if (arr !== null && arr.length > 0 && arr[1] !== undefined) {
        template = template.replace(rx, arr[1]);
    } else {
        template = template.replace(rx, defaultReplace);
    }
    return template;
}

function createModule(editor:vscode.TextEditor, item:string):string {
	switch (item) {

		// case "Cowboy Websocket Handler":
        //     return generateModuleTemplate(
        //         utils.generateHeader("Cowboy Websocket Handler"),
        //         generateWebsocketExportAndDefs(),
        //         generateWebsocketMainBody()
        //     );

		case "Cowboy Websocket Handler":
            return generateModuleTemplate("websocketHandlerModule.template");

		case "Lager Handler":
            return generateModuleTemplate("lagerHandlerModule.template");

        // case "Cowboy REST Handler":
        //     return generateModuleTemplate(
        //         utils.generateHeader("Cowboy REST Handler"),
        //         generateRestExportAndDefs(),
        //         generateRestHandlerMainBody()
        //     );

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_function(Req, State) ->
	{<<"">>, Req, State}.

put_function(Req, State) ->
	Req1 = cowboy_req:reply(200, #{}, <<"Response">>, Req),
    {stop, Req1, State}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Tests
%%%=============================================================================

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Tests
%%%=============================================================================

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

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

%%%=============================================================================
%%% Tests
%%%=============================================================================

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
`;
	}
}