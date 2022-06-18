import { userInfo } from 'os';
import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	
	console.log('Congratulations, your extension "erlang-code-generation" is now active!');

	let code = vscode.commands.registerCommand('erlang-code-generation.code-gen', () => {
		if (isFileOk()) {
			vscode.window.showInformationMessage('code-gen from erlang-code-generation!');
		};
	});

	let comment = vscode.commands.registerCommand('erlang-code-generation.comment-gen', () => {
		if (isFileOk()) {
			createCommentQuickPickBox(["Header", "Section", "Function"], "Select the comment template you wish to generate");
		};
	});

	let module = vscode.commands.registerCommand('erlang-code-generation.module-gen', () => {
		if (isFileOk()) {
			createModuleQuickPickBox(["Gen-Server", "Supervisor", "Header", "Empty", "CT"], "Select the module behavior you wish to implement");
		};
	})

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
%%% @title <MODULE_NAME>
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
		case "Gen-Server":
			return `%%%-----------------------------------------------------------------------------
%%% @title <MODULE_NAME>
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

-spec init(list()) -> {ok, ()}.
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
`;
		case "Supervisor":
			return `%%%-----------------------------------------------------------------------------
%%% @title <MODULE_NAME>
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
`;
		case "Header":
return `%%%-----------------------------------------------------------------------------
%%% @title <MODULE_NAME>
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
%%% @title <MODULE_NAME>
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
		default:
			return `%%%-----------------------------------------------------------------------------
%%% @title <MODULE_NAME>
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
`;
	}
}