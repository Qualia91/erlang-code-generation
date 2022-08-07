import { userInfo } from 'os';
import * as vscode from 'vscode';

import * as moduleGen from './generators/moduleGen';
import * as utils from './generic/utils';

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
				"Supervisor Child Spec", 
				"Record"
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
			moduleGen.createModuleQuickPickBox([
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

function convertToPrompt(input:string):string {
	return input
		.replace(/</g, "Enter ")
		.replace(/>/g, "")
		.replace(/_/g, " ")
		.toLowerCase();
}

function showBoxForEdits(retTuple:[string, string[]], index:number): Thenable<string> {

	return vscode.window.showInputBox({prompt: convertToPrompt(retTuple[1][index]), placeHolder: retTuple[1][index]}).then(value => {
		if (value === undefined) {
			throw new Error('cancelled');
		}
		return value;
	}).then(nextReplaceVal => {
		var reg = new RegExp(retTuple[1][index], 'g');
		var updatedTuple:string = retTuple[0].replace(reg, nextReplaceVal);
		if (index >= retTuple[1].length - 1) {
			return updatedTuple;
		} else {
			return showBoxForEdits([updatedTuple, retTuple[1]], index + 1);
		}
	});
};

function createCodeQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: false, placeHolder: pickableTitle})
		.then(item => {
			if (item === undefined) {
				throw new Error('cancelled');
			}
			return createCodeSnippet2(item);
		}).then(retTuple => {
			return showBoxForEdits(retTuple, 0);
		}).then(retStr => {	
			let editor = vscode.window.activeTextEditor;
			if (editor === undefined) {
				if (editor === undefined) {
					throw new Error('cancelled');
				}
			}		
			utils.insertText(editor, 
				retStr,
				editor.selection.start);
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
								utils.insertText(editor, 
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

function createCodeSnippet2(item:string):[string, string[]] {
	switch (item) {
		case "Record":
	return [`-record(<RECORD_NAME>, {
		
}).
-type <RECORD_NAME> = <RECORD_NAME>().`, ["<RECORD_NAME>"]];
;
		case "Worker Child Spec":
	return [`#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => worker
}`, ["<ID>", "<MODULE>"]];
		case "Supervisor Child Spec":
	return [`#{
	id => <ID>,
	start => {<MODULE>, start_link, []},
	restart => permanent,
	shutdown => brutal_kill,
	type => supervisor
}`, ["<ID>", "<MODULE>"]];
		case "Case":
	return [`case <CASE_TARGET> of
	_ ->
		ok
end,`, ["<CASE_TARGET>"]];
		default:
	return [createCodeSnippet(item), []];
	}
};

function createCodeSnippet(item:string):string {
	switch (item) {
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
};