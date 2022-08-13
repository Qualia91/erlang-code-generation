import { userInfo } from 'os';
import * as vscode from 'vscode';

import * as edp from './erlang-data-provider'

import * as moduleGen from './generators/moduleGen';
import * as commentGen from './generators/commentGen';
import * as utils from './generic/utils';

export function activate(context: vscode.ExtensionContext) {
	const rootPath =
		vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0
		? vscode.workspace.workspaceFolders[0].uri.fsPath
		: undefined;
	if (rootPath !== undefined) {
		vscode.window.registerTreeDataProvider("erlang-project-outline", new edp.ErlangDataProvider(rootPath));
		// vscode.commands.registerCommand("exampleTreeView.selectNode", (item:vscode.TreeItem) => {
		// 	console.log(item.label);
		// });
	}
	
	console.log('Congratulations, your extension "erlang-code-generation" is now active!');

	let comment = vscode.commands.registerCommand('erlang-code-generation.comment-gen', () => {
		if (isFileOk()) {
			commentGen.createCommentQuickPickBox([
				"Header", 
				"Section", 
				"Function"
			], "Select the comment template you wish to generate");
		};
	});

	let module = vscode.commands.registerCommand('erlang-code-generation.module-gen', () => {
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
			"Lager Handler",
			"EScript"
		], "Select the module behavior you wish to implement");
	});

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
	if (!(editor.document.fileName.endsWith(".erl") || editor.document.fileName.endsWith(".hrl") || editor.document.fileName.endsWith(".escript"))) {
		vscode.window.showErrorMessage("File ".concat(editor.document.fileName).concat(" is not an erlang (.erl, .hrl or .escript) file"));
		return false;
	}

	return true;
}