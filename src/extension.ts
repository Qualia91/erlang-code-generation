import { userInfo } from 'os';
import * as vscode from 'vscode';

import * as moduleGen from './generators/moduleGen';
import * as commentGen from './generators/commentGen';
import * as utils from './generic/utils';

export function activate(context: vscode.ExtensionContext) {
	
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
			"Lager Handler"
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
	if (!(editor.document.fileName.endsWith(".erl") || editor.document.fileName.endsWith(".hrl"))) {
		vscode.window.showErrorMessage("File ".concat(editor.document.fileName).concat(" is not an erlang (.erl or .hrl) file"));
		return false;
	}

	return true;
}