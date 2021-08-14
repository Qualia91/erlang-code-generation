import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	
	console.log('Congratulations, your extension "erlang-code-generation" is now active!');

	let code = vscode.commands.registerCommand('erlang-code-generation.code-gen', () => {
		isFileOk();
		vscode.window.showInformationMessage('code-gen from erlang-code-generation!');
	});

	let comment = vscode.commands.registerCommand('erlang-code-generation.comment-gen', () => {
		//TODO Remove
		vscode.window.showInformationMessage('comment-gen from erlang-code-generation!');
		isFileOk();
		createQuickPickBox(["Header", "Section", "Function"], "Select the comment template you wish to generate");
	});

	let module = vscode.commands.registerCommand('erlang-code-generation.module-gen', () => {
		isFileOk();
		vscode.window.showInformationMessage('module-gen from erlang-code-generation!');
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

	// check we have a go file open
	if (!editor.document.fileName.endsWith(".erl") || !editor.document.fileName.endsWith(".hrl")) {
		vscode.window.showErrorMessage("File is not an erlang (.erl or .hrl) file");
		return false;
	}

	return true;
}

function createQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: true, placeHolder: pickableTitle})
		.then(items => {

			if (items !== undefined) {
				items.reverse().forEach(item => {
					let editor = vscode.window.activeTextEditor;
					if (editor !== undefined) {
						var commentPositions = getCommentPositions(editor, item);
						commentPositions.forEach(position => {
							if (editor !== undefined) {
								insertText(editor, createComment(editor, item), position);
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
%%% @title TODO: <MODULE_TITLE>
%%% @doc TODO: <MODULE_COMMENT_TITLE>
%%%
%%% TODO: <MODULE_COMMENT_SUBTITLE>
%%%
%%% @author TODO: <USER_NAME>
%%% @copyright TODO: <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------
`;
		case "Section":
			return `
%%%=============================================================================
%%% TODO: <SECTION_TITLE>
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