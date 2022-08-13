import * as vscode from 'vscode';
import { userInfo } from 'os';
import * as fs from 'fs';

import * as utils from './../generic/utils';

//=============================================================================
// External Functions
//=============================================================================

export function createCommentQuickPickBox(pickableNames:string[], pickableTitle:string) {
	vscode.window.showQuickPick(pickableNames, {canPickMany: true, placeHolder: pickableTitle})
		.then(items => {

			if (items !== undefined) {
				items.reverse().forEach(item => {
					let editor = vscode.window.activeTextEditor;
					if (editor !== undefined) {
						var fileName = editor.document.fileName.replace(".erl", "").replace(".hrl", "").replace(/^.*[\\\/]/, '');
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

//=============================================================================
// Internal Functions
//=============================================================================

function createComment(editor:vscode.TextEditor, item:string):string {
	switch (item) {
		case "Header":
            return utils.generateHeader("");
		case "Section":
            return utils.generateSectionComment("");
		default:
            return utils.generateFunctionComment("");
    }
};

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