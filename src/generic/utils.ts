import * as vscode from 'vscode';

export function insertText(editor:vscode.TextEditor, text:string, pos:vscode.Position) {
    var snippet = new vscode.SnippetString(text);
    editor.insertSnippet(snippet, editor.selection.end);
};