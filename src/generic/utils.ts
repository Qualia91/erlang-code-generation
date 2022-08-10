import * as vscode from 'vscode';
import * as fs from 'fs';

export function insertText(editor:vscode.TextEditor, text:string, pos:vscode.Position) {
    var snippet = new vscode.SnippetString(text);
    editor.insertSnippet(snippet, editor.selection.end);
};

export function generateHeader(headerTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/header.template','utf8');
    return template.replace(/<HEADER_TITLE>/g, headerTitle);
};

export function generateSectionComment(sectionTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/sectionComment.template','utf8');
    return template.replace(/<SECTION_TITLE>(.*)?/g, sectionTitle);
};

export function generateEunitTest() : string {
    var template = fs.readFileSync(__dirname + '/../templates/eunitTests.template','utf8');
    var commentTemplate = fs.readFileSync(__dirname + '/../templates/sectionComment.template','utf8');
    var rx = /<SECTION_COMMENT>(.*)?/g;
    var arr = rx.exec(template);
    if (arr !== null && arr.length > 0 && arr[1] !== undefined) {
        commentTemplate = commentTemplate.replace(/<SECTION_TITLE>(.*)?/g, arr[1]);
    } else {
        commentTemplate = commentTemplate.replace(/<SECTION_TITLE>(.*)?/g, "Eunit Tests");
    }
    return template.replace(/<SECTION_COMMENT>(.*)?/g, commentTemplate);
};

export function generateFromTemplate(template:string, header:string, exportAndDefs:string, mainBody:string) : string {
    return template
        .replace(/<HEADER>/g, header)
        .replace(/<EXPORT_AND_DEFS>/g, exportAndDefs)
        .replace(/<MAIN_BODY>/g, mainBody);
}