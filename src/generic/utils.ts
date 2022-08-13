import * as vscode from 'vscode';
import * as fs from 'fs';

export function insertText(editor:vscode.TextEditor, text:string, pos:vscode.Position) {
    var snippet = new vscode.SnippetString(text);
    editor.insertSnippet(snippet, editor.selection.end);
};

export function generateHeader(headerTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/snippets/header.template','utf8');
    return template.replace(/<HEADER_TITLE>/g, headerTitle) + "\n";
};

export function generateSectionComment(sectionTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/snippets/sectionComment.template','utf8');
    return template.replace(/<SECTION_TITLE>(.*)?/g, sectionTitle) + "\n";
};

export function generateFunctionComment(sectionTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/snippets/functionComment.template','utf8');
    return template.replace(/<FUNCTION_COMMENT_TITLE>(.*)?/g, sectionTitle) + "\n";
};

export function generateEunitTest() : string {
    var template = fs.readFileSync(__dirname + '/../templates/eunitTests.template','utf8');
    var commentTemplate = fs.readFileSync(__dirname + '/../templates/snippets/sectionComment.template','utf8');
    var rx = /<SECTION_COMMENT>(.*)?/g;
    var arr = rx.exec(template);
    if (arr !== null && arr.length > 0 && arr[1] !== undefined) {
        commentTemplate = commentTemplate.replace(/<SECTION_TITLE>(.*)?/g, arr[1]);
    } else {
        commentTemplate = commentTemplate.replace(/<SECTION_TITLE>(.*)?/g, "Eunit Tests");
    }
    return template.replace(/<SECTION_COMMENT>(.*)?/g, commentTemplate);
};

export function replaceStructureInTemplate(structureGetter:RegExp, varInStructureGetter:RegExp, template:string, structureTemplate:string, defaultStructureInput:string) : string {
    var arr;
    while (arr = structureGetter.exec(template)) {
        var updatedStructureTemplate = structureTemplate.replace(varInStructureGetter, arr[1]);
        template = template.replace(structureGetter, updatedStructureTemplate);
    }
    return template;
};

export function getFilename(editor:vscode.TextEditor) {
    return editor.document.fileName.replace(".erl", "").replace(".hrl", "").replace(/^.*[\\\/]/, '');
}