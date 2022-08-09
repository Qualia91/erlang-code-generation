import * as vscode from 'vscode';
import * as fs from 'fs';

export function insertText(editor:vscode.TextEditor, text:string, pos:vscode.Position) {
    var snippet = new vscode.SnippetString(text);
    editor.insertSnippet(snippet, editor.selection.end);
};


export function generateHeader(headerTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/headerTemplate.txt','utf8');
    return template.replace(/<HEADER_TITLE>/g, headerTitle);
};

export function generateSectionComment(sectionTitle:string) : string {
    var template = fs.readFileSync(__dirname + '/../templates/sectionCommentTemplate.txt','utf8');
    return template.replace(/<SECTION_TITLE>/g, sectionTitle);
};

export function generateEunitTest() : string {
    return `${generateSectionComment("Eunit Tests")}

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.`;
};

export function generateFromTemplate(template:string, header:string, exportAndDefs:string, mainBody:string) : string {
    return template
        .replace(/<HEADER>/g, header)
        .replace(/<EXPORT_AND_DEFS>/g, exportAndDefs)
        .replace(/<MAIN_BODY>/g, mainBody);
}