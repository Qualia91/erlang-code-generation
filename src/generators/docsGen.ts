import * as vscode from 'vscode';
import { userInfo } from 'os';
import * as fs from 'fs';

import * as utils from './../generic/utils';
import * as moduleGen from './moduleGen';

type Pair<T,K> = [T,K];
type Pairs<T,K> = Pair<T,K>[];

//=============================================================================
// External Functions
//=============================================================================

export function generateDocs() {

    var docsTemplate = fs.readFileSync(__dirname + '/../templates/docs/docs.template','utf8');

    var [commentContents, commentCode] =  generateCommentsReadmeSections();
    var [moduleContents, moduleCode] =  generateModuleReadmeSections();

    docsTemplate = docsTemplate
        .replace(/<COMMENTS_CONTENTS>/g, commentContents)
        .replace(/<COMMENTS_DOCS>/g, commentCode)
        .replace(/<MODULE_CONTENTS>/g, moduleContents)
        .replace(/<MODULE_DOCS>/g, moduleCode);

    fs.writeFile(__dirname + '/../test.md', docsTemplate,  function(err) {
        if (err) {
            return console.error(err);
        }
    });
}

//=============================================================================
// Internal Functions
//=============================================================================

function generateCommentsReadmeSections() : string[] {
    var codeTemplate = fs.readFileSync(__dirname + '/../templates/docs/code.template','utf8');
    var headerTemplate = fs.readFileSync(__dirname + '/../templates/snippets/header.template','utf8');
    var sectionTemplate = fs.readFileSync(__dirname + '/../templates/snippets/sectionComment.template','utf8');
    var functionTemplate = fs.readFileSync(__dirname + '/../templates/snippets/functionComment.template','utf8');

    var headerCodeTemplate = codeTemplate
        .replace(/<NAME>/g, "Header")
        .replace(/<CODE>/g, headerTemplate.replace(/<HEADER_TITLE>(.*)?/, ""));
    var sectionCodeTemplate = codeTemplate
        .replace(/<NAME>/g, "Section")
        .replace(/<CODE>/g, sectionTemplate.replace(/<SECTION_TITLE>(.*)?/, ""));
    var functionCodeTemplate = codeTemplate
        .replace(/<NAME>/g, "Function")
        .replace(/<CODE>/g, functionTemplate.replace(/<FUNCTION_COMMENT_TITLE>(.*)?/, ""));

    var contents = `- [Comments](#comments)
    - [Header](#header)
    - [Section](#section)
    - [Function](#function)`;

    var code = headerCodeTemplate + '\n' + sectionCodeTemplate + '\n' + functionCodeTemplate;

    return [contents, code];
}

function generateModuleReadmeSections() : string[] {
    var codeTemplate = fs.readFileSync(__dirname + '/../templates/docs/code.template','utf8');

    var order = [
        ["Supervisor", "supervisor.template"],
        ["Empty", "emptyModule.template"],
        ["Gen Server", "genServer.template"],
        ["Gen State Machine", "genStateM.template"],
        ["Header", "header.template"],
        ["CT", "ct.template"],
        ["EScript", "escript.template"],
        ["Lager Handler", "lagerHandler.template"],
        ["Poolboy Worker", "poolboyWorker.template"],
        ["Cowboy Rest Handler", "cowboyRestHandler.template"],
        ["Cowboy Websocket Handler", "websocketHandler.template"],
    ];

    var contents = "- [Modules](#modules)\n";
    var code = "";
    for (let [Name, File] of order) {
        contents = contents + '\t- [' + Name + '](#' + Name.toLowerCase().replace(/ /g, "-") + ')\n';
        var updatedTemplate = codeTemplate
            .replace(/<NAME>/g, Name)
            .replace(/<CODE>/g, moduleGen.generateModuleTemplate(File));
        code = code + updatedTemplate + '\n';
    }

    return [contents, code];
}