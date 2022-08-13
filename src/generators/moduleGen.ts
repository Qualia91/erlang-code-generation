import * as vscode from 'vscode';
import { userInfo } from 'os';
import * as fs from 'fs';

import * as utils from './../generic/utils';

//=============================================================================
// External Functions
//=============================================================================

export function createModuleQuickPickBox(pickableNames:string[], pickableTitle:string) {
    vscode.window.showQuickPick(pickableNames, {canPickMany: false, placeHolder: pickableTitle})
        .then(item => {
            if (item !== undefined) {
                let editor = vscode.window.activeTextEditor;
                if (editor !== undefined) {
                    if (editor !== undefined) {
                        var filename = utils.getFilename(editor);
                        utils.insertText(editor, 
                            createModule(editor, item)
                                .replace(/<MODULE_NAME>/g, filename)
                                .replace(/<USER_NAME>/g, userInfo().username),
                            editor.selection.start);
                    }
                }
            }
        }
    );
};

//=============================================================================
// Internal Functions
//=============================================================================

function generateModuleTemplate(moduleTemplateFileName:string) : string {
    var moduleTemplate = fs.readFileSync(__dirname + '/../templates/modules/' + moduleTemplateFileName,'utf8');
    var headerTemplate = fs.readFileSync(__dirname + '/../templates/snippets/header.template','utf8');
    var sectionCommentTemplate = fs.readFileSync(__dirname + '/../templates/snippets/sectionComment.template','utf8');
    var functionCommentTemplate = fs.readFileSync(__dirname + '/../templates/snippets/functionComment.template','utf8');
    var testsTemplate = fs.readFileSync(__dirname + '/../templates/snippets/eunitTests.template','utf8');

    moduleTemplate = moduleTemplate.replace(/<EUNIT_TESTS>(.*)?/g, testsTemplate);
    moduleTemplate = utils.replaceStructureInTemplate(/<HEADER>(.*)?/, /<HEADER_TITLE>(.*)?/, moduleTemplate, headerTemplate, "Generated from " + moduleTemplateFileName);
    moduleTemplate = utils.replaceStructureInTemplate(/<SECTION_COMMENT>(.*)?/, /<SECTION_TITLE>(.*)?/, moduleTemplate, sectionCommentTemplate, "");
    moduleTemplate = utils.replaceStructureInTemplate(/<FUNCTION_COMMENT>(.*)?/, /<FUNCTION_COMMENT_TITLE>(.*)?/, moduleTemplate, functionCommentTemplate, "");

    return moduleTemplate;
}

function createModule(editor:vscode.TextEditor, item:string):string {
	switch (item) {

		case "Cowboy Websocket Handler":
            return generateModuleTemplate("websocketHandler.template");

		case "Lager Handler":
            return generateModuleTemplate("lagerHandler.template");

		case "Cowboy REST Handler":
			return generateModuleTemplate("cowboyRestHandler.template");

		case "Gen Server":
			return generateModuleTemplate("genServer.template");

		case "Gen State Machine":
			return generateModuleTemplate("genStateM.template");

		case "Supervisor":
			return generateModuleTemplate("supervisor.template");

		case "Header":
			return generateModuleTemplate("header.template");

		case "CT":
			return generateModuleTemplate("ct.template");

		case "Poolboy Worker":
			return generateModuleTemplate("poolboyWorker.template");

        case "EScript":
            return generateModuleTemplate("escript.template");

		default:
			return generateModuleTemplate("emptyModule.template");
	}
}