import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

export class RegexFunctions {

  public static matchFunctionRegex: RegExp = /^([a-z0-9_]*)\(([^)]*)?\)\s(?:when ([^-]*))?->/gms;
  public static matchFunctionInputsRegex: RegExp = /((?:[a-zA-Z0-9_]+)+),?\s?/gms;
  public static matchBehaviourRegex: RegExp = /^-behaviour\(([a-z0-9_]*)\)./gms;
  public static matchDefineRegex: RegExp = /^-define\(([^,]*),(?:\s)([^)]*)\)./gms;
  public static matchTypeRegex: RegExp = /^-type ([^(]*)/gms;
  public static matchExportRegex: RegExp = /^-export\((?:\s*)\[([^\]]*)\](?:\s*)\)./gms;
  public static matchWithinExportRegex: RegExp = /([a-zA-Z0-9_]+)\/?\d?,?\s*/gms;
  public static matchRecordRegex: RegExp = /^-record\(([a-z0-9_]*),\s*{\s*([a-z0-9_:=.\(\),\s*]*)}\)./gms;
  public static matchRecordValuesVarsRegex: RegExp = /\s*([^,]+)\s*/gms;
  public static matchRecordValueVarRegex: RegExp = /([a-zA-Z0-9().{}]+:?[a-zA-Z0-9().{}]*)/gms; // 1 = name, 2 is either nothing, type or value, 3 is either nothing or value
  public static matchTestFunctionsRegex: RegExp = /^([a-z0-9_]*_test)\(([^)]*)?\)\s->/gms;
  
}

export class ErlangDataProvider implements vscode.TreeDataProvider<vscode.TreeItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<vscode.TreeItem | undefined | void> = new vscode.EventEmitter<vscode.TreeItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<vscode.TreeItem | undefined | void> = this._onDidChangeTreeData.event;
  
  constructor(private workspaceRoot: string) {
    vscode.commands.registerCommand('module.show_file', openTextDocument);
    vscode.commands.registerCommand('module.show_file_line', openTextDocumentAtLine);
  }

  refresh(): void {
		this._onDidChangeTreeData.fire();
	}

  getTreeItem(element: vscode.TreeItem): vscode.TreeItem {
    return element;
  }

  getChildren(element?: vscode.TreeItem): Thenable<vscode.TreeItem[]> {

    if (!this.workspaceRoot) {
      vscode.window.showInformationMessage('Project in workspace');
      return Promise.resolve([]);
    }

    if (element) {

      if (!(element instanceof ModuleInfo)) {
        return Promise.resolve([]);
      }
      return Promise.resolve(
        this.readFileForMetadata(
          path.join(this.workspaceRoot, path.join(element.filePath, element.fileName))
        )
      );
    } else {
      return Promise.resolve(this.readFilesInFolder(this.workspaceRoot, ""));
    }
  }

  private readFilesInFolder(solutionPath: string, projectPath:string): vscode.TreeItem[] {
    if (projectPath.includes("_build") || projectPath.includes(".git")) {
      return [];
    }
    var currentFolderPath = path.join(solutionPath, projectPath);
    if (this.pathExists(currentFolderPath)) {

        let deps: vscode.TreeItem[] = [];
        var fileNames = fs.readdirSync(currentFolderPath);
        fileNames.forEach(file => {
            var ext = file.split('.').pop();
            var knownExt = false;
            var fileType = "Unknown";
            switch(ext) {
              case "erl": {
                fileType = "Module";
                knownExt = true;
                break; 
              }
              case "hrl": {
                fileType = "Header";
                knownExt = true;
                break; 
              }
              case "escript": {
                fileType = "EScript";
                knownExt = true;
                break; 
              }
              case "config": {
                fileType = "Config";
                knownExt = true;
                break; 
              }
              case "src": {
                fileType = "Application Resource";
                knownExt = true;
                break; 
              }
              case "app": {
                fileType = "Application Resource";
                knownExt = true;
                break; 
              }
              default:
                if (ext !== undefined) {
                  fileType = ext;
                }
            }
            var stat = fs.lstatSync(path.join(currentFolderPath, file));
            if (stat.isFile() && knownExt) {
                deps.push(new ModuleInfo(file, projectPath, fileType, vscode.TreeItemCollapsibleState.Collapsed));
            } else if (stat.isDirectory()) {
                var items = this.readFilesInFolder(path.join(solutionPath), path.join(projectPath, file));
                deps = deps.concat(items);
            }
        });

        return deps;
    } else {
        return [];
    }
  }

  private pathExists(p: string): boolean {
    try {
      fs.accessSync(p);
    } catch (err) {
      return false;
    }
    return true;
  }

  private readFileForMetadata(filePath:string): vscode.TreeItem[] {
    var module = fs.readFileSync(filePath,'utf8');
    var exports = this.getExports(module);
    var textFunctions = this.getTestFunctions(module);
    const functionMatchSwitchCurr = (filePath:string, module:string, arr:RegExpExecArray) => this.functionMatchSwitch(filePath, module, arr, exports, textFunctions);
    var treeItems = this.matchInModule(filePath, RegexFunctions.matchBehaviourRegex, module, this.behaviourMatchSwitch);
    treeItems = treeItems.concat(this.matchInModule(filePath, RegexFunctions.matchDefineRegex, module, this.defineMatchSwitch));
    treeItems = treeItems.concat(this.matchInModule(filePath, RegexFunctions.matchRecordRegex, module, this.defineRecordSwitch));
    treeItems = treeItems.concat(this.matchInModule(filePath, RegexFunctions.matchTypeRegex, module, this.defineTypeSwitch));
    treeItems = treeItems.concat(this.matchInModule(filePath, RegexFunctions.matchFunctionRegex, module, functionMatchSwitchCurr));

    return treeItems;
  };

  private getExports(filePath:string):string[] {
    var exports = [];
    var arr;
    while (arr = RegexFunctions.matchExportRegex.exec(filePath)) {
      if (arr[1] !== undefined) {
        var arr2;
        while (arr2 = RegexFunctions.matchWithinExportRegex.exec(arr[1])) {
          exports.push(arr2[1]);          
        }
      }
      
    }
    return exports;
  }

  private getTestFunctions(filePath:string):string[] {
    var textFuncs = [];
    var arr;
    while (arr = RegexFunctions.matchTestFunctionsRegex.exec(filePath)) {
      if (arr[1] !== undefined) {
        textFuncs.push(arr[1]);
      }
      
    }
    return textFuncs;
  }

  private matchInModule(filePath:string, structureGetter:RegExp, module:string, switchFunction:Function) : vscode.TreeItem[] {
    var items = [];
    var arr;
    while (arr = structureGetter.exec(module)) {
      items.push(switchFunction(filePath, module, arr));
    }
    return items;
  };

  private functionMatchSwitch(filePath:string, module:string, arr:RegExpExecArray, exports:string[], testFunctions:string[]): vscode.TreeItem {
    var funcName = "";
    var inputs: string[] | any[] = [];
    var guards = "";
    if (arr[1] !== undefined) {
      funcName = arr[1];
    }
    if (arr[2] !== undefined) {
      inputs = inputGetter(arr[2]);
    }
    if (arr[3] !== undefined) {
      guards = arr[3];
    }

    return new FunctionInfo(
      filePath,
      findLineNumber(module, funcName),
      funcName,
      inputs,
      guards,
      inputs.length,
      exports.includes(funcName),
      testFunctions.includes(funcName),
      vscode.TreeItemCollapsibleState.None
    );

  }

  private behaviourMatchSwitch(filePath:string, module:string, arr:RegExpExecArray): vscode.TreeItem {
    // 1 = function name
    var name = "";
    if (arr[1] !== undefined) {
      name = arr[1];
    }

    return new BehaviourInfo(
      filePath,
      findLineNumber(module, "-behaviour(" + name),
      name,
      vscode.TreeItemCollapsibleState.None
    );

  }

  private defineTypeSwitch(filePath:string, module:string, arr:RegExpExecArray): vscode.TreeItem {
    var name = "";
    if (arr[1] !== undefined) {
      name = arr[1];
    }

    return new TypeInfo(
      filePath,
      findLineNumber(module, "-type " + name),
      name,
      vscode.TreeItemCollapsibleState.None
    );

  }

  private defineMatchSwitch(filePath:string, module:string, arr:RegExpExecArray): vscode.TreeItem {
    // 1 = function name
    var name = "";
    var action = "";
    if (arr[1] !== undefined) {
      name = arr[1];
    }
    if (arr[2] !== undefined) {
      action = arr[2];
    }

    return new DefineInfo(
      filePath,
      findLineNumber(module, "-define(" + name),
      name,
      action,
      vscode.TreeItemCollapsibleState.None
    );

  }

  private defineRecordSwitch(filePath:string, module:string, arr:RegExpExecArray): vscode.TreeItem {
    var name = "";
    var simpleVariables = [];
    var variables = "";
    if (arr[1] !== undefined) {
      name = arr[1];
    }
    if (arr[2] !== undefined) {
      var arr2;
      var varLineArr = "";
      while (arr2 = RegexFunctions.matchRecordValuesVarsRegex.exec(arr[2])) {
        varLineArr = varLineArr.concat(arr2[1]).concat("\n");

        var recVar = new RecordVar();

        var arr3;
        while (arr3 = RegexFunctions.matchRecordValueVarRegex.exec(arr2[1])) {
          recVar.addData(arr3[1]);
        }

        simpleVariables.push(recVar.name);
      }
      variables = varLineArr;
    }

    return new RecordInfo(
      filePath,
      findLineNumber(module, "-record(" + name),
      name,
      simpleVariables.join("\n"),
      variables,
      vscode.TreeItemCollapsibleState.None
    );

  }
}

function findLineNumber(module:string, startsWith:string):number {
  var moduleLines = module.split('\n');
  for (let i = 0; i < moduleLines.length; i++) {
    if (moduleLines[i].startsWith(startsWith)) {
      return i;
    }
  }
  return 0;
}

export function openTextDocument(filePath:string) {
  const rootPath =
		vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0
		? vscode.workspace.workspaceFolders[0].uri.fsPath
		: undefined;
  if (rootPath !== undefined) {
    vscode.workspace.openTextDocument(path.join(rootPath, filePath)).then(document => vscode.window.showTextDocument(document));
  }
}

export function openTextDocumentAtLine(filePath:string, lineNumber:number) {
  vscode.workspace.openTextDocument(filePath)
    .then(document => vscode.window.showTextDocument(document)
      .then(editor =>{
        var pos = new vscode.Position(lineNumber,0);
        editor.selections = [new vscode.Selection(pos,pos)];
        var range = new vscode.Range(pos, pos);
        editor.revealRange(range);
      }));
}

class ModuleInfo extends vscode.TreeItem {
  constructor(
    public readonly fileName: string,
    public readonly filePath: string,
    public readonly type: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(path.join(filePath, fileName), collapsibleState);
    this.tooltip = `${this.label}-${this.type}`;
    this.description = this.type;
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'module.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'module.svg')
  };

  command = {
    "title": "Show File",
    "command": "module.show_file",
    "arguments": [path.join(this.filePath, this.fileName)]
  };
}

class ModuleData extends vscode.TreeItem {
  constructor(
      label: string,
      collapsibleState: vscode.TreeItemCollapsibleState,
      public readonly filePath:string,
      public readonly lineNumber:number) {
    super(label, collapsibleState);
  };

  command = {
    "title": "Show File Line",
    "command": "module.show_file_line",
    "arguments": [this.filePath, this.lineNumber]
  };
}

class FunctionInfo extends ModuleData {
  constructor(
    filePath: string,
    lineNumber: number,
    public readonly functionName: string,
    public readonly inputs: string[],
    public readonly guards: string,
    public readonly arity: number,
    public readonly isPublic: boolean,
    public readonly isTestFunction: boolean,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(functionName + "/" + arity, collapsibleState, filePath, lineNumber);
    this.tooltip = inputs.join("\n");
    if (guards !== "") {
      this.description = " when " + guards;
    } else {
      this.description = "";
    }
    if (isPublic) {
      this.iconPath = {
        light: path.join(__filename, '..', '..', 'images', 'light', 'public-function.svg'),
        dark: path.join(__filename, '..', '..', 'images', 'dark', 'public-function.svg')
      };
    } else if (isTestFunction) {
      this.iconPath = {
        light: path.join(__filename, '..', '..', 'images', 'light', 'test.svg'),
        dark: path.join(__filename, '..', '..', 'images', 'dark', 'test.svg')
      };
    } else {
      this.iconPath = {
        light: path.join(__filename, '..', '..', 'images', 'light', 'private-function.svg'),
        dark: path.join(__filename, '..', '..', 'images', 'dark', 'private-function.svg')
      };
    }
  }
}

class BehaviourInfo extends ModuleData {
  constructor(
    filePath: string,
    lineNumber: number,
    public readonly name: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(name, collapsibleState, filePath, lineNumber);
    this.tooltip = `${this.label}`;
    this.description = "";
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'behaviour.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'behaviour.svg')
  };
}

class TypeInfo extends ModuleData {
  constructor(
    filePath: string,
    lineNumber: number,
    public readonly name: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(name, collapsibleState, filePath, lineNumber);
    this.tooltip = `${this.label}`;
    this.description = "";
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'type.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'type.svg')
  };
}

class DefineInfo extends ModuleData {
  constructor(
    filePath: string,
    lineNumber: number,
    public readonly name: string,
    public readonly action: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(name, collapsibleState, filePath, lineNumber);
    this.tooltip = `${this.label}`;
    this.description = action;
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'define.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'define.svg')
  };
}

class RecordInfo extends ModuleData {
  constructor(
    filePath: string,
    lineNumber: number,
    public readonly name: string,
    public readonly simpleVariables: string,
    public readonly variables: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(name, collapsibleState, filePath, lineNumber);
    this.tooltip = variables;
    this.description = simpleVariables;   
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'record.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'record.svg')
  };
}

class RecordVar {

  private index: number = 0;
  public name: string = "";
  public type: string = "";
  public value: string = "";

  constructor(
  ) {}

  public addData(data:string) {
    switch (this.index) {
      case 0:
        this.name = data;
        break;
      case 1:
        this.type = data;
        break;
      case 2:
        this.value = data;
        break;
      default:
        break;
    }
    this.index++;
  }
}

function inputGetter(str:string):string[] {
  var inputs: string[] = [''];
  var inputIndex = 0;
  var bracketDepth = 0;
  var listDepth = 0;
  for (var i = 0; i < str.length; i++) {
    var char = str.charAt(i);
    if (listDepth === 0 && bracketDepth === 0 && char === ",") {
      inputIndex++;
    } else if (/\s/.test(char)) {

    } else {
      if (char === "{") {
        bracketDepth++;
      } else if (char === "}") {
        bracketDepth--;
      }
      if (char === "[") {
        listDepth++;
      } else if (char === "]") {
        listDepth--;
      }
      if (inputIndex >= inputs.length) {
        inputs.push('');
      }
      inputs[inputIndex] = inputs[inputIndex].concat(char);
    }
  }
  return inputs;
}