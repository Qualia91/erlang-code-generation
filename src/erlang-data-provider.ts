import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

export class RegexFunctions {

  public static matchFunctionRegex: RegExp = /^([a-z0-9_]*)\(([^)]*)?\)\s(?:when ([^-]*))?->/gms;
  public static matchFunctionInputsRegex: RegExp = /((?:[a-zA-Z0-9_]+)+),?\s?/gms;
  public static matchBehaviourRegex: RegExp = /^-behaviour\(([a-z0-9_]*)\)./gm;
  public static matchDefineRegex: RegExp = /^-define\(([^,]*),(?:\s)([^,]*)\)./gm;
  public static matchExportRegex: RegExp = /^-export\((?:\s*)\[([^\]]*)\](?:\s*)\)./gms;
  public static matchRecordRegex: RegExp = /^-record\(([a-z0-9_]*,\s*){([a-z0-9_:\(\),\s*]*)}\)./gms;
  public static matchTestFunctionsRegex: RegExp = /^([a-z0-9_]*)_test\(([^)]*)?\)\s->/gms;

}

export class ErlangDataProvider implements vscode.TreeDataProvider<vscode.TreeItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<vscode.TreeItem | undefined | void> = new vscode.EventEmitter<vscode.TreeItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<vscode.TreeItem | undefined | void> = this._onDidChangeTreeData.event;
  
  constructor(private workspaceRoot: string) {
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
    var currentFolderPath = path.join(solutionPath, projectPath);
    if (this.pathExists(currentFolderPath)) {

        let deps: vscode.TreeItem[] = [];
        var fileNames = fs.readdirSync(currentFolderPath);
        fileNames.forEach(file => {
            var ext =  file.split('.').pop();
            var fileType = "Unknown";
            switch(ext) {
              case "erl": {
                fileType = "Module";
                break; 
              }
              case "hrl": {
                fileType = "Header";
                break; 
              }
              case "escript": {
                fileType = "EScript";
                break; 
              }
              case "config": {
                fileType = "Config";
                break; 
              }
              case "src": {
                fileType = "Application Resource";
                break; 
              }
              case "app": {
                fileType = "Application Resource";
                break; 
              }
              default:
                if (ext !== undefined) {
                  fileType = ext;
                }
            }
            var stat = fs.lstatSync(path.join(currentFolderPath, file));
            if (stat.isFile()) {
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
    return this.matchInModule(RegexFunctions.matchFunctionRegex, module, this.functionMatchSwitch);
  };

  private matchInModule(structureGetter:RegExp, module:string, switchFunction:Function) : vscode.TreeItem[] {
    var items = [];
    var arr;
    while (arr = structureGetter.exec(module)) {
      items.push(switchFunction(arr));
    }
    return items;
  };

  private functionMatchSwitch(arr:RegExpExecArray): vscode.TreeItem {
    var funcMap = new Map<string, string>();
    // 1 = function name
    // 2 = inputs (Optional)
    // 3 = guards (Optional)
    var fileName = "";
    var inputs = [];
    var guards = "";
    if (arr[1] !== undefined) {
      fileName = arr[1];
    }
    if (arr[2] !== undefined) {

      if (arr[2] !== undefined) {

        var inputArr;
        while (inputArr = RegexFunctions.matchFunctionInputsRegex.exec(arr[2])) {
          inputs.push(inputArr[1]);
        }

      }
      
    }
    if (arr[3] !== undefined) {
      
      guards = arr[3];

    }

    return new FunctionInfo(
      fileName,
      inputs,
      guards,
      inputs.length,
      vscode.TreeItemCollapsibleState.None
    );

  }
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
    light: path.join(__filename, '..', '..', 'images', 'light', 'document.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'document.svg')
  };
}

class FunctionInfo extends vscode.TreeItem {
  constructor(
    public readonly functionName: string,
    public readonly inputs: string[],
    public readonly guards: string,
    public readonly arity: number,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(functionName + "/" + arity, collapsibleState);
    this.tooltip = `${this.label}`;
    if (guards !== "") {
      this.description = inputs.toString().replace(/,/g, ", ") + " when " + guards;
    } else {
      this.description = inputs.toString().replace(/,/g, ", ");
    }
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'images', 'light', 'document.svg'),
    dark: path.join(__filename, '..', '..', 'images', 'dark', 'document.svg')
  };
}