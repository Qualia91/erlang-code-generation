import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

export class ErlangDataProvider implements vscode.TreeDataProvider<ModuleInfo> {

	private _onDidChangeTreeData: vscode.EventEmitter<ModuleInfo | undefined | void> = new vscode.EventEmitter<ModuleInfo | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<ModuleInfo | undefined | void> = this._onDidChangeTreeData.event;

  readonly matchFunctionRegex: RegExp = /^([a-z0-9_]*)\(([^)]*)?\)\s(?:when ([^-]*))?->/gms;
  readonly matchBehaviourRegex: RegExp = /^-behaviour\(([a-z0-9_]*)\)./gm;
  readonly matchDefineRegex: RegExp = /^-define\(([^,]*),(?:\s)([^,]*)\)./gm;
  readonly matchExportRegex: RegExp = /^-export\((?:\s*)\[([^\]]*)\](?:\s*)\)./gms;
  readonly matchRecordRegex: RegExp = /^-record\(([a-z0-9_]*,\s*){([a-z0-9_:\(\),\s*]*)}\)./gms;
  readonly matchTestFunctionsRegex: RegExp = /^([a-z0-9_]*)_test\(([^)]*)?\)\s->/gms;
  
  constructor(private workspaceRoot: string) {
  }

  refresh(): void {
		this._onDidChangeTreeData.fire();
	}

  getTreeItem(element: ModuleInfo): vscode.TreeItem {
    return element;
  }

  getChildren(element?: ModuleInfo): Thenable<ModuleInfo[]> {
    if (!this.workspaceRoot) {
      vscode.window.showInformationMessage('Project in workspace');
      return Promise.resolve([]);
    }

    if (element) {
      return Promise.resolve(
        this.readFileForMetadata(
          path.join(this.workspaceRoot, path.join(element.filePath, element.fileName))
        )
      );
    } else {
      return Promise.resolve(this.readFilesInFolder(this.workspaceRoot, ""));
    }
  }

  private readFilesInFolder(solutionPath: string, projectPath:string): ModuleInfo[] {
    var currentFolderPath = path.join(solutionPath, projectPath);
    if (this.pathExists(currentFolderPath)) {

        let deps: ModuleInfo[] = [];
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

  private readFileForMetadata(filePath:string): ModuleInfo[] {
    var module = fs.readFileSync(filePath,'utf8');
    this.matchInModule(this.matchFunctionRegex, module, this.functionMatchSwitch);
    return [];
  };

  private matchInModule(structureGetter:RegExp, module:string, switchFunction:Function) : void {
    var arr;
    while (arr = structureGetter.exec(module)) {
      switchFunction(arr);
    }
  };

  private functionMatchSwitch(arr:RegExpExecArray) {
    var funcMap = new Map<string, string>();
      // 1 = function name
      // 2 = inputs (Optional)
      // 3 = guards (Optional)
      console.log("Next match");
      if (arr[1] !== undefined) {
        console.log(arr[1]);
      }
      if (arr[2] !== undefined) {
        console.log(arr[2]);
      }
      if (arr[3] !== undefined) {
        console.log(arr[3]);
      }
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
    light: path.join(__filename, '..', '..', 'resources', 'light', 'dependency.svg'),
    dark: path.join(__filename, '..', '..', 'resources', 'dark', 'dependency.svg')
  };
}