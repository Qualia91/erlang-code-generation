import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

export class ErlangDataProvider implements vscode.TreeDataProvider<ModuleInfo> {
  constructor(private workspaceRoot: string) {
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
        this.readFilesInFolder(
          path.join(this.workspaceRoot, element.fileName)
        )
      );
    } else {
      return Promise.resolve(this.readFilesInFolder(this.workspaceRoot));
    }
  }

  /**
   * Given the path to package.json, read all its dependencies and devDependencies.
   */
  private readFilesInFolder(solutionPath: string): ModuleInfo[] {
    if (this.pathExists(solutionPath)) {

        let deps: ModuleInfo[] = [];
        var fileNames = fs.readdirSync(solutionPath);
        fileNames.forEach(file => {
            var stat = fs.lstatSync(path.join(solutionPath, file));
            if (stat.isFile()) {
                deps.push(new ModuleInfo(file, "File", vscode.TreeItemCollapsibleState.Collapsed));
            } else if (stat.isDirectory()) {
                var items = this.readFilesInFolder(path.join(solutionPath, file));
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
}

class ModuleInfo extends vscode.TreeItem {
  constructor(
    public readonly fileName: string,
    public readonly type: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState
  ) {
    super(fileName, collapsibleState);
    this.tooltip = `${this.label}-${this.type}`;
    this.description = this.type;
  }

  iconPath = {
    light: path.join(__filename, '..', '..', 'resources', 'light', 'dependency.svg'),
    dark: path.join(__filename, '..', '..', 'resources', 'dark', 'dependency.svg')
  };
}