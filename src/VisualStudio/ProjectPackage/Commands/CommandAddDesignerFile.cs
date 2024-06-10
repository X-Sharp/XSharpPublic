using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.CodeDom;
using System.IO;
using System.Text;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Text.RegularExpressions;
using XSharp.Settings;
namespace XSharp.Project
{
    [Command(PackageIds.idAddDesignerFile)]
    internal sealed class CommandAddDesignerFile : BaseCommand<CommandAddDesignerFile>
    {
        PhysicalFile currentFile = null;
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            currentFile = null;
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            var items = await VS.Solutions.GetActiveItemsAsync();

            bool visible = false;
            foreach (var item in items)
            {
                if (item is PhysicalFile file)
                {
                    var subtype = await file.GetAttributeAsync(ProjectFileConstants.SubType);
                    if (subtype == ProjectFileAttributeValue.Form || subtype == ProjectFileAttributeValue.UserControl)
                    {
                        visible = true;
                        currentFile = file;
                        foreach (var child in file.Children)
                        {
                            if (child.FullPath.ToLower().EndsWith(".designer.prg"))
                            {
                                visible = false;
                                currentFile = null;
                            }
                        }
                    }
                }
            }
            Command.Visible = visible;
            if (items.Count() != 1)
            {
                Command.Enabled = false;
                return;
            }
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            if (currentFile != null)
            {
                var newfile = System.IO.Path.ChangeExtension(currentFile.FullPath, ".designer.prg");
                await VS.StatusBar.ShowMessageAsync("Creating designer file:" + newfile);
                System.IO.File.WriteAllText(newfile, "");
                var project = currentFile.ContainingProject;
                XSharpProjectNode projectNode = null;
                if (project != null)
                {
                    var xproject = XSolution.FindProject(project.FullPath);
                    if (xproject != null)
                    {
                        xproject.ProjectNode.AddFileNode(newfile);
                        projectNode = (XSharpProjectNode)xproject.ProjectNode;
                    }

                    XSettings.Information($"Reading file data for {currentFile.FullPath} from database");
                    XSharpFileNode fileNode;
                    fileNode = (XSharpFileNode) projectNode.FindChild(currentFile.FullPath);
                    var provider = new VSXSharpCodeDomProvider(fileNode);
                    provider.FileName = currentFile.FullPath;
                    var source = File.ReadAllText(currentFile.FullPath);
                    var reader = new StringReader(source);
                    var ccuForm = provider.Parse(reader) as XCodeCompileUnit;
                    ccuForm.Source = source;
                    ccuForm.FileName = currentFile.FullPath;
                    ccuForm.MustWrite = true;
                    var formClass = (XCodeTypeDeclaration) XSharpCodeDomHelper.FindDesignerClass(ccuForm);
                    var designerClass = new XCodeTypeDeclaration(formClass.Name);
                    var ccuDesigner = new XCodeCompileUnit();
                    ccuDesigner.FileName = newfile;
                    CodeNamespace lastns = null;
                    foreach (CodeNamespace ns in ccuForm.Namespaces)
                    {
                        var newns = new XCodeNamespace(ns.Name);
                        ccuDesigner.Namespaces.Add(newns);
                        lastns = newns;
                        if (ns.Imports.Count> 0)
                        {
                            foreach (CodeNamespaceImport import in ns.Imports)
                            {
                                newns.Imports.Add(import);
                            }
                        }
                    }
                    if (lastns != null)
                    {
                        formClass.IsPartial = true;
                        designerClass.IsPartial = true;
                        lastns.Types.Add(designerClass);
                    }
                    foreach (CodeTypeMember obj in formClass.Members)
                    {
                        // mark the fields and the 2 methods so they goto the designer.prg
                        if (obj is CodeMemberField)
                        {
                            designerClass.Members.Add(obj);
                        }
                        else if (obj is CodeMemberMethod m)
                        {
                            if (m.Name.ToLower() == "dispose")
                            {
                                designerClass.Members.Add(obj);
                            }
                            else if (m.Name.ToLower() == "initializecomponent")
                            {
                                designerClass.Members.Add(obj);
                                if (obj.HasLeadingTrivia())
                                {
                                    var trivia = obj.GetLeadingTrivia().ToLower();
                                    if (trivia.Contains("#region"))
                                    {
                                        obj.SetEndingTrivia("\r\n#endregion\r\n");
                                    }
                                }
                            }
                        }
                    }
                    foreach (CodeTypeMember obj in designerClass.Members)
                    {
                        formClass.Members.Remove(obj);
                    }
                    var mergedccu = XSharpCodeDomHelper.MergeCodeCompileUnit(ccuForm, ccuDesigner);
                    mergedccu.DesignerUnit = ccuDesigner;
                    var writer = new StringWriter();
                    var options = new CodeGeneratorOptions();
                    provider.GenerateCodeFromCompileUnit(mergedccu, writer, options);
                    var newSource = writer.ToString().ToLower();
                    if (newSource.Contains("#endregion") && !newSource.Contains("#region"))
                    {
                        newSource = writer.ToString();
                        var lines = Regex.Split(newSource, "\r\n|\r|\n");
                        var sb = new StringBuilder();
                        foreach (var line in lines)
                        {
                            if (!line.Trim().StartsWith("#endregion",StringComparison.OrdinalIgnoreCase))
                                sb.AppendLine(line);
                        }
                        newSource = sb.ToString();
                    }
                    File.WriteAllText(currentFile.FullPath, newSource);
                }


                await VS.StatusBar.ShowMessageAsync("");
            }
        }
    }
}
