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
                if (item is PhysicalFile file && IsXSharpSourceFile(file))
                {
                    var subtype = await file.GetAttributeAsync(ProjectFileConstants.SubType);
                    if (string.IsNullOrEmpty(subtype) && !await Commands.ProjectIsMpfProjectAsync())
                    {
                        // SDK-style projects (CPS) rarely have a SubType
                        subtype = InferSubType(file.FullPath);
                    }
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
                        if (visible && File.Exists(XSharpCodeDomHelper.BuildDesignerFileName(file.FullPath)))
                        {
                            // a designer file that is not nested below the form
                            visible = false;
                            currentFile = null;
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

        private static bool IsXSharpSourceFile(PhysicalFile file)
        {
            return string.Equals(Path.GetExtension(file.FullPath), ".prg", StringComparison.OrdinalIgnoreCase) &&
                !file.FullPath.EndsWith(".designer.prg", StringComparison.OrdinalIgnoreCase) &&
                file.ContainingProject.IsXSharp();
        }

        /// <summary>
        /// Form or UserControl when the first class of the file inherits from one of them, directly or through
        /// its base classes. The same rule as XSharpFileNode.DetermineSubType, from the code model.
        /// </summary>
        private static string InferSubType(string path)
        {
            var xFile = XSolution.FindFile(path);
            if (xFile?.Project == null)
                return null;
            var first = XDatabase.GetTypesInFile(xFile).FirstOrDefault(c => c.TypeName != XLiterals.GlobalName);
            if (first == null)
                return null;
            var usings = xFile.Usings.Concat(xFile.StaticUsings).ToList();
            var baseType = first.BaseTypeName;
            // a limit against cycles in broken code
            for (int i = 0; i < 20 && !string.IsNullOrEmpty(baseType); i++)
            {
                var subType = XSharpFileNode.TypeNameToSubtype(baseType);
                if (!string.IsNullOrEmpty(subType))
                    return subType;
                baseType = xFile.Project.FindType(baseType, usings)?.BaseTypeName;
            }
            return null;
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            try
            {
                await AddDesignerFileAsync();
            }
            catch (Exception ex)
            {
                // The command framework would only write it to the activity log
                XSettings.Exception(ex);
                await VS.MessageBox.ShowErrorAsync(CommandTitle, ex.Message);
            }
            finally
            {
                await VS.StatusBar.ShowMessageAsync("");
            }
        }

        private const string CommandTitle = "Add .designer file";

        private async Task AddDesignerFileAsync()
        {
            if (currentFile != null)
            {
                var newfile = System.IO.Path.ChangeExtension(currentFile.FullPath, ".designer.prg");
                var project = currentFile.ContainingProject;
                var xproject = project != null ? XSolution.FindProjectByFileName(project.FullPath) : null;
                if (xproject != null)
                {
                    XSettings.Information($"Reading file data for {currentFile.FullPath} from database");
                    VSXSharpCodeDomProvider provider;
                    if (xproject.ProjectNode is XSharpProjectNode projectNode &&
                        projectNode.FindChild(currentFile.FullPath) is XSharpFileNode fileNode)
                    {
                        provider = new VSXSharpCodeDomProvider(fileNode);
                    }
                    else
                    {
                        provider = new VSXSharpCodeDomProvider(xproject, currentFile.FullPath);
                    }
                    provider.FileName = currentFile.FullPath;
                    var source = File.ReadAllText(currentFile.FullPath);
                    var reader = new StringReader(source);
                    var ccuForm = provider.Parse(reader) as XCodeCompileUnit;
                    // The parser recognizes InitializeComponent (returning System.Void) and the types in it only with
                    // the references of the project. Right after the solution was opened they may not be loaded yet;
                    // then the generated code would be wrong (':' instead of '.' for static members).
                    var formClass = ccuForm == null ? null : XSharpCodeDomHelper.FindDesignerClass(ccuForm) as XCodeTypeDeclaration;
                    if (formClass == null)
                    {
                        await VS.MessageBox.ShowErrorAsync(CommandTitle,
                            $"Could not find the form class with its InitializeComponent method in {Path.GetFileName(currentFile.FullPath)}.\n\n" +
                            "When the solution was just opened, try again after the project has finished loading.");
                        return;
                    }
                    ccuForm.Source = source;
                    ccuForm.FileName = currentFile.FullPath;
                    ccuForm.MustWrite = true;

                    await VS.StatusBar.ShowMessageAsync("Creating designer file:" + newfile);
                    System.IO.File.WriteAllText(newfile, "");
                    // MPFproj adds the designer file below the form. The CPS project system does nothing here: the
                    // SDK globs include the new file, and XSharp.SDK.Props makes it depend on the form.
                    xproject.ProjectNode.AddFileNode(newfile);

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
                    var newSource = writer.ToString();
                    // only the check is case insensitive: the form file is written as generated
                    var lowerSource = newSource.ToLower();
                    if (lowerSource.Contains("#endregion") && !lowerSource.Contains("#region"))
                    {
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
            }
        }
    }
}
