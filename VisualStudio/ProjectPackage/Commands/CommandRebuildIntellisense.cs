using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using System.IO;
namespace XSharp.Project
{
    [Command(PackageIds.idRebuildIntellisense)]
    internal sealed class CommandRebuildIntellisense : BaseCommand<CommandRebuildIntellisense>
    {
        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XSolution.HasProject;
            base.BeforeQueryStatus(e);
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            // unfortunately we cannot close the solution with File_CloseSolution
            // because that brings up a dialog
            // so we create a temporary empty solution here in the XsTemp subfolder of the users temp folder
            // and remove that solution immediately after we reopened the current solution
            // Vs is smart enough not to add this no longer existing temp solution not to the recent projects menu
            // We also need to add double quotes around the file name. The File_OpenProject command is not smart enough otherwise
            await VS.StatusBar.ShowMessageAsync("Reloading the project to rebuild the XSharp intellisense database");
            var fileName = XDatabase.FileName;
            XDatabase.DeleteOnClose = true;
            var sol = await VS.Solutions.GetCurrentSolutionAsync();
            var path = "\"" + sol.FullPath + "\"";
            var temp = createDummyProject();
            await VS.Commands.ExecuteAsync(KnownCommands.File_OpenProject, temp);
            await VS.Commands.ExecuteAsync(KnownCommands.File_OpenProject, path);
            var folder = Path.GetDirectoryName(temp);
            DeleteFolderRecursively(new DirectoryInfo(folder));
            await VS.StatusBar.ClearAsync();
        }
        private string createDummyProject()
        {
            var sb = new System.Text.StringBuilder();
            sb.AppendLine("Microsoft Visual Studio Solution File, Format Version 12.00");
            sb.AppendLine("# Visual Studio 15");
            sb.AppendLine("VisualStudioVersion = 15.0.0.0");
            sb.AppendLine("MinimumVisualStudioVersion = 10.0.40219.1");
            sb.AppendLine("Global");
            sb.AppendLine("GlobalSection(SolutionConfigurationPlatforms) = preSolution");
            sb.AppendLine("Debug | Any CPU = Debug | Any CPU");
            sb.AppendLine("Release | Any CPU = Release | Any CPU");
            sb.AppendLine("EndGlobalSection");
            sb.AppendLine("EndGlobal");
            var file = Path.Combine(Path.GetTempPath(), "XsTemp");
            DeleteFolderRecursively(new DirectoryInfo(file));
            Directory.CreateDirectory(file);
            file = Path.Combine(file, "temp.sln");
            File.WriteAllText(file, sb.ToString());
            return file;
        }
        private static void DeleteFolderRecursively(DirectoryInfo directory)
        {
            // Scan all files in the current path
            if (!directory.Exists)
                return;
            foreach (FileInfo file in directory.GetFiles())
            {
                file.Attributes &= ~FileAttributes.ReadOnly;
                file.Delete();
            }

            DirectoryInfo[] subDirectories = directory.GetDirectories();

            // Scan the directories in the current directory and call this method 
            // again to go one level into the directory tree
            foreach (DirectoryInfo subDirectory in subDirectories)
            {
                DeleteFolderRecursively(subDirectory);
            }
            directory.Attributes &= ~FileAttributes.ReadOnly;

            directory.Delete();


        }
    }
}
