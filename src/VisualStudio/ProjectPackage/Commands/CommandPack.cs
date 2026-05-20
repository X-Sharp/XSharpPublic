using Community.VisualStudio.Toolkit;

using System;
using System.IO;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idPackProject)]
    internal sealed class CommandPack : CommandBuild<CommandPack>
    {
        protected override string CommandName => "Build.PackSelection";
        protected override string CommandDescription => "Pack";
        public static readonly Guid CommandGroup = new Guid("{568ABDF7-D522-474D-9EED-34B5E5095BA5}");
        public const int CommandID = 8193;

        protected override async Task DoCmdAsync()
        {
            if (!await VerifySdkProjectAsync())
            {
                return;
            }
            // Show confirmation dialog
            var result = await VS.MessageBox.ShowAsync(
                "Create NuGet Package",
                "This will create a NuGet package from the project using 'dotnet pack'.\n\n" +
                "The package will be created in the project's bin folder.\n\n" +
                "Continue?",
                Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_QUERY,
                Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OKCANCEL);

            if (result == Microsoft.VisualStudio.VSConstants.MessageBoxResult.IDOK)
            {
                await PackProjectAsync(projectPath);
            }
        }

        private async Task PackProjectAsync(string projectPath)
        {
            try
            {
                await VS.StatusBar.ShowMessageAsync("Creating NuGet package...");

                // Build dotnet pack command
                var arguments = $"pack \"{projectPath}\" -c Release";

                var result = await CreateProcessAsync(arguments,
                    $"Creating NuGet package for: {Path.GetFileName(projectPath)}");

                if (result == 0)
                {
                    string packagePath = null;
                    var message = "NuGet package created successfully.";
                    if (!string.IsNullOrEmpty(packagePath))
                    {
                        message += $"\n\nPackage location:\n{packagePath}";
                    }
                    await VS.MessageBox.ShowAsync(CommandDescription,
                        message,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_INFO,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OK);
                }
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync(CommandDescription+" Error", $"Failed to create NuGet package:\n{ex.Message}");
            }
        }
    }
}
