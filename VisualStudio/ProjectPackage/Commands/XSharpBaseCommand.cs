using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.Threading.Tasks;
using System.Diagnostics;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.Settings;
namespace Community.VisualStudio.Toolkit
{
    internal static class CVTProjectExtensions
    {
        internal static bool IsXSharp(this Project project)
        {
            if (project != null)
            {
                var path = project.FullPath;
                var ext = System.IO.Path.GetExtension(path).ToLower();
                return ext == ".xsproj" || ext == ".xsprj";
            }
            return false;
        }
    }
}
namespace XSharp.Project
{
    internal static class Commands
    {
        internal async static Task<bool> ProjectIsXSharpProjectAsync()
        {
            var project = await VS.Solutions.GetActiveProjectAsync();
            return project.IsXSharp();
        }
        internal static string GetXsPath(string subpath)
        {
            string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
            string InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
            if (string.IsNullOrEmpty(InstallPath))
            {
                REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey64;
                InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
                
            }
            return System.IO.Path.Combine(InstallPath, subpath);
        }
        internal async static System.Threading.Tasks.Task StartProcessAsync(string process, string parameters = "")
        {
            if (System.IO.File.Exists(process))
            {
                var info = new ProcessStartInfo();
                info.FileName = process;
                if (! string.IsNullOrEmpty(parameters))
                {
                    if (parameters.IndexOf(' ') > -1)
                        parameters = "\""+parameters+"\"";
                    info.Arguments = parameters;
                }
                info.WorkingDirectory = System.IO.Path.GetDirectoryName(process);
                Process.Start(info);
            }
            else
            {
                await VS.MessageBox.ShowErrorAsync("Can't show process", "Cannot find file \"" + process + "\"");
            }
        }
    }
    internal class XSharpDebuggerBaseCommand<T> : BaseCommand<T>
        where T : class, new()
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XDebuggerSettings.DebuggerMode == DebuggerMode.Break;
            base.BeforeQueryStatus(e);
        }
        protected override Task InitializeCompletedAsync()
        {
            Command.Enabled = false;
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
    }
}
