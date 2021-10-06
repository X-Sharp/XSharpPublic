using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using Task = System.Threading.Tasks.Task;
using System.Diagnostics;
namespace XSharp.Project
{
    internal static class Commands
    {
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
        internal async static Task StartProcessAsync(string process, string parameters = "")
        {
            if (System.IO.File.Exists(process))
            {
                var info = new ProcessStartInfo();
                info.FileName = process;
                if (! string.IsNullOrEmpty(parameters))
                {
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
}
