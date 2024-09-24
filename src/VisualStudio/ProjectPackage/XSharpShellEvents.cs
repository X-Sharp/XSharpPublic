using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.Project
{
    internal class XSharpShellEvents
    {
        static ILogger Logger => XSettings.Logger;

        static bool hasEnvironmentvariable = false;
        static XSharpShellEvents()
        {
            hasEnvironmentvariable = !string.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
            if (!hasEnvironmentvariable)
            {
                VS.MessageBox.ShowWarning("The environment variable 'XSharpMsBuildDir' is missing. \rSome projects may have problems loading. \rPlease run the XSharp setup program again.");
            }
        }

        internal XSharpShellEvents()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
            });

        }
 

        #region Project Events
    
        private void SolutionEvents_OnBeforeOpenProject(string projectFileName)
        {
            if (IsXSharpProject(projectFileName))
            {
                checkProjectFile(projectFileName);
            }
        }
        bool IsXSharpProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            return string.Equals(Path.GetExtension(fileName), ".xsproj", StringComparison.OrdinalIgnoreCase);
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        const string MsTestGuid = @"{3AC096D0-A1C2-E12C-1390-A8335801FDAB};";

        private void checkProjectFile(string fileName)
        {
            if (fileName != null && fileName.ToLower().EndsWith("xsproj") && File.Exists(fileName))
            {
                string xml = File.ReadAllText(fileName);
                var original = Path.ChangeExtension(fileName, ".original");
                bool changed = false;
                if (hasEnvironmentvariable)
                {
                    var pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                    if (pos >= 0)
                    {
                        while (pos > 0)
                        {
                            xml = xml.Substring(0, pos) + newText + xml.Substring(pos + oldText.Length);
                            pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                        }
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                        DeleteFileSafe(fileName);
                        File.WriteAllText(fileName, xml);
                        changed = true;
                    }
                }
                var testpos = xml.IndexOf(MsTestGuid, StringComparison.OrdinalIgnoreCase);
                if (testpos >= 0)
                {
                    var left = xml.Substring(0, testpos);
                    var right = xml.Substring(testpos + MsTestGuid.Length);
                    if (!changed)
                    {
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                    }
                    xml = left + right;
                    DeleteFileSafe(fileName);
                    File.WriteAllText(fileName, xml);
                    changed = true;
                }
                if (changed)
                {
                    Logger.SingleLine();
                    Logger.Information("==> Project must be upgraded: " + fileName);
                    Logger.SingleLine();

                    XSharpProjectNode.ChangedProjectFiles.Add(fileName, original);
                }
            }
        }

        public static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (File.Exists(fileName))
                {
                    File.SetAttributes(fileName, FileAttributes.Normal);
                    File.Delete(fileName);

                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "DeleteFileSafe");
                return false;
            }
            return true;

        }
        #endregion

    }
}
