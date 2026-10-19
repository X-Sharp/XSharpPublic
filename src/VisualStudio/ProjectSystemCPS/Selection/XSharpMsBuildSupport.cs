//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.IO;
using Microsoft.Win32;
using XSharp.Settings;

namespace XSharp.ProjectSystem.Selection
{
    /// <summary>
    /// Checks whether the installed X# MSBuild support files can be used by the CPS project system.
    /// </summary>
    /// <remarks>
    /// CPS projects need the design-time import in XSharp.CurrentVersion.targets (property XSharpCpsProjectSystem)
    /// and the XAML rules in the Rules folder. These are part of the X# installation, not of the VSIX. When an older
    /// X# installation is combined with this VSIX, SDK-style projects stay on MPFproj instead of loading in CPS
    /// without rules.
    /// The folder is determined like XSharp.BeforeCommon.Props does: environment variable XSharpMsBuildDir,
    /// the XSharpPath in the registry, or Program Files (x86)\XSharp\MsBuild.
    /// </remarks>
    internal static class XSharpMsBuildSupport
    {
        private static readonly Lazy<bool> supportsCps = new Lazy<bool>(CheckSupportsCps);

        public static bool SupportsCps => supportsCps.Value;

        private static bool CheckSupportsCps()
        {
            try
            {
                var folder = FindMsBuildFolder();
                if (string.IsNullOrEmpty(folder))
                {
                    XSettings.Information("XSharpMsBuildSupport: X# MsBuild folder not found, CPS disabled");
                    return false;
                }
                var targets = Path.Combine(folder, "XSharp.CurrentVersion.targets");
                var rule = Path.Combine(folder, "Rules", "XSharpCompilerCommandLineArgs.xaml");
                var supported = File.Exists(targets) && File.Exists(rule) &&
                    File.ReadAllText(targets).IndexOf("XSharpCpsProjectSystem", StringComparison.OrdinalIgnoreCase) >= 0;
                XSettings.Information("XSharpMsBuildSupport: " + folder + (supported ? " supports CPS" : " does not support CPS (X# installation too old), SDK-style projects stay on the classic project system"));
                return supported;
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
                return false;
            }
        }

        private static string FindMsBuildFolder()
        {
            var folder = Environment.GetEnvironmentVariable("XSharpMsBuildDir");
            if (!string.IsNullOrEmpty(folder) && Directory.Exists(folder))
                return folder;
            foreach (var key in new[] { @"SOFTWARE\WOW6432Node\XSharpBV\XSharp", @"SOFTWARE\XSharpBV\XSharp" })
            {
                using (var regKey = Registry.LocalMachine.OpenSubKey(key))
                {
                    if (regKey?.GetValue("XSharpPath") is string path && !string.IsNullOrEmpty(path))
                    {
                        folder = Path.Combine(path, "MsBuild");
                        if (Directory.Exists(folder))
                            return folder;
                    }
                }
            }
            folder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "XSharp", "MsBuild");
            return Directory.Exists(folder) ? folder : null;
        }
    }
}
