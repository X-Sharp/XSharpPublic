using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Threading;

namespace XSharpDebugger
{
    public static class VsVersion
    {
        static VsVersion()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                vers = await VS.Shell.GetVsVersionAsync();
            });
        }
        static Version vers ;
        internal static bool Vs15 => vers.Major == 15;
        internal static bool Vs16 => vers.Major == 16;
        internal static bool Vs17 => vers.Major == 17;
    }
}
