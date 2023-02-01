using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;

namespace XSharpDebugger
{
    internal static class VsVersion
    {

        static bool _vs15 = false;
        static bool _vs16 = false;
        static bool _vs17 = false;
        internal static bool Vs15 => _vs15;
        internal static bool Vs16 => _vs16;
        internal static bool Vs17 => _vs17;
        static VsVersion()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var vers = await VS.Shell.GetVsVersionAsync();
                _vs15 = vers.Major == 15;
                _vs16 = vers.Major == 16;
                _vs17 = vers.Major == 17;
            });
        }

    }
}
