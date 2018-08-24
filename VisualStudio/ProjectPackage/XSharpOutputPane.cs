using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Task = System.Threading.Tasks.Task;
namespace XSharp.Project
{
    class XSharpOutputPane
    {
        private static readonly Guid XSharpOutputWindowPane = new Guid("{58980A02-889C-4AC6-8321-D55E92976A70}");
        internal static void DisplayOutPutMessage(string message)
        {
            IVsOutputWindowPane outputPane = null;
            var outputWindow = ServiceProvider.GlobalProvider.GetService(typeof(SVsOutputWindow)) as IVsOutputWindow;
            if (outputWindow != null && ErrorHandler.Failed(outputWindow.GetPane(XSharpOutputWindowPane, out outputPane)))
            {
                IVsWindowFrame windowFrame;
                var vsUiShell = ServiceProvider.GlobalProvider.GetService(typeof(SVsUIShell)) as IVsUIShell;
                uint flags = (uint)__VSFINDTOOLWIN.FTW_fForceCreate;
                vsUiShell.FindToolWindow(flags, VSConstants.StandardToolWindows.Output, out windowFrame);
                windowFrame.Show();

                outputWindow.CreatePane(XSharpOutputWindowPane, "XSharp - Debug Window", 1, 1);
                outputWindow.GetPane(XSharpOutputWindowPane, out outputPane);
                outputPane.Activate();
            }
            message = DateTime.Now.ToString("hh:mm:ss.fff") + " "+message + "\r\n";
            outputPane?.OutputStringThreadSafe(message);

        }
    }
}
