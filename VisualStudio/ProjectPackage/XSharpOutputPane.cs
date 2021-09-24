using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Task = System.Threading.Tasks.Task;
using Microsoft;
using Community.VisualStudio.Toolkit;

namespace XSharp.Project
{
    class XSharpOutputPane
    {
        static OutputWindowPane pane;

        internal static void DisplayException(Exception e)
        {
            DisplayOutputMessage(e.ToString());
        }
        internal static void DisplayOutputMessage(string message)
        {
            DisplayOutputMessageAsync(message).FireAndForget(true);
        }
        internal async static Task DisplayOutputMessageAsync(string message)
        {
            if (!XSharpModel.XSettings.EnableLogging)
                return;
            if (pane == null)
            {
                pane = await VS.Windows.CreateOutputWindowPaneAsync("XSharp - Debug Window", false);
            }
            pane.WriteLineAsync(message).FireAndForget(true);

        }
    }
}
