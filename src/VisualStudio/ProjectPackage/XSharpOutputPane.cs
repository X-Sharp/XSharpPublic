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
using XSharp.Settings;

namespace XSharp.Project
{
    class XSharpOutputPane
    {
        static OutputWindowPane pane = null;
        static bool busy = false;

        internal static void DisplayException(Exception e, string message = "")
        {
            DisplayOutputMessage("****************************");
            DisplayOutputMessage("Exception occurred: " + message);
            DisplayOutputMessage("****************************");
            DisplayOutputMessage(e.ToString());
        }
        internal static void DisplayOutputMessage(string message)
        {
            DisplayOutputMessageAsync(message).FireAndForget(true);
        }
        internal async static Task DisplayOutputMessageAsync(string message)
        {
            if (!XSettings.EnableOutputWindowLogging)
                return;
            if (! busy && pane == null)
            {
                busy = true;
                pane = await VS.Windows.CreateOutputWindowPaneAsync("XSharp - Debug Window", false);
                busy = false;
            }
            if (pane != null)
            {
                await pane.WriteLineAsync(message);
            }

        }
    }
}
