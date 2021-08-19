using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idWebsite)]
    internal sealed class CommandXsWebsite : BaseCommand<CommandXsWebsite>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            var webservice = await VS.GetServiceAsync<SVsWebBrowsingService, IVsWebBrowsingService>();
            if (webservice != null)
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                string url = "https://www.xsharp.eu";
                IVsWindowFrame frame = null;
                webservice.Navigate(url, (uint)(__VSWBNAVIGATEFLAGS.VSNWB_WebURLOnly | __VSWBNAVIGATEFLAGS.VSNWB_ForceNew), out frame);
                frame.Show();
            }
        }
    }
}
