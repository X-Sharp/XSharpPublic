#if REPOWINDOW
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    public class RepositoryWindow : BaseToolWindow<RepositoryWindow>
    {
        public override string GetTitle(int toolWindowId) => "RepositoryWindow";

        public override Type PaneType => typeof(Pane);

        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Version version = await VS.Shell.GetVsVersionAsync();
            return new RepositoryWindowControl();
        }

        [Guid("081ecd3c-1ca5-4ed9-be98-40d301598c34")]
        internal class Pane : ToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.StatusInformation;
            }
        }
    }
}
#endif
