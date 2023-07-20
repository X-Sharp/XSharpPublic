using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using XSharpModel;
using XSharp.Settings;

namespace XSharp.LanguageService.Editors.LightBulb
{
    internal abstract class CommonAction
    {
        protected readonly ITextSnapshot m_snapshot;
        protected readonly ITextView m_textView;

        public CommonAction(ITextView textView)
        {
            this.m_textView = textView;
            this.m_snapshot = textView.TextSnapshot;
        }
        public bool HasActionSets
        {
            get { return false; }
        }
        public ImageMoniker IconMoniker
        {
            get { return default; }
        }
        public string IconAutomationText
        {
            get
            {
                return null;
            }
        }
        public string InputGestureText
        {
            get
            {
                return null;
            }
        }
        public bool HasPreview
        {
            get { return true; }
        }

        public void Dispose()
        {
        }
        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            // This is a sample action and doesn't participate in LightBulb telemetry  
            telemetryId = Guid.Empty;
            return false;
        }

        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableLogging)
            {
                var prefix = this.GetType().FullName;
                Logger.Information(prefix+":" + strMessage);
            }
        }
        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            return Task.FromResult<IEnumerable<SuggestedActionSet>>(null);
        }
        public abstract Task<object> GetPreviewAsync(CancellationToken cancellationToken);

        public abstract string DisplayText { get; }

        public abstract void Invoke(CancellationToken cancellationToken);
    }
}
