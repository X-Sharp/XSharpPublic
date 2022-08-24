using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Imaging.Interop;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using Microsoft.VisualStudio.Text;
using System.Threading;
using Microsoft.VisualStudio.Imaging;

namespace XSharp.LanguageService.Editors.LightBulb
{
    internal class CreateStubSuggestedAction : ISuggestedAction
    {
        private readonly ITrackingSpan m_span;
        private readonly string m_upper;
        private readonly string m_display;
        private readonly ITextSnapshot m_snapshot;

        public CreateStubSuggestedAction(ITrackingSpan span)         {
            m_span = span;
            m_snapshot = span.TextBuffer.CurrentSnapshot;
            m_upper = span.GetText(m_snapshot).ToUpper();
            m_display = string.Format("Convert '{0}' to upper case", span.GetText(m_snapshot));
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            var textBlock = new TextBlock();
            textBlock.Padding = new Thickness(5);
            textBlock.Inlines.Add(new Run() { Text = m_upper });
            return Task.FromResult<object>(textBlock);
        }

        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            return Task.FromResult<IEnumerable<SuggestedActionSet>>(null);
        }

        public bool HasActionSets
        {
            get { return false; }
        }
        public string DisplayText
        {
            get { return m_display; }
        }
        public ImageMoniker IconMoniker
        {
            get { return KnownMonikers.IntellisenseLightBulb; }
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

        public void Invoke(CancellationToken cancellationToken)
        {
            m_span.TextBuffer.Replace(m_span.GetSpan(m_snapshot), m_upper);
        }

        public override bool Equals(object obj)
        {
            return obj is CreateStubSuggestedAction action &&
                   EqualityComparer<ITrackingSpan>.Default.Equals(m_span, action.m_span);
        }
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }
}
