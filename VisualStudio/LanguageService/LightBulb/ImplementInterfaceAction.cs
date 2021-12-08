using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Imaging.Interop;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using Microsoft.VisualStudio.Text;
using System.Threading;
using XSharpModel;
using System.Reflection;
using Microsoft.VisualStudio.Text.Editor;
using XSharp.LanguageService;
using System.Collections.Immutable;
using Microsoft.VisualStudio.Text.Adornments;

namespace XSharp.Project.Editors.LightBulb
{
    internal class ImplementInterfaceSuggestedAction : ISuggestedAction
    {
        private ITrackingSpan m_span;
        private string m_display;
        private ITextSnapshot m_snapshot;
        private ITextView m_textView;
        private IXTypeSymbol _classEntity;
        private Dictionary<string, List<IXMemberSymbol>> _members;
        private XSharpModel.TextRange _range;

        public ImplementInterfaceSuggestedAction(ITrackingSpan span, ITextView textView, ITextBuffer m_textBuffer, IXTypeSymbol entity, Dictionary<string, List<IXMemberSymbol>> memberstoAdd, XSharpModel.TextRange range)
        {
            m_span = span;
            m_snapshot = span.TextBuffer.CurrentSnapshot;
            m_display = string.Format("Implement Interfaces", span.GetText(m_snapshot));
            m_textView = textView;
            _classEntity = entity;
            _members = memberstoAdd;
            _range = range;
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            int count = 0;
            List<Inline> content = new List<Inline>();
            foreach (KeyValuePair<string, List<IXMemberSymbol>> mbrs in _members)
            {
                var temp = new Run("Interface " + mbrs.Key + Environment.NewLine);
                content.Add(temp);
                int max = mbrs.Value.Count;
                foreach (IXMemberSymbol mbr in mbrs.Value)
                {
                    temp = new Run(mbr.Description + Environment.NewLine);
                    content.Add(temp);
                    count++;
                    if (( count >= 3) && ( max > 3 ))
                    {
                        temp = new Run("..." + Environment.NewLine);
                        content.Add(temp);
                        temp = new Run(max.ToString() + " total members." + Environment.NewLine);
                        content.Add(temp);
                        break;
                    }
                }
            }
            var textBlock = new TextBlock();
            textBlock.Padding = new Thickness(5);
            textBlock.Inlines.AddRange(content);
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
            get { return default(ImageMoniker); }
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

        // The job is done here !!
        public void Invoke(CancellationToken cancellationToken)
        {
            //m_span.TextBuffer.Replace(m_span.GetSpan(m_snapshot), ");
            String insertText;
            ITextSnapshotLine lastLine = m_snapshot.GetLineFromLineNumber(_range.EndLine);
            List<Inline> content = new List<Inline>();
            insertText = Environment.NewLine;
            foreach (KeyValuePair<string, List<IXMemberSymbol>> mbrs in _members)
            {
                foreach (IXMemberSymbol mbr in mbrs.Value)
                {
                    insertText += mbr.Description + Environment.NewLine;
                }
                insertText += Environment.NewLine;
            }
            // Create an Edit Session
            var editSession = m_textView.TextBuffer.CreateEdit();
            try
            {
                editSession.Insert(lastLine.Start.Position, insertText);
            }
            catch (Exception e)
            {
                WriteOutputMessage("editSession : error " + e.Message);
            }
            finally
            {
                // Validate the Edit Session ?
                if (editSession.HasEffectiveChanges)
                {
                    editSession.Apply();
                }
                else
                {
                    editSession.Cancel();
                }
            }
        }


        private XSourceMemberSymbol CreateMember(MemberInfo member, MemberInfo[] members)
        {
            // NOOooooo
            return null;
        }

        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("ImplementInterfaceSuggestedAction:" + strMessage);
            }
        }


    }






}
