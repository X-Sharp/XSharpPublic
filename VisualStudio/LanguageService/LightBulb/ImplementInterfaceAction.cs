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
using System.Text;
using System.Linq;

namespace XSharp.Project.Editors.LightBulb
{
    internal class ImplementInterfaceSuggestedAction : ISuggestedAction
    {
        private string m_interface;
        private ITextSnapshot m_snapshot;
        private ITextView m_textView;
        private IXTypeSymbol _classEntity;
        private List<IXMemberSymbol> _members;
        private XSharpModel.TextRange _range;

        public ImplementInterfaceSuggestedAction(ITextView textView, ITextBuffer m_textBuffer, string intface, IXTypeSymbol entity, List<IXMemberSymbol> memberstoAdd, XSharpModel.TextRange range)
        {
            m_snapshot = m_textBuffer.CurrentSnapshot;
            m_interface = intface;
            m_textView = textView;
            _classEntity = entity;
            _members = memberstoAdd;
            _range = range;
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            int count = 0;
            List<Inline> content = new List<Inline>();
            int max = _members.Count;
            foreach (IXMemberSymbol mbr in _members)
            {
                Run temp = new Run(this.GetDescription(mbr) + Environment.NewLine);
                content.Add(temp);
                count++;
                if ((count >= 3) && (max > 3))
                {
                    temp = new Run("..." + Environment.NewLine);
                    content.Add(temp);
                    temp = new Run(max.ToString() + " total members." + Environment.NewLine);
                    content.Add(temp);
                    break;
                }
            }
            // Create a "simple" list... Would be cool to have Colors...
            var textBlock = new TextBlock();
            textBlock.Padding = new Thickness(5);
            textBlock.Inlines.AddRange(content);
            return Task.FromResult<object>(textBlock);
        }

        private string GetDescription(IXMemberSymbol mbr)
        {
            string desc = mbr.Description;
            if ((mbr is XPEMethodSymbol) || (mbr is XPEPropertySymbol))
            {
                desc = desc.Replace(" ABSTRACT ", "");
                desc = desc.Replace(" NEW ", "");
                desc = desc.Replace(" OVERRIDE ", "");
            }
            return desc;
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
            get { return "Implement " + m_interface; }
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
            var settings = m_textView.TextBuffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
            //m_span.TextBuffer.Replace(m_span.GetSpan(m_snapshot), ");
            StringBuilder insertText;
            // Last line of the Entity
            int lineNumber = Math.Min(_range.EndLine, m_snapshot.LineCount - 1);
            ITextSnapshotLine lastLine = m_snapshot.GetLineFromLineNumber(lineNumber);
            // Retrieve the text
            string lineText = lastLine.GetText();
            // and count how many spaces we have before
            int count = lineText.TakeWhile(Char.IsWhiteSpace).Count();
            // Get these plus one as prefix
            string prefix = lineText.Substring(0, count);
            string indent = new String(' ', settings.IndentSize);
            List<Inline> content = new List<Inline>();
            // Add a comment with the Interface name ??
            insertText = new StringBuilder();
            insertText.AppendLine();
            insertText.Append(prefix);
            insertText.Append(indent);
            insertText.AppendLine("#region Implement " + m_interface);
            insertText.AppendLine();
            foreach (IXMemberSymbol mbr in _members)
            {
                // Todo : Check these
                // Add XML doc on top of generated member ? <- Could be a Setting ?
                // Add a return with default value ? <- Could be a Setting ?
                // Add a THROW NotImplementedException ? <- Could be a Setting ?
                //
                string desc = this.GetDescription(mbr);
                if (mbr.Kind.IsMethod())
                {
                    insertText.Append(prefix);
                    insertText.Append(indent);
                    insertText.AppendLine(desc);
                    insertText.Append(prefix);
                    insertText.Append(indent);
                    insertText.Append(indent);
                    insertText.AppendLine("THROW NotImplementedException{}");
                }
                else if (mbr.Kind.IsProperty())
                {
                    bool hasGet = false;
                    bool hasSet = false;
                    bool hasAuto = false;
                    string propDef = "";
                    if (mbr is XSourceMemberSymbol xsMbr)
                    {
                        propDef = xsMbr.SourceCode;
                        var keywords = propDef.Split(new char[] { ' ', '\t' });
                        hasGet = keywords.Contains("get", StringComparer.InvariantCultureIgnoreCase);
                        hasSet = keywords.Contains("set", StringComparer.InvariantCultureIgnoreCase);
                        hasAuto = keywords.Contains("auto", StringComparer.InvariantCultureIgnoreCase);
                    }
                    else if (mbr is XPEPropertySymbol peMbr)
                    {
                        hasGet = peMbr.Accessors.HasFlag(AccessorKind.Get);
                        hasSet = peMbr.Accessors.HasFlag(AccessorKind.Set);
                    }
                    insertText.Append(prefix);
                    insertText.Append(indent);
                    //insertText.Append(' '); ModVis already has a final space
                    if (hasAuto)
                    {
                        insertText.Append(mbr.ModVis);
                        insertText.AppendLine(propDef);
                    }
                    else
                    {
                        insertText.AppendLine(desc);
                        if (hasGet)
                        {
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("GET");
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("THROW NotImplementedException{}");
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("END GET");
                        }
                        if (hasSet)
                        {
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("SET");
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("THROW NotImplementedException{}");
                            insertText.Append(prefix);
                            insertText.Append(indent);
                            insertText.Append(indent);
                            insertText.AppendLine("END SET");
                        }
                        insertText.Append(prefix);
                        insertText.Append(indent);
                        insertText.Append("END PROPERTY");
                    }
                }
                insertText.AppendLine();
            }
            insertText.Append(prefix);
            insertText.Append(indent);
            insertText.AppendLine("#endregion");
            // Create an Edit Session
            var editSession = m_textView.TextBuffer.CreateEdit();
            try
            {
                // Inject code
                editSession.Insert(lastLine.Start.Position, insertText.ToString());
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
                //
            }
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
