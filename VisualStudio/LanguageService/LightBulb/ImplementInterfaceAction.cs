using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using Microsoft.VisualStudio.Text;
using System.Threading;
using XSharpModel;
using Microsoft.VisualStudio.Text.Editor;
using System.Collections.Immutable;
using System.Text;
using System.Linq;

namespace XSharp.LanguageService.Editors.LightBulb
{
    internal class ImplementInterfaceSuggestedAction : CommonAction, ISuggestedAction
    {
        private string m_interface;
        private IXTypeSymbol _classEntity;
        private List<IXMemberSymbol> _members;
        private XSharpModel.TextRange _range;
        private bool _explicitly;

        public ImplementInterfaceSuggestedAction(ITextView textView, ITextBuffer m_textBuffer, string intface,
            IXTypeSymbol entity, List<IXMemberSymbol> memberstoAdd, XSharpModel.TextRange range, bool fqn)
            : base(textView)
        {
            m_interface = intface;
            _classEntity = entity;
            _members = memberstoAdd;
            _range = range;
            _explicitly = fqn;
        }

        public override Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            int count = 0;
            List<Inline> content = new List<Inline>();
            int max = _members.Count;
            foreach (IXMemberSymbol mbr in _members)
            {
                string desc = this.GetModVis(mbr);
                if (_explicitly)
                    desc += m_interface + ".";
                desc += mbr.Prototype;
                //
                Run temp = new Run( desc + Environment.NewLine);
                content.Add(temp);
                count++;
                if ((count >= 3) && (max > 3))
                {
                    temp = new Run("..." + Environment.NewLine);
                    content.Add(temp);
                    temp = new Run(max.ToString() + " members." + Environment.NewLine);
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

        private string GetModVis(IXMemberSymbol mbr)
        {
            string desc = mbr.ModVis.ToUpper();
            if (mbr is IXMemberSymbol)
            {
                desc = desc.Replace(" ABSTRACT ", " ");
                desc = desc.Replace(" NEW ", " ");
                desc = desc.Replace(" OVERRIDE ", " ");
                desc = desc.Replace("  ", " ");
            }
            return desc;
        }

 
          public override string DisplayText
        {
            get
            {
                if (!_explicitly)
                {
                    return "Implement " + m_interface;
                }
                else
                {
                    return "Implement explicitly " + m_interface;
                }
            }
        }
  
   
        // The job is done here !!
        public override void Invoke(CancellationToken cancellationToken)
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
                string desc = this.GetModVis(mbr);
                if (_explicitly)
                    desc += m_interface + ".";
                if (mbr.Kind.IsMethod())
                {
                    desc += "METHOD ";
                }
                else if (mbr.Kind.IsProperty())
                {
                    desc += "PROPERTY ";
                }
                desc += mbr.Prototype;
                //
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
                        if (_explicitly)
                            insertText.Append(m_interface + ".");
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
                        insertText.AppendLine("END PROPERTY");
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

    }

}
