using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using static XSharp.XSharpConstants;
using XSharp.LanguageService;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using System.Threading.Tasks;
using System.Threading;
using System.Collections.Immutable;
using Microsoft.VisualStudio.Text.Formatting;
using XSharpModel;
using System.Reflection;
using XSharp.Project.Editors.LightBulb;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace XSharp.LanguageService.Editors.LightBulb
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [Name("Implement Interface Suggested Action")]
    [ContentType(LanguageName)]
    internal class ImplementInterfaceSuggestedActionsSourceProvider : ISuggestedActionsSourceProvider
    {
        [Import(typeof(ITextStructureNavigatorSelectorService))]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            if (XSettings.DisableLightBulb)
                return null;
            if (textBuffer == null && textView == null)
            {
                return null;
            }
            return new ImplementInterfaceSuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class ImplementInterfaceSuggestedActionsSource : ISuggestedActionsSource
    {
        private ImplementInterfaceSuggestedActionsSourceProvider m_factory;
        private ITextBuffer m_textBuffer;
        private ITextView m_textView;

        private IXTypeSymbol _classEntity;
        private Dictionary<string, List<XSourceMemberSymbol>> _members;
        private XFile _xfile;
        private TextRange _range;

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public ImplementInterfaceSuggestedActionsSource(ImplementInterfaceSuggestedActionsSourceProvider ImplementInterfaceSuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer)
        {
            this.m_factory = ImplementInterfaceSuggestedActionsSourceProvider;
            this.m_textView = textView;
            this.m_textBuffer = textBuffer;
            //
            _classEntity = null;
        }

#pragma warning disable VSTHRD105
        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            //return Task.Factory.StartNew(() =>
            //{
            //    TextExtent extent;
            //    if (TryGetWordUnderCaret(out extent))
            //    {
            //        // don't display the action if the extent has whitespace  
            //        return extent.IsSignificant;
            //    }
            //    return false;
            //});

            return Task.Factory.StartNew(() =>
            {
                return SearchImplement();
            });
        }
#pragma warning restore VSTHRD105
        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            // Do we have members to Add ?
            if (SearchMissingMembers())
            {
                List<SuggestedActionSet> suggest = new List<SuggestedActionSet>();
                foreach (KeyValuePair<string, List<XSourceMemberSymbol>> intfaces in _members)
                {
                    var ImplementInterfaceAction = new ImplementInterfaceSuggestedAction(this.m_textView, this.m_textBuffer, intfaces.Key, this._classEntity, intfaces.Value, this._range);
                    suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { ImplementInterfaceAction }));
                }

                return suggest.ToArray();
            }
            return Enumerable.Empty<SuggestedActionSet>();
        }


        public void Dispose()
        {
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            // This is a sample provider and doesn't participate in LightBulb telemetry  
            telemetryId = Guid.Empty;
            return false;
        }

        /// <summary>
        /// Retrieve the Entity, and check if it has an Interface and if some members are missing
        /// </summary>
        /// <returns></returns>
        public bool SearchMissingMembers()
        {
            // Reset
            _classEntity = null;
            _members = null;
            // Sorry, we are lost...
            _xfile = m_textBuffer.GetFile();
            if (_xfile == null)
                return false;
            //
            int caretLine = SearchRealStartLine();
            if (caretLine < 0)
                return false;
            //
            foreach (var entity in _xfile.EntityList)
            {
                if (entity is XSourceTypeSymbol typeEntity)
                {
                    if (typeEntity.Range.StartLine == caretLine)
                    {
                        // Got it !
                        _classEntity = _xfile.FindType(typeEntity.FullName);
                        if (_classEntity != null)
                        {
                            _range = typeEntity.Range;
                            break;
                        }
                    }
                }
            }
            // Ok, we know the class, now does it have an Interface..and does it need it ?
            if (_classEntity != null)
            {
                _members = BuildMissingMembers();
                if (_members != null)
                    return true;
            }
            return false;
        }


        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.LightBulb:" + strMessage);
            }
        }

        private Dictionary<string, List<XSourceMemberSymbol>> BuildMissingMembers()
        {
            Dictionary<string, List<XSourceMemberSymbol>> toAdd = new Dictionary<string, List<XSourceMemberSymbol>>();
            //
            if (_classEntity.Interfaces.Count == 0)
                return null;
            //
            foreach (string iface in _classEntity.Interfaces)
            {
                // Search The interface
                // --> Default NameSpace
                var iftype = _xfile.FindType(iface.Trim());
                if (iftype != null)
                {
                    if (iftype.Kind == Kind.Interface)
                    {
                        List<XSourceMemberSymbol> elementsToAdd = new List<XSourceMemberSymbol>();
                        // Search all Interface Members
                        foreach (XSourceMemberSymbol mbr in iftype.Members)
                        {
                            bool found = false;
                            // Is it already in classEntity ?
                            foreach (IXMemberSymbol entityMbr in _classEntity.Members)
                            {
                                if (String.Compare(mbr.Description, entityMbr.Description, true) == 0)
                                {
                                    found = true;
                                    break;
                                }
                            }
                            // No, Add 
                            if (!found)
                            {
                                // No
                                elementsToAdd.Add(mbr);
                            }
                        }
                        if ((elementsToAdd.Count > 0) && !toAdd.ContainsKey(iftype.Name))
                        {
                            toAdd.Add(iftype.Name, elementsToAdd);
                        }
                    }
                }
            }
            //
            if (toAdd.Count == 0)
                return null;
            return toAdd;
        }

        private bool SearchImplement()
        {
            if (m_textBuffer.Properties == null)
                return false;
            //
            XSharpLineState linesState = null;
            if (!m_textBuffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out linesState))
            {
                return false;
            }
            //
            XSharpTokens xTokens;
            if (!m_textBuffer.Properties.TryGetProperty(typeof(XSharpTokens), out xTokens))
            {
                return false;
            }
            if (!xTokens.Complete)
                return false;
            //
            var xLines = xTokens.Lines;
            //
            SnapshotPoint caret = this.m_textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            //
            IList<XSharpToken> lineTokens = null;
            List<XSharpToken> fulllineTokens = new List<XSharpToken>();
            var lineNumber = line.LineNumber;
            var lineState = linesState.GetFlags(lineNumber);
            // Search the first line
            while (lineState == LineFlags.Continued)
            {
                // Move back
                lineNumber--;
                lineState = linesState.GetFlags(lineNumber);
            }
            // It must be a EntityStart
            if (lineState != LineFlags.EntityStart)
                return false;
            if (!xLines.TryGetValue(lineNumber, out lineTokens))
                return false;
            //
            fulllineTokens.AddRange(lineTokens);
            // Check if the following line is continuing this one
            do
            {
                lineNumber++;
                lineState = linesState.GetFlags(lineNumber);
                if (lineState == LineFlags.Continued)
                {
                    xLines.TryGetValue(lineNumber, out lineTokens);
                    if (lineTokens != null)
                    {
                        fulllineTokens.AddRange(lineTokens);
                        continue;
                    }
                }
            } while (lineState == LineFlags.Continued);
            // Now, search for IMPLEMENT
            bool found = false;
            foreach( var token in fulllineTokens)
            {
                if ( token.Type == XSharpLexer.IMPLEMENTS )
                {
                    found = true;
                    break;
                }
            }
            return found;
        }

        /// <summary>
        /// Based on the Caret line position, check if this is a continuig line
        /// </summary>
        /// <returns></returns>
        private int SearchRealStartLine()
        {
            //
            XSharpLineState linesState = null;
            if (!m_textBuffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out linesState))
            {
                return -1;
            }
            //
            SnapshotPoint caret = this.m_textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            //
            var lineNumber = line.LineNumber;
            var lineState = linesState.GetFlags(lineNumber);
            // Search the first line
            while (lineState == LineFlags.Continued)
            {
                // Move back
                lineNumber--;
                lineState = linesState.GetFlags(lineNumber);
            }
            return lineNumber;
        }

    }
}
