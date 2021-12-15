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
    [Name("Property Suggested Action")]
    [ContentType(LanguageName)]
    internal class PropertySuggestedActionsSourceProvider : ISuggestedActionsSourceProvider
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
            return new PropertySuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class PropertySuggestedActionsSource : ISuggestedActionsSource
    {
        private PropertySuggestedActionsSourceProvider m_factory;
        private ITextBuffer m_textBuffer;
        private ITextView m_textView;

        private XSourceMemberSymbol _memberEntity;
        private Dictionary<string, List<XSourceMemberSymbol>> _members;
        private XFile _xfile;
        private TextRange _range;

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public PropertySuggestedActionsSource(PropertySuggestedActionsSourceProvider propertySuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer)
        {
            this.m_factory = propertySuggestedActionsSourceProvider;
            this.m_textView = textView;
            this.m_textBuffer = textBuffer;
            //
            _memberEntity = null;
        }

#pragma warning disable VSTHRD105
        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            return Task.Factory.StartNew(() =>
            {
                return SearchField();
            });
        }
#pragma warning restore VSTHRD105
        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            // Do we have PROPERTY to Add ?
            if (SearchMissingProperty())
            {
                List<SuggestedActionSet> suggest = new List<SuggestedActionSet>();
                // Single line
                var PropAction = new PropertySuggestedAction(this.m_textView, this.m_textBuffer, this._memberEntity, this._range, false);
                suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { PropAction }));
                // Multi Line
                PropAction = new PropertySuggestedAction(this.m_textView, this.m_textBuffer, this._memberEntity, this._range, true);
                suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { PropAction }));
                //
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
        public bool SearchMissingProperty()
        {
            // Reset
            _memberEntity = null;
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
                if (entity is XSourceMemberSymbol memberEntity)
                {
                    if ((memberEntity.Kind == Kind.Field) && (memberEntity.Range.StartLine == caretLine))
                    {
                        // Got it !
                        var mbr = _xfile.FindMemberAtRow(caretLine);
                        if (mbr is XSourceMemberSymbol mbrEntity)
                        {
                            _memberEntity = mbrEntity;
                            _range = mbrEntity.Range;
                            break;
                        }
                    }
                }
            }
            // Ok, we know the Field, Check that we don't already have a Property with same name
            if (_memberEntity != null)
            {
                // Sorry... ;)
                bool alreadyExist = true;
                if (_memberEntity.Parent is XSourceTypeSymbol container)
                {
                    // Remove the first Underscore
                    string searchFor = _memberEntity.Name.Substring(1);
                    alreadyExist = false;
                    foreach (var member in container.Members)
                    {
                        if ((member.Kind == Kind.Property) && (String.Compare(member.Name, searchFor, true) == 0))
                        {
                            alreadyExist = true;
                            break;
                        }
                    }
                }
                return !alreadyExist;
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

        /// <summary>
        /// Search a potential field-ID
        /// </summary>
        /// <returns></returns>
        private bool SearchField()
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
            // It must be a SingleLineEntity
            if (lineState != LineFlags.SingleLineEntity)
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
            // Now, search for an ID, which is a Field... Here just check that we have an ID
            // Todo : Let's say it MUST START with an underscore... We may have a setting for that
            bool found = false;
            bool foundID = false;
            bool hasVisibility = false;
            foreach (var token in fulllineTokens)
            {
                if ((token.Type == XSharpLexer.PUBLIC) ||
                     (token.Type == XSharpLexer.EXPORT) ||
                      (token.Type == XSharpLexer.PROTECTED) ||
                       (token.Type == XSharpLexer.HIDDEN) ||
                       (token.Type == XSharpLexer.PRIVATE) ||
                       (token.Type == XSharpLexer.INTERNAL) ||
                        (token.Type == XSharpLexer.INSTANCE))
                {
                    hasVisibility = true;
                }
                if (hasVisibility && (token.Type == XSharpLexer.ID))
                {
                    // TODO : Use a Setting
                    if (token.Text.StartsWith("_"))
                    {
                        foundID = true;
                    }
                }
                if (hasVisibility && foundID && (token.Type == XSharpLexer.AS))
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
