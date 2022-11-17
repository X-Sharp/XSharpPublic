using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using static XSharp.XSharpConstants;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using System.Threading.Tasks;
using System.Threading;
using XSharpModel;


namespace XSharp.LanguageService.Editors.LightBulb
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [Name("Constructor Suggested Action")]
    [ContentType(LanguageName)]
    internal class ConstructorSuggestedActionsSourceProvider : CommonActionProvider, ISuggestedActionsSourceProvider
    {
        [Import(typeof(ITextStructureNavigatorSelectorService))]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            if (XEditorSettings.DisableLightBulb)
                return null;
            if (textBuffer == null && textView == null)
            {
                return null;
            }
            return new ConstructorSuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class ConstructorSuggestedActionsSource : CommonActionsSource, ISuggestedActionsSource
    {
        private ConstructorSuggestedActionsSourceProvider m_factory;

        private IXTypeSymbol _classEntity;
        private List<IXMemberSymbol> _fieldNProps;
        private List<String> _existingCtor;
        private int _insertionLine;
        private bool _hasDefault;

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public ConstructorSuggestedActionsSource(ConstructorSuggestedActionsSourceProvider ctorSuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer) : base(textView, textBuffer)
        {
            this.m_factory = ctorSuggestedActionsSourceProvider;
            //
            _classEntity = null;
            _existingCtor = new List<string>();
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            // Return False : The Bulb won't appear; but when called by the Menu we may return some actions
            return Task.Run(() =>
            {
                return false;
            });
        }

        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (!XEditorSettings.DisableLightBulb)
            {
                try
                {
                    if (SearchCtor())
                    {
                        List<SuggestedActionSet> suggest = new List<SuggestedActionSet>();
                        if ((!_hasDefault) || (_fieldNProps?.Count > 0))
                        {
                            if (!_hasDefault)
                            {
                                var ctorAction = new ConstructorSuggestedAction(this.m_textView, this.m_textBuffer, this._classEntity, this._insertionLine, null);
                                suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { ctorAction }));
                            }

                            if (_fieldNProps?.Count > 0)
                            {
                                var ctorAction = new ConstructorSuggestedAction(this.m_textView, this.m_textBuffer, this._classEntity, this._insertionLine, _fieldNProps, _existingCtor);
                                suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { ctorAction }));
                            }
                            return suggest.ToArray();
                        }
                    }
                }
                catch (Exception e)
                {
                    WriteOutputMessage(e.Message);
                }
            }
            return Enumerable.Empty<SuggestedActionSet>();
        }


        /// <summary>
        /// Retrieve the Entity, and check if it has an Interface and if some members are missing
        /// </summary>
        /// <returns></returns>
        public bool SearchCtor()
        {
            // Reset
            _classEntity = null;
            _fieldNProps = null;
            _existingCtor = new List<string>();
            // Sorry, we are lost...
            if (! GetFile())
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
                    if ((typeEntity.Range.StartLine <= caretLine) && (typeEntity.Range.EndLine >= caretLine) && ( typeEntity.Kind == Kind.Class ))
                    {
                        // Got it !
                        _classEntity = _xfile.FindType(typeEntity.FullName);
                        if (_classEntity != null)
                        {
                            _insertionLine = this.SearchInsertionPoint(typeEntity);
                            break;
                        }
                    }
                }
            }
            // Ok, we know the class, now does it have an empty Constructor ? does the class have fields ? or Properties ?
            if (_classEntity != null)
            {
                _fieldNProps = BuildMissingCtor();
                //
                return ((!_hasDefault) || (_fieldNProps != null));
            }
            return false;
        }

        private int SearchInsertionPoint(XSourceTypeSymbol typeEntity)
        {
            int insertAt = Math.Min(typeEntity.Range.EndLine, this.m_textView.TextSnapshot.LineCount - 1);
            foreach ( XSourceMemberSymbol mbr in typeEntity.XMembers )
            {
                if ( (mbr.Kind == Kind.Constructor) || (mbr.Kind == Kind.Method) || (mbr.Kind == Kind.Property))
                {
                    insertAt = mbr.Range.StartLine;
                    break;
                }
            }
            return insertAt;
        }

        private List<IXMemberSymbol> BuildMissingCtor()
        {
            List<IXMemberSymbol> toAdd = new List<IXMemberSymbol>();
            //
            if (_classEntity.Members.Count == 0)
            {
                _hasDefault = false;
                return null;
            }
            //
            _hasDefault = false;
            foreach (IXMemberSymbol mbr in _classEntity.Members)
            {
                //
                if (mbr.Kind == Kind.Constructor)
                {
                    if (mbr.Parameters.Count == 0)
                    {
                        _hasDefault = true;
                    }
                    else
                    {
                        string ctorDef = "";
                        int max = mbr.Parameters.Count;
                        foreach (var param in mbr.Parameters)
                        {
                            ctorDef += param.TypeName;
                            max--;
                            if (max > 0)
                                ctorDef += ",";
                        }
                        _existingCtor.Add(ctorDef);
                    }

                }
                else if ((mbr.Kind == Kind.Field) || (mbr.Kind == Kind.Property))
                {
                    toAdd.Add(mbr);
                }
            }
            //
            if (toAdd.Count == 0)
                return null;
            return toAdd;
        }

    }
}
