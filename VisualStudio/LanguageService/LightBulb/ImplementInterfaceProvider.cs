//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using XSharpModel;
using static XSharp.XSharpConstants;

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
            if (XEditorSettings.DisableLightBulb)
                return null;
            if (textBuffer == null && textView == null)
            {
                return null;
            }
            return new ImplementInterfaceSuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class ImplementInterfaceSuggestedActionsSource : CommonActionsSource, ISuggestedActionsSource
    {
        private ImplementInterfaceSuggestedActionsSourceProvider m_factory;

        private IXTypeSymbol _classEntity;
        private Dictionary<string, List<IXMemberSymbol>> _members;
        private TextRange _range;


#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public ImplementInterfaceSuggestedActionsSource(ImplementInterfaceSuggestedActionsSourceProvider ImplementInterfaceSuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer)
            : base(textView, textBuffer)
        {
            this.m_factory = ImplementInterfaceSuggestedActionsSourceProvider;
            //
            _classEntity = null;
        }

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

            return Task.Run(() =>
            {
                return SearchMissingMembers();
            });
        }
        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (!XEditorSettings.DisableLightBulb)
            {
                // Do we have members to Add ?
                if (SearchMissingMembers())
                {
                    List<SuggestedActionSet> suggest = new List<SuggestedActionSet>();
                    foreach (KeyValuePair<string, List<IXMemberSymbol>> intfaces in _members)
                    {
                        // "Simple" Name
                        var ImplementInterfaceAction = new ImplementInterfaceSuggestedAction(this.m_textView, this.m_textBuffer, intfaces.Key, this._classEntity, intfaces.Value, this._range, false);
                        suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { ImplementInterfaceAction }));
                        // "Fully Qualified Name"
                        ImplementInterfaceAction = new ImplementInterfaceSuggestedAction(this.m_textView, this.m_textBuffer, intfaces.Key, this._classEntity, intfaces.Value, this._range, true);
                        suggest.Add(new SuggestedActionSet(new ISuggestedAction[] { ImplementInterfaceAction }));

                    }

                    return suggest.ToArray();
                }
            }
            return Enumerable.Empty<SuggestedActionSet>();
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
            if (!GetFile())
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




        private Dictionary<string, List<IXMemberSymbol>> BuildMissingMembers()
        {
            Dictionary<string, List<IXMemberSymbol>> toAdd = new Dictionary<string, List<IXMemberSymbol>>();
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
                        List<IXMemberSymbol> elementsToAdd = new List<IXMemberSymbol>();
                        // Search all Interface Members
                        foreach (IXMemberSymbol mbr in iftype.Members)
                        {
                            bool found = false;
                            // Is it already in classEntity ?
                            foreach (IXMemberSymbol entityMbr in _classEntity.Members)
                            {
                                if (mbr.Kind == entityMbr.Kind)
                                {
                                    if (String.Compare(this.GetModVis(mbr), this.GetModVis(entityMbr), true) == 0)
                                    {
                                        // Just the "simple" Name
                                        if (String.Compare(mbr.Prototype, entityMbr.Prototype, true) == 0)
                                        {
                                            found = true;
                                            break;
                                        }
                                        // Or it could be the Fully Qualified Name
                                        else if (String.Compare(iftype.Name + "." + mbr.Prototype, entityMbr.Prototype, true) == 0)
                                        {
                                            found = true;
                                            break;
                                        }
                                    }
                                }
                                //if (String.Compare(this.GetDescription(mbr), this.GetDescription(entityMbr), true) == 0)
                                //{
                                //    found = true;
                                //    break;
                                //}
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
            var xDocument = m_textBuffer.GetDocument();
            if (xDocument == null)
            {
                return false;
            }
            var linesState = xDocument.LineState;
            //
            var xLines = xDocument.TokensPerLine;
            //
            var fulllineTokens = new List<IToken>();
            var lineNumber = SearchRealStartLine();
            linesState.Get(lineNumber, out var lineState);
            // It must be a EntityStart
            if (lineState != LineFlags.EntityStart)
                return false;
            if (!xLines.TryGetValue(lineNumber, out var lineTokens))
                return false;
            //
            fulllineTokens.AddRange(lineTokens);
            // Check if the following line is continuing this one
            do
            {
                lineNumber++;
                linesState.Get(lineNumber, out lineState);
                if (lineState.HasFlag(LineFlags.Continued))
                {
                    xLines.TryGetValue(lineNumber, out lineTokens);
                    if (lineTokens != null)
                    {
                        fulllineTokens.AddRange(lineTokens);
                        continue;
                    }
                }
            } while (lineState.HasFlag(LineFlags.Continued));
            // Now, search for IMPLEMENT
            bool found = false;
            foreach (var token in fulllineTokens)
            {
                if (token.Type == XSharpLexer.IMPLEMENTS)
                {
                    found = true;
                    break;
                }
            }
            return found;
        }

        private string GetModVis(IXMemberSymbol mbr)
        {
            string desc = mbr.ModVis;
            if ((mbr is XPEMethodSymbol) || (mbr is XPEPropertySymbol))
            {
                desc = desc.Replace(" ABSTRACT ", "");
                desc = desc.Replace(" NEW ", "");
                desc = desc.Replace(" OVERRIDE ", "");
            }
            return desc;
        }

    }
}
