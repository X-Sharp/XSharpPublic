using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using XSharp.LanguageService.ForeignSource;
using XSharpModel;

namespace XSharp.LanguageService
{
    class XSharpDefinitionPeekItem : IPeekableItem
    {
        internal readonly IPeekResultFactory _peekResultFactory;
        // what the peek window shows: a file, a 0-based range in it, and the editor that opens it
        internal readonly string FilePath;
        internal readonly int StartLine;
        internal readonly int StartColumn;
        internal readonly int EndLine;
        internal readonly int EndColumn;
        internal readonly string Label;
        internal readonly Guid EditorGuid;

        public XSharpDefinitionPeekItem(XSourceSymbol gotoElement, IPeekResultFactory peekResultFactory)
        {
            var range = gotoElement.Range;
            FilePath = gotoElement.File.FullPath;
            StartLine = range.StartLine;
            StartColumn = range.StartColumn;
            EndLine = range.EndLine;
            EndColumn = range.EndColumn;
            Label = gotoElement.Name;
            EditorGuid = new Guid(XSharpConstants.EditorFactoryGuidString);
            _peekResultFactory = peekResultFactory;
        }

        /// <summary>A declaration in a C# or VB project of the solution, shown in its own editor.</summary>
        public XSharpDefinitionPeekItem(ForeignLocation target, IPeekResultFactory peekResultFactory)
        {
            FilePath = target.FilePath;
            StartLine = target.StartLine;
            StartColumn = target.StartColumn;
            EndLine = target.EndLine;
            EndColumn = target.EndColumn;
            Label = target.Name;
            EditorGuid = Guid.Empty;
            _peekResultFactory = peekResultFactory;
        }

        public string DisplayName
        {
            // This is unused, and was supposed to have been removed from IPeekableItem.
            get { return null; }
        }

        public IEnumerable<IPeekRelationship> Relationships
        {
            get { yield return PredefinedPeekRelationships.Definitions; }
        }

        public IPeekResultSource GetOrCreateResultSource(string relationshipName)
        {
            return new XSharpResultSource(this);
        }
    }
}
