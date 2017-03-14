using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;

namespace XSharp.Project
{
    class XSharpDefinitionPeekItem : IPeekableItem
    {
        internal readonly IPeekResultFactory _peekResultFactory;
        internal SnapshotPoint _point;
        internal readonly ITextBuffer _textbuffer;
        internal string _fileName;

        public XSharpDefinitionPeekItem(SnapshotPoint point, IPeekResultFactory peekResultFactory, ITextBuffer textbuffer)
        {
            _point = point;
            _peekResultFactory = peekResultFactory;
            _textbuffer = textbuffer;
            _fileName = EditorHelpers.GetDocumentFileName(textbuffer);
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