using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using XSharpModel;

namespace XSharp.LanguageService
{
    class XSharpDefinitionPeekItem : IPeekableItem
    {
        internal readonly IPeekResultFactory _peekResultFactory;
        internal XSourceSymbol _gotoElement;

        public XSharpDefinitionPeekItem(XSourceSymbol gotoElement, IPeekResultFactory peekResultFactory)
        {
            _gotoElement = gotoElement;
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
