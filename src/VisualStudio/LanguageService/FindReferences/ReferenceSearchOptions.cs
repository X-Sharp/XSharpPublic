//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>What a search for references returns besides the places that surely refer to the target.</summary>
    internal sealed class ReferenceSearchOptions
    {
        /// <summary>Sends o:Name that the lookup cannot resolve and symbols #Name, in a group of their own</summary>
        internal bool IncludeLateBound { get; set; }
        /// <summary>
        /// Everything that may refer to the target: late-bound sends, symbols, unresolved names, ambiguous overloads,
        /// inactive code and files that could not be read. Rename reports them instead of changing them.
        /// </summary>
        internal bool IncludeUncertain { get; set; }
        /// <summary>The name as a whole word in comments and/or strings</summary>
        internal TextTokenKinds IncludeText { get; set; }
        /// <summary>Also projects that reference the target's project through other projects</summary>
        internal bool TransitiveProjects { get; set; }

        internal static ReferenceSearchOptions ForFindReferences(bool includeLateBound)
        {
            return new ReferenceSearchOptions { IncludeLateBound = includeLateBound };
        }

        internal static ReferenceSearchOptions ForRename(bool comments, bool strings)
        {
            var text = TextTokenKinds.None;
            if (comments)
                text |= TextTokenKinds.Comments;
            if (strings)
                text |= TextTokenKinds.Strings;
            return new ReferenceSearchOptions { IncludeUncertain = true, IncludeText = text, TransitiveProjects = true };
        }
    }
}
