//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;

namespace XSharp.LanguageService.FindReferences
{
    internal sealed class ReferenceHit
    {
        internal string FilePath { get; set; }
        internal string ProjectName { get; set; }
        /// <summary>0 based</summary>
        internal int Line { get; set; }
        /// <summary>0 based</summary>
        internal int Column { get; set; }
        internal int Length { get; set; }
        internal string LineText { get; set; }
        /// <summary>Editor classifications of the line, relative to the line start</summary>
        internal IReadOnlyList<ClassifiedRange> Classifications { get; set; }
        internal bool IsDefinition { get; set; }
        /// <summary>The declaration the hit belongs to: the target or a member of its override family</summary>
        internal string Group { get; set; }
        /// <summary>Position in the file text</summary>
        internal int Start { get; set; }
        /// <summary>None: the place surely refers to the target</summary>
        internal ReferenceUncertainty Uncertainty { get; set; }
        /// <summary>Comments or Strings when the hit is the name in a comment or string, otherwise None</summary>
        internal TextTokenKinds InText { get; set; }
    }

    /// <summary>Why a place may, or may not, refer to the target.</summary>
    internal enum ReferenceUncertainty
    {
        None,
        /// <summary>o:Name that the lookup cannot resolve</summary>
        LateBound,
        /// <summary>#Name</summary>
        Symbol,
        /// <summary>The name that the lookup cannot resolve at all</summary>
        Unresolved,
        /// <summary>Another overload fits the arguments as well</summary>
        AmbiguousOverload,
        /// <summary>The name in an excluded #ifdef block</summary>
        InactiveCode,
        /// <summary>The file could not be searched</summary>
        FileNotRead
    }

    internal readonly struct ClassifiedRange
    {
        internal ClassifiedRange(int start, int length, string classification)
        {
            Start = start;
            Length = length;
            Classification = classification;
        }
        internal int Start { get; }
        internal int Length { get; }
        /// <summary>Name of the classification type, e.g. "keyword"</summary>
        internal string Classification { get; }

        public override string ToString() => $"{Start},{Length},{Classification}";
    }

    internal interface IReferenceSink
    {
        void Add(IReadOnlyList<ReferenceHit> batch);
    }

    internal sealed class FindReferencesStats
    {
        internal int FilesInScope { get; set; }
        internal int FilesLexed { get; set; }
        internal int Candidates { get; set; }
        internal int Resolved { get; set; }
        internal int Unresolved { get; set; }
        internal int Hits { get; set; }
        /// <summary>Files (or other items) that could not be searched</summary>
        internal int Failed { get; set; }
        internal TimeSpan Elapsed { get; set; }

        public override string ToString()
        {
            return $"files in scope {FilesInScope}, lexed {FilesLexed}, candidates {Candidates}, resolved {Resolved}, unresolved {Unresolved}, hits {Hits}, failed {Failed}, {Elapsed.TotalMilliseconds:F0} ms";
        }
    }
}
