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
        internal bool IsDefinition { get; set; }
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
        internal TimeSpan Elapsed { get; set; }

        public override string ToString()
        {
            return $"files in scope {FilesInScope}, lexed {FilesLexed}, candidates {Candidates}, resolved {Resolved}, unresolved {Unresolved}, hits {Hits}, {Elapsed.TotalMilliseconds:F0} ms";
        }
    }
}
