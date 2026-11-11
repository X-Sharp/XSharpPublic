//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharp.LanguageService.ForeignSource
{
    /// <summary>A declaration in the source of a C# or VB project of the solution. Lines and columns are 0-based.</summary>
    internal sealed class ForeignLocation
    {
        internal ForeignLocation(string filePath, int startLine, int startColumn, int endLine, int endColumn, string name, string containerName)
        {
            FilePath = filePath;
            StartLine = startLine;
            StartColumn = startColumn;
            EndLine = endLine;
            EndColumn = endColumn;
            Name = name;
            ContainerName = containerName;
        }

        internal string FilePath { get; }
        internal int StartLine { get; }
        internal int StartColumn { get; }
        internal int EndLine { get; }
        internal int EndColumn { get; }
        /// <summary>The name of the declared symbol.</summary>
        internal string Name { get; }
        /// <summary>The containing type, or the namespace for a type.</summary>
        internal string ContainerName { get; }
    }
}
