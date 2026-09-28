//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Errors of the files of a CPS project for the editor (GetIntellisenseErrors, used for the error squiggles by
    /// XSharpErrorColorizer) and the Error List.
    /// </summary>
    /// <remarks>
    /// The MPFproj project system keeps these in its ErrorListManager (ProjectPackage), which is tied to the
    /// MPFproj hierarchy. Two sources, like there:
    /// <list type="bullet">
    /// <item>intellisense errors reported by the code model (AddIntellisenseError); every change is reported
    ///       through <see cref="Changed"/> (used for the Error List, see IntellisenseErrorList)</item>
    /// <item>errors and warnings of the last real build (<see cref="SetBuildErrors"/>, see BuildErrorLoggerProvider).
    ///       Only for the squiggles: VS itself shows the build errors in the Error List.</item>
    /// </list>
    /// </remarks>
    internal sealed class IntellisenseErrorStore
    {
        private readonly Dictionary<string, List<XError>> errors = new Dictionary<string, List<XError>>(StringComparer.OrdinalIgnoreCase);

        // Build errors per project configuration (one build per target framework), each keyed by the full file name
        private readonly Dictionary<string, ILookup<string, IXErrorPosition>> buildErrors = new Dictionary<string, ILookup<string, IXErrorPosition>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Called with all intellisense errors of the project after each change.
        /// </summary>
        public Action<IReadOnlyList<XError>> Changed { get; set; }

        public void Add(XError error)
        {
            if (error == null || string.IsNullOrEmpty(error.Path))
                return;
            lock (errors)
            {
                if (!errors.TryGetValue(error.Path, out var list))
                {
                    list = new List<XError>();
                    errors.Add(error.Path, list);
                }
                list.Add(error);
            }
            OnChanged();
        }

        public void Clear(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return;
            bool removed;
            lock (errors)
            {
                removed = errors.Remove(fileName);
            }
            if (removed)
                OnChanged();
        }

        /// <summary>
        /// Replaces the build errors of <paramref name="configuration"/> with the errors and warnings of its last build.
        /// </summary>
        public void SetBuildErrors(string configuration, IEnumerable<BuildErrorPosition> positions)
        {
            var lookup = positions.ToLookup(p => p.FileName, p => (IXErrorPosition)p, StringComparer.OrdinalIgnoreCase);
            lock (errors)
            {
                buildErrors[configuration ?? ""] = lookup;
            }
        }

        private void OnChanged()
        {
            var changed = Changed;
            if (changed == null)
                return;
            List<XError> all;
            lock (errors)
            {
                all = errors.Values.SelectMany(l => l).ToList();
            }
            changed(all);
        }

        public List<IXErrorPosition> Get(string fileName)
        {
            var result = new List<IXErrorPosition>();
            if (string.IsNullOrEmpty(fileName))
                return result;
            // dedupe errors on the same position, like the ErrorListManager does
            var positions = new HashSet<(int, int)>();
            lock (errors)
            {
                if (errors.TryGetValue(fileName, out var list))
                {
                    foreach (var error in list)
                    {
                        if (positions.Add((error.Span.Line, error.Span.Column)))
                            result.Add(new ErrorPosition(error.Span.Line, error.Span.Column, 1));
                    }
                }
                foreach (var lookup in buildErrors.Values)
                {
                    foreach (var error in lookup[fileName])
                    {
                        if (positions.Add((error.Line, error.Column)))
                            result.Add(error);
                    }
                }
            }
            return result;
        }

        private sealed class ErrorPosition : IXErrorPosition
        {
            public ErrorPosition(int line, int column, int length)
            {
                Line = line;
                Column = column;
                Length = length;
            }

            public int Column { get; set; }
            public int Length { get; set; }
            public int Line { get; set; }
        }
    }

    /// <summary>
    /// Position of a build error or warning (1-based, like the MSBuild events and the code model errors).
    /// </summary>
    internal sealed class BuildErrorPosition : IXErrorPosition
    {
        public BuildErrorPosition(string fileName, int line, int column)
        {
            FileName = fileName;
            Line = line;
            Column = column;
            Length = 1;
        }

        public string FileName { get; }
        public int Column { get; set; }
        public int Length { get; set; }
        public int Line { get; set; }
    }
}
