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
    /// Intellisense errors that the code model reports for the files of a CPS project.
    /// </summary>
    /// <remarks>
    /// The MPFproj project system keeps these in its ErrorListManager (ProjectPackage), which is tied to the
    /// MPFproj hierarchy. This store keeps the errors for the code model (GetIntellisenseErrors) and reports
    /// every change through <see cref="Changed"/> (used for the Error List, see IntellisenseErrorList).
    /// </remarks>
    internal sealed class IntellisenseErrorStore
    {
        private readonly Dictionary<string, List<XError>> errors = new Dictionary<string, List<XError>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Called with all errors of the project after each change.
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
            lock (errors)
            {
                if (errors.TryGetValue(fileName, out var list))
                {
                    // dedupe errors on the same position, like the ErrorListManager does
                    foreach (var error in list.GroupBy(e => (e.Span.Line, e.Span.Column)).Select(g => g.First()))
                    {
                        result.Add(new ErrorPosition(error.Span.Line, error.Span.Column, 1));
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
}
