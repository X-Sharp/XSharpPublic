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
    /// MPFproj hierarchy. This store only keeps the errors for the code model (GetIntellisenseErrors), it does
    /// not show them in the Error List yet.
    /// </remarks>
    internal sealed class IntellisenseErrorStore
    {
        private readonly Dictionary<string, List<XError>> errors = new Dictionary<string, List<XError>>(StringComparer.OrdinalIgnoreCase);

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
        }

        public void Clear(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return;
            lock (errors)
            {
                errors.Remove(fileName);
            }
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
