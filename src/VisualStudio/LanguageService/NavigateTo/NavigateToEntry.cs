//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using XSharpModel;

namespace XSharp.LanguageService.NavigateTo
{
    /// <summary>A declaration that Go To All can find: a type, a member, a function, a DEFINE or a GLOBAL.</summary>
    internal sealed class NavigateToEntry
    {
        internal string Name { get; set; }
        /// <summary>The type that declares a member; the namespace of a type; empty for functions, DEFINEs and GLOBALs</summary>
        internal string Container { get; set; } = "";
        internal Kind Kind { get; set; }
        internal Modifiers Visibility { get; set; }
        internal string FilePath { get; set; }
        internal string ProjectName { get; set; }
        /// <summary>As stored in the code model database; passed unchanged to XDocuments.Open</summary>
        internal int Line { get; set; }
        internal int Column { get; set; }

        /// <summary>A type (class, structure, interface, delegate, enum), not a member</summary>
        internal bool IsType => _typeKinds.Contains(Kind);

        /// <summary>
        /// The "types" and "members" filters of Go To All: one of them alone keeps only that group; both or none keep
        /// everything.
        /// </summary>
        internal bool PassesFilter(bool typesOnly, bool membersOnly)
        {
            if (typesOnly == membersOnly)
                return true;
            return typesOnly == IsType;
        }

        private static readonly HashSet<Kind> _typeKinds = new HashSet<Kind>
        {
            Kind.Class, Kind.Structure, Kind.Interface, Kind.Delegate, Kind.Enum, Kind.VOStruct, Kind.Union
        };

        private static readonly HashSet<Kind> _memberKinds = new HashSet<Kind>
        {
            Kind.Method, Kind.Access, Kind.Assign, Kind.Property, Kind.Function, Kind.Procedure, Kind.Field,
            Kind.Event, Kind.Operator, Kind.EnumMember, Kind.VODefine, Kind.VODLL, Kind.VOGlobal, Kind.Define,
            Kind.Delegate
        };

        /// <summary>
        /// The entries for the rows of the code model: the global type itself and kinds that Go To All does not list
        /// (constructors, usings, …) are left out; a declaration in a file of several projects is listed once, and
        /// ACCESS and ASSIGN of the same name in the same type and file once, as C# lists a property.
        /// </summary>
        internal static IReadOnlyList<NavigateToEntry> FromRows(IEnumerable<XDbResult> types, IEnumerable<XDbResult> members)
        {
            var result = new List<NavigateToEntry>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            // the functions of the runtime that the code model writes to a temporary file are not in the solution
            var builtIn = XSolution.BuiltInFunctions;
            bool IsBuiltIn(XDbResult row) => !string.IsNullOrEmpty(builtIn) && string.Equals(row.FileName, builtIn, StringComparison.OrdinalIgnoreCase);
            foreach (var row in types)
            {
                if (!_typeKinds.Contains(row.Kind) || row.TypeName == XLiterals.GlobalName || IsBuiltIn(row))
                    continue;
                Add(row, row.TypeName, row.Namespace);
            }
            foreach (var row in members)
            {
                if (!_memberKinds.Contains(row.Kind) || IsBuiltIn(row))
                    continue;
                var container = row.TypeName == XLiterals.GlobalName ? "" : row.TypeName;
                if (row.Kind == Kind.Access || row.Kind == Kind.Assign)
                {
                    // one entry for the property, whichever of the two comes first
                    if (!seen.Add($"property|{row.FileName}|{container}|{row.MemberName}"))
                        continue;
                }
                Add(row, row.MemberName, container);
            }
            return result;

            void Add(XDbResult row, string name, string container)
            {
                if (string.IsNullOrEmpty(name) || !seen.Add($"{row.FileName}|{row.StartLine}|{row.StartColumn}|{name}"))
                    return;
                result.Add(new NavigateToEntry
                {
                    Name = name,
                    Container = container ?? "",
                    Kind = row.Kind,
                    Visibility = row.Visibility,
                    FilePath = row.FileName,
                    ProjectName = ProjectNameOf(row.Project),
                    Line = row.StartLine,
                    Column = row.StartColumn
                });
            }
        }

        private static string ProjectNameOf(string projectFile)
        {
            if (string.IsNullOrEmpty(projectFile))
                return "";
            try
            {
                return Path.GetFileNameWithoutExtension(projectFile);
            }
            catch (ArgumentException)
            {
                return projectFile;
            }
        }
    }
}
