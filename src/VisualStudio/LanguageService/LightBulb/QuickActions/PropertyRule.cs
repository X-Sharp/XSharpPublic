//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>An ACCESS and/or ASSIGN of one name that can become a PROPERTY.</summary>
    internal sealed class AccessAssignPair
    {
        internal AccessAssignPair(string name, XSourceMemberSymbol access, XSourceMemberSymbol assign)
        {
            Name = name;
            Access = access;
            Assign = assign;
        }

        internal string Name { get; }
        internal XSourceMemberSymbol Access { get; }
        internal XSourceMemberSymbol Assign { get; }
    }

    /// <summary>Which ACCESS/ASSIGN of a class can be converted to a PROPERTY without changing what the code does.</summary>
    internal static class PropertyRule
    {
        /// <summary>
        /// The convertible pairs of <paramref name="type"/> among <paramref name="membersInFile"/> (the members in this
        /// file). None in a class without END CLASS (a PROPERTY has no CLASS clause). A pair is left out when a base
        /// class has a member of that name (an override), the other part is in another file, the types or the index
        /// parameters differ, other modifiers than the visibility differ, or a member carries an attribute (the
        /// compiler drops attributes on ACCESS/ASSIGN; on a PROPERTY they would start to work).
        /// </summary>
        internal static IReadOnlyList<AccessAssignPair> Pairs(XSourceTypeSymbol type, IEnumerable<XSourceMemberSymbol> membersInFile,
            Func<int, string> lineText, bool hasEndClass, Func<IXTypeSymbol, IXTypeSymbol> baseOf, bool caseSensitive)
        {
            var result = new List<AccessAssignPair>();
            if (type == null || !hasEndClass)
                return result;
            var comparer = caseSensitive ? StringComparer.Ordinal : StringComparer.OrdinalIgnoreCase;
            var local = membersInFile.Where(m => m.Kind == Kind.Access || m.Kind == Kind.Assign).ToList();
            var all = type.Members.Where(m => m.Kind == Kind.Access || m.Kind == Kind.Assign).ToList();
            var baseNames = new HashSet<string>(GenerateMethodRule.SelfAndBases(type, baseOf).Skip(1)
                .SelectMany(t => t.Members)
                .Where(m => m.Kind == Kind.Access || m.Kind == Kind.Assign || m.Kind == Kind.Property)
                .Select(m => m.Name), comparer);
            foreach (var group in local.GroupBy(m => m.Name, comparer).OrderBy(g => g.Min(m => m.Range.StartLine)))
            {
                var access = group.Where(m => m.Kind == Kind.Access).ToList();
                var assign = group.Where(m => m.Kind == Kind.Assign).ToList();
                if (access.Count > 1 || assign.Count > 1 || baseNames.Contains(group.Key))
                    continue;
                // the other part lives in another file of a partial class
                if (all.Count(m => comparer.Equals(m.Name, group.Key)) != group.Count())
                    continue;
                var pair = new AccessAssignPair(group.First().Name, access.FirstOrDefault(), assign.FirstOrDefault());
                if (HasAttribute(pair.Access, lineText) || HasAttribute(pair.Assign, lineText))
                    continue;
                if (pair.Access != null && pair.Assign != null && !Match(pair.Access, pair.Assign))
                    continue;
                result.Add(pair);
            }
            return result;
        }

        /// <summary>The pair whose ACCESS or ASSIGN header is on <paramref name="caretLine"/>, or null.</summary>
        internal static AccessAssignPair At(IReadOnlyList<AccessAssignPair> pairs, int caretLine)
        {
            return pairs.FirstOrDefault(p => p.Access?.Range.StartLine == caretLine || p.Assign?.Range.StartLine == caretLine);
        }

        /// <summary>Same type (the ASSIGN's value parameter), same index parameters, same modifiers apart from the visibility.</summary>
        private static bool Match(XSourceMemberSymbol access, XSourceMemberSymbol assign)
        {
            var assignParameters = assign.Parameters ?? new List<IXParameterSymbol>();
            if (assignParameters.Count == 0)
                return false;
            if (TypeOf(access.IsTyped, access.TypeName) != TypeOf(assignParameters[0].IsTyped, assignParameters[0].TypeName))
                return false;
            var index = access.Parameters ?? new List<IXParameterSymbol>();
            if (index.Count != assignParameters.Count - 1)
                return false;
            for (int i = 0; i < index.Count; i++)
            {
                if (TypeOf(index[i].IsTyped, index[i].TypeName) != TypeOf(assignParameters[i + 1].IsTyped, assignParameters[i + 1].TypeName))
                    return false;
            }
            return (access.Modifiers & ~Modifiers.VisibilityMask) == (assign.Modifiers & ~Modifiers.VisibilityMask);
        }

        private static string TypeOf(bool isTyped, string typeName)
        {
            return isTyped && !string.IsNullOrEmpty(typeName) ? Regex.Replace(typeName, @"(\w+\.)+", "").ToLowerInvariant() : "usual";
        }

        /// <summary>An attribute on the header line or on the line above it.</summary>
        private static bool HasAttribute(XSourceMemberSymbol member, Func<int, string> lineText)
        {
            if (member == null)
                return false;
            var header = member.Range.StartLine;
            return lineText(header).TrimStart().StartsWith("[") ||
                   (header > 0 && lineText(header - 1).Trim().StartsWith("[") && lineText(header - 1).Trim().EndsWith("]"));
        }
    }
}
