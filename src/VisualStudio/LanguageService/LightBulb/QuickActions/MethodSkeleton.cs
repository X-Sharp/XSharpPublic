//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>An argument of the call a method is generated from.</summary>
    internal sealed class CallArgument
    {
        internal CallArgument(string name, string type)
        {
            Name = name;
            Type = type;
        }

        /// <summary>The variable passed, or null when the argument is not a plain identifier.</summary>
        internal string Name { get; }

        /// <summary>The declared type of the argument, or null when unknown.</summary>
        internal string Type { get; }
    }

    /// <summary>The class a member is generated into, and where.</summary>
    internal sealed class TargetClass
    {
        internal TargetClass(string name, bool hasEndClass, int insertLine, string memberIndent)
        {
            Name = name;
            HasEndClass = hasEndClass;
            InsertLine = insertLine;
            MemberIndent = memberIndent ?? "";
        }

        internal string Name { get; }

        /// <summary>The members are inside CLASS ... END CLASS; otherwise they carry a CLASS clause (VO style).</summary>
        internal bool HasEndClass { get; }

        /// <summary>0-based: the END CLASS line, or the line after the last member of the class.</summary>
        internal int InsertLine { get; }

        /// <summary>The leading white space of the members of the class.</summary>
        internal string MemberIndent { get; }
    }

    /// <summary>The empty method generated from a call to a method that does not exist yet.</summary>
    internal static class MethodSkeleton
    {
        internal static TextInsertion Build(string methodName, IList<CallArgument> args, TargetClass target, CodeStyle style)
        {
            var names = ParameterNames(args);
            var parameters = new List<string>();
            for (int i = 0; i < args.Count; i++)
                parameters.Add(names[i] + " " + style.Kw("AS") + " " + (args[i].Type ?? style.UntypedType));
            var header = new StringBuilder();
            header.Append(target.MemberIndent).Append(style.Kw("METHOD")).Append(' ').Append(methodName)
                .Append('(').Append(string.Join(", ", parameters)).Append(')');
            if (!style.HasUsual)
                header.Append(' ').Append(style.Kw("AS VOID"));
            if (!target.HasEndClass)
                header.Append(' ').Append(style.Kw("CLASS")).Append(' ').Append(target.Name);
            var body = target.MemberIndent + style.IndentUnit;
            var text = style.NewLine + header + style.NewLine + body;
            var caret = text.Length;
            text += style.NewLine;
            // an untyped method returns USUAL: without /vo9 it needs its RETURN
            if (style.HasUsual)
                text += body + style.Kw("RETURN NIL") + style.NewLine;
            return new TextInsertion(target.InsertLine, text, caret);
        }

        /// <summary>Does the line close a class block (END CLASS, ENDCLASS, END DEFINE, ENDDEFINE)?</summary>
        internal static bool IsEndClassLine(string line)
        {
            var words = (line ?? "").Trim().Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (words.Length == 0)
                return false;
            var first = words[0].ToUpperInvariant();
            if (first == "ENDCLASS" || first == "ENDDEFINE")
                return true;
            if (first != "END" || words.Length < 2)
                return false;
            var second = words[1].ToUpperInvariant();
            return second == "CLASS" || second == "DEFINE";
        }

        /// <summary>The argument's variable name, else p1, p2, ...; a name already taken gets 2, 3, ... appended.</summary>
        private static IList<string> ParameterNames(IList<CallArgument> args)
        {
            var taken = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var wanted = new List<string>();
            for (int i = 0; i < args.Count; i++)
                wanted.Add(args[i].Name ?? "p" + (i + 1));
            var result = new List<string>();
            foreach (var name in wanted)
            {
                var candidate = name;
                for (int n = 2; taken.Contains(candidate); n++)
                    candidate = name + n;
                taken.Add(candidate);
                result.Add(candidate);
            }
            return result;
        }
    }
}
