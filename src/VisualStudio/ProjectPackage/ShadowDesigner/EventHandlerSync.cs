//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Detects event-handler stubs and wiring statements the out-of-process Designer added
    /// to the companion project's C# files, and writes the X# equivalents into the real
    /// .prg sources. Scoped narrowly to handler *creation* only -- rename/delete of an
    /// existing wired handler isn't attempted. Uses Roslyn to parse the companion C# files,
    /// since Microsoft.CSharp.CSharpCodeProvider.Parse() doesn't implement C# parsing at all
    /// (CodeDom only ever supported generation, not parsing, for this provider).
    /// </summary>
    internal static class EventHandlerSync
    {
        public sealed class SyncResult
        {
            public List<string> NewHandlerNames { get; } = new List<string>();
            public List<string> NewWiringDescriptions { get; } = new List<string>();
            public bool HasChanges => NewHandlerNames.Count > 0 || NewWiringDescriptions.Count > 0;
        }

        // Only the couple of type keywords event-handler signatures actually need -- this is
        // intentionally not a general C#->X# type translator (XSharpCodeGenerator already
        // exists for that, and is used directly by DesignerChangesSync for the harder case).
        private static readonly Dictionary<string, string> TypeTranslations = new Dictionary<string, string>(StringComparer.Ordinal)
        {
            ["object"] = "OBJECT",
            ["void"] = "VOID",
            ["string"] = "STRING",
            ["int"] = "INT",
            ["bool"] = "LOGIC",
        };

        // Companion-file boilerplate that always exists -- must be excluded, otherwise
        // scanning for "new" methods would wrongly flag these as Designer-added handler
        // stubs every single run.
        private static readonly HashSet<string> ExcludedMethodNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "InitializeComponent", "Dispose",
        };

        public static SyncResult Sync(ShadowDesignerBridge.CompanionLocation location)
        {
            var result = new SyncResult();

            string mainSource = File.ReadAllText(location.MainPrgPath);
            string designerSource = File.ReadAllText(location.DesignerPrgPath);

            // The Designer doesn't reliably put new handler stubs in the companion Form1.cs --
            // observed empirically landing directly in Form1.Designer.cs for a plain stock
            // control, vs. Form1.cs for a 3rd-party control. Scan BOTH companion files and
            // check "already exists" against BOTH real files combined.
            string combinedRealSource = mainSource + "\n" + designerSource;
            var candidateMethods = new List<MethodDeclarationSyntax>();
            if (File.Exists(location.CompanionFormCsPath))
            {
                candidateMethods.AddRange(ParseMethods(location.CompanionFormCsPath));
            }
            if (File.Exists(location.CompanionDesignerCsPath))
            {
                candidateMethods.AddRange(ParseMethods(location.CompanionDesignerCsPath));
            }

            var newMethodStubs = new List<string>();
            foreach (var method in candidateMethods)
            {
                string name = method.Identifier.Text;
                if (ExcludedMethodNames.Contains(name)) continue;
                if (result.NewHandlerNames.Contains(name)) continue; // dedupe across both files
                if (Regex.IsMatch(combinedRealSource, $@"\bMETHOD\s+{Regex.Escape(name)}\s*\(", RegexOptions.IgnoreCase)) continue;

                var paramTexts = method.ParameterList.Parameters
                    .Select(p => $"{p.Identifier.Text} AS {TranslateType(p.Type?.ToString() ?? "OBJECT")}");

                string stub =
$@"    PRIVATE METHOD {name}({string.Join(", ", paramTexts)}) AS VOID
        // TODO: implement
        RETURN
    END METHOD
";
                newMethodStubs.Add(stub);
                result.NewHandlerNames.Add(name);
            }

            if (newMethodStubs.Count > 0)
            {
                mainSource = InsertBeforeLastEndClass(mainSource, string.Join("\n", newMethodStubs));
            }

            if (File.Exists(location.CompanionDesignerCsPath))
            {
                designerSource = SyncNewEventWiring(location, designerSource, result);
            }

            if (result.NewHandlerNames.Count > 0)
            {
                File.WriteAllText(location.MainPrgPath, mainSource);
            }
            if (result.NewWiringDescriptions.Count > 0)
            {
                File.WriteAllText(location.DesignerPrgPath, designerSource);
            }

            return result;
        }

        private static IEnumerable<MethodDeclarationSyntax> ParseMethods(string path)
        {
            string source = File.ReadAllText(path);
            var root = CSharpSyntaxTree.ParseText(source).GetRoot();
            return root.DescendantNodes().OfType<MethodDeclarationSyntax>();
        }

        private static string SyncNewEventWiring(ShadowDesignerBridge.CompanionLocation location, string designerSource, SyncResult result)
        {
            string companionDesignerSource = File.ReadAllText(location.CompanionDesignerCsPath);
            var root = CSharpSyntaxTree.ParseText(companionDesignerSource).GetRoot();
            var initializeComponent = root.DescendantNodes().OfType<MethodDeclarationSyntax>()
                .FirstOrDefault(m => m.Identifier.Text == "InitializeComponent");

            if (initializeComponent?.Body == null)
            {
                return designerSource;
            }

            foreach (var statement in initializeComponent.Body.Statements.OfType<ExpressionStatementSyntax>())
            {
                if (!(statement.Expression is AssignmentExpressionSyntax assignment)) continue;
                if (!assignment.OperatorToken.IsKind(SyntaxKind.PlusEqualsToken)) continue;
                if (!(assignment.Left is MemberAccessExpressionSyntax fieldEvent)) continue;

                string eventName = fieldEvent.Name.Identifier.Text;
                string fieldName = ExtractTrailingName(fieldEvent.Expression);

                // Handles both "new EventHandler(this.X_Click)" and a bare method-group
                // "this.X_Click" -- observed the plain-generated form via CodeDom, and want to
                // tolerate whichever shape the real out-of-process Designer emits too.
                ExpressionSyntax handlerExpr = assignment.Right;
                if (handlerExpr is ObjectCreationExpressionSyntax objectCreation &&
                    objectCreation.ArgumentList?.Arguments.Count == 1)
                {
                    handlerExpr = objectCreation.ArgumentList.Arguments[0].Expression;
                }
                string handlerName = ExtractTrailingName(handlerExpr);

                if (fieldName == null || handlerName == null) continue;

                // Idempotency guard -- re-check against the current (possibly already-edited-
                // this-run) designerSource each iteration, not a snapshot from before the loop.
                if (Regex.IsMatch(designerSource, $@"\b{Regex.Escape(fieldName)}\s*:\s*{Regex.Escape(eventName)}\s*\+=", RegexOptions.IgnoreCase))
                {
                    continue;
                }

                string wiringLine = $"        SELF:{fieldName}:{eventName} += SELF:{handlerName}";
                designerSource = InsertNearFieldOrMethodEnd(designerSource, fieldName, wiringLine, "InitializeComponent");
                result.NewWiringDescriptions.Add($"{fieldName}:{eventName} -> {handlerName}");
            }

            return designerSource;
        }

        private static string ExtractTrailingName(ExpressionSyntax expr)
        {
            switch (expr)
            {
                case MemberAccessExpressionSyntax memberAccess:
                    return memberAccess.Name.Identifier.Text;
                case IdentifierNameSyntax identifier:
                    return identifier.Identifier.Text;
                default:
                    return null;
            }
        }

        private static string TranslateType(string csharpType) =>
            TypeTranslations.TryGetValue(csharpType, out var xsharpType) ? xsharpType : csharpType;

        private static string InsertBeforeLastEndClass(string source, string textToInsert)
        {
            var matches = Regex.Matches(source, @"\bEND\s+CLASS\b", RegexOptions.IgnoreCase);
            if (matches.Count == 0)
            {
                throw new InvalidOperationException("No 'END CLASS' found in the real .prg to insert new method(s) before.");
            }
            var last = matches[matches.Count - 1];
            return source.Substring(0, last.Index) + textToInsert + "\n" + source.Substring(last.Index);
        }

        /// <summary>
        /// Groups the new wiring statement with the field's other init statements instead of
        /// always landing after the method's trailing Form1-level statements. Falls back to
        /// the old "before END METHOD" behavior only if no line referencing the field exists.
        /// </summary>
        private static string InsertNearFieldOrMethodEnd(string source, string fieldName, string lineToInsert, string methodName)
        {
            var methodStart = Regex.Match(source, $@"\bMETHOD\s+{Regex.Escape(methodName)}\s*\(", RegexOptions.IgnoreCase);
            if (!methodStart.Success)
            {
                throw new InvalidOperationException($"Could not find 'METHOD {methodName}(' in the real Designer .prg.");
            }
            var endMethod = Regex.Match(source.Substring(methodStart.Index), @"\bEND\s+METHOD\b", RegexOptions.IgnoreCase);
            if (!endMethod.Success)
            {
                throw new InvalidOperationException($"Could not find the closing 'END METHOD' for {methodName}.");
            }
            int methodEndPos = methodStart.Index + endMethod.Index;
            string methodBody = source.Substring(methodStart.Index, methodEndPos - methodStart.Index);

            var fieldLines = Regex.Matches(methodBody, $@"^.*\b{Regex.Escape(fieldName)}\s*:.*$", RegexOptions.IgnoreCase | RegexOptions.Multiline);
            if (fieldLines.Count > 0)
            {
                var lastLine = fieldLines[fieldLines.Count - 1];
                int insertPos = methodStart.Index + lastLine.Index + lastLine.Length;
                return source.Substring(0, insertPos) + "\n" + lineToInsert + source.Substring(insertPos);
            }

            return source.Substring(0, methodEndPos) + lineToInsert + "\n" + source.Substring(methodEndPos);
        }
    }
}
