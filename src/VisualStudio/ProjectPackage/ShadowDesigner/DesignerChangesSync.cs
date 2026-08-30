//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using XSharp.CodeDom;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// "Designer -> Code, beyond handler creation" (general property edits, control
    /// add/remove/reorder). Fully regenerates the real Form1.Designer.prg from the companion
    /// project's current InitializeComponent on every sync: parse the companion C# with
    /// Roslyn, translate to System.CodeDom objects (RoslynToCodeDom), then hand that
    /// CodeCompileUnit to the existing, already-production-proven XSharpCodeGenerator (the
    /// SAME class VSXSharpCodeDomProvider.cs uses for the classic .NET Framework
    /// integration) to emit X# text.
    ///
    /// This calls XSharpCodeGenerator directly -- see ShadowDesignerBridge's class doc for
    /// why that's safe here.
    ///
    /// Not coordinated with EventHandlerSync at the file level: that command also writes
    /// wiring (+=) lines into Form1.Designer.prg, via targeted regex insertion rather than
    /// a full regenerate. Run event sync before this one when both apply in the same pass.
    /// </summary>
    internal static class DesignerChangesSync
    {
        public sealed class SyncResult
        {
            public int FieldCount { get; set; }
            public int StatementCount { get; set; }
            public List<string> SkippedStatements { get; } = new List<string>();
        }

        public static SyncResult Sync(ShadowDesignerBridge.CompanionLocation location)
        {
            string companionSource = File.ReadAllText(location.CompanionDesignerCsPath);
            var root = CSharpSyntaxTree.ParseText(companionSource).GetRoot();

            var classDecl = root.DescendantNodes().OfType<ClassDeclarationSyntax>().FirstOrDefault();
            if (classDecl == null)
            {
                throw new InvalidOperationException($"No class declaration found in {location.CompanionDesignerCsPath}.");
            }

            string namespaceName =
                root.DescendantNodes().OfType<NamespaceDeclarationSyntax>().FirstOrDefault()?.Name.ToString()
                ?? root.DescendantNodes().OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault()?.Name.ToString()
                ?? throw new InvalidOperationException("Could not determine the companion file's namespace.");

            var fieldDecls = classDecl.Members.OfType<FieldDeclarationSyntax>().ToList();
            var initializeComponent = classDecl.Members.OfType<MethodDeclarationSyntax>()
                .FirstOrDefault(m => m.Identifier.Text == "InitializeComponent");
            if (initializeComponent?.Body == null)
            {
                throw new InvalidOperationException(
                    $"No InitializeComponent method body found in {location.CompanionDesignerCsPath}.");
            }

            var ctx = new RoslynToCodeDom.TranslationContext();
            foreach (var fieldDecl in fieldDecls)
            {
                foreach (var variable in fieldDecl.Declaration.Variables)
                {
                    ctx.KnownFieldNames.Add(variable.Identifier.Text);
                }
            }

            var compileUnit = new CodeCompileUnit();
            var ns = new CodeNamespace(namespaceName);
            compileUnit.Namespaces.Add(ns);
            var type = new CodeTypeDeclaration(classDecl.Identifier.Text) { IsClass = true, IsPartial = true };
            ns.Types.Add(type);

            foreach (var fieldDecl in fieldDecls)
            {
                string typeName = fieldDecl.Declaration.Type.ToString();
                foreach (var variable in fieldDecl.Declaration.Variables)
                {
                    type.Members.Add(new CodeMemberField(typeName, variable.Identifier.Text)
                    {
                        Attributes = MemberAttributes.Private,
                    });
                }
            }

            var method = new CodeMemberMethod
            {
                Name = "InitializeComponent",
                Attributes = MemberAttributes.Private | MemberAttributes.Final,
                ReturnType = new CodeTypeReference(typeof(void)),
            };
            int statementCount = 0;
            foreach (var stmt in initializeComponent.Body.Statements)
            {
                if (stmt.HasLeadingTrivia)
                {
                    var trivia = stmt.GetLeadingTrivia().ToString();
                    if (! string.IsNullOrWhiteSpace(trivia))
                    {
                        bool docComment = trivia.Contains("///");
                        if (docComment)
                        {
                            trivia = trivia.Replace("///", "");
                        }
                        else
                        {
                            trivia = trivia.Replace("//", "");
                        }
                        var lines = trivia.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
                        // remove the last line because that is the trivia receding the statement
                        for (int i = 0; i < lines.Length-1; i++)
                        {
                            var line = lines[i];
                            var comment = new CodeCommentStatement(line.TrimStart(), docComment);
                            method.Statements.Add(comment);
                        }
                    }

                }
                var translated = RoslynToCodeDom.TranslateStatement(stmt, ctx);
                if (translated != null)
                {
                    method.Statements.Add(translated);
                    statementCount++;
                }
            }
            type.Members.Add(method);

            string generatedXSharp = GenerateXSharpText(compileUnit);

            File.WriteAllText(location.DesignerPrgPath, generatedXSharp);

            var result = new SyncResult
            {
                FieldCount = fieldDecls.Sum(f => f.Declaration.Variables.Count),
                StatementCount = statementCount,
            };
            result.SkippedStatements.AddRange(ctx.SkippedStatements);
            return result;
        }

        private static string GenerateXSharpText(CodeCompileUnit compileUnit)
        {
            // Directly instantiated -- XSharpCodeGenerator lives in this same assembly's
            // reference graph (XSharp.CodeDomProvider.dll, already used unqualified by
            // VSXSharpCodeDomProvider.cs's classic .NET Framework integration).
            var generator = new XSharpCodeGenerator();
            var options = new CodeGeneratorOptions { BracingStyle = "C" };
            using (var writer = new StringWriter())
            {
                ((ICodeGenerator)generator).GenerateCodeFromCompileUnit(compileUnit, writer, options);
                return writer.ToString();
            }
        }
    }
}
