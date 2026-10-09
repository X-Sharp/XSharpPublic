//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Immutable;
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.Diagnostics;

namespace XSharpTestAnalyzer
{
    /// <summary>
    /// Analyzer that reports nothing. Passing it with /analyzer: makes the compiler run its analyzer
    /// code path (including the AnalyzerConfig options for generated syntax trees), which is what the
    /// compiler tests that use it want to cover.
    /// </summary>
    // "X#" is the value of LanguageNames.CSharp in the X# compiler
    [DiagnosticAnalyzer("X#")]
    public sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            id: "XST0001",
            title: "Test analyzer",
            messageFormat: "Test analyzer",
            category: "Test",
            defaultSeverity: DiagnosticSeverity.Hidden,
            isEnabledByDefault: false);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxTreeAction(_ => { });
        }
    }
}
