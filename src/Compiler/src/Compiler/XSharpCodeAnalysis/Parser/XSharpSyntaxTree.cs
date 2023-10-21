//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Concurrent;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using MCA = Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static partial class SyntaxFactory
    {
        internal static readonly SyntaxTrivia WS = Whitespace(" ");
        // do NOT cache these tokens. That will cause many problems !
        // the only exception is the Whitespace trivia
        internal static SyntaxToken AmpersandToken => Token(SyntaxKind.AmpersandToken);
        internal static SyntaxToken CloseBraceToken => Token(SyntaxKind.CloseBraceToken);
        internal static SyntaxToken CloseBracketToken => Token(SyntaxKind.CloseBracketToken);
        internal static SyntaxToken CloseParenToken => Token(SyntaxKind.CloseParenToken);
        internal static SyntaxToken ColonColonToken => Token(SyntaxKind.ColonColonToken);
        internal static SyntaxToken ColonToken => Token(SyntaxKind.ColonToken);
        internal static SyntaxToken CommaToken => Token(SyntaxKind.CommaToken);
        internal static SyntaxToken DotToken => Token(SyntaxKind.DotToken);
        internal static SyntaxToken EqualsToken => Token(SyntaxKind.EqualsToken);
        internal static SyntaxToken OpenBraceToken => Token(SyntaxKind.OpenBraceToken);
        internal static SyntaxToken OpenBracketToken => Token(SyntaxKind.OpenBracketToken);
        internal static SyntaxToken OpenParenToken => Token(SyntaxKind.OpenParenToken);
        internal static SyntaxToken SemicolonToken => Token(SyntaxKind.SemicolonToken);
        internal static SyntaxToken MakeToken(SyntaxKind kind) => Token(WS, kind, WS);
        internal static SyntaxToken MakeGeneratedToken(SyntaxKind kind)
        {
            var token = MakeToken(kind);
            token.XGenerated = true;
            return token;
        }
        internal static SyntaxToken MakeToken(SyntaxKind kind, string text) => Token(WS, kind, text, text, WS);
        internal static SyntaxToken MakeIdentifier(string text) => Identifier(text);
    }
    [FlagsAttribute]
    enum XNodeFlags : short
    {
        None = 0,
        XCanBeVoStruct = 1 << 0,
        XVoIsDecl = 1 << 1,
        XPCall = 1 << 2,
        XGenerated = 1 << 3,
        XVoIsDim = 1 << 4,
        XDocComments = 1 << 5,
        XNeedsProcessing = 1 << 6,
        XIsChr = 1 << 7,
        XDefaultTree = 1 << 8,
        XIsString2Psz = 1 << 9,
        XNoWarning = 1 << 10,
        XNoTypeWarning = 1 << 11,
        XVoStructUnion = 1 << 12,
    }

    internal abstract partial class CSharpSyntaxNode
    {
        public IXParseTree XNode { get; internal set; }
        private XNodeFlags xflags = XNodeFlags.None;
        /// <summary>
        /// Return TRUE when a symbol can be a vostruct
        /// </summary>
        public bool XCanBeVoStruct
        {
            get => xflags.HasFlag(XNodeFlags.XCanBeVoStruct);
            set => xflags = xflags.SetFlag(XNodeFlags.XCanBeVoStruct, value);
        }

        /// <summary>
        /// Return TRUE for a local or field declaration with the IS keyword
        /// </summary>
        public bool XVoIsDecl
        {
            get => xflags.HasFlag(XNodeFlags.XVoIsDecl);
            set => xflags = xflags.SetFlag(XNodeFlags.XVoIsDecl, value);
        }
        /// <summary>
        /// Return TRUE for a syntax node that contains a PCall(Native) or CCall(Native) 
        /// </summary>
        public bool XPCall
        {
            get => xflags.HasFlag(XNodeFlags.XPCall);
            set => xflags = xflags.SetFlag(XNodeFlags.XPCall, value);
        }
        /// <summary>
        /// Return TRUE for a syntax node that was created by the compiler
        /// </summary>
        public bool XGenerated
        {
            get => xflags.HasFlag(XNodeFlags.XGenerated);
            set => xflags = xflags.SetFlag(XNodeFlags.XGenerated, value);
        }
        /// <summary>
        /// Return TRUE for a local or field that has the DIM keyword
        /// </summary>
        public bool XVoIsDim
        {
            get => xflags.HasFlag(XNodeFlags.XVoIsDim);
            set => xflags = xflags.SetFlag(XNodeFlags.XVoIsDim, value);
        }
        /// <summary>
        /// Return TRUE for an expression that is a Chr()
        /// </summary>
        public bool XIsChr
        {
            get => xflags.HasFlag(XNodeFlags.XIsChr);
            set => xflags = xflags.SetFlag(XNodeFlags.XIsChr, value);
        }
        /// <summary>
        /// Return TRUE for the default syntax tree (Functions class) that is generated by the compiler 
        /// </summary>
        public bool XDefaultTree
        {
            get => xflags.HasFlag(XNodeFlags.XDefaultTree);
            set => xflags = xflags.SetFlag(XNodeFlags.XDefaultTree, value);
        }
        /// <summary>
        /// Return TRUE for expressions that contain String2Psz
        /// </summary>
        public bool XIsString2Psz
        {
            get => xflags.HasFlag(XNodeFlags.XIsString2Psz);
            set => xflags = xflags.SetFlag(XNodeFlags.XIsString2Psz, value);
        }

        /// <summary>
        /// Return TRUE when a warning should be generated
        /// </summary>
        public bool XNoWarning
        {
            get => xflags.HasFlag(XNodeFlags.XNoWarning);
            set => xflags = xflags.SetFlag(XNodeFlags.XNoWarning, value);
        }
        /// <summary>
        /// Return TRUE for expressions for which the sign was changed (SLen(), SizeOf())
        /// </summary>
        public bool XNoTypeWarning
        {
            get => xflags.HasFlag(XNodeFlags.XNoTypeWarning);
            set => xflags = xflags.SetFlag(XNodeFlags.XNoTypeWarning, value);
        }
        /// <summary>
        /// Return TRUE for a DataType that was declared as 
        /// </summary>
        public bool XVoStructUnion
        {
            get => xflags.HasFlag(XNodeFlags.XVoStructUnion);
            set => xflags = xflags.SetFlag(XNodeFlags.XVoStructUnion, value);
        }
    }

    internal sealed partial class CompilationUnitSyntax
    {
        internal SyntaxTree SyntaxTree { get; set; }
        private XNodeFlags xflags = XNodeFlags.None;
        internal Dictionary<string, SourceText> IncludedFiles { get; set; } = new Dictionary<string, SourceText>();
        internal ITokenStream XTokens { get; set; } = null;
        internal ITokenStream XPPTokens { get; set; } = null;
        internal IList<Tuple<int, string>> InitProcedures { get; set; } = new List<Tuple<int, string>>();
        internal IList<MemVarFieldInfo> FileWidePublics { get; set; } = new List<MemVarFieldInfo>();
        internal IList<FieldDeclarationSyntax> Globals { get; set; } = new List<FieldDeclarationSyntax>();
        internal IList<PragmaWarningDirectiveTriviaSyntax> PragmaWarnings { get; set; } = null;
        internal IList<PragmaOption> PragmaOptions { get; set; } = null;

        private ConcurrentDictionary<MCA.CSharpSyntaxNode, (bool, List<LocalSymbol>)> functionsThatNeedAccessToLocals;

        internal bool RegisterFunctionThatNeedsAccessToLocals(MCA.CSharpSyntaxNode node, bool writeAccess, List<LocalSymbol> locals)
        {
            if (functionsThatNeedAccessToLocals == null)
                functionsThatNeedAccessToLocals = new ConcurrentDictionary<MCA.CSharpSyntaxNode, (bool, List<LocalSymbol>)>();
            return functionsThatNeedAccessToLocals.TryAdd(node, new(writeAccess, locals));
        }
        internal bool GetLocalsForFunction(MCA.CSharpSyntaxNode node, out bool writeAccess, out List<LocalSymbol> locals, bool remove = true)
        {
            locals = null;
            writeAccess = false;
            var found = false;
            if (functionsThatNeedAccessToLocals != null)
            {
                if (remove)
                {
                    found = functionsThatNeedAccessToLocals.TryRemove(node, out var element);
                    (writeAccess, locals) = element;
                }
                else
                {
                    found = functionsThatNeedAccessToLocals.TryGetValue(node, out var element);
                    (writeAccess, locals) = element;
                }
            }
            return found;
        }
        public bool HasPCall
        {
            get => xflags.HasFlag(XNodeFlags.XPCall);
            internal set => xflags = xflags.SetFlag(XNodeFlags.XPCall, value);
        }

        public bool NeedsProcessing
        {
            get => xflags.HasFlag(XNodeFlags.XNeedsProcessing);
            internal set => xflags = xflags = xflags.SetFlag(XNodeFlags.XNeedsProcessing, value);
        }

        public bool HasDocComments
        {
            get => xflags.HasFlag(XNodeFlags.XDocComments);
            internal set => xflags = xflags.SetFlag(XNodeFlags.XDocComments, value);
        }

        internal XSharpParser.SourceContext XSource => XNode as XSharpParser.SourceContext;
        internal Dictionary<String, FieldDeclarationSyntax> LiteralSymbols { get; set; } = new Dictionary<string, FieldDeclarationSyntax>();
        internal Dictionary<String, Tuple<string, FieldDeclarationSyntax>> LiteralPSZs { get; set; } = new Dictionary<string, Tuple<string, FieldDeclarationSyntax>>();
    }
}

namespace Microsoft.CodeAnalysis.CSharp
{
    using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
    public abstract partial class CSharpSyntaxNode
    {
        internal bool XVoStructUnion => CsGreen.XVoStructUnion;
        internal bool XVoIsDecl => CsGreen.XVoIsDecl;
        internal bool XPCall => CsGreen.XPCall;
        internal bool XVoIsDim => CsGreen.XVoIsDim;
        internal bool XIsChr => CsGreen.XIsChr;
        internal bool XIsString2Psz => CsGreen.XIsString2Psz;
        internal bool XDefaultTree => CsGreen.XDefaultTree;

    }
}

namespace Microsoft.CodeAnalysis
{
    public abstract partial class SyntaxNode
    {
        internal CSharp.CSharpSyntaxNode CsNode => (CSharp.CSharpSyntaxNode)this;
        internal IXParseTree XNode => CsNode.CsGreen.XNode ?? CsNode.Parent?.XNode;
        internal bool XCanBeVoStruct => CsNode.CsGreen.XCanBeVoStruct;

        internal bool XGenerated
        {
            get => CsNode.CsGreen.XGenerated;
            set => CsNode.CsGreen.XGenerated = value;
        }
        internal bool XNoWarning
        {
            get => CsNode.CsGreen.XNoWarning;
            set => CsNode.CsGreen.XNoWarning = value;
        }
        /// <summary>
        /// Walk the X# parse tree to detect if the expression or one of its children is a "cast" in code.
        /// </summary>
        internal bool XIsExplicitTypeCastInCode
        {
            get
            {
                if (XNode is XSharpParserRuleContext node)
                {
                    return node.ContainsCast;
                }
                return false;
            }
        }
        internal bool XIsMissingArgument
        {
            get
            {
                var n = XNode;
                if (n != null)
                {
                    if (n is XSharpParser.NamedArgumentContext nac)
                        return nac.IsMissing;
                    if (n is XSharpParser.UnnamedArgumentContext uac)
                        return uac.IsMissing;
                }
                return false;
            }
        }
        internal bool XIsNil
        {
            get
            {
                var n = XNode;
                return n is XSharpParserRuleContext prc && prc.Start.Type == XSharpLexer.NIL;
            }

        }
        internal bool XIsCodeBlock
        {
            get
            {
                var n = XNode;
                if (n != null)
                {
                    return n.IsRealCodeBlock();
                }
                return false;
            }
        }
        internal string XCodeBlockSource
        {
            get
            {
                var n = XNode;
                if (n != null)
                {
                    return n.CodeBlockSource();
                }
                return null;
            }
        }
        /// <summary>
        /// Return TRUE for expressions for which the sign was changed (SLen(), SizeOf())
        /// </summary>
        internal bool XNoTypeWarning
        {
            get
            {
                if (this.CsNode.CsGreen.XNoTypeWarning)
                    return true;
                foreach (var child in this.ChildNodes())
                {
                    if (child.XNoTypeWarning)
                        return true;
                }
                return false;
            }
        }

        /// <summary>
        /// Walk a an Syntax tree to detect if a generated element is involved.
        /// NOTE: this may not be complete !
        /// </summary>
        internal bool XContainsGeneratedExpression
        {
            get
            {
                if (this.XGenerated)
                    return true;
                foreach (var child in this.ChildNodes())
                {
                    if (child.XContainsGeneratedExpression)
                        return true;
                }
                return false;
            }
        }
    }
}

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public sealed partial class DocumentationCommentTriviaSyntax
    {
        // Add field so we can resolve the cref nodes
        internal CSharpSyntaxNode OriginalNode;
    }
    public sealed partial class CompilationUnitSyntax
    {
        private InternalSyntax.CompilationUnitSyntax internalUnit => (InternalSyntax.CompilationUnitSyntax)this.CsGreen;
        public XSharpParser.SourceContext XSource => internalUnit.XSource;
        public ITokenStream XTokens => internalUnit.XTokens;
        public ITokenStream XPPTokens => internalUnit.XPPTokens;
        public Dictionary<string, SourceText> IncludedFiles => internalUnit.IncludedFiles;
        public bool NeedsProcessing => internalUnit.NeedsProcessing;
        public bool HasDocComments => internalUnit.HasDocComments;
        internal Dictionary<String, InternalSyntax.FieldDeclarationSyntax> LiteralSymbols => internalUnit.LiteralSymbols;
        internal Dictionary<String, Tuple<string, InternalSyntax.FieldDeclarationSyntax>> LiteralPSZs => internalUnit.LiteralPSZs;
        internal IList<InternalSyntax.PragmaWarningDirectiveTriviaSyntax> PragmaWarnings => internalUnit.PragmaWarnings;
        internal IList<PragmaOption> PragmaOptions => internalUnit.PragmaOptions;
        internal bool RegisterFunctionThatNeedsAccessToLocals(MCA.CSharpSyntaxNode node, bool writeAccess, List<LocalSymbol> locals)
        {
            return internalUnit.RegisterFunctionThatNeedsAccessToLocals(node, writeAccess, locals);
        }
        internal bool GetLocalsForFunction(MCA.CSharpSyntaxNode node, out bool writeAccess, out List<LocalSymbol> locals, bool remove = true)
        {
            return internalUnit.GetLocalsForFunction(node, out writeAccess, out locals, remove);
        }
    }
}


