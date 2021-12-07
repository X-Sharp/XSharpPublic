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

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static partial class SyntaxFactory
    {
        internal static readonly SyntaxTrivia WS = Whitespace(" ");
        //internal static readonly ConcurrentDictionary<SyntaxKind, SyntaxToken> tokens = new ConcurrentDictionary<SyntaxKind, SyntaxToken>();

        internal static SyntaxToken MakeTokenNoWs(SyntaxKind kind)
        {
            return Token(kind);
        }
        internal static SyntaxToken MakeToken(SyntaxKind kind)
        {
            //if (kind != SyntaxKind.NullKeyword && tokens.TryGetValue(kind, out var token))
            //{
            //    return token;
            //}
            var token = Token(WS, kind, WS);
            //if (kind != SyntaxKind.NullKeyword)
            //{ 
            //    tokens.TryAdd(kind, token);
            //}
            return token;
        }

        internal static SyntaxToken MakeToken(SyntaxKind kind, string text)
        {
            return Token(WS, kind, text, text, WS);
        }

        internal static SyntaxToken MakeIdentifier(string text)
        {
            return Identifier(WS, text, WS);
        }
    }
    [FlagsAttribute]
    enum XNodeFlags : short
    {
        None = 0,
        XVodecl = 1 << 0,
        XVoIsDecl = 1 << 1,
        XPCall = 1 << 2,
        XGenerated = 1 << 3,
        XVoIsDim = 1 << 4,
        XDocComments = 1 << 5,
        XNeedsProcessing = 1 << 6,
        XIsChr = 1 << 7,
        XDefaultTree = 1 << 8,
        XIsString2Psz = 1 << 9,
    }

    internal abstract partial class CSharpSyntaxNode
    {
        public IXParseTree XNode { get; internal set; }
        private XNodeFlags xflags = XNodeFlags.None;
        public bool XVoDecl
        {
            get => xflags.HasFlag(XNodeFlags.XVodecl);
            set => xflags = xflags.SetFlag(XNodeFlags.XVodecl, value);
        }

        public bool XVoIsDecl
        {
            get => xflags.HasFlag(XNodeFlags.XVoIsDecl);
            set => xflags = xflags.SetFlag(XNodeFlags.XVoIsDecl, value);
        }
        public bool XPCall
        {
            get => xflags.HasFlag(XNodeFlags.XPCall);
            set => xflags = xflags.SetFlag(XNodeFlags.XPCall, value);
        }
        public bool XGenerated
        {
            get => xflags.HasFlag(XNodeFlags.XGenerated);
            set => xflags = xflags.SetFlag(XNodeFlags.XGenerated, value);
        }
        public bool XVoIsDim
        {
            get => xflags.HasFlag(XNodeFlags.XVoIsDim);
            set => xflags = xflags.SetFlag(XNodeFlags.XVoIsDim, value);
        }
        public bool XIsChr
        {
            get => xflags.HasFlag(XNodeFlags.XIsChr);
            set => xflags = xflags.SetFlag(XNodeFlags.XIsChr, value);
        }
        public bool XDefaultTree
        {
            get => xflags.HasFlag(XNodeFlags.XDefaultTree);
            set => xflags = xflags.SetFlag(XNodeFlags.XDefaultTree, value);
        }
        public bool XIsString2Psz
        {
            get => xflags.HasFlag(XNodeFlags.XIsString2Psz);
            set => xflags = xflags.SetFlag(XNodeFlags.XIsString2Psz, value);
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
        internal IList< MemVarFieldInfo> FileWidePublics { get; set; } = new List<MemVarFieldInfo>();
        internal IList<FieldDeclarationSyntax> Globals { get; set; } = new List<FieldDeclarationSyntax>();
        internal IList<PragmaWarningDirectiveTriviaSyntax> PragmaWarnings { get; set; } = null;
        internal IList<PragmaOption> PragmaOptions { get; set; } = null;

        private ConcurrentDictionary<MCA.CSharpSyntaxNode, (bool, List<LocalSymbol>) > functionsThatNeedAccessToLocals ;

        internal bool RegisterFunctionThatNeedsAccessToLocals(MCA.CSharpSyntaxNode node, bool writeAccess, List<LocalSymbol> locals)
        {
            if (functionsThatNeedAccessToLocals == null)
                functionsThatNeedAccessToLocals = new ConcurrentDictionary<MCA.CSharpSyntaxNode, (bool, List<LocalSymbol>) >();
            return functionsThatNeedAccessToLocals.TryAdd(node, new (writeAccess, locals));
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
        internal bool XVoDecl => CsGreen.XVoDecl;
        internal bool XVoIsDecl => CsGreen.XVoIsDecl;
        internal bool XPCall => CsGreen.XPCall;
        internal bool XGenerated => CsGreen.XGenerated;
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
        internal bool XIsVoCast
        {
            get
            {
                var node = XNode as XSharpParserRuleContext;
                if (node is XSharpParser.PrimaryExpressionContext prim)
                    return prim.Expr is XSharpParser.VoCastExpressionContext;
                else
                    return node is XSharpParser.VoCastExpressionContext;
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
        private InternalSyntax.CompilationUnitSyntax internalUnit => (InternalSyntax.CompilationUnitSyntax) this.CsGreen;
        public XSharpParser.SourceContext XSource => internalUnit.XSource;
        public ITokenStream XTokens => internalUnit.XTokens;
        public ITokenStream XPPTokens => internalUnit.XPPTokens;
        public Dictionary<string, SourceText> IncludedFiles => internalUnit.IncludedFiles;
        public bool NeedsProcessing => internalUnit.NeedsProcessing;
        public bool HasDocComments => internalUnit.HasDocComments;
        internal Dictionary<String, InternalSyntax.FieldDeclarationSyntax> LiteralSymbols => internalUnit.LiteralSymbols;
        internal Dictionary<String, Tuple<string, InternalSyntax.FieldDeclarationSyntax> > LiteralPSZs => internalUnit.LiteralPSZs;
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


