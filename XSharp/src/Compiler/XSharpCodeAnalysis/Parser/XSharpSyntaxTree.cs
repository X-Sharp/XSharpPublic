//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static partial class SyntaxFactory
    {
        readonly static internal SyntaxTrivia WS = Whitespace(" ");

        internal static SyntaxToken MakeTokenNoWs(SyntaxKind kind)
        {
            return Token(kind);
        }
        internal static SyntaxToken MakeToken(SyntaxKind kind)
        {
            return Token(WS, kind, WS);
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
    enum XNodeFlags : byte
    {
        None = 0,
        XVodecl = 1 << 0,
        XVoIsDecl = 1 << 1,
        XPCall = 1 << 2,
        XGenerated = 1 << 3,
        XVoIsDim = 1 << 4,
        XPragmas = 1 << 5,
        XDocComments = 1 << 6,
        XNeedsProcessing = 1 << 7,
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
    }

    internal sealed partial class CompilationUnitSyntax
    {
        private XNodeFlags xflags = XNodeFlags.None;
        public Dictionary<string, SourceText> IncludedFiles { get; internal set; } = new Dictionary<string, SourceText>();
        public ITokenStream XTokens { get; internal set; } = default(ITokenStream);
        public ITokenStream XPPTokens { get; internal set; } = default(ITokenStream);
        public IList<Tuple<int, string>> InitProcedures { get; internal set; } = new List<Tuple<int, string>>();
        public IList< MemVarFieldInfo> FileWidePublics { get; internal set; } = new List<MemVarFieldInfo>();
        public IList<FieldDeclarationSyntax> Globals { get; internal set; } = new List<FieldDeclarationSyntax>();
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

        public bool HasPragmas
        {
            get { return xflags.HasFlag(XNodeFlags.XPragmas); }
            internal set { xflags = xflags.SetFlag(XNodeFlags.XPragmas, value); }
        }
        public XSharpParser.SourceContext XSource => XNode as XSharpParser.SourceContext;
        public Dictionary<String, FieldDeclarationSyntax> LiteralSymbols { get; internal set; } = new Dictionary<string, FieldDeclarationSyntax>();
        public Dictionary<String, Tuple<string, FieldDeclarationSyntax>> LiteralPSZs { get; internal set; } =new Dictionary<string, Tuple<string, FieldDeclarationSyntax>>();
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
                        return nac.Expr == null;
                    if (n is XSharpParser.UnnamedArgumentContext uac)
                        return uac.Expr == null;
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
    }
}

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{

    public sealed partial class CompilationUnitSyntax
    {
        private InternalSyntax.CompilationUnitSyntax internalUnit => (InternalSyntax.CompilationUnitSyntax) this.CsGreen;
        public XSharpParser.SourceContext XSource => internalUnit.XSource;
        public ITokenStream XTokens => internalUnit.XTokens;
        public ITokenStream XPPTokens => internalUnit.XPPTokens;
        public Dictionary<string, SourceText> IncludedFiles => internalUnit.IncludedFiles;
        public bool NeedsProcessing => internalUnit.NeedsProcessing;
        public bool HasDocComments => internalUnit.HasDocComments;
        public bool HasPragmas => internalUnit.HasPragmas;
        internal Dictionary<String, InternalSyntax.FieldDeclarationSyntax> LiteralSymbols => internalUnit.LiteralSymbols;
        internal Dictionary<String, Tuple<string, InternalSyntax.FieldDeclarationSyntax> > LiteralPSZs => internalUnit.LiteralPSZs;
    }
}


