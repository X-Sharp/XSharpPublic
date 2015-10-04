using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public partial class XSharpParser
    {
        internal ParseTreeTypedProperty Errors = new ParseTreeTypedProperty();

        //internal ParseTreeTypedProperty CsNodes = new ParseTreeTypedProperty();
    }
}

namespace Antlr4.Runtime
{
    namespace Tree
    {
        using Microsoft.CodeAnalysis.CSharp;

        internal class ParseErrorData
        {
            internal readonly IParseTree Node;
            internal readonly ErrorCode Code;
            internal readonly object[] Args;
            internal ParseErrorData(ErrorCode code) : this(null, code, SpecializedCollections.EmptyObjects) { }
            internal ParseErrorData(ErrorCode code, params object[] args) : this(null, code, args) { }
            internal ParseErrorData(IParseTree node, ErrorCode code) : this(node, code, SpecializedCollections.EmptyObjects) { }
            internal ParseErrorData(IParseTree node, ErrorCode code, params object[] args)
            {
                this.Node = node;
                this.Code = code;
                this.Args = args;
            }
            /*protected static SyntaxDiagnosticInfo MakeError(CSharpSyntaxNode node, ErrorCode code, params object[] args)
            {
                return new SyntaxDiagnosticInfo(node.GetLeadingTriviaWidth(), node.Width, code, args);
            }*/
        }

        public partial interface IParseTree
        {
            object CsNode { get; set;  }
        }
        public partial class TerminalNodeImpl
        {
            public object CsNode { get; set; }
        }
    }

    public partial class RuleContext
    {
        public object CsNode { get; set; }
    }

}

/*namespace Microsoft.CodeAnalysis.CSharp
{
    public abstract partial class CSharpSyntaxNode
    {
        public ParserRuleContext XNode { get { return (ParserRuleContext)(((InternalSyntax.CSharpSyntaxNode)(this.Green)).XNode); } }
    }
}*/

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal abstract partial class CSharpSyntaxNode
    {
        public IParseTree XNode { get; internal set; }
    }
}

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public sealed partial class CompilationUnitSyntax
    {
        public XSharpParser.SourceContext XSource { get { return (XSharpParser.SourceContext)(((InternalSyntax.CompilationUnitSyntax)(this.Green)).XNode); } }
    }
}