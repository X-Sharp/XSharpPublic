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
        public ParserRuleContext XNode { get; internal set; }
    }
}

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public sealed partial class CompilationUnitSyntax
    {
        public XSharpParser.SourceContext XSource { get { return (XSharpParser.SourceContext)(((InternalSyntax.CompilationUnitSyntax)(this.Green)).XNode); } }
    }
}