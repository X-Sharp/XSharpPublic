/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public partial class XSharpParser
    {
        //internal ParseTreeTypedProperty Errors = new ParseTreeTypedProperty();

        //internal ParseTreeTypedProperty CsNodes = new ParseTreeTypedProperty();
    }
}

namespace Antlr4.Runtime {
    namespace Tree {
        using Microsoft.CodeAnalysis.CSharp;

        internal class ParseErrorData {
            internal readonly IParseTree Node;
            internal readonly ErrorCode Code;
            internal readonly object[] Args;
            internal ParseErrorData(ErrorCode code) : this(node: null, code: code, args: SpecializedCollections.EmptyObjects) { }
            internal ParseErrorData(ErrorCode code, params object[] args) : this(node: null, code: code, args: args) { }
            internal ParseErrorData(IParseTree node, ErrorCode code) : this(node, code, SpecializedCollections.EmptyObjects) { }
            internal ParseErrorData(IParseTree node, ErrorCode code, params object[] args) {
                this.Node = node;
                this.Code = code;
                this.Args = args;
            }
            internal ParseErrorData(IToken token, ErrorCode code, params object[] args) {
                if (token == null)
                    token = new PPToken(0, "");
                else if (!(token is PPToken))
                {
                    token = new PPToken(token);
                }
                this.Node = new TerminalNodeImpl(token);
                this.Code = code;
                this.Args = args;
            }
            /*protected static SyntaxDiagnosticInfo MakeError(CSharpSyntaxNode node, ErrorCode code, params object[] args)
            {
                return new SyntaxDiagnosticInfo(node.GetLeadingTriviaWidth(), node.Width, code, args);
            }*/
            internal static List<ParseErrorData> NewBag() {
                return new List<ParseErrorData>();
            }
        }

        public partial interface IParseTree {
            object CsNode { get; set; }
            bool IsHidden { get; }
            int Position { get; }
            int FullWidth { get; }
            string SourceFileName { get; }
            string MappedFileName { get; }
            int MappedLine { get; }
            IToken SourceSymbol { get; }
        }
        public partial class TerminalNodeImpl : Microsoft.CodeAnalysis.IMessageSerializable {
            public object CsNode { get; set; }
            public bool IsHidden { get { return false; } }
            public int Position { get { return Symbol.StartIndex; } }
            public int FullWidth { get { return Symbol.StopIndex - Symbol.StartIndex + 1; } }
            public string SourceFileName
            {
                get
                {
                    var ct = (Symbol as PPToken);
                    if (ct != null)
                    {
                        if (ct.TokenSource != null && !String.IsNullOrEmpty(ct.TokenSource.SourceName))
                            return ct.TokenSource.SourceName;
                        return ct.SourceFileName;
                    }
                    return "<unknown>";
                }
            }
            public string MappedFileName { get { return ((PPToken)Symbol).MappedFileName; } }
            public int MappedLine { get { return ((PPToken)Symbol).MappedLine; } }
            public IToken SourceSymbol { get { return ((PPToken)Symbol).SourceSymbol; } }
            public override string ToString() { return this.GetText(); }
        }
    }

     public partial class RuleContext {
        public object CsNode { get; set; }
        public virtual int Position { get; }
        public virtual bool IsHidden { get { return false; } }
        public virtual int FullWidth { get; }
        public virtual string SourceFileName { get; }
        public virtual string MappedFileName { get; }
        public virtual int MappedLine { get; }
        public virtual IToken SourceSymbol { get; }

        internal List<ParseErrorData> ErrorData;

        internal bool HasErrors() {
            return (ErrorData != null) && ErrorData.Count > 0;
        }

        internal void AddError(ParseErrorData e) {
            if(ErrorData == null)
                ErrorData = new List<ParseErrorData>();
            ErrorData.Add(e);
        }

    }


    public class MemVarFieldInfo {

        public string Name { get; private set; }
        public string Alias { get; private set; }
        public bool IsField { get; private set; }
        internal MemVarFieldInfo(string name, string alias, bool field) {
            Name = name;
            Alias = alias;
            IsField = field;
        }
    }


    public partial class ParserRuleContext : Microsoft.CodeAnalysis.IMessageSerializable {
        int iBPLength = -1;
        int iBpStart = -1;
        public override bool IsHidden { get { return iBPLength == -1; } }
        public override int Position
        {
            get
            {
                if (iBpStart >= 0)
                    return iBpStart;
                return Start.StartIndex;
            }
        }
        public override int FullWidth {
            get {
                if(iBPLength > 0)
                    return iBPLength;
                if(Stop != null)
                    return Stop.StopIndex - Start.StartIndex + 1;
                else
                    return Start.StopIndex - Start.StartIndex + 1;

            }
        }
        public override string SourceFileName { get { return (Start as PPToken).SourceFileName; } }
        public override string MappedFileName { get { return (Start as PPToken).MappedFileName; } }
        public override int MappedLine { get { return (Start as PPToken).MappedLine; } }
        public override IToken SourceSymbol { get { return (Start as PPToken).SourceSymbol; } }
        public override string ToString() {
            /*return this.GetText();*/
            var s = this.GetType().ToString();
            return s.Substring(s.LastIndexOfAny(".+".ToCharArray()) + 1).Replace("Context", "");
        }
        public void SetSequencePoint(IToken start, IToken end)
        {
            if (end != null && start != null)
            {
                iBpStart = start.StartIndex;
                if (end.StopIndex >= start.StartIndex)
                    iBPLength = end.StopIndex - start.StartIndex+1;
                else
                    iBPLength = 1;
            }

        }

        public void SetSequencePoint(IToken next)
        {
			if (next != null)
			{
	            if (next.StartIndex > this.Start.StartIndex)
	                iBPLength = next.StartIndex - this.Start.StartIndex ;
	            else
	                iBPLength = 1;
	            if (iBPLength < 0)
	                iBPLength = 1;
			}

        }

        public void SetSequencePoint()
        {
            SetSequencePoint(Start, Stop);
        }

        public void SetSequencePoint(ParserRuleContext context)
        {
            if (context.Stop != null)
                SetSequencePoint(context.Start, context.Stop);
            else
                SetSequencePoint(context.Start, context.Start);
        }
        internal string ParentName
        {
            get
            {
                string name = "";
                if (Parent is XSharpParser.IEntityContext)
                {
                    name = ((XSharpParser.IEntityContext)Parent).Name + ".";
                }
                else if (Parent.Parent is XSharpParser.IEntityContext)
                {
                    name = ((XSharpParser.IEntityContext)Parent.Parent).Name + ".";
                }
                else if (Parent is XSharpParser.Namespace_Context)
                {
                    name = ((XSharpParser.Namespace_Context)Parent).Name.GetText() + "." ;
                }
                return name;
            }
        }
    }

    internal static class RuleExtensions {
        internal static void Put<T>([NotNull] this IParseTree t, T node) where T : InternalSyntax.CSharpSyntaxNode {
            if(node != null) {
                node.XNode = t;
                t.CsNode = node;
            }
        }

        internal static T Get<T>([NotNull] this IParseTree t) where T : InternalSyntax.CSharpSyntaxNode {
            if(t == null || t.CsNode == null)
                return default(T);

            return (T)t.CsNode;
        }

        internal static void PutList<T>([NotNull] this IParseTree t, InternalSyntax.SyntaxList<T> node) where T : InternalSyntax.CSharpSyntaxNode {
            //node.XNode = t;
            t.CsNode = node;
        }

        internal static InternalSyntax.SyntaxList<T> GetList<T>([NotNull] this IParseTree t) where T : InternalSyntax.CSharpSyntaxNode {
            if(t.CsNode == null)
                return default(InternalSyntax.SyntaxList<T>);

            return (InternalSyntax.SyntaxList<T>)t.CsNode;
        }

        internal static TNode WithAdditionalDiagnostics<TNode>([NotNull] this TNode node, params DiagnosticInfo[] diagnostics) where TNode : InternalSyntax.CSharpSyntaxNode {
            DiagnosticInfo[] existingDiags = node.GetDiagnostics();
            int existingLength = existingDiags.Length;
            if(existingLength == 0) {
                return node.WithDiagnosticsGreen(diagnostics);
            } else {
                DiagnosticInfo[] result = new DiagnosticInfo[existingDiags.Length + diagnostics.Length];
                existingDiags.CopyTo(result, 0);
                diagnostics.CopyTo(result, existingLength);
                return node.WithDiagnosticsGreen(result);
            }
        }

        internal static bool isInInterface([NotNull] this RuleContext context) {
            return ((context.Parent is XSharpParser.ClassmemberContext) && (context.Parent.Parent is XSharpParser.Interface_Context))
                || (context.Parent is XSharpParser.Interface_Context);
        }

        internal static bool isInClass([NotNull] this RuleContext context) {
            return ((context.Parent is XSharpParser.ClassmemberContext) && (context.Parent.Parent is XSharpParser.Class_Context))
                || (context.Parent is XSharpParser.Class_Context);
        }
    }
}


namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static partial class SyntaxFactory
    {
        readonly static internal SyntaxTrivia WS = Whitespace(" ");

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
        XGenerated =1 << 3,
    }

    internal abstract partial class CSharpSyntaxNode
    {
        public IParseTree XNode { get; internal set; }
        private XNodeFlags xflags = XNodeFlags.None;
        public bool XVoDecl
        {
            get { return xflags.HasFlag(XNodeFlags.XVodecl); }
            set { if (value) xflags |= XNodeFlags.XVodecl; else xflags &= ~XNodeFlags.XVodecl; }
        }

        public bool XVoIsDecl
        {
            get { return xflags.HasFlag(XNodeFlags.XVoIsDecl); }
            set { if (value) xflags |= XNodeFlags.XVoIsDecl; else xflags &= ~XNodeFlags.XVoIsDecl; }
        }
        public bool XPCall
        {
            get { return xflags.HasFlag(XNodeFlags.XPCall); }
            set { if (value) xflags |= XNodeFlags.XPCall; else xflags &= ~XNodeFlags.XPCall; }
        }
        public bool XGenerated
        {
            get { return xflags.HasFlag(XNodeFlags.XGenerated); }
            set { if (value) xflags |= XNodeFlags.XGenerated; else xflags &= ~XNodeFlags.XGenerated; }
        }
    }

    internal sealed partial class CompilationUnitSyntax
    {
        public Dictionary<string, SourceText> IncludedFiles { get; internal set; }
        public ITokenStream XTokens { get; internal set; }
        public IList<Tuple<int, string>> InitProcedures { get; internal set; }
        public IList<FieldDeclarationSyntax> Globals { get; internal set; }
        public bool HasPCall { get; internal set; }

    }

}

namespace Microsoft.CodeAnalysis.CSharp
{
    public abstract partial class CSharpSyntaxNode
    {
        internal IParseTree XNode { get { return (((InternalSyntax.CSharpSyntaxNode)(Green)).XNode) ?? Parent?.XNode; } }
        internal bool XVoDecl { get { return ((InternalSyntax.CSharpSyntaxNode)(Green)).XVoDecl; } }
        internal bool XVoIsDecl { get { return ((InternalSyntax.CSharpSyntaxNode)(Green)).XVoIsDecl; } }
        internal bool XPCall { get { return ((InternalSyntax.CSharpSyntaxNode)(Green)).XPCall; } }
        internal bool XGenerated { get { return ((InternalSyntax.CSharpSyntaxNode)(Green)).XGenerated; } }
        internal bool XIsMissingArgument {
            get
            {
                var n = XNode;
                if (n != null)
                {
                    if (n is XSharpParser.NamedArgumentContext)
                        return ((XSharpParser.NamedArgumentContext)n).Expr == null;
                    if (n is XSharpParser.UnnamedArgumentContext)
                        return ((XSharpParser.UnnamedArgumentContext)n).Expr == null;
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
        public XSharpParser.SourceContext XSource { get { return (XSharpParser.SourceContext)(((InternalSyntax.CompilationUnitSyntax)(this.Green)).XNode); } }
        public ITokenStream XTokenStream { get { return (((InternalSyntax.CompilationUnitSyntax)(this.Green)).XTokens); } }
        internal Dictionary<string, SourceText> IncludedFiles
        {
            get
            {
                return (((InternalSyntax.CompilationUnitSyntax)(this.Green)).IncludedFiles);
            }
        }
    }
}

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser {


    public partial class XSharpParser : Parser {
        public interface ILoopStmtContext
        {
            StatementBlockContext Statements { get; }
        }
        public interface ITypeContext : IEntityContext
        {
            IList<ClassmemberContext> Members { get; }
        }
        public interface IEntityContext : IRuleNode {
            EntityData Data { get; }
            IList<ParameterContext> Params { get; }
            DatatypeContext ReturnType { get; }
            String Name { get; }
            String ShortName { get; }
        }
        [FlagsAttribute]
        enum MethodFlags: byte {
            None = 0,
            ClipperCallingConvention = 1 << 0,
            MissingReturnType = 1 << 1,
            UsesPSZ = 1 << 2,
            MustBeUnsafe = 1 << 3,
            HasTypedParameter = 1 << 4,
            UsesPCount = 1 << 5,
            UsesGetMParam = 1 << 6,
            MustBeVoid = 1 << 7
        }

        public class EntityData {
            MethodFlags flags;

            public bool HasClipperCallingConvention {
                get { return flags.HasFlag( MethodFlags.ClipperCallingConvention) ; }
                set { if(value) flags |= MethodFlags.ClipperCallingConvention; else flags &= ~MethodFlags.ClipperCallingConvention; }
            }

            public bool HasMissingReturnType {
                get { return flags.HasFlag( MethodFlags.MissingReturnType) ; }
                set { if(value) flags |= MethodFlags.MissingReturnType; else flags &= ~MethodFlags.MissingReturnType; }
            }
            public bool HasTypedParameter {
                get { return flags.HasFlag(MethodFlags.HasTypedParameter) ; }
                set { if(value) flags |= MethodFlags.HasTypedParameter; else flags &= ~MethodFlags.HasTypedParameter; }
            }
            public bool UsesPSZ {
                get { return flags.HasFlag(MethodFlags.UsesPSZ); }
                set { if(value) flags |= MethodFlags.UsesPSZ; else flags &= ~MethodFlags.UsesPSZ; }
            }
            public bool MustBeUnsafe {
                get { return flags.HasFlag(MethodFlags.MustBeUnsafe) ; }
                set { if(value) flags |= MethodFlags.MustBeUnsafe; else flags &= ~MethodFlags.MustBeUnsafe; }
            }

            public bool UsesPCount {
                get { return flags.HasFlag(MethodFlags.UsesPCount) ; }
                set { if(value) flags |= MethodFlags.UsesPCount; else flags &= ~MethodFlags.UsesPCount; }
            }
            public bool UsesGetMParam
            {
                get { return flags.HasFlag( MethodFlags.UsesGetMParam) ; }
                set { if(value) flags |= MethodFlags.UsesGetMParam; else flags &= ~MethodFlags.UsesGetMParam; }
            }

            public bool MustBeVoid            // Assign, SET, Event Accessor
            {
                get { return flags.HasFlag(MethodFlags.MustBeVoid); }
                set { if (value) flags |= MethodFlags.MustBeVoid; else flags &= ~MethodFlags.MustBeVoid; }
            }

            private List<MemVarFieldInfo> Fields;
            public void AddField(string Name, string Alias, bool Field) {
                if(Fields == null)
                    Fields = new List<MemVarFieldInfo>();
                Fields.Add(new MemVarFieldInfo(Name, Alias, Field));
            }
            public MemVarFieldInfo GetField(string Name) {
                if(Fields != null) {
                    foreach(var field in Fields) {
                        if(string.Compare(Name, field.Name, StringComparison.OrdinalIgnoreCase) == 0)
                            return field;
                    }
                }
                return null;
            }


        }

        public partial class RepeatStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class WhileStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class ForeachStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }
        public partial class ForStmtContext : ILoopStmtContext
        {
            public StatementBlockContext Statements { get { return StmtBlk; } }
        }

        public partial class ProcedureContext : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
        }

        public partial class FunctionContext : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
        }

        public partial class MethodContext : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String ShortName => this.Id.GetText();
            public String Name
            {
                get
                {
                    string name= this.Id.GetText();
                    if (this.T.Token.Type == XSharpParser.ACCESS)
                        name += ":Access";
                    else if (this.T.Token.Type == XSharpParser.ASSIGN)
                        name += ":Assign";
                    else
                        name += "()";
                    return ParentName + name;
                }
            }
        }

        public partial class EventAccessorContext : ParserRuleContext, IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
        }

        public partial class PropertyAccessorContext : ParserRuleContext, IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
        }

        public partial class ClsctorContext : ClassmemberContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "ctor";
        }
        public partial class ClsdtorContext : ClassmemberContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "Finalize";
        }
        public partial class Event_Context : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class VodefineContext : ParserRuleContext, IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class PropertyContext : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data { get { return data; } }
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class Operator_Context : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName
            {
                get
                {
                    string name;
                    if (Operation != null)
                        name = Operation.GetText() + Gt?.Text;
                    else
                        name = Conversion.GetText();
                    return name;
                }

            }
        }
        public partial class Delegate_Context : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
        }
        public partial class Interface_Context : ParserRuleContext, ITypeContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();

            public IList<ClassmemberContext> Members => this._Members;
        }
        public partial class Class_Context : ParserRuleContext, ITypeContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
            public IList<ClassmemberContext> Members => this._Members;
        }
        public partial class Structure_Context : ParserRuleContext, ITypeContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
            public IList<ClassmemberContext> Members => this._Members;
        }
        public partial class VodllContext : ParserRuleContext, IEntityContext {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();
        }

        public partial class VoglobalContext : ParserRuleContext, IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => this.Vars.DataType;
            public String Name => this.Vars._Var.FirstOrDefault().Id.GetText();
            public String ShortName => this.Vars._Var.FirstOrDefault().Id.GetText();
        }
        public partial class FuncprocModifiersContext: ParserRuleContext {
            public bool IsStaticVisible { get; set; }
        }
    }
}
