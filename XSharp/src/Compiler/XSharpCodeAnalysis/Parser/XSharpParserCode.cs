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
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

using Microsoft.CodeAnalysis.CSharp;
using System.Linq;
using System.Collections.Generic; 
using System;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Microsoft.CodeAnalysis; 
#if !VSPARSER
using MCT = Microsoft.CodeAnalysis.Text;
using CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax;

#endif
namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public partial class XSharpParser
    {
        public CSharpParseOptions Options { get; set; }
        public XSharpDialect Dialect => Options.Dialect;
        public bool AllowNamedArgs => Options.AllowNamedArguments;
        public bool AllowXBaseVariables => Dialect.AllowXBaseVariables();
        public bool IsScript => Options.Kind == SourceCodeKind.Script;
        public bool IsXPP => Options.Dialect == XSharpDialect.XPP;
        
        void missingToken(string token)
        {
            if (Interpreter.PredictionMode == Antlr4.Runtime.Atn.PredictionMode.Sll)
                throw new ParseCanceledException("Missing token'" + token + "'");
            NotifyErrorListeners("Missing token'" + token + "'");
        }
        void unexpectedToken(string token)
        {
            if (Interpreter.PredictionMode == Antlr4.Runtime.Atn.PredictionMode.Sll)
                throw new ParseCanceledException("Unexpected '" + token + "'token");

            NotifyErrorListeners("Unexpected '" + token + "' token");
        }
        void eosExpected(IToken token)
        {
            if (Interpreter.PredictionMode == Antlr4.Runtime.Atn.PredictionMode.Sll)
                unexpectedToken(token?.Text);
            string msg = "Expecting end of statement, found '" + token?.Text + "'";
            NotifyErrorListeners(token, msg, null);

        }
        void unexpectedToken(IToken token)
        {
            if (Interpreter.PredictionMode == Antlr4.Runtime.Atn.PredictionMode.Sll)
                unexpectedToken(token?.Text);
            string msg = "Too many '" + token?.Text + "' tokens";
            NotifyErrorListeners(token, msg, null);
        }


        bool validExpressionStmt()
        {
            var la = InputStream.La(2);
            if (la != LPAREN)
                return true;
            la = InputStream.La(1);
            return la != CONSTRUCTOR && la != DESTRUCTOR;
        }
        public partial class ParenExpressionContext
        {
            public ExpressionContext Expr => _Exprs[_Exprs.Count - 1];
        }

#if !VSPARSER
        public interface IPartialPropertyContext : IEntityContext
        {
            List<MethodContext> PartialProperties { get; set; }
        }

        public interface IGlobalEntityContext : IEntityContext
        {
            FuncprocModifiersContext FuncProcModifiers { get; }
        }
        public interface ILoopStmtContext
        {
            StatementBlockContext Statements { get; }
        }

        public interface IEntityWithBodyContext : IEntityContext
        {
            StatementBlockContext Statements { get; }
        }
        public interface IEntityContext : IRuleNode, IXParseTree
        {
            EntityData Data { get; }
            ParameterListContext Params { get; }
            DatatypeContext ReturnType { get; }
            String Name { get; }
            String ShortName { get; }
        }
        internal interface IXPPEntityContext: IEntityWithBodyContext
        {
            XppmemberModifiersContext Mods { get; }
            AttributesContext Atts { get; }
            InternalSyntax.XppDeclaredMethodInfo Info { get; }
            ParameterListContext Parameters { get; }
            new StatementBlockContext Statements { get; set; }
        }

        [FlagsAttribute]
        enum EntityFlags : int
        {
            None = 0,
            ClipperCallingConvention = 1 << 0, // Member property
            MissingReturnType = 1 << 1, // Member property
            UsesPSZ = 1 << 2,           // Member property
            MustBeUnsafe = 1 << 3,      // Member property
            HasTypedParameter = 1 << 4, // Member property
            // 5
            HasParametersStmt = 1 << 6, // Member property
            MustBeVoid = 1 << 7,        // Member property
            IsInitAxit = 1 << 8,        // Member property
            HasInstanceCtor = 1 << 9,   // Class property
            Partial = 1 << 10,          // Class property
            HasStatic = 1 << 10,        // XPP Class property
            PartialProps = 1 << 11,     // Class property
            HasDimVar = 1 << 12,        // Member property
            HasSync = 1 << 13,          // Member property
			HasAddressOf = 1 << 14,     // Member property
            IsInitProcedure = 1 << 15,  // Member property
            HasMemVars = 1 << 16,       // Member property
            HasYield = 1 << 17,         // Member property
        }


        public class EntityData
        {
            EntityFlags setFlag(EntityFlags oldFlag, EntityFlags newFlag, bool set)
            {
                if (set)
                    oldFlag |= newFlag;
                else
                    oldFlag &= ~newFlag;
                return oldFlag;
            }


            EntityFlags flags;

            public bool HasClipperCallingConvention
            {
                get { return flags.HasFlag(EntityFlags.ClipperCallingConvention); }
                set { flags = setFlag(flags, EntityFlags.ClipperCallingConvention, value); }
            }

            public bool HasParametersStmt
            {
                get { return flags.HasFlag(EntityFlags.HasParametersStmt); }
                set { flags = setFlag(flags, EntityFlags.HasParametersStmt, value); }
            }

            public bool HasMissingReturnType
            {
                get { return flags.HasFlag(EntityFlags.MissingReturnType); }
                set { flags = setFlag(flags, EntityFlags.MissingReturnType, value); }
            }
            public bool HasTypedParameter
            {
                get { return flags.HasFlag(EntityFlags.HasTypedParameter); }
                set { flags = setFlag(flags, EntityFlags.HasTypedParameter, value); }
            }
            public bool UsesPSZ
            {
                get { return flags.HasFlag(EntityFlags.UsesPSZ); }
                set { flags = setFlag(flags, EntityFlags.UsesPSZ, value); }
            }
            public bool MustBeUnsafe
            {
                get { return flags.HasFlag(EntityFlags.MustBeUnsafe); }
                set { flags = setFlag(flags, EntityFlags.MustBeUnsafe, value); }
            }

            public bool MustBeVoid            // Assign, SET, Event Accessor
            {
                get { return flags.HasFlag(EntityFlags.MustBeVoid); }
                set { flags = setFlag(flags, EntityFlags.MustBeVoid, value); }
            }
            public bool IsInitAxit            // init or axit with /vo1
            {
                get { return flags.HasFlag(EntityFlags.IsInitAxit); }
                set { flags = setFlag(flags, EntityFlags.IsInitAxit, value); }
            }

            public bool HasInstanceCtor
            {
                get { return flags.HasFlag(EntityFlags.HasInstanceCtor); }
                set { flags = setFlag(flags, EntityFlags.HasInstanceCtor, value); }
            }
            public bool Partial
            {
                get { return flags.HasFlag(EntityFlags.Partial); }
                set { flags = setFlag(flags, EntityFlags.Partial, value); }
            }
            public bool PartialProps
            {
                get { return flags.HasFlag(EntityFlags.PartialProps); }
                set { flags = setFlag(flags, EntityFlags.PartialProps, value); }
            }
            public bool HasDimVar
            {
                get { return flags.HasFlag(EntityFlags.HasDimVar); }
                set { flags = setFlag(flags, EntityFlags.HasDimVar, value); }
            }
            public bool HasSync
            {
                get { return flags.HasFlag(EntityFlags.HasSync); }
                set { flags = setFlag(flags, EntityFlags.HasSync, value); }
            }
            public bool HasAddressOf
            {
                get { return flags.HasFlag(EntityFlags.HasAddressOf); }
                set { flags = setFlag(flags, EntityFlags.HasAddressOf, value); }
            }
            public bool HasStatic
            {
                get { return flags.HasFlag(EntityFlags.HasStatic); }
                set { flags = setFlag(flags, EntityFlags.HasStatic, value); }
            }
            public bool HasMemVars
            {
                get { return flags.HasFlag(EntityFlags.HasMemVars); }
                set { flags = setFlag(flags, EntityFlags.HasMemVars, value); }
            }

            public bool HasYield
            {
                get { return flags.HasFlag(EntityFlags.HasYield); }
                set { flags = setFlag(flags, EntityFlags.HasYield, value); }
            }

            public bool IsInitProcedure
            {
                get { return flags.HasFlag(EntityFlags.IsInitProcedure); }
                set { flags = setFlag(flags, EntityFlags.IsInitProcedure, value); }
            }
            private List<MemVarFieldInfo> Fields = null;
            internal void AddField(string Name, string Alias, bool Field)
            {
                if (Fields == null)
                { 
                    Fields = new List<MemVarFieldInfo>();
                }
                Fields.Add(new MemVarFieldInfo(Name, Alias, Field));
            }
            internal MemVarFieldInfo GetField(string Name)
            {
                if (Fields != null)
                {
                    foreach (var field in Fields)
                    {
                        if (string.Compare(Name, field.Name, StringComparison.OrdinalIgnoreCase) == 0)
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

        public partial class ProcedureContext : IEntityWithBodyContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
            public StatementBlockContext Statements => StmtBlk;
        }

        public partial class FunctionContext : IEntityWithBodyContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
            public StatementBlockContext Statements => StmtBlk;

        }

        public partial class MethodContext : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String ShortName => this.Id.GetText();
            public String Name
            {
                get
                {
                    string name = this.Id.GetText();
                    if (this.T.Token.Type == XSharpParser.ACCESS)
                        name += ":Access";
                    else if (this.T.Token.Type == XSharpParser.ASSIGN)
                        name += ":Assign";
                    else
                        name += "()";
                    return ParentName + name;
                }
            }
            public StatementBlockContext Statements => StmtBlk;
        }

        public partial class EventAccessorContext : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
            public StatementBlockContext Statements => StmtBlk;

        }

        public partial class PropertyAccessorContext : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
            public StatementBlockContext Statements => StmtBlk;
        }

        public partial class ConstructorContext : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "ctor";
            public StatementBlockContext Statements => StmtBlk;
        }
        public partial class DestructorContext : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "Finalize";
            public StatementBlockContext Statements => StmtBlk;
        }
        public partial class Event_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class VodefineContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }
        public partial class PropertyContext : IEntityContext
        {
            EntityData data = new EntityData();

            public EntityData Data { get { return data; } }
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class Operator_Context : IEntityWithBodyContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
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
            public StatementBlockContext Statements => StmtBlk;
        }
        public partial class Delegate_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
        }
        public partial class Interface_Context : IPartialPropertyContext, IEntityContext
        {
            EntityData data = new EntityData();
            List<MethodContext> partialProperties = null;
            public  List<MethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            } 
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();

        }
        public partial class Class_Context : IPartialPropertyContext, IEntityContext
        {
            EntityData data = new EntityData();
            List<MethodContext> partialProperties = null;
            public List<MethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class Structure_Context : IPartialPropertyContext, IEntityContext
        {
            EntityData data = new EntityData();
            List<MethodContext> partialProperties = null;
            public List<MethodContext> PartialProperties
            {
                get { return partialProperties; }
                set { partialProperties = value; }
            }
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class VodllContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }

        public partial class VoglobalContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Vars.DataType;
            public String Name => this.Vars._Var.FirstOrDefault().Id.GetText();
            public String ShortName => this.Vars._Var.FirstOrDefault().Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }
        public partial class FuncprocModifiersContext
        {
            public bool IsStaticVisible { get; set; }
        }

        public partial class VounionContext : IEntityContext 
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();

        }
        public partial class VostructContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();

        }
        public partial class XppclassContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class XppclassvarsContext
        {
            public int Visibility { get; set; }
        }

        public partial class XppmethodContext : IXPPEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String ShortName => this.Id.GetText();
            public String Name
            {
                get
                {
                    string name = this.Id.GetText() +"()";
                    return ParentName + name;
                }
            }
            public InternalSyntax.XppDeclaredMethodInfo Info { get; set; }
            public XppmemberModifiersContext Mods => this.Modifiers;
            public AttributesContext Atts => this.Attributes;
            public StatementBlockContext Statements { get { return this.StmtBlk; } set { this.StmtBlk = value; } }
            public ParameterListContext Parameters => this.ParamList;

        }
        public partial class XppinlineMethodContext : IXPPEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => this.ParamList;
            public DatatypeContext ReturnType => this.Type;
            public String ShortName => this.Id.GetText();
            public String Name
            {
                get
                {
                    string name = this.Id.GetText() + "()";
                    return ParentName + name;
                }
            }
            public InternalSyntax.XppDeclaredMethodInfo Info { get; set; }
            public XppmemberModifiersContext Mods => this.Modifiers;
            public AttributesContext Atts => this.Attributes;
            public StatementBlockContext Statements { get { return this.StmtBlk; } set { this.StmtBlk = value; } }
            public ParameterListContext Parameters => this.ParamList;

        }
        public partial class XpppropertyContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public ParameterListContext Params => null;
            public DatatypeContext ReturnType => this.Type;
            public String ShortName => this.Id.GetText();
            public String Name
            {
                get
                {
                    string name = this.Id.GetText() ;
                    return ParentName + name;
                }
            }
        }
#endif
    }


    internal class ParseErrorData
    {
        internal readonly IXParseTree Node;
        internal readonly ErrorCode Code;
        internal readonly object[] Args;
        internal ParseErrorData(IErrorNode enode, ErrorCode code, params object[] args) :
            this(token: enode.Symbol, code: code, args: args)
        {
        }
        internal ParseErrorData(ErrorCode code) :
            this(node: null, code: code, args: Array.Empty<object>())
        { }
        internal ParseErrorData(ErrorCode code, params object[] args) :
            this(node: null, code: code, args: args)
        { }
        internal ParseErrorData(IXParseTree node, ErrorCode code) :
            this(node, code, Array.Empty<object>())
        { }
        internal ParseErrorData(IXParseTree node, ErrorCode code, params object[] args)
        {
            this.Node = node;
            this.Code = code;
            this.Args = args;
        }
        internal ParseErrorData(IToken token, ErrorCode code, params object[] args)
        {
            if (token == null)
                token = new XSharpToken(0, "");
            else if (!(token is XSharpToken))
            {
                token = new XSharpToken(token);
            }
            this.Node = new XTerminalNodeImpl(token);
            this.Code = code;
            this.Args = args;
        }
        internal ParseErrorData(ITerminalNode tnode, ErrorCode code, params object[] args)
        {
            this.Node = new XTerminalNodeImpl(tnode.Symbol);
            this.Code = code;
            this.Args = args;
        }

        /*protected static SyntaxDiagnosticInfo MakeError(CSharpSyntaxNode node, ErrorCode code, params object[] args)
        {
            return new SyntaxDiagnosticInfo(node.GetLeadingTriviaWidth(), node.Width, code, args);
        }*/
        internal static List<ParseErrorData> NewBag()
        {
            return new List<ParseErrorData>();
        }
    }

    public interface IXParseTree : IParseTree
    {
        object CsNode { get; set; }
        bool IsHidden { get; }
        int Position { get; }
        int FullWidth { get; }
        string SourceFileName { get; }
        string MappedFileName { get; }
        int MappedLine { get; }
        IToken SourceSymbol { get; }
#if !VSPARSER
        Microsoft.CodeAnalysis.Location GetLocation();
#endif
    }
    [Serializable]
    public class XTerminalNodeImpl : Antlr4.Runtime.Tree.TerminalNodeImpl,
        IFormattable,
        IXParseTree,
        IErrorNode
    {
        public XTerminalNodeImpl(IToken symbol) : base(symbol)
        { }
        public object CsNode { get; set; }
        public bool IsHidden { get { return false; } }
        public int Position { get { return Symbol.StartIndex; } }
        public int FullWidth { get { return Symbol.StopIndex - Symbol.StartIndex + 1; } }
        public string ToString(string format, IFormatProvider formatProvider)
        {
            return ToString();
        }
        public string SourceFileName
        {
            get
            {
                var ct = (Symbol as XSharpToken);
                if (ct != null)
                {
                    if (ct.TokenSource != null && !String.IsNullOrEmpty(ct.TokenSource.SourceName))
                        return ct.TokenSource.SourceName;
                    return ct.SourceName;
                }
                return "<unknown>";
            }
        }
        public string MappedFileName { get { return ((XSharpToken)Symbol).MappedFileName; } }
        public int MappedLine { get { return ((XSharpToken)Symbol).MappedLine; } }
        public IToken SourceSymbol
        {
            get
            {
                return ((XSharpToken)Symbol).SourceSymbol;
            }
        }
        public override string ToString() { return this.GetText(); }
#if !VSPARSER
        public Microsoft.CodeAnalysis.Location GetLocation()
        {
            var token = this.Symbol;
            var ts = new MCT.TextSpan(token.StartIndex, this.FullWidth);
            var lp1 = new MCT.LinePosition(token.Line - 1, token.Column);
            var lp2 = new MCT.LinePosition(token.Line - 1, token.Column + this.FullWidth - 1);
            // prevent error  at EOF
            if (lp2 < lp1)
            {
                lp2 = lp1;
            }
            var ls = new MCT.LinePositionSpan(lp1, lp2);
            return Microsoft.CodeAnalysis.Location.Create(this.SourceFileName, ts, ls);


        }
#endif
    }

#if !VSPARSER
    public class MemVarFieldInfo
    {
        public string Name { get; private set; }
        public string Alias { get; private set; }
        public bool IsField { get; private set; }
        internal MemVarFieldInfo(string name, string alias, bool field)
        {
            Name = name;
            Alias = alias;
            IsField = field;
        }
    }


    internal static class RuleExtensions
    {

        internal static bool IsStatic(this InternalSyntax.ClassDeclarationSyntax classdecl)
        {
            return classdecl.Modifiers.Any((int)SyntaxKind.StaticKeyword);
        }

        internal static bool IsStatic(this InternalSyntax.ConstructorDeclarationSyntax ctordecl)
        {
            return ctordecl.Modifiers.Any((int)SyntaxKind.StaticKeyword);
        }

        internal static void Put<T>([NotNull] this IXParseTree t, T node)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (node != null)
            {
                node.XNode = t;
                t.CsNode = node;
            }
        }

        internal static T Get<T>([NotNull] this IXParseTree t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null || t.CsNode == null)
                return default(T);

            return (T)t.CsNode;
        }

        internal static void PutList<T>([NotNull] this IXParseTree t, CoreInternalSyntax.SyntaxList<T> node)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            //node.XNode = t;
            t.CsNode = node;
        }

        internal static CoreInternalSyntax.SyntaxList<T> GetList<T>([NotNull] this IXParseTree t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t.CsNode == null)
                return default(CoreInternalSyntax.SyntaxList<T>);

            return (CoreInternalSyntax.SyntaxList<T>)t.CsNode;
        }

        internal static TNode WithAdditionalDiagnostics<TNode>([NotNull] this TNode node, params DiagnosticInfo[] diagnostics)
            where TNode : InternalSyntax.CSharpSyntaxNode
        {
            DiagnosticInfo[] existingDiags = node.GetDiagnostics();
            int existingLength = existingDiags.Length;
            if (existingLength == 0)
            {
                return node.WithDiagnosticsGreen(diagnostics);
            }
            else
            {
                DiagnosticInfo[] result = new DiagnosticInfo[existingDiags.Length + diagnostics.Length];
                existingDiags.CopyTo(result, 0);
                diagnostics.CopyTo(result, existingLength);
                return node.WithDiagnosticsGreen(result);
            }
        }

        internal static bool isInInterface([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            return ((parent is XSharpParser.ClassmemberContext) && (parent.Parent is XSharpParser.Interface_Context))
                || (parent is XSharpParser.Interface_Context);
        }

        internal static bool isInClass([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            return ((parent is XSharpParser.ClassmemberContext) && (parent.Parent is XSharpParser.Class_Context))
                || (parent is XSharpParser.Class_Context);
        }
        internal static bool isInStructure([NotNull] this RuleContext context)
        {
            var parent = context.Parent;
            return ((parent is XSharpParser.ClassmemberContext) && (parent.Parent is XSharpParser.Structure_Context))
                || (parent is XSharpParser.Structure_Context);
        }

        internal static bool IsRealCodeBlock([NotNull] this IXParseTree context )
        {

            if (context is XSharpParser.ArrayElementContext)
                return ((XSharpParser.ArrayElementContext)context).Expr.IsRealCodeBlock();
            if (context is XSharpParser.PrimaryExpressionContext)
                return ((XSharpParser.PrimaryExpressionContext)context).Expr.IsRealCodeBlock();
            if (context is XSharpParser.CodeblockExpressionContext)
                return ((XSharpParser.CodeblockExpressionContext)context).CbExpr.IsRealCodeBlock();
            if (context is XSharpParser.AliasedExprContext)
                return true;
            if (context is XSharpParser.AliasedFieldContext)
                return true;
            if (context is XSharpParser.CodeblockCodeContext)
                return ((IXParseTree) context.Parent).IsRealCodeBlock();
            if (context is XSharpParser.CodeblockContext)
            {
                var cbc = context as XSharpParser.CodeblockContext;
                if (cbc.lambda != null)
                    return false;
                // when no => operator and no explicit parameters
                // then this is a true codeblock
                return cbc.LambdaParamList == null || cbc.LambdaParamList.ImplicitParams != null;
            }
            return false;
        }
    }
#endif
}
