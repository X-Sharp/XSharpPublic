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
using Roslyn.Utilities;
using Microsoft.CodeAnalysis;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public partial class XSharpParser 
    {
        bool _ClsFunc = true;
        public bool AllowFunctionInsideClass
        {
            get { return _ClsFunc; }
            set { _ClsFunc = value; }
        }
        bool _xBaseVars = false;
        public bool AllowXBaseVariables
        {
            get { return _xBaseVars; }
            set { _xBaseVars = value; }
        }
        bool _namedArgs = false;
        public bool AllowNamedArgs
        {
            get { return _namedArgs; }
            set { _namedArgs = value; }
        }
        bool _allowGarbage;
        public bool AllowGarbageAfterEnd
        {
            get { return _allowGarbage; }
            set { _allowGarbage = value; }
        }

        public interface IGlobalEntityContext : IEntityContext
        {
            FuncprocModifiersContext FuncProcModifiers { get; }
        }
        public interface ILoopStmtContext
        {
            StatementBlockContext Statements { get; }
        }
        public interface IEntityContext : IRuleNode, IXParseTree
        {
            EntityData Data { get; }
            IList<ParameterContext> Params { get; }
            DatatypeContext ReturnType { get; }
            String Name { get; }
            String ShortName { get; }
        }
        [FlagsAttribute]
        enum EntityFlags : short
        {
            None = 0,
            ClipperCallingConvention = 1 << 0, // Member property
            MissingReturnType = 1 << 1, // Member property
            UsesPSZ = 1 << 2,           // Member property
            MustBeUnsafe = 1 << 3,      // Member property
            HasTypedParameter = 1 << 4, // Member property
            UsesPCount = 1 << 5,        // Member property
            UsesGetMParam = 1 << 6,     // Member property
            MustBeVoid = 1 << 7,        // Member property
            IsInitAxit = 1 << 8,        // Member property
            HasCtor = 1 << 9,           // Class property
            Partial = 1 << 10,          // Class property
        }

        public class EntityData
        {
            EntityFlags flags;

            public bool HasClipperCallingConvention
            {
                get { return flags.HasFlag(EntityFlags.ClipperCallingConvention); }
                set { if (value) flags |= EntityFlags.ClipperCallingConvention; else flags &= ~EntityFlags.ClipperCallingConvention; }
            }

            public bool HasMissingReturnType
            {
                get { return flags.HasFlag(EntityFlags.MissingReturnType); }
                set { if (value) flags |= EntityFlags.MissingReturnType; else flags &= ~EntityFlags.MissingReturnType; }
            }
            public bool HasTypedParameter
            {
                get { return flags.HasFlag(EntityFlags.HasTypedParameter); }
                set { if (value) flags |= EntityFlags.HasTypedParameter; else flags &= ~EntityFlags.HasTypedParameter; }
            }
            public bool UsesPSZ
            {
                get { return flags.HasFlag(EntityFlags.UsesPSZ); }
                set { if (value) flags |= EntityFlags.UsesPSZ; else flags &= ~EntityFlags.UsesPSZ; }
            }
            public bool MustBeUnsafe
            {
                get { return flags.HasFlag(EntityFlags.MustBeUnsafe); }
                set { if (value) flags |= EntityFlags.MustBeUnsafe; else flags &= ~EntityFlags.MustBeUnsafe; }
            }

            public bool UsesPCount
            {
                get { return flags.HasFlag(EntityFlags.UsesPCount); }
                set { if (value) flags |= EntityFlags.UsesPCount; else flags &= ~EntityFlags.UsesPCount; }
            }
            public bool UsesGetMParam
            {
                get { return flags.HasFlag(EntityFlags.UsesGetMParam); }
                set { if (value) flags |= EntityFlags.UsesGetMParam; else flags &= ~EntityFlags.UsesGetMParam; }
            }

            public bool MustBeVoid            // Assign, SET, Event Accessor
            {
                get { return flags.HasFlag(EntityFlags.MustBeVoid); }
                set { if (value) flags |= EntityFlags.MustBeVoid; else flags &= ~EntityFlags.MustBeVoid; }
            }
            public bool IsInitAxit            // init or axit with /vo1
            {
                get { return flags.HasFlag(EntityFlags.IsInitAxit); }
                set { if (value) flags |= EntityFlags.IsInitAxit; else flags &= ~EntityFlags.IsInitAxit; }
            }

            public bool HasCtor
            {
                get { return flags.HasFlag(EntityFlags.HasCtor); }
                set { if (value) flags |= EntityFlags.HasCtor; else flags &= ~EntityFlags.HasCtor; }
            }
            public bool Partial
            {
                get { return flags.HasFlag(EntityFlags.Partial); }
                set { if (value) flags |= EntityFlags.Partial; else flags &= ~EntityFlags.Partial; }
            }

            private List<MemVarFieldInfo> Fields;
            public void AddField(string Name, string Alias, bool Field)
            {
                if (Fields == null)
                    Fields = new List<MemVarFieldInfo>();
                Fields.Add(new MemVarFieldInfo(Name, Alias, Field));
            }
            public MemVarFieldInfo GetField(string Name)
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

        public partial class ProcedureContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }

        public partial class FunctionContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;

        }

        public partial class MethodContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
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
        }

        public partial class EventAccessorContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
        }

        public partial class PropertyAccessorContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + Key.Text;
            public String ShortName => ParentName + Key.Text;
        }

        public partial class ClsctorContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "ctor";
        }
        public partial class ClsdtorContext : ClassmemberContext, IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => "Finalize";
        }
        public partial class Event_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class VodefineContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;

        }
        public partial class PropertyContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data { get { return data; } }
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class Operator_Context : IEntityContext
        {
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
        public partial class Delegate_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();
        }
        public partial class Interface_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => this.Id.GetText();

        }
        public partial class Class_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class Structure_Context : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => ParentName + ShortName;
            public String ShortName => Id.GetText();
        }
        public partial class VodllContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => this.ParamList?._Params;
            public DatatypeContext ReturnType => this.Type;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();
            public FuncprocModifiersContext FuncProcModifiers => Modifiers;
        }

        public partial class VoglobalContext : IEntityContext, IGlobalEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
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
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();

        }
        public partial class VostructContext : IEntityContext
        {
            EntityData data = new EntityData();
            public EntityData Data => data;
            public IList<ParameterContext> Params => null;
            public DatatypeContext ReturnType => null;
            public String Name => this.Id.GetText();
            public String ShortName => this.Id.GetText();

        }
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
            this(node: null, code: code, args: SpecializedCollections.EmptyObjects)
        { }
        internal ParseErrorData(ErrorCode code, params object[] args) :
            this(node: null, code: code, args: args)
        { }
        internal ParseErrorData(IXParseTree node, ErrorCode code) :
            this(node, code, SpecializedCollections.EmptyObjects)
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
    }
    [Serializable]
    public class XTerminalNodeImpl : Antlr4.Runtime.Tree.TerminalNodeImpl,
        IMessageSerializable,
        IXParseTree,
        IErrorNode
    {
        public XTerminalNodeImpl(IToken symbol) : base(symbol)
        { }
        public object CsNode { get; set; }
        public bool IsHidden { get { return false; } }
        public int Position { get { return Symbol.StartIndex; } }
        public int FullWidth { get { return Symbol.StopIndex - Symbol.StartIndex + 1; } }
        public string SourceFileName
        {
            get
            {
                var ct = (Symbol as XSharpToken);
                if (ct != null)
                {
                    if (ct.TokenSource != null && !String.IsNullOrEmpty(ct.TokenSource.SourceName))
                        return ct.TokenSource.SourceName;
                    return ct.SourceFileName;
                }
                return "<unknown>";
            }
        }
        public string MappedFileName { get { return ((XSharpToken)Symbol).MappedFileName; } }
        public int MappedLine { get { return ((XSharpToken)Symbol).MappedLine; } }
        public IToken SourceSymbol { get { return ((XSharpToken)Symbol).SourceSymbol; } }
        public override string ToString() { return this.GetText(); }
    }


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

        internal static void PutList<T>([NotNull] this IXParseTree t, InternalSyntax.SyntaxList<T> node)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            //node.XNode = t;
            t.CsNode = node;
        }

        internal static InternalSyntax.SyntaxList<T> GetList<T>([NotNull] this IXParseTree t)
            where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t.CsNode == null)
                return default(InternalSyntax.SyntaxList<T>);

            return (InternalSyntax.SyntaxList<T>)t.CsNode;
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
            return ((context.Parent is XSharpParser.ClassmemberContext) && (context.Parent.Parent is XSharpParser.Interface_Context))
                || (context.Parent is XSharpParser.Interface_Context);
        }

        internal static bool isInClass([NotNull] this RuleContext context)
        {
            return ((context.Parent is XSharpParser.ClassmemberContext) && (context.Parent.Parent is XSharpParser.Class_Context))
                || (context.Parent is XSharpParser.Class_Context);
        }
    }

}
