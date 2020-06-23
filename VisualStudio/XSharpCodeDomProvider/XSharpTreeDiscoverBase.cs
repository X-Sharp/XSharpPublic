//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.SyntaxTree.Tree;
using System.CodeDom;
using System.Reflection;
using Microsoft.VisualStudio.Shell.Design.Serialization.CodeDom;
using System.Diagnostics;

using XSharpModel;
namespace XSharp.CodeDom
{
    internal class XSharpBaseDiscover : XSharpBaseListener
    {

        protected IProjectTypeHelper _projectNode;
        private Dictionary<string, IXType> _types;    // type cache
        private List<string> _usings;          // uses for type lookup
        protected IList<IToken> _tokens;          // used to find comments

        internal Dictionary<ParserRuleContext, List<XCodeMemberField>> FieldList { get; set; }
        internal string SourceCode { get; set; }
        internal string CurrentFile { get; set; }
        const string SnippetsTxt = @"D:\Snippets.txt";
        protected IDictionary<string, IXType> _locals;          // used to keep track of local vars

        protected CodeExpression BuildSnippetExpression(String txt)
        {
            if (!System.IO.File.Exists(SnippetsTxt))
                System.IO.File.WriteAllText(SnippetsTxt, "");
            string old = System.IO.File.ReadAllText(SnippetsTxt);
            old += "\n" + txt;
            System.IO.File.WriteAllText(SnippetsTxt, old);
            return new CodeSnippetExpression(txt);
        }

        internal XSharpBaseDiscover(IProjectTypeHelper projectNode) : base()
        {
            FieldList = new Dictionary<ParserRuleContext, List<XCodeMemberField>>();
            _projectNode = projectNode;
            this._types = new Dictionary<string, IXType>(StringComparer.OrdinalIgnoreCase);
            this._usings = new List<string>();
            this._locals = new Dictionary<string, IXType>(StringComparer.OrdinalIgnoreCase);
            this._members = new Dictionary<string, XMemberType>(StringComparer.OrdinalIgnoreCase);

        }

        #region Members Cache
        protected Dictionary<string, XMemberType> _members;  // member cache for our members and parent class members
        protected void addClassMember(XMemberType mtype)
        {
            if (!_members.ContainsKey(mtype.Name)) // overloads ?
            {
                this._members.Add(mtype.Name, mtype);
            }
        }
        protected void ClearMembers()
        {
            _members.Clear();
        }

        private bool hasClassMember(IXType type, string name, MemberTypes mtype)
        {
            if (_members.ContainsKey(name))
            {
                // no need to include the type itself in the cache. This cache only
                // has fields and properties for the current window/control in the designer
                return (_members[name].MemberType | mtype) != 0;
            }
            if (type == null)
                return false;
            bool result = false;

            IXMember element = type.GetMembers(name, true).FirstOrDefault();
            if (element != null)
            {
                IXType t = null;
                var tm = element as IXMember;
                t = findType(tm.OriginalTypeName);
                var typeName = element.OriginalTypeName;
                switch (element.Kind)
                {
                    case Kind.Field:
                        result = (mtype | MemberTypes.Field) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Field, true, t, typeName));
                        break;
                    case Kind.Property:
                    case Kind.Access:
                    case Kind.Assign:
                        result = (mtype | MemberTypes.Property) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Property, true, t, typeName));
                        break;
                    case Kind.Method:
                        result = (mtype | MemberTypes.Method) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Method, true, t, typeName));
                        break;
                    case Kind.Event:
                        result = (mtype | MemberTypes.Event) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Event, true, t, typeName));
                        break;
                    case Kind.Constructor:
                        result = (mtype | MemberTypes.Constructor) != 0;
                        addClassMember(new XMemberType(name, MemberTypes.Constructor, true, t, typeName));
                        break;
                }
            }
            else
            {
                // not in our class, maybe in a parent class
                var parentType = findType(type.BaseType);
                return hasClassMember(parentType, name, mtype);
            }
            return result;
        }



        protected IXType getClassMemberType(string name, MemberTypes memberType)
        {
            if (_members.ContainsKey(name))
            {
                var mem = _members[name];
                if (mem.MemberType == memberType)
                    return mem.Type;
            }
            return null;
        }
        protected bool isField(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Field)
            {
                return true;
            }

            return findMemberInBaseTypes(name, MemberTypes.Field);
        }
        protected bool isProperty(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Property)
            {
                return true;
            }
            return findMemberInBaseTypes(name, MemberTypes.Property);
        }
        protected bool isMethod(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Method)
            {
                return true;
            }
            return findMemberInBaseTypes(name, MemberTypes.Method);
        }
        private bool isEvent(string name)
        {
            if (_members.ContainsKey(name) && _members[name].MemberType == MemberTypes.Event)
            {
                return true;
            }
            return findMemberInBaseTypes(name, MemberTypes.Event);
        }

        private bool isLocal(string name)
        {
            return _locals.ContainsKey(name);
        }


        #endregion

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            var source = (XSharpLexer)context.Start.TokenSource;
            source.Reset();
            _tokens = source.GetAllTokens();
        }


        #region Helpers

        protected XCodeTypeReference BuildDataType(XSharpParser.DatatypeContext context)
        {
            //
            // Datatype is ptrDatatype
            // or arrayDatatype
            // or simpleDatatype
            // or nullableDatatype
            // they all have a TypeName
            XSharpParser.TypeNameContext tn = null;
            if (context == null)
            {
                return new XCodeTypeReference("USUAL");
            }
            var sName = context.GetText();
            if (context is XSharpParser.PtrDatatypeContext)
            {
                XSharpParser.PtrDatatypeContext ptrData = (XSharpParser.PtrDatatypeContext)context;
                tn = ptrData.TypeName;
            }
            else if (context is XSharpParser.ArrayDatatypeContext)
            {
                XSharpParser.ArrayDatatypeContext aData = (XSharpParser.ArrayDatatypeContext)context;
                tn = aData.TypeName;
            }
            else if (context is XSharpParser.SimpleDatatypeContext)
            {
                XSharpParser.SimpleDatatypeContext sdt = (XSharpParser.SimpleDatatypeContext)context;
                tn = sdt.TypeName;
            }
            else if (context is XSharpParser.NullableDatatypeContext)
            {
                XSharpParser.NullableDatatypeContext ndc = context as XSharpParser.NullableDatatypeContext;
                tn = ndc.TypeName;
            }
            XCodeTypeReference expr = null;
            if (tn != null)
            {
                if (tn.NativeType != null)
                {
                    expr = BuildNativeType(tn.NativeType);
                }
                else if (tn.XType != null)
                {
                    expr = BuildXBaseType(tn.XType);
                }
                else if (tn.Name != null)
                {
                    expr = BuildName(tn.Name);
                }
            }
            else
            {
                expr = new XCodeTypeReference("System.Void");
            }

            if (sName.Contains("[") || sName.Contains(">"))
            {
                // work around to fix type problems with generics and arrays
                expr.UserData[XSharpCodeConstants.USERDATA_CODE] = sName;
            }
            return expr;
        }

        protected CodeExpression BuildExpression(XSharpParser.ExpressionContext expression, bool right)
        {
            CodeExpression expr = null;
            var source = expression.GetText();
            //
            if (expression is XSharpParser.PrimaryExpressionContext) // xyz.SimpleName
            {
                expr = BuildPrimaryExpression((XSharpParser.PrimaryExpressionContext)expression);
            }
            else if (expression is XSharpParser.AccessMemberContext) // xyz.SimpleName
            {
                expr = BuildAccessMember(expression as XSharpParser.AccessMemberContext, right);
            }
            else if (expression is XSharpParser.MethodCallContext)
            {
                expr = BuildMethodCall(expression as XSharpParser.MethodCallContext);
            }
            else if (expression is XSharpParser.TypeCastContext)
            {
                var tc = expression as XSharpParser.TypeCastContext;
                var name = tc.Type.GetCleanText();
                var typeref = BuildTypeReference(name);
                expr = BuildExpression(tc.Expr, true);
                expr = new CodeCastExpression(typeref, expr);
            }
            else if (expression is XSharpParser.BinaryExpressionContext)
            {
                expr = BuildBinaryExpression(expression as XSharpParser.BinaryExpressionContext);
            }
            else if (expression is XSharpParser.PrefixExpressionContext)
            {
                var pref = expression as XSharpParser.PrefixExpressionContext;
                if (pref.PLUS() != null)
                {
                    expr = BuildExpression(pref.Expr, true);
                }
                else if (pref.MINUS() != null)
                {
                    expr = BuildExpression(pref.Expr, true);
                }
                else
                {
                    expr = BuildSnippetExpression(expression.GetText());
                }
            }
            // Unhandled expression types
            // ArrayAccessExpressionContext
            // CondAccessExpressionContext
            // PostFixExpressionContext
            // AwaitExpressionContext
            // PrefixExpressionContext
            // TypeCheckExpressionContext
            // AssignmentExpressionContext: Handled separately because CodeDom wants a Assign Statement. Check CreateAssignStatement below
            else
            {
                expr = BuildSnippetExpression(expression.GetText());
            }
            //
            if (expr != null)
            {
                expr.UserData[XSharpCodeConstants.USERDATA_CODE] = source;
            }
            return expr;
        }

        protected CodeStatement CreateAssignStatement(XSharpParser.AssignmentExpressionContext exp)
        {
            // Can be normal assign but also event handler assign
            CodeStatement stmt = null;
            //
            //what is the left hand side ?
            //    Self  -> check if Right is in the member of CurrentClass --> FieldReference
            // else --> always Property
            //
            CodeExpression left = BuildExpression(exp.Left, false);
            CodeExpression right = BuildExpression(exp.Right, true);
            CodeEventReferenceExpression cere = null;
            switch (exp.Op.Type)
            {
                case XSharpLexer.ASSIGN_OP:
                    stmt = new CodeAssignStatement(left, right);
                    break;
                case XSharpLexer.ASSIGN_ADD:
                    // += Event Handler
                    // We will decode Left as CodeFieldReferenceExpression or CodePropertyReferenceExpression, but we need a CodeEventReferenceExpression
                    if (left is CodeFieldReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeAttachEventStatement(cere, right);
                    break;
                case XSharpLexer.ASSIGN_SUB:
                    // -= Event Handler
                    if (left is CodeFieldReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodeFieldReferenceExpression)left).TargetObject, ((CodeFieldReferenceExpression)left).FieldName);
                    else if (left is CodePropertyReferenceExpression)
                        cere = new XCodeEventReferenceExpression(((CodePropertyReferenceExpression)left).TargetObject, ((CodePropertyReferenceExpression)left).PropertyName);
                    else if (left is CodeEventReferenceExpression)
                        cere = (CodeEventReferenceExpression)left;
                    if (cere != null)
                        stmt = new CodeRemoveEventStatement(cere, right);
                    break;
                default:
                    var snip = BuildSnippetExpression(exp.GetText());
                    stmt = new CodeExpressionStatement(snip);
                    break;
            }
            return stmt;
        }
        private CodeExpression BuildBinaryExpression(XSharpParser.BinaryExpressionContext be)
        {
            var l = BuildExpression(be.Left, false);
            var r = BuildExpression(be.Right, true);
            var op = CodeBinaryOperatorType.Add;
            switch (be.Op.Type)
            {
                case XSharpParser.EXP:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // MULT|DIV|MOD
                case XSharpParser.MULT:
                    op = CodeBinaryOperatorType.Multiply;
                    break;
                case XSharpParser.DIV:
                    op = CodeBinaryOperatorType.Divide;
                    break;
                case XSharpParser.MOD:
                    op = CodeBinaryOperatorType.Modulus;
                    break;
                // PLUS|MINUS
                case XSharpParser.PLUS:
                    op = CodeBinaryOperatorType.Add;
                    break;
                case XSharpParser.MINUS:
                    op = CodeBinaryOperatorType.Subtract;
                    break;
                // LSHIFT
                // RSHIFT
                case XSharpParser.LSHIFT:
                case XSharpParser.RSHIFT:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // LT | LTE | GT | GTE | EQ | EEQ | SUBSTR | NEQ | NEQ2
                case XSharpParser.LT:
                    op = CodeBinaryOperatorType.LessThan;
                    break;
                case XSharpParser.LTE:
                    op = CodeBinaryOperatorType.LessThanOrEqual;
                    break;
                case XSharpParser.GT:
                    op = CodeBinaryOperatorType.GreaterThan;
                    break;
                case XSharpParser.GTE:
                    op = CodeBinaryOperatorType.GreaterThanOrEqual;
                    break;
                case XSharpParser.EQ:
                case XSharpParser.EEQ:
                    op = CodeBinaryOperatorType.IdentityEquality;
                    break;
                case XSharpParser.NEQ:
                case XSharpParser.NEQ2:
                    op = CodeBinaryOperatorType.IdentityInequality;
                    break;
                case XSharpParser.SUBSTR:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // AMP
                case XSharpParser.AMP:
                    op = CodeBinaryOperatorType.BitwiseAnd;
                    break;
                // TILDE
                case XSharpParser.TILDE:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
                // PIPE
                case XSharpParser.PIPE:
                    op = CodeBinaryOperatorType.BitwiseOr;
                    break;
                // LOGIC_AND | AND
                case XSharpParser.LOGIC_AND:
                case XSharpParser.AND:
                    op = CodeBinaryOperatorType.BooleanAnd;
                    break;
                // LOGIC_OR | OR
                case XSharpParser.LOGIC_OR:
                case XSharpParser.OR:
                    op = CodeBinaryOperatorType.BooleanOr;
                    break;
                // DEFAULT
                case XSharpParser.DEFAULT:
                    // cannot be expressed in codedom
                    return BuildSnippetExpression(be.GetText());
            }
            return new CodeBinaryOperatorExpression(l, op, r);
        }


        private CodeExpression BuildAccessMember(XSharpParser.AccessMemberContext amc, bool right)
        {
            var elements = new List<XSharpParser.AccessMemberContext>();
            string typeName;
            IXType xtype = null;
            // if the top level element has a Dot it may be a type of a field of a type.
            if (amc.Op.Type == XSharpParser.DOT)
            {
                // aa.bb.cc.dd
                typeName = amc.Expr.GetCleanText();
                var memberName = amc.Name.GetCleanText();
                xtype = findType(typeName);
                if (xtype != null)
                {

                    var result = buildTypeMemberExpression(xtype, memberName);
                    if (result != null)
                        return result;
                }
                typeName = amc.GetCleanText();
                xtype = findType(typeName);
                if (xtype != null)
                {
                    return new XCodeTypeReferenceExpression(typeName);
                }
            }

            // Build a list of the elements from Left to Right so we can easily find without recursion
            // are AccessMember
            // LHS of first element is usually:
            // - self
            // - super
            // - cast with params
            // - local variable
            // - what else ?
            var element = amc;
            while (true)
            {
                elements.Insert(0, element);
                if (element.Expr is XSharpParser.AccessMemberContext)
                {
                    element = (XSharpParser.AccessMemberContext)element.Expr;
                }
                else
                {
                    break;
                }
            }
            // Determine the type of the first expression

            CodeExpression lhs = null;
            bool isSelf = false;
            amc = elements[0];    // Access Member
            xtype = null;
            switch (amc.Expr.Start.Type)
            {
                case XSharpParser.SELF:
                    lhs = new XCodeThisReferenceExpression();
                    isSelf = true;
                    break;
                case XSharpParser.SUPER:
                    lhs = new XCodeBaseReferenceExpression();
                    isSelf = true;
                    break;
                case XSharpParser.LPAREN:
                    var pe = amc.Expr as XSharpParser.PrimaryExpressionContext;
                    var pec = pe.Expr as XSharpParser.ParenExpressionContext;
                    var exp = pec.Expr as XSharpParser.TypeCastContext;
                    xtype = findType(exp.Type.GetCleanText());
                    lhs = BuildExpression(pec.Expr, false);
                    lhs = new CodeCastExpression(xtype.FullName, lhs);
                    break;
                case XSharpParser.ID:
                    var lname = amc.Expr.GetCleanText();
                    if (this.isLocal(lname))
                    {
                        xtype = _locals[lname];
                        lhs = new XCodeVariableReferenceExpression(lname);
                    }
                    break;
                default:
                    // what else ?
                    break;
            }
            // expr should have a value here
            if (lhs != null)
            {
                for (int i = 0; i < elements.Count; i++)
                {
                    amc = elements[i];
                    var rhs = amc.Name.GetCleanText();
                    IXType memberType = null;
                    if (i == 0)
                    {
                        if (isSelf)
                        {
                            lhs = buildSelfExpression(lhs, rhs, out memberType);

                        }
                        else
                        {
                            lhs = buildTypeMemberExpression(lhs, xtype, rhs, out memberType);
                        }
                    }
                    else
                    {
                        lhs = buildTypeMemberExpression(lhs, xtype, rhs, out memberType);
                    }
                    xtype = memberType;
                    if (xtype == null)
                        break;
                }
            }
            else
            {
                lhs = BuildSnippetExpression(amc.GetText());
            }
            return lhs;
        }
        private CodeExpression buildTypeMemberExpression(IXType xtype, string name)
        {
            var l = new XCodeTypeReferenceExpression(xtype.FullName);
            var members = xtype.GetMembers(name, true);
            var m = members.Where(e => (e.Kind == Kind.Field || e.Kind == Kind.EnumMember)).FirstOrDefault();
            if (m != null)
            {
                var fld = new XCodeFieldReferenceExpression(l, name);
                return fld;
            }
            m = members.Where(e => (e.Kind == Kind.Property || e.Kind == Kind.Access || e.Kind == Kind.Assign)).FirstOrDefault();
            if (m != null)
            {
                var prop = new XCodePropertyReferenceExpression(l, name);
                return prop;
            }
            m = members.Where(e => e.Kind == Kind.Method).FirstOrDefault();
            if (m != null)
            {
                var met = new XCodeMethodReferenceExpression(l, name);
                return met;
            }
            m = members.Where(e => e.Kind == Kind.Event).FirstOrDefault();
            if (m != null)
            {
                var evt = new XCodeEventReferenceExpression(l, name);
                return evt;
            }
            return null;
        }


        private CodeExpression buildTypeMemberExpression(CodeExpression target, IXType xtype, string name, out IXType memberType)
        {
            // Special Name ? (Keyword)
            CodeExpression expr = null;
            memberType = null;
            while (xtype != null)
            {
                var mi = xtype.GetMember(name);
                if (mi.Count() == 0)
                {
                    // Member not found !? MayBe in the parent
                    xtype = findParentType(xtype);
                    continue;
                }
                //
                if (mi.Count() > 0)
                {
                    var m = mi[0];
                    memberType = findType(m.OriginalTypeName);
                    switch (m.Kind)
                    {
                        case Kind.Field:
                            expr = new XCodeFieldReferenceExpression(target, name);
                            break;
                        case Kind.Access:
                        case Kind.Assign:
                        case Kind.Property:
                            expr = new XCodePropertyReferenceExpression(target, name);
                            break;
                        case Kind.Method:
                            expr = new XCodeMethodReferenceExpression(target, name);
                            break;
                        case Kind.Event:
                            expr = new XCodeEventReferenceExpression(target, name);
                            break;
                        default:
                            break;
                    }
                }
                break;
            }
            return expr;
        }

        private CodeExpression buildSelfExpression(CodeExpression target, string name, out IXType memberType)
        {
            CodeExpression expr = null;
            memberType = null;
            if (!_members.ContainsKey(name))
            {
                findMemberInBaseTypes(name, MemberTypes.All);
            }
            if (_members.ContainsKey(name))
            {
                var mi = _members[name];
                switch (mi.MemberType)
                {
                    case MemberTypes.Field:
                        expr = new XCodeFieldReferenceExpression(target, name);
                        break;
                    case MemberTypes.Property:
                        expr = new XCodePropertyReferenceExpression(target, name);
                        break;
                    case MemberTypes.Method:
                        expr = new XCodeMethodReferenceExpression(target, name);
                        break;
                    case MemberTypes.Event:
                        expr = new XCodeEventReferenceExpression(target, name);
                        break;
                }
                //
                if (mi.Type != null)
                {
                    memberType = mi.Type;
                }
                else
                {
                    memberType = findType(mi.TypeName);
                }
            }
            return expr;
        }

        public XCodeTypeDeclaration CurrentClass { get; protected set; }


        private bool findMemberInBaseTypes(string name, MemberTypes mtype)
        {
            foreach (XCodeTypeReference basetype in CurrentClass.BaseTypes)
            {
                string typeName = basetype.BaseType;
                var baseType = findType(typeName);
                if (baseType != null)
                {
                    return hasClassMember(baseType, name, mtype);
                }
            }
            return false;
        }

        private CodeExpression BuildPrimaryExpression(XSharpParser.PrimaryExpressionContext expression)
        {
            CodeExpression expr = null;
            XSharpParser.PrimaryContext ctx = expression.Expr;
            //
            if (ctx is XSharpParser.SelfExpressionContext) // Self
            {
                expr = new XCodeThisReferenceExpression();
            }
            else if (ctx is XSharpParser.SuperExpressionContext) // Super
            {
                expr = new XCodeBaseReferenceExpression();
            }
            else if (ctx is XSharpParser.LiteralExpressionContext)
            {
                XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)ctx;
                expr = BuildLiteralValue(lit.Literal);
            }
            else if (ctx is XSharpParser.LiteralArrayExpressionContext) // { expr [, expr] }
            {
                XSharpParser.LiteralArrayContext arr = ((XSharpParser.LiteralArrayExpressionContext)ctx).LiteralArray;
                // Typed Array ?
                if (arr.Type != null)
                {
                    List<CodeExpression> exprlist = new List<CodeExpression>();
                    foreach (var Element in arr._Elements)
                    {
                        exprlist.Add(BuildExpression(Element.Expr, true));
                    }
                    expr = new CodeArrayCreateExpression(BuildDataType(arr.Type), exprlist.ToArray());
                }
                else
                {
                    expr = BuildSnippetExpression(arr.GetText());
                }
            }
            else if (ctx is XSharpParser.DelegateCtorCallContext)
            {
                XSharpParser.DelegateCtorCallContext delg = (XSharpParser.DelegateCtorCallContext)ctx;
                //
                XCodeTypeReference ctr = BuildDataType(delg.Type);
                CodeExpression ce = BuildExpression(delg.Obj, true);
                //
                expr = new CodeDelegateCreateExpression(BuildDataType(delg.Type), BuildExpression(delg.Obj, true), delg.Func.GetText());

            }
            else if (ctx is XSharpParser.CtorCallContext)
            {
                XSharpParser.CtorCallContext ctor = (XSharpParser.CtorCallContext)ctx;
                XCodeTypeReference ctr = BuildDataType(ctor.Type);
                List<CodeExpression> exprlist = new List<CodeExpression>();
                if (ctor.ArgList != null)
                {
                    foreach (var arg in ctor.ArgList._Args)
                    {
                        // We should handle arg.Name if arg.ASSIGN_OP is not null...
                        exprlist.Add(BuildExpression(arg.Expr, true));
                    }
                }
                expr = new CodeObjectCreateExpression(ctr.BaseType, exprlist.ToArray());
            }
            else if (ctx is XSharpParser.TypeOfExpressionContext)
            {
                XCodeTypeReference ctr = BuildDataType(((XSharpParser.TypeOfExpressionContext)ctx).Type);
                expr = new CodeTypeOfExpression(ctr);
            }
            else if (ctx is XSharpParser.NameExpressionContext)
            {
                string name = ((XSharpParser.NameExpressionContext)ctx).Name.Id.GetCleanText();
                // Sometimes, we will need to do it that way....
                if (name.ToLower() == "self")
                {
                    expr = new XCodeThisReferenceExpression();
                }
                else if (name.ToLower() == "super")
                {
                    expr = new XCodeBaseReferenceExpression();
                }
                // Locals preferred over same named fields
                else if (isLocal(name))
                {
                    expr = new XCodeVariableReferenceExpression(name);
                }
                else if (isField(name))
                {
                    expr = new XCodeFieldReferenceExpression(new XCodeThisReferenceExpression(), name);
                }
                else if (isProperty(name))
                {
                    expr = new XCodePropertyReferenceExpression(new XCodeThisReferenceExpression(), name);
                }
                else
                {
                    var tr = BuildTypeReference(((XSharpParser.NameExpressionContext)ctx).Name.Id.GetCleanText());
                    expr = new XCodeTypeReferenceExpression(tr);
                }
            }
            else if (ctx is XSharpParser.ParenExpressionContext)
            {
                var par = ctx as XSharpParser.ParenExpressionContext;
                expr = BuildExpression(par.Expr, true);
            }
            else if (ctx is XSharpParser.TypeExpressionContext)
            {
                var typ = ctx as XSharpParser.TypeExpressionContext;
                var name = typ.Type;
                // NativeType, xBaseType, Name
                if (name.Name is XSharpParser.QualifiedNameContext)
                {
                    var qnc = name.Name as XSharpParser.QualifiedNameContext;
                    var left = qnc.Left;
                    var right = qnc.Right;
                    var tr = new XCodeTypeReferenceExpression(left.GetCleanText());
                    expr = new XCodePropertyReferenceExpression(tr, right.GetCleanText());
                }
                else
                {
                    expr = BuildSnippetExpression(ctx.GetText());
                }
            }
            else
            // Unhandled expression types:
            // ---------------------------
            // AnonTypeExpressionContext
            // CodeBlockExpressionContext
            // QueryExpressionContext
            // CheckedExpressionContext
            // SizeOfExpressionContext
            // DefaultExpressionContext
            // VoConversionExpressionContext
            // VoCastExpressionContext
            // VoCastPtrExpressionContext
            // VoTypeNameExpressionContext
            // IifExpressionContext
            // IntrinsicExpressionContext  _OR(..)
            // AliasedFieldContect
            // AliasedExprContext
            // MacroContext
            // ArgListExpressionContext
            {
                expr = BuildSnippetExpression(ctx.GetText());
            }
            return expr;
        }


        private CodeMethodInvokeExpression BuildMethodCall(XSharpParser.MethodCallContext meth)
        {
            CodeMethodInvokeExpression expr = null;

            // target: for example SELF:Controls:Add
            CodeExpression target = BuildExpression(meth.Expr, false);

            List<CodeExpression> exprlist = new List<CodeExpression>();
            if (meth.ArgList != null)
            {
                foreach (var arg in meth.ArgList._Args)
                {
                    exprlist.Add(BuildExpression(arg.Expr, true));
                }
            }
            // When we cannot find a field then we always map something like
            // SELF:Controls:Add to a propertyReferenceExpression
            // Change it to a method call now
            if (target is CodeMethodReferenceExpression)
            {
                var cmr = target as CodeMethodReferenceExpression;
                var lhs = cmr.TargetObject;
                var methodName = cmr.MethodName;
                expr = new CodeMethodInvokeExpression(lhs, methodName, exprlist.ToArray());
            }
            else if (target is CodePropertyReferenceExpression)
            {
                // method call on a property
                var cpr = target as CodePropertyReferenceExpression;
                var lhs = cpr.TargetObject;
                var methodName = cpr.PropertyName;
                expr = new CodeMethodInvokeExpression(lhs, methodName, exprlist.ToArray());
            }
            // can't be a variable ref: this will have only one element and not two
            // can't be a field ref: cannot have a field with the same name as a method
            else
            {
                // simple method call
                var methodName = meth.Expr.GetCleanText();
                var pos = methodName.LastIndexOf(":");
                if (pos > 0)
                {
                    methodName = methodName.Substring(pos + 1);
                }
                expr = new CodeMethodInvokeExpression(null, methodName, exprlist.ToArray());
            }
            return expr;
        }


        private CodeExpression BuildLiteralValue(XSharpParser.LiteralValueContext context)
        {
            CodeExpression expr = null;
            string value;
            //
            switch (context.Token.Type)
            {
                case XSharpParser.BIN_CONST:
                case XSharpParser.INT_CONST:
                case XSharpParser.HEX_CONST:
                case XSharpParser.REAL_CONST:
                    expr = new CodePrimitiveExpression(GetNumericValue(context));
                    break;
                case XSharpParser.TRUE_CONST:
                    expr = new CodePrimitiveExpression(true);
                    break;
                case XSharpParser.FALSE_CONST:
                    expr = new CodePrimitiveExpression(false);
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.ESCAPED_STRING_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (context.Token.Type == XSharpParser.ESCAPED_STRING_CONST)
                        value = BuildUnEscapedString(value);
                    expr = new CodePrimitiveExpression(value);
                    break;
                case XSharpParser.CHAR_CONST:
                    value = context.GetText();
                    value = value.Substring(1, value.Length - 2);
                    if (value.Length >= 1)
                        expr = new CodePrimitiveExpression(value[0]);
                    break;
                case XSharpParser.NULL:
                    expr = new CodePrimitiveExpression(null);
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                case XSharpParser.SYMBOL_CONST:
                case XSharpParser.DATE_CONST:
                default:
                    expr = BuildSnippetExpression(context.Token.Text);
                    break;
            }
            return expr;
        }
        private XCodeTypeReference BuildName(XSharpParser.NameContext context)
        {
            XCodeTypeReference expr = null;
            //
            var sName = context.GetText();
            //if (!sName.Contains(".") && !sName.Contains(":") && !sName.Contains(">"))
            //{
            //    return BuildTypeReference(sName);
            //}
            if (!sName.EndsWith(">"))
            {
                var type = findType(sName);
                if (type != null)
                {
                   return new XCodeTypeReference(type);
                }
            }
            if (context is XSharpParser.QualifiedNameContext)
            {
                XSharpParser.QualifiedNameContext qual = (XSharpParser.QualifiedNameContext)context;
                expr = BuildName(qual.Left);
                expr = BuildTypeReference(expr.BaseType + "." + BuildSimpleName(qual.Right).BaseType);
            }
            else if (context is XSharpParser.SimpleOrAliasedNameContext)
            {
                var alias = context as XSharpParser.SimpleOrAliasedNameContext;
                var name = alias.Name as XSharpParser.AliasedNameContext;

                //
                if (name is XSharpParser.AliasQualifiedNameContext)
                {
                    XSharpParser.AliasQualifiedNameContext al = (XSharpParser.AliasQualifiedNameContext)name;
                    expr = BuildSimpleName(al.Right);
                    expr = BuildTypeReference(al.Alias.GetText() + "::" + expr.BaseType);
                }
                else if (name is XSharpParser.GlobalQualifiedNameContext)
                {
                    var gqn = name as XSharpParser.GlobalQualifiedNameContext;
                    expr = BuildSimpleName(gqn.Right);
                    expr = BuildTypeReference("global::" + expr.BaseType);
                }
                else if (name is XSharpParser.IdentifierOrGenericNameContext)
                {
                    var id = name as XSharpParser.IdentifierOrGenericNameContext;
                    expr = BuildSimpleName(id.Name);
                }
            }
            return expr;
        }
        protected TypeAttributes ContextToClassModifiers(XSharpParser.ClassModifiersContext modifiers)
        {
            TypeAttributes retValue = TypeAttributes.Public;
            //
            if (modifiers.INTERNAL().Length > 0)
                retValue = TypeAttributes.NestedAssembly;
            //
            if (modifiers.HIDDEN().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PRIVATE().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PROTECTED().Length > 0)
            {
                if (modifiers.INTERNAL().Length > 0)
                    retValue = TypeAttributes.NestedFamORAssem;
                else
                    retValue = TypeAttributes.NestedFamily;
            }
            //
            if (modifiers.EXPORT().Length > 0 || modifiers.PUBLIC().Length > 0)
                retValue = TypeAttributes.Public;
            //
            return retValue;
        }
        protected TypeAttributes ContextToStructureModifiers(XSharpParser.ClassModifiersContext modifiers)
        {
            TypeAttributes retValue = TypeAttributes.Public;
            //
            if (modifiers.INTERNAL().Length > 0)
                retValue = TypeAttributes.NestedAssembly;
            //
            if (modifiers.HIDDEN().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PRIVATE().Length > 0)
                retValue = TypeAttributes.NestedPrivate;
            //
            if (modifiers.PROTECTED().Length > 0)
            {
                if (modifiers.INTERNAL().Length > 0)
                    retValue = TypeAttributes.NestedFamORAssem;
                else
                    retValue = TypeAttributes.NestedFamily;
            }
            //
            if (modifiers.EXPORT().Length > 0 || modifiers.PUBLIC().Length > 0)
                retValue = TypeAttributes.Public;
            //
            return retValue;
        }
        protected MemberAttributes decodeMemberAttributes(IList<IToken> tokens)
        {
            MemberAttributes retValue = MemberAttributes.Public;
            bool bHasProtect = false;
            bool bHasInternal = false;
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case XSharpParser.INTERNAL:
                        bHasInternal = true;
                        if (bHasProtect)
                            retValue = MemberAttributes.FamilyOrAssembly;
                        else
                            retValue = MemberAttributes.Assembly;
                        break;
                    case XSharpParser.PROTECTED:
                        bHasProtect = true;
                        if (bHasInternal)
                            retValue = MemberAttributes.FamilyOrAssembly;
                        else
                            retValue = MemberAttributes.Family;
                        break;
                    case XSharpParser.PRIVATE:
                    case XSharpParser.HIDDEN:
                        retValue = MemberAttributes.Private;
                        break;
                    case XSharpParser.EXPORT:
                    case XSharpParser.PUBLIC:
                        //
                        retValue = MemberAttributes.Public;
                        break;
                }
            }
            return retValue;
        }
        protected MemberAttributes ContextToMethodModifiers(XSharpParser.MemberModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }
        protected MemberAttributes ContextToConstructorModifiers(XSharpParser.ConstructorModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected MemberAttributes ContextToEventModifiers(XSharpParser.MemberModifiersContext modifiers)
        {
            return decodeMemberAttributes(modifiers._Tokens);
        }

        protected bool IsEmpty(CodeNamespace nspace)
        {
            return (nspace.Types.Count == 0);
        }

        protected CodeParameterDeclarationExpressionCollection GetParametersList(XSharpParser.ParameterListContext paramList)
        {
            CodeParameterDeclarationExpressionCollection pList = new CodeParameterDeclarationExpressionCollection();
            //
            if (paramList != null)
            {
                foreach (var param in paramList._Params)
                {
                    CodeParameterDeclarationExpression pm = new CodeParameterDeclarationExpression();
                    pm.Name = param.Id.GetText();
                    pm.Type = BuildDataType(param.Type);
                    pm.Direction = FieldDirection.In;
                    if (param.Modifiers != null)
                    {

                        if (param.Modifiers.REF() != null)
                        {
                            pm.Direction = FieldDirection.Ref;
                        }
                        else if (param.Modifiers.OUT() != null)
                        {
                            pm.Direction = FieldDirection.Out;
                        }
                    }
                    //
                    pList.Add(pm);
                }
            }
            //
            return pList;
        }


        protected void FillCodeDomDesignerData(CodeObject newElement, int line, int col)
        {
            CodeDomDesignerData data = new CodeDomDesignerData();
            //
            data.CaretPosition = new System.Drawing.Point(col, line);
            data.FileName = this.CurrentFile;
            // point is where the designer will try to focus if the
            // user wants to add event handler stuff.
            newElement.UserData[typeof(CodeDomDesignerData)] = data;
            newElement.UserData[typeof(System.Drawing.Point)] = data.CaretPosition;
        }

        protected void FillCodeSource(CodeObject element, ParserRuleContext context, IList<IToken> tokens)
        {
            StringBuilder prototype = new StringBuilder();
            var index = ((XSharpToken) context.Start).OriginalTokenIndex;
            var lastindex = ((XSharpToken)context.Stop).OriginalTokenIndex;
            while (index > 0 && index < tokens.Count)
            {
                prototype.Append(tokens[index].Text);
                if (index == lastindex)
                    break;
                index++;
            }
            element.UserData[XSharpCodeConstants.USERDATA_CODE] = prototype.ToString();
            FillCodeDomDesignerData(element, context.Start.Line, context.Start.Column);
        }

        protected CodeSnippetTypeMember CreateSnippetMember(ParserRuleContext context)
        {
            // The original source code
            var xtoken = context.Start as XSharpToken;
            var start = xtoken.OriginalTokenIndex;

            var startIndex = _tokens[start].StartIndex;
            int length = context.Stop.StopIndex - startIndex + 1;
            string sourceCode = this.SourceCode.Substring(startIndex, length);
            XCodeSnippetTypeMember snippet = new XCodeSnippetTypeMember(sourceCode);
            FillCodeDomDesignerData(snippet, context.Start.Line, context.Start.Column);
            return snippet;
        }

        /// <summary>
        /// Get a LiteralValueContext containing a BIN_CONST, INT_CONST, HEX_CONST, or a REAL_CONST
        /// as a string, and convert it to the "real" value, with the corresponding Type.
        /// </summary>
        /// <param name="context"></param>
        /// <returns>An Object of the needed Type, with the value</returns>
        protected object GetNumericValue(XSharpParser.LiteralValueContext context)
        {
            Object ret = null;
            string value = context.Token.Text;
            var type = context.Token.Type;
            // try to check if this literal has a Prefix
            try
            {
                if ( context.Parent.Parent is XSharpParser.PrimaryExpressionContext )
                {
                    var prim = context.Parent.Parent as XSharpParser.PrimaryExpressionContext;
                    if ( prim.Parent is XSharpParser.PrefixExpressionContext )
                    {
                        var pref = prim.Parent as XSharpParser.PrefixExpressionContext;
                        if( pref.MINUS() != null )
                        {
                            value = "-" + value;
                        }
                    }
                }
            }
            catch
            {
                // eat the troubles and ignore please...
            }
            //
            if (type == XSharpParser.BIN_CONST || type == XSharpParser.INT_CONST || type == XSharpParser.HEX_CONST)
            {
                bool isUnsigned = value.EndsWith("u", StringComparison.OrdinalIgnoreCase);
                bool isSigned = value.EndsWith("l", StringComparison.OrdinalIgnoreCase);
                if (isSigned || isUnsigned)
                {
                    value = value.Substring(0, value.Length - 1);
                }
                // -1 for Unsigned; -2 for 0x or 0b
                int len = value.Length - (context.BIN_CONST() != null || context.HEX_CONST() != null ? 2 : 0);
                //
                if (context.BIN_CONST() != null)
                {
                    if (len > 64)
                    {
                        ret = Double.NaN;
                    }
                    else
                    {
                        // Don't forget to remove the prefix !!!
                        value = value.Substring(2);
                        // BIN are always unsigned (??)
                        UInt64 bin64;
                        try
                        {
                            bin64 = Convert.ToUInt64(value, 2);
                            // Maybe 32 bits is enough ?
                            if (bin64 <= UInt32.MaxValue)
                            {
                                UInt32 bin32 = Convert.ToUInt32(bin64);
                                ret = bin32;
                            }
                            else
                            {
                                ret = bin64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
                else if (type == XSharpParser.HEX_CONST)
                {
                    if (len > 16)
                    {
                        ret = Double.NaN;
                    }
                    else
                    {
                        // Don't forget to remove the prefix !!!
                        value = value.Substring(2);
                        // HEX are always unsigned (??)
                        UInt64 hex64;
                        try
                        {
                            hex64 = Convert.ToUInt64(value, 16);
                            // Maybe 32 bits is enough ?
                            if (hex64 <= UInt32.MaxValue)
                            {
                                UInt32 hex32 = Convert.ToUInt32(hex64);
                                ret = hex32;
                            }
                            else
                            {
                                ret = hex64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
                else
                {
                    // context.INT_CONST() != null
                    if (len > 64)
                    {
                        ret = Double.NaN;
                    }
                    else if (isUnsigned)
                    {
                        UInt64 myUInt64;
                        try
                        {
                            myUInt64 = Convert.ToUInt64(value, 10);
                            // Maybe 32 bits is enough ?
                            if (myUInt64 <= UInt32.MaxValue)
                            {
                                UInt32 myUInt32 = Convert.ToUInt32(myUInt64);
                                ret = myUInt32;
                            }
                            else
                            {
                                ret = myUInt64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                    else
                    {
                        Int64 myInt64;
                        try
                        {
                            myInt64 = Convert.ToInt64(value, 10);
                            // Maybe 32 bits is enough ?
                            if ((myInt64 >= UInt32.MinValue) && (myInt64 <= UInt32.MaxValue))
                            {
                                Int32 myInt32 = Convert.ToInt32(myInt64);
                                ret = myInt32;
                            }
                            else
                            {
                                ret = myInt64;
                            }
                        }
                        catch
                        {
                            ret = Double.NaN;
                        }
                    }
                }
            }
            else
            {
                double d;
                if (value.EndsWith("m", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("r", StringComparison.OrdinalIgnoreCase) ||
                    value.EndsWith("d", StringComparison.OrdinalIgnoreCase))
                {
                    value = value.Substring(0, value.Length - 1);
                }
                try
                {
                    d = double.Parse(value, System.Globalization.CultureInfo.InvariantCulture);
                }
                catch (Exception)
                {
                    d = Double.NaN;
                }
                ret = d;
            }
            return ret;
        }

        /// <summary>
        /// UnEscape the string, converting the Escape sequence into it's "real" form
        /// https://en.wikipedia.org/wiki/Escape_sequences_in_C
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        public string BuildUnEscapedString(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                return text;
            }
            StringBuilder retval = new StringBuilder(text.Length);
            for (int ix = 1; ix < text.Length;)
            {
                // Search the next escape char '\'
                int jx = text.IndexOf('\\', ix);
                // If none, or too far (at least one char is needed AFTER the escape)
                if (jx < 0 || jx == text.Length - 1)
                    jx = text.Length;
                // Copy all text, up to that escape char
                retval.Append(text, ix, jx - ix);
                // End ?
                if (jx >= text.Length)
                    break;
                // Get the next one
                switch (text[jx + 1])
                {
                    case 'a':
                        // Alarm
                        retval.Append('\a');
                        break;
                    case 'b':
                        // BackSpace
                        retval.Append('\b');
                        break;
                    case 'f':
                        // Form feed
                        retval.Append('\f');
                        break;
                    case 'n':
                        // Line feed
                        retval.Append('\n');
                        break;
                    case 'r':
                        // Carriage return
                        retval.Append('\r');
                        break;
                    case 't':
                        // Tab
                        retval.Append('\t');
                        break;
                    case 'v':
                        // Vertical Tab
                        retval.Append('\v');
                        break;
                    case '\\':
                        // Don't escape
                        retval.Append('\\');
                        break;
                    case '\'':
                        // Simple Quote
                        retval.Append('\'');
                        break;
                    case '\"':
                        // Double Quote
                        retval.Append('\"');
                        break;
                    case '?':
                        // Question Mark
                        retval.Append('?');
                        break;
                    case '0':
                        // EOS
                        retval.Append('\0');
                        break;
                    case 'x':
                        // Hexadecimal code is following
                        // We need at least ONE char after the "\x"
                        if (jx + 2 >= text.Length)
                        {
                            break;
                        }
                        // Move after the 'x'
                        jx = jx + 2;
                        int digits = 0;
                        char[] hex = new char[4];
                        while (jx < text.Length && digits < 4)
                        {
                            char hexChar = text[jx];
                            if ((hexChar >= '0' && hexChar <= '9') || (hexChar >= 'a' && hexChar <= 'f') || (hexChar >= 'A' && hexChar <= 'F'))
                            {
                                hex[digits] = hexChar;
                                digits++;
                                jx++;
                            }
                            else
                            {
                                break;
                            }
                        }
                        // Something wrong in the value
                        if (digits == 0)
                        {
                            break;
                        }
                        else
                        {
                            // Put the Digits in a string
                            string hexValue = new string(hex, 0, digits);
                            // and convert....
                            UInt32 intValue = Convert.ToUInt32(hexValue, 16);
                            Char asciiChar = (char)intValue;
                            retval.Append(asciiChar);
                        }
                        // Adjust jx, because we have jx+2 for the loop
                        jx = jx - 2;
                        break;
                    default:
                        // Unrecognized, copy as-is
                        retval.Append('\\').Append(text[jx + 1]);
                        break;
                }
                // Jump over the escape char and the next one
                ix = jx + 2;
            }
            return retval.ToString();
        }

        protected XCodeTypeReference BuildTypeReference(string name)
        {
            var xtype = findType(name);
            if (xtype != null)
            {
                return new XCodeTypeReference(xtype);
            }
            else
            {
                return new XCodeTypeReference(name);
            }


        }

        protected IXType findInCache(string typeName)
        {
            if (_types.ContainsKey(typeName))
            {
                return _types[typeName];
            }
            return null;
        }


        protected IXType findType(string typeName, IList<string> usings = null)
        {
            if (_types.ContainsKey(typeName))
            {
                return _types[typeName];
            }
            if (usings == null)
            {
                usings = _usings;
            }
            IXType type;
            var myusings = usings.ToArray();
            type = _projectNode.ResolveXType(typeName, myusings);
            if (type != null)
            {
                _types.Add(typeName, type);
                return type;
            }
            type = _projectNode.ResolveReferencedType(typeName, myusings);
            if (type != null)
            {
                _types.Add(typeName, type);
                return type;
            }
            type = _projectNode.ResolveExternalType(typeName, myusings);
            if (type != null)
            {
                _types.Add(typeName, type);
            }
            return type;
        }


        protected IXType findParentType(IXType xtype)
        {
            var result = findInCache(xtype.BaseType);
            if (result != null)
                return result;
            var xparent = findType(xtype.BaseType, xtype.FileUsings);
            if (xparent != null)
                return xparent;
            var parent = findType(xtype.BaseType);
            if (parent == null)
            {
                parent = _projectNode.ResolveXType(xtype.BaseType, xtype.FileUsings.ToArray());
            }
            return parent;

        }


        protected XCodeTypeReference BuildNativeType(XSharpParser.NativeTypeContext nativeType)
        {
            //
            Type type;
            switch (nativeType.Token.Type)
            {
                case XSharpParser.BYTE:
                    type = typeof(System.Byte);
                    break;
                case XSharpParser.DWORD:
                    type = typeof(System.UInt32);
                    break;
                case XSharpParser.SHORTINT:
                    type = typeof(System.Int16);
                    break;
                case XSharpParser.INT:
                case XSharpParser.LONGINT:
                    type = typeof(System.Int32);
                    break;
                case XSharpParser.INT64:
                    type = typeof(System.Int64);
                    break;
                case XSharpParser.UINT64:
                    type = typeof(System.UInt64);
                    break;
                case XSharpParser.LOGIC:
                    type = typeof(System.Boolean);
                    break;
                case XSharpParser.OBJECT:
                    type = typeof(System.Object);
                    break;
                case XSharpParser.REAL4:
                    type = typeof(System.Single);
                    break;
                case XSharpParser.REAL8:
                    type = typeof(System.Double);
                    break;
                case XSharpParser.STRING:
                    type = typeof(System.String);
                    break;
                case XSharpParser.WORD:
                    type = typeof(System.UInt16);
                    break;
                case XSharpParser.VOID:
                    type = typeof(void);
                    break;
                case XSharpParser.DYNAMIC:
                    type = typeof(System.Object);
                    break;
                default:
                    var strType = nativeType.Token.Text;
                    return BuildTypeReference(strType);
            }
            return new XCodeTypeReference(type.FullName);
        }

        protected XCodeTypeReference BuildXBaseType(XSharpParser.XbaseTypeContext xbaseType)
        {
            return new XCodeTypeReference(xbaseType.Token.Text);
        }

        protected XCodeTypeReference BuildSimpleName(XSharpParser.SimpleNameContext simpleName)
        {
            XCodeTypeReference expr = null;
            //
            string name = simpleName.Id.GetText();
            string gen = "";
            if (simpleName.GenericArgList != null)
            {
                string argList = "";
                int i = 0;
                foreach (var generic in simpleName.GenericArgList._GenericArgs)
                {
                    if (i > 0)
                        argList += ",";
                    var tmp = BuildDataType(generic);
                    argList += tmp.BaseType;
                    i++;
                }
                //
                gen = "`" + i.ToString() + "[" + argList + "]";
            }
            expr = BuildTypeReference(name + gen);
            //
            return expr;
        }
        #endregion
        #region Common Methods
        private XCodeNamespace _currentNamespace;
        public XCodeNamespace CurrentNamespace
        {
            get
            {
                if (_currentNamespace == null)
                {
                    _currentNamespace = new XCodeNamespace("");
                }
                return _currentNamespace;
            }

            internal set
            {
                _currentNamespace = value;
            }
        }
        public override void EnterUsing_([NotNull] XSharpParser.Using_Context context)
        {
            var import = new XCodeNamespaceImport(context.Name.GetText());
            if (!_usings.Contains(import.Namespace))
            {
                writeTrivia(import, context);
                CurrentNamespace.Imports.Add(import);
                _usings.Add(import.Namespace);
            }
        }

        protected void writeTrivia(CodeObject o, XSharpParserRuleContext context, bool end = false)
        {
            string trivia;
            if (end)
                trivia = context.GetEndingTrivia(_tokens);
            else
                trivia = context.GetLeadingTrivia(_tokens);
            if (!string.IsNullOrEmpty(trivia))
            {
                var key = end ? XSharpCodeConstants.USERDATA_ENDINGTRIVIA : XSharpCodeConstants.USERDATA_LEADINGTRIVIA;
                o.UserData[key] = trivia;
            }

        }
        #endregion
    }
}
