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
using System.Collections.Immutable;
using XSharpModel;
namespace XSharp.CodeDom
{
    internal class XSharpFieldsDiscover : XSharpBaseDiscover
    {

        internal Stack<ParserRuleContext> classes;
        internal ParserRuleContext currentClass;
        private MemberAttributes classVarModifiers;

        internal XSharpFieldsDiscover(IProjectTypeHelper projectNode) : base(projectNode)
        {
            classes = new Stack<ParserRuleContext>();
            currentClass = null;
        }

        public override void EnterClass_(XSharpParser.Class_Context context)
        {
            // pop previous class
            classes.Push(currentClass);
            currentClass = context;
            FieldList.Add(context, new List<XCodeMemberField>());
        }
        public override void ExitClass_(XSharpParser.Class_Context context)
        {
            // restore previous class 
            currentClass = classes.Pop();
        }
        public override void EnterClassvarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {

            this.classVarModifiers = MemberAttributes.Public;
            //
            ITerminalNode[] visibility;
            //
            visibility = context.INTERNAL();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Assembly;
            //
            visibility = context.HIDDEN();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PRIVATE();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PROTECTED();
            if (visibility.Length > 0)
            {
                visibility = context.INTERNAL();
                if (visibility.Length > 0)
                    this.classVarModifiers = MemberAttributes.FamilyOrAssembly;
                else
                    this.classVarModifiers = MemberAttributes.Family;
            }
            //
            visibility = context.EXPORT();
            if (visibility.Length > 0)
                this.classVarModifiers = MemberAttributes.Public;
            //
            if (context.CONST().Length > 0)
                this.classVarModifiers |= MemberAttributes.Const;
            if (context.STATIC().Length > 0)
                this.classVarModifiers |= MemberAttributes.Static;
        }
        public override void EnterClassVarList([NotNull] XSharpParser.ClassVarListContext context)
        {
            //
            if (context.DataType != null && currentClass != null)
            {
                var fieldType = BuildDataType(context.DataType);
                //
                foreach (var varContext in context._Var)
                {
                    var  field = new XCodeMemberField();
                    field.Name = varContext.Id.GetText();
                    field.Type = fieldType;
                    field.Attributes = this.classVarModifiers;
                    if (varContext.Initializer != null)
                    {
                        if (varContext.Initializer is XSharpParser.PrimaryExpressionContext)
                        {
                            XSharpParser.PrimaryContext ctx = ((XSharpParser.PrimaryExpressionContext)varContext.Initializer).Expr;
                            if (ctx is XSharpParser.LiteralExpressionContext)
                            {
                                XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)ctx;
                                field.InitExpression = BuildLiteralValue(lit.Literal);
                            }
                        }
                        else
                        {
                            field.InitExpression = BuildSnippetExpression(varContext.Initializer.GetText());
                        }
                    }
                    FillCodeDomDesignerData(field, varContext.Start.Line, varContext.Start.Column);
                    //
                    FieldList[currentClass].Add(field);
                }
                //
            }
        }


    }
}