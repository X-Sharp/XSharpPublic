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
    internal class XSharpFieldsDiscover : XSharpBaseDiscover
    {

        internal Stack<ParserRuleContext> classes;
        internal ParserRuleContext currentClass;
        

        internal XSharpFieldsDiscover(IProjectTypeHelper projectNode, CodeTypeDeclaration typeInOtherFile) : base(projectNode, typeInOtherFile)
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
        public MemberAttributes decodeClassVarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {

            var classVarModifiers = MemberAttributes.Public;
            //
            ITerminalNode[] visibility;
            //
            visibility = context.INTERNAL();
            if (visibility.Length > 0)
                classVarModifiers = MemberAttributes.Assembly;
            //
            visibility = context.HIDDEN();
            if (visibility.Length > 0)
                classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PRIVATE();
            if (visibility.Length > 0)
                classVarModifiers = MemberAttributes.Private;
            //
            visibility = context.PROTECTED();
            if (visibility.Length > 0)
            {
                visibility = context.INTERNAL();
                if (visibility.Length > 0)
                    classVarModifiers = MemberAttributes.FamilyOrAssembly;
                else
                    classVarModifiers = MemberAttributes.Family;
            }
            //
            visibility = context.EXPORT();
            if (visibility.Length > 0)
                classVarModifiers = MemberAttributes.Public;
            //
            if (context.CONST().Length > 0)
                classVarModifiers |= MemberAttributes.Const;
            if (context.STATIC().Length > 0)
                classVarModifiers |= MemberAttributes.Static;
            return classVarModifiers;
        }
        public override void EnterClassvars([NotNull] XSharpParser.ClassvarsContext context)
        {
            // PROTECT a,b as STRING
            // copy type from b to a
            var classVarModifiers = decodeClassVarModifiers(context.Modifiers);
            if (context._Vars.Count > 1)
            {
                XSharpParser.DatatypeContext dtc = null; ;
                foreach (var classvar in context._Vars.Reverse())
                {
                    if (classvar.DataType != null)
                        dtc = classvar.DataType;
                    else
                        classvar.DataType = dtc;
                }
            }

            foreach (var varContext in context._Vars)
            {
                var field = new XCodeMemberField();
                var fieldType = BuildDataType(varContext.DataType);
                field.Name = varContext.Id.GetText();
                field.Type = fieldType;
                field.Attributes = classVarModifiers;
                if (varContext.Initializer != null)
                {
                    field.InitExpression = BuildExpression(varContext.Initializer, false);
                }
                FillCodeDomDesignerData(field, varContext.Start.Line, varContext.Start.Column);
                //
                FieldList[currentClass].Add(field);
            }
            //
        }


    }
}
