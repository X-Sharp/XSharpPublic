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

        

        internal XSharpFieldsDiscover(IProjectTypeHelper projectNode, CodeTypeDeclaration typeInOtherFile) : base(projectNode, typeInOtherFile)
        {
        }

 
        //classvarModifiers   : (Tokens+=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC
        //                      | EXPORT | INTERNAL | VOLATILE | UNSAFE | FIXED) )+

        public MemberAttributes decodeClassVarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {

            var visibility = MemberAttributes.Public;
            var modifiers = (MemberAttributes)0;
            foreach (var token in context._Tokens)
            {
                switch (token.Type)
                {
                    case XSharpLexer.INTERNAL:
                        if (visibility == MemberAttributes.Family)
                            visibility = MemberAttributes.FamilyOrAssembly;
                        else
                            visibility = MemberAttributes.Assembly;
                        break;
                    case XSharpLexer.HIDDEN:
                    case XSharpLexer.PRIVATE:
                        visibility = MemberAttributes.Private;
                        break;
                    case XSharpLexer.PUBLIC:
                    case XSharpLexer.EXPORT:
                        visibility = MemberAttributes.Public;
                        break;
                    case XSharpLexer.PROTECTED:
                        if (visibility == MemberAttributes.Assembly)
                            visibility = MemberAttributes.FamilyOrAssembly;
                        else
                            visibility = MemberAttributes.Family;
                        break;
                    case XSharpLexer.CONST:
                        modifiers |= MemberAttributes.Const;
                        break;
                    case XSharpLexer.STATIC:
                        modifiers |= MemberAttributes.Static;
                        break;
                    case XSharpLexer.INITONLY:
                    case XSharpLexer.VOLATILE:
                    case XSharpLexer.INSTANCE:
                    case XSharpLexer.UNSAFE:
                    case XSharpLexer.FIXED:
                        break;
                }
            }
            return visibility | modifiers;
        }
        public override void EnterClassvars([NotNull] XSharpParser.ClassvarsContext context)
        {
            // PROTECT a,b as STRING
            // copy type from b to a
            var classVarModifiers = decodeClassVarModifiers(context.Modifiers);
            if (context._Vars.Count > 1)
            {
                XSharpParser.DatatypeContext dtc = null; 
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
                    try
                    {
                        field.InitExpression = BuildExpression(varContext.Initializer, false);
                    }
                    catch
                    {
                        field.InitExpression = BuildSnippetExpression(varContext.Initializer);
                    }

                    SaveSourceCode(field.InitExpression, varContext.Initializer);
                }
                FillCodeDomDesignerData(field, varContext.Start.Line, varContext.Start.Column);
                // write original source for the attributes
                AddMemberAttributes(field, classVarModifiers, context.Modifiers);
                writeTrivia(field, context);
                SaveSourceCode(field, varContext);
                //
                FieldList[currentContext].Add(field);
            }
            //
        }


    }
}
