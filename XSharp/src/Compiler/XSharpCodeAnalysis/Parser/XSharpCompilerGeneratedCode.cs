//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#nullable disable
using System;
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    internal partial class XSharpLanguageParser : SyntaxParser
    {
        private SyntaxTree ProcessTreesImpl(SyntaxTree[] trees, CSharpParseOptions parseoptions)
        {
            // this method gives us the ability to check all the generated syntax trees,
            // add generated constructors to partial classes when none of the parts has a constructor
            // merge accesses and assigns from different source files into one property etc.
            // we pass the usings from the compilation units along because the new compilation unit will
            // have to the same (combined) list of usings
            // When compiling in VO/Vulcan dialect we also collect the literal symbols from the compilation unit.
            // When we have one or more of these then we create a symbol table in the class "Xs$SymbolTable"
            var partialClasses = new Dictionary<string, List<PartialPropertyElement>>(XSharpString.Comparer);
            var symbolTable = new Dictionary<string, FieldDeclarationSyntax>();
            var pszTable = new Dictionary<string, Tuple<string, FieldDeclarationSyntax>>();
            foreach (var tree in trees)
            {
                var compilationunit = tree.GetRoot() as Syntax.CompilationUnitSyntax;
                if (compilationunit.NeedsProcessing)
                {
                    foreach (var member in compilationunit.Members)
                    {
                        if (member is Syntax.NamespaceDeclarationSyntax)
                        {
                            ProcessNameSpace(member as Syntax.NamespaceDeclarationSyntax, partialClasses, compilationunit.Usings);
                        }
                        else
                        {
                            var node = member.Green as CSharpSyntaxNode;
                            if (node.XNode is XP.EntityContext)
                            {
                                ProcessType(node.XNode as XP.EntityContext, partialClasses, compilationunit.Usings);
                            }
                        }
                    }
                }
                if (_options.HasRuntime)
                {
                    if (compilationunit.LiteralSymbols.Count > 0)
                    {
                        foreach (var pair in compilationunit.LiteralSymbols)
                        {
                            if (!symbolTable.ContainsKey(pair.Key))
                            {
                                symbolTable.Add(pair.Key, pair.Value);
                            }
                        }
                        compilationunit.LiteralSymbols.Clear();
                    }
                    if (compilationunit.LiteralPSZs.Count > 0)
                    {
                        foreach (var pair in compilationunit.LiteralPSZs)
                        {
                            if (!pszTable.ContainsKey(pair.Key))
                            {
                                pszTable.Add(pair.Key, pair.Value);
                            }
                        }
                        compilationunit.LiteralSymbols.Clear();
                    }
                }
            }
            if (partialClasses.Count > 0 || symbolTable.Count > 0 || pszTable.Count > 0)
            {
                // Create a new tree which shall have the generated constructors and properties
                // and return this tree to the caller.
                // we copy the attributes, modifiers etc from one of the class instances to make sure that
                // do not specify a conflicting modifier.
                var trans = CreateTransform(null, _options, _pool, _syntaxFactory, _fileName);
                SyntaxListBuilder<UsingDirectiveSyntax> usingslist = _pool.Allocate<UsingDirectiveSyntax>();
                var members = _pool.Allocate<MemberDeclarationSyntax>();
                var clsmembers = _pool.Allocate<MemberDeclarationSyntax>();
                foreach (var element in partialClasses)
                {
                    var name = element.Key;
                    bool hasctor = false;
                    bool haspartialprop = false;
                    XP.IPartialPropertyContext ctxt = null;
                    //XP.Namespace_Context xns;
                    foreach (var val in element.Value)
                    {
                        var xnode = val.Type;
                        ctxt = xnode;
                        if (xnode.TypeData.HasInstanceCtor)
                        {
                            hasctor = true;
                        }
                        if (xnode.TypeData.PartialProps)
                        {
                            haspartialprop = true;
                        }
                    }
                    if (ctxt.CsNode is InterfaceDeclarationSyntax)
                    {
                        var ifdecl = ctxt.Get<InterfaceDeclarationSyntax>();
                        // ctxt.Parent is XP.EntityContext
                        // ctxt.Parent.Parent may be XP.Namespace_Context
                        var xns = ctxt.Parent.Parent as XP.Namespace_Context;
                        if (haspartialprop)
                        {
                            clsmembers.Clear();
                            GeneratePartialProperties(element.Value, usingslist, trans, clsmembers);
                            if (clsmembers.Count > 0)
                            {
                                ifdecl = _syntaxFactory.InterfaceDeclaration(
                                            default,
                                            ifdecl.Modifiers,
                                            ifdecl.Keyword,
                                            ifdecl.Identifier,
                                            ifdecl.TypeParameterList,
                                            ifdecl.BaseList,
                                            ifdecl.ConstraintClauses,
                                            ifdecl.OpenBraceToken,
                                            clsmembers,
                                            ifdecl.CloseBraceToken,
                                            ifdecl.SemicolonToken);
                                ifdecl.XGenerated = true;
                                members.Add(WrapInNamespace(trans, ifdecl, xns, _options.DefaultNamespace));
                            }
                        }
                    }
                    else if (ctxt.CsNode is StructDeclarationSyntax)
                    {
                        var strucdecl = ctxt.Get<StructDeclarationSyntax>();
                        // ctxt.Parent is XP.EntityContext
                        // ctxt.Parent.Parent may be XP.Namespace_Context
                        var xns = ctxt.Parent.Parent as XP.Namespace_Context;
                        if (haspartialprop)
                        {
                            clsmembers.Clear();
                            GeneratePartialProperties(element.Value, usingslist, trans, clsmembers);
                            if (clsmembers.Count > 0)
                            {
                                strucdecl = _syntaxFactory.StructDeclaration(
                                                default,
                                                strucdecl.Modifiers,
                                                strucdecl.Keyword,
                                                strucdecl.Identifier,
                                                strucdecl.TypeParameterList,
                                                strucdecl.BaseList,
                                                strucdecl.ConstraintClauses,
                                                strucdecl.OpenBraceToken,
                                                clsmembers,
                                                strucdecl.CloseBraceToken,
                                                strucdecl.SemicolonToken);
                                strucdecl.XGenerated = true;
                                members.Add(WrapInNamespace(trans, strucdecl, xns, _options.DefaultNamespace));
                            }
                        }
                    }
                    else if (ctxt.CsNode is ClassDeclarationSyntax)
                    {
                        // ctxt.Parent is XP.EntityContext
                        // ctxt.Parent.Parent may be XP.Namespace_Context
                        var xns = ctxt.Parent.Parent as XP.Namespace_Context;
                        if (!hasctor || haspartialprop)
                        {
                            clsmembers.Clear();
                            var classdecl = ctxt.Get<ClassDeclarationSyntax>();
                            var classcontext = ctxt as XP.Class_Context;
                            if (!hasctor && !classdecl.IsStatic() && trans != null && _options.HasOption(CompilerOption.DefaultClipperContructors, classcontext, trans.PragmaOptions))
                            {
                                var ctor = trans.GenerateDefaultCtor(classdecl.Identifier, classcontext, usingslist, element.Value);
                                if (ctor != null)
                                {
                                    clsmembers.Add(ctor);
                                }
                            }
                            if (haspartialprop)
                            {
                                GeneratePartialProperties(element.Value, usingslist, trans, clsmembers);
                            }
                            if (clsmembers.Count > 0)
                            {
                                classdecl = _syntaxFactory.ClassDeclaration(
                                                            default,
                                                            classdecl.Modifiers,
                                                            classdecl.Keyword,
                                                            classdecl.Identifier,
                                                            classdecl.TypeParameterList,
                                                            classdecl.BaseList,
                                                            classdecl.ConstraintClauses,
                                                            classdecl.OpenBraceToken,
                                                            clsmembers,
                                                            classdecl.CloseBraceToken,
                                                            classdecl.SemicolonToken);
                                classdecl.XGenerated = true;

                                members.Add(WrapInNamespace(trans, classdecl, xns, _options.DefaultNamespace));
                            }
                        }
                    }
                }
                if (symbolTable.Count > 0)
                {
                    // build internal static symbol table class 
                    members.Add(GenerateSymbolsClass(symbolTable, trans));
                }
                if (pszTable.Count > 0)
                {
                    // build internal static psz table class 
                    members.Add(GeneratePszClass(pszTable, trans));
                }
                var result = _syntaxFactory.CompilationUnit(
                                    default,
                                    usingslist,
                                    default,
                                    members,
                                    SyntaxFactory.Token(SyntaxKind.EndOfFileToken));
                result.XGenerated = true;
                var tree = (CSharpSyntaxTree)CSharpSyntaxTree.Create((Syntax.CompilationUnitSyntax)result.CreateRed(), parseoptions);
                result.SyntaxTree = tree;
                tree.Generated = true;
                _pool.Free(members);
                _pool.Free(usingslist);
                _pool.Free(clsmembers);
                return tree;
            }
            return null;
        }

        private ClassDeclarationSyntax GenerateClass(string name, SyntaxListBuilder<MemberDeclarationSyntax> members, XSharpTreeTransformationCore trans)
        {
            var decl = _syntaxFactory.ClassDeclaration(
                                default,
                                trans.TokenList(SyntaxKind.StaticKeyword, SyntaxKind.InternalKeyword),
                                SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                                SyntaxFactory.MakeIdentifier(name),
                                null,
                                null,
                                default,
                                SyntaxFactory.OpenBraceToken,
                                members,
                                SyntaxFactory.CloseBraceToken,
                                null);
            return decl;
        }
        private ClassDeclarationSyntax GenerateSymbolsClass(Dictionary<string, FieldDeclarationSyntax> fields, XSharpTreeTransformationCore trans)
        {
            var clsmembers = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var field in fields)
            {
                clsmembers.Add(field.Value);
            }
            fields.Clear();
            return GenerateClass(XSharpSpecialNames.SymbolTable, clsmembers, trans);
        }
        private ClassDeclarationSyntax GeneratePszClass(Dictionary<string, Tuple<string, FieldDeclarationSyntax>> fields, XSharpTreeTransformationCore trans)
        {
            var clsmembers = _pool.Allocate<MemberDeclarationSyntax>();
            foreach (var field in fields)
            {
                var fieldDecl = field.Value.Item2;
                clsmembers.Add(fieldDecl);
            }
            fields.Clear();
            return GenerateClass(XSharpSpecialNames.PSZTable, clsmembers, trans);
        }
        private void GeneratePartialProperties(List<PartialPropertyElement> classes,
                                    SyntaxListBuilder<UsingDirectiveSyntax> usingslist,
                                    XSharpTreeTransformationCore trans,
                                    SyntaxListBuilder<MemberDeclarationSyntax> members)
        {
            // Create a list of member declarations for all partial types 
            // that do not have a constructor (when /vo16 is selected)
            // or where access and assign were not found in the same source file
            // the usingList will contain an unique list of using statements combined from the various
            // source files where the types were found.

            var dict = new Dictionary<string, List<XP.IMethodContext>>(XSharpString.Comparer);
            var tmpUsings = new List<Syntax.UsingDirectiveSyntax>();
            foreach (var clsctx in classes)
            {
                if (clsctx.Type.PartialProperties != null)
                {
                    foreach (var m in clsctx.Type.PartialProperties)
                    {
                        var idName = m.Id.Get<SyntaxToken>();
                        var name = idName.Text;
                        if (dict.ContainsKey(name))
                        {
                            dict[name].Add(m);
                        }
                        else
                        {
                            var list = new List<XP.IMethodContext>() { m };
                            dict.Add(name, list);
                        }
                    }
                    // Collect quickly now. Remove duplicates later.
                    tmpUsings.AddRange(clsctx.Usings);
                }
            }
            // now we have a list of PropertyNames and methods

            // add the usings when they are not in the list yet
            foreach (var u in tmpUsings)
            {
                var green = u.Green as UsingDirectiveSyntax;
                trans.AddUsingWhenMissing(usingslist, green.Name, green.StaticKeyword != null, green.Alias);
            }

            // For each unique name add a property
            foreach (var element in dict)
            {
                var prop = new XSharpTreeTransformationCore.SyntaxClassEntities.VoPropertyInfo();
                prop.idName = SyntaxFactory.Identifier(element.Key);
                foreach (var m in element.Value)
                {
                    if (m.RealType == XSharpLexer.ACCESS)
                    {
                        if (prop.AccessMethodCtx == null)
                            prop.AccessMethodCtx = m;
                        else
                            prop.DupAccess = m;
                    }
                    else
                    {
                        if (prop.AssignMethodCtx == null)
                            prop.AssignMethodCtx = m;
                        else
                            prop.DupAssign = m;
                    }
                    if (m.Mods != null && m.Mods._Tokens.Any(t => t.Type == XSharpLexer.STATIC))
                        prop.IsStatic = true;
                }
                var propdecl = trans.GenerateVoProperty(prop, null);

                members.Add(propdecl);
            }
            return;
        }
        private static void ProcessType(XP.EntityContext xnode, Dictionary<string, List<PartialPropertyElement>> partialClasses,
                                    IEnumerable<Syntax.UsingDirectiveSyntax> usings)
        {
            if (xnode != null && xnode.ChildCount == 1)
            {
                var cls = xnode.GetChild(0) as XP.IPartialPropertyContext;
                if (cls != null && (cls.TypeData.Partial || cls.TypeData.PartialProps))
                {
                    var name = cls.Name;
                    if (!partialClasses.ContainsKey(name))
                    {
                        partialClasses.Add(name, new List<PartialPropertyElement>());
                    }
                    partialClasses[name].Add(new PartialPropertyElement(cls, usings));
                }
            }
        }
        private static void ProcessNameSpace(Syntax.NamespaceDeclarationSyntax ns,
            Dictionary<string, List<PartialPropertyElement>> partialClasses,
            IEnumerable<Syntax.UsingDirectiveSyntax> usings)
        {
            foreach (var member in ns.Members)
            {
                if (member is Syntax.NamespaceDeclarationSyntax)
                {
                    ProcessNameSpace(member as Syntax.NamespaceDeclarationSyntax, partialClasses, usings);
                }
                else
                {
                    var node = member.Green as CSharpSyntaxNode;
                    if (node.XNode is XP.EntityContext)
                    {
                        ProcessType(node.XNode as XP.EntityContext, partialClasses, usings);
                    }
                }
            }
        }
    }
    //#endif
}
