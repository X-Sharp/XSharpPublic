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
using Microsoft.CodeAnalysis.CSharp.Emit;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal partial class SourceMemberFieldSymbol : SourceFieldSymbolWithSyntaxReference
    {
        internal TypeSymbol GetVOGlobalType(CSharpCompilation compilation, TypeSyntax typeSyntax, Binder binder, ConsList<FieldSymbol> fieldsBeingBound)
        {
            var xNode = this.SyntaxNode.XNode;
            if (compilation.Options.VOResolveTypedFunctionPointersToPtr)
            {
                if (xNode is XP.ClassvarContext &&
                    xNode.Parent is XP.ClassVarListContext)
                {
                    var cvl = xNode.Parent as XP.ClassVarListContext;
                    var dt = cvl.DataType;
                    if (dt is XP.PtrDatatypeContext)
                    {
                        // So we have a global as typed ptr
                        // change the type from typed ptr to just ptr
                        var ptrdtc = dt as XP.PtrDatatypeContext;
                        if (ptrdtc.TypeName.Name != null)           // User Define Typename PTR
                        {
                            string name = ptrdtc.TypeName.Name.GetText();
                            // Lookup name ?
                            return compilation.GetSpecialType(SpecialType.System_IntPtr);
                        }

                    }
                }
            }
            TypeSymbol type = null;
            if (xNode is XP.VodefineContext && ! this.IsConst)
            {
                var vodef = xNode as XP.VodefineContext;
                DiagnosticBag diagnostics = DiagnosticBag.GetInstance();
                type = binder.BindType(typeSyntax, diagnostics);
                // parser could not determine the type
                fieldsBeingBound = new ConsList<FieldSymbol>(this, fieldsBeingBound);
                var declarator = (VariableDeclaratorSyntax)this.DeclaringSyntaxReferences.AsSingleton().GetSyntax();
                var initializerBinder = new ImplicitlyTypedFieldBinder(binder, fieldsBeingBound);
                var initializerOpt = initializerBinder.BindInferredVariableInitializer(diagnostics, RefKind.None, declarator.Initializer, declarator);
                if (initializerOpt != null && !type.IsPsz())
                {
                    if ((object)initializerOpt.Type != null && !initializerOpt.Type.IsErrorType())
                    {
                        type = initializerOpt.Type;

                        if (!type.IsVoidPointer() && initializerOpt.ConstantValue != null && !this.IsConst)
                        {
                            this._modifiers |= DeclarationModifiers.Const;
                            this._modifiers |= DeclarationModifiers.Static;
                            this._modifiers &= ~DeclarationModifiers.ReadOnly;
                        }
                        if (type.IsEnumType())
                        {
                            type = type.GetEnumUnderlyingType();
                        }
                    }
                }
                if (type == null)
                {
                    type = compilation.GetSpecialType(SpecialType.System_Object);
                }
                //System.Diagnostics.Debug.WriteLine($"Looking for type of define {vodef.Name.ToString()}, found {type.ToString()}, const: {IsConst}");

                return type;
            }
            return null;
        }
    }
}
