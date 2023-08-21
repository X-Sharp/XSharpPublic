//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Linq;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal sealed partial class AnonymousTypeManager : CommonAnonymousTypeManager
    {
        /// <summary>
        /// Given a codeblock delegate provided constructs a codeblock type symbol.
        /// </summary>
        public NamedTypeSymbol ConstructCodeblockTypeSymbol(TypeSymbol[] codeblockParameters, Location location)
        {
            return new CodeblockTypePublicSymbol(this, codeblockParameters, location);
        }

        public NamedTypeSymbol GetCodeblockDelegateType(NamedTypeSymbol cbType)
        {
            return (NamedTypeSymbol)((CodeblockTypePublicSymbol)cbType).Properties[0].Type;
        }
        public NamedTypeSymbol CodeblockType
        {
            get { return this.Compilation.CodeBlockType(); }
        }

        public NamedTypeSymbol UsualType
        {
            get { return this.Compilation.UsualType(); }
        }
        public ArrayTypeSymbol UsualArrayType
        {
            get { return this.Compilation.CreateArrayTypeSymbol(this.Compilation.UsualType()); }
        }

        internal (ImmutableArray<AnonymousTypePropertySymbol>, ImmutableArray<Symbol>, ImmutableArray<MethodSymbol>) GetCodeBlockMembers(
            AnonymousTypeDescriptor typeDescr, AnonymousTypeTemplateSymbol template, MultiDictionary<string, Symbol> nameToSymbols)
        {
            ImmutableArray<Symbol> members;
            int cbParamCount = typeDescr.Fields.Length;
            NamedTypeSymbol[] cbParameters = new NamedTypeSymbol[cbParamCount];
            for (int i = 0; i < cbParamCount; i++)
            {
                cbParameters[i] = this.UsualType;
            }
            var cbDelegate = this.SynthesizeDelegate(typeDescr.Fields.Length - 1, default, false, 0).Construct(cbParameters);

            Symbol[] cbMembers = new Symbol[7]; // 2 fields, 2 properties, ctor, ToString() and Eval()
            int cbMemberIndex = 0;

            var tDelegate = TypeWithAnnotations.Create(cbDelegate);
            var tSource = TypeWithAnnotations.Create(this.System_String);
            // Add properties
            var eval = new AnonymousTypePropertySymbol(template, new AnonymousTypeField(XSharpSpecialNames.CodeBlockLambda, typeDescr.Location, tDelegate), tDelegate, 0);
            var source = new AnonymousTypePropertySymbol(template, new AnonymousTypeField(XSharpSpecialNames.CodeBlockSource, typeDescr.Location, tSource), tSource, 0);

            ImmutableArray<AnonymousTypePropertySymbol> properties = new[] { eval, source }.ToImmutableArray();

            // Property related symbols
            cbMembers[cbMemberIndex++] = eval;
            cbMembers[cbMemberIndex++] = eval.BackingField;
            cbMembers[cbMemberIndex++] = eval.GetMethod;
            cbMembers[cbMemberIndex++] = source;
            cbMembers[cbMemberIndex++] = source.BackingField;
            cbMembers[cbMemberIndex++] = source.GetMethod;

            cbMembers[cbMemberIndex++] = new AnonymousTypeConstructorSymbol(template, new[] { eval, source }.ToImmutableArray());

            members = cbMembers.AsImmutable();

            Debug.Assert(cbMemberIndex == members.Length);

            // fill nameToSymbols map
            foreach (var symbol in members)
            {
                nameToSymbols.Add(symbol.Name, symbol);
            }

            MethodSymbol[] cbSpecialMembers = new MethodSymbol[2];
            cbSpecialMembers[0] = new CodeblockEvalMethod(template);
            cbSpecialMembers[1] = new AnonymousTypeToStringMethodSymbol(template);
            var specialMembers = cbSpecialMembers.AsImmutable();

            return (properties, members, specialMembers);
        }

        internal (ImmutableArray<Symbol>, ImmutableArray<AnonymousTypePropertySymbol>, AnonymousTypeDescriptor)
            CreateCodeBlockType(AnonymousTypePublicSymbol codeblockSymbol, TypeSymbol[] codeblockParams,
            Location location, MultiDictionary<string, Symbol> nameToSymbols)
        {
            Debug.Assert(codeblockParams.Length > 0);

            var fields = ArrayBuilder<AnonymousTypeField>.GetInstance(codeblockParams.Length + 2);
            for (int i = 0; i < codeblockParams.Length; i++)
            {
                fields.Add(new AnonymousTypeField(XSharpSpecialNames.CodeBlockParameter + i, location, TypeWithAnnotations.Create(codeblockParams[i])));
            }
            var typeDescriptor = new AnonymousTypeDescriptor(fields.ToImmutable(), location);
            var codeblockDelegate = SynthesizeDelegate(codeblockParams.Length - 1, default, false, 0).Construct(codeblockParams);
            var lambda = new AnonymousTypeField(XSharpSpecialNames.CodeBlockLambda, location, TypeWithAnnotations.Create(codeblockDelegate));
            var source = new AnonymousTypeField(XSharpSpecialNames.CodeBlockSource, location, TypeWithAnnotations.Create(System_String));
            var properties = new[] {
                    new AnonymousTypePropertySymbol(codeblockSymbol, lambda,0),
                    new AnonymousTypePropertySymbol(codeblockSymbol, source,0)
                }.AsImmutableOrNull();

            Symbol[] members = new Symbol[4];
            int memberIndex = 0;
            members[memberIndex++] = properties[0];
            members[memberIndex++] = properties[1];
            members[memberIndex++] = new AnonymousTypeConstructorSymbol(codeblockSymbol, properties);
            members[memberIndex++] = new CodeblockEvalMethod(codeblockSymbol);

            var amembers = members.AsImmutableOrNull();
            Debug.Assert(memberIndex == members.Length);

            //  fill nameToSymbols map
            foreach (var symbol in members)
            {
                nameToSymbols.Add(symbol.Name, symbol);
            }

            return (amembers, properties, typeDescriptor);

        }
    }
}
