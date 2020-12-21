//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal sealed partial class SourcePropertySymbol
    {
        internal PropertySymbol validateProperty(PropertySymbol overriddenProperty, DiagnosticBag diagnostics, Location location)
        {
            if (overriddenProperty == null && XSharpString.CaseSensitive)
            {
                // check if we have a base type and if the base type has a method with the same name but different casing
                var baseType = this.ContainingType.BaseTypeNoUseSiteDiagnostics;
                var members = baseType.GetMembersUnordered().Where(
                        member => member.Kind == SymbolKind.Property && member.IsVirtual && String.Equals(member.Name, this.Name, StringComparison.OrdinalIgnoreCase) );
                if (members.Count() > 0)
                {
                    foreach (var member in members)
                    {
                        var propSym = member as PropertySymbol;
                        bool equalSignature = propSym.ParameterCount == this.ParameterCount && TypeSymbol.Equals(this.Type, propSym.Type);
                        if (equalSignature)
                        {
                            var thisTypes = this.Parameters;
                            var theirTypes = propSym.Parameters;
                            for (int i = 0; i < thisTypes.Length; i++)
                            {
                                if (thisTypes[i].Type != theirTypes[i].Type)
                                {
                                    equalSignature = false;
                                    break;
                                }
                            }
                        }
                        if (equalSignature)
                        {
                            diagnostics.Add(ErrorCode.ERR_CaseDifference, location, baseType.Name, "property", member.Name, this.Name);
                        }

                    }
                }

            }
            return this;
        }
    }

}
