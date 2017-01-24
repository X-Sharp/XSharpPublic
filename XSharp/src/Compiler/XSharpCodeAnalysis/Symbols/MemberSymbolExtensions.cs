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
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class MemberSymbolExtensions
    {
        

        internal static bool HasClipperCallingConvention(this Symbol method)
        {
            if (method is SourceMethodSymbol)
            {
                var sms = method as SourceMethodSymbol;
                var xnode = sms.SyntaxNode?.XNode;
                if (xnode != null)
                {
                    var clsmethod = xnode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ClsmethodContext;
                    if (clsmethod != null)
                    {
                        return clsmethod.Member.Data.HasClipperCallingConvention;
                    }
                }
            }

            if (method.GetParameterCount() != 1)
                return false;
            var atts = method.GetAttributes();
            foreach (var att in atts)
            {
                var aclass = att.AttributeClass;
                if (aclass.IsVulcanRTAttribute(VulcanTypeNames.ClipperCallingConventionAttribute))
                    return true;
            }
            return false;
        }
    }

}