﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpClearSequences : XSharpBaseListener
    {
        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
            if (context is XSharpParserRuleContext xpr)
            {
                xpr.ClearSequencePoint();
            }
        }
    }

}

