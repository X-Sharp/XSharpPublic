//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService
{
    public class XSharpTokens
    {
        public BufferedTokenStream TokenStream { get; set; }
        public ITextSnapshot SnapShot { get; set; }
        public XSharpTokens(BufferedTokenStream tokenstream, ITextSnapshot snapshot)
        {
            TokenStream = tokenstream;
            SnapShot = snapshot;
        }
        public bool Complete => TokenStream != null && SnapShot != null;
    }
}
