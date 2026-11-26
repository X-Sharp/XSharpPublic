//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    [Guid(XSharpConstants.CompletionOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    public class CompletionOptionsPage : XSDialogPage<CompletionOptionsControl, CompletionOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
  

}
