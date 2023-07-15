//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp", false)]
    [Guid(XSharpConstants.FormattingOptionsPageGuidString)]
    [ComVisible(true)]
    public class FormattingOptionsPage : XSDialogPage<FormattingOptionsControl, FormattingOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
  

}
