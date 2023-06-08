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

namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp", false)]
    [Guid(XSharpConstants.FormattingOptionsPageGuidString)]
    [ComVisible(true)]
    public class FormattingOptionsPage : XSDialogPage<FormattingOptionsControl, FormattingOptions>
    {
        #region Properties that are delegated to the Options object
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public KeywordCase KeywordCase
        {
            get => Options.KeywordCase;
            set => Options.KeywordCase = value;
        }
        public bool IdentifierCase
        {
            get => Options.IdentifierCase;
            set => Options.IdentifierCase = value;
        }
        public bool UdcCase
        {
            get => Options.UdcCase;
            set => Options.UdcCase = value;
        }
        public bool TrimTrailingWhiteSpace
        {
            get => Options.TrimTrailingWhiteSpace;
            set => Options.TrimTrailingWhiteSpace = value;
        }
        public bool InsertFinalNewLine
        {
            get => Options.InsertFinalNewLine;
            set => Options.InsertFinalNewLine = value;
        }
        #endregion
    }
}
