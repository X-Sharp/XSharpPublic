//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.Settings;

namespace XSharp.LanguageService
{
    public class FormattingOptions : OptionsBase
    {
        #region Properties
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public KeywordCase KeywordCase { get; set; }
        public bool IdentifierCase { get; set; }
        public bool UdcCase { get; set; }
        public bool TrimTrailingWhiteSpace { get; set; }
        public bool InsertFinalNewLine { get; set; }
        #endregion
        public FormattingOptions()
        {
            KeywordCase = KeywordCase.Upper;
            IdentifierCase = true;
            TrimTrailingWhiteSpace = true;
            InsertFinalNewLine = false;
        }
        public override void WriteToSettings()
        {
            XEditorSettings.IdentifierCase = IdentifierCase;
            XEditorSettings.UDCKeywordCase = UdcCase;
            XEditorSettings.TrimTrailingWhiteSpace = TrimTrailingWhiteSpace;
            XEditorSettings.InsertFinalNewline = InsertFinalNewLine;
            XEditorSettings.KeywordCase = KeywordCase;
        }
    }
}
