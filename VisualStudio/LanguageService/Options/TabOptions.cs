//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.TextManager.Interop;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    public class TabOptions : OptionsBase
    {
        public TabOptions()
        {
            IndentStyle = (int)vsIndentStyle.vsIndentStyleSmart;
            TabSize = IndentSize = 3;
            HideAdvancedMembers = true;
            TabsAsSpaces = true;
        }
        public int IndentStyle { get; set; }
        public bool HideAdvancedMembers { get; set; }
        public int TabSize { get; set; }
        public int IndentSize { get; set; }
        public bool TabsAsSpaces { get; set; }

        public override void WriteToSettings()
        {
            XEditorSettings.IndentStyle = IndentStyle;
            XEditorSettings.HideAdvancedMembers = HideAdvancedMembers;
            XEditorSettings.TabSize = TabSize;
            XEditorSettings.IndentSize = IndentSize;
            XEditorSettings.TabsAsSpaces = TabsAsSpaces;
        }
    }
}
