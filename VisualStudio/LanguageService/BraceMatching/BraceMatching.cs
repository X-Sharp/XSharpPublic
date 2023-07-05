//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [TagType(typeof(TextMarkerTag))]

    // we now have 2 separate taggers
    // 1 that matches the operator tokens which is implemented inside the toolkit
    // 2 tagger for keyword open/close pairs. This needs revisiting to use info
    // collected in the classifier
    internal sealed class BraceMatchingProvider : BraceMatchingBase
    {
        public override string TextMarketTagType => ColorizerConstants.BraceFormatDefinition;
        public override Dictionary<char, char> BraceList
        {
            get
            {
                var result = new Dictionary<char, char>()
                {
                    { '{', '}' },
                    { '(', ')' },
                    { '[', ']' },
                };
                if (XEditorSettings.DisableBraceMatching )
                {
                    result.Clear();
                }
                return result;
            }
        }

    }

}
