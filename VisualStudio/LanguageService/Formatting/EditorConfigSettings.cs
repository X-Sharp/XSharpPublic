//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharpModel;
using System.Data.Common;
using EditorConfig.Core;
using Microsoft.VisualStudio.Text;

namespace XSharp.LanguageService
{
    internal partial class EditorConfigReader
    {
        static EditorConfigParser configParser;
        private const string KEYWORDCASE = "keyword_case";
        private const string IDENTIFIERCASE = "identifier_case";
        private const string UDCCASE = "udc_case";
        private const string ALIGNDOCASE = "align_do_case";
        private const string ALIGNMETHOD = "align_method";
        private const string TRUE = "true";
        private const string UPPER = "upper";
        private const string LOWER = "lower";
        private const string TITLE = "title";

        static EditorConfigReader()
        {
            configParser = new EditorConfigParser();
        }
        internal static SourceCodeEditorSettings ReadSettings(ITextBuffer buffer, string fileName)
        {
            
            var configuration = configParser.Parse(fileName);
            var settings = new SourceCodeEditorSettings(); // this gets a copy of the current Tools/Options settings
            if (configuration.Properties.Count > 0)
            {
                if (configuration.IndentStyle.HasValue)
                    settings.TabsAsSpaces = configuration.IndentStyle.Value == IndentStyle.Space;
                if (configuration.TabWidth.HasValue)
                    settings.TabSize = configuration.TabWidth.Value;
                if (configuration.IndentSize != null)
                {
                    if (configuration.IndentSize.NumberOfColumns.HasValue)
                        settings.IndentSize = configuration.IndentSize.NumberOfColumns.Value;
                    else if (configuration.IndentSize.UseTabWidth)
                        settings.IndentSize = settings.TabSize;
                }
                if (configuration.TrimTrailingWhitespace.HasValue)
                    settings.TrimTrailingWhiteSpace = configuration.TrimTrailingWhitespace.Value;
                if (configuration.InsertFinalNewline.HasValue)
                    settings.InsertFinalNewline = configuration.InsertFinalNewline.Value;

                if (configuration.Properties.ContainsKey(KEYWORDCASE))
                {
                    var temp = configuration.Properties[KEYWORDCASE].ToLower();
                    switch (temp)
                    {
                        case UPPER:
                            settings.KeywordCase = KeywordCase.Upper;
                            break;
                        case LOWER:
                            settings.KeywordCase = KeywordCase.Lower;
                            break;
                        case TITLE:
                            settings.KeywordCase = KeywordCase.Title;
                            break;
                        default:
                            settings.KeywordCase = KeywordCase.None;
                            break;
                    }

                }
                if (configuration.Properties.ContainsKey(IDENTIFIERCASE))
                {
                    var temp = configuration.Properties[IDENTIFIERCASE].ToLower();
                    settings.IdentifierCase = temp == TRUE;
                }
                if (configuration.Properties.ContainsKey(UDCCASE))
                {
                    var temp = configuration.Properties[UDCCASE].ToLower();
                    settings.UDCKeywordCase = temp == TRUE;
                }
                if (configuration.Properties.ContainsKey(ALIGNDOCASE))
                {
                    var temp = configuration.Properties[ALIGNDOCASE].ToLower();
                    settings.IdentifierCase = temp == TRUE;
                }
                if (configuration.Properties.ContainsKey(ALIGNMETHOD))
                {
                    var temp = configuration.Properties[ALIGNMETHOD].ToLower();
                    settings.IdentifierCase = temp == TRUE;
                }
            }
            if (buffer.Properties.ContainsProperty(typeof(SourceCodeEditorSettings)))
                buffer.Properties.RemoveProperty(typeof(SourceCodeEditorSettings));
            buffer.Properties.AddProperty(typeof(SourceCodeEditorSettings), settings);
            return settings;
        }

    }
}
