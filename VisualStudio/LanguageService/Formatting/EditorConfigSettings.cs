//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharpModel;
using EditorConfig.Core;
using System.Data.Common;

namespace XSharp.LanguageService
{
    internal sealed partial class CommandFilter
    {

        private const string KEYWORDCASE = "keyword_case";
        private const string IDENTIFIERCASE = "identifier_case";
        private const string UDCCASE = "udc_case";
        private const string ALIGNDOCASE = "align_do_case";
        private const string ALIGNMETHOD = "align_method";
        private const string TRUE = "true";
        private const string UPPER = "upper";
        private const string LOWER = "lower";
        private const string TITLE = "title";
        


        EditorConfig.Core.FileConfiguration _configuration = null;
        SourceCodeEditorSettings _settings = null;

        private void ReadSettings(string fileName)
        {
            var configParser = new EditorConfigParser();
            _configuration = configParser.Parse(fileName);
            _settings = new SourceCodeEditorSettings(); // this gets a copy of the current Tools/Options settings
            if (_configuration.Properties.Count > 0)
            {
                if (_configuration.IndentStyle.HasValue)
                    _settings.TabsAsSpaces = _configuration.IndentStyle.Value == IndentStyle.Space;
                if (_configuration.TabWidth.HasValue)
                    _settings.TabSize = _configuration.TabWidth.Value;
                if (_configuration.IndentSize != null)
                {
                    if (_configuration.IndentSize.NumberOfColumns.HasValue)
                        _settings.IndentSize = _configuration.IndentSize.NumberOfColumns.Value;
                    else if (_configuration.IndentSize.UseTabWidth)
                        _settings.IndentSize = _settings.TabSize;
                }
                if (_configuration.TrimTrailingWhitespace.HasValue)
                    _settings.TrimTrailingWhiteSpace = _configuration.TrimTrailingWhitespace.Value;
                if (_configuration.InsertFinalNewline.HasValue)
                    _settings.InsertFinalNewline = _configuration.InsertFinalNewline.Value;

                if (_configuration.Properties.ContainsKey(KEYWORDCASE))
                {
                    var temp = _configuration.Properties[KEYWORDCASE].ToLower();
                    switch (temp)
                    {
                        case UPPER:
                            _settings.KeywordCase = KeywordCase.Upper;
                            break;
                        case LOWER:
                            _settings.KeywordCase = KeywordCase.Lower;
                            break;
                        case TITLE:
                            _settings.KeywordCase = KeywordCase.Title;
                            break;
                        default:
                            _settings.KeywordCase = KeywordCase.None;
                            break;
                    }

                }
                if (_configuration.Properties.ContainsKey(IDENTIFIERCASE))
                {
                    var temp = _configuration.Properties[IDENTIFIERCASE].ToLower();
                    _settings.IdentifierCase = temp == TRUE;
                }
                if (_configuration.Properties.ContainsKey(UDCCASE))
                {
                    var temp = _configuration.Properties[UDCCASE].ToLower();
                    _settings.UDCKeywordCase = temp == TRUE;
                }
                if (_configuration.Properties.ContainsKey(ALIGNDOCASE))
                {
                    var temp = _configuration.Properties[ALIGNDOCASE].ToLower();
                    _settings.IdentifierCase = temp == TRUE;
                }
                if (_configuration.Properties.ContainsKey(ALIGNMETHOD))
                {
                    var temp = _configuration.Properties[ALIGNMETHOD].ToLower();
                    _settings.IdentifierCase = temp == TRUE;
                }
            }
            if (_buffer.Properties.ContainsProperty(typeof(SourceCodeEditorSettings)))
                _buffer.Properties.RemoveProperty(typeof(SourceCodeEditorSettings));
            _buffer.Properties.AddProperty(typeof(SourceCodeEditorSettings), _settings);
        }
    }
}
