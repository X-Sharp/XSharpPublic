//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using EditorConfig.Core;
using Microsoft.VisualStudio.Text;
using System.Text;
using XSharp.Settings;

namespace XSharp.LanguageService
{
    internal partial class EditorConfigReader
    {
        static EditorConfigParser configParser;
        private const string KEYWORDCASE = "keyword_case";
        private const string IDENTIFIERCASE = "identifier_case";
        private const string INDENT_NAMESPACE = "indent_namespace";
        private const string INDENT_ENTITY_CONTENT = "indent_entity_content";
        private const string INDENT_TYPE_MEMBERS  = "indent_type_members";
        private const string INDENT_TYPE_FIELDS = "indent_type_fields";
        private const string INDENT_BLOCK_CONTENT = "indent_block_content";
        private const string INDENT_STATEMENTS = "indent_statements";
        private const string INDENT_CASE_CONTENT = "indent_case_content";
        private const string INDENT_CASE_LABEL = "indent_case_label";
        private const string INDENT_PREPROCESSOR = "indent_preprocessor";
        private const string INDENT_CONTINUED_LINES = "indent_continued_lines";
        private const string UDCCASE = "udc_case";
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
                settings.IdentifierCase = GetSetting(configuration, settings.IdentifierCase, IDENTIFIERCASE);
                settings.UDCKeywordCase = GetSetting(configuration, settings.UDCKeywordCase, UDCCASE);
                // old syntax
                settings.IndentTypeMembers = GetSetting(configuration, settings.IndentTypeMembers, INDENT_ENTITY_CONTENT);
                // new syntax
                settings.IndentTypeMembers = GetSetting(configuration, settings.IndentTypeMembers, INDENT_TYPE_MEMBERS);
                settings.IndentTypeFields = GetSetting(configuration, settings.IndentTypeFields, INDENT_TYPE_FIELDS);
                // old syntax
                settings.IndentStatements = GetSetting(configuration, settings.IndentStatements, INDENT_BLOCK_CONTENT);
                // new syntax
                settings.IndentStatements = GetSetting(configuration, settings.IndentStatements, INDENT_STATEMENTS);
                settings.IndentCaseContent = GetSetting(configuration, settings.IndentCaseContent, INDENT_CASE_CONTENT);
                settings.IndentCaseLabel = GetSetting(configuration, settings.IndentCaseLabel, INDENT_CASE_LABEL);
                settings.IndentContinuedLines = GetSetting(configuration, settings.IndentContinuedLines, INDENT_CONTINUED_LINES);
                settings.IndentPreprocessorLines = GetSetting(configuration, settings.IndentPreprocessorLines, INDENT_PREPROCESSOR);
                settings.IndentNamespace = GetSetting(configuration, settings.IndentNamespace, INDENT_NAMESPACE);

            }
            if (buffer.Properties.ContainsProperty(typeof(SourceCodeEditorSettings)))
                buffer.Properties.RemoveProperty(typeof(SourceCodeEditorSettings));
            buffer.Properties.AddProperty(typeof(SourceCodeEditorSettings), settings);
            return settings;
        }

        string SaveSettings(SourceCodeEditorSettings settings)
        {
            var sb = new StringBuilder();
            switch (settings.KeywordCase)
            {
                case KeywordCase.None:
                    sb.AppendLine(KEYWORDCASE + " = none"  );
                    break;
                case KeywordCase.Upper:
                    sb.AppendLine(KEYWORDCASE + " = "+ UPPER);
                    break;
                case KeywordCase.Lower:
                    sb.AppendLine(KEYWORDCASE + " = "+LOWER );
                    break;
                case KeywordCase.Title:
                    sb.AppendLine(KEYWORDCASE + " = " + TITLE);
                    break;
            }
            sb.AppendLine("indent_style = " + (settings.TabsAsSpaces ? "space" : "tab"));
            sb.AppendLine("tab_width = " + settings.TabSize.ToString());
            sb.AppendLine("indent_size = " + settings.IndentSize.ToString());
            sb.AppendLine("trim_trailing_whitespace = " + settings.TrimTrailingWhiteSpace.ToString());
            sb.AppendLine("insert_final_newline = " + settings.InsertFinalNewline.ToString());
            sb.AppendLine(IDENTIFIERCASE + " = " + settings.IdentifierCase.ToString());
            sb.AppendLine(UDCCASE + " = " + settings.UDCKeywordCase.ToString());
            sb.AppendLine(INDENT_TYPE_MEMBERS + " = " + settings.IndentTypeMembers.ToString());
            sb.AppendLine(INDENT_TYPE_FIELDS + " = " + settings.IndentTypeFields.ToString());
            sb.AppendLine(INDENT_STATEMENTS + " = " + settings.IndentStatements.ToString());
            sb.AppendLine(INDENT_CASE_CONTENT + " = " + settings.IndentCaseContent.ToString());
            sb.AppendLine(INDENT_CASE_LABEL + " = " + settings.IndentCaseLabel.ToString());
            sb.AppendLine(INDENT_CONTINUED_LINES + " = " + settings.IndentContinuedLines.ToString());
            sb.AppendLine(INDENT_PREPROCESSOR + " = " + settings.IndentPreprocessorLines.ToString());
            sb.AppendLine(INDENT_NAMESPACE + " = " + settings.IndentNamespace.ToString());
            return sb.ToString();
        }


        static bool GetSetting(FileConfiguration configuration, bool oldValue, string name)
        {
            if (configuration.Properties.ContainsKey(name))
            {
                var temp = configuration.Properties[name].ToLower();
                return temp == TRUE;
            }
            return oldValue;
        }

    }
}
