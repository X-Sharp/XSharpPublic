using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;
using Newtonsoft.Json;
using System;
using System.ComponentModel;
using XSharpModel;

namespace XSharp.LanguageService
{
    public static class LanguagePreferenceExtensions
    {
        public static void GetSettings(this IVsTextManager4 txtManager, TabOptions options)
        {
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = XSharpConstants.guidLanguageService;
            int result = VSConstants.S_FALSE;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = txtManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            if (result == VSConstants.S_OK)
            {
                options.IndentStyle = (int)languagePreferences[0].IndentStyle;
                options.HideAdvancedMembers = languagePreferences[0].fHideAdvancedAutoListMembers != 0;
                options.TabSize = (int)languagePreferences[0].uTabSize;
                options.IndentSize = (int)languagePreferences[0].uIndentSize;
                options.TabsAsSpaces = languagePreferences[0].fInsertTabs == 0;
            }
        }
        public static bool SetSettings(this IVsTextManager4 txtManager, TabOptions options)
        {
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = XSharpConstants.guidLanguageService;
            int result = VSConstants.S_FALSE;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = txtManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            languagePreferences[0].IndentStyle = (vsIndentStyle)options.IndentStyle;
            languagePreferences[0].fHideAdvancedAutoListMembers = (uint)(options.HideAdvancedMembers ? 1 : 0);
            languagePreferences[0].uTabSize = (uint)options.TabSize;
            languagePreferences[0].uIndentSize = (uint)options.IndentSize;
            languagePreferences[0].fInsertTabs = (uint)(options.TabsAsSpaces ? 0 : 1);
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = txtManager.SetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            return result == VSConstants.S_OK;

        }
    }
    public abstract class OptionsBase
    {
        public abstract void WriteToSettings();

    }
    public class Options : OptionsBase
    {
        #region Properties
        public IntellisenseOptions IntellisenseOptions { get; set; } 
        public FormattingOptions FormattingOptions { get; set; } 
        public OtherOptions OtherOptions { get; set; } 
        public CompletionOptions CompletionOptions { get; set; } 
        public IndentingOptions IndentingOptions { get; set; } 
        public GeneratorOptions GeneratorOptions { get; set; } 
        public TabOptions TabOptions { get; set; }

        #endregion
        public Options()
        {
            TabOptions = new TabOptions();
            IntellisenseOptions = new IntellisenseOptions();
            FormattingOptions = new FormattingOptions();
            OtherOptions = new OtherOptions();
            CompletionOptions = new CompletionOptions();
            IndentingOptions = new IndentingOptions();
            GeneratorOptions = new GeneratorOptions();
        }

        public override void WriteToSettings()
        {
            IntellisenseOptions.WriteToSettings();
            FormattingOptions.WriteToSettings();
            GeneratorOptions.WriteToSettings();
            CompletionOptions.WriteToSettings();
            IndentingOptions.WriteToSettings();
            OtherOptions.WriteToSettings();
            TabOptions.WriteToSettings();
        }
        public static string GetPath()
        {
            var path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            path = System.IO.Path.Combine(path, "XSharp");
            return path;
        }
        public bool Save(string sFile)
        {
            try
            {
                var str = JsonConvert.SerializeObject(this, Formatting.Indented);
                sFile = System.IO.Path.Combine(GetPath(), sFile);
                System.IO.File.WriteAllText(sFile, str);
                return true;
            }
            catch
            {
                return false;
            }
        }
        public static Options Load(string sFile)
        {
            try
            {
                sFile = System.IO.Path.Combine(GetPath(), sFile);
                if (System.IO.File.Exists(sFile))
                {
                    var str = System.IO.File.ReadAllText(sFile);
                    return JsonConvert.DeserializeObject<Options>(str);
                }
            }
            catch
            {
            }
            return null;
        }

        public void WriteToRegistry()
        {
            // Persist in registry for CodeDomProvider code generation
            Constants.WriteSetting(Constants.RegistryKeywordCase, (int)XEditorSettings.KeywordCase);
            Constants.WriteSetting(Constants.RegistryPrivateKeyword, (int)XSettings.CodeGeneratorPrivateStyle);
            Constants.WriteSetting(Constants.RegistryPublicKeyword, (int)XSettings.CodeGeneratorPublicStyle);
            Constants.WriteSetting(Constants.RegistryUseTabs, XEditorSettings.TabsAsSpaces ? 0 : 1);
            Constants.WriteSetting(Constants.RegistryTabSize, XEditorSettings.TabSize);
            Constants.WriteSetting(Constants.RegistryIndentSize, XEditorSettings.IndentSize);
        }
    }
    public class IntellisenseOptions : OptionsBase
    {
        public const string DefaultCommitChars = "{}[]().,:;+-*/%&|^!~<>?@#\'\"\\";
        #region Properties
        public bool CompletionListTabs { get; set; }
        public String CommitChars { get; set; }
        public bool ExcludeMembersFromOtherFiles { get; set; }
        public bool KeywordsInAll { get; set; }
        public bool ShowAfterChar { get; set; }
        public XSharpModel.KeywordCase KeywordCase { get; set; }
        public bool IncludeFieldsInNavigationBars { get; set; }
        public bool SortNavigationBars { get; set; }
        public bool ShowMembersOfCurrentTypeOnly { get; set; }
        public bool DisableSyntaxColorization { get; set; }
        public bool DisableEntityParsing { get; set; }
        public bool DisableEditorDropdowns { get; set; }
        public bool DisableClassViewObjectView { get; set; }
        public bool DisableAssemblyReferences { get; set; }
        public bool DisableForeignProjectReferences { get; set; }
        public bool DisableXSharpProjectReferences { get; set; }
        public bool EnableOutputPane { get; set; }
        public bool EnableDatabaseLog { get; set; }
        public bool EnableParserLog { get; set; }
        public bool EnableCodeCompletionLog { get; set; }
        public bool EnableParameterLog { get; set; }
        public bool EnableBraceMatchLog { get; set; }
        public bool EnableQuickInfoLog { get; set; }
        public bool EnableTypelookupLog { get; set; }
        public bool EnableReferenceInfoLog { get; set; }
        #endregion
        public IntellisenseOptions()
        {
            CompletionListTabs = true;
            CommitChars = DefaultCommitChars;
            ExcludeMembersFromOtherFiles = true;
            KeywordsInAll = true;
            ShowAfterChar = true;
            KeywordCase = KeywordCase.Upper;
            IncludeFieldsInNavigationBars = true;
            SortNavigationBars = true;
            ShowMembersOfCurrentTypeOnly = false;
            DisableSyntaxColorization = false;
            DisableEntityParsing = false;
            DisableEditorDropdowns = false;
            DisableClassViewObjectView = false;
            DisableAssemblyReferences = false;
            DisableForeignProjectReferences = false;
            DisableXSharpProjectReferences = false;
            EnableOutputPane = false;
            EnableDatabaseLog = false;
            EnableParserLog = false;
            EnableCodeCompletionLog = false;
            EnableParameterLog = false;
            EnableBraceMatchLog = false;
            EnableQuickInfoLog = false;
            EnableTypelookupLog = false;
            EnableReferenceInfoLog = false;
        }

        public override void WriteToSettings()
        {
            XSettings.EnableOutputWindowLogging = EnableOutputPane;
            XSettings.EnableBraceMatchLog = EnableBraceMatchLog;
            XSettings.EnableCodeCompletionLog = EnableCodeCompletionLog;
            XSettings.EnableDatabaseLog = EnableDatabaseLog;
            XSettings.EnableParameterLog = EnableParameterLog;
            XSettings.EnableParseLog = EnableParserLog;
            XSettings.EnableQuickInfoLog = EnableQuickInfoLog;
            XSettings.EnableReferenceInfoLog = EnableReferenceInfoLog;
            XSettings.EnableTypelookupLog = EnableTypelookupLog;

            XSettings.DisableAssemblyReferences = DisableAssemblyReferences;
            XSettings.DisableClassViewObjectView = DisableClassViewObjectView;
            XSettings.DisableEditorDropDowns = DisableEditorDropdowns;
            XSettings.DisableEntityParsing = DisableEntityParsing;
            XSettings.DisableForeignProjectReferences = DisableForeignProjectReferences;
            XSettings.DisableXSharpProjectReferences = DisableXSharpProjectReferences;

            XEditorSettings.DisableSyntaxHighlighting = DisableSyntaxColorization;
            XEditorSettings.CompletionListTabs = CompletionListTabs;
            XEditorSettings.CommitChars = CommitChars;
            XEditorSettings.CompletionListAfterEachChar = false; // ShowAfterChar;
            XEditorSettings.KeywordsInAll = KeywordsInAll;
            XEditorSettings.NavigationSorted = SortNavigationBars;
            XEditorSettings.NavigationIncludeFields = IncludeFieldsInNavigationBars;
            XEditorSettings.NavigationMembersOfCurrentTypeOnly = ShowMembersOfCurrentTypeOnly;
            XEditorSettings.NavigationExcludeMembersFromOtherFiles = ExcludeMembersFromOtherFiles;

        }

    }
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
    public class OtherOptions : OptionsBase
    {
        #region Properties
        public bool AutoPairs { get; set; }
        public bool AutoOpen { get; set; }
        public bool ShowDividers { get; set; }
        public bool ShowSingleLineDividers { get; set; }
        public bool ShowXmlComments { get; set; }
        public bool LanguageServiceLogging { get; set; }
        public bool FormEditorMakeBackupFiles { get; set; }
        public bool EnableHighlightWord { get; set; }
        public bool EnableBraceMatching { get; set; }
        public bool EnableKeywordmatching { get; set; }
        public bool EnableLightBulbs { get; set; }
        public bool EnableQuickInfo { get; set; }
        public bool EnableParameterInfo { get; set; }
        public bool EnableRegions { get; set; }
        public bool EnableCodeCompletion { get; set; }

        #endregion
        public OtherOptions()
        {
            AutoPairs = true;
            AutoOpen = true;
            EnableHighlightWord = true;
            EnableBraceMatching = true;
            EnableKeywordmatching = true;
            EnableLightBulbs = true;
            EnableQuickInfo = true;
            EnableParameterInfo = true;
            EnableCodeCompletion = true;
            EnableRegions = true;
            ShowDividers = true;
            ShowSingleLineDividers = true;
            ShowXmlComments = true;
            LanguageServiceLogging = false;
            FormEditorMakeBackupFiles = true;
        }
        public override void WriteToSettings()
        {
            // Other
            XEditorSettings.ShowDividers = ShowDividers;
            XEditorSettings.CompletionAutoPairs = AutoPairs;
            XEditorSettings.ShowSingleLineDividers = ShowSingleLineDividers;
            XEditorSettings.DisableAutoOpen = !AutoOpen;
            XEditorSettings.DisableHighLightWord = !EnableHighlightWord;
            XEditorSettings.DisableBraceMatching = !EnableBraceMatching;
            XEditorSettings.DisableKeywordMatching = !EnableKeywordmatching;
            XEditorSettings.DisableCodeCompletion = !EnableCodeCompletion;
            XEditorSettings.DisableLightBulb = !EnableLightBulbs;
            XEditorSettings.DisableParameterInfo = !EnableParameterInfo;
            XEditorSettings.DisableQuickInfo = !EnableQuickInfo;
            XEditorSettings.DisableRegions = !EnableRegions;
            XSettings.EnableFileLogging = LanguageServiceLogging;

        }

    }
    public class CompletionOptions : OptionsBase
    {
        #region Properties
        public int CompleteNumChars { get; set; }
        public bool CompleteLocals { get; set; }
        public bool CompleteSelf { get; set; }
        public bool CompleteParent { get; set; }
        public bool CompleteNamespaces { get; set; }
        public bool CompleteTypes { get; set; }
        public bool CompleteKeywords { get; set; }
        public bool CompleteSnippets { get; set; }
        public bool CompleteGlobals { get; set; }
        public bool CompleteGlobalsP { get; set; }
        public bool CompleteGlobalsA { get; set; }
        public bool CompleteFunctions { get; set; }
        public bool CompleteFunctionsP { get; set; }
        public bool CompleteFunctionsA { get; set; }
        #endregion
        public CompletionOptions()
        {
            CompleteNumChars = 4;
            CompleteLocals = true;
            CompleteSelf = true;
            CompleteParent = true;
            CompleteNamespaces = true;
            CompleteTypes = true;
            CompleteKeywords = true;
            CompleteSnippets = true;
            CompleteGlobals = true;
            CompleteGlobalsA = true;
            CompleteGlobalsP = true;
            CompleteFunctions = true;
            CompleteFunctionsA = true;
            CompleteFunctionsP = true;
        }
        public override void WriteToSettings()
        {
            XEditorSettings.CompleteLocals = CompleteLocals;
            XEditorSettings.CompleteSelf = CompleteSelf;
            XEditorSettings.CompleteParent = CompleteParent;
            XEditorSettings.CompleteNamespaces = CompleteNamespaces;
            XEditorSettings.CompleteTypes = CompleteTypes;
            XEditorSettings.CompleteKeywords = CompleteKeywords;
            XEditorSettings.CompleteSnippets = CompleteSnippets;
            XEditorSettings.CompleteGlobals = CompleteGlobals;
            XEditorSettings.CompleteGlobalsP = CompleteGlobalsP;
            XEditorSettings.CompleteGlobalsA = CompleteGlobalsA;
            XEditorSettings.CompleteFunctions = CompleteFunctions;
            XEditorSettings.CompleteFunctionsP = CompleteFunctionsP;
            XEditorSettings.CompleteFunctionsA = CompleteFunctionsA;
            XEditorSettings.CompleteNumChars = CompleteNumChars;
        }
    }
    public class GeneratorOptions : OptionsBase
    {
        #region Properties
        public bool ShowXmlComments { get; set; }
        public int PublicStyle { get; set; }
        public int PrivateStyle { get; set; }
        #endregion
        public GeneratorOptions()
        {
            ShowXmlComments = true;
            PublicStyle = 0;
            PrivateStyle = 0;
        }
        public override void WriteToSettings()
        {
            XSettings.CodeGeneratorPrivateStyle = (PrivateStyle)PrivateStyle;
            XSettings.CodeGeneratorPublicStyle = (PublicStyle)PublicStyle;
            XSettings.CodeGeneratorShowXmlComments = ShowXmlComments;

        }
    }
    public class IndentingOptions : OptionsBase
    {
        #region Properties
        [DefaultValue(true)]
        public bool IndentEntityContent { get; set; }
        [DefaultValue(true)]
        public bool IndentFieldContent { get; set; }
        [DefaultValue(true)]
        public bool IndentBlockContent { get; set; }
        [DefaultValue(true)]
        public bool IndentCaseContent { get; set; }
        [DefaultValue(true)]
        public bool IndentMultiLines { get; set; }
        [DefaultValue(false)]

        public bool IndentCaseLabel { get; set; }
        [DefaultValue(false)]
        public bool IndentPreprocessorLines { get; set; }
        [DefaultValue(false)]
        public bool IndentNamespace { get; set; }
        #endregion
        public IndentingOptions()
        {
            IndentEntityContent = true; // class members
            IndentFieldContent = true;  // class fields
            IndentBlockContent = true;  // statements
            IndentCaseContent = true;   // statement block inside case
            IndentMultiLines = true;    // Multi line statements
            IndentCaseLabel = false;
            IndentPreprocessorLines = false;
            IndentNamespace = false;
        }
        public override void WriteToSettings()
        {
            XEditorSettings.IndentTypeMembers = IndentEntityContent;
            XEditorSettings.IndentTypeFields = IndentFieldContent;
            XEditorSettings.IndentStatements = IndentBlockContent;
            XEditorSettings.IndentCaseContent = IndentCaseContent;
            XEditorSettings.IndentCaseLabel = IndentCaseLabel;
            XEditorSettings.IndentContinuedLines = IndentMultiLines;
            XEditorSettings.IndentPreprocessorLines = IndentPreprocessorLines;
            XEditorSettings.IndentNamespace = IndentNamespace;
        }
    }
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
