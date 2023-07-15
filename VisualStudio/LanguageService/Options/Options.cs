//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;
using Newtonsoft.Json;
using XSharp.LanguageService.OptionsPages;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    public static class LanguagePreferenceExtensions
    {
        public static void GetSettings(this IVsTextManager4 txtManager, TabOptions options)
        {
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = XSharpConstants.guidLanguageService;
            int result = VSConstants.S_FALSE;
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = txtManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            languagePreferences[0].IndentStyle = (vsIndentStyle)options.IndentStyle;
            languagePreferences[0].fHideAdvancedAutoListMembers = (uint)(options.HideAdvancedMembers ? 1 : 0);
            languagePreferences[0].uTabSize = (uint)options.TabSize;
            languagePreferences[0].uIndentSize = (uint)options.IndentSize;
            languagePreferences[0].fInsertTabs = (uint)(options.TabsAsSpaces ? 0 : 1);
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = txtManager.SetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            return result == VSConstants.S_OK;

        }
    }

    public class LanguageServiceOptions : OptionsBase
    {
        #region Properties
        public IntellisenseOptions IntellisenseOptions { get; set; } 
        public FormattingOptions FormattingOptions { get; set; } 
        public OtherOptions OtherOptions { get; set; } 
        public CompletionOptions CompletionOptions { get; set; } 
        public IndentingOptions IndentingOptions { get; set; } 
        public GeneratorOptions GeneratorOptions { get; set; } 
        public TabOptions TabOptions { get; set; }
        [JsonIgnore]
        public LogOptions LogOptions { get; set; }

        #endregion
        public LanguageServiceOptions()
        {
            TabOptions = new TabOptions();
            IntellisenseOptions = new IntellisenseOptions();
            FormattingOptions = new FormattingOptions();
            OtherOptions = new OtherOptions();
            CompletionOptions = new CompletionOptions();
            IndentingOptions = new IndentingOptions();
            GeneratorOptions = new GeneratorOptions();
            LogOptions = LogOptions.Instance;
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
            LogOptions.WriteToSettings();
        }
        public bool Save()
        {
            try
            {
                var str = JsonConvert.SerializeObject(this, Formatting.Indented);
                CreatePath();
                var sFile = System.IO.Path.Combine(GetPath(), SETTINGS);
                System.IO.File.WriteAllText(sFile, str);
                return true;
            }
            catch
            {
                return false;
            }
        }
        private const string SETTINGS = "EditorSettings.json";
        public static LanguageServiceOptions Load()
        {
            try
            {
                var sFile = System.IO.Path.Combine(GetPath(), SETTINGS);
                if (System.IO.File.Exists(sFile))
                {
                    var str = System.IO.File.ReadAllText(sFile);
                    return JsonConvert.DeserializeObject<LanguageServiceOptions>(str);
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
  
}
