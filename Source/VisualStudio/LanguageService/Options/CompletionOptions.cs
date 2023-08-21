//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.Settings;

namespace XSharp.LanguageService
{
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
}
