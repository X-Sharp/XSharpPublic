//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Runtime.InteropServices;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharp.Settings;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Factory for creating our editor. Base class is from the Community Toolkit
    /// </summary>
    [Guid(XSharpConstants.EditorFactoryGuidString)]
    [ProvideView(LogicalView.Code, "")]
    public class XSharpEditorFactory : LanguageBase
    {
        public XSharpEditorFactory(object site) : base(site)
        {
        }
        public override string Name => Constants.LanguageName;
        public override string[] FileExtensions { get; } = new[] { ".prg", ".xh", ".ppo", ".ch", ".xh" };

        public override void SetDefaultPreferences(LanguagePreferences preferences)
        {
           
        }
    }
}
