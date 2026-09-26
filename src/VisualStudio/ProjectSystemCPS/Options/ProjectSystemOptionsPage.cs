//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel;
using Microsoft.VisualStudio.Shell;

namespace XSharp.ProjectSystem.Options
{
    /// <summary>
    /// Tools > Options > X# > Project System.
    /// </summary>
    internal sealed class ProjectSystemOptionsPage : DialogPage
    {
        internal const string CategoryName = "X#";
        internal const string PageName = "Project System";

        [Category("Project System")]
        [DisplayName("Load SDK-style projects with the new project system (CPS)")]
        [Description("When True, SDK-style X# projects (<Project Sdk=\"...\">) are loaded by the new CPS based project system; " +
                     "legacy X# projects are always loaded by the classic project system. " +
                     "The change applies to projects that are loaded (or reloaded) after the change.")]
        [DefaultValue(true)]
        public bool UseCpsForSdkProjects { get; set; } = true;
    }
}
