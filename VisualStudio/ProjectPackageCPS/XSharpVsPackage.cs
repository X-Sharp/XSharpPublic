/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/
using XSharp;
namespace XSharpLanguage
{
    using System;
    using System.ComponentModel;
    using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Shell;
    using System.ComponentModel.Composition;
    using Microsoft.VisualStudio.LanguageServices;
    using Microsoft.VisualStudio.Shell.Interop;
    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// This package is required if you want to define adds custom commands (ctmenu)
    /// or localized resources for the strings that appear in the New Project and Open Project dialogs.
    /// Creating project extensions or project types does not actually require a VSPackage.
    /// </remarks>
    [InstalledProductRegistration("#110", "#112", XSharp.Constants.Version, IconResourceID = 400)]
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [Description("XSharp project type based on CPS")]
    [Guid(XSharpVsPackage.PackageGuid)]
    public sealed class XSharpVsPackage : Package
    {
        /// <summary>
        /// The GUID for this package.
        /// </summary>
        public const string PackageGuid = "E299FB7B-D273-4678-9ACF-B37B4AF04A12";

        /// <summary>
        /// The GUID for this project type.  It is unique with the project file extension and
        /// appears under the VS registry hive's Projects key.
        /// </summary>
        public const string ProjectTypeGuid = "AA6C8D78-22FF-423A-9C7C-5F2393824E04";

        /// <summary>
        /// The file extension of this project type.  No preceding period.
        /// </summary>
        public const string ProjectExtension = "xsproj";

        /// <summary>
        /// The default namespace this project compiles with, so that manifest
        /// resource names can be calculated for embedded resources.
        /// </summary>
        internal const string DefaultNamespace = "XSharp";

        [Import(typeof(VisualStudioWorkspace))]
        public VisualStudioWorkspace myWorkspace { get; set; }
    }
}
