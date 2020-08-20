//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using XSharp.ProjectSystem;
[assembly: ProvideDiffSupportedContentType(".xsproj", "")]
[assembly: ProvideEditorFactoryMapping("{f6819a78-a205-47b5-be1c-675b3c7f0b8e}", ".xsproj")] // Use the XML editor
namespace XSharp.ProjectSystem
{
    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// This package is required if you want to define adds custom commands (ctmenu)
    /// or localized resources for the strings that appear in the New Project and Open Project dialogs.
    /// Creating project extensions or project types does not actually require a VSPackage.
    /// </remarks>
    [InstalledProductRegistration("#110", "#112", XSharp.Constants.ProductVersion, IconResourceID = 400)]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading =true)]
    [Description("XSharp project type based on CPS")]
    [Guid(XSharpVsPackage.PackageGuid)]
    public sealed class XSharpVsPackage : AsyncPackage
    {
        /// <summary>
        /// The GUID for this package.
        /// </summary>
        public const string PackageGuid = "D79AA3F8-FE25-42C0-854D-CF22EBE83818";

        /// <summary>
        /// The GUID for this project type.  It is unique with the project file extension and
        /// appears under the VS registry hive's Projects key.
        /// </summary>
        public const string ProjectTypeGuid = "286E78A2-2FBA-47EA-A12B-EAEC3D38BC7C";
        public const string LegacyXSharpGuid = "AA6C8D78-22FF-423A-9C7C-5F2393824E04";
        public const string ProjectSelectorGuid = "5F81BD32-54BC-4245-9EBA-FA00F3DA9A35";
        /// <summary>
        /// The file extension of this project type.  No preceding period.
        /// </summary>
        public const string ProjectExtension = "xsproj";

        /// <summary>
        /// The default namespace this project compiles with, so that manifest
        /// resource names can be calculated for embedded resources.
        /// </summary>
        internal const string DefaultNamespace = "XSharp";

        //[Import(typeof(VisualStudioWorkspace))]
        //public VisualStudioWorkspace myWorkspace { get; set; }
    }
}
