//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio;
using XSharp.VisualStudio.ProjectSystem;
using XSharp;
using Microsoft.VisualStudio.ProjectSystem.VS;


// copied from the settings for the managed F# project system
[assembly: ProjectTypeRegistration(
  projectTypeGuid: XSharpConstants.guidCpsProjectTypeString,
    displayName: "#1",                      // "XSharp"
    displayProjectFileExtensions: "#2",     // "XSharp Project Files (*.xsproj);*.xsproj"
    defaultProjectExtension: "xsproj",
    language: XSharpConstants.LanguageName,
    resourcePackageGuid: XSharpConstants.guidCpsProjectTypeString,
    Capabilities = ProjectTypeCapabilities.XSharp,
    DisableAsynchronousProjectTreeLoad = true,
    PossibleProjectExtensions = "xsproj",
    NewProjectRequireNewFolderVsTemplate = true,
    SupportsCodespaces = true,
    SupportsSolutionChangeWithoutReload = true)]

[assembly: ProvideDiffSupportedContentType(".xsproj", "")]   // Empty string because content type is not important, we just want to tell the diff that the file type is supported
[assembly: ProvideEditorFactoryMapping("{f6819a78-a205-47b5-be1c-675b3c7f0b8e}", ".xsproj")] // Use the XML editor
