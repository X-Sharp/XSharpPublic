//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.ProjectSystem
{
    /// <summary>
    /// Project capabilities used to scope the X# CPS MEF parts with [AppliesTo(...)].
    /// </summary>
    internal static class XSharpCapabilities
    {
        /// <summary>
        /// Capability that marks a project as an X# project. Gate for all X# specific MEF extensions.
        /// Declared by the X# MSBuild targets, so it is also present for projects loaded by the MPFproj project system.
        /// </summary>
        public const string XSharp = "XSharp";

        /// <summary>
        /// Initial capability of the X# CPS project type (see <see cref="XSharpProjectSystemPackage"/>).
        /// CPS passes it to the project before the first MSBuild evaluation, so it is available to
        /// UnconfiguredProject scoped parts that influence that evaluation. Never declared in MSBuild.
        /// </summary>
        public const string XSharpCps = "XSharpCps";

        /// <summary>
        /// Initial capabilities of the X# CPS project type (ProjectTypeRegistration.Capabilities).
        /// Modelled on the registration of the C# project type of the managed project system, without
        /// LanguageService (X# has its own language service, see XSharp.DesignTime.targets), CSharp,
        /// SharedImports (VB) and EditAndContinue (not supported by the X# debugger integration).
        /// AppDesigner: "Properties" folder; OpenProjectFile: "Edit Project File";
        /// HandlesOwnReload: the project system reloads the project file itself after external changes;
        /// ProjectConfigurationsDeclaredDimensions: configurations/platforms/target frameworks from the project file;
        /// ProjectPropertiesEditor: the AppDesigner opens the new property editor (XAML rules). Also declared in
        /// XSharp.DesignTime.targets, but repeated here so that it does not depend on the installed X# targets.
        /// </summary>
        public const string ProjectTypeCapabilities =
            XSharpCps + "; AppDesigner; HandlesOwnReload; OpenProjectFile; PreserveFormatting; ProjectConfigurationsDeclaredDimensions; ProjectPropertiesEditor; .NET; UseProjectEvaluationCache";
    }
}
