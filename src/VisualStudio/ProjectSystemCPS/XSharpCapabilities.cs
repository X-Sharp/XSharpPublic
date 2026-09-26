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
    }
}
