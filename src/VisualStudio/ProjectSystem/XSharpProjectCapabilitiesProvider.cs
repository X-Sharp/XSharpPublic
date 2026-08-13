//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.ComponentModel.Composition;

using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Exports the set of CPS capabilities that every XSharp project advertises.
    /// CPS uses capabilities to decide which MEF components apply to a given project.
    /// </summary>
    [Export(typeof(IProjectCapabilitiesProvider))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpProjectCapabilitiesProvider : IProjectCapabilitiesProvider
    {
        private static readonly ImmutableHashSet<string> s_capabilities =
            ImmutableHashSet.Create(
                XSharpProjectCapabilities.XSharp,
                XSharpProjectCapabilities.Managed,
                XSharpProjectCapabilities.CSharp,
                XSharpProjectCapabilities.ProjectReferences,
                XSharpProjectCapabilities.AssemblyReferences,
                XSharpProjectCapabilities.PackageReferences,
                XSharpProjectCapabilities.OutputGroups,
                XSharpProjectCapabilities.AllTargetOutputGroups,
                XSharpProjectCapabilities.MultipleTargetFrameworks,
                XSharpProjectCapabilities.BuildEvents,
                XSharpProjectCapabilities.SourceControlled);

        /// <inheritdoc />
        public IImmutableSet<string> GetProjectCapabilities(IProjectTree projectTree) =>
            s_capabilities;
    }
}
