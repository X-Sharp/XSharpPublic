//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Build;

namespace XSharp.ProjectSystem.Evaluation
{
    /// <summary>
    /// Adds the global property XSharpCpsProjectSystem=true to the MSBuild evaluation of projects that are
    /// loaded by the X# CPS project type.
    /// </summary>
    /// <remarks>
    /// The X# targets (XSharp.CurrentVersion.targets) only import XSharp.DesignTime.targets, and with it the
    /// managed design-time targets of Visual Studio, when this property is set. Projects loaded by the MPFproj
    /// project system and command line builds are therefore not affected.
    /// The provider applies to the initial capability of the project type, so it is active for the first evaluation.
    /// </remarks>
    [Export(typeof(IProjectGlobalPropertiesProvider))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    internal sealed class XSharpCpsGlobalPropertiesProvider : StaticGlobalPropertiesProviderBase
    {
        internal const string PropertyName = "XSharpCpsProjectSystem";

        private static readonly Task<IImmutableDictionary<string, string>> properties =
            Task.FromResult<IImmutableDictionary<string, string>>(
                ImmutableDictionary.Create<string, string>(StringComparer.OrdinalIgnoreCase).Add(PropertyName, "true"));

        [ImportingConstructor]
        public XSharpCpsGlobalPropertiesProvider(IProjectService projectService)
            : base(projectService.Services)
        {
        }

        public override Task<IImmutableDictionary<string, string>> GetGlobalPropertiesAsync(CancellationToken cancellationToken)
        {
            return properties;
        }
    }
}
