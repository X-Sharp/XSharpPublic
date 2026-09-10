//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Build;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Hooks into CPS multi-targeting support for XSharp SDK-style projects.
    ///
    /// When a project file contains <c>&lt;TargetFrameworks&gt;net48;net6.0-windows&lt;/TargetFrameworks&gt;</c>
    /// CPS automatically creates one <see cref="ConfiguredProject"/> per framework.
    /// This provider participates in that mechanism by advertising the list of
    /// active target frameworks so that VS can show the framework picker and build
    /// all targets on Rebuild.
    ///
    /// <b>This capability is VS2022+ / CPS only.</b>
    /// The MPF-based VS2019 path does not support multi-targeting.
    /// </summary>
    [Export(typeof(IActiveConfiguredProjectsDimensionProvider))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    [ExportMetadata("DimensionName", ConfigurationGeneral.TargetFrameworkProperty)]
    internal sealed class XSharpTargetFrameworkProvider
        : IActiveConfiguredProjectsDimensionProvider
    {
        private readonly UnconfiguredProject _unconfiguredProject;
        private readonly IProjectPropertiesProvider _propertiesProvider;

        [ImportingConstructor]
        public XSharpTargetFrameworkProvider(
            UnconfiguredProject unconfiguredProject,
            [Import(ExportContractNames.ProjectPropertyProviders.ProjectFile)]
            IProjectPropertiesProvider propertiesProvider)
        {
            _unconfiguredProject = unconfiguredProject;
            _propertiesProvider = propertiesProvider;
        }

        /// <inheritdoc />
        /// <remarks>
        /// Returns the set of target frameworks declared in the project.
        /// When only <c>&lt;TargetFramework&gt;</c> (singular) is set, a single entry is
        /// returned.  When <c>&lt;TargetFrameworks&gt;</c> (plural) is set, all entries
        /// from the semicolon-separated list are returned.
        ///
        /// CPS uses this list to create one <see cref="ConfiguredProject"/> slice per
        /// framework, enabling simultaneous multi-targeted builds.
        /// </remarks>
        public async Task<IImmutableSet<string>> GetDefaultValuesForDimensionsAsync(
            CancellationToken cancellationToken)
        {
            IProjectProperties props = _propertiesProvider.GetCommonProperties();

            // Prefer the plural TargetFrameworks property first.
            string multiTarget = await props.GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.TargetFrameworks).ConfigureAwait(false);

            if (!string.IsNullOrWhiteSpace(multiTarget))
            {
                var frameworks = multiTarget
                    .Split(';')
                    .Select(f => f.Trim())
                    .Where(f => !string.IsNullOrEmpty(f))
                    .ToImmutableHashSet(System.StringComparer.OrdinalIgnoreCase);

                if (frameworks.Count > 0)
                    return frameworks;
            }

            // Fall back to the singular TargetFramework.
            string singleTarget = await props.GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.TargetFramework).ConfigureAwait(false);

            if (!string.IsNullOrWhiteSpace(singleTarget))
                return ImmutableHashSet.Create(
                    System.StringComparer.OrdinalIgnoreCase, singleTarget.Trim());

            // No framework declared — return the default.
            return ImmutableHashSet.Create(
                System.StringComparer.OrdinalIgnoreCase, "net48");
        }

        /// <inheritdoc />
        public string DimensionName => ConfigurationGeneral.TargetFrameworkProperty;

        /// <inheritdoc />
        public bool ShouldBeVisibleInConfigurationUI => true;
    }
}
