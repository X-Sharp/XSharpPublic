// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using Microsoft.VisualStudio.Buffers.PooledObjects;

namespace Microsoft.VisualStudio.ProjectSystem.VS.Properties.XSharp
{
    /// <summary>
    ///     Provides project designer property pages.
    /// </summary>
    [Export(typeof(IVsProjectDesignerPageProvider))]
    [AppliesTo(ProjectCapability.XSharpAppDesigner)]
    internal class XSharpProjectDesignerPageProvider : IVsProjectDesignerPageProvider
    {
        private readonly IProjectCapabilitiesService _capabilities;

        [ImportingConstructor]
        internal XSharpProjectDesignerPageProvider(IProjectCapabilitiesService capabilities)
        {
            _capabilities = capabilities;
        }

        public Task<IReadOnlyCollection<IPageMetadata>> GetPagesAsync()
        {
            var builder = PooledArray<IPageMetadata>.GetInstance(capacity: 7);

            builder.Add(XSharpProjectDesignerPage.Application);
            builder.Add(XSharpProjectDesignerPage.Build);
            builder.Add(XSharpProjectDesignerPage.BuildEvents);

            if (_capabilities.Contains(ProjectCapability.Pack))
            {
                builder.Add(XSharpProjectDesignerPage.Package);
            }

            if (_capabilities.Contains(ProjectCapability.LaunchProfiles))
            {
                builder.Add(XSharpProjectDesignerPage.Debug);
            }

            builder.Add(XSharpProjectDesignerPage.Signing);
            builder.Add(XSharpProjectDesignerPage.CodeAnalysis);

            return Task.FromResult<IReadOnlyCollection<IPageMetadata>>(builder.ToImmutableAndFree());
        }
    }
}
