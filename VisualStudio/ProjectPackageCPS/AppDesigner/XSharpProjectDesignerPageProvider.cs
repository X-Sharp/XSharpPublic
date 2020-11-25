// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading.Tasks;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS.Properties;
using Microsoft.VisualStudio.Buffers.PooledObjects;

namespace XSharp.ProjectSystem
{
    /// <summary>
    ///     Provides project designer property pages.
    /// </summary>
    [Export(typeof(IVsProjectDesignerPageProvider))]
    [AppliesTo(ProjectCapability.XSharpAppDesigner)]
    internal class XSharpProjectDesignerPageProvider : IVsProjectDesignerPageProvider
    {

        [ImportingConstructor]
        internal XSharpProjectDesignerPageProvider()
        {
        }

        public Task<IReadOnlyCollection<IPageMetadata>> GetPagesAsync()
        {
            var builder = PooledArray<IPageMetadata>.GetInstance(capacity: 6);

            builder.Add(XSharpProjectDesignerPage.General);
            builder.Add(XSharpProjectDesignerPage.Language);
            builder.Add(XSharpProjectDesignerPage.Dialect);
            builder.Add(XSharpProjectDesignerPage.Build);
            builder.Add(XSharpProjectDesignerPage.BuildEvents);
            builder.Add(XSharpProjectDesignerPage.Debug);

            return Task.FromResult<IReadOnlyCollection<IPageMetadata>>(builder.ToImmutableAndFree());
        }
    }
}
