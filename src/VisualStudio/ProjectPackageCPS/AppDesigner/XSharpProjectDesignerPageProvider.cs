//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
