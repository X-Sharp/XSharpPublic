// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using Microsoft.VisualStudio.Imaging;


namespace Microsoft.VisualStudio.ProjectSystem.Imaging.XSharp
{
    /// <summary>
    ///     Provides X# project images.
    /// </summary>
    [Export(typeof(IProjectImageProvider))]
    [AppliesTo(ProjectCapability.XSharp)]
    internal class XSharpProjectImageProvider : IProjectImageProvider
    {
        [ImportingConstructor]
        public XSharpProjectImageProvider()
        {
        }

        public ProjectImageMoniker? GetProjectImage(string key)
        {
            Requires.NotNullOrEmpty(key);

            return key switch
            {
                ProjectImageKey.ProjectRoot => XSProjectNode,
                ProjectImageKey.SharedItemsImportFile or ProjectImageKey.SharedProjectRoot => KnownProjectImageMonikers.SharedProject,
                _ => null
            };
        }
        public static ProjectImageMoniker XSProjectNode { get; } = KnownMonikers.PYProjectNode.ToProjectSystemType();
        //public static ProjectImageMoniker XSProjectNode { get; } = XSharpImagesMonikers.ProjectImage.ToProjectSystemType();
    }
}
