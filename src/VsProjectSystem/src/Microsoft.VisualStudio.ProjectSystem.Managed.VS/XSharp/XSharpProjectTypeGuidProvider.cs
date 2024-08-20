// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.
using System.Runtime.CompilerServices;

[assembly: IgnoresAccessChecksTo("Microsoft.VisualStudio.LanguageServices")]
[assembly: IgnoresAccessChecksTo("Microsoft.CodeAnalysis")]
[assembly: IgnoresAccessChecksTo("Microsoft.CodeAnalysis.CSharp")]

namespace Microsoft.VisualStudio.ProjectSystem.VS.XSharp
{
    /// <summary>
    ///     Provides the Visual Basic implementation of <see cref="IItemTypeGuidProvider"/>.
    /// </summary>
    [Export(typeof(IItemTypeGuidProvider))]
    [AppliesTo(ProjectCapability.XSharp)]
    internal class XSharpProjectTypeGuidProvider : IItemTypeGuidProvider
    {
        [ImportingConstructor]
        public XSharpProjectTypeGuidProvider()
        {
        }

        public Guid ProjectTypeGuid
        {
            get { return ProjectType.LegacyXSharpGuid; }
        }
    }
}
