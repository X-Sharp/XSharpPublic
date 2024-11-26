// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

namespace Microsoft.VisualStudio.ProjectSystem.SpecialFileProviders.XSharp
{
    /// <summary>
    ///     Provides a <see cref="ISpecialFileProvider"/> that handles the WPF Application Definition file,
    ///     typically called "App.xaml" in X# projects.
    /// </summary>
    [ExportSpecialFileProvider(SpecialFiles.AppXaml)]
    [AppliesTo(ProjectCapability.XSharp)]
    internal class XSharpAppXamlSpecialFileProvider : AbstractAppXamlSpecialFileProvider
    {
        [ImportingConstructor]
        public XSharpAppXamlSpecialFileProvider(IPhysicalProjectTree projectTree)
            : base("App.xaml", projectTree)
        {
        }

        protected override Task CreateFileAsync(string path)
        {
            // We don't have a template for X# for App.xaml, deliberately 
            // throw NotImplementedException (which gets mapped to E_NOTIMPL) to
            // indicate we don't support this.
            throw new NotImplementedException();
        }
    }
}
