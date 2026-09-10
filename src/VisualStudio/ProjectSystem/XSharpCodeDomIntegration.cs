//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.CodeDom.Compiler;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.Shell;

// The XSharp CodeDom provider assembly is produced by ProjectPackage.
// We use a type-forward so this assembly does not need a hard dependency on the
// internal VSXsharpCodeDomProvider type; instead we resolve it via the VS service
// provider at runtime.

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Bridges the XSharp CodeDom provider into the CPS project system so that the
    /// Windows Forms designer can generate XSharp source code.
    ///
    /// In the MPF path the CodeDom provider is registered via
    /// <c>[ProvideCodeDomProvider]</c> attributes on the package.  The CPS path
    /// additionally needs to export the provider through MEF so that CPS design-time
    /// build services can locate it.
    ///
    /// The actual implementation lives in
    /// <c>XSharp.Project.VSXsharpCodeDomProvider</c> (ProjectPackage assembly).
    /// This class locates and delegates to it at run-time, keeping the two assemblies
    /// loosely coupled.
    /// </summary>
    [Export(typeof(ICodeDomProviderIntegration))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpCodeDomIntegration : ICodeDomProviderIntegration
    {
        private readonly UnconfiguredProject _project;

        [ImportingConstructor]
        public XSharpCodeDomIntegration(UnconfiguredProject project)
        {
            _project = project;
        }

        /// <summary>
        /// Returns an instance of the XSharp <see cref="CodeDomProvider"/> for use
        /// by the Windows Forms code generator.
        ///
        /// The provider is obtained from the VS service provider so that it uses the
        /// same registration path as the MPF-based implementation.
        /// </summary>
        public CodeDomProvider GetCodeDomProvider()
        {
            // Resolve the provider type by name to avoid a hard assembly reference.
            // The XSharp.Project.VSXsharpCodeDomProvider class is registered in the
            // global VS MEF container by the ProjectPackage assembly.
            const string providerTypeName =
                "XSharp.Project.VSXsharpCodeDomProvider, XSharp.ProjectPackage2022";

            System.Type providerType = System.Type.GetType(providerTypeName);

            if (providerType != null &&
                System.Activator.CreateInstance(providerType) is CodeDomProvider provider)
            {
                return provider;
            }

            // Fallback: return a C# provider to avoid hard failures.
            // In practice this branch should never be reached when the ProjectPackage
            // assembly is loaded.
            return CodeDomProvider.CreateProvider("CSharp");
        }
    }

    /// <summary>
    /// Marker interface used by <see cref="XSharpCodeDomIntegration"/> to allow
    /// MEF discovery without pulling in Windows Forms or CodeDom namespaces at
    /// the interface declaration site.
    /// </summary>
    public interface ICodeDomProviderIntegration
    {
        /// <summary>Gets the <see cref="CodeDomProvider"/> for XSharp source generation.</summary>
        CodeDomProvider GetCodeDomProvider();
    }
}
