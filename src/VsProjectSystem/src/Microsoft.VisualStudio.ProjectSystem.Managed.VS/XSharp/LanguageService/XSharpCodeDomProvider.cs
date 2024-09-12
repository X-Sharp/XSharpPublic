//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.CodeDom.Compiler;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Shell;

[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeModel")]

namespace Microsoft.VisualStudio.ProjectSystem.VS.LanguageServices.XSharp
{
    /// <summary>
    ///     Provides the X# <see cref="CodeDomProvider"/> for use by designers and code generators.
    /// </summary>
    /// <remarks>
    ///     This service is requested by <see cref="IVSMDCodeDomCreator.CreateCodeDomProvider(object, int)"/> and
    ///     returned by <see cref="IVSMDCodeDomProvider.CodeDomProvider"/>.
    /// </remarks>
    [ExportVsProfferedProjectService(typeof(CodeDomProvider))]
    [AppliesTo(ProjectCapability.XSharp)]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    internal class XSharpCodeProvider : XSharpModel.XSharpCodeDomProvider
    {
        [ImportingConstructor]
        public XSharpCodeProvider() : base()
        {

        }
    }
}
