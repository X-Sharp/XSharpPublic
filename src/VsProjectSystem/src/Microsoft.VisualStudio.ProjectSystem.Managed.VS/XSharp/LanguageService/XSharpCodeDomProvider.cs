// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System.CodeDom;
using System.CodeDom.Compiler;
using System.Reflection;
using System.Runtime.InteropServices;
using Microsoft.CSharp;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Shell;
using XSharp.CodeDom;

[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeDomSupport")]
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
    internal class XSharpCodeProvider : CSharpCodeProvider
    {
        private readonly CodeCompiler _generator;
        [ImportingConstructor]
        public XSharpCodeProvider()
        {
            _generator = new XSharpCodeGenerator();
        }
        public override string FileExtension => "prg";


        [Obsolete("Callers should not use the ICodeGenerator interface and should instead use the methods directly on the CodeDomProvider class.")]
        public override ICodeGenerator CreateGenerator()
        {
            return _generator;
        }
        [Obsolete("Callers should not use the ICodeCompiler interface and should instead use the methods directly on the CodeDomProvider class.")]
        public override ICodeCompiler CreateCompiler()
        {
            return _generator;
        }
        public override void GenerateCodeFromMember(CodeTypeMember member, TextWriter writer, CodeGeneratorOptions options)
        {
            _generator.GenerateCodeFromMember(member, writer, options);
        }
    }
}
