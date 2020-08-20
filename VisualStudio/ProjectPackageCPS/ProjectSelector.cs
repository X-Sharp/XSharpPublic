//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using IAsyncServiceProvider = Microsoft.VisualStudio.Shell.IAsyncServiceProvider;
using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio.ProjectSystem.VS;
using Microsoft.VisualStudio;
using System.ComponentModel.Composition.Primitives;

namespace XSharp.ProjectSystem
{
    [Export(typeof(IPackageService))]
    [Guid(XSharpVsPackage.ProjectSelectorGuid)]
    [ProvideObject(typeof(XSharpProjectSelector), RegisterUsing = RegistrationMethod.CodeBase)]
    internal sealed class XSharpProjectSelector : IVsProjectSelector, IPackageService, IDisposable
    {
        private const string MSBuildXmlNamespace = "http://schemas.microsoft.com/developer/msbuild/2003";

        private readonly JoinableTaskContext _context;
        private IVsRegisterProjectSelector _projectSelector;
        private uint _cookie = VSConstants.VSCOOKIE_NIL;

        [ImportingConstructor]
        public XSharpProjectSelector(JoinableTaskContext context)
        {
            _context = context;
        }

        public async Task InitializeAsync(IAsyncServiceProvider asyncServiceProvider)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            _projectSelector = (IVsRegisterProjectSelector)await asyncServiceProvider.GetServiceAsync(typeof(SVsRegisterProjectTypes));

            Guid selectorGuid = GetType().GUID;
            _projectSelector.RegisterProjectSelector(ref selectorGuid, this, out _cookie);
        }

        public void GetProjectFactoryGuid(Guid guidProjectType, string pszFilename, out Guid guidProjectFactory)
        {
            var doc = XDocument.Load(pszFilename);
            GetProjectFactoryGuid(doc, out guidProjectFactory);
        }

        internal static void GetProjectFactoryGuid(XDocument doc, out Guid guidProjectFactory)
        {
            var nsm = new XmlNamespaceManager(new NameTable());
            nsm.AddNamespace("msb", MSBuildXmlNamespace);

            // If the project has either a Project-level SDK attribute or an Import-level SDK attribute, we'll open it with the new project system.
            // Check both namespace-qualified and unqualified forms to include projects with and without the xmlns attribute.
            bool hasProjectElementWithSdkAttribute = doc.XPathSelectElement("/msb:Project[@Sdk]", nsm) != null || doc.XPathSelectElement("/Project[@Sdk]") != null;
            bool hasImportElementWithSdkAttribute = doc.XPathSelectElement("/*/msb:Import[@Sdk]", nsm) != null || doc.XPathSelectElement("/*/Import[@Sdk]") != null;

            if (hasProjectElementWithSdkAttribute || hasImportElementWithSdkAttribute)
            {
                guidProjectFactory = new Guid(XSharpVsPackage.ProjectTypeGuid);
                return;
            }

            guidProjectFactory = new Guid(XSharpVsPackage.LegacyXSharpGuid);
        }

        public void Dispose()
        {

            if (_cookie != VSConstants.VSCOOKIE_NIL)
            {
                _projectSelector.UnregisterProjectSelector(_cookie);
            }
        }
    }
}
namespace Microsoft.VisualStudio.ProjectSystem.VS
{
    /// <summary>
    /// A service that is initialized when the VS package is initialized.
    /// </summary>
    /// <remarks>
    /// Implementations must be exported in global scope.
    /// </remarks>
    [ProjectSystemContract(ProjectSystemContractScope.ConfiguredProject, ProjectSystemContractProvider.Private, Cardinality = Composition.ImportCardinality.ZeroOrMore)]
    internal interface IPackageService
    {
        /// <summary>
        /// Called when the package is initializing.
        /// </summary>
        /// <remarks>
        /// Always called on the UI thread.
        /// </remarks>
        Task InitializeAsync(IAsyncServiceProvider asyncServiceProvider);
    }
}

