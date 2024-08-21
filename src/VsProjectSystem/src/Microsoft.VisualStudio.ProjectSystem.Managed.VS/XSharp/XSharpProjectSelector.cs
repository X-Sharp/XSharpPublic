// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using IAsyncServiceProvider = Microsoft.VisualStudio.Shell.IAsyncServiceProvider;

namespace Microsoft.VisualStudio.ProjectSystem.VS.XSharp
{
    [Export(typeof(IXPackageService))]
    [Guid("DF89AA3B-6AE6-4971-8F78-1A03B346A7D5")]
    [ProvideObject(typeof(XSharpProjectSelector), RegisterUsing = RegistrationMethod.Assembly)]
    internal sealed class XSharpProjectSelector : IVsProjectSelector, IXPackageService, IDisposable
    {
        private const string MSBuildXmlNamespace = "http://schemas.microsoft.com/developer/msbuild/2003";

        private readonly JoinableTaskContext _context;
        private IVsRegisterProjectSelector? _projectSelector;
        private uint _cookie = VSConstants.VSCOOKIE_NIL;

        [ImportingConstructor]
        public XSharpProjectSelector(JoinableTaskContext context)
        {
            _context = context;
        }

        public async Task InitializeAsync(IAsyncServiceProvider asyncServiceProvider)
        {
            Assumes.Null(_projectSelector);

            _context.VerifyIsOnMainThread();

            _projectSelector = await asyncServiceProvider.GetServiceAsync<SVsRegisterProjectTypes, IVsRegisterProjectSelector>();

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
            bool hasProjectElementWithSdkAttribute = doc.XPathSelectElement("/msb:Project[@Sdk]", nsm) is not null || doc.XPathSelectElement("/Project[@Sdk]") is not null;
            bool hasImportElementWithSdkAttribute = doc.XPathSelectElement("/*/msb:Import[@Sdk]", nsm) is not null || doc.XPathSelectElement("/*/Import[@Sdk]") is not null;

            if (hasProjectElementWithSdkAttribute || hasImportElementWithSdkAttribute)
            {
                guidProjectFactory = ProjectType.XSharpGuid;
                return;
            }

            guidProjectFactory = ProjectType.LegacyXSharpGuid;
        }

        public void Dispose()
        {
            _context.VerifyIsOnMainThread();

            if (_cookie != VSConstants.VSCOOKIE_NIL)
            {
                _projectSelector?.UnregisterProjectSelector(_cookie);
            }
        }
    }
}
