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
using IAsyncServiceProvider = Microsoft.VisualStudio.Shell.IAsyncServiceProvider;
using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.Threading;
using Microsoft;
using Microsoft.VisualStudio.ProjectSystem.VS;
using Microsoft.VisualStudio.Composition;

namespace XSharp.VisualStudio.ProjectSystem
{
    [Export(typeof(IPackageService))]
    [Guid(XSharpConstants.guidProjectSelectorString)]
    [ProvideObject(typeof(XSharpProjectSelector), RegisterUsing = RegistrationMethod.CodeBase)]
    internal sealed class XSharpProjectSelector : IVsProjectSelector, IDisposable, IPackageService
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
#pragma warning disable VSTHRD010
        public async Task InitializeAsync(IAsyncServiceProvider asyncServiceProvider)
        {
            Assumes.Null(_projectSelector);
            _context.VerifyIsOnMainThread();
            var service = await asyncServiceProvider.GetServiceAsync(typeof(SVsRegisterProjectTypes)); ;
            if (service != null)
            {
                _projectSelector = (IVsRegisterProjectSelector)service;
                if (_projectSelector != null)
                {
                    Guid selectorGuid = GetType().GUID;
                    _projectSelector.RegisterProjectSelector(ref selectorGuid, this, out _cookie);
                }
            }
            return;
        }
#pragma warning restore VSTHRD010
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
                guidProjectFactory = XSharpConstants.guidCpsProjectType;
                return;
            }

            guidProjectFactory = XSharpConstants.guidXSharpProjectFactory;
        }
#pragma warning disable VSTHRD010
        public void Dispose()
        {
            _context.VerifyIsOnMainThread();
            if (_cookie != VSConstants.VSCOOKIE_NIL)
            {
                _projectSelector.UnregisterProjectSelector(_cookie);
            }
        }
#pragma warning restore VSTHRD010
    }

    internal static class JoinableTaskContextExtensions
    {
        /// <summary>
        ///     Verifies that this method is called on the main ("UI") thread,
        ///     and throws an exception if not.
        /// </summary>
        public static void VerifyIsOnMainThread(this JoinableTaskContext joinableTaskContext)
        {
            Requires.NotNull(joinableTaskContext, nameof(joinableTaskContext));

            if (!joinableTaskContext.IsOnMainThread)
            {
                throw new COMException("This method must be called on the UI thread.", HResult.WrongThread);
            }
        }
    }
   

}

