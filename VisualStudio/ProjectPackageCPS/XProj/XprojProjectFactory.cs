// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.
using System;
using System.IO;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Microsoft;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Flavor;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using IAsyncServiceProvider = Microsoft.VisualStudio.Shell.IAsyncServiceProvider;
#pragma warning disable VSTHRD010
namespace XSharp.ProjectSystem
{
    [Export(typeof(IPackageService))]
    [Guid(XSharpConstants.guidCpsProjectTypeString)]
    internal sealed class XprojProjectFactory : FlavoredProjectFactoryBase, IVsProjectUpgradeViaFactory4, IPackageService, IDisposable
    {
        private readonly JoinableTaskContext _context;
        private IVsRegisterProjectTypes _registerProjectTypes;
        private uint _cookie = VSConstants.VSCOOKIE_NIL;

        [ImportingConstructor]
        public XprojProjectFactory(JoinableTaskContext context)
        {
            _context = context;
        }

        public async Task InitializeAsync(IAsyncServiceProvider asyncServiceProvider)
        {
            Assumes.Null(_registerProjectTypes);

            _context.VerifyIsOnMainThread();

            _registerProjectTypes = await asyncServiceProvider.GetServiceAsync<SVsRegisterProjectTypes, IVsRegisterProjectTypes>();

            ((IVsProjectFactory)this).SetSite(new ServiceProviderToOleServiceProviderAdapter(ServiceProvider.GlobalProvider));

            Guid guid = GetType().GUID;
            Verify.HResult(_registerProjectTypes.RegisterProjectType(ref guid, this, out _cookie));
        }

        public void UpgradeProject_CheckOnly(
            string fileName,
            IVsUpgradeLogger logger,
            out uint upgradeRequired,
            out Guid migratedProjectFactory,
            out uint upgradeProjectCapabilityFlags)
        {
            // Xproj is deprecated. It cannot be upgraded, and cannot be loaded.
            upgradeRequired = (uint)__VSPPROJECTUPGRADEVIAFACTORYREPAIRFLAGS.VSPUVF_PROJECT_DEPRECATED;
            migratedProjectFactory = GetType().GUID;
            upgradeProjectCapabilityFlags = 0;
            _context.VerifyIsOnMainThread();
            // Log a message explaining that the project cannot be automatically upgraded
            // and how to perform the upgrade manually. This message will be presented in
            // the upgrade report.
            logger.LogMessage(
                (uint)__VSUL_ERRORLEVEL.VSUL_ERROR,
                Path.GetFileNameWithoutExtension(fileName),
                fileName,
                "Xproj project files are no longer supported. ");
        }

        protected override object PreCreateForOuter(IntPtr outerProjectIUnknown)
        {
            // Should not be called
            throw new NotImplementedException();
        }

        public void Dispose()
        {
            _context.VerifyIsOnMainThread();
            if (_cookie != VSConstants.VSCOOKIE_NIL && !(_registerProjectTypes is null))
            {
                Verify.HResult(_registerProjectTypes.UnregisterProjectType(_cookie));
            }
            Dispose(disposing: true);
        }
    }
}
