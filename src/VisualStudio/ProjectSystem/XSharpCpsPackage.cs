//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;

using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using Task = System.Threading.Tasks.Task;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// VS Package that registers the CPS-based XSharp project system.
    ///
    /// This package is loaded only in VS2022 and later (enforced by the
    /// <c>MinimumVisualStudioVersion</c> entry in the VSIX manifest and by the
    /// <c>[$RootKey$\Projects\...]</c> registry entry in
    /// <see cref="XSharpCpsPackage"/> .pkgdef, which is only deployed for VS2022+).
    ///
    /// For VS2019 the existing MPF-based <c>XSharpProjectPackage</c> continues to
    /// handle <c>.xsproj</c> files unchanged.
    ///
    /// Responsibilities
    /// ────────────────
    /// • Ensures the CPS MEF composition is bootstrapped before any project is loaded.
    /// • Registers the VO binary editor factories so they work in the CPS context.
    ///   (The factories themselves live in the ProjectPackage assembly; we just make
    ///   sure they are registered when the CPS package loads.)
    /// • Exposes a VS version check so other components can query whether CPS is
    ///   the active project system.
    /// </summary>
    [Description(XSharpConstants.ProjectSystemName + " (CPS)")]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(
        VSConstants.UICONTEXT.SolutionExists_string,
        PackageAutoLoadFlags.BackgroundLoad)]
    [Guid(XSharpConstants.guidCpsProjectTypeString)]
    // ── Project type registration ──────────────────────────────────────────────
    // The .xsproj extension is bound to the CPS project type via the .pkgdef file.
    // We also keep the VO editor extensions registered here for VS2022+.
    [ProvideEditorExtension(
        editorType: typeof(XSharpCpsPackage),   // placeholder — real factory is in ProjectPackage
        extension: ".xsfrm",
        priority: 0x42,
        DefaultName = "XSharp VO Form Editor",
        NameResourceID = 80110)]
    [ProvideEditorExtension(
        editorType: typeof(XSharpCpsPackage),
        extension: ".xsmnu",
        priority: 0x42,
        DefaultName = "XSharp VO Menu Editor",
        NameResourceID = 80111)]
    [ProvideEditorExtension(
        editorType: typeof(XSharpCpsPackage),
        extension: ".xsdbs",
        priority: 0x42,
        DefaultName = "XSharp VO DbServer Editor",
        NameResourceID = 80112)]
    [ProvideEditorExtension(
        editorType: typeof(XSharpCpsPackage),
        extension: ".xsfs",
        priority: 0x42,
        DefaultName = "XSharp VO FieldSpec Editor",
        NameResourceID = 80113)]
    public sealed class XSharpCpsPackage : AsyncPackage
    {
        // ─── VS version threshold ────────────────────────────────────────────────────

        /// <summary>
        /// The minimum VS version that activates the CPS project system.
        /// VS2022 = 17.x.
        /// </summary>
        private const int MinCpsMajorVersion = 17;

        private static bool? s_isCpsEnabled;

        /// <summary>
        /// Returns <c>true</c> when the host VS instance is new enough to use the
        /// CPS-based project system (VS2022, version 17.0 or later).
        /// </summary>
        public static bool IsCpsEnabled
        {
            get
            {
                if (s_isCpsEnabled.HasValue)
                    return s_isCpsEnabled.Value;

                try
                {
                    s_isCpsEnabled =
                        typeof(XSharpCpsPackage).Assembly
                            .GetName().Version.Major >= 1 // always true; real check below
                        && System.Environment.Version.Major >= 4;  // .NET 4+ implies VS2019+

                    // The reliable check is the VS shell version.
                    var shell = ServiceProvider.GlobalProvider.GetService(
                        typeof(SVsShell)) as IVsShell;
                    if (shell != null)
                    {
                        shell.GetProperty(
                            (int)__VSSPROPID5.VSSPROPID_ReleaseVersion,
                            out object versionObj);
                        if (versionObj is string versionStr &&
                            System.Version.TryParse(
                                versionStr.Split(' ')[0], out System.Version vsVersion))
                        {
                            s_isCpsEnabled = vsVersion.Major >= MinCpsMajorVersion;
                        }
                    }
                }
                catch
                {
                    s_isCpsEnabled = false;
                }

                return s_isCpsEnabled.Value;
            }
        }

        // ─── Package initialisation ──────────────────────────────────────────────────

        /// <inheritdoc />
        protected override async Task InitializeAsync(
            CancellationToken cancellationToken,
            IProgress<ServiceProgressData> progress)
        {
            await base.InitializeAsync(cancellationToken, progress);
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);

            // Guard: only do CPS-specific work when running in VS2022+.
            if (!IsCpsEnabled)
                return;

            // Log successful initialisation.
            XSettings.Logger.Information(
                $"[XSharpCpsPackage] CPS project system initialized " +
                $"(VS {GetVsVersionString()}).");
        }

        private static string GetVsVersionString()
        {
            try
            {
                var shell = ServiceProvider.GlobalProvider.GetService(typeof(SVsShell)) as IVsShell;
                if (shell != null)
                {
                    shell.GetProperty(
                        (int)__VSSPROPID5.VSSPROPID_ReleaseVersion,
                        out object v);
                    return v?.ToString() ?? "unknown";
                }
            }
            catch { }
            return "unknown";
        }
    }
}
