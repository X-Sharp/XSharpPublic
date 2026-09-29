//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Well-known CPS project capability strings advertised by the XSharp project system.
    /// These are consumed by MEF components that use [AppliesTo] to scope themselves to
    /// XSharp projects only.
    /// </summary>
    internal static class XSharpProjectCapabilities
    {
        // ─── XSharp-specific ────────────────────────────────────────────────────────

        /// <summary>
        /// The primary capability that identifies an XSharp project.
        /// All XSharp-specific MEF exports should use [AppliesTo(XSharp)].
        /// </summary>
        public const string XSharp = nameof(XSharp);

        // ─── Standard CPS capabilities ──────────────────────────────────────────────

        /// <summary>Marks this as a managed (.NET) project.</summary>
        public const string Managed = ProjectCapabilities.Managed;

        /// <summary>
        /// Reuses the C# capability string so the Roslyn language service (which
        /// already supports XSharp via its own MEF exports) can be associated with
        /// this project type without extra plumbing.
        /// </summary>
        public const string CSharp = ProjectCapabilities.CSharp;

        /// <summary>The project supports project-to-project references.</summary>
        public const string ProjectReferences = ProjectCapabilities.ProjectReferences;

        /// <summary>The project supports raw assembly (DLL) references.</summary>
        public const string AssemblyReferences = ProjectCapabilities.AssemblyReferences;

        /// <summary>The project supports NuGet package references.</summary>
        public const string PackageReferences = ProjectCapabilities.PackageReferences;

        /// <summary>The project exposes output groups (for VSIX packaging, etc.).</summary>
        public const string OutputGroups = ProjectCapabilities.OutputGroups;

        /// <summary>All target output groups are available.</summary>
        public const string AllTargetOutputGroups = ProjectCapabilities.AllTargetOutputGroups;

        /// <summary>The project supports multi-targeting via &lt;TargetFrameworks&gt;.</summary>
        /// <remarks>
        /// Multi-targeting is a VS2022+ / CPS-only feature.
        /// The MPF-based path (VS2019) does NOT support this capability.
        /// </remarks>
        public const string MultipleTargetFrameworks = ProjectCapabilities.MultipleTargetFrameworks;

        /// <summary>The project supports pre-/post-build events.</summary>
        public const string BuildEvents = nameof(BuildEvents);

        /// <summary>The project supports source control operations.</summary>
        public const string SourceControlled = ProjectCapabilities.SourceControlled;
    }
}
