//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Globalization;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using MSBuildConstruction = Microsoft.Build.Construction;

namespace Microsoft.VisualStudio.Project
{

    /// <summary>
    /// Allows getting and setting configuration-dependent properties for a project.
    /// </summary>
    [CLSCompliant(false)]
    public class XProjectConfig : ProjectConfig
    {
        internal const string X86Platform = "x86";
        internal const string X64Platform = "x64";
        internal const string IA64Platform = "ia64";
        internal const string AnyCpuPlatform = "AnyCPU";

        internal const string DebugConfiguration = "Debug";
        internal const string ReleaseConfiguration = "Release";

        internal const string ConfigConditionString = " '$(Configuration)' == '{0}' ";
        internal const string PlatformConditionString = " '$(Platform)' == '{0}' ";
        internal const string ConfigAndPlatformConditionString = " '$(Configuration)|$(Platform)' == '{0}|{1}' ";

        /// <summary>
        /// Creates a new project config instance.
        /// </summary>
        /// <param name="project">Parent project node.</param>
        /// <param name="name">Canonical Name</param>
        public XProjectConfig(ProjectNode project, ConfigCanonicalName name)
            : base(project, name)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
        }

        /// <summary>
        /// Gets the conditional expression for the PropertyGroup corresponding to the project config.
        /// </summary>
        public override string Condition
        {
            get
            {
                return String.Format(CultureInfo.InvariantCulture, XProjectConfig.ConfigAndPlatformConditionString, this.ConfigCanonicalName.ConfigName, this.ConfigCanonicalName.MSBuildPlatform);
            }
        }
    }

    internal class XConfigProvider: ConfigProvider
    {
        internal XConfigProvider(ProjectNode manager) : base(manager)
        {
        }
        protected override  ProjectConfig CreateProjectConfiguration(ConfigCanonicalName canonicalName)
        {
            return new XProjectConfig(this.ProjectMgr, canonicalName);
        }

    }
}
