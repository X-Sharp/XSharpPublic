//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Globalization;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{

    /// <summary>
    /// Allows getting and setting configuration-dependent properties for a WiX project.
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
        /// <param name="configName">Configuration name such as "Debug".</param>
        /// <param name="platformName">Platform name such as "x86".</param>
        public XProjectConfig(XProjectNode project, string configName, string platformName)
            : base(project, new ConfigCanonicalName(configName, platformName))
        {
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
}
