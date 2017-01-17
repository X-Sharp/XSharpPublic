//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Globalization;
using Microsoft.VisualStudio.Project;
using MSBuildConstruction = Microsoft.Build.Construction;

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

       private void RemovePropertyUnderCondition(string propertyName, string condition)
       {
            string conditionTrimmed = (condition == null) ? String.Empty : condition.Trim();
            var evaluatedProject = this.ProjectMgr.BuildProject;

            if (conditionTrimmed.Length == 0)
            {
                var prop = evaluatedProject.GetProperty(propertyName) ;
                if (prop != null)
                    evaluatedProject.RemoveProperty(prop);
                return;
            }

            // New OM doesn't have a convenient equivalent for setting a property with a particular property group condition.
            // So do it ourselves.
            MSBuildConstruction.ProjectPropertyGroupElement newGroup = null;

            foreach (MSBuildConstruction.ProjectPropertyGroupElement group in evaluatedProject.Xml.PropertyGroups)
            {
                if (String.Equals(group.Condition.Trim(), conditionTrimmed, StringComparison.OrdinalIgnoreCase))
                {
                    newGroup = group;
                    break;
                }
            }

            foreach (MSBuildConstruction.ProjectPropertyElement property in newGroup.PropertiesReversed) // If there's dupes, pick the last one so we win
            {
                if (String.Equals(property.Name, propertyName, StringComparison.OrdinalIgnoreCase) && property.Condition.Length == 0)
                {
                    var prop = evaluatedProject.GetProperty(property.Name);
                    if (prop != null)
                        evaluatedProject.RemoveProperty(prop);
                    return;
                }
            }
        }
    }
}
