// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System.ComponentModel.Composition;
using VSLangProj110;
using VSLangProj80;
using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.ProjectSystem
{
    [Export(ExportContractNames.VsTypes.ConfiguredProjectPropertiesAutomationObject)]
    [AppliesTo("XSharp")]
    public class XSharpProjectConfigurationProperties : AbstractProjectConfigurationProperties
    {
        [ImportingConstructor]
        internal XSharpProjectConfigurationProperties(
            ProjectProperties projectProperties,
            IProjectThreadingService threadingService)
            : base(projectProperties, threadingService)
        {
        }

        public string ErrorReport { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }
    }
}
