//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System.ComponentModel.Composition;
using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.ProjectSystem
{
    [Export(ExportContractNames.VsTypes.ConfiguredProjectPropertiesAutomationObject)]
    [AppliesTo(ProjectCapability.XSharp)]
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
