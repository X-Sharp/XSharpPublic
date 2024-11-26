//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma warning disable RS0016
using VSLangProj110;
using VSLangProj80;

namespace Microsoft.VisualStudio.ProjectSystem.VS.Properties.XSharp
{
    [Export(ExportContractNames.VsTypes.ConfiguredProjectPropertiesAutomationObject)]
    [Order(Order.Default)]
    [AppliesTo(ProjectCapability.XSharp)]
    public class XSharpProjectConfigurationProperties : AbstractProjectConfigurationProperties,
        CSharpProjectConfigurationProperties3,
        CSharpProjectConfigurationProperties6
    {
        [ImportingConstructor]
        internal XSharpProjectConfigurationProperties(
            ProjectProperties projectProperties,
            IProjectThreadingService threadingService)
            : base(projectProperties, threadingService)
        {
        }

        public string ErrorReport { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }
    }
}
