//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.ComponentModel.Composition;
using System.Threading.Tasks.Dataflow;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Threading.Tasks;
using XSharpModel;
namespace XSharp.ProjectSystem
{
    [Export]
    [AppliesTo(XSharpConstants.LanguageName)]
    [ProjectTypeRegistration(GuidStrings.guidCpsProjectType, "XSharp", "#2",
        XSharpConstants.ProjectExtension, XSharpConstants.LanguageName,
        resourcePackageGuid: GuidStrings.guidCpsProjectType,
        PossibleProjectExtensions = XSharpConstants.ProjectExtension,
        ProjectTemplatesDir = @"..\..\Templates\Projects\XSharp")]
    [ProvideProjectItem(GuidStrings.guidCpsProjectType, "XSharp Items", @"..\..\Templates\ProjectItems", 500)]
    internal class XSharpUnconfiguredProject 
    {
        /// <summary>
        /// The file extension used by your project type.
        /// This does not include the leading period.
        /// </summary>


        [ImportingConstructor]
        public XSharpUnconfiguredProject(UnconfiguredProject unconfiguredProject)
        {
            this.ProjectHierarchies = new OrderPrecedenceImportCollection<IVsHierarchy>(projectCapabilityCheckProvider: unconfiguredProject);
        }


        [Import]
        internal UnconfiguredProject UnconfiguredProject { get; }

        [Import]
        internal IActiveConfiguredProjectSubscriptionService SubscriptionService { get; }

        //[Import]
        //internal IThreadHandling ThreadHandling { get; }

        [Import]
        internal ActiveConfiguredProject<ConfiguredProject> ActiveConfiguredProject { get; }

        [Import]
        internal ActiveConfiguredProject<XSharpConfiguredProject> MyActiveConfiguredProject { get; }

        [ImportMany(ExportContractNames.VsTypes.IVsProject, typeof(IVsProject))]
        internal OrderPrecedenceImportCollection<IVsHierarchy> ProjectHierarchies { get; }

        internal IVsHierarchy ProjectHierarchy
        {
            get { return this.ProjectHierarchies.Single().Value; }
        }
    }
}
