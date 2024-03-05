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
namespace XSharp.VisualStudio.ProjectSystem
{
    [Export]
    [AppliesTo(XSharpConstants.LanguageName)]
    [ProjectTypeRegistration(XSharpConstants.guidCpsProjectTypeString, "XSharp", "#2",
        XSharpConstants.DottedProjectExtension, XSharpConstants.LanguageName,
        resourcePackageGuid: XSharpConstants.guidCpsProjectTypeString,
        PossibleProjectExtensions = XSharpConstants.DottedProjectExtension,
        ProjectTemplatesDir = @"..\..\Templates\Projects\XSharp")]
    [ProvideProjectItem(XSharpConstants.guidCpsProjectTypeString, "XSharp Items", @"..\..\Templates\ProjectItems", 500)]
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
        internal UnconfiguredProject UnconfiguredProject { get; set; }

        [Import]
        internal IActiveConfiguredProjectSubscriptionService SubscriptionService { get; set; }

        //[Import]
        //internal IThreadHandling ThreadHandling { get; }

        [Import]
        internal ActiveConfiguredProject<ConfiguredProject> ActiveConfiguredProject { get; set; }

        [Import]
        internal ActiveConfiguredProject<XSharpConfiguredProject> MyActiveConfiguredProject { get; set; }

        [ImportMany(ExportContractNames.VsTypes.IVsProject, typeof(IVsProject))]
        internal OrderPrecedenceImportCollection<IVsHierarchy> ProjectHierarchies { get; }

        internal IVsHierarchy ProjectHierarchy
        {
            get { return this.ProjectHierarchies.Single().Value; }
        }
    }
}
