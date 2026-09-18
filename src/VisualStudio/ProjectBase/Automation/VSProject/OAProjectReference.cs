/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/
using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using VSLangProj;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents a project reference of the solution
    /// </summary>
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    [CLSCompliant(false), ComVisible(true)]
    public class OAProjectReference : OAReferenceBase<ProjectReferenceNode>
    {
        internal OAProjectReference(ProjectReferenceNode projectReference) :
            base(projectReference)
        {
        }

        #region Reference override
        public override string Culture
        {
            get { return string.Empty; }
        }
        public override string Name
        {
            get { return BaseReferenceNode.ReferencedProjectName; }
        }
        public override string Identity
        {
            get
            {
                return BaseReferenceNode.Caption;
            }
        }
        public override string Path
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
               {
                   await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                   var file = BaseReferenceNode.ReferencedProjectOutputPath;
                   return file;
               });
            }
        }
        public override EnvDTE.Project SourceProject
        {
            get
            {
                var referencedGuid = BaseReferenceNode.ReferencedProjectGuid;
                if (Guid.Empty == referencedGuid)
                {
                    return null;
                }
                if (BaseReferenceNode.ProjectMgr == null)
                {
                    return null;
                }
                // The ProjectInfo is shared by every project reference that points at this
                // project, and it is dropped or cleared when that project closes, unloads or
                // reloads. So anything found here is both current and worth reusing, which
                // keeps this resolution at once per project instead of once per reference.
                // Register the entry when it is missing: the build dependency pass that
                // normally creates it can run after this property is first read, and without
                // an entry there is nowhere to cache and every caller resolves again.
                var projectInfo = ProjectInfo.GetOrCreate(referencedGuid, BaseReferenceNode.Url);
                var cached = projectInfo?.DteProject;
                if (cached != null)
                {
                    return cached;
                }
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    IVsHierarchy hierarchy = projectInfo?.Hierarchy;
                    if (hierarchy == null)
                    {
                        hierarchy = VsShellUtilities.GetHierarchy(BaseReferenceNode.ProjectMgr.Site, referencedGuid);
                        Logger.Information($"OAProjectReference: Resolved hierarchy for project reference {referencedGuid} through the shell");
                    }
                    Logger.Information($"OAProjectReference: GetHierarchy for project reference {referencedGuid} returned {(hierarchy != null ? "a hierarchy" : "null")}");
                    if (null == hierarchy)
                    {
                        return null;
                    }
                    // Cache the hierarchy even when the automation object below cannot be
                    // obtained: the hierarchy is what costs a solution wide lookup, and
                    // leaving it unstored made every later caller pay for it again.
                    if (projectInfo != null)
                    {
                        projectInfo.Hierarchy = hierarchy;
                    }
                    object extObject;
                    if (Microsoft.VisualStudio.ErrorHandler.Succeeded(
                            hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out extObject))
                        && extObject is EnvDTE.Project project)
                    {
                        if (projectInfo != null)
                        {
                            projectInfo.DteProject = project;
                        }
                        return project;
                    }
                    return null;
                });
            }
        }
        public override prjReferenceType Type
        {
            // TODO: Write the code that finds out the type of the output of the source project.
            get { return prjReferenceType.prjReferenceTypeAssembly; }
        }
        public override string Version
        {
            get { return string.Empty; }
        }
        public override bool SpecificVersion => false;
        public override string RuntimeVersion
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    var project = SourceProject;
                    return project.Properties.Item("TargetFrameworkVersion").Value.ToString();
                });
            }
        }
        #endregion
    }
}
