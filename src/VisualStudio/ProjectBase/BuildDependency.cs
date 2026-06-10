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

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using CVT = Community.VisualStudio.Toolkit;

using XSharp.Settings;

namespace Microsoft.VisualStudio.Project
{
    public class BuildDependency : IVsBuildDependency
    {
        Guid referencedProjectGuid = Guid.Empty;
        ProjectNode projectMgr = null;

        [CLSCompliant(false)]
        internal BuildDependency(ProjectNode projectMgr, Guid projectReference)
        {
            this.referencedProjectGuid = projectReference;
            this.projectMgr = projectMgr;
        }

        #region IVsBuildDependency methods
        public int get_CanonicalName(out string canonicalName)
        {
            canonicalName = null;
            return VSConstants.S_OK;
        }

        public int get_Type(out System.Guid guidType)
        {
            // All our dependencies are build projects
            guidType = VSConstants.GUID_VS_DEPTYPE_BUILD_PROJECT;
            return VSConstants.S_OK;
        }

        public int get_Description(out string description)
        {
            description = null;
            return VSConstants.S_OK;
        }

        [CLSCompliant(false)]
        public int get_HelpContext(out uint helpContext)
        {
            helpContext = 0;
            return VSConstants.E_NOTIMPL;
        }

        public int get_HelpFile(out string helpFile)
        {
            helpFile = null;
            return VSConstants.E_NOTIMPL;
        }

        public int get_MustUpdateBefore(out int mustUpdateBefore)
        {
            // Must always update dependencies
            mustUpdateBefore = 1;

            return VSConstants.S_OK;
        }

        public int get_ReferredProject(out object unknownProject)
        {
            unknownProject = null;

            unknownProject = this.GetReferencedHierarchy();

            // If we cannot find the referenced hierarchy return S_FALSE.
            return (unknownProject == null) ? VSConstants.S_FALSE : VSConstants.S_OK;
        }

        #endregion

        #region helper methods
        private IVsHierarchy GetReferencedHierarchy()
        {
            IVsHierarchy hierarchy = null;

            if(this.referencedProjectGuid == Guid.Empty || this.projectMgr == null || this.projectMgr.IsClosed)
            {
                return hierarchy;
            }
            IVsHierarchy result = null;
            var projectInfo = ProjectInfo.GetProjectInfo(this.referencedProjectGuid);
            if (projectInfo != null)
            {
                if (projectInfo.Hierarchy != null)
                {
                    Logger.Information($"Retrieve hierarchy for project reference {this.referencedProjectGuid} from cache");
                    result = projectInfo.Hierarchy;
                }
                else
                {
                    var project = (CVT.Project)XSettings.ShellLink.FindVsProject(projectInfo.Url);
                    if (project != null)
                    {
                        project.GetItemInfo(out result, out _, out _);
                        projectInfo.Hierarchy = result;
                        Logger.Information($"Read hierarchy for project {projectInfo.Url} from CVT");
                    }
                }
            }
            if (result == null)
            {
                result = VsShellUtilities.GetHierarchy(this.projectMgr.Site, this.referencedProjectGuid);
                if (result != null && projectInfo != null)
                {
                    projectInfo.Hierarchy = result;
                }
            }
            if (result == null)
            {
                Logger.Error($"BuildDependency: GetHierarchy for project reference {this.referencedProjectGuid} returned null");
            }
            else
            {
                Logger.Information($"BuildDependency: GetHierarchy for project reference {this.referencedProjectGuid} returned {result}");
            }
            return result;

        }

        #endregion

    }
}
