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
using System.Diagnostics;
using Microsoft.Build.Execution;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Project;
namespace Microsoft.VisualStudio.Project
{
    //
    // RvdH: Note that the C# project system calls the constructor with a null outputassembly
    //       we therefore retrieve all the info through the ProjectNode in stead.
    //
    //
    class Output : IVsOutput2
    {
        private ProjectNode project;
        //private ProjectItemInstance output;

        /// <summary>
        /// Constructor for IVSOutput2 implementation
        /// </summary>
        /// <param name="projectManager">Project that produce this output</param>
        /// <param name="outputAssembly">MSBuild generated item corresponding to the output assembly (by default, these would be of type MainAssembly</param>
        public Output(ProjectNode projectManager, ProjectItemInstance outputAssembly)
        {
            Utilities.ArgumentNotNull("projectManager", projectManager);
            // outputAssembly may be empty!
            //Utilities.ArgumentNotNull("outputAssembly", outputAssembly);
            project = projectManager;
            //output = outputAssembly;
        }

        #region IVsOutput2 Members

        public int get_CanonicalName(out string pbstrCanonicalName)
        {
            // Get the output assembly path (including the name)
            pbstrCanonicalName = project.GetProjectProperty(ProjectFileConstants.TargetPath);
            Debug.Assert(!String.IsNullOrEmpty(pbstrCanonicalName), "Output Assembly not defined");

            // Make sure we have a full path
            if(!System.IO.Path.IsPathRooted(pbstrCanonicalName))
            {
                pbstrCanonicalName = new Url(project.BaseURI, pbstrCanonicalName).AbsoluteUrl;
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// This path must start with file:/// if it wants other project
        /// to be able to reference the output on disk.
        /// If the output is not on disk, then this requirement does not
        /// apply as other projects probably don't know how to access it.
        /// </summary>
        public virtual int get_DeploySourceURL(out string pbstrDeploySourceURL)
        {
            string path = string.Empty; ;
            if (this.project is XProjectNode)
            {
                var xproject = project as XProjectNode;
                path = project.GetOutputAssembly(xproject.CurrentConfig.ConfigCanonicalName);
            }
            if (String.IsNullOrEmpty(path))
            {
                path = project.GetProjectProperty(ProjectFileConstants.TargetPath);
            }
            if(path.Length < 9 || String.Compare(path.Substring(0, 8), "file:///", StringComparison.OrdinalIgnoreCase) != 0)
                path = "file:///" + path; // TODO: does not work with '#' char, see e.g. bug 641942
            pbstrDeploySourceURL = path;
            return VSConstants.S_OK;
        }

        public int get_DisplayName(out string pbstrDisplayName)
        {
            return this.get_CanonicalName(out pbstrDisplayName);
        }

        public virtual int get_Property(string szProperty, out object pvar)
        {
            if (string.IsNullOrEmpty(szProperty))
            {
                pvar = null;
                return VSConstants.E_INVALIDARG;
            }
	        if (string.Equals(szProperty, "OUTPUTLOC", StringComparison.OrdinalIgnoreCase))
            {
                szProperty = ProjectFileConstants.TargetPath;
            }
            if (string.Equals(szProperty, "FinalOutputPath", StringComparison.OrdinalIgnoreCase))
            {
                szProperty = ProjectFileConstants.TargetPath;
            }
            string value = project.GetProjectProperty(szProperty);
            pvar = value;

            // If we don't have a value, we are expected to return unimplemented
            if (string.IsNullOrEmpty(value))
            {
                return VSConstants.E_NOTIMPL;
            }

            // Special hack for COM2REG property: it's a bool rather than a string, and always true, for some reason.
            if (string.Equals(szProperty, "COM2REG", StringComparison.OrdinalIgnoreCase))
            {
                pvar = true;
            }

            return VSConstants.S_OK;
        }

        public int get_RootRelativeURL(out string pbstrRelativePath)
        {

            pbstrRelativePath = String.Empty;
            object variant;
            // get the corresponding property
            if(ErrorHandler.Succeeded(this.get_Property("TargetPath", out variant)))
            {
                string var = variant as String;

                if(var != null)
                {
                    pbstrRelativePath = var;
                }
            }

            return VSConstants.S_OK;
        }

        public virtual int get_Type(out Guid pguidType)
        {
            pguidType = Guid.Empty;
            throw new NotImplementedException();
        }

        #endregion
    }
}
