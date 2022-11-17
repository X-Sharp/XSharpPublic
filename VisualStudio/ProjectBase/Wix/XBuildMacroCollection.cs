//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.IO;
    using System.Runtime.InteropServices;
    using System.Text;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;
    using XSharp;

    public class XBuildMacroCollection : ICollection, IEnumerable<XBuildMacroCollection.MacroNameValuePair>
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        private static readonly string[] globalMacroNames =
              {
                XProjectFileConstants.DevEnvDir,
                XProjectFileConstants.SolutionDir,
                XProjectFileConstants.SolutionExt,
                XProjectFileConstants.SolutionName,
                XProjectFileConstants.SolutionFileName,
                XProjectFileConstants.SolutionPath,
            };

        private static readonly string[] macroNames =
              {
                "BaseIntermediateOutputPath",
                "BaseOutputPath",
                "ConfigurationName",
                "OutDir",
                "PlatformName",
                "ProjectDir",
                "ProjectExt",
                "ProjectFileName",
                "ProjectName",
                "ProjectPath",
                "TargetDir",
                "TargetExt",
                "TargetFileName",
                "TargetName",
                "TargetPath",
            };

        private SortedList<string, string> list = new SortedList<string, string>(macroNames.Length, StringComparer.OrdinalIgnoreCase);

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildMacros"/> class.
        /// </summary>
        /// <param name="project">The project from which to read the properties.</param>
        public XBuildMacroCollection(ProjectNode project)
        {
            XHelperMethods.VerifyNonNullArgument(project, "project");

            // get the global SolutionX properties
            XBuildMacroCollection.DefineSolutionProperties(project);
            foreach (string globalMacroName in globalMacroNames)
            {
                string property = null;
                project.BuildProject.GlobalProperties.TryGetValue(globalMacroName, out property);
                if (null == property)
                {
                    this.list.Add(globalMacroName, "*Undefined*");
                }
                else
                {
                    this.list.Add(globalMacroName, property);
                }
            }
            // we need to call the Build Target GetTargetPath first so that TargetDir and TargetPath are resolved correctly
            ConfigCanonicalName configCanonicalName;
            if (!Utilities.TryGetActiveConfigurationAndPlatform(project.Site, project, out configCanonicalName))
            {
                throw new InvalidOperationException();
            }
            BuildResult res = project.Build(configCanonicalName, XProjectFileConstants.GetTargetPath);

            // get the ProjectX and TargetX variables
            foreach (string macroName in macroNames)
            {
                string value;
                ThreadHelper.ThrowIfNotOnUIThread();
                if (res.ProjectInstance != null)
                {
                    value = res.ProjectInstance.GetPropertyValue(macroName);
                }
                else
                {
                    value = project.GetProjectProperty(macroName);
                }

                this.list.Add(macroName, value);
            }
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets the number of elements in the collection.
        /// </summary>
        public int Count
        {
            get { return this.list.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether access to the <see cref="ICollection"/> is synchronized (thread safe).
        /// </summary>
        bool ICollection.IsSynchronized
        {
            get { return ((ICollection)this.list).IsSynchronized; }
        }

        /// <summary>
        /// Gets an object that can be used to synchronize access to the <see cref="ICollection"/>.
        /// </summary>
        object ICollection.SyncRoot
        {
            get { return ((ICollection)this.list).SyncRoot; }
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Copies the elements of the <see cref="ICollection"/> to an <see cref="Array"/>, starting
        /// at a particular <b>Array</b> index.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements copied
        /// from <see cref="ICollection"/>. The <b>Array</b> must have zero-based indexing.
        /// </param>
        /// <param name="index">The zero-based index in <paramref name="array"/> at which copying begins.</param>
        void ICollection.CopyTo(Array array, int index)
        {
            ((ICollection)this.list).CopyTo(array, index);
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>An <see cref="IEnumerator&lt;T&gt;"/> object that can be used to iterate through the collection.</returns>
        IEnumerator<MacroNameValuePair> IEnumerable<MacroNameValuePair>.GetEnumerator()
        {
            foreach (KeyValuePair<string, string> pair in this.list)
            {
                yield return (MacroNameValuePair)pair;
            }
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>An <see cref="IEnumerator"/> object that can be used to iterate through the collection.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<MacroNameValuePair>)this).GetEnumerator();
        }

        /// <summary>
        /// When building with only a xsproj in the solution, the SolutionX variables are not
        /// defined, so we have to define them here.
        /// </summary>
        /// <param name="project">The project where the properties are defined.</param>
        internal static void DefineSolutionProperties(ProjectNode project)
        {
            IVsSolution solution = XHelperMethods.GetService<IVsSolution, SVsSolution>(project.Site);
            object solutionPathObj;
            ThreadHelper.ThrowIfNotOnUIThread();

            ErrorHandler.ThrowOnFailure(solution.GetProperty((int)__VSPROPID.VSPROPID_SolutionFileName, out solutionPathObj));
            string solutionPath = (string)solutionPathObj;
            XPackageSettings settings = XPackageSettings.Instance;
            string devEnvDir = XHelperMethods.EnsureTrailingDirectoryChar(Path.GetDirectoryName(settings.DevEnvPath));

            string[][] properties = new string[][]
                   {
                    new string[] { XProjectFileConstants.DevEnvDir, devEnvDir },
                    new string[] { XProjectFileConstants.SolutionPath, solutionPath },
                    new string[] { XProjectFileConstants.SolutionDir, XHelperMethods.EnsureTrailingDirectoryChar(Path.GetDirectoryName(solutionPath)) },
                    new string[] { XProjectFileConstants.SolutionExt, Path.GetExtension(solutionPath) },
                    new string[] { XProjectFileConstants.SolutionFileName, Path.GetFileName(solutionPath) },
                    new string[] { XProjectFileConstants.SolutionName, Path.GetFileNameWithoutExtension(solutionPath) },
                   };

            foreach (string[] property in properties)
            {
                string propertyName = property[0];
                string propertyValue = property[1];

                project.BuildProject.SetGlobalProperty(propertyName, propertyValue);
            }
        }

        /// <summary>
        /// When building a project in VS, the configuration of referenced projects cannot be determined
        /// by MSBuild or from within an MSBuild task. So we'll get them from the VS project system here.
        /// </summary>
        /// <param name="project">The project where the properties are being defined; also the project
        /// whose references are being examined.</param>
        internal static void DefineProjectReferenceConfigurations(XProjectNode project)
        {
            StringBuilder configList = new StringBuilder();

            IVsSolutionBuildManager solutionBuildManager =
                XHelperMethods.GetService<IVsSolutionBuildManager, SVsSolutionBuildManager>(project.Site);

            List<ProjectReferenceNode> referenceNodes = new List<ProjectReferenceNode>();
            project.FindNodesOfType(referenceNodes);

            foreach (ProjectReferenceNode referenceNode in referenceNodes)
            {
                try
                {
                    IVsHierarchy hierarchy = VsShellUtilities.GetHierarchy(referenceNode.ProjectMgr.Site, referenceNode.ReferencedProjectGuid);

                    string configuration = null;
                    IVsProjectCfg2 projectCfg2 = null;
                    IVsProjectCfg[] projectCfgArray = new IVsProjectCfg[1];
                    ThreadHelper.ThrowIfNotOnUIThread();

                    // this can fail for some reason... this code was copied from Wix and probably isn't stable yet.
                    // this routine is called from InvokeMSBuild and we don't want that to fix because of
                    // some bug here, so this code is surrounded by try/catch until we figure this out
                    int hr = solutionBuildManager.FindActiveProjectCfg(IntPtr.Zero, IntPtr.Zero, hierarchy, projectCfgArray);
                    ErrorHandler.ThrowOnFailure(hr);

                    projectCfg2 = projectCfgArray[0] as IVsProjectCfg2;

                    if (projectCfg2 != null)
                    {
                        hr = projectCfg2.get_DisplayName(out configuration);
                        if (hr != 0)
                        {
                            Marshal.ThrowExceptionForHR(hr);
                        }
                    }

                    if (configuration != null)
                    {
                        if (configList.Length > 0)
                        {
                            configList.Append(';');
                        }

                        configList.Append(referenceNode.ReferencedProjectName);
                        configList.Append('=');
                        configList.Append(configuration);
                    }
                }
                catch (Exception)
                {
                    ;
                }
            }

            if (configList.Length > 0)
            {
                project.BuildProject.SetGlobalProperty("VSProjectConfigurations", configList.ToString());
            }
        }

        // =========================================================================================
        // Classes
        // =========================================================================================

        /// <summary>
        /// Defines a macro name/value pair that can be set or retrieved.
        /// </summary>
        public struct MacroNameValuePair
        {
            private KeyValuePair<string, string> pair;

            /// <summary>
            /// Initializes a new instance of the <see cref="MacroNameValuePair"/> class.
            /// </summary>
            /// <param name="pair">The KeyValuePair&lt;string, string&gt; to store.</param>
            private MacroNameValuePair(KeyValuePair<string, string> pair)
            {
                this.pair = pair;
            }

            /// <summary>
            /// Gets the macro name in the macro name/value pair.
            /// </summary>
            public string MacroName
            {
                get { return this.pair.Key; }
            }

            /// <summary>
            /// Gets the value in the macro name/value pair.
            /// </summary>
            public string Value
            {
                get { return this.pair.Value; }
            }

            /// <summary>
            /// Converts a <see cref="KeyValuePair&lt;T, T&gt;">KeyValuePair&lt;string, string&gt;</see>
            /// to a <see cref="MacroNameValuePair"/>.
            /// </summary>
            /// <param name="source">The KeyValuePair&lt;string, string&gt; to convert.</param>
            /// <returns>The converted <see cref="MacroNameValuePair"/>.</returns>
            public static implicit operator MacroNameValuePair(KeyValuePair<string, string> source)
            {
                return new MacroNameValuePair(source);
            }
        }
    }
}
