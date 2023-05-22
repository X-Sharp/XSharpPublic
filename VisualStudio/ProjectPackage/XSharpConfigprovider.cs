//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

using System.IO;
using MSBuild = Microsoft.Build.Evaluation;
using MSBuildExecution = Microsoft.Build.Execution;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using XSharpModel;
using Community.VisualStudio.Toolkit;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ConfigProvider
    /// </summary>
    ///
    internal class XSharpConfigProvider : ConfigProvider
    {
        public XSharpConfigProvider(XSharpProjectNode manager) : base(manager)
        {
        }

        protected override ProjectConfig CreateProjectConfiguration(ConfigCanonicalName canonicalName)
        {
            return new XSharpProjectConfig(base.ProjectMgr, canonicalName);
        }

        public override int GetCfgOfName(string name, string platName, out IVsCfg cfg)
        {
            if (name.IndexOf("|") >= 0)
            {
                var elements = name.Split("|".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                name = elements[0];
                platName = elements[1];
            }
            ThreadHelper.ThrowIfNotOnUIThread();
            return base.GetCfgOfName(name, platName, out cfg);

        }
    }

    internal class XSharpProjectConfig : DebuggableProjectConfig, IVsDebuggableProjectCfg2

    {
        private XSharpProjectNode _project;
        internal XSharpProjectConfig(ProjectNode project, ConfigCanonicalName configuration) : base(project, configuration)
        {
            _project = project as XSharpProjectNode;
        }
        public override string GetConfigurationProperty(string propertyName, bool resetCache)
        {
            string result = base.GetConfigurationProperty(propertyName, resetCache);
            if (string.IsNullOrEmpty(result))
            {
                foreach (MSBuild.ProjectProperty prop in this._project.BuildProject.Properties)
                {
                    if (prop.Name == propertyName)
                    {
                        // This triggers the evaluation of the property
                        try
                        {
                            if (!prop.IsImported)
                            {
                                prop.UnevaluatedValue = prop.UnevaluatedValue;
                            }
                            result = prop.EvaluatedValue;
                        }
                        catch (Exception)
                        {
                            result = null;
                        }

                        break;
                    }
                }
            }


            return result;
        }

        public override int get_CfgType(ref Guid iidCfg, out IntPtr ppCfg)
        {
            ppCfg = IntPtr.Zero;

            ThreadHelper.ThrowIfNotOnUIThread();
            if (iidCfg == typeof(IVsDebuggableProjectCfg2).GUID)
            {
                ppCfg = Marshal.GetComInterfaceForObject(this, typeof(IVsDebuggableProjectCfg2));
            }

            // If supported
            if (ppCfg != IntPtr.Zero)
                return VSConstants.S_OK;

            return base.get_CfgType(ref iidCfg, out ppCfg);
        }

        public int OnBeforeDebugLaunch(uint grfLaunch)
        {
            return VSConstants.S_OK;
        }

        public void StoreDebuggerOptions()
        {
            var project = this.ProjectMgr as XSharpProjectNode;
            if (project == null)
                return;
            XDebuggerSettings.Dialect = (int) project.Dialect;
            XDebuggerSettings.ArrayZero = project.GetLogicProjectProperty(XSharpProjectFileConstants.AZ);
            XDebuggerSettings.Vo4 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo4);
            XDebuggerSettings.Vo6 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo6);
            XDebuggerSettings.Vo7 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo7);
            XDebuggerSettings.Vo10 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo10);
            XDebuggerSettings.Vo12 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo12);
            XDebuggerSettings.Vo13 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo13);
            XDebuggerSettings.Vo14 = project.GetLogicProjectProperty(XSharpProjectFileConstants.Vo14);
            XDebuggerSettings.MemVars = project.GetLogicProjectProperty(XSharpProjectFileConstants.MemVar);
            XDebuggerSettings.UndeclaredMemvars = project.GetLogicProjectProperty(XSharpProjectFileConstants.Undeclared);
            XDebuggerSettings.LateBinding = project.GetLogicProjectProperty(XSharpProjectFileConstants.LB);
            XDebuggerSettings.CaseSensitive = project.GetLogicProjectProperty(XSharpProjectFileConstants.CS);
            XDebuggerSettings.DebuggingXSharpExe = true;

        }
        public override int DebugLaunch(uint grfLaunch)
        {
            CCITracing.TraceCall();

            try
            {
                StoreDebuggerOptions();
                if (grfLaunch == 0)
                    grfLaunch = (uint)__VSDBGLAUNCHFLAGS.DBGLAUNCH_Silent;
                VsDebugTargetInfo info = new VsDebugTargetInfo();
                info.cbSize = (uint)Marshal.SizeOf(info);
                info.dlo = DEBUG_LAUNCH_OPERATION.DLO_CreateProcess;

                // On first call, reset the cache, following calls will use the cached values

                string property = GetConfigurationProperty(XSharpProjectFileConstants.DebuggerCommand, true);
                if (string.IsNullOrEmpty(property))
                {
                    property = this._project.GetOutputAssembly(this.ConfigCanonicalName);
                }
                info.bstrExe = property;

                property = GetConfigurationProperty(XSharpProjectFileConstants.DebuggerWorkingDirectory, false);
                if (string.IsNullOrEmpty(property))
                {
                    property = Path.GetDirectoryName(info.bstrExe);
                }
                if (!Path.IsPathRooted(property))
                {
                    property = Path.Combine(this.ProjectMgr.ProjectFolder, property);
                }
                info.bstrCurDir = property;

                property = GetConfigurationProperty(XSharpProjectFileConstants.DebuggerCommandArguments, false);
                if (!string.IsNullOrEmpty(property))
                {
                    info.bstrArg = property;
                }

                property = GetConfigurationProperty("RemoteDebugMachine", false);
                if (property != null && property.Length > 0)
                {
                    info.bstrRemoteMachine = property;
                }

                property = GetConfigurationProperty("RedirectToOutputWindow", false);
                if (property != null && string.Compare(property, "true", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    info.fSendStdoutToOutputWindow = 1;
                }
                else
                {
                    info.fSendStdoutToOutputWindow = 0;
                }

                if (string.IsNullOrEmpty(info.bstrRemoteMachine))
                {
                    if (!File.Exists(info.bstrExe))
                    {
                        VS.MessageBox.ShowError($"The program does not exist: '{info.bstrExe}'.");
                        return VSConstants.S_FALSE;
                    }
                    if (!Directory.Exists(info.bstrCurDir))
                    {
                        VS.MessageBox.ShowError($"The working directory does not exist: '{info.bstrCurDir}'.");
                        return VSConstants.S_FALSE;
                    }
                }


                property = GetConfigurationProperty(XSharpProjectFileConstants.EnableUnmanagedDebugging, false);
                if (property != null && string.Compare(property, "true", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    info.clsidCustom = VSConstants.DebugEnginesGuids.ManagedAndNative_guid; // {92EF0900-2251-11D2-B72E-0000F87572EF}
                }
                else
                {
                    info.clsidCustom = VSConstants.DebugEnginesGuids.ManagedOnly_guid;      // {449EC4CC-30D2-4032-9256-EE18EB41B62B}
                }
                if (!string.IsNullOrEmpty(this.ProjectMgr.BuildProject.Xml.Sdk))
                {
                    // Sdk style project
                    info.clsidCustom = VSConstants.DebugEnginesGuids.CoreSystemClr_guid;
                }

                info.grfLaunch = grfLaunch;
                VsShellUtilities.LaunchDebugger(this._project.Site, info);
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "DebugLaunch");

                return Marshal.GetHRForException(e);
            }

            return VSConstants.S_OK;
        }

        protected override OutputGroup CreateOutputGroup(ProjectNode project, KeyValuePair<string, string> group)
        {
            OutputGroup outputGroup = new XSharpOutputGroup(group.Key, group.Value, project, this);
            return outputGroup;
        }
    }

    internal class XSharpOutputGroup : OutputGroup
    {
        internal XSharpOutputGroup(string outputName, string msBuildTargetName, ProjectNode projectManager, ProjectConfig configuration)
            : base(outputName, msBuildTargetName, projectManager, configuration)
        {

        }

        // added to make sure our key output is picked up properly
        protected override void Refresh()
        {
            // Let MSBuild know which configuration we are working with
            this.Project.SetConfiguration(this.ProjectCfg.ConfigCanonicalName);
            ThreadHelper.ThrowIfNotOnUIThread();

            // Generate dependencies if such a task exist
            if (this.Project.ProjectInstance.Targets.ContainsKey(ProjectFileConstants.AllProjectOutputGroups))
            {
                bool succeeded = false;
                this.Project.BuildTarget(ProjectFileConstants.AllProjectOutputGroups, out succeeded);
                if (!succeeded)
                {
                    if (System.Diagnostics.Debugger.IsAttached)
                        Debug.WriteLine("Failed to build target {0}", this.TargetName);
                    this.Outputs.Clear();
                    return;
                }
            }
            // Rebuild the content of our list of output
            string outputType = this.TargetName + "Output";
            if (TargetName == "BuiltProjectOutputGroup")
                outputType = this.TargetName + "KeyOutput";
            this.Outputs.Clear();
            foreach (MSBuildExecution.ProjectItemInstance item in MSBuildProjectInstance.GetItems(this.Project.ProjectInstance, outputType))
            {
                Output output = new Output(this.Project, item);
                this.Outputs.Add(output);

                // See if it is our key output
                if (String.Compare(MSBuildItem.GetMetadataValue(item, "IsKeyOutput"), true.ToString(), StringComparison.OrdinalIgnoreCase) == 0)
                    KeyOutput = output;
            }

            this.Project.SetCurrentConfiguration();

            // Now that the group is built we have to check if it is invalidated by a property
            // change on the project.
            this.Project.OnProjectPropertyChanged += new EventHandler<ProjectPropertyChangedArgs>(OnProjectPropertyChanged);
        }
    }
}
