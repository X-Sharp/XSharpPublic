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
using System.Runtime.InteropServices;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ConfigProvider
    /// </summary>
    /// 
    internal class XSharpConfigProvider : ConfigProvider
    {
        public XSharpConfigProvider(ProjectNode manager) : base(manager)
        {
        }

        protected override ProjectConfig CreateProjectConfiguration(string configName)
        {
            return new XSharpProjectConfig(base.ProjectMgr, configName);
        }
    }

    internal class XSharpProjectConfig : ProjectConfig
    {
        private ProjectNode _project;
        internal XSharpProjectConfig(ProjectNode project, string configuration) : base(project, configuration)
        {
            _project = project;
        }
        public override string GetConfigurationProperty(string propertyName, bool resetCache)
        {
            string result = base.GetConfigurationProperty(propertyName, resetCache);
            if (String.IsNullOrEmpty(result))
            {
                foreach (MSBuild.ProjectProperty prop in this._project.BuildProject.Properties)
                {
                    if (prop.Name == propertyName)
                    {
                        // This triggers the evaluation of the property
                        prop.UnevaluatedValue = prop.UnevaluatedValue;
                        result = prop.EvaluatedValue;
                        break;
                    }
                }
            }


            return result;
        }
        public override int DebugLaunch(uint grfLaunch)
        {
            CCITracing.TraceCall();

            try
            {
                VsDebugTargetInfo info = new VsDebugTargetInfo();
                info.cbSize = (uint)Marshal.SizeOf(info);
                info.dlo = Microsoft.VisualStudio.Shell.Interop.DEBUG_LAUNCH_OPERATION.DLO_CreateProcess;

                // On first call, reset the cache, following calls will use the cached values
                string property = GetConfigurationProperty("DebuggerCommand", true);
                if (string.IsNullOrEmpty(property))
                {
                    property = this._project.GetOutputAssembly(this.ConfigName);
                }
                info.bstrExe = property;

                property = GetConfigurationProperty("DebuggerWorkingDirectory", false);
                if (string.IsNullOrEmpty(property))
                {
                    property = Path.GetDirectoryName(info.bstrExe);
                }
                info.bstrCurDir = property;

                property = GetConfigurationProperty("DebuggerCommandArguments", false);
                if (!string.IsNullOrEmpty(property))
                {
                    info.bstrArg = property;
                }

                property = GetConfigurationProperty("RemoteDebugMachine", false);
                if (property != null && property.Length > 0)
                {
                    info.bstrRemoteMachine = property;
                }

                info.fSendStdoutToOutputWindow = 0;

                property = GetConfigurationProperty("EnableUnmanagedDebugging", false);
                if (property != null && string.Compare(property, "true", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    //Set the unmanged debugger
                    //TODO change to vsconstant when it is available in VsConstants (guidNativeOnlyEng was the old name, maybe it has got a new name)
                    info.clsidCustom = new Guid("{92EF0900-2251-11D2-B72E-0000F87572EF}"); // VSConstants2.guidCOMPlusNativeEng;
                    //info.clsidCustom = new Guid("{3B476D35-A401-11D2-AAD4-00C04F990171}");
                }
                else
                {
                    //Set the managed debugger
                    info.clsidCustom = VSConstants.CLSID_ComPlusOnlyDebugEngine;
                }
                info.grfLaunch = grfLaunch;
                VsShellUtilities.LaunchDebugger(this._project.Site, info);
            }
            catch (Exception e)
            {
                Trace.WriteLine("Exception : " + e.Message);

                return Marshal.GetHRForException(e);
            }

            return VSConstants.S_OK;
        }
    }

}
