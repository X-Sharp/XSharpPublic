using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading.Tasks;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Debug;
using Microsoft.VisualStudio.ProjectSystem.Properties;
using Microsoft.VisualStudio.ProjectSystem.VS.Debug;
using XSharp;
namespace XSharp.ProjectSystem
{
    [ExportDebugger(XSharpDebugger.SchemaName)]
    [AppliesTo(MyUnconfiguredProject.UniqueCapability)]
    public class XSharpDebuggerLaunchProvider : DebugLaunchProviderBase
    {
        // 
        [ExportPropertyXamlRuleDefinition("XSharpProjectCPS, Version=" + Constants.Version + ", Culture=neutral, PublicKeyToken=9be6e469bc4921f1", "XamlRuleToCode:PropertyPage_Debugger.xaml", "Project")]
        [AppliesTo(MyUnconfiguredProject.UniqueCapability)]
        private object DebuggerXaml { get { throw new NotImplementedException(); } }

        [ImportingConstructor]
        public XSharpDebuggerLaunchProvider(ConfiguredProject configuredProject)
            : base(configuredProject)
        {
        }

        /// <summary>
        /// Gets project properties that the debugger needs to launch.
        /// </summary>
        [Import]
        private ProjectProperties DebuggerProperties { get; set; }

        public override async Task<bool> CanLaunchAsync(DebugLaunchOptions launchOptions)
        {      
            var properties = await DebuggerProperties.GetXSharpDebuggerPropertiesAsync();
            string commandValue = await properties.XSharpDebuggerCommand.GetEvaluatedValueAtEndAsync();
            return !string.IsNullOrEmpty(commandValue);
        }

        public override async Task<IReadOnlyList<IDebugLaunchSettings>> QueryDebugTargetsAsync(DebugLaunchOptions launchOptions)
        {
            var settings = new DebugLaunchSettings(launchOptions);

            // The properties that are available via DebuggerProperties are determined by the property XAML files in your project.
            var debuggerProperties = await DebuggerProperties.GetXSharpDebuggerPropertiesAsync();
            settings.CurrentDirectory = await debuggerProperties.XSharpDebuggerWorkingDirectory.GetEvaluatedValueAtEndAsync();
            settings.Executable = await debuggerProperties.XSharpDebuggerCommand.GetEvaluatedValueAtEndAsync();
            settings.Arguments = await debuggerProperties.XSharpDebuggerCommandArguments.GetEvaluatedValueAtEndAsync();
            settings.LaunchOperation = DebugLaunchOperation.CreateProcess;

            // TODO: Specify the right debugger engine
            settings.LaunchDebugEngineGuid = DebuggerEngines.ManagedOnlyEngine;

            return new IDebugLaunchSettings[] { settings };
        }
    }
}
