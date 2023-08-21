//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading.Tasks;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Debug;
using Microsoft.VisualStudio.ProjectSystem.Properties;
using Microsoft.VisualStudio.ProjectSystem.VS.Debug;
namespace XSharp.ProjectSystem
{
    [ExportDebugger(XSharpDebugger.SchemaName)]
    [AppliesTo(XSharpConstants.LanguageName)]
    public class XSharpDebuggerLaunchProvider : DebugLaunchProviderBase
    {
        [ExportPropertyXamlRuleDefinition("XSharpProjectCPS, Version=" + Constants.Version + ", Culture=neutral, PublicKeyToken="+ Constants.PublicKey,"XamlRuleToCode:PropertyPage_Debugger.xaml", "Project")]
        [AppliesTo(XSharpConstants.LanguageName)]
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
