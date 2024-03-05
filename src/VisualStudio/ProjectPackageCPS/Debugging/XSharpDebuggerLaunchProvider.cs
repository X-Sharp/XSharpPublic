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
using Microsoft.VisualStudio.Shell;
namespace XSharp.VisualStudio.ProjectSystem
{
    [ExportDebugger(Debugger.SchemaName)]
    [AppliesTo(XSharpConstants.LanguageName)]
    public class XSharpDebuggerLaunchProvider : DebugLaunchProviderBase
    {
        [ExportPropertyXamlRuleDefinition("XSharp.VisualStudio.ProjectSystem, Version=" + Constants.Version + ", Culture=neutral, PublicKeyToken="+ Constants.PublicKey,"XamlRuleToCode:PropertyPage_Debugger.xaml", "Project")]
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
        private ProjectProperties ProjectProperties { get; set; }

        public override async Task<bool> CanLaunchAsync(DebugLaunchOptions launchOptions)
        {
            var properties = await ProjectProperties.GetDebuggerPropertiesAsync();
            var exe = await properties.DebuggerCommand.GetEvaluatedValueAsync();
            return !string.IsNullOrEmpty(exe) ;
        }
        public override async Task<IReadOnlyList<IDebugLaunchSettings>> QueryDebugTargetsAsync(DebugLaunchOptions launchOptions)
        {
            // The properties that are available via DebuggerProperties are determined by the property XAML files in your project.
            var settings = new DebugLaunchSettings(launchOptions);

            var debuggerProperties = await ProjectProperties.GetDebuggerPropertiesAsync();
            settings.CurrentDirectory = await debuggerProperties.DebuggerWorkingDirectory.GetEvaluatedValueAtEndAsync();
            settings.Executable = await debuggerProperties.DebuggerCommand.GetEvaluatedValueAtEndAsync();
            var ext = System.IO.Path.GetExtension(settings.Executable).ToUpper();
            if (ext == ".DLL")
            {
                settings.Executable = System.IO.Path.ChangeExtension(settings.Executable, ".EXE");
            }

            settings.Arguments = await debuggerProperties.DebuggerCommandArguments.GetEvaluatedValueAtEndAsync();
            settings.LaunchOperation = DebugLaunchOperation.CreateProcess;
            settings.Project = VsHierarchy;
            var props = this.ConfiguredProject.Services.ProjectPropertiesProvider.GetCommonProperties();
            //var propNames = await props.GetPropertyNamesAsync();
            //var propValues = new Dictionary<string,object>();
            //foreach (var name in propNames)
            //{
            //    propValues.Add( name, await props.GetEvaluatedPropertyValueAsync(name));
            //}
            var framework = await props.GetEvaluatedPropertyValueAsync(ConfigurationGeneralPage.TargetFrameworkIdentifierProperty);
            settings.LaunchDebugEngineGuid =
                IsDotNetCoreFramework(framework) ?
                DebuggerEngines.ManagedCoreEngine :
                DebuggerEngines.ManagedOnlyEngine;


            return new IDebugLaunchSettings[] { settings };
        }
        private static bool IsDotNetCoreFramework(string targetFramework)
        {
            const string NetStandardPrefix = ".NetStandard";
            const string NetCorePrefix = ".NetCore";
            return targetFramework.StartsWith(NetCorePrefix, StringComparison.OrdinalIgnoreCase) ||
                   targetFramework.StartsWith(NetStandardPrefix, StringComparison.OrdinalIgnoreCase);
        }

    }
}
