//===============================================================================================================
// System  : Sandcastle Help File Builder Plug-Ins
// File    : AdditionalReferenceLinksPlugIn.cs
// Author  : Eric Woodruff  (Eric@EWoodruff.us)
// Updated : 12/22/2015
// Note    : Copyright 2008-2015, Eric Woodruff, All rights reserved
// Compiler: Microsoft Visual C#
//
// This file contains a plug-in designed to add additional reference link targets to the Reflection Index Data
// and Resolve Reference Links build components so that links can be created to other third party help in a
// help collection or additional online content.
//
// This code is published under the Microsoft Public License (Ms-PL).  A copy of the license should be
// distributed with the code and can be found at the project website: https://GitHub.com/EWSoftware/SHFB.  This
// notice, the author's name, and all copyright notices must remain intact in all applications, documentation,
// and source files.
//
//    Date     Who  Comments
// ==============================================================================================================
// 02/25/2008  EFW  Created the code
// 08/13/2008  EFW  Updated to support the new project format
// 06/07/2010  EFW  Added support for multi-format build output
// 01/01/2013  EFW  Updated for use with the new cached build components.  Added code to insert the reflection
//                  file names into the GenerateInheritedDocs tool configuration file.
// 12/17/2013  EFW  Updated to use MEF for the plug-ins
//===============================================================================================================

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using System.Xml;
using System.Xml.XPath;

using Sandcastle.Core;

using SandcastleBuilder.Utils;
using SandcastleBuilder.Utils.BuildComponent;
using SandcastleBuilder.Utils.BuildEngine;

namespace SandcastleBuilder.PlugIns
{
    /// <summary>
    /// This plug-in class is designed to add additional reference link targets to the <strong>Reflection Index
    /// Data</strong> and <strong>Resolve Reference Links</strong> build components so that links can be created
    /// to other third party help in a help collection or additional online content.
    /// </summary>
    [HelpFileBuilderPlugInExport("XSharp Function Output", IsConfigurable = false,
      Version = "2020.01.28", Copyright = "Copyright © XSharp BV 2015-2020",
      Description = "This plug-in is used to convert the documentation for X# functions from static methods in the functions class to 'normal' function documentation.")]
    public sealed class XSharpFunctionOutput : IPlugIn, IProgress<BuildProgressEventArgs>
    {
        #region Private data members
        //=====================================================================

        private List<ExecutionPoint> executionPoints;

        private BuildProcess builder;

        #endregion

        #region IPlugIn implementation
        //=====================================================================

        /// <summary>
        /// This read-only property returns a collection of execution points that define when the plug-in should
        /// be invoked during the build process.
        /// </summary>
        public IEnumerable<ExecutionPoint> ExecutionPoints
        {
            get
            {
                if(executionPoints == null)
                    executionPoints = new List<ExecutionPoint>
                    {
                        new ExecutionPoint(BuildStep.GenerateHelpProject, ExecutionBehaviors.After),
                        new ExecutionPoint(BuildStep.CopyingWebsiteFiles, ExecutionBehaviors.After),
                    };

                return executionPoints;
            }
        }

        /// <summary>
        /// This method is used by the Sandcastle Help File Builder to let the plug-in perform its own
        /// configuration.
        /// </summary>
        /// <param name="project">A reference to the active project</param>
        /// <param name="currentConfig">The current configuration XML fragment</param>
        /// <returns>A string containing the new configuration XML fragment</returns>
        /// <remarks>The configuration data will be stored in the help file builder project.</remarks>
        public string ConfigurePlugIn(SandcastleProject project, string currentConfig)
        {
            //using(AdditionalReferenceLinksConfigDlg dlg = new AdditionalReferenceLinksConfigDlg(project, currentConfig))
            //{
            //    if(dlg.ShowDialog() == DialogResult.OK)
            //        currentConfig = dlg.Configuration;
            //}

            return currentConfig;
        }

        /// <summary>
        /// This method is used to initialize the plug-in at the start of the build process.
        /// </summary>
        /// <param name="buildProcess">A reference to the current build process.</param>
        /// <param name="configuration">The configuration data that the plug-in should use to initialize
        /// itself.</param>
        /// <exception cref="BuilderException">This is thrown if the plug-in configuration is not valid.</exception>
        public void Initialize(BuildProcess buildProcess, XPathNavigator configuration)
        {

            builder = buildProcess;
            otherLinks = new ReferenceLinkSettingsCollection();

            var metadata = (HelpFileBuilderPlugInExportAttribute)this.GetType().GetCustomAttributes(
                typeof(HelpFileBuilderPlugInExportAttribute), false).First();

            builder.ReportProgress("{0} Version {1}\r\n{2}", metadata.Id, metadata.Version, metadata.Copyright);

 
         }

        /// <summary>
        /// This method is used to execute the plug-in during the build process
        /// </summary>
        /// <param name="context">The current execution context</param>
        public void Execute(ExecutionContext context)
        {
            //string workingPath, configFilename;
            //bool success;

            if(context.BuildStep == BuildStep.GenerateHelpProject)
            {
                // Merge the additional reference links information
                if (builder.CurrentFormat == HelpFileFormats.HtmlHelp1)
                {
                    builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for local help");
                    XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                }
                if(builder.CurrentFormat == HelpFileFormats.MSHelpViewer)
                {
                    builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for local help");
                    XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                }
                return;
            }
            if(context.BuildStep == BuildStep.CopyingWebsiteFiles)
            {
                // Merge the additional reference links information
                builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for website");
                XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                return;
            }

        }


        #endregion

        #region IDisposable implementation
        //=====================================================================

        /// <summary>
        /// This implements the Dispose() interface to properly dispose of the plug-in object.
        /// </summary>
        public void Dispose()
        {
            // Nothing to dispose of in this one
            GC.SuppressFinalize(this);
        }
        #endregion

 
        #region IProgress<BuildProgressEventArgs> Members
        //=====================================================================

        /// <summary>
        /// This is called by the build process to report build progress for the reference link projects
        /// </summary>
        /// <param name="value">The event arguments</param>
        /// <remarks>Since the build is synchronous in this plug-in, we need to implement the interface and
        /// report progress synchronously as well or the final few messages can get lost and it looks like the
        /// build failed.</remarks>
        public void Report(BuildProgressEventArgs value)
        {
            if(value.StepChanged)
                builder.ReportProgress(value.BuildStep.ToString());
        }
        #endregion
    }
}
