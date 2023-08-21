using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Xml.XPath;

using SandcastleBuilder.Utils;
using Sandcastle.Core;
using SandcastleBuilder.Utils.BuildEngine;
using SandcastleBuilder.Utils.BuildComponent;
using System.Xml.Linq;
using System.IO;

// Search for "TODO" to find changes that you need to make to this plug-in template.

namespace XSharpDocs
{
    /// <summary>
    /// TODO: Set your plug-in's unique ID and description in the export attribute below.
    /// </summary>
    /// <remarks>The <c>HelpFileBuilderPlugInExportAttribute</c> is used to export your plug-in so that the help
    /// file builder finds it and can make use of it.  The example below shows the basic usage for a common
    /// plug-in.  Set the additional attribute values as needed:
    ///
    /// <list type="bullet">
    ///     <item>
    ///         <term>IsConfigurable</term>
    ///         <description>Set this to true if your plug-in contains configurable settings.  The
    /// <c>ConfigurePlugIn</c> method will be called to let the user change the settings.</description>
    ///     </item>
    ///     <item>
    ///         <term>RunsInPartialBuild</term>
    ///         <description>Set this to true if your plug-in should run in partial builds used to generate
    /// reflection data for the API Filter editor dialog or namespace comments used for the Namespace Comments
    /// editor dialog.  Typically, this is left set to false.</description>
    ///     </item>
    /// </list>
    ///
    /// Plug-ins are singletons in nature.  The composition container will create instances as needed and will
    /// dispose of them when the container is disposed of.</remarks>
    [HelpFileBuilderPlugInExport("XSharp Function Output", IsConfigurable = false,
    Version = XSharp.Constants.ProductVersion, Copyright = XSharp.Constants.Copyright,
        Description = "This plug-in is used to convert the documentation for X# functions from static methods in the functions class to 'normal' function documentation.")]

    public sealed class XSharpDocsPlugIn : IPlugIn
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
                if (executionPoints == null)
                    executionPoints = new List<ExecutionPoint>
                    {
                        new ExecutionPoint(BuildStep.BuildTopics, ExecutionBehaviors.After),
                        new ExecutionPoint(BuildStep.CompilingHelpFile, ExecutionBehaviors.Before),
                        new ExecutionPoint(BuildStep.GenerateHelpFormatTableOfContents, ExecutionBehaviors.After),
                        new ExecutionPoint(BuildStep.GenerateFullTextIndex, ExecutionBehaviors.Before,10000)

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
        /// <remarks>The configuration data will be stored in the help file builder project</remarks>
        public string ConfigurePlugIn(SandcastleProject project, string currentConfig)
        {
            // TODO: Add and invoke a configuration dialog if you need one.  You will also need to set the
            // IsConfigurable property to true on the class's export attribute.
            MessageBox.Show("This plug-in has no configurable settings", "Build Process Plug-In",
                MessageBoxButtons.OK, MessageBoxIcon.Information);

            return currentConfig;
        }

        /// <summary>
        /// This method is used to initialize the plug-in at the start of the build process
        /// </summary>
        /// <param name="buildProcess">A reference to the current build process</param>
        /// <param name="configuration">The configuration data that the plug-in should use to initialize itself</param>
        public void Initialize(BuildProcess buildProcess, XPathNavigator configuration)
        {
            builder = buildProcess;

            var metadata = (HelpFileBuilderPlugInExportAttribute)this.GetType().GetCustomAttributes(
                typeof(HelpFileBuilderPlugInExportAttribute), false).First();

            builder.ReportProgress("{0} Version {1}\r\n{2}", metadata.Id, metadata.Version, metadata.Copyright);

            // TODO: Add your initialization code here such as reading the configuration data
        }

        /// <summary>
        /// This method is used to execute the plug-in during the build process
        /// </summary>
        /// <param name="context">The current execution context</param>
        public void Execute(ExecutionContext context)
        {
            if (context.BuildStep == BuildStep.CompilingHelpFile)
            {
                // Merge the additional reference links information
                if (builder.CurrentFormat == HelpFileFormats.HtmlHelp1)
                {
                    builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for local help");
                    XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                }
                if (builder.CurrentFormat == HelpFileFormats.MSHelpViewer)
                {
                    builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for ms help viewer");
                    XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                }
                return;
            }
            if (context.BuildStep == BuildStep.GenerateHelpFormatTableOfContents)
            {
                ;
            }
            if (context.BuildStep == BuildStep.GenerateFullTextIndex)
            {
                // Merge the additional reference links information
                XSharpDocChanger.Convert(builder, builder.CurrentFormat);
                builder.ReportProgress("Adjusting web TOC for X# Functions and procedures for website");
                AdjustWebToc(context);
                return;
            }
        }


        void AdjustWebToc(ExecutionContext context)
        {
            // Load the web TOC generated by SandcastleHtmlExtract
            string file = Path.Combine(builder.WorkingFolder, "WebTOC.xml");
            XDocument webtoc = XDocument.Load(file);
            var elements = webtoc.XPathSelectElements("//*");
            var websitePath = builder.WorkingFolder + "Output\\Website";
            foreach (var element in elements)
            {
                var attr = element.Attribute("Url");
                if (attr != null)
                {
                    var url = attr.Value;
                    var path = Path.Combine(websitePath, url);
                    if (File.Exists(path))
                    {
                        // Load the title from the topic page
                        XDocument topic = XDocument.Load(path);
                        var titleElement = topic.XPathSelectElement("//title");
                        attr = element.Attribute("Title");
                        if (attr != null)
                        {
                            var newTitle = titleElement.Value;
                            var title = attr.Value;
                            var asmName = "";
                            var filename = Path.GetFileName(path);
                            if (path.IndexOf("_Functions_", StringComparison.OrdinalIgnoreCase) > 0 && title.IndexOf("Method", StringComparison.OrdinalIgnoreCase) > 0)
                            {
                                asmName = XSharpDocChanger.fileNameToAssemblyName(filename);
                            }
                            if (!string.IsNullOrEmpty(asmName))
                            {
                                if (!newTitle.EndsWith(" "))
                                    newTitle += " ";
                                newTitle += "(" + asmName + ")";
                            }

                            element.SetAttributeValue("Title", newTitle);
                        }
                    }
                }
            }
            webtoc.Save(file);
            return;
        }

        #endregion

        #region IDisposable implementation
        //=====================================================================

        // TODO: If the plug-in hasn't got any disposable resources, this finalizer can be removed
        /// <summary>
        /// This handles garbage collection to ensure proper disposal of the plug-in if not done explicitly
        /// with <see cref="Dispose()"/>.
        /// </summary>
        ~XSharpDocsPlugIn()
        {
            this.Dispose();
        }

        /// <summary>
        /// This implements the Dispose() interface to properly dispose of the plug-in object
        /// </summary>
        public void Dispose()
        {
            // TODO: Dispose of any resources here if necessary
            GC.SuppressFinalize(this);
        }
        #endregion
    }
}
