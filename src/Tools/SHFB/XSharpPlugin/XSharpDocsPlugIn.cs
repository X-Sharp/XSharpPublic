//===============================================================================================================
// System  : Sandcastle Help File Builder Presentation Style Mods Plug-In
// File    : PresentationStyleModsPlugIn.cs
// Author  : Eric Woodruff  (Eric@EWoodruff.us)
// Updated : 09/30/2025
// Note    : Copyright 2022-2025, Eric Woodruff, All rights reserved
//
// This file contains the class used to demonstrate how a presentation style can be extended in various ways
// without having to clone and implement an entirely new presentation style.
//
// This code is published under the Microsoft Public License (Ms-PL).  A copy of the license should be
// distributed with the code and can be found at the project website:
// https://GitHub.com/EWSoftware/PresentationStyleModsPlugIn.  This notice, the author's name, and all copyright
// notices must remain intact in all applications, documentation, and source files.
//
//    Date     Who  Comments
// ==============================================================================================================
// 08/06/2022  EFW  Created the code
//===============================================================================================================

// Ignore Spelling: stylesheet href html

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using System.Xml.XPath;

using Sandcastle.Core.BuildEngine;
using Sandcastle.Core.PlugIn;
using Sandcastle.Core.Project;

namespace XSharpDocs;

/// <summary>
/// This plug-in is used to demonstrate how a presentation style can be extended in various ways without
/// having to clone and implement an entirely new presentation style.
/// </summary>
/// <remarks>These are just some test remarks.  This section will be moved by the plug-in.</remarks>
/// <customSection>
/// <para>This is an example of a custom XML comments section.  Our plug-in will render this section
/// including any <customElement style="Style1">custom elements</customElement> that are present.</para>
///
/// <para>The custom section can itself contain other standard XML comment elements.</para>
///
/// <list type="bullet">
///     <item>An example of the custom element <customElement style="Style1">style 1</customElement>.</item>
///     <item>An example of the custom element <customElement style="Style2">style 2</customElement>.</item>
///     <item>An example of the custom element <customElement style="Style3">style 3</customElement>.</item>
/// </list>
/// </customSection>
[HelpFileBuilderPlugInExport("XSharp Doc Changer",
    Version = XSharp.Constants.ProductVersion, Copyright = XSharp.Constants.Copyright,
        Description = "This plug-in is used to convert the documentation for X# functions from static methods in the functions class to 'normal' function documentation.")]

public sealed class XSharpDocsPlugIn : IPlugIn
{
    #region Private data members
    //=====================================================================

    private IBuildProcess builder;

    #endregion

    #region IPlugIn implementation
    //=====================================================================

    /// <summary>
    /// This read-only property returns a collection of execution points that define when the plug-in should
    /// be invoked during the build process.
    /// </summary>
    public IEnumerable<ExecutionPoint> ExecutionPoints { get; } =
    [
        new ExecutionPoint(BuildStep.BuildTopics, ExecutionBehaviors.After),
        new ExecutionPoint(BuildStep.CompilingHelpFile, ExecutionBehaviors.Before),
        new ExecutionPoint(BuildStep.GenerateIntermediateTableOfContents, ExecutionBehaviors.After),
        new ExecutionPoint(BuildStep.GenerateFullTextIndex, ExecutionBehaviors.Before,10000)

    ];

    /// <summary>
    /// This method is used to initialize the plug-in at the start of the build process
    /// </summary>
    /// <param name="buildProcess">A reference to the current build process</param>
    /// <param name="configuration">The configuration data that the plug-in should use to initialize itself</param>
    public void Initialize(IBuildProcess buildProcess, XElement configuration)
    {
        builder = buildProcess;

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
        if(context == null)
            throw new ArgumentNullException(nameof(context));

        if(context.BuildStep == BuildStep.CompilingHelpFile)
        {
            // Merge the additional reference links information
            if(builder.CurrentFormat == HelpFileFormats.HtmlHelp1)
            {
                builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for local help");
                XSharpDocChanger.Convert(builder, builder.CurrentFormat);
            }
            if(builder.CurrentFormat == HelpFileFormats.MSHelpViewer)
            {
                builder.ReportProgress("Adjusting generated help topics for X# Functions and procedures for ms help viewer");
                XSharpDocChanger.Convert(builder, builder.CurrentFormat);
            }
            return;
        }
        if(context.BuildStep == BuildStep.GenerateIntermediateTableOfContents)
        {
            ;
        }
        if(context.BuildStep == BuildStep.GenerateFullTextIndex)
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
        foreach(var element in elements)
        {
            var attr = element.Attribute("Url");
            if(attr != null)
            {
                var url = attr.Value;
                var path = Path.Combine(websitePath, url);
                if(File.Exists(path))
                {
                    // Load the title from the topic page
                    XDocument topic = XDocument.Load(path);
                    var titleElement = topic.XPathSelectElement("//title");
                    attr = element.Attribute("Title");
                    if(attr != null)
                    {
                        var newTitle = titleElement.Value;
                        var title = attr.Value;
                        var asmName = "";
                        var filename = Path.GetFileName(path);
                        if(path.IndexOf("_Functions_", StringComparison.OrdinalIgnoreCase) > 0 && title.IndexOf("Method", StringComparison.OrdinalIgnoreCase) > 0)
                        {
                            asmName = XSharpDocChanger.fileNameToAssemblyName(filename);
                        }
                        if(!string.IsNullOrEmpty(asmName))
                        {
                            if(!newTitle.EndsWith(" "))
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

    /// <summary>
    /// This handles garbage collection to ensure proper disposal of the plug-in if not done explicitly
    /// with <see cref="Dispose()"/>.
    /// </summary>
    ~XSharpDocsPlugIn()
    {
        // If the plug-in hasn't got any disposable resources, this finalizer can be removed
        this.Dispose();
    }

    /// <summary>
    /// This implements the Dispose() interface to properly dispose of the plug-in object
    /// </summary>
    public void Dispose()
    {
        // Dispose of any resources here if necessary
        GC.SuppressFinalize(this);
    }
    #endregion


}
