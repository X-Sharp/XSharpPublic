//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Settings;

namespace XSharp.ProjectSystem.Selection
{
    /// <summary>
    /// Chooses the project system for projects with the X# (MPFproj) project type GUID {AA6C8D78-...}:
    /// SDK-style projects are loaded by the CPS project type, all other (legacy) projects by MPFproj.
    /// </summary>
    /// <remarks>
    /// Same mechanism as the managed project system uses for C#/VB/F# (FSharpProjectSelector): the selector is
    /// registered for the legacy project type GUID (pkgdef "Projects\{guid}\ProjectSelector", see
    /// <see cref="ProvideProjectSelectorAttribute"/>), so existing solution files keep their project type GUID.
    /// Solutions that use the CPS project type GUID directly do not go through the selector.
    /// Any problem (unreadable project file, routing switched off) falls back to MPFproj, the project system
    /// that loaded these projects before.
    /// </remarks>
    [Guid(XSharpConstants.guidProjectSelectorString)]
    internal sealed class XSharpProjectSelector : IVsProjectSelector
    {
        private const string MSBuildXmlNamespace = "http://schemas.microsoft.com/developer/msbuild/2003";

        private readonly Func<bool> useCpsForSdkProjects;

        public XSharpProjectSelector(Func<bool> useCpsForSdkProjects)
        {
            this.useCpsForSdkProjects = useCpsForSdkProjects;
        }

        public void GetProjectFactoryGuid(Guid guidProjectType, string pszFilename, out Guid guidProjectFactory)
        {
            guidProjectFactory = XSharpConstants.guidXSharpProjectFactory;
            try
            {
                if (useCpsForSdkProjects() && IsSdkProject(pszFilename))
                {
                    guidProjectFactory = XSharpConstants.guidCpsProjectType;
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
                guidProjectFactory = XSharpConstants.guidXSharpProjectFactory;
            }
            XSettings.Information("XSharpProjectSelector: " + pszFilename + " -> " +
                (guidProjectFactory == XSharpConstants.guidCpsProjectType ? "CPS" : "MPFproj"));
        }

        /// <summary>
        /// True for SDK-style project files: &lt;Project Sdk="..."&gt;, &lt;Import Sdk="..." /&gt; or &lt;Sdk Name="..." /&gt;.
        /// </summary>
        internal static bool IsSdkProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            XDocument document;
            try
            {
                document = XDocument.Load(fileName);
            }
            catch (Exception)
            {
                // Not (yet) a valid project file, e.g. a new project that is still being created
                return false;
            }
            return IsSdkProject(document);
        }

        internal static bool IsSdkProject(XDocument document)
        {
            var namespaces = new XmlNamespaceManager(new NameTable());
            namespaces.AddNamespace("msb", MSBuildXmlNamespace);
            return document.XPathSelectElement("/msb:Project[@Sdk]", namespaces) != null
                || document.XPathSelectElement("/Project[@Sdk]") != null
                || document.XPathSelectElement("/*/msb:Import[@Sdk]", namespaces) != null
                || document.XPathSelectElement("/*/Import[@Sdk]") != null
                || document.XPathSelectElement("/*/msb:Sdk[@Name]", namespaces) != null
                || document.XPathSelectElement("/*/Sdk[@Name]") != null;
        }
    }
}
