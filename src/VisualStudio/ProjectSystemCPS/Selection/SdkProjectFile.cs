//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;

namespace XSharp.ProjectSystem.Selection
{
    /// <summary>
    /// Recognizes SDK-style project files. Used by <see cref="XSharpProjectSelector"/> and by the MPFproj project
    /// factory, which rejects SDK-style projects.
    /// </summary>
    public static class SdkProjectFile
    {
        private const string MSBuildXmlNamespace = "http://schemas.microsoft.com/developer/msbuild/2003";

        /// <summary>
        /// True for SDK-style project files: &lt;Project Sdk="..."&gt;, &lt;Import Sdk="..." /&gt; or &lt;Sdk Name="..." /&gt;.
        /// </summary>
        public static bool IsSdkProject(string fileName)
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
