//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.ProjectSystem;
using XSharp.ProjectSystem.ShadowDesigner;

namespace XSharp.ProjectSystem.Imaging
{
    /// <summary>
    /// Sets the X# icons in Solution Explorer, like XSharpFileNode does in MPFproj: the project node, forms and
    /// user controls, the VO binaries and the other X# source files.
    /// </summary>
    /// <remarks>
    /// The managed project system provides the project icon through IProjectImageProvider, which is internal
    /// to it and only implemented for C#, VB and F#. IProjectTreePropertiesProvider is the public CPS way.
    /// A .prg is a form/user control when its SubType says so, or -- SDK-style projects rarely have a SubType --
    /// when it has a matching .Designer.prg and its class inherits from Form/UserControl (same inference as the
    /// MPFproj SDK project node).
    /// </remarks>
    [Export(typeof(IProjectTreePropertiesProvider))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    [Order(1000)]
    internal sealed class XSharpProjectTreePropertiesProvider : IProjectTreePropertiesProvider
    {
        private static readonly ProjectImageMoniker WindowsForm = Known(KnownImageIds.WindowsForm);
        private static readonly ProjectImageMoniker UserControl = Known(KnownImageIds.UserControl);

        private static readonly Dictionary<string, ProjectImageMoniker> ExtensionIcons =
            new Dictionary<string, ProjectImageMoniker>(StringComparer.OrdinalIgnoreCase)
            {
                { ".prg", XSharpImages.Document },
                { ".prgx", XSharpImages.Document },
                { ".xs", XSharpImages.Document },
                { ".xsfrm", Known(KnownImageIds.FormInstance) },
                { ".vnfrm", Known(KnownImageIds.FormInstance) },
                { ".xsdbs", Known(KnownImageIds.Database) },
                { ".vndbs", Known(KnownImageIds.Database) },
                { ".xssql", Known(KnownImageIds.Database) },
                { ".vnsqs", Known(KnownImageIds.Database) },
                { ".xsmnu", Known(KnownImageIds.MainMenuControl) },
                { ".vnmnu", Known(KnownImageIds.MainMenuControl) },
                { ".xsfs", Known(KnownImageIds.ValidationRule) },
                { ".vnfs", Known(KnownImageIds.ValidationRule) },
                { ".xsrep", Known(KnownImageIds.Report) },
                { ".vnrep", Known(KnownImageIds.Report) },
            };

        // Result of the base class inference per file, invalidated by the file's timestamp
        private static readonly ConcurrentDictionary<string, (DateTime Stamp, string SubType)> inferredSubTypes =
            new ConcurrentDictionary<string, (DateTime, string)>(StringComparer.OrdinalIgnoreCase);

        private static readonly Regex BaseClassRegex =
            new Regex(@"\bCLASS\s+\S+\s+INHERIT\s+([A-Za-z_][\w.]*)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private readonly UnconfiguredProject project;

        [ImportingConstructor]
        public XSharpProjectTreePropertiesProvider(UnconfiguredProject project)
        {
            this.project = project;
        }

        private static ProjectImageMoniker Known(int id) => new ProjectImageMoniker(KnownImageIds.ImageCatalogGuid, id);

        public void CalculatePropertyValues(IProjectTreeCustomizablePropertyContext propertyContext, IProjectTreeCustomizablePropertyValues propertyValues)
        {
            if (propertyValues.Flags.Contains(ProjectTreeFlags.ProjectRoot))
            {
                propertyValues.Icon = XSharpImages.Project;
                return;
            }
            if (propertyValues.Flags.Contains(ProjectTreeFlags.Folder) || string.IsNullOrEmpty(propertyContext.ItemName))
                return;
            var extension = Path.GetExtension(propertyContext.ItemName);
            if (!ExtensionIcons.TryGetValue(extension, out var icon))
                return;
            if (string.Equals(extension, ".prg", StringComparison.OrdinalIgnoreCase))
            {
                switch (GetDesignerSubType(propertyContext))
                {
                    case "Form":
                        icon = WindowsForm;
                        break;
                    case "UserControl":
                        icon = UserControl;
                        break;
                }
            }
            propertyValues.Icon = icon;
            propertyValues.ExpandedIcon = icon;
        }

        private string GetDesignerSubType(IProjectTreeCustomizablePropertyContext propertyContext)
        {
            if (propertyContext.Metadata != null && propertyContext.Metadata.TryGetValue("SubType", out var subType) &&
                !string.IsNullOrEmpty(subType))
            {
                return subType;
            }
            // ItemName is only the file name; the full path is in the FullPath metadata (as used by the managed
            // project system). Fall back to the project folder for items without metadata.
            string path = null;
            if (propertyContext.Metadata == null || !propertyContext.Metadata.TryGetValue("FullPath", out path) || string.IsNullOrEmpty(path))
                path = Path.Combine(Path.GetDirectoryName(project.FullPath), propertyContext.ItemName);
            if (!ShadowDesignerBridge.HasDesignerFile(path))
                return null;
            return InferSubTypeFromBaseClass(path);
        }

        /// <summary>
        /// "CLASS Foo INHERIT System.Windows.Forms.Form" -> Form, "... INHERIT UserControl" -> UserControl.
        /// Same rule as XSharpFileNode.InferSubTypeFromBaseClass (MPFproj).
        /// </summary>
        private static string InferSubTypeFromBaseClass(string path)
        {
            try
            {
                var stamp = File.GetLastWriteTimeUtc(path);
                if (inferredSubTypes.TryGetValue(path, out var cached) && cached.Stamp == stamp)
                    return cached.SubType;
                string subType = null;
                var match = BaseClassRegex.Match(File.ReadAllText(path));
                if (match.Success)
                {
                    var baseType = match.Groups[1].Value;
                    if (baseType.Equals("Form", StringComparison.OrdinalIgnoreCase) || baseType.EndsWith(".Form", StringComparison.OrdinalIgnoreCase))
                        subType = "Form";
                    else if (baseType.Equals("UserControl", StringComparison.OrdinalIgnoreCase) || baseType.EndsWith(".UserControl", StringComparison.OrdinalIgnoreCase))
                        subType = "UserControl";
                }
                inferredSubTypes[path] = (stamp, subType);
                return subType;
            }
            catch (Exception)
            {
                // Best effort, a failure must never break the tree
                return null;
            }
        }
    }
}
