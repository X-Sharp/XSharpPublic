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
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.ProjectSystem;
using XSharp.CodeDom;

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

        // This provider runs synchronously on the CPS tree thread for every node, on every tree update, so it keeps
        // file system access low: the designer files are looked up in a cached list per folder, and a form's file is
        // only read until its CLASS declaration. Both caches are cleared when they grow beyond MaxCacheEntries.
        private const int MaxCacheEntries = 1000;
        // A folder listing is reused for this long, then again as long as the folder's timestamp is unchanged
        // (on NTFS it changes when a file in it is created, deleted or renamed). Short: it only has to cover the burst
        // of one tree update (all nodes within milliseconds); a longer interval could miss a .Designer.prg that was
        // just created (Add New Item) and leave the form icon generic until the next tree update.
        private static readonly TimeSpan FolderRecheckInterval = TimeSpan.FromMilliseconds(250);
        // Longest part of a form's file that is searched for the CLASS declaration
        private const int MaxClassSearchLength = 64 * 1024;

        // Result of the base class inference per file, invalidated by the file's timestamp
        private static readonly ConcurrentDictionary<string, (DateTime Stamp, string SubType)> inferredSubTypes =
            new ConcurrentDictionary<string, (DateTime, string)>(StringComparer.OrdinalIgnoreCase);

        // *.Designer.prg file names per folder
        private static readonly ConcurrentDictionary<string, (DateTime Checked, DateTime Stamp, HashSet<string> Names)> designerFilesByFolder =
            new ConcurrentDictionary<string, (DateTime, DateTime, HashSet<string>)>(StringComparer.OrdinalIgnoreCase);

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
            if (!HasDesignerFile(path))
                return null;
            return InferSubTypeFromBaseClass(path);
        }

        /// <summary>
        /// Like ShadowDesignerBridge.HasDesignerFile, but with the cached folder listing (see above). The bridge
        /// itself stays uncached: the commands and NewFormDesignerRedirect need the current state.
        /// </summary>
        private static bool HasDesignerFile(string prgPath)
        {
            try
            {
                if (!string.Equals(Path.GetExtension(prgPath), ".prg", StringComparison.OrdinalIgnoreCase) ||
                    prgPath.EndsWith(".designer.prg", StringComparison.OrdinalIgnoreCase))
                {
                    return false;
                }
                var folder = Path.GetDirectoryName(prgPath);
                var designerName = Path.GetFileName(XSharpCodeDomHelper.BuildDesignerFileName(prgPath));
                if (string.IsNullOrEmpty(folder) || string.IsNullOrEmpty(designerName))
                    return false;
                return GetDesignerFileNames(folder).Contains(designerName);
            }
            catch (Exception)
            {
                return false;
            }
        }

        private static HashSet<string> GetDesignerFileNames(string folder)
        {
            var now = DateTime.UtcNow;
            if (designerFilesByFolder.TryGetValue(folder, out var cached) && now - cached.Checked < FolderRecheckInterval)
                return cached.Names;
            var stamp = Directory.GetLastWriteTimeUtc(folder);
            HashSet<string> names;
            if (cached.Names != null && cached.Stamp == stamp)
            {
                names = cached.Names;
            }
            else
            {
                names = new HashSet<string>(
                    Directory.EnumerateFiles(folder, "*.designer.prg").Select(Path.GetFileName),
                    StringComparer.OrdinalIgnoreCase);
            }
            if (designerFilesByFolder.Count > MaxCacheEntries)
                designerFilesByFolder.Clear();
            designerFilesByFolder[folder] = (now, stamp, names);
            return names;
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
                var match = FindClassDeclaration(path);
                if (match != null)
                {
                    var baseType = match.Groups[1].Value;
                    if (baseType.Equals("Form", StringComparison.OrdinalIgnoreCase) || baseType.EndsWith(".Form", StringComparison.OrdinalIgnoreCase))
                        subType = "Form";
                    else if (baseType.Equals("UserControl", StringComparison.OrdinalIgnoreCase) || baseType.EndsWith(".UserControl", StringComparison.OrdinalIgnoreCase))
                        subType = "UserControl";
                }
                if (inferredSubTypes.Count > MaxCacheEntries)
                    inferredSubTypes.Clear();
                inferredSubTypes[path] = (stamp, subType);
                return subType;
            }
            catch (Exception)
            {
                // Best effort, a failure must never break the tree
                return null;
            }
        }

        /// <summary>
        /// The first "CLASS ... INHERIT ..." of the file. Reads the file in blocks and stops at the first match (a form's
        /// class is near the top) or after <see cref="MaxClassSearchLength"/> characters, instead of reading it all.
        /// </summary>
        private static Match FindClassDeclaration(string path)
        {
            var text = new StringBuilder();
            var buffer = new char[4096];
            using (var reader = new StreamReader(path, detectEncodingFromByteOrderMarks: true))
            {
                int read;
                while (text.Length < MaxClassSearchLength && (read = reader.Read(buffer, 0, buffer.Length)) > 0)
                {
                    text.Append(buffer, 0, read);
                    // The whole text so far: a declaration can span two blocks
                    var match = BaseClassRegex.Match(text.ToString());
                    if (match.Success)
                        return match;
                }
            }
            return null;
        }
    }
}
