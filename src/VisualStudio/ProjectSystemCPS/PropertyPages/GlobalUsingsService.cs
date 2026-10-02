//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Linq;

using Microsoft.Build.Construction;
using Microsoft.Build.Evaluation;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Reads and writes the <c>&lt;Using&gt;</c> MSBuild items of an X# project file.
    /// </summary>
    /// <remarks>
    /// Operates on the project XML rather than on an evaluated project: the project
    /// system does not publish its projects through
    /// <see cref="ProjectCollection.GlobalProjectCollection"/>. Only items declared in
    /// the project file itself are visible; items contributed by the SDK are not.
    /// </remarks>
    internal sealed class GlobalUsingsService
    {
        private const string UsingItemType = "Using";
        private const string AliasMetadata = "Alias";
        private const string StaticMetadata = "Static";

        private readonly string _projectFullPath;

        public GlobalUsingsService(string projectFullPath)
        {
            _projectFullPath = projectFullPath;
        }

        /// <summary>Returns the <c>&lt;Using&gt;</c> items declared in the project file.</summary>
        public IReadOnlyList<GlobalUsingItem> GetUsings()
        {
            var xml = OpenProjectXml();
            if (xml == null)
                return new List<GlobalUsingItem>();

            return xml.Items
                .Where(item => string.Equals(item.ItemType, UsingItemType, StringComparison.OrdinalIgnoreCase))
                .Select(item => new GlobalUsingItem
                {
                    Include = item.Include,
                    Alias = GetMetadata(item, AliasMetadata),
                    IsStatic = string.Equals(GetMetadata(item, StaticMetadata), "true",
                                             StringComparison.OrdinalIgnoreCase),
                    IsReadOnly = false
                })
                .ToList();
        }

        /// <summary>Replaces the <c>&lt;Using&gt;</c> items in the project file.</summary>
        public void SetUsings(IEnumerable<GlobalUsingItem> usings)
        {
            var xml = OpenProjectXml();
            if (xml == null)
                return;

            var desired = usings.Where(u => !u.IsReadOnly && !string.IsNullOrWhiteSpace(u.Include))
                                .ToList();

            foreach (var item in xml.Items
                         .Where(i => string.Equals(i.ItemType, UsingItemType, StringComparison.OrdinalIgnoreCase))
                         .ToList())
            {
                item.Parent.RemoveChild(item);
            }

            foreach (var group in xml.ItemGroups.Where(g => g.Count == 0).ToList())
            {
                xml.RemoveChild(group);
            }

            if (desired.Count > 0)
            {
                var itemGroup = xml.AddItemGroup();
                foreach (var entry in desired)
                {
                    var item = itemGroup.AddItem(UsingItemType, entry.Include);
                    if (!string.IsNullOrWhiteSpace(entry.Alias))
                        item.AddMetadata(AliasMetadata, entry.Alias, expressAsAttribute: true);
                    if (entry.IsStatic)
                        item.AddMetadata(StaticMetadata, "true", expressAsAttribute: true);
                }
            }

            xml.Save();
        }

        private ProjectRootElement OpenProjectXml()
        {
            if (string.IsNullOrEmpty(_projectFullPath) || !System.IO.File.Exists(_projectFullPath))
                return null;

            // Reuse the cached instance when the file is already open, so that edits made
            // here are seen by the project system without a reload.
            return ProjectRootElement.TryOpen(_projectFullPath)
                   ?? ProjectRootElement.Open(_projectFullPath);
        }

        private static string GetMetadata(ProjectItemElement item, string name)
        {
            var metadata = item.Metadata.FirstOrDefault(
                m => string.Equals(m.Name, name, StringComparison.OrdinalIgnoreCase));
            return string.IsNullOrWhiteSpace(metadata?.Value) ? null : metadata.Value;
        }
    }
}
