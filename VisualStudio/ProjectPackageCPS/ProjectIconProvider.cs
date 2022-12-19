//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.ProjectSystem
{
    [Export(typeof(IProjectTreePropertiesProvider))]
    [AppliesTo("XSharp")] // [Order(Order.Default)]
    internal class XSharpSourcesIconProvider : IProjectTreePropertiesProvider
    {
        public XSharpSourcesIconProvider()
        {
            ;
        }
        private static readonly Dictionary<string, ProjectImageMoniker> s_fileExtensionImageMap = new Dictionary<string, ProjectImageMoniker>(StringComparer.OrdinalIgnoreCase)
        {
            { ".prg",   XSharpImagesMonikers.FileImage.ToProjectSystemType() },
            { ".xh",  XSharpImagesMonikers.FileImage.ToProjectSystemType() },
            { ".ppo",  XSharpImagesMonikers.FileImage.ToProjectSystemType() }
        };

        public void CalculatePropertyValues(IProjectTreeCustomizablePropertyContext propertyContext, IProjectTreeCustomizablePropertyValues propertyValues)
        {
            if (propertyValues.Flags.Contains(ProjectTreeFlags.ProjectRoot))
            {
                propertyValues.Icon = XSharpImagesMonikers.ProjectImage.ToProjectSystemType();
            }
            if (!propertyValues.Flags.Contains(ProjectTreeFlags.Common.Folder))
            {
                if (!propertyValues.Flags.Contains(ProjectTreeFlags.Common.SourceFile | ProjectTreeFlags.Common.FileSystemEntity))
                {
                    return;
                }

                propertyValues.Icon = GetIconForItem(propertyContext.ItemName);
            }
        }

        private static ProjectImageMoniker GetIconForItem(string itemName)
        {
            if (s_fileExtensionImageMap.TryGetValue(Path.GetExtension(itemName), out ProjectImageMoniker moniker))
            {
                return moniker;
            }

            // Return null so VS can supply the default icons.
            return null;
        }
    }
    [Export(typeof(IProjectImageProvider))]
    [AppliesTo("XSharp")]
    internal class XSharpProjectImageProvider : IProjectImageProvider
    {
        [ImportingConstructor]
        public XSharpProjectImageProvider()
        {
        }

        public ProjectImageMoniker GetProjectImage(string key)
        {

            return XSharpImagesMonikers.ProjectImage.ToProjectSystemType();
        }
    }
    [ProjectSystemContract(ProjectSystemContractScope.UnconfiguredProject, ProjectSystemContractProvider.Private)]
    internal interface IProjectImageProvider
    {
        /// <summary>
        ///     Returns the <see cref="ProjectImageMoniker"/> for the specified key, returning <see langword="null"/>
        ///     if the provider does handle the specified key.
        /// </summary>
        ProjectImageMoniker GetProjectImage(string key);
    }
}
