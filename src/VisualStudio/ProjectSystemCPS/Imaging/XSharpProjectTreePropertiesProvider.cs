//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.ComponentModel.Composition;
using System.IO;
using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.ProjectSystem.Imaging
{
    /// <summary>
    /// Sets the X# icons in Solution Explorer: the project node and the X# source files.
    /// </summary>
    /// <remarks>
    /// The managed project system provides the project icon through IProjectImageProvider, which is internal
    /// to it and only implemented for C#, VB and F#. IProjectTreePropertiesProvider is the public CPS way.
    /// </remarks>
    [Export(typeof(IProjectTreePropertiesProvider))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    [Order(1000)]
    internal sealed class XSharpProjectTreePropertiesProvider : IProjectTreePropertiesProvider
    {
        public void CalculatePropertyValues(IProjectTreeCustomizablePropertyContext propertyContext, IProjectTreeCustomizablePropertyValues propertyValues)
        {
            if (propertyValues.Flags.Contains(ProjectTreeFlags.ProjectRoot))
            {
                propertyValues.Icon = XSharpImages.Project;
            }
            else if (!propertyValues.Flags.Contains(ProjectTreeFlags.Folder) && IsXSharpSourceFile(propertyContext.ItemName))
            {
                propertyValues.Icon = XSharpImages.Document;
            }
        }

        private static bool IsXSharpSourceFile(string itemName)
        {
            if (string.IsNullOrEmpty(itemName))
                return false;
            var extension = Path.GetExtension(itemName);
            return string.Equals(extension, ".prg", StringComparison.OrdinalIgnoreCase)
                || string.Equals(extension, ".prgx", StringComparison.OrdinalIgnoreCase)
                || string.Equals(extension, ".xs", StringComparison.OrdinalIgnoreCase);
        }
    }
}
