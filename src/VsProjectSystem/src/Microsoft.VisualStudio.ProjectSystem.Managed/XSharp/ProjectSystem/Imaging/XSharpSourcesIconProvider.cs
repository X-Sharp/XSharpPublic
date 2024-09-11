// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using Microsoft.VisualStudio.Imaging;

namespace Microsoft.VisualStudio.ProjectSystem.Imaging.XSharp
{
    [Export(typeof(IProjectTreePropertiesProvider))]
    [AppliesTo(ProjectCapability.XSharp)]
    [Order(Order.Default)]
    internal class XSharpSourcesIconProvider : IProjectTreePropertiesProvider
    {
        private static readonly Dictionary<string, ProjectImageMoniker> s_fileExtensionImageMap = new(StringComparers.Paths)
        {
            { ".prg", SourceIcon },
            { ".ppo", SourceIcon },
            { ".xs", SourceIcon },
            { ".xsfrm", FormIcon},
            { ".xsdbs", DatabaseIcon },
            { ".xsfs", FieldIcon},
            { ".xsmnu", MenuIcon},
            { ".xssql", DatabaseIcon},
            { ".xsrep", ReportIcon},
            { ".vnfrm", FormIcon},
            { ".vndbs", DatabaseIcon },
            { ".vnfs", FieldIcon},
            { ".vnmnu", MenuIcon}
        };

        //private static ProjectImageMoniker SourceIcon => XSharpImagesMonikers.FileImage.ToProjectSystemType();
        private static ProjectImageMoniker SourceIcon => KnownMonikers.PYSourceFile.ToProjectSystemType();
        private static ProjectImageMoniker VOBinaryIcon => KnownMonikers.BinaryFile.ToProjectSystemType();
        private static ProjectImageMoniker FormIcon => KnownMonikers.WindowsForm.ToProjectSystemType();
        private static ProjectImageMoniker DatabaseIcon => KnownMonikers.Database.ToProjectSystemType();
        private static ProjectImageMoniker FieldIcon => KnownMonikers.Database.ToProjectSystemType();
        private static ProjectImageMoniker MenuIcon => KnownMonikers.MenuBar.ToProjectSystemType();
        private static ProjectImageMoniker ReportIcon => KnownMonikers.Report.ToProjectSystemType();

        public void CalculatePropertyValues(IProjectTreeCustomizablePropertyContext propertyContext, IProjectTreeCustomizablePropertyValues propertyValues)
        {
            if (!propertyValues.Flags.Contains(ProjectTreeFlags.Common.Folder))
            {
                if (!propertyValues.Flags.Contains(ProjectTreeFlags.Common.SourceFile | ProjectTreeFlags.Common.FileSystemEntity))
                {
                    return;
                }

                propertyValues.Icon = GetIconForItem(propertyContext.ItemName);
            }
        }

        private static ProjectImageMoniker? GetIconForItem(string itemName)
        {
            if (s_fileExtensionImageMap.TryGetValue(Path.GetExtension(itemName), out ProjectImageMoniker moniker))
            {
                return moniker;
            }

            // Return null so VS can supply the default icons.
            return null;
        }
    }
}
