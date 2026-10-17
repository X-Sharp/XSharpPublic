//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using MSBuild = Microsoft.Build.Evaluation;

using System.Collections.Generic;
using XSharpModel;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    public static class XSharpFileType
    {
        /// <summary>
        /// Returns if the buildItem is a file item or not.
        /// </summary>
        /// <param name="buildItem">BuildItem to be checked.</param>
        /// <returns>Returns true if the buildItem is a file item, false otherwise.</returns>
        static HashSet<string> types;
        static XSharpFileType()
        {
            types = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            types.Add(ProjectFileConstants.Compile);
            types.Add(ProjectFileConstants.EmbeddedResource);
            types.Add(ProjectFileConstants.Content);
            types.Add(ProjectFileConstants.NativeResource);
            types.Add(ProjectFileConstants.VOBinary);
            types.Add(ProjectFileConstants.ApplicationDefinition);
            types.Add(ProjectFileConstants.Page);
            types.Add(ProjectFileConstants.Resource);
            types.Add(ProjectFileConstants.None);
        }

        public static bool IsProjectItemType(MSBuild.ProjectItem buildItem)
        {
            Utilities.ArgumentNotNull("buildItem", buildItem);
            return types.Contains(buildItem.ItemType);
        }


        public static string GetItemType(string fileName)
        {
            return GetProjectItemType(XFileTypeHelpers.GetFileType(fileName));
        }
        public static string GetProjectItemType(string url)
        {
            return GetProjectItemType(XFileTypeHelpers.GetFileType(url));
        }
        public static string GetProjectItemType(XFileType type)
        {
            switch (type)
            {
                case XFileType.SourceCode:
                    return ProjectFileConstants.Compile;
                case XFileType.NativeResource:
                    return ProjectFileConstants.NativeResource;
                case XFileType.VOForm:
                case XFileType.VODBServer:
                case XFileType.VOFieldSpec:
                case XFileType.VOMenu:
                case XFileType.VOIndex:
                case XFileType.VOOrder:
                case XFileType.VOSqlTable:
                case XFileType.VOReport:
                    return ProjectFileConstants.VOBinary;
                case XFileType.ManagedResource:
                    return ProjectFileConstants.Resource;
                case XFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

        static public bool HasDesigner(string fileName, string subType)
        {
            bool hasDesigner = false;
            string itemType = GetProjectItemType(fileName);
            switch (itemType)
            {
                case ProjectFileConstants.VOBinary:
                case ProjectFileConstants.Settings:
                case ProjectFileConstants.Resource:
                case ProjectFileConstants.Page:
                case ProjectFileConstants.ApplicationDefinition:
                    hasDesigner = true;
                    break;
                default:
                    switch (subType)
                    {
                        case ProjectFileAttributeValue.Component:
                        case ProjectFileAttributeValue.Form:
                        case ProjectFileAttributeValue.UserControl:
                            hasDesigner = true;
                            break;
                        default:
                            hasDesigner = false;
                            break;
                    }
                    break;
            }
            return hasDesigner;
        }
        static public ImageMoniker GetIconMoniker(string fileName)
        {
            switch (XFileTypeHelpers.GetFileType(fileName))
            {
                case XFileType.SourceCode:
                case XFileType.Header:
                case XFileType.PreprocessorOutput:
                    return XSharpImageMonikers.Source;
                case XFileType.VOForm:
                    return XSharpImageMonikers.Form;
                case XFileType.VOMenu:
                    return XSharpImageMonikers.Menu;
                case XFileType.VODBServer:
                    return XSharpImageMonikers.Server;
                case XFileType.VOFieldSpec:
                    return XSharpImageMonikers.FieldSpec;
                case XFileType.VOSqlTable:
                    return XSharpImageMonikers.SQLTable;
                case XFileType.VOReport:
                    return XSharpImageMonikers.Report;
                default:
                    return default(ImageMoniker);
            }
        }

    }
}

