//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using MSBuild = Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.Project;
using XSharpModel;
using System.Collections.Generic;
namespace XSharp.Project
{
    internal static class XSharpFileType
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

        internal static bool IsProjectItemType(MSBuild.ProjectItem buildItem)
        {
            Utilities.ArgumentNotNull("buildItem", buildItem);
            return types.Contains(buildItem.ItemType);
        }


        internal static string GetItemType(string fileName)
        {
            return GetProjectItemType(XFileTypeHelpers.GetFileType(fileName));
        }
        internal static string GetProjectItemType(string url)
        {
            return GetProjectItemType(XFileTypeHelpers.GetFileType(url));
        }
        internal static string GetProjectItemType(XFileType type)
        {
            switch (type)
            {
                case XFileType.SourceCode:
                    return ProjectFileConstants.Compile;
                case XFileType.NativeResource:
                    return XSharpProjectFileConstants.NativeResource;
                case XFileType.VOForm:
                case XFileType.VODBServer:
                case XFileType.VOFieldSpec:
                case XFileType.VOMenu:
                case XFileType.VOIndex:
                case XFileType.VOOrder:
                    return XSharpProjectFileConstants.VOBinary;
                case XFileType.ManagedResource:
                    return ProjectFileConstants.Resource;
                case XFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

        static internal bool HasDesigner(string fileName, string subType)
        {
            bool hasDesigner = false;
            string itemType = GetProjectItemType(fileName);
            switch (itemType)
            {
                case XSharpProjectFileConstants.VOBinary:
                case XSharpProjectFileConstants.Settings:
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
        static internal int ImageIndex(string fileName)
        {
            int ret ;
            switch (XFileTypeHelpers.GetFileType(fileName))
            {
                case XFileType.SourceCode:
                    ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                    break;
                case XFileType.Header:
                case XFileType.PreprocessorOutput:
                    ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                    break;
                case XFileType.VOForm:
                    ret = XSharpImageListIndex.Form + XSharpProjectNode.imageOffset;
                    break;
                case XFileType.VOMenu:
                    ret = XSharpImageListIndex.Menu + XSharpProjectNode.imageOffset;
                    break;
                case XFileType.VODBServer:
                    ret = XSharpImageListIndex.Server + XSharpProjectNode.imageOffset;
                    break;
                case XFileType.VOFieldSpec:
                    ret = XSharpImageListIndex.FieldSpec + XSharpProjectNode.imageOffset;
                    break;
                default:
                    ret = -1;
                    break;

            }
            return ret;
        }

    }
}

