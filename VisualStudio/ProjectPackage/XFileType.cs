//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using MSBuild = Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    enum XSharpFileType
    {
        Unknown = -1,
        SourceCode,
        PreprocessorOutput,
        Header,
        VOForm,
        VOMenu,
        VODBServer,
        VOIndex,
        VOOrder,
        VOFieldSpec,
        NativeResource,
        ManagedResource,
        XAML,
        Settings,

    }
    internal static class XFileType
    {
        /// <summary>
        /// Returns if the buildItem is a file item or not.
        /// </summary>
        /// <param name="buildItem">BuildItem to be checked.</param>
        /// <returns>Returns true if the buildItem is a file item, false otherwise.</returns>
        static string[] types = {
                                ProjectFileConstants.Compile,
                                ProjectFileConstants.EmbeddedResource,
                                ProjectFileConstants.Content,
                                XSharpProjectFileConstants.NativeResource,
                                XSharpProjectFileConstants.VOBinary,
                                ProjectFileConstants.ApplicationDefinition,
                                ProjectFileConstants.Page,
                                ProjectFileConstants.Resource,
                                ProjectFileConstants.None
                                };

        internal static bool IsXFileItem(MSBuild.ProjectItem buildItem)
        {
            Utilities.ArgumentNotNull("buildItem", buildItem);
            string name = buildItem.ItemType;
            foreach (var type in types)
            {
                if (String.Equals(name, type, StringComparison.OrdinalIgnoreCase))
                    return true;
            }
            return false;
        }

        public static bool IsVoBinary(string fileName)
        {
            switch (GetFileType(fileName))
            {
                case XSharpFileType.VOMenu:
                case XSharpFileType.VODBServer:
                case XSharpFileType.VOFieldSpec:
                case XSharpFileType.VOForm:
                case XSharpFileType.VOIndex:
                case XSharpFileType.VOOrder:
                    return true;

            }
            return false;
        }


        internal static string GetItemType(string fileName)
        {
            return GetItemType(GetFileType(fileName));
        }

        internal static string GetItemType(XSharpFileType type)
        {
            switch (type)
            {
                case XSharpFileType.SourceCode:
                    return ProjectFileConstants.Compile;
                case XSharpFileType.NativeResource:
                    return XSharpProjectFileConstants.NativeResource;
                case XSharpFileType.VOForm:
                case XSharpFileType.VODBServer:
                case XSharpFileType.VOFieldSpec:
                case XSharpFileType.VOMenu:
                case XSharpFileType.VOIndex:
                case XSharpFileType.VOOrder:
                    return XSharpProjectFileConstants.VOBinary;
                case XSharpFileType.ManagedResource:
                    return ProjectFileConstants.Resource;
                case XSharpFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

        static internal XSharpFileType GetFileType(string filename)
        {
            string ext = Path.GetExtension(filename);
            switch (ext)
            {
                case ".prg":
                case ".xs":
                    return XSharpFileType.SourceCode;
                case ".vh":
                case ".xh":
                    return XSharpFileType.Header;
                case ".xsfrm":
                case ".vnfrm":
                    return XSharpFileType.VOForm;
                case ".xsmnu":
                case ".vnmnu":
                    return XSharpFileType.VOMenu;
                case ".xsdbs":
                case ".vndbs":
                    return XSharpFileType.VODBServer;
                case ".xsfs":
                case ".vnfs":
                    return XSharpFileType.VOFieldSpec;
                case ".xaml":
                    return XSharpFileType.XAML;
                case ".settings":
                    return XSharpFileType.Settings;
                case ".resx":
                    return XSharpFileType.ManagedResource;
                case ".rc":
                    return XSharpFileType.NativeResource;
                default:
                    return XSharpFileType.Unknown;

            }
        }
        static internal bool HasDesigner(string fileName, string subType)
        {
            bool hasDesigner = false;
            string itemType = XFileType.GetItemType(fileName);
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
            switch (GetFileType(fileName))
            {
                case XSharpFileType.SourceCode:
                    ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                    break;
                case XSharpFileType.Header:
                case XSharpFileType.PreprocessorOutput:

                    ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                    break;
                case XSharpFileType.VOForm:
                    ret = XSharpImageListIndex.Form + XSharpProjectNode.imageOffset;
                    break;
                case XSharpFileType.VOMenu:
                    ret = XSharpImageListIndex.Menu + XSharpProjectNode.imageOffset;
                    break;
                case XSharpFileType.VODBServer:
                    ret = XSharpImageListIndex.Server + XSharpProjectNode.imageOffset;
                    break;
                case XSharpFileType.VOFieldSpec:
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

