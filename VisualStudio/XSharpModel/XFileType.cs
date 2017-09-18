//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections.Immutable;
using System.Collections.Concurrent;
namespace XSharpModel
{
    public enum XFileType
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

    public static class XFileTypeHelpers
    {
        public static bool IsVOBinary(this XFileType type)
        {
            switch (type)
            {
                case XFileType.VOMenu:
                case XFileType.VODBServer:
                case XFileType.VOFieldSpec:
                case XFileType.VOForm:
                case XFileType.VOIndex:
                case XFileType.VOOrder:
                    return true;
            }
            return false;
        }
        public static bool OpenInSourceCodeEditor(this XFileType type)
        {
            switch (type)
            {
                case XFileType.SourceCode:
                case XFileType.Header:
                case XFileType.NativeResource:
                case XFileType.Unknown:
                    return true;
            }
            return false;
        }

        public static XFileType GetFileType(string filename)
        {
            string ext = System.IO.Path.GetExtension(filename).ToLower();
            switch (ext)
            {
                case ".prg":
                case ".xs":
                    return XFileType.SourceCode;
                case ".ppo":
                    return XFileType.PreprocessorOutput;
                case ".vh":
                case ".xh":
                    return XFileType.Header;
                case ".xsfrm":
                case ".vnfrm":
                    return XFileType.VOForm;
                case ".xsmnu":
                case ".vnmnu":
                    return XFileType.VOMenu;
                case ".xsdbs":
                case ".vndbs":
                    return XFileType.VODBServer;
                case ".xsfs":
                case ".vnfs":
                    return XFileType.VOFieldSpec;
                case ".xaml":
                    return XFileType.XAML;
                case ".settings":
                    return XFileType.Settings;
                case ".resx":
                    return XFileType.ManagedResource;
                case ".rc":
                    return XFileType.NativeResource;
                default:
                    return XFileType.Unknown;
            }
        }

    }
}