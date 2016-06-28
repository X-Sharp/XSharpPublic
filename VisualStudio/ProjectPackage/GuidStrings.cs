//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;

namespace XSharp.Project
{
    /// <summary>
    /// The package GUID 
    /// </summary>
    public static class GuidStrings
    {
        public const string guidXSharpProjectPkgString = "E299FB7B-D273-4678-9ACF-B37B4AF04A12";
        public const string guidXSharpProjectCmdSetString =     "32A7B1CD-3663-4A70-B855-486671F5839C";
        public const string guidXSharpProjectFactoryString = "AA6C8D78-22FF-423A-9C7C-5F2393824E04";

        public const string guidXSharpVOFormEditor = "FC9F8E69-F338-4FA6-ACA3-AA41E445849A";
        public const string guidXSharpVOMenuEditor = "E4AE4582-98AE-40C8-9E48-6F3BE61EBF79";
        public const string guidXSharpVOServerEditor = "C7E4C5F6-98B8-4826-9000-6B9B94DC2D97";
        public const string guidXSharpVOFieldSpecEditor = "8C5D0BAE-7A69-437B-AD0E-1E1B89721EBD";

        public static readonly Guid guidXSharpProjectCmdSet = new Guid(guidXSharpProjectCmdSetString);
        public static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);
        public static readonly Guid guidVOFormEditorFactory = new Guid(guidXSharpVOFormEditor);
        public static readonly Guid guidVOMenuEditorFactory= new Guid(guidXSharpVOMenuEditor);
        public static readonly Guid guidVOServerEditorFactory = new Guid(guidXSharpVOServerEditor);
        public static readonly Guid guidVOFieldSpecEditorFactory= new Guid(guidXSharpVOFieldSpecEditor);

    }
}
