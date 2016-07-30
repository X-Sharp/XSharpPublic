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

        // These two defines are hardcoded in VS and must not be changed
        public const string LOGVIEWID_Designer = "{7651a702-06e5-11d1-8ebd-00a0c90f26ea}";
        public const string LOGVIEWID_Code = "{7651a701-06e5-11d1-8ebd-00a0c90f26ea}";

        public const string guidVulcanLanguageServiceString = "8d3f6d25-c81c-4fd8-9599-2f72b5d4b0c9";

        public const string guidXSharpLanguageServicePkgString = "e9b6ee0f-9bfa-4cff-a60b-51a08bbc5050";
        public const string guidXSharpLanguageServiceCmdSetString = "6511ea00-4558-4ae7-84ee-0e2aebd40d88";


        public static readonly Guid guidXSharpProjectCmdSet = new Guid(guidXSharpProjectCmdSetString);
        public static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);
        public static readonly Guid guidVOFormEditorFactory = new Guid(guidXSharpVOFormEditor);
        public static readonly Guid guidVOMenuEditorFactory= new Guid(guidXSharpVOMenuEditor);
        public static readonly Guid guidVOServerEditorFactory = new Guid(guidXSharpVOServerEditor);
        public static readonly Guid guidVOFieldSpecEditorFactory= new Guid(guidXSharpVOFieldSpecEditor);
        public static readonly Guid guidSourcecodeEditorFactory = new Guid(XSharpConstants.EditorFactoryGuidString);
        public static readonly Guid guidLanguageService = new Guid(guidXSharpLanguageServicePkgString);
        public static readonly Guid guidXSharpLanguageServiceCmdSet = new Guid(guidXSharpLanguageServiceCmdSetString);
        public static readonly Guid guidVulcanLanguageService  = new Guid(guidVulcanLanguageServiceString);

    }
}
