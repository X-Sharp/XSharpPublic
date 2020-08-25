//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;

namespace XSharp
{
    /// <summary>
    /// The package GUID 
    /// </summary>
    internal static class GuidStrings
    {
        public const string guidXSharpProjectPkgString = "e299fb7b-d273-4678-9acf-b37b4af04a12";
        public const string guidXSharpProjectCmdSetString =     "32a7b1cd-3663-4a70-b855-486671f5839c";
        public const string guidXSharpProjectFactoryString = "aa6c8d78-22ff-423a-9c7c-5f2393824e04";

        public const string guidXSharpVOFormEditor = "fc9f8e69-f338-4fa6-aca3-aa41e445849a";
        public const string guidXSharpVOFormEditorCmdSet = "b9ce6f12-e66c-4e77-9be6-0491dfcdc2d3";

        public const string guidXSharpVOMenuEditor = "e4ae4582-98ae-40c8-9e48-6f3be61ebf79";
        public const string guidXSharpVOMenuEditorCmdSet = "416d760b-7d71-4362-b4a3-97048db8f65f";

        public const string guidXSharpVODbServerEditor = "c7e4c5f6-98b8-4826-9000-6b9b94dc2d97";
        public const string guidXSharpVODbServerEditorCmdSet = "15e7094c-8202-4b0a-a276-50a0d76136d4";

        public const string guidXSharpVOFieldSpecEditor = "8c5d0bae-7a69-437b-ad0e-1e1b89721ebd";
        public const string guidXSharpVOFieldSpecEditorCmdSet = "24ea5441-eb10-45e7-9a44-797df84f8775";

        public const string guidVulcanLanguageServiceString = "8d3f6d25-c81c-4fd8-9599-2f72b5d4b0c9";
        public const string guidVulcanSourceCodeEditor = "{e6787d5e-718e-4810-9c26-7cc920baa335}";
        public const string guidVulcanFormEditor = "{e9eecf7e-7aa2-490e-affc-c55fa2acc5a3}";
        public const string guidVulcanMenuEditor = "{adee1755-5ac3-485b-b857-f82d902362ca}";
        public const string guidVulcanDbEditor = "{5325db94-5d6c-41fd-be44-c5b277612ce6}";
        public const string guidVulcanFsEditor = "{4849278c-aacb-4bbe-9a15-d96da837aeb7}";

        public const string guidVSXmlEditorString = "{fa3cd31e-987b-443a-9b81-186104e8dac1}";


        public const string guidXSharpLanguageServicePkgString = "e9b6ee0f-9bfa-4cff-a60b-51a08bbc5050";
        public const string guidXSharpLanguageServiceCmdSetString = "6511ea00-4558-4ae7-84ee-0e2aebd40d88";

        public const string EditorFactoryGuidString = XSharpConstants.EditorFactoryGuidString;

        public static readonly Guid guidXSharpProjectCmdSet = new Guid(guidXSharpProjectCmdSetString);
        public static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);

        public static readonly Guid guidVOFormEditorFactory = new Guid(guidXSharpVOFormEditor);
        public static readonly Guid guidVOFormEditorCmdSet = new Guid(guidXSharpVOFormEditorCmdSet);

        public static readonly Guid guidVOMenuEditorFactory= new Guid(guidXSharpVOMenuEditor);
        public static readonly Guid guidVOMenuEditorCmdSet = new Guid(guidXSharpVOMenuEditorCmdSet);

        public static readonly Guid guidVODbServerEditorFactory = new Guid(guidXSharpVODbServerEditor);
        public static readonly Guid guidVODbServerEditorCmdSet = new Guid(guidXSharpVODbServerEditorCmdSet);

        public static readonly Guid guidVOFieldSpecEditorFactory= new Guid(guidXSharpVOFieldSpecEditor);
        public static readonly Guid guidVOFieldSpecEditorCmdSet = new Guid(guidXSharpVOFieldSpecEditorCmdSet);
        public static readonly Guid guidSourcecodeEditorFactory = new Guid(EditorFactoryGuidString);
        public static readonly Guid guidLanguageService = new Guid(guidXSharpLanguageServicePkgString);
        public static readonly Guid guidXSharpLanguageServiceCmdSet = new Guid(guidXSharpLanguageServiceCmdSetString);
        public static readonly Guid guidVulcanLanguageService  = new Guid(guidVulcanLanguageServiceString);

        public static readonly Guid guidVSXmlEditor = new Guid(guidVSXmlEditorString);

        public const int cmdidShowGrid = 0x6001;
        public const int cmdidTestDialog = 0x6002;
        public const int VOFormEditorToolbar = 0x6003;
        public const int VOFormEditorToolbarGroup = 0x6004;
        public const int cmdidTabOrder = 0x6005;
    }
}
