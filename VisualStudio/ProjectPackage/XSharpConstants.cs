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

namespace XSharp.Project
{
    public static class XSharpConstants
    {

        public const string FileExtension1 = ".prg";
        public const string FileExtension2 = ".xs";
        public const string PpoExtension = ".ppo";
        public const string HeaderExtension1 = ".vh";
        public const string HeaderExtension2 = ".xh";
        public const string LanguageName = "XSharp";
        internal const string LanguageServiceName = "XSharp Language Service";
        internal const string EditorName = "XSharp Source Code Editor";
        internal const string ProjectSystemName = "XSharp Project System";

        internal const string ProjectExtension = "xsproj";
        internal const string ProjectExtensions = "xsproj;xsprj";
        internal const string ProjectFileMask = LanguageName + " Project Files (*." + ProjectExtension + ");*." + ProjectExtension;

        public const string EditorFactoryGuidString = GuidStrings.EditorFactoryGuidString;

        public const string IntellisenseOptionsPageGuidString = "FDE6E4C9-FA8A-4B93-8B6D-88D9D2A5063E";
        public const string FileNodePropertiesGuidString = "B7971A68-EA46-4814-AC67-1424A59DC7EB";

        public const string WPFProjectFactory = "5ADB76EC-7017-476A-A8E0-25D4202FFCF0";
        public const string WPFFlavor = "14989543-69A4-4C47-A31C-74B6A6DB719B";
    }
    public static class XSharpProjectFileConstants
    {

        public const string NativeResource = "NativeResource";
        public const string VOBinary = "VOBinary";
        public const string Settings = "Settings";

    }
	  /// <summary>
        /// Indexes to the embedded ImageList .bmp
        /// </summary>
    public static class XSharpImageListIndex
    {
        public const int Project = 0;
        public const int Source = 1;
        public const int Form = 2;
        public const int Server = 3;
        public const int FieldSpec = 4;
        public const int Menu = 5;

    }
}
