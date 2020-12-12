//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharp
{
    internal static class Constants
    {
        internal const string LanguageName = "XSharp";
        internal const string Company = "XSharpBV";
        internal const string RegCompany = "XSharpBV";
        internal const string ProductName = "XSharp Cahors";
        internal const string Product = "XSharp";
        // NOTE: DO NOT FORGET THE VERSION NUMBER IN THE BUILDNUMBER.H FILE
        internal const string Version = "2.7.0.0";
        internal const string FileVersion = "2.7.0.0";
        internal const string ProductVersion = "2.7 GA";
        internal const string PublicKey = "ed555a0467764586";
        internal const string Copyright = "Copyright © XSharp BV 2015-2020";

        internal const string RegistryKey = @"Software\" + RegCompany + @"\" + Product;
        internal const string RegistryKey64 = @"Software\WOW6432Node\" + RegCompany + @"\" + Product;
        internal const string RegistryValue = "XSharpPath";

        // Environment variable that points to the BIN folder where xsc.exe AND rc.exe are located
        internal const string EnvironmentXSharpBin = "XSHARPBINPATH";
        internal const string EnvironmentXSharp = "XSHARPPATH";
        // Environment variable on _developers_ machine to override location of xsc.exe. 
        // This also enables the "magic" button on the tools-options dialog
        // and adds CRLF to the response file between the various commands to make it easier to read.
        internal const string EnvironmentXSharpDev = "XSHARPDEV";       
    }
    internal static class XSharpConstants
    {
        public const string LanguageName = "XSharp";
        internal const string LanguageServiceName = "XSharp Language Service";
        internal const string EditorName = "XSharp Source Code Editor";
        internal const string ProjectSystemName = "XSharp Project System";

        public const string LegacyXSharpGuid = "AA6C8D78-22FF-423A-9C7C-5F2393824E04";
        public const string ProjectSelectorGuid = "5F81BD32-54BC-4245-9EBA-FA00F3DA9A35";

        public const string CPSPackageGuid = "1460504D-9003-404C-A78A-57E9A2F59358";
        public const string CpsProjectTypeGuid = "286E78A2-2FBA-47EA-A12B-EAEC3D38BC7C";

        internal const string ProjectExtension = "xsproj";
        internal const string ProjectExtensions = "xsproj;xsprj";   // the first version of X# has .xsprj (without o) as extension
        internal const string ProjectFileMask = LanguageName + " Project Files (*." + ProjectExtension + ");*." + ProjectExtension;

        public const string EditorFactoryGuidString = "B4829761-2BFA-44B7-8F8F-D2625EBCF218";

        public const string IntellisenseOptionsPageGuidString = "FDE6E4C9-FA8A-4B93-8B6D-88D9D2A5063E";
        public const string FileNodePropertiesGuidString = "B7971A68-EA46-4814-AC67-1424A59DC7EB";

        public const string WPFProjectFactory = "5ADB76EC-7017-476A-A8E0-25D4202FFCF0";
        public const string WPFFlavor = "14989543-69A4-4C47-A31C-74B6A6DB719B";

        public const string LibraryManagerService = "93F79240-85A9-4697-9A1C-71DE150BA363";
        public const string LibraryManager = "1A36F2B9-EB46-42C7-8421-D5DF35653ED4";
        public const string Library = "3AB768D9-DF41-443F-BECE-497EB5C234DB";

        public const string GeneralPropertiesPage = "53651BEA-799A-45EB-B58C-C884F5417219";
        public const string BuildPropertiesPage = "E994C210-9D6D-4CF4-A061-EBBEA2BC626B";
        public const string DebugPropertiesPage = "2955A638-C389-4675-BB1C-6B2BC173C1E7";
        public const string DialectPropertiesPage = "2652FCA6-1C45-4D25-942D-4C5D5EDE9539";
        public const string LanguagePropertiesPage = "0DFC7EF7-3F1A-4ACB-AFD8-DF56AEF9467A";
        public const string BuildEventsPropertiesPage = "49306259-9119-466E-8780-486CFBE2597D";
    }

}
