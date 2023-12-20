//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharp
{
    internal static partial class Constants
    {
        internal const string LanguageName = "XSharp";
        internal const string Company = "XSharp BV";
        internal const string RegCompany = "XSharpBV";
        internal const string ProductName = "XSharp Cahors";
        internal const string Product = "XSharp";
        // NOTE: DO NOT FORGET THE VERSION NUMBER IN THE BUILDNUMBER.H FILE and the Versions.Props file
#if RUNTIME
        internal const string Version = "2.6.0.0";
#else
        internal const string Version = FileVersion;
#endif
        internal const string FileVersion = "2.18.1.0";
        internal const string ProductVersion = "2.18 GA";
        internal const string PublicKey = "ed555a0467764586";
        internal const string Copyright = "Copyright © XSharp BV 2015-2023";

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
        internal const string StandardHeaderFile = "XSharpDefs.xh";
        internal const string SourceFileExtension = ".prg";
        internal const string PublicKeyLong = "0024000004800000940000000602000000240000525341310004000001000100b16a35b62bb33ce476c595e75bcc83fe4566c0a7cb9c093ce23e7add61fe1fc8a6edca2e542f0dc9ce41ec6b4260a73dda598c81f61a6f9522653ebfeae098a3bdb641020e843cbab825afe1c3910d42d17a1dcf211abb1cba4fc5e19569307c67a11c92b848d2df23f454d5ed1ab8b479afa4ece799445292b11012225aee96";
        internal const string XSharpVendorString = "{363398fa-76ad-44bb-a750-0abdc02bf4de}";
        internal const string XSharpLanguageString = "{f579403a-d3d3-47a0-bf03-9709575bee69}";
        internal const string DbgXSharpLanguageName = "X#";
    }
}
