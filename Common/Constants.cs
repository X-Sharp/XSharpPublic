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
        internal const string Company = "XSharp BV";
        internal const string RegCompany = "XSharpBV";
        internal const string ProductName = "XSharp Cahors";
        internal const string Product = "XSharp";
        // NOTE: DO NOT FORGET THE VERSION NUMBER IN THE BUILDNUMBER.H FILE and the Versions.Props file
#if RUNTIME
        internal const string Version = "2.6.0.0";
#else
        internal const string Version = "2.13.2.2";
#endif
        internal const string FileVersion = "2.13.2.2";
        internal const string ProductVersion = "2.13b GA";
        internal const string PublicKey = "ed555a0467764586";
        internal const string Copyright = "Copyright © XSharp BV 2015-2022";

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
#if ! RUNTIME
        // Read Setting from Registry
        // Names of registry keys that contain settings used by code generation in the Code Generators
        // These are stored in the registry because the CodeDomProvider is also called "stand alone" from the
        // build process when generating code for XAML files
        internal const string RegistryKeywordCase = "KeywordCase";
        internal const string RegistryPrivateKeyword = "PrivateKeyword";
        internal const string RegistryPublicKeyword = "PublicKeyword";
        internal const string RegistryUseTabs = "UseTabs";
        internal const string RegistryTabSize = "TabSize";
        internal const string RegistryIndentSize = "IndentSize";
        internal static bool optionWasChanged = false;



        internal static bool WriteSetting(string name, int defvalue)
        {
            object result = defvalue;
            try
            {
                var key = Microsoft.Win32.Registry.CurrentUser;
                var subkey = key.OpenSubKey(Constants.RegistryKey, true);
                if (subkey == null)
                {
                    subkey = key.CreateSubKey(Constants.RegistryKey, true);
                }
                subkey.SetValue(name, defvalue);
                return true;
            }
            catch
            {
            }
            return false;
        }
        // Write Setting from Registry
        internal static object GetSetting(string name, int defvalue)
        {
            object result = defvalue;
            try
            {
                var key = Microsoft.Win32.Registry.CurrentUser;
                var subkey = key.OpenSubKey(Constants.RegistryKey, true);
                if (subkey == null)
                {
                    subkey = key.CreateSubKey(Constants.RegistryKey, true);
                }
                result = subkey.GetValue(name);
                if (result == null)
                {
                    subkey.SetValue(name, defvalue);
                    result = defvalue;
                }
            }
            catch
            {
                result = defvalue;
            }
            return result;
        }

#endif
    }
}
