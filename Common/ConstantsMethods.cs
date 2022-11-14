//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharp
{
    internal static partial class Constants
    {

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
    }
}
