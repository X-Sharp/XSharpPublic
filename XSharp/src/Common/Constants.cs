/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
namespace XSharp
{
    internal static class Constants
    {
        internal const string Company = "XSharpBV";
        internal const string RegCompany = "XSharpBV";
        internal const string ProductName = "XSharp Bandol";
        internal const string Product = "XSharp";
        // NOTE: DO NOT FORGET THE VERSION NUMBER IN THE BUILDNUMBER.H FILE
        internal const string Version = "2.1.0.0";
        internal const string FileVersion = "2.1.0.0";
        internal const string ProductVersion = "2.10 GA";
        internal const string Copyright = "Copyright © XSharp BV 2015-2019";

        internal const string RegistryKey = @"Software\" + RegCompany + @"\" + Product;
        internal const string RegistryKey64 = @"Software\WOW6432Node\" + RegCompany + @"\" + Product;
        internal const string RegistryValue = "XSharpPath";


    }
}
