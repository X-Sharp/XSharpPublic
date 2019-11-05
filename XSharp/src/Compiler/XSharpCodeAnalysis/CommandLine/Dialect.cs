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

namespace Microsoft.CodeAnalysis.CSharp
{
    public enum XSharpDialect
    {
        Core = 0,
        VO = 1,
        Vulcan = 2,
        Harbour = 3,
        FoxPro = 4,
        XPP = 5,
        dBase = 6,
        Last = 6
    }
    public static class DialectExtensions
    {
        public static bool HasRuntime(this XSharpDialect dialect)
        {
            return dialect != XSharpDialect.Core;
        }
        public static bool SupportsMemvars(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.Core:
                case XSharpDialect.Vulcan:
                    return false;
                default:
                    return true;
            }
        }
        public static bool AllowGarbage(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.Core:
                    return false;
                default:
                    return true;
            }
        }

        public static bool AllowXBaseVariables(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                    return true;
                case XSharpDialect.Core:
                case XSharpDialect.Vulcan:
                default:
                    return false;
            }
        }
        public static bool AllowFourLetterAbbreviations(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
                case XSharpDialect.FoxPro:
                    return true;
                case XSharpDialect.XPP:
                default:
                    return false;
            }
        }
        // Allow && comments
        public static bool AllowOldStyleComments(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                    return true;
                case XSharpDialect.Core:
                case XSharpDialect.Vulcan:
                default:
                    return false;
            }
        }

        public static bool AllowDotAsSendOperator(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.FoxPro:
                case XSharpDialect.Core:
                    return true;
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.Vulcan:
                default:
                    return false;
            }
        }
        public static bool AllowStringsWithSingleQuotes(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                    return true;
                case XSharpDialect.Core:
                case XSharpDialect.Vulcan:
                default:
                    return false;
            }
        }
        public static bool AllowASend(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.Core:
                    return false;
                case XSharpDialect.VO:
                case XSharpDialect.Vulcan:
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                default:
                    return true;
            }
        }
        public static bool SupportsAddressOf(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.Core:
                case XSharpDialect.VO:
                case XSharpDialect.Vulcan:
                    return true;
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                default:
                    return false;
            }
        }
    }
}
