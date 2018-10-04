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
        XBasePP = 5,
        dBase = 6,
    }
    public static class DialectExtensions
    {
        public static bool IsDialectVO (this XSharpDialect dialect)
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

        public static bool AllowNamedArgs(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
				case XSharpDialect.XBasePP:
                //case XSharpDialect.Vulcan:
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
				case XSharpDialect.XBasePP:
                    return true;
                default:
                    return false;
            }
        }
        public static bool AllowFunctionsInsideClass(this XSharpDialect dialect)
        {
            return false;
        }
        public static bool AllowFourLetterAbbreviations(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Harbour:
				case XSharpDialect.XBasePP:
                    return true;
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
				case XSharpDialect.XBasePP:
                    return true;
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
				case XSharpDialect.XBasePP:
                    return true;
                default:
                    return false;
            }
        }
    }
}
