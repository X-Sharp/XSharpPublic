//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
        public static bool NeedsRuntime(this XSharpDialect dialect)
        {
            return dialect != XSharpDialect.Core;
        }

        public static bool SupportsSymbols(this XSharpDialect dialect)
        {
            //switch (dialect)
            //{
            //    case XSharpDialect.VO:
            //    case XSharpDialect.Vulcan:
            //        return true;
            //    default:
            //        return false;
            //}
            return true;
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

        public static bool AllowDotForInstanceMembers(this XSharpDialect dialect)
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
        public static bool AddressOfIsAlwaysByRef(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.Core:
                case XSharpDialect.VO:
                case XSharpDialect.Vulcan:
                    return false;
                case XSharpDialect.Harbour:
                case XSharpDialect.XPP:
                case XSharpDialect.FoxPro:
                default:
                    return true;
            }
        }

        public static bool AllowPointerMagic(this XSharpDialect dialect)
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

        public static bool AllowLateBindingForTypesWithTheAttribute(this XSharpDialect dialect)
        {
            switch (dialect)
            {
                case XSharpDialect.FoxPro:
                case XSharpDialect.XPP:
                    return true;
                default:
                    return false;
            }
        }
    }
}
