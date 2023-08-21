//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Globalization;

namespace XSharp.Build
{
    /// <summary>
    /// Utility functions for dealing with Culture information.
    /// Source from GitHub: https://github.com/Microsoft/msbuild/blob/master/src/XMakeTasks/Culture.cs
    /// </summary>
    internal static class Culture
    {
        /// <summary>
        /// Culture information about an item.
        /// </summary>
        internal struct ItemCultureInfo
        {
            internal string culture;
            internal string cultureNeutralFilename;
        }
        /// <summary>
        /// Given an item's filename, return information about the item including the culture and the culture-neutral filename.
        /// </summary>
        /// <remarks>
        /// We've decided to ignore explicit Culture attributes on items.
        /// </remarks>
        /// <param name="name"></param>
        /// <param name="dependentUponFilename"></param>
        /// <returns></returns>

        internal static ItemCultureInfo GetItemCultureInfo(string name, string dependentUponFilename)
        {
            ItemCultureInfo info;
            info.culture = null;
            string path = (dependentUponFilename == null) ? string.Empty : dependentUponFilename;
            if (string.Compare(Path.GetFileNameWithoutExtension(path), Path.GetFileNameWithoutExtension(name), StringComparison.OrdinalIgnoreCase) == 0)
            {
                info.cultureNeutralFilename = name;
                return info;
            }
            // Either not dependent on another file, or it has a distinct base filename
            // If the item is defined as "Strings.en-US.resx", then ...
            // ... base file name will be "Strings.en-US" ..
            string baseFileNameWithCulture  = Path.GetFileNameWithoutExtension(name);
            // ... and cultureName will be ".en-US".
            string cultureName  = Path.GetExtension(baseFileNameWithCulture );
            bool validCulture  = false;
            if ((cultureName  != null) && (cultureName .Length > 1))
            {
                // ... strip the "." to make "en-US"
                cultureName = cultureName.Substring(1);
                validCulture  = CultureStringUtilities.IsValidCultureString(cultureName );
            }
            if (validCulture)
            {
                // A valid culture was found.
                string extension = Path.GetExtension(name);
                string baseFileName = Path.GetFileNameWithoutExtension(baseFileNameWithCulture);
                string baseFolder = Path.GetDirectoryName(name);
                string fileName = baseFileName + extension;
                info.cultureNeutralFilename = Path.Combine(baseFolder, fileName);

            }
            else
            {
                // No valid culture was found. In this case, the culture-neutral
                // name is the just the original file name.
                info.cultureNeutralFilename = name;
            }
            return info;
        }
    }


    internal static class CultureStringUtilities
    {
        private static string[] s_cultureInfoStrings;

        internal static bool IsValidCultureString(string cultureString)
        {
            PopulateCultureInfoArray();
            bool flag = true;
            if (Array.BinarySearch<string>(s_cultureInfoStrings, cultureString, StringComparer.OrdinalIgnoreCase) < 0)
            {
                flag = false;
            }
            return flag;
        }

        internal static void PopulateCultureInfoArray()
        {
            if (s_cultureInfoStrings == null)
            {
                CultureInfo[] cultures = CultureInfo.GetCultures(CultureTypes.AllCultures);
                s_cultureInfoStrings = new string[cultures.Length];
                for (int i = 0; i < cultures.Length; i++)
                {
                    s_cultureInfoStrings[i] = cultures[i].Name;
                }
                Array.Sort<string>(s_cultureInfoStrings, StringComparer.OrdinalIgnoreCase);
            }
        }
    }
}
