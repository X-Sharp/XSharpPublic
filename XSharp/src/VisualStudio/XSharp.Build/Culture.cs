using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Globalization;

namespace XSharp.Build
{

    internal static class Culture
    {
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
            string fileNameWithoutExtension = Path.GetFileNameWithoutExtension(name);
            string extension = Path.GetExtension(fileNameWithoutExtension);
            bool flag = false;
            if ((extension != null) && (extension.Length > 1))
            {
                extension = extension.Substring(1);
                flag = CultureStringUtilities.IsValidCultureString(extension);
            }
            if (flag)
            {
                info.culture = extension;
                string str4 = Path.GetExtension(name);
                string directoryName = Path.GetDirectoryName(name);
                string str6 = Path.GetFileNameWithoutExtension(fileNameWithoutExtension) + str4;
                info.cultureNeutralFilename = Path.Combine(directoryName, str6);
                return info;
            }
            info.cultureNeutralFilename = name;
            return info;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct ItemCultureInfo
        {
            internal string culture;
            internal string cultureNeutralFilename;
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
