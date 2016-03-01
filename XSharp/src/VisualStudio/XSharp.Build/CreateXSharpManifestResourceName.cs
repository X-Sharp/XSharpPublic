using System;
using System.Text;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.Build.Tasks;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Generic;

namespace XSharp.Build
{

    public class CreateXSharpManifestResourceName : CreateManifestResourceName
    {

         protected override string CreateManifestName(string fileName, string linkFileName, string rootNamespace, string dependentUponFileName, Stream binaryStream)
        {
            ITaskItem item = null;
            string culture = null;
            if ((fileName != null) && base.itemSpecToTaskitem.TryGetValue(fileName, out item))
            {
                culture = item.GetMetadata("Culture");
            }
            return CreateManifestNameImpl(fileName, linkFileName, base.PrependCultureAsDirectory, rootNamespace, dependentUponFileName, culture, binaryStream, base.Log);
        }

        internal string CreateManifestNameImpl(string fileName, string linkFileName, bool prependCultureAsDirectory, string rootNamespace, string dependentUponFileName, string culture, Stream binaryStream, TaskLoggingHelper log)
        {
            string str = linkFileName;
            if ((str == null) || (str.Length == 0))
            {
                str = fileName;
            }
            Culture.ItemCultureInfo itemCultureInfo = Culture.GetItemCultureInfo(str, dependentUponFileName);
            if (!string.IsNullOrEmpty(culture))
            {
                itemCultureInfo.culture = culture;
            }
            StringBuilder builder = new StringBuilder();
            if (binaryStream != null)
            {
                ExtractedClassName firstClassNameFullyQualified = GetFirstClassNameFullyQualified(fileName, binaryStream);
                if (firstClassNameFullyQualified.IsInsideConditionalBlock && (log != null))
                {
                    object[] messageArgs = new object[] { dependentUponFileName, str };
                    log.LogWarningWithCodeFromResources("CreateManifestResourceName.DefinitionFoundWithinConditionalDirective", messageArgs);
                }
                if ((firstClassNameFullyQualified.Name != null) && (firstClassNameFullyQualified.Name.Length > 0))
                {
                    builder.Append(firstClassNameFullyQualified.Name);
                    if ((itemCultureInfo.culture != null) && (itemCultureInfo.culture.Length > 0))
                    {
                        builder.Append(".").Append(itemCultureInfo.culture);
                    }
                }
            }
            if (builder.Length == 0)
            {
                if ((rootNamespace != null) && (rootNamespace.Length > 0))
                {
                    builder.Append(rootNamespace).Append(".");
                }
                string str2 = CreateManifestResourceName.MakeValidEverettIdentifier(Path.GetDirectoryName(itemCultureInfo.cultureNeutralFilename));
                string extension = Path.GetExtension(itemCultureInfo.cultureNeutralFilename);
                if (((string.Compare(extension, ".resx", StringComparison.OrdinalIgnoreCase) == 0) || (string.Compare(extension, ".restext", StringComparison.OrdinalIgnoreCase) == 0)) || (string.Compare(extension, ".resources", StringComparison.OrdinalIgnoreCase) == 0))
                {
                    builder.Append(Path.Combine(str2, Path.GetFileNameWithoutExtension(itemCultureInfo.cultureNeutralFilename)));
                    builder.Replace(Path.DirectorySeparatorChar, '.');
                    builder.Replace(Path.AltDirectorySeparatorChar, '.');
                    if ((itemCultureInfo.culture != null) && (itemCultureInfo.culture.Length > 0))
                    {
                        builder.Append(".").Append(itemCultureInfo.culture);
                    }
                    if (string.Equals(extension, ".resources", StringComparison.OrdinalIgnoreCase))
                    {
                        builder.Append(extension);
                    }
                }
                else
                {
                    builder.Append(Path.Combine(str2, Path.GetFileName(itemCultureInfo.cultureNeutralFilename)));
                    builder.Replace(Path.DirectorySeparatorChar, '.');
                    builder.Replace(Path.AltDirectorySeparatorChar, '.');
                    if ((prependCultureAsDirectory && (itemCultureInfo.culture != null)) && (itemCultureInfo.culture.Length > 0))
                    {
                        builder.Insert(0, Path.DirectorySeparatorChar);
                        builder.Insert(0, itemCultureInfo.culture);
                    }
                }
            }
            return builder.ToString();
        }
        private const RegexOptions options = RegexOptions.CultureInvariant | RegexOptions.Singleline | RegexOptions.Compiled | RegexOptions.IgnoreCase;
        private static Regex classRx = new Regex(@"\s*(?:(?:static|partial|private|protect(?:ed)?|internal|hidden|abstract|sealed)\s+)*class\s+(\S+)", options);
        private static Regex endifRx = new Regex(@"\s*#\s*endif", options);
        private static Regex ifdefRx = new Regex(@"\s*#\s*ifn?def\s+", options);
        private static Regex namespaceBeginRx = new Regex(@"\s*begin\s+namespace\s+(\S+)", options);
        private static Regex namespaceEndRx = new Regex(@"\s*end\s+namespace", options);

        private ExtractedClassName GetFirstClassNameFullyQualified(string fileName, Stream binaryStream)
        {
            Match m = null;
            string currentNamespace = null;
            Stack<string> namespaces = null;
            ExtractedClassName name = new ExtractedClassName();
            currentNamespace = "";
            int conditionalDepth = 0;
            StreamReader reader = new StreamReader(binaryStream, true); // let the reader determine the encoding

            namespaces = new Stack<string>();
            while (! reader.EndOfStream)
            {
                var line = reader.ReadLine();
                // Does the line contain "CLASS"
                m = classRx.Match(line);
                if (m.Success)
                {
                    name.Name = currentNamespace + m.Groups[1].Value;
                    name.IsInsideConditionalBlock = conditionalDepth > 0;
                    return name;
                }
                // Does the line contain "BEGIN NAMESPACE"
                m = namespaceBeginRx.Match(line);
                if (m.Success)
                {
                    namespaces.Push(currentNamespace);
                    currentNamespace = currentNamespace + (m.Groups[1].Value + ".");
                }
                // Does the line contain "END NAMESPACE"
                else if (namespaceEndRx.Match(line).Success)
                {
                    if (namespaces.Count > 0)
                    {
                        currentNamespace = namespaces.Pop();
                    }
                    else
                    {
                        object[] messageArgs = new object[] { fileName };
                        base.Log.LogError("CreateXSharpManifestResourceName: found 'END NAMESPACE' with no matching 'BEGIN NAMESPACE' in '{0}'", messageArgs);
                    }
                }
                // Does the line contain "#IFDEF"
                else if (ifdefRx.Match(line).Success)
                {
                    conditionalDepth++;
                }
                // Does the line contain "#ENDIF"
                else if (endifRx.Match(line).Success)
                {
                    conditionalDepth--;
                }
            }
            return name;
        }

        protected override bool IsSourceFile(string fileName)
        {
            return string.Compare(Path.GetExtension(fileName), ".prg", StringComparison.OrdinalIgnoreCase) == 0;
        }
    }


}
