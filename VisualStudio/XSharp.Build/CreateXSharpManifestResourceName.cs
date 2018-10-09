//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Text;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.Build.Tasks;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace XSharp.Build
{
    /// <summary>
    /// Base class for task that determines the appropriate manifest resource name to
    /// assign to a given resx or other resource.
    /// Inspired by https://github.com/Microsoft/msbuild/blob/master/src/XMakeTasks/CreateCSharpManifestResourceName.cs
    /// </summary>
    public class CreateXSharpManifestResourceName : CreateManifestResourceName
    {
        /// <summary>
        /// Utility function for creating a C#-style manifest name from
        /// a resource name.
        /// </summary>
        /// <param name="fileName">The file name of the dependent (usually a .resx)</param>
        /// <param name="linkFileName">The file name of the dependent (usually a .resx)</param>
        /// <param name="rootNamespace">The root namespace (usually from the project file). May be null</param>
        /// <param name="dependentUponFileName">The file name of the parent of this dependency (usually a .cs file). May be null</param>
        /// <param name="binaryStream">File contents binary stream, may be null</param>
        /// <returns>Returns the manifest name</returns>
        protected override string CreateManifestName
            (
                string fileName,
                string linkFileName,
                string rootNamespace,
                string dependentUponFileName,
                Stream binaryStream
            )
        {
            ITaskItem item = null;
            string culture = null;
            if ((fileName != null) && base.itemSpecToTaskitem.TryGetValue(fileName, out item))
            {
                culture = item.GetMetadata("Culture");
            }
            /*
                  Actual implementation is in a static method called CreateManifestNameImpl.
                  The reason is that CreateManifestName can't be static because it is an
                  override of a method declared in the base class, but its convenient
                  to expose a static version anyway for unittesting purposes.
              */

            return CreateXSharpManifestResourceName.CreateManifestNameImpl
                (
                    fileName,
                    linkFileName,
                    base.PrependCultureAsDirectory,
                    rootNamespace,
                    dependentUponFileName,
                    culture,
                    binaryStream,
                    base.Log
                );
        }
        /// <summary>
        /// Utility function for creating a X#-style manifest name from
        /// a resource name. Note that this function is inspired by the similar C# function
        /// </summary>
        /// <param name="fileName">The file name of the dependent (usually a .resx)</param>
        /// <param name="linkFileName">The file name of the dependent (usually a .resx)</param>
        /// <param name="rootNamespace">The root namespace (usually from the project file). May be null</param>
        /// <param name="prependCultureAsDirectory">should the culture name be prepended to the manifest name as a path</param>
        /// <param name="dependentUponFileName">The file name of the parent of this dependency (usually a .cs file). May be null</param>
        /// <param name="culture">The override culture of this resource, if any</param>
        /// <param name="binaryStream">File contents binary stream, may be null</param>
        /// <param name="log">Task's TaskLoggingHelper, for logging warnings or errors</param>
        /// <returns>Returns the manifest name</returns>
        internal static string CreateManifestNameImpl
            (
                string fileName,
                string linkFileName,
                bool prependCultureAsDirectory,
                string rootNamespace,
                string dependentUponFileName,
                string culture,
                Stream binaryStream,
                TaskLoggingHelper log
            )
        {
            string embeddedFileName = linkFileName;
            // Use the link file name if there is one, otherwise, fall back to file name.

            if ((embeddedFileName == null) || (embeddedFileName.Length == 0))
            {
                embeddedFileName = fileName;
            }
            Culture.ItemCultureInfo info = Culture.GetItemCultureInfo(embeddedFileName, dependentUponFileName);
            // If the item has a culture override, respect that.
            if (!string.IsNullOrEmpty(culture))
            {
                info.culture = culture;
            }
            StringBuilder manifestName = new StringBuilder();
            if (binaryStream != null)
            {
                // Resource depends on a form. Now, get the form's class name fully
                // qualified with a namespace.

                ExtractedClassName result = CreateXSharpManifestResourceName.GetFirstClassNameFullyQualified(fileName, binaryStream, log);
                if (result.IsInsideConditionalBlock && log != null)
                {
                    log.LogWarningWithCodeFromResources("CreateManifestResourceName.DefinitionFoundWithinConditionalDirective", dependentUponFileName, embeddedFileName);
                }
                if ((result.Name != null) && (result.Name.Length > 0))
                {
                    manifestName.Append(result.Name);
                    if ((info.culture != null) && (info.culture.Length > 0))
                    {
                        manifestName.Append(".").Append(info.culture);
                    }
                }
            }
            // If there's no manifest name at this point, then fall back to using the
            // RootNamespace+Filename_with_slashes_converted_to_dots

            if (manifestName.Length == 0)
            {
                // If Rootnamespace was null, then it wasn't set from the project resourceFile.
                // Empty namespaces are allowed.
                if ((rootNamespace != null) && (rootNamespace.Length > 0))
                {
                    manifestName.Append(rootNamespace).Append(".");
                }
                // Replace spaces in the directory name with underscores. Needed for compatibility with Everett.
                // Note that spaces in the file name itself are preserved.

                string everettCompatibleDirectoryName  = CreateManifestResourceName.MakeValidEverettIdentifier(Path.GetDirectoryName(info.cultureNeutralFilename));
                // only strip extension for .resx and .restext files
                string sourceExtension = Path.GetExtension(info.cultureNeutralFilename);
                if (
                        (0 == String.Compare(sourceExtension, ".resx", StringComparison.OrdinalIgnoreCase))
                        ||
                        (0 == String.Compare(sourceExtension, ".restext", StringComparison.OrdinalIgnoreCase))
                        ||
                        (0 == String.Compare(sourceExtension, ".resources", StringComparison.OrdinalIgnoreCase))
                    )
                {

                    manifestName.Append(Path.Combine(everettCompatibleDirectoryName , Path.GetFileNameWithoutExtension(info.cultureNeutralFilename)));
                    // Replace all '\' with '.'

                    manifestName.Replace(Path.DirectorySeparatorChar, '.');
                    manifestName.Replace(Path.AltDirectorySeparatorChar, '.');
                    // Append the culture if there is one.
                    if ((info.culture != null) && (info.culture.Length > 0))
                    {
                        manifestName.Append(".").Append(info.culture);
                    }
                    // If the original extension was .resources, add it back
                    if (string.Equals(sourceExtension, ".resources", StringComparison.OrdinalIgnoreCase))
                    {
                        manifestName.Append(sourceExtension);
                    }
                }
                else
                {
                    manifestName.Append(Path.Combine(everettCompatibleDirectoryName , Path.GetFileName(info.cultureNeutralFilename)));
                    // Replace all '\' with '.'
                    manifestName.Replace(Path.DirectorySeparatorChar, '.');
                    manifestName.Replace(Path.AltDirectorySeparatorChar, '.');
                    // Prepend the culture as a subdirectory if there is one.
                    if (prependCultureAsDirectory)
                    {
                        if (info.culture != null && info.culture.Length > 0)
                        {
                            manifestName.Insert(0, Path.DirectorySeparatorChar);
                            manifestName.Insert(0, info.culture);
                        }
                    }
                }
            }
            return manifestName.ToString();
        }
        private const RegexOptions options = RegexOptions.CultureInvariant | RegexOptions.Singleline | RegexOptions.Compiled | RegexOptions.IgnoreCase;
        private static Regex classRx = new Regex(@"\s*(?:(?:static|partial|private|protect(?:ed)?|internal|hidden|abstract|sealed)\s+)*class\s+(\S+)", options);
        private static Regex endifRx = new Regex(@"\s*#\s*endif", options);
        private static Regex ifdefRx = new Regex(@"\s*#\s*ifn?def\s+", options);
        private static Regex namespaceBeginRx = new Regex(@"\s*begin\s+namespace\s+(\S+)", options);
        private static Regex namespaceEndRx = new Regex(@"\s*end\s+namespace", options);


        private static string stripComments(string line)
        {
            // find the line comment mark. Make sure that we do not check inside string literals
            var sb = new StringBuilder();
            bool instring = false;
            bool done = false;
            char last = '\0';
            foreach (char c in line)
            {
                switch (c)
                {
                    case '"':
                        instring = !instring;
                        break;
                    case '/':
                        if (!instring)
                        {
                            if (last == '/')
                            {
                                sb.Remove(sb.Length - 1, 1);
                                done = true;
                            }
                        }
                        break;
                    default:
                        break;
                }
                if (done)
                    break;
                sb.Append(c);
                last = c;
            }
            return sb.ToString();
        }

        private static string stripMultiLineComments(string contents)
        {
            var sb = new StringBuilder();
            int index = contents.IndexOf("/*");
            while (index >= 0 && contents.Length > 0)
            {
                sb.Append(contents.Substring(0, index - 1));
                contents = contents.Substring(index + 2);
                index = contents.IndexOf("*/");
                if (index >= 0)
                {
                    contents = contents.Substring(index + 2);
                }
                index = contents.IndexOf("/*");
            }
            return sb.ToString();
        }
        /// <summary>
        ///
        /// </summary>
        /// <param name="fileName">The file name of the dependent (usually a .resx)</param>
        /// <param name="binaryStream">File contents binary stream, may be null</param>
        /// <param name="log">Task's TaskLoggingHelper, for logging warnings or errors</param>
        /// <returns></returns>
        private static ExtractedClassName GetFirstClassNameFullyQualified(string fileName, Stream binaryStream,TaskLoggingHelper log)
        {
            Match m = null;
            string currentNamespace = "";
            ExtractedClassName name = new ExtractedClassName();
            int conditionalDepth = 0;
            StreamReader reader = new StreamReader(binaryStream, true); // let the reader determine the encoding
            var namespaces = new Stack<string>();
            var contents = reader.ReadToEnd();
            if (contents.Contains("/*"))
            {
                contents = stripMultiLineComments(contents);
            }
            var lines = contents.Split("\n\r".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            foreach (string l in lines)
            {
                var line = l;
                if (line.Contains("//"))
                {
                    // find the line comment mark. Make sure that we do not check inside string literals
                    line = stripComments(line);
                }
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
                    var ns = m.Groups[1].Value + ".";
                    if (ns.StartsWith("global::", StringComparison.OrdinalIgnoreCase))
                    {
                        ns = ns.Substring(8);
                    }
                    currentNamespace = currentNamespace + ns;
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
                        log.LogError("CreateXSharpManifestResourceName: found 'END NAMESPACE' with no matching 'BEGIN NAMESPACE' in '{0}'", messageArgs);
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

        /// <summary>
        /// Return 'true' if this is a X# source file.
        /// </summary>
        /// <param name="fileName">Name of the candidate source file.</param>
        /// <returns>True, if this is a validate source file.</returns>
        protected override bool IsSourceFile(string fileName)
        {
            string extension = Path.GetExtension(fileName);
            return string.Compare(extension, ".prg", StringComparison.OrdinalIgnoreCase) == 0 ||
                string.Compare(extension, ".xs", StringComparison.OrdinalIgnoreCase) == 0;
        }
    }


}
