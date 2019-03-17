//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using LanguageService.SyntaxTree;
using System.Reflection;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Linq;
using System.Xml;
using XSharpModel;

namespace XSharp.Project
{
    // Based on https://stackoverflow.com/questions/230925/retrieve-xml-doc-comments-programmatically#231005
    //
    static public class XSharpXMLDocTools
    {
        public static FileInfo GetXmlDocFile(Assembly assembly, XProject project)
        {
            string assemblyDirPath = Path.GetDirectoryName(assembly.Location);
            string fileName = Path.GetFileNameWithoutExtension(assembly.Location) + ".xml";
            // First try at the same location as the Assembly
            FileInfo xmlFile = null;

            try
            {
                xmlFile = GetFallbackDirectories(CultureInfo.CurrentCulture)
                .Select(dirName => CombinePath(assemblyDirPath, dirName, fileName))
                .Select(filePath => new FileInfo(filePath))
                .Where(file => file.Exists)
                .First();
            }
            catch
            { }
            // Ok, search in the "Reference Assemblies" folder
            if (xmlFile == null)
            {
                //
                string dllFile = Path.GetFileName(assembly.Location);
                List<AssemblyInfo> refs = project.AssemblyReferences;
                foreach (AssemblyInfo dllRef in refs)
                {
                    if (String.Equals(dllRef.DisplayName, dllFile, StringComparison.InvariantCultureIgnoreCase))
                    {
                        // Got it !!
                        assemblyDirPath = Path.GetDirectoryName(dllRef.FileName);
                        fileName = Path.GetFileNameWithoutExtension(dllRef.DisplayName) + ".xml";
                        // So, xml file should be here
                        xmlFile = new FileInfo(Path.Combine(assemblyDirPath, fileName));
                        if (!xmlFile.Exists)
                            xmlFile = null;
                        break;
                    }
                }
            }
            //// References assemblies are in %ProgramFiles(x86)% on
            //// 64 bit machines
            //var programFiles = Environment.GetEnvironmentVariable("ProgramFiles(x86)");

            //if (string.IsNullOrEmpty(programFiles))
            //{
            //    // On 32 bit machines they are in %ProgramFiles%
            //    programFiles = Environment.GetEnvironmentVariable("ProgramFiles");
            //}

            //if (string.IsNullOrEmpty(programFiles))
            //{
            //    // Reference assemblies aren't installed
            //    return null;
            //}
            //String prefixPath = Path.Combine(programFiles, "Reference Assemblies", "Microsoft", "Framework", ".NETFramework");


            //xmlFile = new FileInfo( prefixPath );
            //if (!xmlFile.Exists)
            //    return null;
            return xmlFile;
        }

        static IEnumerable<string> GetFallbackDirectories(CultureInfo culture)
        {
            return culture
              .Enumerate(c => c.Parent.Name != c.Name ? c.Parent : null)
              .Select(c => c.Name);
        }

        static IEnumerable<T> Enumerate<T>(this T start, Func<T, T> next)
        {
            for (T item = start; !object.Equals(item, default(T)); item = next(item))
                yield return item;
        }

        static string CombinePath(params string[] args)
        {
            return args.Aggregate(Path.Combine);
        }
    }

    static public class XSharpXMLDocMember
    {
        static public string GetDocSummary(MemberInfo member, XProject project)
        {
            string summary = null;
            System.Xml.Linq.XElement docSummary = null;
            Assembly declarationAssembly = null;
            //
            if (member is Type)
                declarationAssembly = ((Type)member).Assembly;
            else if (member.DeclaringType != null)
                declarationAssembly = member.DeclaringType.Assembly;
            else
                return summary;

            //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            try
            {

                var docXml = System.Xml.Linq.XDocument.Load(file.FullName);
                var docMembers = docXml.Root.Element("members");

                var docMember = GetDocMember(docMembers, member);
                docSummary = docMember.Elements("summary").First();
            }
            catch { }
            //
            if (docSummary != null)
                summary = docSummary.Value;
            //
            return summary;
        }

        /// <summary>
        /// Search the XElement corresponding to member in the docMembers xml tree
        /// </summary>
        /// <param name="docMembers"></param>
        /// <param name="member"></param>
        /// <returns></returns>
        static System.Xml.Linq.XElement GetDocMember(System.Xml.Linq.XElement docMembers, MemberInfo member)
        {
            string memberId = GetMemberId(member);
            return docMembers.Elements("member")
              .Where(e => e.Attribute("name").Value == memberId)
              .First();
        }


        /// <summary>
        /// Build the Member Name to search for in the XMLDoc file.
        /// ie : member name="T:System.Version"
        /// </summary>
        /// <param name="member"></param>
        /// <returns></returns>
        static string GetMemberId(MemberInfo member)
        {
            char memberKindPrefix = GetMemberPrefix(member);
            if (memberKindPrefix == 'C')  // ctor
                memberKindPrefix = 'M';
            string memberName = GetMemberFullName(member);
            return memberKindPrefix + ":" + memberName;
        }


        /// <summary>
        /// Retrieve Member prefix : P(roperty); M(ethod); T(ype)
        /// </summary>
        /// <param name="member"></param>
        /// <returns></returns>
        static char GetMemberPrefix(MemberInfo member)
        {
            return member.GetType().Name
              .Replace("Runtime", "")[0];
        }

        /// <summary>
        /// Retrieve the Fully Qualified Name of a Member
        /// </summary>
        /// <param name="member"></param>
        /// <returns></returns>
        static string GetMemberFullName(MemberInfo member)
        {
            string memberScope = "";
            if (member.DeclaringType != null)
                memberScope = GetMemberFullName(member.DeclaringType);
            else if (member is Type)
                memberScope = ((Type)member).Namespace;
            string memberName = member.Name;
            if (memberName == ".ctor")
                memberName = "#ctor";
            return memberScope + "." + memberName;
        }
    }
}