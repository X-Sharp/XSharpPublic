using Sandcastle.Core;
using SandcastleBuilder.Utils.BuildEngine;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

namespace XSharpDocs
{
    internal class XSharpDocChanger
    {
        // Concrete visible replacements in the output
        const string seeAlsoNew = "Functions, Globals and Defines";
        const string TitleFunction = "Function"; // is added after the name
        const string TitleFunctionFieldsPageNew = "List of Globals and Defines";
        const string TitleFunctionMethodsPageNew = "List of Functions";
        const string TitleFunctionClassPageNew = "Functions, Globals and Defines";
        const string listFunctions = "Functions";
        const string listGlobalsDefines = "Globals and Defines";
        const string Global = "Global";
        const string Define = "Define";

        //useful for Regex
        const string anyString = ".*";
        const string safeAnyString = "[^<>]*";
        const string groupedAnyString = "(" + anyString + ")";
        const string safeGroupedAnyString = "(" + safeAnyString + ")";
        const string h1Open = "<h1>";
        const string h1Close = "</h1>";
        const string titleOpen = "<title>";
        const string titleClose = "</title>";


        // strings needed for replacing the title
        //mark the start and end of the . in the title!
        const string TitleRemoveTagStart = "span";
        const string TitleRemoveTagEnd = "script";

        const string TitleRemoveFunctions = "Functions"; //removes the functions before the . in function class methods and function class fields
        const string TitleFieldOld = " Field"; // The space is important! first occurrence is removed
        const string TitleMethodOld = "Method";

        const string TitleFunctionFieldsPageOld = "Functions Fields";
        const string TitleFunctionMethodsPageOld = "Functions Methods";
        const string TitleFunctionClassPageOld = "Functions Class";

        //strings needed for replacing the seeAlsoField
        const string seeAlsoTagStart = "div class=\"seeAlsoStyle\"";
        const string seeAlsoTagEnd = "div";
        const string seeAlsoOld = "Functions Class";

        // strings needed for replacing the list titles
        const string listTagStart = "span class=\"collapsibleRegionTitle\"";
        const string listTagEnd = "span";
        const string listMethodOld = "Methods";
        const string listFieldsOld = "Fields";

        // strings to define/find the type of the files
        const string pageFieldPrefix = "F_";
        const string pageFieldsPrefix = "Fields_T";
        const string pageMethodsPrefix = "Methods_T";
        const string pagePropertiesPrefix = "Properties_T";
        const string pageTypePrefix = "T_";
        const string pageFunctionPageSuffix = "_Functions";
        const string pageFunctionsMethodsRegex = "M_" + anyString + "_Functions_" + anyString;
        const string pageOverloadedPrefix = "Overload_";
        const string pageSdkMethodPrefix = "M_VO_";
        const string pageTypedSdkMethodPrefix = "M_XSharp_VO_SDK_";
        const string pageTypedSdkTypePrefix = "T_XSharp_VO_SDK_";
        const string pageTypedSdkProperty = "P_XSharp_VO_SDK";
        const string pageConstructorSuffix = "__ctor";
        const string pageTypedVoSDK = "XSharp_VO_SDK_";


        //needed for hhc editing
        const string hhcTocStart = "<param name=\"Name\" value=\"";
        const string hhcTocEnd = "\">";
        const string hhcTocTypeStart = "<param name=\"Local\" value=\"html\\";
        const string hhcTocTypeEnd = ".htm\">";
        const string hhcTocTypeMiddle = "\\"; // We need an extra backslash for the regex to parse correctly

        //needed for hhk editing
        const string hhkTypeStart = "<param name=\"Local\" value=\"html/";
        const string hhkTypeEnd = hhcTocTypeEnd;
        const string hhkSeeAlso = "<param name=\"See Also\"";
        const string hhkMethodPageOld = "methods";
        const string hhkFieldPageOld = "fields";
        const string hhkSpecialCase = "<param name=\"See Also\" value=\"Functions class\">";
        const string hhkSpecialCaseReplace = "<param name=\"See Also\" value=\"" + TitleFunctionClassPageNew + "\">";
        const string hhkFieldMethodPrefix = "Functions.";
        const string hhkFieldPostfix = " Field";
        const string hhkMethodPostfix = "Method";
        const string hhkConstructorPostfix = "Constructor";
        const string hhkPropertyPostfix = "Property";
        const string hhkClassPostfix = "Class";
        const string hhkFunctionClassPageOld = "Functions class";

        //needed for editing for MSHV
        const string MSHVTitleStart = "<title>";
        const string MSHVTitleEnd = "</title>";
        const string MSHVKeywordStart = "<meta name=\"System.Keywords\" content=\"";
        const string MSHVKeywordStartTwo = "<meta name=\"Microsoft.Help.Keywords\" content=\"";
        const string MSHVKeywordEnd = "\" />";

        // Types of files for classifying
        internal enum FileType
        {
            FunctionClass,
            ListOfFunctions,
            ListOfMethods,
            ListOfFields,
            ListOfProperties,
            Function,
            Define,
            TypedsdkMethod,
            TypedsdkProperty,
            TypedsdkConstructor,
            TypedsdkClass,
            Other
        }


        static internal void Convert(BuildProcess builder, HelpFileFormats format)
        {
            // Check which modes all need to be compiled
            string basePath = builder.WorkingFolder;
            string htmlHelp1Path = @"Output\HtmlHelp1";
            string MsHelpViewerPath = @"Output\MsHelpViewer";
            string websitePath = @"Output\Website";
            string htmlPath = "html";
            if (format == HelpFileFormats.HtmlHelp1)
            {
                builder.ReportProgress("Editing htmlHelp1 files...");
                string htmlFolderPath = Path.Combine(basePath, htmlHelp1Path, htmlPath);
                string[] hhcPaths = Directory.GetFiles(basePath, "*.hhc", SearchOption.TopDirectoryOnly);
                string[] hhkPaths = Directory.GetFiles(basePath, "*.hhk", SearchOption.TopDirectoryOnly);

                if (Directory.Exists(htmlFolderPath))
                {
                    builder.ReportProgress("  Editing html topic pages...");
                    EditHtmlFolder(builder, htmlFolderPath, format);
                }
                else
                {
                    builder.ReportProgress("  Could not find html folder!");
                }
                if (hhcPaths.Length > 0)
                {
                    builder.ReportProgress("  Editing {0} TOC's for HtmlHelp1...", hhcPaths.Length);
                    foreach (string hhc in hhcPaths)
                    {
                        EditHhc(hhc);
                        EditXSharpTypeNames(hhc);
                    }
                }
                else
                {
                    builder.ReportProgress("   Found no TOC for HtmlHelp1");
                }

                if (hhkPaths.Length > 0)
                {
                    builder.ReportProgress("  Editing {0} index file(s) for HtmlHelp1...", hhkPaths.Length);
                    foreach (string hhk in hhkPaths)
                    {
                        EditHhk(hhk);
                        EditXSharpTypeNames(hhk);
                    }
                }
                else
                {
                    builder.ReportProgress("  Found no index file for HtmlHelp1");
                }
            }
            if (format == HelpFileFormats.MSHelpViewer)
            {
                builder.ReportProgress("Editing MsHelpViewer files...");
                string htmlFolderPath = Path.Combine(basePath, MsHelpViewerPath, htmlPath);
                if (Directory.Exists(htmlFolderPath))
                {
                    builder.ReportProgress("  Editing html topic pages...");
                    EditHtmlFolder(builder, htmlFolderPath, format);
                }
                else
                {
                    builder.ReportProgress("  Could not find html folder!");
                }
            }
            if (format == HelpFileFormats.Website)
            {
                builder.ReportProgress("Editing Website files...");
                string htmlFolderPath = Path.Combine(basePath, websitePath, htmlPath);
                if (Directory.Exists(htmlFolderPath))
                {
                    builder.ReportProgress("  Editing html topic pages...");
                    EditHtmlFolder(builder, htmlFolderPath, format);

                }
            }
        }



        // classifies a type based on its name.
        static FileType findFileType(string htmlName)
        {
            if (htmlName.StartsWith(pageFieldsPrefix))
            {
                if (htmlName.EndsWith(pageFunctionPageSuffix))
                {
                    return FileType.ListOfFields;
                }
            }
            else if (htmlName.StartsWith(pageFieldPrefix) && htmlName.Contains("Functions"))
            {
                return FileType.Define;
            }
            else if (htmlName.StartsWith(pageMethodsPrefix))
            {
                if (htmlName.EndsWith(pageFunctionPageSuffix))
                    return FileType.ListOfFunctions;
                else
                    return FileType.ListOfMethods;
            }
            else if (htmlName.StartsWith(pagePropertiesPrefix))
            {
                return FileType.ListOfProperties;
            }
            else if (htmlName.StartsWith(pageTypePrefix) && htmlName.EndsWith(pageFunctionPageSuffix))
            {
                return FileType.FunctionClass;
            }
            else if (Regex.IsMatch(htmlName, pageFunctionsMethodsRegex))
            {
                return FileType.Function;
            }
            else if (htmlName.StartsWith(pageOverloadedPrefix) && htmlName.Contains("Functions"))
            {
                return FileType.Function;
            }
            else if (htmlName.EndsWith(pageConstructorSuffix))
            {
                if (htmlName.Contains(pageTypedVoSDK))
                {
                    return FileType.TypedsdkConstructor;
                }
            }
            else if (htmlName.StartsWith(pageSdkMethodPrefix))
            {
                return FileType.Function;
            }
            else if (htmlName.StartsWith(pageTypedSdkMethodPrefix))
            {
                return FileType.TypedsdkMethod;
            }

            else if (htmlName.StartsWith(pageTypedSdkTypePrefix))
            {
                return FileType.TypedsdkClass;
            }
            else if (htmlName.StartsWith(pageTypedSdkProperty))
            {
                return FileType.TypedsdkProperty;
            }
            return FileType.Other;
        }

        //edits a page at htmlName based on it's FileType
        static void editWebPage(hhtopic topic)
        {
            // we only want to edit files we actually need to edit.
            // All types need to replace their title
            ReplaceWebPageTitle(topic);
            //Edit the See Also for all but functions classPage
            if (topic.Type != FileType.FunctionClass)
            {
                ReplaceSeeAlso(topic.FileName);
            }
            if (topic.Type != FileType.Function && topic.Type != FileType.Define)
            {
                ReplaceListTitles(topic);
            }
        }

        static void EditHtmlFolder(BuildProcess builder, string path, HelpFileFormats format)
        {
            string[] htmlFilesPaths = Directory.GetFiles(path, "*.htm", SearchOption.TopDirectoryOnly);
            // Then we classify the Files into different types
            var topics = new List<hhtopic>(htmlFilesPaths.Length);
            foreach (string htmlFilePath in htmlFilesPaths)
            {
                string htmlFileName = htmlFilePath.Remove(0, path.Length + 1);// +1 comes from backslash character in the path ending.
                htmlFileName = htmlFileName.Remove(htmlFileName.Length - 4); // remove the .htm at the end of the string.
                FileType fileType = findFileType(htmlFileName);
                string strType = "";
                if (fileType == FileType.Define)
                {
                    var contents = File.ReadAllText(htmlFilePath);
                    strType = DetermineFieldType(contents);
                }
                if (fileType != FileType.Other)
                {
                    topics.Add(new hhtopic() { FileName = htmlFilePath, Type = fileType, FieldType = strType });
                }
            }
            //edit each html page
            int counter = 0;
            foreach (var topic in topics)
            {
                // edit the page itself
                editWebPage(topic);
                // edit all the typeNames on the page
                EditXSharpTypeNames(topic.FileName);
                if (format == HelpFileFormats.MSHelpViewer)
                {
                    editFileForMSHV(topic);
                }
                counter++;
                if (counter % 500 == 0)
                {
                    builder.ReportProgress("   Adjusted {0} pages", counter);
                }
            }
            builder.ReportProgress("   Finished adjusting {0} pages", counter);
            return;
        }

        static private string editTag(string original, string toEdit, string editInto, string startTag, string endTag)
        {
            string regexTagStart = @"<" + startTag;
            string regexTagEnd = @"/" + endTag + ">";
            string regex = anyString + regexTagStart + anyString + toEdit + anyString + regexTagEnd + anyString;
            string output = original;
            if (Regex.IsMatch(original, regex))
            {
                string regexReplace = regexTagStart + groupedAnyString + toEdit + groupedAnyString + regexTagEnd;
                string replacement = regexTagStart + "$1" + editInto + "$2" + regexTagEnd;
                output = Regex.Replace(original, regexReplace, replacement);
            }
            return output;
        }

        static private void ReplaceSeeAlso(string path)
        {
            // For all lines
            string[] fileLines = File.ReadAllLines(path);
            int i = 0, newLineIndex = -1;
            string newLine = "";
            foreach (string line in fileLines)
            {
                // If the line contains both these we assume it is the correct line
                if (line.Contains(seeAlsoOld) && line.Contains(seeAlsoTagStart))
                {
                    // edit the line to be correct
                    newLine = editTag(line, seeAlsoOld, seeAlsoNew, seeAlsoTagStart, seeAlsoTagEnd);
                    newLineIndex = i;
                    if (newLine == line)
                    {
                        Console.WriteLine("Could not edit the See Also for file: {0} ", path);
                        return;
                    }
                    break;
                }
                i++;
            }
            if (newLineIndex != -1)
            {
                fileLines[newLineIndex] = newLine;
            }
            else
            {
                Console.WriteLine("Could not find the See Also for file: {0} ", path);
                return;
            }
            var writer = new StreamWriter(path);
            foreach (string line in fileLines)
            {
                writer.WriteLine(line);
            }
            writer.Close();
            return;
        }

        static string DetermineFieldType(string folder, string fileName)
        {
            folder = Path.Combine(folder, "html");
            fileName = Path.Combine(folder, fileName + ".htm");
            if (File.Exists(fileName))
            {
                var contents = File.ReadAllText(fileName);
                return DetermineFieldType(contents);
            }
            return "";
        }
        static string DetermineFieldType(string topic)
        {
            string strType;
            if (topic.IndexOf("GLOBAL</span>") >= 0)
                strType = Global;
            else
                strType = Define;
            return strType;
        }

        private static string WebPageAdjustSdkTitles(string source, string search)
        {
            search = "(" + search + ")[ ]?";    // group the search keyword and optional trailing space
            var pattern = h1Open + groupedAnyString + search + h1Close;
            source = Regex.Replace(source, pattern, h1Open + "$1$2 (Typed)" + h1Close);
            pattern = titleOpen + groupedAnyString + search + titleClose;
            source = Regex.Replace(source, pattern, titleOpen + "$1$2 (Typed)" + titleClose);
            return source;
        }

        private static string WebPageReplaceTitles(string source, string search, string replace)
        {
            var pattern = h1Open + search + h1Close;
            source = Regex.Replace(source, pattern, h1Open + replace + h1Close);
            pattern = titleOpen + search + titleClose;
            source = Regex.Replace(source, pattern, titleOpen + replace + titleClose);
            return source;
        }

        static private void ReplaceWebPageTitle(hhtopic topic)
        {
            var path = topic.FileName;
            string fileWhole = File.ReadAllText(topic.FileName);
            string newFileWhole = fileWhole;
            bool isSdk = path.IndexOf(pageTypedVoSDK, StringComparison.OrdinalIgnoreCase) > 0;
            switch (topic.Type)
            {
                case FileType.ListOfFields:
                    newFileWhole = WebPageReplaceTitles(fileWhole, TitleFunctionFieldsPageOld, TitleFunctionFieldsPageNew);
                    break;
                case FileType.ListOfFunctions:
                    newFileWhole = WebPageReplaceTitles(fileWhole, TitleFunctionMethodsPageOld, TitleFunctionMethodsPageNew);
                    break;
                case FileType.FunctionClass:
                    newFileWhole = WebPageReplaceTitles(fileWhole, TitleFunctionClassPageOld, TitleFunctionClassPageNew);
                    break;
                case FileType.Define:
                    string strType = DetermineFieldType(fileWhole);
                    newFileWhole = WebPageReplaceTitles(fileWhole, "Functions." + groupedAnyString + "Field", "$1" + strType);
                    break;
                case FileType.Function:
                    newFileWhole = WebPageReplaceTitles(fileWhole, "Functions." + groupedAnyString + "Method", "$1Function$2");
                    break;
                case FileType.TypedsdkClass:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Class");
                    break;
                case FileType.TypedsdkConstructor:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Constructor");
                    break;
                case FileType.TypedsdkMethod:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Method");
                    break;
                case FileType.TypedsdkProperty:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Property");
                    break;
                case FileType.ListOfMethods when isSdk:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Methods");
                    break;
                case FileType.ListOfProperties when isSdk:
                    newFileWhole = WebPageAdjustSdkTitles(fileWhole, "Properties");
                    break;
                default:
                    break;
            }
            if (newFileWhole == fileWhole)
            {
                Console.WriteLine("Could not edit Title for file: {0}", path);
                return;
            }
            var writer = new StreamWriter(path);
            writer.Write(newFileWhole);
            writer.Close();
            return;
        }
        static private void ReplaceListTitles(hhtopic topic)
        {
            var path = topic.FileName;
            var type = topic.Type;
            // these do not have list titles that need to be edited
            if (type == FileType.Define || type == FileType.Function || type == FileType.Other)
            {
                return;
            }
            // For all lines
            string[] fileLines = File.ReadAllLines(path);
            List<Tuple<string, int>> newLines = new List<Tuple<string, int>>();
            int i = 0, count = 0;
            foreach (string line in fileLines)
            {
                // If the line contains both these we assume it is the correct line.
                // If this classifies wrong the editTag function should do nothing
                if (line.Contains(listTagStart) && line.Contains(listTagEnd))
                {
                    if (line.Contains(listMethodOld))
                    {
                        string newLine = editTag(line, listMethodOld, listFunctions, listTagStart, listTagEnd);
                        Tuple<string, int> tupleLine = new Tuple<string, int>(newLine, i);
                        newLines.Add(tupleLine);
                        count++;
                    }
                    else if (line.Contains(listFieldsOld))
                    {
                        string newLine = editTag(line, listFieldsOld, listGlobalsDefines, listTagStart, listTagEnd);
                        Tuple<string, int> tupleLine = new Tuple<string, int>(newLine, i);
                        newLines.Add(tupleLine);
                        count++;
                    }
                }
                i++;
            }
            if (count != 0)
            {
                foreach (Tuple<string, int> linePair in newLines)
                {
                    fileLines[linePair.Item2] = linePair.Item1;
                }
            }
            else
            {
                Console.WriteLine("Could not replace one or more ListTitles for file: {0} ", path);
                return;
            }
            var writer = new StreamWriter(path);
            foreach (string line in fileLines)
            {
                writer.WriteLine(line);
            }
            writer.Close();
            return;
        }

        static string ReplaceHhcTitle(string original, string oldField, string newField, string strType = "")
        {
            string pattern = hhcTocStart + oldField + hhcTocEnd;
            string newPattern = hhcTocStart + newField + hhcTocEnd;
            string output = Regex.Replace(original, pattern, newPattern);
            return output;
        }
        static string editHhcTocField(string original, hhtopic topic)
        {
            string output = original;
            //Remove the last occurrence of 'Field'
            var regex = new Regex(Regex.Escape(TitleFieldOld), RegexOptions.RightToLeft | RegexOptions.IgnoreCase);
            output = regex.Replace(output, " " + topic.FieldType, 1);
            return output;
        }
        static string EditHhcTocMethod(string original, hhtopic topic)
        {
            string output = original;
            //Remove the last occurrence of 'Method'
            var regex = new Regex(Regex.Escape(TitleMethodOld), RegexOptions.RightToLeft | RegexOptions.IgnoreCase);
            if (!string.IsNullOrEmpty(topic.Assembly))
            {
                output = regex.Replace(output, TitleFunction + " (" + topic.Assembly + ")", 1);
            }
            else
            {
                output = regex.Replace(output, TitleFunction, 1);
            }
            return output;
        }
        static string changeHhcTocField(string original, hhtopic topic)
        {
            string output = original;
            switch (topic.Type)
            {
                case FileType.FunctionClass:
                    output = ReplaceHhcTitle(original, TitleFunctionClassPageOld, TitleFunctionClassPageNew);
                    break;
                case FileType.ListOfFields:
                    output = ReplaceHhcTitle(original, TitleFunctionFieldsPageOld, TitleFunctionFieldsPageNew);
                    break;
                case FileType.ListOfFunctions:
                    output = ReplaceHhcTitle(original, TitleFunctionMethodsPageOld, TitleFunctionMethodsPageNew);
                    break;
                case FileType.Define:
                    output = editHhcTocField(original, topic);
                    break;
                case FileType.Function:
                    output = EditHhcTocMethod(original, topic);
                    break;
            }
            return output;
        }

        internal static string fileNameToAssemblyName(string fileName)
        {
            var assemblyName = "";
            if (fileName.ToLower().StartsWith("M_XSharp_", StringComparison.OrdinalIgnoreCase))
            {
                assemblyName = fileName.Substring(9);
                assemblyName = assemblyName.Substring(0, assemblyName.IndexOf("_"));
            }
            else if (fileName.ToLower().StartsWith("Overload_XSharp_", StringComparison.OrdinalIgnoreCase))
            {
                assemblyName = fileName.Substring(16);
                assemblyName = assemblyName.Substring(0, assemblyName.IndexOf("_"));
            }
            return assemblyName;
        }

        static string getAsmName(string fileName, FileType fileType)
        {
            var assemblyName = "";
            if (fileName.IndexOf("_Functions_", StringComparison.OrdinalIgnoreCase) > 0)
            {
                assemblyName = fileNameToAssemblyName(fileName);
            }
            else
            {
                bool isSdk = fileName.IndexOf(pageTypedVoSDK, StringComparison.OrdinalIgnoreCase) > 0;
                switch (fileType)
                {
                    case FileType.TypedsdkProperty:
                    case FileType.TypedsdkMethod:
                    case FileType.TypedsdkConstructor:
                    case FileType.TypedsdkClass:
                        assemblyName = "Typed";
                        break;
                    case FileType.ListOfMethods when isSdk:
                    case FileType.ListOfProperties when isSdk:
                        assemblyName = "Typed";
                        break;
                }
            }
            return assemblyName;
        }


        static FileType ClassifyHhcTopic(string line, string folder, out string strType, out string assemblyName)
        {
            strType = "";
            assemblyName = "";
            if (Regex.IsMatch(line, hhcTocTypeStart + hhcTocTypeMiddle + groupedAnyString + hhcTocTypeEnd))
            {
                string fileName = Regex.Replace(line, anyString + hhcTocTypeStart + hhcTocTypeMiddle + groupedAnyString + hhcTocTypeEnd + anyString, "$1");
                var fileType = findFileType(fileName);
                if (fileType == FileType.Define)
                {
                    strType = DetermineFieldType(folder, fileName);
                }
                assemblyName = getAsmName(fileName, fileType);

                return fileType;
            }
            return FileType.Other;
        }
        [DebuggerDisplay("{Type}")]
        internal class hhtopic
        {
            internal int Line;
            internal string FileName;
            internal FileType Type;
            internal string FieldType;
            internal string Assembly;
            internal int SeeAlso;
        }

        static public void EditHhc(string path)
        {
            string[] fileLines = File.ReadAllLines(path);
            string folder = Path.GetDirectoryName(path);
            folder = Path.Combine(folder, @"Output\HtmlHelp1");

            var topics = new List<hhtopic>();
            int i = 0;
            foreach (string line in fileLines)
            {
                // when the line starts with <param name=\"Local\"
                // then this is the line following the toc entry
                if (line.Contains(hhcTocTypeStart) && line.Contains(hhcTocTypeEnd))
                {
                    string strType;
                    string strAssembly;
                    var type = ClassifyHhcTopic(line, folder, out strType, out strAssembly);
                    if (type != FileType.Other)
                    {
                        topics.Add(new hhtopic() { Line = i - 1, Type = type, FieldType = strType, Assembly = strAssembly });
                    }
                }
                i++;
            }
            foreach (var topic in topics)
            {
                string output = changeHhcTocField(fileLines[topic.Line], topic);
                if (fileLines[topic.Line] == output)
                {
                    Console.WriteLine("Could not edit the hhc field of {2} on line {0} of type {1}", topic.Line, topic.Type, path);
                    continue;
                }
                fileLines[topic.Line] = output;
            }
            var writer = new StreamWriter(path);
            foreach (string line in fileLines)
            {
                writer.WriteLine(line);
            }
            writer.Close();
            return;
        }

        static FileType ClassifyHhkTopic(string line, string folder, out string strType, out string assemblyName)
        {
            strType = "";
            assemblyName = "";
            if (Regex.IsMatch(line, hhkTypeStart + groupedAnyString + hhkTypeEnd))
            {
                string fileName = Regex.Replace(line, anyString + hhkTypeStart + groupedAnyString + hhkTypeEnd + anyString, "$1");
                var fileType = findFileType(fileName);
                if (fileType == FileType.Define)
                {
                    strType = DetermineFieldType(folder, fileName);
                }
                assemblyName = getAsmName(fileName, fileType);

                return fileType;
            }
            else return FileType.Other;
        }
        static string EditHhkField(string original, hhtopic topic)
        {
            string output = original;
            var regex = new Regex(Regex.Escape(hhkFieldMethodPrefix));  // Remove "Functions."
            output = regex.Replace(output, string.Empty, 1);
            regex = new Regex(Regex.Escape(hhkFieldPostfix), RegexOptions.RightToLeft | RegexOptions.IgnoreCase);//Remove the last occurrence of 'Field'
            output = regex.Replace(output, " " + topic.FieldType, 1);
            return output;
        }

        static string EditHhkMethod(string original, hhtopic topic)
        {
            string output = original;
            var regex = new Regex(Regex.Escape(hhkFieldMethodPrefix));   // Remove "Functions."
            output = regex.Replace(output, string.Empty, 1);
            regex = new Regex(Regex.Escape(hhkMethodPostfix), RegexOptions.RightToLeft | RegexOptions.IgnoreCase);//Remove the last occurrence of 'Method'
            if (string.IsNullOrEmpty(topic.Assembly))
                output = regex.Replace(output, TitleFunction, 1); //TODO: perhaps make this lowercase?
            else
                output = regex.Replace(output, TitleFunction + " (" + topic.Assembly + ")", 1);
            return output;
        }

        static string EditHhkTypedMember(string original, hhtopic topic, string postFix)
        {
            string output = original;
            var regex = new Regex(Regex.Escape(postFix), RegexOptions.RightToLeft | RegexOptions.IgnoreCase);//Remove the last occurrence of 'Method'
            // just add the assembly name
            if (!string.IsNullOrEmpty(topic.Assembly) && output.IndexOf("XSharp.VO.SDK") == -1)
            {
                output = regex.Replace(output, "$0 (" + topic.Assembly + ")", 1);
            }
            return output;
        }

        static string EditHhkClass(string original, hhtopic topic)
        {
            string output = original;
            output = Regex.Replace(output, "(?i)" + hhkFunctionClassPageOld, TitleFunctionClassPageNew); //Todo: change output to lowercase? info: The i flag is for case insensitivity
            return output;
        }
        static string ChangeHhkTopic(string original, hhtopic topic)
        {
            string output = original;
            switch (topic.Type)
            {
                case FileType.Define:
                    output = EditHhkField(original, topic);
                    break;
                case FileType.Function:
                    output = EditHhkMethod(original, topic);
                    break;
                case FileType.TypedsdkMethod:
                    output = EditHhkTypedMember(original, topic, hhkMethodPostfix);
                    break;
                case FileType.TypedsdkConstructor:
                    output = EditHhkTypedMember(original, topic, hhkConstructorPostfix);
                    break;
                case FileType.TypedsdkProperty:
                    output = EditHhkTypedMember(original, topic, hhkPropertyPostfix);
                    break;
                case FileType.TypedsdkClass:
                    output = EditHhkTypedMember(original, topic, hhkClassPostfix);
                    break;
                case FileType.FunctionClass:
                    output = EditHhkClass(original, topic);
                    break;
                case FileType.ListOfFields:
                    output = ReplaceHhcTitle(original, hhkFieldPageOld, TitleFunctionFieldsPageNew);
                    break;
                case FileType.ListOfFunctions:
                    output = ReplaceHhcTitle(original, hhkMethodPageOld, TitleFunctionMethodsPageNew);
                    break;
                case FileType.ListOfMethods:
                    output = EditHhkTypedMember(original, topic, "methods");
                    break;
                case FileType.ListOfProperties:
                    output = EditHhkTypedMember(original, topic, "properties");
                    break;
            }
            return output;
        }
        static public void EditHhk(string path)
        {
            string[] fileLines = File.ReadAllLines(path);
            string folder = Path.GetDirectoryName(path);
            folder = Path.Combine(folder, @"Output\HtmlHelp1");
            var topics = new List<hhtopic>();
            int lastSeeAlso = -1;
            for (int i = 0; i < fileLines.Length; i++)
            {
                var line = fileLines[i];
                // when the line starts with <param name=\"Local\"
                // then this is the line following the toc entry
                if (line.Contains(hhkSeeAlso))
                {
                    lastSeeAlso = i;
                }
                if (line.Contains(hhkTypeStart) && line.Contains(hhkTypeEnd))
                {
                    string strType;
                    string asmName;
                    var type = ClassifyHhkTopic(line, folder, out strType, out asmName);
                    if (type != FileType.Other)
                    {
                        topics.Add(new hhtopic() { Line = i - 1, Type = type, FieldType = strType, Assembly = asmName, SeeAlso = lastSeeAlso });
                        lastSeeAlso = -1;
                    }
                }
                else if (line.Contains(hhkSpecialCase) || line.Contains(hhkSpecialCaseReplace)) //make sure it is also working if we run it a second time.
                {
                    // SeeAlso
                    topics.Add(new hhtopic() { Line = i - 1, Type = FileType.FunctionClass });
                    string temp = Regex.Replace(line, hhkSpecialCase, hhkSpecialCaseReplace);// inplace editing is a bit easier for the see also line.
                    if (temp == fileLines[i])
                    {
                        Console.WriteLine("Could not edit the hhk field of {2} on line {0} of type {1}", i, FileType.FunctionClass, path);
                    }
                    else
                    {
                        fileLines[i] = temp;
                    }
                }
            }
            foreach (var topic in topics)
            {
                string output = ChangeHhkTopic(fileLines[topic.Line], topic);
                if (fileLines[topic.Line] == output)
                {
                    Console.WriteLine("Could not edit the hhk field of {2} on line {0} of type {1}", topic.Line, topic.Type, path);
                    continue;
                }
                fileLines[topic.Line] = output;
                // update header for 2 or more functions with the same name
                if (output.IndexOf("Function") > 0)
                {
                    if (topic.SeeAlso != -1 && topic.Type == FileType.Function)
                    {
                        fileLines[topic.SeeAlso - 1] = fileLines[topic.SeeAlso - 1].Replace("method\"", "Function\"");
                        fileLines[topic.SeeAlso] = fileLines[topic.SeeAlso].Replace("method\"", "Function\"");
                    }
                }
            }
            var writer = new StreamWriter(path);
            foreach (string line in fileLines)
            {
                writer.WriteLine(line);
            }
            writer.Close();
            return;
        }


        static void editFileForMSHV(hhtopic topic)
        {
            string fileText = File.ReadAllText(topic.FileName);
            string newText = fileText;
            bool errorKw = false;
            bool errorTitle = false;
            switch (topic.Type)
            {
                case FileType.Define:
                    {
                        // replace title
                        string pattern = MSHVTitleStart + "Functions." + safeGroupedAnyString + TitleFieldOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + "$1" + MSHVTitleEnd);
                        if (newText == fileText)
                        { errorTitle = true; }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + "Functions." + safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1 " + topic.FieldType + MSHVKeywordEnd);
                        pattern = MSHVKeywordStart + safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1 " + topic.FieldType + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + "$1 " + topic.FieldType + MSHVKeywordEnd);
                        if (checkCopy == newText)
                        { errorKw = true; }
                        break;
                    }
                case FileType.Function:
                    {
                        // replace title
                        string pattern = MSHVTitleStart + "Functions." + groupedAnyString + "Method" + groupedAnyString + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + "$1" + "Function" + "$2" + MSHVTitleEnd);
                        if (newText == fileText)
                        { errorTitle = true; }
                        // replace keywords
                        string checkCopy = newText;
                        // overloaded function names do not have keywords but are redirected to their shared page, that's why we check here if this is even necessary
                        if (newText.Contains(MSHVKeywordStart))
                        {
                            pattern = MSHVKeywordStart + "Functions." + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + TitleFunction + MSHVKeywordEnd);
                            pattern = MSHVKeywordStart + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + TitleFunction + MSHVKeywordEnd);
                            pattern = MSHVKeywordStartTwo + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + "$1" + TitleFunction + MSHVKeywordEnd);
                            if (checkCopy == newText)
                            { errorKw = true; }
                        }
                        break;
                    }
                case FileType.ListOfFields:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionFieldsPageOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionFieldsPageNew + MSHVTitleEnd);
                        if (newText == fileText) { errorTitle = true; }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionFieldsPageNew + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionFieldsPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText)
                        { errorKw = true; }
                        break;
                    }
                case FileType.FunctionClass:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionClassPageOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionClassPageNew + MSHVTitleEnd);
                        if (newText == fileText)
                        { errorTitle = true; }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionClassPageNew + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionClassPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText)
                        { errorKw = true; }
                        break;
                    }
                case FileType.ListOfFunctions:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionMethodsPageOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionMethodsPageNew + MSHVTitleEnd);
                        if (newText == fileText)
                        { errorTitle = true; }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionMethodsPageNew + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionMethodsPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText)
                        { errorKw = true; }
                        break;
                    }
            }
            if (errorTitle)
            {
                Console.WriteLine("Could not edit title of {0} of type {1}", topic.FileName, topic.Type);
            }
            if (errorKw)
            {
                Console.WriteLine("Could not edit keywords of {0} of type {1}", topic.FileName, topic.Type);
            }
            var writer = new StreamWriter(topic.FileName);
            writer.Write(newText);
            writer.Close();
            return;
        }
        static public void EditWebsiteTOC(string path)
        {
            string text = File.ReadAllText(path);
            string[] seperatingChars = { "<div class=\"leftNav\" id=\"leftNav\">", "<div class=\"topicContent\" id=\"TopicContent\">" };
            string[] seperatedText = text.Split(seperatingChars, StringSplitOptions.RemoveEmptyEntries);
            bool edited = false;
            // if the length is not good
            if (seperatedText.Length != 3)
            {
                if (!path.Contains("GeneralError") && !path.Contains("PageNotFound"))
                {
                    Console.WriteLine("Could not find table for webiste topics for file: {0}", path);
                }
                return;
            }
            string toc = seperatedText[1];
            //adjust the tags:
            string[] seperateTags = Regex.Split(toc, "(?=<)");
            for (int i = 0; i < seperateTags.Length; i++)
            {
                string tag = seperateTags[i];
                // find type of the title (if it matches the pattern)
                Match match = Regex.Match(tag, "/(\\w*)\\.htm");
                if (match.Success)
                {
                    string tagReference = match.Groups[1].Value;
                    FileType tocItemType = findFileType(tagReference);
                    if (tocItemType != FileType.Other) { edited = true; }
                    else { continue; }
                    //edit accordingly
                    string newTag = EditWebsiteTopic(tag, tocItemType);
                    if (newTag == tag)
                    {
                        Console.WriteLine("Could not replace a websiteTocItem of type: {0} for file {1}", tocItemType, path);
                    }
                    seperateTags[i] = newTag;
                }
            }
            if (!edited) { return; }
            string newToc = string.Join("", seperateTags);
            var writer = new StreamWriter(path);
            writer.Write(seperatedText[0]);
            writer.Write("<div class=\"leftNav\" id=\"leftNav\">");
            writer.Write(newToc);
            writer.Write("<div class=\"topicContent\" id=\"TopicContent\">");
            writer.Write(seperatedText[2]);
            writer.Close();
            return;
        }
        static string EditWebsiteTopic(string tag, FileType tocItemType)
        {
            string newTag = tag;
            switch (tocItemType)
            {
                case FileType.FunctionClass:
                    newTag = Regex.Replace(tag, ">" + TitleFunctionClassPageOld, ">" + TitleFunctionClassPageNew);
                    break;
                case FileType.ListOfFields:
                    newTag = Regex.Replace(tag, ">" + TitleFunctionFieldsPageOld, ">" + TitleFunctionFieldsPageNew);
                    break;
                case FileType.ListOfFunctions:
                    newTag = Regex.Replace(tag, ">" + TitleFunctionMethodsPageOld, ">" + TitleFunctionMethodsPageNew);
                    break;
                case FileType.Define:
                    newTag = Regex.Replace(tag, ">" + "([a-zA-z0-9_\\(\\)]*)" + " Field", ">" + "$1");
                    break;
                case FileType.Function:
                    newTag = Regex.Replace(tag, ">" + "([a-zA-z0-9_\\(\\)]*)" + " Method", ">" + "$1" + " Function"); break;
                case FileType.Other:
                    break;
            }

            return newTag;
        }
        static public void EditWebsiteFolder(BuildProcess builder, string path)
        {
            string[] htmlFilesPaths = Directory.GetFiles(path, "*.htm", SearchOption.TopDirectoryOnly);
            int counter = 0;
            foreach (string filePath in htmlFilesPaths)
            {
                EditWebsiteTOC(filePath);
                counter++;
                if (counter % 500 == 0)
                {
                    builder.ReportProgress("   Adjusted {0} pages", counter);
                }
            }
            builder.ReportProgress("   Finished adjusting {0} pages", counter);
        }

        static public void EditXSharpTypeNames(string path)
        {
            string delimiters = "(>|&lt;|\\(|to |, |value=\"|content=\")"; // the start of all the delimiters
            string Array = "__Array";
            string ArrayBase = "__ArrayBase";
            string Date = "__Date";
            string Float = "__Float";
            string FoxArray = "__FoxArray";
            string Symbol = "__Symbol";
            string Psz = "__Psz";
            string Usual = "__Usual";
            string Binary = "__Binary";
            string Currency = "__Currency";
            string ArrayReplace = "Array";
            string FoxArrayReplace = "FoxArray";
            string ArrayBaseReplace = "Array Of";
            string DateReplace = "Date";
            string SymbolReplace = "Symbol";
            string UsualReplace = "Usual";
            string BinaryReplace = "Binary";
            string CurrencyReplace = "Currency";
            string PszReplace = "Psz";
            string FloatReplace = "Float";
            Dictionary<string, string> replacements = new Dictionary<string, string>();
            replacements.Add(ArrayBase, ArrayBaseReplace);
            replacements.Add(Array, ArrayReplace);
            replacements.Add(Date, DateReplace);
            replacements.Add(Float, FloatReplace);
            replacements.Add(FoxArray, FoxArrayReplace);
            replacements.Add(Symbol, SymbolReplace);
            replacements.Add(Psz, PszReplace);
            replacements.Add(Usual, UsualReplace);
            replacements.Add(Currency, CurrencyReplace);
            replacements.Add(Binary, BinaryReplace);
            replacements.Add("Int32", "Long");
            replacements.Add("UInt32", "DWord");
            replacements.Add("UInt16", "Word");
            replacements.Add("Int16", "Short");
            replacements.Add("Double", "Real8");
            replacements.Add("Single", "Real4");
            replacements.Add("Boolean", "Logic");
            string allText = File.ReadAllText(path);
            bool usualType = false;
            if (allText.IndexOf("__UsualType") >= 0)
            {
                usualType = true;
                allText = allText.Replace("__UsualType", "__XUsualType");
            }
            string or = "|";
            string pattern = delimiters + "(" + string.Join(or, new List<string>(replacements.Keys).ToArray()) + ")";
            Regex regex = new Regex(pattern);
            MatchCollection coll = regex.Matches(allText);
            string newText = "";
            // regex lambda magic to replace using the dictionary
            //System.Diagnostics.Debugger.Break();
            newText = regex.Replace(allText, replace =>
            {
                if (replacements.ContainsKey(replace.Groups[2].Value))
                {
                    return replace.Groups[1].Value + replacements[replace.Groups[2].Value];
                }
                else return replace.Value;
            });
            if (usualType)
            {
                newText = newText.Replace("__XUsualType", "__UsualType");
            }
            if (newText != allText)
            {
                var writer = new StreamWriter(path);
                writer.Write(newText);
                writer.Close();
            }
            return;
        }
    }

}
