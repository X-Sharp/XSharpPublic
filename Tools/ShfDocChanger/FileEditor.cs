using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace DocumentationChanger
{

    public class FileEditor
    {
        // Concrete visible replacements in the output
        const string seeAlsoNew = "Functions, Globals and Defines";
        const string TitleMethodNew = "Function"; // is added after the name
        const string TitleFunctionFieldsPageNew = "List of Globals and Defines";
        const string TitleFunctionMethodsPageNew = "List of Functions";
        const string TitleFunctionClassPageNew = "Functions, Globals and Defines";
        const string listMethodNew = "Functions";
        const string listFieldsNew = "Globals and Defines";

        //useful for Regex
        const string anyString = ".*";
        const string safeAnyString = "[^<>]*";
        const string groupedAnyString = "("+anyString+")";
        const string safeGroupedAnyString = "(" + safeAnyString + ")";

        // strings needed for replacing the title
        //mark the start and end of the . in the title!
        const string TitleRemoveTagStart = "span";
        const string TitleRemoveTagEnd = "script";

        const string TitleRemoveFunctions = "Functions"; //removes the functions before the . in function class methods and function class fields
        const string TitleFieldOld = " Field"; // The space is important! first occurence is removed
        const string TitleMethodOld = "Method";

        const string TitleFunctionFieldsPageOld = "Functions Fields";
        const string TitleFunctionMethodsPageOld = "Functions Methods";
        const string TitleFunctionClassPageOld = "Functions Class";

        //strings needed for replacing the seeAlsoField
        const string seeAlsoTagStart = "div class=\"seeAlsoStyle\"";
        const string seeAlsoTagEnd ="div";
        const string seeAlsoOld = "Functions Class";

        // strings needed for replacing the list titles
        const string listTagStart = "span class=\"collapsibleRegionTitle\"";
        const string listTagEnd = "span";
        const string listMethodOld = "Methods";
        const string listFieldsOld = "Fields";

        // strings to define/find the type of the files
        const string htmlNameFunctionsFields = "F_";
        const string htmlNameFunctionsFieldPage = "Fields_T";
        const string htmlNameFunctionMethodsPageStart = "Methods_T";
        const string htmlNameFunctionMethodsPageEnd = "_Functions";
        const string htmlNameFunctionsClassPageStart = "T_";
        const string htmlNameFunctionsClassPageEnd = "_Functions";
        const string htmlNameFunctionsMethodsRegex = "M_" + anyString + "_Functions_" + anyString;
        const string htmlNameOverloadedFunctionMethods = "Overload_";


        //needed for hhc editing
        const string hhcTocStart = "<param name=\"Name\" value=\"";
        const string hhcTocEnd = "\">";
        const string hhcTocTypeStart ="<param name=\"Local\" value=\"html\\";
        const string hhcTocTypeEnd =".htm\">";
        const string hhcTocTypeMiddle = "\\"; // We need an extra backslash for the regex to parse correctly

        //needed for hhk editing
        const string hhkTypeStart = "<param name=\"Local\" value=\"html/";
        const string hhkTypeEnd =hhcTocTypeEnd;
        const string hhkMethodPageOld = "methods";
        const string hhkFieldPageOld = "fields";
        const string hhkSpecialCase = "<param name=\"See Also\" value=\"Functions class\">";
        const string hhkSpecialCaseReplace = "<param name=\"See Also\" value=\""+TitleFunctionClassPageNew+"\">";
        const string hhkFieldMethodPrefix = "Functions.";
        const string hhkFieldPostfix = " field";
        const string hhkMethodPostfix = "method";
        const string hhkFunctionClassPageOld = "Functions class";

        //needed for editing for MSHV
        const string MSHVTitleStart = "<title>";
        const string MSHVTitleEnd = "</title>";
        const string MSHVKeywordStart = "<meta name=\"System.Keywords\" content=\"";
        const string MSHVKeywordStartTwo= "<meta name=\"Microsoft.Help.Keywords\" content=\"";
        const string MSHVKeywordEnd = "\" />";

        // Types of files for classifying
        public enum FileType
        {
            ClassDefinition,
            listOfMethod,
            listOfFields,
            singleMethod,
            singleField,
            other
        }

        // classifies a type based on its name.
        static public FileType findFileType(string htmlName)
        {
            if (htmlName.StartsWith(htmlNameFunctionsFieldPage)&&htmlName.Contains("Functions")){
                return FileType.listOfFields;
            }
            else if (htmlName.StartsWith(htmlNameFunctionsFields)&& htmlName.Contains("Functions"))
            {
                return FileType.singleField;
            }
            else if (htmlName.StartsWith(htmlNameFunctionMethodsPageStart) && htmlName.EndsWith(htmlNameFunctionMethodsPageEnd))
            {
                return FileType.listOfMethod;
            }
            else if (htmlName.StartsWith(htmlNameFunctionsClassPageStart) && htmlName.EndsWith(htmlNameFunctionsClassPageEnd))
            {
                return FileType.ClassDefinition;
            }
            else if (Regex.IsMatch(htmlName, htmlNameFunctionsMethodsRegex)||(htmlName.StartsWith(htmlNameOverloadedFunctionMethods) && htmlName.Contains("Functions"))){
                return FileType.singleMethod;
            }
            return FileType.other;
        }
        //edits a page at htmlName based on it's FileType
        static public void editPage(string htmlName,FileType type)
        {
            // we only want to edit files we actually need to edit.
            if (type == FileType.other)
            {
                return;
            }
            // All types need to replace their title
            replaceTitle(htmlName, type);
            //Edit the See Also for all but functions classPage
            if (type != FileType.ClassDefinition)
            {
                replaceSeeAlso(htmlName);
            }
            if (type != FileType.singleMethod && type != FileType.singleField)
            {
                replaceListTitles(htmlName, type);
            }
        }
            static public void editHtmlFolder(string path)
        {
            string[] htmlFilesPaths = Directory.GetFiles(path, "*.htm", SearchOption.TopDirectoryOnly);
            // Then we classify the Files into different types
            List<Tuple<string, FileType>> filesWithType = new List<Tuple<string,FileType>>(htmlFilesPaths.Length);
            foreach (string htmlFilePath in htmlFilesPaths)
            {
                string htmlFileName = htmlFilePath.Remove(0, path.Length + 1);// +1 comes from backslash character in the path ending.
                htmlFileName = htmlFileName.Remove(htmlFileName.Length - 4); // remove the .htm at the end of the string.
                FileType htmlType = findFileType(htmlFileName);
                //if (htmlType != FileType.other)
                //{
                //    Console.WriteLine(htmlFileName + "  |  " + htmlType);
                //}
                Tuple<string, FileType> fileWithType = new Tuple<string, FileType>(htmlFilePath, htmlType);
                filesWithType.Add(fileWithType);
            }
            //edit each html page
            foreach (Tuple<string, FileType> fileWithType in filesWithType)
            {
                // edit the page itself
                editPage(fileWithType.Item1, fileWithType.Item2);
                // edit all the typeNames on the page
                editForTypeNames(fileWithType.Item1);
            }
            return;
        }
        public FileEditor()
        {
        }
        static private string removeTag(string original, string startTag, string endTag)
        {
            string replacement = string.Empty;
            string regexTagStart = @"<" + startTag;
            string regexTagEnd = @"/" + endTag + ">";
            string regex = regexTagStart +anyString + regexTagEnd;
            string output = Regex.Replace(original,regex, replacement, RegexOptions.Singleline);
            return output;
        }
        static private string editTag(string original,string toEdit, string editInto, string startTag, string endTag)
        {
            string regexTagStart = @"<" + startTag;
            string regexTagEnd = @"/" + endTag + ">";
            string regex = anyString + regexTagStart + anyString + toEdit + anyString + regexTagEnd + anyString;
            string output = original;
            if (Regex.IsMatch(original, regex))
            {
                string regexReplace= regexTagStart + groupedAnyString + toEdit + groupedAnyString + regexTagEnd;
                string replacement = regexTagStart + "$1" + editInto + "$2" + regexTagEnd;
                output = Regex.Replace(original, regexReplace, replacement);
            }
            return output;
        }

        static private void replaceSeeAlso(string path)
        {
            // For all lines
            string[] fileLines = File.ReadAllLines(path);
            int i = 0, newLineIndex = -1;
            string newLine = "";
            foreach (string line in fileLines)
            {
                // If the line contains both these we assume it is the correct line
                if (line.Contains(seeAlsoOld)&&line.Contains(seeAlsoTagStart))
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
        static private void replaceTitle(string path, FileType type)
        {
            string fileWhole = File.ReadAllText(path);
            string titleLeft = "<h1>";
            string titleRight = "</h1>";
            string newFileWhole = fileWhole;
            string pattern;
            if (type == FileType.listOfFields)
            {
                pattern = titleLeft + TitleFunctionFieldsPageOld + titleRight;
                newFileWhole = Regex.Replace(fileWhole, pattern, titleLeft + TitleFunctionFieldsPageNew + titleRight);
            }
            else if (type == FileType.listOfMethod)
            {
                pattern = titleLeft + TitleFunctionMethodsPageOld + titleRight;
                newFileWhole = Regex.Replace(fileWhole, pattern, titleLeft + TitleFunctionMethodsPageNew + titleRight);
            }
            else if (type == FileType.ClassDefinition)
            {
                pattern = titleLeft + TitleFunctionClassPageOld + titleRight;
                newFileWhole = Regex.Replace(fileWhole, pattern, titleLeft + TitleFunctionClassPageNew + titleRight);
            }
            else if (type == FileType.singleField)
            {
                pattern = titleLeft + "Functions."+groupedAnyString+"Field" +titleRight;
                newFileWhole= Regex.Replace(fileWhole, pattern, titleLeft + "$1"+ titleRight);
            }
            else if (type == FileType.singleMethod)
            {
                pattern = titleLeft + "Functions." + groupedAnyString + "Method" + groupedAnyString+ titleRight;
                newFileWhole = Regex.Replace(fileWhole, pattern, titleLeft + "$1" + "Function" + "$2"+titleRight);
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
        static private void replaceListTitles(string path, FileType type)
        {
            // these do not have list titles that need to be edited
            if (type == FileType.singleField || type == FileType.singleMethod || type == FileType.other) { return; }
            // For all lines
            string[] fileLines = File.ReadAllLines(path);
            List<Tuple<string, int>> newLines = new List<Tuple<string, int>>();
            int i = 0, count = 0;
            foreach (string line in fileLines)
            {
                // If the line contains both these we assume it is the correct line. If this classifies wrong the editTag function should do nothing
                if (line.Contains(listTagStart)&&line.Contains(listTagEnd))
                {
                    if (line.Contains(listMethodOld))
                    {
                        string newLine = editTag(line, listMethodOld, listMethodNew, listTagStart, listTagEnd);
                        Tuple<string, int> tupleLine = new Tuple<string, int>(newLine, i);
                        newLines.Add(tupleLine);
                        count++;
                    }
                    else if (line.Contains(listFieldsOld))
                    {
                        string newLine = editTag(line, listFieldsOld, listFieldsNew, listTagStart, listTagEnd);
                        Tuple<string, int> tupleLine = new Tuple<string, int>(newLine, i);
                        newLines.Add(tupleLine);
                        count++;
                    }
                }
                i++;
            }
            if (count!=0)
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

        static public string replaceHhcTocField(string original, string oldField, string newField)
        {
            string pattern = hhcTocStart + oldField + hhcTocEnd;
            string newPattern = hhcTocStart + newField + hhcTocEnd;
            string output = Regex.Replace(original, pattern, newPattern);
            return output;
        }
        static public string editHhcTocField(string original, FileType type)
        {
            string output = original;
            if (type == FileType.singleField)
            {
                var regex = new Regex(Regex.Escape(TitleFieldOld), RegexOptions.RightToLeft|RegexOptions.IgnoreCase);//Remove the first occurrence of 'Field'
                output = regex.Replace(output, string.Empty, 1);
            }
            else if (type == FileType.singleMethod)
            {
                var regex = new Regex(Regex.Escape(TitleMethodOld), RegexOptions.RightToLeft|RegexOptions.IgnoreCase);//Remove the first occurrence of 'Method'
                output = regex.Replace(output, TitleMethodNew, 1);
            }
            return output;
        }
        static public string changeHhcTocField(string original, FileType type)
        {
            string output = original;
            switch (type) {
                case FileType.ClassDefinition: output = replaceHhcTocField(original, TitleFunctionClassPageOld, TitleFunctionClassPageNew); break;
                case FileType.listOfFields: output = replaceHhcTocField(original, TitleFunctionFieldsPageOld, TitleFunctionFieldsPageNew); break;
                case FileType.listOfMethod: output = replaceHhcTocField(original, TitleFunctionMethodsPageOld, TitleFunctionMethodsPageNew); break;
                case FileType.singleField: output = editHhcTocField(original, type); break;
                case FileType.singleMethod: output = editHhcTocField(original, type); break;
                case FileType.other: break;
            }
            return output;
        }
        static public FileType classifyHhcTocField(string line)
        {
            if (Regex.IsMatch(line, hhcTocTypeStart + hhcTocTypeMiddle + groupedAnyString+ hhcTocTypeEnd))
            {
                string fileName = Regex.Replace(line,anyString+ hhcTocTypeStart + hhcTocTypeMiddle+ groupedAnyString + hhcTocTypeEnd+anyString,"$1");
                return findFileType(fileName);
            }
            else return FileType.other;
        }
        static public void editHhc(string path)
        {
            string[] fileLines = File.ReadAllLines(path);
            List<Tuple<int,FileType>> noAndTypes = new List<Tuple<int, FileType>>();
            int i = 0;
            foreach (string line in fileLines)
            {
                    if (line.Contains(hhcTocTypeStart) && line.Contains(hhcTocTypeEnd))
                    {
                        Tuple<int, FileType> noAndType = new Tuple<int, FileType>(i - 1, classifyHhcTocField(line)); // We need to edit the previous line!
                        noAndTypes.Add(noAndType);
                    }
                i++;
            }
            foreach(Tuple<int, FileType> noAndType in noAndTypes)
            {
                string output=changeHhcTocField(fileLines[noAndType.Item1], noAndType.Item2);
                if (fileLines[noAndType.Item1]==output&&noAndType.Item2!=FileType.other){
                    Console.WriteLine("Could not edit the hhc field of {2} on line {0} of type {1}", noAndType.Item1, noAndType.Item2,path);
                    continue;
                }
                fileLines[noAndType.Item1] = output;
            }
            var writer = new StreamWriter(path);
            foreach (string line in fileLines)
            {
                writer.WriteLine(line);
            }
            writer.Close();
            return;
        }
        static public FileType classifyHhkField(string line)
        {
            if (Regex.IsMatch(line, hhkTypeStart + groupedAnyString + hhkTypeEnd))
            {
                string fileName = Regex.Replace(line, anyString + hhkTypeStart + groupedAnyString + hhkTypeEnd + anyString, "$1");
                return findFileType(fileName);
            }
            else return FileType.other;
        }
        static public string editHhkField(string original, FileType type)
        {
            string output = original;
            if (type == FileType.singleField)
            {
                var regex = new Regex(Regex.Escape(hhkFieldMethodPrefix));// Remove the first occurence of 'Functions.
                output = regex.Replace(output, string.Empty, 1);
                regex = new Regex(Regex.Escape(hhkFieldPostfix), RegexOptions.RightToLeft| RegexOptions.IgnoreCase);//Remove the first occurrence of 'Field'
                output = regex.Replace(output, string.Empty, 1);
            }
            else if (type == FileType.singleMethod)
            {
                var regex = new Regex(Regex.Escape(hhkFieldMethodPrefix));// Remove the first occurence of 'Functions.
                output = regex.Replace(output, string.Empty, 1);
                regex = new Regex(Regex.Escape(hhkMethodPostfix), RegexOptions.RightToLeft|RegexOptions.IgnoreCase);//Remove the first occurrence of 'Method'
                output = regex.Replace(output, TitleMethodNew, 1); //TODO: perhaps make this lowercase?
            }
            else if (type == FileType.ClassDefinition)
            {
                output = Regex.Replace(output, "(?i)"+hhkFunctionClassPageOld, TitleFunctionClassPageNew); //Todo: change output to lowercase? info: The i flag is for case insensitivity
            }
            return output;
        }
        static public string changeHhkField(string original, FileType type)
        {
            string output = original;
            switch (type)
            {
                case FileType.singleField: output = editHhkField(original, type); break;
                case FileType.singleMethod: output = editHhkField(original, type); break;
                case FileType.ClassDefinition: output = editHhkField(original, type); break;
                case FileType.listOfFields: output = replaceHhcTocField(original,hhkFieldPageOld,TitleFunctionFieldsPageNew); break;
                case FileType.listOfMethod: output = replaceHhcTocField(original, hhkMethodPageOld,TitleFunctionMethodsPageNew); break;
                case FileType.other: break;
            }
            return output;
        }
        static public void editHhk(string path)
        {
                string[] fileLines = File.ReadAllLines(path);
                List<Tuple<int, FileType>> noAndTypes = new List<Tuple<int, FileType>>();
                int i = 0;
                foreach (string line in fileLines)
                {
                    if (line.Contains(hhkTypeStart) && line.Contains(hhkTypeEnd))
                    {
                        Tuple<int, FileType> noAndType = new Tuple<int, FileType>(i - 1, classifyHhkField(line)); // We need to edit the previous line!
                        noAndTypes.Add(noAndType);
                    }
                    else if (line.Contains(hhkSpecialCase)||line.Contains(hhkSpecialCaseReplace)) //make sure it is also working if we run it a second time.
                    {
                        Tuple<int, FileType> noAndType = new Tuple<int, FileType>(i - 1, FileType.ClassDefinition); // We need to edit the previous line!
                        noAndTypes.Add(noAndType);
                        string temp = Regex.Replace(line, hhkSpecialCase, hhkSpecialCaseReplace);// inplace editing is a bit easier for the see also line.
                        if (temp == fileLines[i])
                        {
                            Console.WriteLine("Could not edit the hhk field of {2} on line {0} of type {1}", i, FileType.ClassDefinition, path);
                        }
                        else
                        {
                            fileLines[i] = temp;
                        }
                    }
                    i++;
                }
                foreach (Tuple<int, FileType> noAndType in noAndTypes)
                {
                    string output = changeHhkField(fileLines[noAndType.Item1], noAndType.Item2);
                    if (fileLines[noAndType.Item1] == output && noAndType.Item2 != FileType.other)
                    {
                        Console.WriteLine("Could not edit the hhk field of {2} on line {0} of type {1}", noAndType.Item1, noAndType.Item2, path);
                        continue;
                    }
                    fileLines[noAndType.Item1] = output;
                }
                var writer = new StreamWriter(path);
                foreach (string line in fileLines)
                {
                    writer.WriteLine(line);
                }
                writer.Close();
                return;
            }

        static public void editHtmlFolderForMSHV(string path)
        {
            string[] htmlFilesPaths = Directory.GetFiles(path, "*.htm", SearchOption.TopDirectoryOnly);
            // Then we classify the Files into different types
            List<Tuple<string, FileType>> filesWithType = new List<Tuple<string, FileType>>(htmlFilesPaths.Length);
            foreach (string htmlFilePath in htmlFilesPaths)
            {
                string htmlFileName = htmlFilePath.Remove(0, path.Length + 1);// +1 comes from backslash character in the path ending.
                htmlFileName = htmlFileName.Remove(htmlFileName.Length - 4); // remove the .htm at the end of the string.
                FileType htmlType = findFileType(htmlFileName);
                Tuple<string, FileType> fileWithType = new Tuple<string, FileType>(htmlFilePath, htmlType);
                filesWithType.Add(fileWithType);
            }
            foreach (Tuple<string, DocumentationChanger.FileEditor.FileType> fileWithType in filesWithType)
            {
                if (fileWithType.Item2 != FileType.other)
                {
                    FileEditor.editFileForMSHV(fileWithType.Item1, fileWithType.Item2);
                }
            }
            return;
        }
        static public void editFileForMSHV(string path, FileType type)
        {
            string fileText = File.ReadAllText(path);
            string newText = fileText;
            switch (type)
            {
                case FileType.singleField:
                    {
                        // replace title
                        string pattern = MSHVTitleStart + "Functions." + safeGroupedAnyString + TitleFieldOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + "$1" + MSHVTitleEnd);
                        if (newText == fileText) { Console.WriteLine("Could not edit title of {0} of type {1}", path, type); }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + "Functions." + safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + MSHVKeywordEnd);
                        pattern = MSHVKeywordStart +  safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeGroupedAnyString + " field" + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + "$1" + MSHVKeywordEnd);
                        if (checkCopy == newText) { Console.WriteLine("Could not edit keywords of {0} of type {1}", path, type); }
                        break;
                    }
                case FileType.singleMethod:
                    {
                        // replace title
                        string pattern = MSHVTitleStart +"Functions." + groupedAnyString + "Method" + groupedAnyString + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + "$1" + "Function" + "$2" + MSHVTitleEnd);
                        if (newText == fileText) { Console.WriteLine("Could not edit title of {0} of type {1}", path, type); }
                        // replace keywords
                        string checkCopy = newText;
                        // overloaded function names do not have keywords but are redirected to their shared page, that's why we check here if this is even necessary
                        if (newText.Contains(MSHVKeywordStart))
                        {
                            pattern = MSHVKeywordStart + "Functions." + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + TitleMethodNew + MSHVKeywordEnd);
                            pattern = MSHVKeywordStart + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStart + "$1" + TitleMethodNew + MSHVKeywordEnd);
                            pattern = MSHVKeywordStartTwo + safeGroupedAnyString + "method" + MSHVKeywordEnd;
                            newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + "$1" + TitleMethodNew + MSHVKeywordEnd);
                            if (checkCopy == newText) { Console.WriteLine("Could not edit keywords of {0} of type {1}", path, type); }
                        }
                        break;
                    }
                case FileType.listOfFields:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionFieldsPageOld+ MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionFieldsPageNew+ MSHVTitleEnd);
                        if (newText == fileText) { Console.WriteLine("Could not edit title of {0} of type {1}", path, type); }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionFieldsPageNew+MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionFieldsPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText) { Console.WriteLine("Could not edit keywords of {0} of type {1}", path, type); }
                        break;
                    }
                case FileType.ClassDefinition:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionClassPageOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionClassPageNew + MSHVTitleEnd);
                        if (newText == fileText) { Console.WriteLine("Could not edit title of {0} of type {1}", path, type); }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString+ MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionClassPageNew + MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionClassPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText) { Console.WriteLine("Could not edit keywords of {0} of type {1}", path, type); }
                        break;
                    }
                case FileType.listOfMethod:
                    {
                        string pattern = MSHVTitleStart + TitleFunctionMethodsPageOld + MSHVTitleEnd;
                        newText = Regex.Replace(fileText, pattern, MSHVTitleStart + TitleFunctionMethodsPageNew + MSHVTitleEnd);
                        if (newText == fileText) { Console.WriteLine("Could not edit title of {0} of type {1}", path, type); }
                        // replace keywords
                        string checkCopy = newText;
                        pattern = MSHVKeywordStart + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStart + TitleFunctionMethodsPageNew +MSHVKeywordEnd);
                        pattern = MSHVKeywordStartTwo + safeAnyString + MSHVKeywordEnd;
                        newText = Regex.Replace(newText, pattern, MSHVKeywordStartTwo + TitleFunctionMethodsPageNew + MSHVKeywordEnd);
                        if (checkCopy == newText) { Console.WriteLine("Could not edit keywords of {0} of type {1}", path, type); }
                        break;
                    }
            }
            var writer = new StreamWriter(path);
            writer.Write(newText);
            writer.Close();
            return;
        }
        static public void editWebsiteToc(string path)
        {
            string text = File.ReadAllText(path);
            string[] seperatingChars = { "<div class=\"leftNav\" id=\"leftNav\">", "<div class=\"topicContent\" id=\"TopicContent\">" };
            string[] seperatedText = text.Split(seperatingChars,StringSplitOptions.RemoveEmptyEntries);
            bool edited = false;
            // if the length is not good 
            if (seperatedText.Length!=3)
            {
                if (!path.Contains("GeneralError") && !path.Contains("PageNotFound"))
                {
                    Console.WriteLine("Could not find table for webiste topics for file: {0}", path);
                }
                return;
            }
            string toc=seperatedText[1];
            //adjust the tags:
            string[] seperateTags = Regex.Split(toc,"(?=<)");
            for(int i=0; i<seperateTags.Length;i++)
            {
                string tag = seperateTags[i];
                // find type of the title (if it matches the pattern)
                Match match = Regex.Match(tag, "/(\\w*)\\.htm");
                if (match.Success) {
                    string tagReference = match.Groups[1].Value;
                    FileType tocItemType=findFileType(tagReference);
                    if (tocItemType != FileType.other){edited = true;}
                    else { continue; }
                    //edit accordingly
                    string newTag = editWebsiteTocItem(tag,tocItemType);
                    if (newTag == tag)
                    {
                        Console.WriteLine("Could not replace a websiteTocItem of type: {0} for file {1}",tocItemType, path);
                    }
                    seperateTags[i] = newTag;
                }
            }
            if (!edited) { return; }
            string newToc=string.Join("", seperateTags);
            var writer = new StreamWriter(path);
            writer.Write(seperatedText[0]);
            writer.Write("<div class=\"leftNav\" id=\"leftNav\">");
            writer.Write(newToc);
            writer.Write("<div class=\"topicContent\" id=\"TopicContent\">");
            writer.Write(seperatedText[2]);
            writer.Close();
            return;
        }
        static public string editWebsiteTocItem(string tag, FileType tocItemType)
        {
            string newTag = tag;
            switch (tocItemType)
            {
                case FileType.ClassDefinition: newTag = Regex.Replace(tag, ">" + TitleFunctionClassPageOld, ">" + TitleFunctionClassPageNew); break;
                case FileType.listOfFields: newTag = Regex.Replace(tag, ">" + TitleFunctionFieldsPageOld, ">" + TitleFunctionFieldsPageNew); break;
                case FileType.listOfMethod: newTag = Regex.Replace(tag, ">" + TitleFunctionMethodsPageOld, ">" + TitleFunctionMethodsPageNew); break;
                case FileType.singleField: newTag = Regex.Replace(tag, ">" + "([a-zA-z0-9_\\(\\)]*)" + " Field", ">" + "$1"); break;
                case FileType.singleMethod: newTag = Regex.Replace(tag, ">" + "([a-zA-z0-9_\\(\\)]*)" + " Method", ">" + "$1" + " Function"); break;
                case FileType.other: return newTag;
            }

            return newTag;
        }
        static public void editForWebsiteFolder(string path)
        {
            string[] htmlFilesPaths = Directory.GetFiles(path, "*.htm", SearchOption.TopDirectoryOnly);
            foreach (string filePath in htmlFilesPaths)
            {
                editWebsiteToc(filePath);
            }
            return;
        }

        static public void editForTypeNames(string path)
        {
            string delimiters = "(>|&lt;|\\(|to |, |value=\"|content=\")__"; // the start of all the delimiters
            string Array = "Array";
            string ArrayBase = "ArrayBase";
            string Date = "Date";
            string Float = "Float";
            string Symbol= "Symbol";
            string Psz = "Psz";
            string Usual= "Usual";
            string ArrayReplace = "Array";
            string ArrayBaseReplace = "Array Of";
            string DateReplace = "Date";
            string SymbolReplace = "Symbol";
            string UsualReplace = "Usual";
            string PszReplace = "Psz";
            string FloatReplace = "Float";
            Dictionary<string, string> replacements = new Dictionary<string, string>();
            replacements.Add(ArrayBase, ArrayBaseReplace);
            replacements.Add(Array, ArrayReplace);
            replacements.Add(Date, DateReplace);
            replacements.Add(Float, FloatReplace);
            replacements.Add(Symbol, SymbolReplace);
            replacements.Add(Psz, PszReplace);
            replacements.Add(Usual, UsualReplace);
            string allText = File.ReadAllText(path);
            string or = "|";
            string pattern = delimiters +"("+string.Join(or, new List<string>(replacements.Keys).ToArray())+")";
            Regex regex = new Regex(pattern);
            MatchCollection coll = regex.Matches(allText);
            string newText = "";
            // regex lambda magic to replace using the dictionary
            newText = regex.Replace(allText, replace => {
                if (replacements.ContainsKey(replace.Groups[2].Value))
                {
                    return replace.Groups[1].Value+replacements[replace.Groups[2].Value];
                }
                else return replace.Value;
            });
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
