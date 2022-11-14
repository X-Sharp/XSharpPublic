using System;
using System.Collections.Generic;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Diagnostics;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Shell.Design.Serialization.CodeDom;

namespace XSharp.CodeDom
{

    internal static class Helpers
    {
        public static CodeDomDesignerData GetDesignerData(this CodeObject e)
        {
            if (e.UserData.Contains(typeof(CodeDomDesignerData)))
            {
                return (CodeDomDesignerData)e.UserData[typeof(CodeDomDesignerData)];
            }
            return null;
        }
        /// <summary>
        /// Sort members on the line/column in which they are declared
        /// New members (without line/column) are sorted at the end of the list
        /// </summary>
        /// <param name="members"></param>
        /// <returns></returns>
        internal static CodeTypeMemberCollection SortMembers(CodeTypeMemberCollection members)
        {
            CodeTypeMemberCollection result = new CodeTypeMemberCollection();
            var items = new System.Collections.SortedList();
            var  processed = new List<string>();
            var newfields = new List<CodeMemberField>();
            var hasfields = false;
            for (int i = 0; i < members.Count; i++)
            {
                var member = members[i];
                if (member is CodeMemberField && member.HasSourceCode())
                {
                    hasfields = true;
                }
                // HACK: prevent duplicate items: there is an error in their code
                // or our code that adds duplicates. This 
                if (member is IXCodeObject)
                {
                    var source = member.GetSourceCode().ToLower().Trim();
                    if (processed.Contains(source))
                        continue;
                   processed.Add(source);
                }
 
                int line, col;
                var data = member.GetDesignerData();
                if (data != null)
                {
                    line = data.CaretPosition.Y;
                    col = data.CaretPosition.X;
                }
                else if (member is CodeMemberField field)
                {
                    line = col = -1;
                    newfields.Add(field);
                }
                else
                {
                    line = col = 999_999_999;
                }
                if (line != -1)
                {
                    var key = line.ToString("D10") + col.ToString("D10") + i.ToString("D10");
                    items.Add(key, member);
                }
            }
            if (! hasfields)
            {
                foreach (var field in newfields)
                {
                    result.Add(field);
                }
                newfields.Clear();
            }
            foreach (System.Collections.DictionaryEntry item in items)
            {
                if (!(item.Value is CodeMemberField))
                {
                    // Insert new fields before the methods in the source file.
                    if (newfields.Count > 0)
                    {
                        result.AddRange(newfields.ToArray());
                        newfields.Clear();
                    }
                }
                result.Add((CodeTypeMember)item.Value);
            }
            return result;
        }
    }

    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }
    public class XSharpCodeParser : CodeParser
    {
        IProjectTypeHelper _projectNode;
        public XSharpCodeParser(IProjectTypeHelper projectNode)
        {
            this.FileName = "";
            _projectNode = projectNode;
        }
        public XSharpCodeParser(IProjectTypeHelper projectNode, CodeTypeDeclaration formClass)
        {
            this.FileName = "";
            _projectNode = projectNode;
            typeInMainFile = formClass;
        }
        public string FileName { get; set; }

        CodeTypeDeclaration typeInMainFile = null;
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit ccu;
            if (codeStream == null)
            {
                ccu = new XCodeCompileUnit();
            }
            else
            {
                string codeFile;
                codeFile = codeStream.ReadToEnd();
                var field = codeStream.GetType().GetField("ClassName");
                if (field != null)
                {
                    typeInMainFile = field.GetValue(codeStream) as CodeTypeDeclaration;
                }
                ccu = Parse(codeFile);
            }
            return ccu;
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="source"></param>
        /// <returns></returns>
        public CodeCompileUnit Parse(string source)
        {
            XCodeCompileUnit ccu = new XCodeCompileUnit();
            //
            try
            {
                var reporter = new ErrorIgnorer();
                ITokenStream tokenStream;
                LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParserRuleContext xtree;
                bool ok = XSharp.Parser.VsParser.Parse(source, this.FileName, _projectNode.ParseOptions, reporter, out tokenStream, out xtree, out _);

                // We need to d 2 steps here:
                // 1 - Scan for the fields , so we know the difference between fields and properties when we perform step 2
                // 2 - Scan for the rest. We pass the list of fields to the tree discover code so it "knows" about all fields

                var discoverFields = new XSharpFieldsDiscover(_projectNode, typeInMainFile);
                discoverFields.SourceCode = source;
                discoverFields.CurrentFile = this.FileName;

                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                walker.Walk(discoverFields, xtree);
                // now the discoverFields class should contain a Dictionary with <context, FieldList>
                var discover = new XSharpClassDiscover(_projectNode, typeInMainFile);
                discover.FieldList = discoverFields.FieldList;
                discover.SourceCode = source;
                discover.CurrentFile = this.FileName;
                //
                walker.Walk(discover, xtree);
                //
                ccu = discover.CodeCompileUnit;
                var firstType = ccu.GetFirstClass();
                if (firstType != null)
                {
                    // save a copy of the member list to the CCU
                    // Sort them first
                    ccu.Members = Helpers.SortMembers(firstType.Members); 
                }
                // save file name & original source
                ccu.FileName = this.FileName;
                ccu.Source = source;
            }
            catch (Exception ex)
            {
                if (System.Diagnostics.Debugger.IsAttached)
                    Debug.WriteLine(ex.Message + Environment.NewLine + ex.StackTrace);
            }
            //
            return ccu;
        }
       
    }
}
