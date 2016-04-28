using Microsoft.VisualStudio.Shell.Design.Serialization;
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    [ComVisibleAttribute(true)]
    [PermissionSetAttribute(SecurityAction.LinkDemand, Name = "FullTrust")]
    [PermissionSetAttribute(SecurityAction.InheritanceDemand, Name = "FullTrust")]
    public partial class XSharpCodeDomProvider : CodeDomProvider
    {
        protected XSharpCodeGenerator xsGenerator;

        public XSharpCodeDomProvider()
        {
            this.xsGenerator = new XSharpCodeGenerator();
        }

        [Obsolete]
        public override ICodeCompiler CreateCompiler()
        {
            return this.xsGenerator;
        }

        [Obsolete]
        public override ICodeGenerator CreateGenerator()
        {
            return this.xsGenerator;
        }

        [Obsolete]
        public override ICodeParser CreateParser()
        {
            return new XSharpCodeParser();
        }

        // Called by the WinForm designer at save time
        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
            // Does that CodeCompileUnit comes from a "Merged" unit ?
            if (compileUnit.UserData.Contains("XSharp:HasDesigner"))
            {
                // Retrieve the Form Class
                CodeNamespace designerNamespace;
                CodeTypeDeclaration designerClass = XSharpCodeDomHelper.FindDesignerClass(compileUnit, out designerNamespace);
            }
            else
                base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
            //
#if WRITE2LOGFILE
            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            path = Path.Combine(path, "XSharpDumpCodeCompileUnit_generate.log");
            XSharpCodeDomHelper.DumpCodeCompileUnit(compileUnit, path, true);
#endif
        }

        // Called by the WinForm designer at load time
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit compileUnit = null;

            // If the TextReader is a DocDataTextReader, we should be running from VisualStudio, called by the designer
            // So, we will guess the FileName to check if we have a .Designer.Prg file at the same place.
            // If so, we will have to handle both .prg to produce two CodeCompileUnit, then we will merge the result into one, with markers in it
            // so we can split again when the Designer is willing to save. ( See GenerateCodeFromCompileUnit )
            if (codeStream is DocDataTextReader)
            {
                // Anyway, we have that source, just parse it.
                compileUnit = base.Parse(codeStream);
                // Now, we should check if we have a partial Class inside, if so, that's a Candidate for .Designer.prg
                CodeNamespace nameSpace;
                CodeTypeDeclaration className;
                if (XSharpCodeDomHelper.HasPartialClass(compileUnit, out nameSpace, out className))
                {
                    // Ok, so get the Filename, to get the .Designer.prg
                    DocDataTextReader ddtr = codeStream as DocDataTextReader;
                    DocData dd = ((IServiceProvider)ddtr).GetService(typeof(DocData)) as DocData;
                    String prgFileName = dd.Name;
                    // Build the Designer FileName
                    String designerPrgFile = XSharpCodeDomHelper.BuildDesignerFileName(prgFileName);
                    if (File.Exists(designerPrgFile))
                    {
                        // Ok, we have a candidate !!!
                        DocData docdata = new DocData((IServiceProvider)ddtr, designerPrgFile);
                        DocDataTextReader reader = new DocDataTextReader(docdata);
                        // so parse
                        CodeCompileUnit designerCompileUnit = base.Parse(reader);
                        // Now we have Two CodeCompileUnit, we must merge them
                        compileUnit = XSharpCodeDomHelper.MergeCodeCompileUnit(compileUnit, designerCompileUnit);
                        compileUnit.UserData["XSharp:HasDesigner"] = true;
                        compileUnit.UserData["XSharp:FileName"] = prgFileName;
                    }
                }
            }
            else
            {
                compileUnit = base.Parse(codeStream);
            }
            //
#if WRITE2LOGFILE
            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            path = Path.Combine(path, "XSharpDumpCodeCompileUnit_parse.log");
            XSharpCodeDomHelper.DumpCodeCompileUnit(compileUnit, path, true);
#endif
            return compileUnit;
        }

    }
}
