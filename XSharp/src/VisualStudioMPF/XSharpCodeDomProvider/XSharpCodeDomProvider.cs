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
            return new XSharpCodeParser( );
        }

        // Called by the WinForm designer at save time
        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
            base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
            //
#if WRITE2LOGFILE
            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            path = Path.Combine(path, "XSharpDumpCodeCompileUnit_generate.log");
            XSharpCodeDomHelper.DumpCodeCompileUnit(compileUnit, path, true);
#endif
        }

        // Called byt the WinForm designer at load time
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit compileUnit = base.Parse(codeStream);
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
