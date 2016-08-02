//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
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

        // The Tab setting is shared by all instance of our CodeDomProvider
        public static int TabSize { get; set; }
        public string FileName { get; set; }

        public XSharpCodeDomProvider()
        {
            this.xsGenerator = new XSharpCodeGenerator();
            XSharpCodeDomProvider.TabSize = 1;
        }

        public override string FileExtension
        {
            get
            {
                return ".prg";
            }
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
            var parser = new XSharpCodeParser();
            parser.TabSize = XSharpCodeDomProvider.TabSize;
            parser.FileName = this.FileName;
            return parser;
        }

        
        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {

            {
                //
                base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
                //
#if WRITE2LOGFILE
            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            path = Path.Combine(path, "XSharpDumpCodeCompileUnit_generate.log");
            XSharpCodeDomHelper.DumpCodeCompileUnit(compileUnit, path, true);
#endif
            }
        }


        // Called by the WinForm designer at load time
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit compileUnit = null;
            compileUnit = base.Parse(codeStream);
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
