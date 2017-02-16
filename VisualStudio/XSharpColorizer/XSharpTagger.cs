//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
//using Microsoft.VisualStudio.ComponentModelHost;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.Win32;

namespace XSharpColorizer
{
    internal class XSharpTagger
    {
        private IClassificationType xsharpIdentifierType;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;
        private IClassificationType xsharpRegionStartType;
        private IClassificationType xsharpRegionStopType;

        private List<ClassificationSpan> tags;

        internal static string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;

        static string _includedirs = null;

        static XSharpCommandLineParser xsparser;
        static XSharpParseOptions xsparseoptions;

        static string Includedirs
        {
            get
            {
                if (string.IsNullOrEmpty(_includedirs))
                {
                    _includedirs = "";
                    var path = (string)Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
                    if (!string.IsNullOrEmpty(path))
                    {
                        if (!path.EndsWith("\\"))
                        path += @"\";
                        path += @"Include\";
                        _includedirs += path;
                    }
                    // Check for Vulcan path
                    var key = @"HKEY_LOCAL_MACHINE\SOFTWARE\Grafx\Vulcan.NET";
                    path = (string)Registry.GetValue(key, "InstallPath", "");
                    if (!string.IsNullOrEmpty(path))
                    {
                        if (!path.EndsWith("\\"))
                            path += @"\";
                        path += @"Include\";
                        _includedirs += ";"+path;
                    }
                }
                return _includedirs;
            }
        }
        static XSharpTagger()
        {
            XSharpSpecificCompilationOptions.SetDefaultIncludeDir(Includedirs);
            XSharpSpecificCompilationOptions.SetWinDir(Environment.GetFolderPath(Environment.SpecialFolder.Windows));
            XSharpSpecificCompilationOptions.SetSysDir(Environment.GetFolderPath(Environment.SpecialFolder.System));
            xsparser = XSharpCommandLineParser.Default;
            var args = new List<string>();
            var cmdlineopts = xsparser.Parse(args, "", "", "");
            xsparseoptions = cmdlineopts.ParseOptions;
        }

        internal XSharpTagger(IClassificationTypeRegistryService registry)
        {
            xsharpIdentifierType = registry.GetClassificationType("identifier");
            xsharpBraceOpenType = registry.GetClassificationType("punctuation");
            xsharpBraceCloseType = registry.GetClassificationType("punctuation");
            xsharpRegionStartType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
            xsharpRegionStopType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            //
            tags = new List<ClassificationSpan>();
        }

        internal List<ClassificationSpan> Tags
        {
            get
            {
                return this.tags;
            }
        }

        internal List<ClassificationSpan> GetClassifications()
        {
            List<ClassificationSpan> classifications = new List<ClassificationSpan>();
            //
            foreach (var tag in Tags)
            {
                classifications.Add(new ClassificationSpan(tag.Span, tag.ClassificationType));
            }
            return classifications;
        }


        internal void Parse(ITextSnapshot snapshot, out LanguageService.SyntaxTree.ITokenStream TokenStream, string path)
        {
            string source = snapshot.GetText();
            // Currently we "eat" all Exception that might be raised
            // by XSharpSyntaxTree.ParseText
            TokenStream = null;
#if GETPROJECTOPTIONS
            EnvDTE80.DTE2 dte = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(EnvDTE.DTE)) as EnvDTE80.DTE2;
            var projectitem = dte?.Solution?.FindProjectItem(path);
            dynamic project = projectitem?.ContainingProject;
            dynamic xproject = project.Project; // XSharpProjectNode
            var dialect = xproject.GetProjectProperty("Dialect", false);
            args.Add( "/dialect:" + dialect );
            dynamic refcont = xproject.GetReferenceContainer();
            foreach (dynamic refnode in refcont.EnumReferences())
            {
                args.Add("/r:" + refnode.Url);
            }
#endif
            try
            {
                // this gets at least the default include path     
                // so we can process Vulcan and XSharp include files           

                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source, xsparseoptions, path);
                var syntaxRoot = tree.GetRoot();
                // To list errors: But how to add to errorlist from here ?
                // if (syntaxRoot.ContainsDiagnostics)
                // {
                //    var diags = syntaxRoot.GetDiagnostics();
                //    foreach (var diag in diags)
                //    {
                //        var loc = diag.Location;
                //        var msg = diag.GetMessage();
                        
                //    }
                // }
                // Get the antlr4 parse tree root
                var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;
                TokenStream = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XTokenStream;
                //
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                var discover = new XSharpTreeDiscover();
                discover.Snapshot = snapshot;
                discover.xsharpBraceCloseType = xsharpBraceCloseType;
                discover.xsharpBraceOpenType = xsharpBraceOpenType;
                discover.xsharpIdentifierType = xsharpIdentifierType;
                discover.xsharpRegionStartType = xsharpRegionStartType;
                discover.xsharpRegionStopType = xsharpRegionStopType;
                // Walk the tree. The TreeDiscover class will collect the tags.
                walker.Walk(discover, xtree);
                this.tags = discover.tags;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
    }
}
