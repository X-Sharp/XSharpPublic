//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.CodeAnalysis.XSharp.Syntax;
using System.Collections.Immutable;
namespace XSharpModel
{
    public class SourceWalker
    {
        private string _source;
        private readonly ITextSnapshot _snapshot;
        private IXSharpProject _prjNode;

        private XFile _file;

        private ITokenStream _TokenStream;
        private bool _hasParseErrors;
        public SourceWalker(XFile file, ITextSnapshot snapshot)
        {
            _file = file;
            _prjNode = _file?.Project?.ProjectNode;
            _snapshot = snapshot;
            _source = snapshot.GetText();
        }
        public SourceWalker(XFile file, string source)
        {
            _file = file;
            _prjNode = _file?.Project?.ProjectNode;
            _snapshot = null;
            _source = source;
        }

        public SourceWalker(XFile file)
        {
            _file = file;
            _prjNode = _file?.Project?.ProjectNode;
            string fullPath = _file.FullPath;
            if (_file.IsXaml)
            {
                fullPath = System.IO.Path.Combine(_prjNode.IntermediateOutputPath, System.IO.Path.GetFileName(fullPath));
                fullPath = System.IO.Path.ChangeExtension(fullPath, ".g.prg");
            }
            if (System.IO.File.Exists(fullPath))
            {
                _source = System.IO.File.ReadAllText(fullPath);
            }
        }

        public ITextSnapshot Snapshot => _snapshot;


        public ITokenStream TokenStream
        {
            get
            {
                return _TokenStream;
            }

        }

        public bool HasParseErrors => _hasParseErrors;

        public ITokenStream LexFile()
        {
            try
            {
                // create new parseoptions because we only lex.
                var opts = XSharpParseOptions.Default;
                opts.SetOptions(parseOptions.CommandLineArguments);
                opts.ParseLevel = ParseLevel.Lex;
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(_source, opts, _file.FullPath);
                var syntaxRoot = tree.GetRoot();
                _TokenStream = ((CompilationUnitSyntax)syntaxRoot).XTokens;
            }
            catch (Exception e)
            {
                Support.Debug("SourceWalker.Lexfile: "+ e.Message);
            }
            return _TokenStream;            
        }
        
        XSharpParseOptions parseOptions
        {
            get
            {
                if (_prjNode == null)
                {
                    return XSharpParseOptions.Default;
                }
                else
                {
                    return _prjNode.ParseOptions;
                }

            }
        }

        public XSharpParser.SourceContext Parse()
        {
            XSharpParser.SourceContext xTree = null;
            try
            {
                
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(_source, parseOptions, _file.FullPath);
                var syntaxRoot = tree.GetRoot();

                // Disabled for now . We may want to enable this for the current document only
                // ShowErrorsAsync(syntaxRoot);

                // Get the antlr4 parse tree root
                var cu = syntaxRoot as CompilationUnitSyntax;
                xTree = cu.XSource;
                _TokenStream = cu.XTokens;
                if (cu.ContainsDiagnostics)
                {
                    var errors = cu.GetDiagnostics().Where(d => d.Severity == LanguageService.CodeAnalysis.DiagnosticSeverity.Error);
                    _hasParseErrors = errors.Count() > 0;
                }
                else
                {
                    _hasParseErrors = false;
                }
            }
            catch (Exception e)
            {
                xTree = null;
                _TokenStream = null;
                Support.Debug("SourceWalker.Parse: "+e.Message);
                _hasParseErrors = true;
            }
            return xTree;
        }

        IEnumerable<LanguageService.CodeAnalysis.Diagnostic> errors = null;
        object _gate = new object();
        void ShowErrorsAsync(LanguageService.CodeAnalysis.SyntaxNode syntaxRoot)
        {
            // To list errors: But how to add to errorlist from here ?
            if (_prjNode == null)
                return;
            lock (_gate)
            {
                errors = syntaxRoot.GetDiagnostics();
            }
            if (errors == null)
                return;
            //var thread = new System.Threading.Thread(delegate ()
            //{
                // wait 2 seconds to allow continuous typing. The error may have disappeared in 2 seconds
                //System.Threading.Thread.Sleep(2000);
                IEnumerable<LanguageService.CodeAnalysis.Diagnostic> current;
                lock (_gate)
                {
                    current = errors;
                    string path = _file.FullPath;
                    _prjNode.ClearIntellisenseErrors(path);
                    if (current != null && _prjNode.IsDocumentOpen(path))
                    {
                        foreach (var diag in current)
                        {
                            var span = diag.Location.GetLineSpan();
                            var loc = span.StartLinePosition;
                            var length = span.Span.End.Character - span.Span.Start.Character + 1;
                            _prjNode.AddIntellisenseError(path, loc.Line + 1, loc.Character + 1,length ,diag.Id, diag.GetMessage(), diag.Severity);
                        }
                    }
                    _prjNode.ShowIntellisenseErrors();
                }
            //});
            //thread.Start();
        }

        public void BuildModel(XSharpParser.SourceContext xTree, bool buildLocals )
        {
            // abort when the project is unloaded or when no project
            // because in these cases there is no need to build a model
            if (_prjNode == null || !_file.Project.Loaded)
                return;

            //
            if (xTree != null )
            {
                try
                {
	        	    var mdiscover = new XSharpModelDiscover(_file, buildLocals);
    	            var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    //
                    // Walk the tree. The XSharpModelDiscover class will build the model.
                    walker.Walk(mdiscover, xTree);
            	}
                catch (Exception e)
                {
                    Support.Debug("SourceWalker.BuildModel failed: "+e.Message);
                }
			}
		}
    }
}
