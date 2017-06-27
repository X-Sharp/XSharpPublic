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
namespace XSharpModel
{
    public class SourceWalker
    {
        private IClassificationType _xsharpRegionStartType;
        private IClassificationType _xsharpRegionStopType;
        private ITextSnapshot _snapshot;
        private string _source;
        private string _fullPath;
        private List<ClassificationSpan> _regionTags;
        private IXSharpProject _prjNode;

        private XFile _file;

        private ITokenStream _TokenStream;
        private XSharpParser.SourceContext _xTree;
        private bool _hasParseErrors;

        public ITextSnapshot Snapshot
        {
            set
            {
                _snapshot = value;
                _source = _snapshot.GetText();
            }
        }


        public string FullPath
        {
            set
            {
                _fullPath = value;
                var file = XSolution.FindFullPath(value);
                if (file == null)
                    file = new XFile(_fullPath);
                this.File = file;       // This also sets the _prjNode and _source

            }
        }

        public XFile File
        {
            set
            {
                System.Diagnostics.Debug.Assert(value != null);
                if (value != null)
                {
                    _file = value;
                    _prjNode = _file?.Project?.ProjectNode;
                    _fullPath = _file.FullPath;
                    if (_source == null )
                    {
                        if (_file.IsXaml)
                        {
                            _fullPath = System.IO.Path.Combine(_prjNode.IntermediateOutputPath, System.IO.Path.GetFileName(_fullPath));
                            _fullPath = System.IO.Path.ChangeExtension(_fullPath, ".g.prg");
                        }
                        if (System.IO.File.Exists(_fullPath))
                        {
                            _source = System.IO.File.ReadAllText(_fullPath);
                        }
                    }
                }
            }

            get
            {
                return this._file;
            }
        }

        public List<ClassificationSpan> RegionTags
        {
            get
            {
                return _regionTags;
            }

        }

        public ITokenStream TokenStream
        {
            get
            {
                return _TokenStream;
            }

        }

        public bool HasParseErrors => _hasParseErrors;

        // Unused ?
        //public string Source
        //{
        //    get
        //    {
        //        return _source;
        //    }

        //    set
        //    {
        //        _source = value;
        //    }
        //}

        public SourceWalker()
        {
            //
            _regionTags = new List<ClassificationSpan>();
        }

        public SourceWalker(IClassificationTypeRegistryService registry):this()
        {
            if (registry != null)
            {
                _xsharpRegionStartType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStartFormat);
                _xsharpRegionStopType = registry.GetClassificationType(ColorizerConstants.XSharpRegionStopFormat);
            }
            //
        }


        public ITokenStream LexFile()
        {
            try
            {
                // create new parseoptions because we only lex.
                var opts = XSharpParseOptions.Default;
                opts.SetOptions(parseOptions.CommandLineArguments);
                opts.ParseLevel = ParseLevel.Lex;
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(_source, opts, _fullPath);
                var syntaxRoot = tree.GetRoot();
                _xTree = null;
                _TokenStream = ((CompilationUnitSyntax)syntaxRoot).XTokenStream;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
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

        public void InitParse()
        {
            _xTree = null;
            //

            try
            {
                
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(_source, parseOptions, _fullPath);
                var syntaxRoot = tree.GetRoot();
               
                // Disabled for now . We may want to enable this for the current document only
                // ShowErrorsAsync(syntaxRoot);

                 // Get the antlr4 parse tree root
                _xTree = ((CompilationUnitSyntax)syntaxRoot).XSource;
                _TokenStream = ((CompilationUnitSyntax)syntaxRoot).XTokenStream;
                _hasParseErrors = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).ContainsDiagnostics;
                //
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
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
                    string path = File.FullPath;
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


        public void BuildRegionTagsOnly()
        {
            if (!_hasParseErrors)
            {
                var discover = new XSharpModelRegionDiscover(_file);
                discover.BuildRegionTags = true;
                discover.BuildLocals = true;
                discover.BuildModel = false;
                //
                if (_xTree != null && _snapshot != null)
                {
                    var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    //
                    discover.Snapshot = _snapshot;
                    discover.xsharpRegionStartType = _xsharpRegionStartType;
                    discover.xsharpRegionStopType = _xsharpRegionStopType;
                    // Walk the tree. The TreeDiscover class will collect the tags.
                    walker.Walk(discover, _xTree);
                }
                //
                _regionTags = discover.tags;
            }
        }


        public void BuildModelOnly()
        {
            // abort when the project is unloaded or when no project
            // because in these cases there is no need to build a model
            if (_prjNode == null || !_file.Project.Loaded)
                return;

            //
            var discover = new XSharpModelRegionDiscover(_file);
            discover.BuildRegionTags = false;
            discover.BuildModel = true;
            if (_file != null)
            {
                if (_file.Project.Loaded)
                {
                    discover.BuildModel = !_file.Parsed;
                }
            }
            //
            if (_xTree != null )
            {
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                //
                // Walk the tree. The TreeDiscover class will build the model.
                walker.Walk(discover, _xTree);
            }
        }

        public void BuildModelAndRegionTags()
        {
            //
            var discover = new XSharpModelRegionDiscover(_file);
            discover.BuildRegionTags = (_snapshot != null && !_hasParseErrors);
            discover.BuildModel = true;
            discover.BuildLocals = true;

            //
            if (_xTree != null)
            {
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                //
                if (_snapshot != null)
                {
                    discover.Snapshot = _snapshot;
                    discover.xsharpRegionStartType = _xsharpRegionStartType;
                    discover.xsharpRegionStopType = _xsharpRegionStopType;
                }
                // Walk the tree. The TreeDiscover class will build the model.
                // Make sure that when the is an error during the walking
                // that we will not abort.
                try
                {
                    walker.Walk(discover, _xTree);
                }
                catch (Exception e)
                {
                    System.Diagnostics.Debug.WriteLine(e.Message);
                }
            }
            if ( discover.BuildRegionTags )
            {
                _regionTags = discover.tags;
            }
        }
    }
}
