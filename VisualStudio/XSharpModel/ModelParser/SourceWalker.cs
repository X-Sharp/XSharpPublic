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
using LanguageService.CodeAnalysis;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;

namespace XSharpModel
{
    public class SourceWalker : IDisposable, IErrorListener
    {
        private string _source;
        private ITextSnapshot _snapshot;
        private IXSharpProject _prjNode;
        private XFile _file;
        private XSharpParser.SourceContext _tree;
        private ITokenStream _tokenStream;
        private bool _hasParseErrors;
        
        public SourceWalker(XFile file, ITextSnapshot snapshot)
        {
            _file = file;
            _prjNode = _file?.Project?.ProjectNode;
            Snapshot = snapshot;
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

        public ITextSnapshot Snapshot
        {
            get
            {
                return _snapshot;
            }
            set
            {
                _snapshot = value;
                _source = _snapshot.GetText();
                _tree = null;
                _tokenStream = null;
            }
        }
        public ITokenStream TokenStream => _tokenStream;
        public XSharpParser.SourceContext Tree =>_tree;
        public bool HasParseErrors => _hasParseErrors;
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

        IList<XError> _errors;
        public ITokenStream Lex()
        {
            System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Lex()");
            ITokenStream tokenStream;
            _errors = new List<XError>();
            bool ok = XSharp.Parser.VsParser.Lex(_source, _file.FullPath, _file.Project.ProjectNode.ParseOptions, 
                this, out tokenStream);
            lock (this)
            {
                _hasParseErrors = !ok;
                if (ok)
                {
                    _tree = null;
                    _tokenStream = tokenStream;
                }
                else
                {
                    _tree = null;
                    _tokenStream = null ;
                    _hasParseErrors = false;
                }
                _file.HasParseErrors = _hasParseErrors;
            }
            System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Lex()");
            return this._tokenStream;
        }

        public XSharpParser.SourceContext Parse()
        {
            System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Parse()");
            ITokenStream tokenStream;
            XSharpParser.SourceContext tree;
            var options = _file.Project.ProjectNode.ParseOptions;
            if (options == null)
            {
                options = XSharpParseOptions.Default;
            }
            _errors = new List<XError>();
            bool ok = XSharp.Parser.VsParser.Parse(_source, _file.FullPath, _file.Project.ProjectNode.ParseOptions,
                this, out tokenStream, out tree); 
            lock (this)
            { 
                _hasParseErrors = !ok;
                if (ok)
                {
                    _tokenStream = tokenStream;
                    _tree = tree;
                }
                else
                {
                    _tokenStream = null;
                    _tree = null;
                }
                _file.HasParseErrors = _hasParseErrors;
            }
            System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Parse()");
            return _tree;
        }
            

        object _gate = new object();
        void ShowErrorsAsync(LanguageService.CodeAnalysis.SyntaxNode syntaxRoot)
        {
            // To list errors: But how to add to errorlist from here ?
            if (_prjNode == null)
                return;
            System.Diagnostics.Trace.WriteLine("-->> SourceWalker.ShowErrorsAsync()");
            //var thread = new System.Threading.Thread(delegate ()
            //{
            // wait 2 seconds to allow continuous typing. The error may have disappeared in 2 seconds
            //System.Threading.Thread.Sleep(2000);
            var current = _errors.ToImmutableList();
            lock (_gate)
            {
                string path = _file.FullPath;
                _prjNode.ClearIntellisenseErrors(path);
                if (current != null && _prjNode.IsDocumentOpen(path))
                {
                    foreach (var diag in current)
                    {
                        var span = diag.Span;
                        var loc = span.Start;
                        var length = span.End.Character - span.Start.Character + 1;

                        _prjNode.AddIntellisenseError(path, loc.Line + 1, loc.Character + 1, length ,diag.ErrCode, diag.ToString(), diag.Severity);
                    }
                }
                _prjNode.ShowIntellisenseErrors();
            }
            //});
            //thread.Start();
            System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.ShowErrorsAsync()");
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

                    XSharpModelDiscover mdiscover;
                    if (buildLocals)
                        mdiscover = new XSharpModelDiscoverWithLocals(_file,xTree, _errors);
                    else
                        mdiscover = new XSharpModelDiscover(_file, xTree, _errors);

    	            var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                    //
                    // Walk the tree. The XSharpModelDiscover class will build the model.
                    walker.Walk(mdiscover, xTree);
                    // Callback for LibraryManager
                    if ( _file.Project != null)
                        _file.Project.FileWalkComplete?.Invoke(_file);
                    //
                }
                catch (Exception e)
                {
                    Support.Debug("SourceWalker.BuildModel failed: "+e.Message);
                }
			}
		}

        #region IDisposable Support
        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                _errors = null;
                _file = null;
                _prjNode = null;
                _tree = null;
                _tokenStream = null;
                _snapshot = null;
            }

        }

        public void Dispose()
        {
            Dispose(true);
        }


        #endregion
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            _errors.Add(new XWarning(fileName, span, errorCode, message, args));
        }
        #endregion
    }


}
