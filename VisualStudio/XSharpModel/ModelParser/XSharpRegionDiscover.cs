//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.SyntaxTree.Tree;
using Microsoft.VisualStudio.Text;

namespace XSharpModel
{
    partial class XSharpModelRegionDiscover : XSharpBaseListener
    {
        internal List<ClassificationSpan> tags = new List<ClassificationSpan>();
        public ITextSnapshot Snapshot { get; set; }


        private bool _reInitModel;
        private bool _buildModel;
        private bool _buildRegionTags;
        private bool _buildLocals = false;

        public bool BuildModel
        {
            get
            {
                return _buildModel;
            }

            internal set
            {
                _buildModel = value;
                _reInitModel = value;
            }
        }

        public bool BuildRegionTags
        {
            get
            {
                return _buildRegionTags;
            }

            internal set
            {
                _buildRegionTags = value;
                _buildLocals = value;
            }
        }

        internal IClassificationType xsharpIdentifierType;
        internal IClassificationType xsharpBraceOpenType;
        internal IClassificationType xsharpBraceCloseType;
        internal IClassificationType xsharpRegionStartType;
        internal IClassificationType xsharpRegionStopType;


        public void RegionExitEveryRule([NotNull] ParserRuleContext context)
        {
            // we can probably move most of this to the Enter and Exit methods
            // for each type and member rule as well.
            try
            {
                if ((context is XSharpParser.Namespace_Context) ||
                    (context is XSharpParser.Class_Context) ||
                    (context is XSharpParser.Interface_Context) ||
                    (context is XSharpParser.Structure_Context) ||
                    (context is XSharpParser.Enum_Context) ||
                    (context is XSharpParser.PropertyContext) ||
                    (context is XSharpParser.PropertyAccessorContext) ||
                    (context is XSharpParser.Event_Context) ||
                    (context is XSharpParser.EventAccessorContext) )
                {
                    // already done
                    // BEGIN         NAMESPACE .... END NAMESPACE 
                    // use -2 because these rules all end with "END <something>"
                    TagRegion(context, context.ChildCount - 2);     
                }
                else if ((context is XSharpParser.VostructContext) ||
                    (context is XSharpParser.VounionContext) )
                {
                    TagRegion(context, context.ChildCount-1);
                }
                else if (context is XSharpParser.IdentifierContext)
                {
                    LanguageService.SyntaxTree.IToken sym = context.Start;
                    // Add tag for Keyword that is used as Identifier
                    if (XSharpLexer.IsKeyword(sym.Type))
                    {
                        TextSpan tokenSpan;
                        tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                        tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpIdentifierType));
                    }
                }
                else if (context is XSharpParser.StatementBlockContext)
                {
                    TagRegion((ParserRuleContext) context.Parent, context.Parent.ChildCount-1);
                }
                else if (context is XSharpParser.SwitchStmtContext)
                {
                    TagRegion((ParserRuleContext)context, context.ChildCount - 2);
                }
                else if (context is XSharpParser.CaseStmtContext)
                {
                    TagRegion((ParserRuleContext)context, context.ChildCount - 2);
                }

                else if (context is XSharpParser.Using_Context)
                {
                    // if we are the first in a list of usings then mark the whole list
                    // parent of using is Entity
                    // parent above that is namespace or source
                    var parent = context.Parent.Parent as ParserRuleContext;
                    var index = parent.children.IndexOf(context.Parent);
                    if (index > 0
                        && parent.children[index - 1].ChildCount > 0
                        && parent.children[index - 1].GetChild(0)  is XSharpParser.Using_Context)
                    {
                        ; // do nothing
                    }
                    else
                    {
                        index += 1;
                        var last = context as XSharpParser.Using_Context;
                        while (index < parent.children.Count)
                        {
                            var ent = parent.children[index];
                            if (ent is XSharpParser.EntityContext && ent.ChildCount > 0 && ent.GetChild(0) is XSharpParser.Using_Context)
                            {
                                last = ent.GetChild(0) as XSharpParser.Using_Context;
                                index += 1;
                            }
                            else
                            {
                                break;
                            }
                        }
                        if (last != context)
                        {
                            var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                            tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                            tokenSpan = new TextSpan(last.Stop.StartIndex - 1, 1);
                            tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("RegionExitEveryRule : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        private void TagRegion(ParserRuleContext context, int endChild)
        {
            var endToken = context.GetChild(endChild);
            if (endToken is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
            {
                LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)endToken).Symbol;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
            }
            else if (endToken is LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext)
            {
                XSharpParser.StatementBlockContext lastTokenInContext = endToken as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
            }
            else if (endToken is ParserRuleContext)
            {
                var  lastTokenInContext = endToken as ParserRuleContext;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
            }
        }

    }
}
