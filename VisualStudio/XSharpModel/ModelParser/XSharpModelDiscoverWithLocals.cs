//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using System;
using System.Collections.Generic;

namespace XSharpModel
{

    /// <summary>
    /// This class finds the types, namespaces, locals and regions <br/>
    /// Internally it is NOT thread safe.
    /// Its properties return Immutable Lists or Arrays.
    /// </summary>
    internal class XSharpModelDiscoverWithLocals : XSharpModelDiscover
    {
        private readonly Stack<XSharpParser.LocalvarContext> _localDecls;
        public XSharpModelDiscoverWithLocals(XFile file, XSharpParser.SourceContext ctx, IEnumerable<XError> errors) : base(file, ctx, errors)
        {
            this._localDecls = new Stack<XSharpParser.LocalvarContext>();

        }

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            // Reset TypeList for this file
            this.File.SetTypes(_types, _usings, _staticusings, true);
        }

        protected override void endMember(LanguageService.SyntaxTree.ParserRuleContext context)
        {
            addVariables(context);
            base.endMember(context);
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            try
            {
                if (context.DataType != null)
                {
                    XVariable local;
                    String localType = context.DataType.GetText();
                    String localName;
                    // Push to stack so we can manage all contexts in one loop
                    _localDecls.Push(context);

                    while (_localDecls.Count > 0)
                    {
                        XSharpParser.LocalvarContext tmpContext = _localDecls.Pop();
                        localName = tmpContext.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(tmpContext), new TextInterval(tmpContext),
                            localType);
                        local.File = this._file;
                        local.IsArray = tmpContext.Dim != null;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                }
                else
                {
                    // We may have something like
                    // LOCAL x,y as STRING
                    // for x, we don't have a DataType, so save it
                    _localDecls.Push(context);
                }
            }
            catch (Exception ex)
            {
                Support.Debug("EnterLocalvar : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }
        protected override void addVariables([NotNull] ParserRuleContext context)
        {
            if (this._currentMethod == null)
                return;
            // Don't forget to add Self and Super as Local vars
            if ((context is XSharpParser.ConstructorContext) ||
                (context is XSharpParser.DestructorContext) ||
                (context is XSharpParser.MethodContext) ||
                (context is XSharpParser.PropertyContext))
            {
                XVariable local;
                //
                local = new XVariable(this._currentMethod, "Self", Kind.Local, Modifiers.Public,
                    new TextRange(context), new TextInterval(context),
                    this._currentMethod.ParentName);
                //
                local.File = this._file;
                this._currentMethod.Locals.Add(local);
                //
                if (!String.IsNullOrEmpty(_currentMethod.Parent.ParentName))
                {
                    local = new XVariable(this._currentMethod, "Super", Kind.Local, Modifiers.Public,
                    new TextRange(context), new TextInterval(context),
                    this._currentMethod.Parent.ParentName);
                    local.File = this._file;
                    this._currentMethod.Locals.Add(local);
                }
                //

            }
        }
        public override void EnterImpliedvar([NotNull] XSharpParser.ImpliedvarContext context)
        {
            try
            {
                if (context.Expression is XSharpParser.PrimaryExpressionContext)
                {
                    XSharpParser.PrimaryExpressionContext primaryEx = (XSharpParser.PrimaryExpressionContext)context.Expression;
                    XSharpParser.PrimaryContext primary = primaryEx.Expr;

                    if (primary is XSharpParser.LiteralExpressionContext)
                    {
                        // LOCAL IMPLIED xxx:= "azertyuiop"
                        XSharpParser.LiteralExpressionContext lit = (XSharpParser.LiteralExpressionContext)primary;
                        XVariable local;
                        String localType = buildLiteralValue(lit.Literal);
                        String localName;

                        localName = context.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(context), new TextInterval(context),
                            localType);
                        local.File = this._file;
                        local.IsArray = false;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                    else if (primary is XSharpParser.NameExpressionContext)
                    {
                        // LOCAL IMPLIED xx:= otherLocalVar
                        XVariable local;
                        XSharpParser.NameExpressionContext expr = (XSharpParser.NameExpressionContext)primary;
                        string name = expr.Name.Id.GetText();
                        //
                        String localName;
                        localName = context.Id.GetText();
                        String localType = buildValueName(name);
                        if (localType == XVariable.VarType)
                        {
                            XVariable xVar = findLocal(name);
                            //
                            local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                                new TextRange(context), xVar.Interval,
                                XVariable.VarType);
                        }
                        else
                        {
                            //
                            local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                                new TextRange(context), new TextInterval(context),
                                localType);
                        }
                        local.File = this._file;
                        local.IsArray = false;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                    else if (primary is XSharpParser.CtorCallContext)
                    {
                        // LOCAL IMPLIED xxxx:= List<STRING>{ }
                        XVariable local;
                        XSharpParser.CtorCallContext expr = (XSharpParser.CtorCallContext)primary;
                        XCodeTypeReference typeRef = buildDataType(expr.Type);
                        //
                        String localType = typeRef.TypeName;
                        String localName;
                        localName = context.Id.GetText();
                        //
                        local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                            new TextRange(context), new TextInterval(context),
                            localType);
                        local.File = this._file;
                        local.IsArray = false;
                        //
                        if (this._currentMethod != null)
                        {
                            this._currentMethod.Locals.Add(local);
                        }
                    }
                }
                else if (context.Expression is XSharpParser.MethodCallContext)
                {
                    // LOCAL IMPLIED xxxxx:= Obj:MethodCall()
                    XSharpParser.MethodCallContext callCtxEx = (XSharpParser.MethodCallContext)context.Expression;
                    XSharpParser.ExpressionContext exprCtx = callCtxEx.Expr;
                    String mtdCall = exprCtx.GetText();
                    XVariable local;
                    String localName;
                    localName = context.Id.GetText();
                    //
                    local = new XVariable(this._currentMethod, localName, Kind.Local, Modifiers.Public,
                        new TextRange(context), new TextInterval(exprCtx),
                        XVariable.VarType);
                    local.File = this._file;
                    local.IsArray = false;
                    //
                    if (this._currentMethod != null)
                    {
                        this._currentMethod.Locals.Add(local);
                    }
                }
            }
            catch (Exception ex)
            {
                Support.Debug("EnterImpliedvar : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }
        protected XVariable findLocal(string name)
        {
            XVariable xVar = null;
            if (this._currentMethod != null)
            {
                xVar = this._currentMethod.Locals.Find(x => String.Equals(x.Name, name, StringComparison.InvariantCultureIgnoreCase));
            }
            return xVar;
        }


        protected string buildValueName(string name)
        {
            String foundType = "";
            if (this._currentMethod != null)
            {
                XVariable xVar = findLocal(name);
                if (xVar != null)
                {
                    foundType = xVar.TypeName;
                }
            }
            return foundType;
        }


    }
}