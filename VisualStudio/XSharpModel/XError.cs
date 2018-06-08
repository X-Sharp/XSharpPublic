//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class XWarning : XError
    {
        public XWarning(string path, LinePositionSpan span,
            string errCode, string message, object[] args) : base(path, span, errCode, message, args)
        {
            Severity = DiagnosticSeverity.Warning;
        }
    }
    public class XError
    {
        public string ErrCode { get; private set; }
        public LinePositionSpan Span { get; private set; }
        public string Message { get; private set; }
        public object[] Params { get; private set; }
        public string Path { get; private set; }
        public DiagnosticSeverity Severity { get; protected set; }
        public XError(string path, LinePositionSpan span , 
            string errCode, string message, object[] @params)
        {
            Path = path;
            Span = span;
            ErrCode = errCode;
            Message = message;
            Params = @params;
            Severity = DiagnosticSeverity.Error;
        }
        public override string ToString()
        {
            return String.Format(Message, Params);
        }
    }
}
