//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Shell.Interop;


namespace XSharp.Project
{
    public enum MessageSeverity
    {
        Info = __VSERRORCATEGORY.EC_MESSAGE,
        Warning = __VSERRORCATEGORY.EC_WARNING,
        Error = __VSERRORCATEGORY.EC_ERROR,
        NoProblems = 3
    }

    internal static class ErrorMessageUtil
    {
        
        static public MessageSeverity ToMessageSeverity(this DiagnosticSeverity sev)
        {
            switch (sev)
            {
                case DiagnosticSeverity.Error:
                    return MessageSeverity.Error;
                case DiagnosticSeverity.Warning:
                    return MessageSeverity.Warning;
                case DiagnosticSeverity.Info:
                    return MessageSeverity.Info;
                default:
                    return MessageSeverity.NoProblems;                
            }
        }

    }


}
