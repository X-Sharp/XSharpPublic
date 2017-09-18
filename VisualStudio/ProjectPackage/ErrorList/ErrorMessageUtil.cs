//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using Microsoft.VisualStudio.Shell.Interop;


namespace XSharp.Project
{
    public enum MessageSeverity
    {
        Info = 0,
        Warning = 1,
        Error = 2,
        NoProblems = 3
    }

    internal static class ErrorMessageUtil
    {
        static public __VSERRORCATEGORY ToVSERRORCATEGORY(this MessageSeverity severity)
        {
            if (severity == MessageSeverity.Warning)
            {
                return __VSERRORCATEGORY.EC_WARNING;
            }
            else if (severity == MessageSeverity.Error)
            {
                return __VSERRORCATEGORY.EC_ERROR;
            }
            return __VSERRORCATEGORY.EC_MESSAGE;
        }

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
