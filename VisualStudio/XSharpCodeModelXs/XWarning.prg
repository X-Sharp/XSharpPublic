using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.Text
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks

USING XSharpModel
BEGIN NAMESPACE XSharpModel
    CLASS XWarning INHERIT XError
        // Methods
         CONSTRUCTOR(path AS string, span AS LinePositionSpan, errCode AS string, message AS string, args AS Object[])
        SUPER(path, span, errCode, message, args)
            //
            SUPER:Severity := DiagnosticSeverity.Warning


    END CLASS

END NAMESPACE 

