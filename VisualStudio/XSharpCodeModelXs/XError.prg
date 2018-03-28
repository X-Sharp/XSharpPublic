using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.Text

BEGIN NAMESPACE XSharpModel
    CLASS XError
        // Fields

        // Methods
         CONSTRUCTOR(path AS string, span AS LinePositionSpan, errCode AS string, message AS string, params AS Object[]);SUPER()
            //
            SELF:Path := path
            SELF:Span := span
            SELF:ErrCode := errCode
            SELF:Message := message
            SELF:Params := params
            SELF:Severity := DiagnosticSeverity.Error

        VIRTUAL METHOD ToString() AS string
            //
            RETURN String.Format(SELF:Message, SELF:Params)


        // Properties
        PROPERTY ErrCode AS string AUTO 

        PROPERTY Message AS string AUTO 

        PROPERTY Params AS Object[] AUTO 

        PROPERTY Path AS string AUTO 

        PROPERTY Severity AS DiagnosticSeverity AUTO 

        PROPERTY Span AS LinePositionSpan AUTO 


    END CLASS

END NAMESPACE 

