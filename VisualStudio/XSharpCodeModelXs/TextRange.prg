using LanguageService.SyntaxTree
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using System.Diagnostics
using System
using System.Runtime.InteropServices
USING System
BEGIN NAMESPACE XSharpModel
    [StructLayout(LayoutKind.Sequential), DebuggerDisplay("{StartLine}.{StartColumn}-{EndLine}.{EndColumn}")];
    STRUCTURE TextRange
        // Fields
        INITONLY PRIVATE _EndColumn AS Long
        INITONLY PRIVATE _EndLine AS Long
        INITONLY PRIVATE _StartColumn AS Long
        INITONLY PRIVATE _StartLine AS Long

        // Methods
         //CONSTRUCTOR(context AS ParserRuleContext)
        //SELF(context:Start:Line, context:Start:Column, context:Stop:Line, context:Stop:Column)
//

         CONSTRUCTOR(sl AS Long, sc AS Long, el AS Long, ec AS Long)
            //
            SELF:_StartLine := sl
            SELF:_StartColumn := sc
            SELF:_EndLine := el
            SELF:_EndColumn := ec

        METHOD ContainsExclusive(line AS Long, col AS Long) AS Logic
            //
            IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
                //
                RETURN TRUE
            ENDIF
            IF (line == SELF:_StartLine)
                //
                IF (col > SELF:_StartColumn)
                    //
                    IF (line < SELF:_EndLine)
                        //
                        RETURN TRUE
                    ENDIF
                    IF (line == SELF:_EndLine)
                        //
                        RETURN (col < SELF:_EndColumn)
                    ENDIF
                ENDIF
                RETURN FALSE
            ENDIF
            RETURN ((line == SELF:_EndLine) .AND. (col < SELF:_EndColumn))

        METHOD ContainsInclusive(line AS Long, col AS Long) AS Logic
            //
            IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
                //
                RETURN TRUE
            ENDIF
            IF (line == SELF:_StartLine)
                //
                IF (col >= SELF:_StartColumn)
                    //
                    IF (line < SELF:_EndLine)
                        //
                        RETURN TRUE
                    ENDIF
                    IF (line == SELF:_EndLine)
                        //
                        RETURN (col <= SELF:_EndColumn)
                    ENDIF
                ENDIF
                RETURN FALSE
            ENDIF
            RETURN ((line == SELF:_EndLine) .AND. (col <= SELF:_EndColumn))


        // Properties
        STATIC PROPERTY Empty AS TextRange
            GET
                //
                RETURN TextRange{1, 1, 1, 1}
            END GET
        END PROPERTY

        PROPERTY EndColumn AS Long
            GET
                //
                RETURN SELF:_EndColumn
            END GET
        END PROPERTY

        PROPERTY EndLine AS Long
            GET
                //
                RETURN SELF:_EndLine
            END GET
        END PROPERTY

        PROPERTY StartColumn AS Long
            GET
                //
                RETURN SELF:_StartColumn
            END GET
        END PROPERTY

        PROPERTY StartLine AS Long
            GET
                //
                RETURN SELF:_StartLine
            END GET
        END PROPERTY


    END STRUCTURE

END NAMESPACE 

