using LanguageService.SyntaxTree
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using System.Diagnostics
using System
using System.Runtime.InteropServices
BEGIN NAMESPACE XSharpModel
    [StructLayout(LayoutKind.Sequential), DebuggerDisplay("{Start}-{Stop}")];
    STRUCTURE TextInterval
        // Fields
        INITONLY PRIVATE _StartIndex AS Long
        INITONLY PRIVATE _StopIndex AS Long

        // Methods
         //CONSTRUCTOR(context AS ParserRuleContext)
			//SELF(context:Start:StartIndex, context:Stop:StopIndex)


         CONSTRUCTOR(start AS Long, stop AS Long)
            //
            SELF:_StartIndex := start
            SELF:_StopIndex := stop

        METHOD ContainsExclusive(position AS Long) AS Logic
            //
            RETURN ((position > SELF:_StartIndex) .AND. (position < SELF:_StopIndex))

        METHOD ContainsInclusive(position AS Long) AS Logic
            //
            RETURN ((position >= SELF:_StartIndex) .AND. (position <= SELF:_StopIndex))

        METHOD IsEmpty() AS Logic
            //
            RETURN ((SELF:_StartIndex == 0) .AND. (SELF:_StopIndex == 0))


        // Properties
        STATIC PROPERTY Empty AS TextInterval
            GET
                //
                RETURN TextInterval{}
            END GET
        END PROPERTY

        PROPERTY Start AS Long
            GET
                //
                RETURN SELF:_StartIndex
            END GET
        END PROPERTY

        PROPERTY Stop AS Long
            GET
                //
                RETURN SELF:_StopIndex
            END GET
        END PROPERTY

        PROPERTY Width AS Long
            GET
                //
                RETURN ((SELF:_StopIndex - SELF:_StartIndex) + 1)
            END GET
        END PROPERTY


    END STRUCTURE

END NAMESPACE 

