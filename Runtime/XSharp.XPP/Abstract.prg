//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
CLASS XSharp.XPP.Abstract
    PRIVATE inSend := FALSE as LOGIC

    METHOD eval(bBlock, uPar1, uPar2, uPar3) as USUAL CLIPPER
         LOCAL aParams as USUAL[]
         IF pCount() > 0
             aParams := USUAL[]{ PCOunt()-1 }
             FOR VAR nX := 2 to PCount()
                aParams[nX-1] := _GetFParam(nX)
             NEXT
             RETURN XSharp.RT.Functions.Eval(bBlock, aParams)
         ELSE
            THROW ArgumentException{"Missing Codeblock parameter", nameof(bBlock)}
         ENDIF

    METHOD setNoIVar(cName , uValue ) AS USUAL CLIPPER
        RETURN IVarPut(SELF, cName, uValue)

    METHOD getNoIVar(cName ) AS USUAL CLIPPER
        RETURN IVarGet(SELF, cName)

    METHOD NoMethod(cName, uParams) AS USUAL CLIPPER
        LOCAL aParams as USUAL[]
        IF ! SELF:inSend
            SELF:inSend := TRUE
            TRY
                aParams := USUAL[]{ PCOunt()-1 }
                FOR VAR nX := 2 to PCount()
                    aParams[nX-1] := _GetFParam(nX)
                NEXT
                RETURN __InternalSend(SELF, cName, aParams)
            FINALLY
                SELF:inSend := FALSE
            END TRY
        ELSE
            THROW Error.VOError( EG_NOMETHOD, __ENTITY__, cName, 1, <OBJECT>{cName} )
        ENDIF
    METHOD Notify(nEvent, nNotification) AS USUAL CLIPPER
        RETURN SELF

END CLASS
