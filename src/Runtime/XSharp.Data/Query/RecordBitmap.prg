//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.Data

PUBLIC CLASS RecordBitmap
    PUBLIC ENUM RecordMatchState
        MEMBER NoMatch := 0  // Record does not match the condition
        MEMBER Match := 1    // Record matches the condition
    END ENUM

    PRIVATE CONST ALLOC_QUANTUM := 4096 AS LONG

    PRIVATE _bits AS BitArray

    PUBLIC PROPERTY Items[recNo AS LONG] AS RecordMatchState GET SELF:GetMatchState(recNo) SET SELF:SetMatchState(recNo, VALUE)
    PUBLIC PROPERTY Length AS LONG GET _bits:Length SET SELF:SetLength(VALUE)

    PUBLIC CONSTRUCTOR()
        SELF:_bits := BitArray{0}

    PUBLIC CONSTRUCTOR(o AS RecordBitmap)
        SELF:_bits := BitArray{o:_bits}
        SELF:Length := o:Length

    PUBLIC METHOD GetMatchState(recNo AS LONG) AS RecordMatchState
        IF recNo < 1 .OR. recNo > Length
            RETURN RecordMatchState.NoMatch
        END IF
        RETURN IIF(_bits[recNo-1], RecordMatchState.Match, RecordMatchState.NoMatch)

    PUBLIC METHOD SetMatchState(recNo AS LONG, state AS RecordMatchState) AS VOID
        IF recNo < 1
            RETURN
        END IF
        IF recNo > Length
            Length := (recNo + ALLOC_QUANTUM - 1) & ~(ALLOC_QUANTUM - 1)
        END IF
        _bits[recNo-1] := (state == RecordMatchState.Match)
        RETURN

    PUBLIC METHOD SetLength(length AS LONG) AS VOID
        _bits:Length := length
        RETURN

    PUBLIC METHOD Clear() AS VOID
        _bits:SetAll(FALSE)
        RETURN

    PUBLIC METHOD Fill() AS VOID
        _bits:SetAll(TRUE)
        RETURN

    PUBLIC METHOD And(o AS RecordBitmap) AS VOID
        IF o == NULL
            RETURN
        ENDIF

        // Ensure both arrays have the same length for operation
        LOCAL otherBits AS BitArray
        otherBits := BitArray{o:_bits}  // Clone the other to avoid modifying it

        // Extend both arrays to the same length if needed
        LOCAL maxLength AS LONG
        maxLength := Math.Max(_bits:Length, o:_bits:Length)
        IF _bits:Length < maxLength
            _bits:Length := maxLength
        ENDIF
        IF otherBits:Length < maxLength
            otherBits:Length := maxLength
        ENDIF

        // Perform the AND operation (modifies this instance's bits in place)
        _bits:And(otherBits)
    END METHOD

    PUBLIC METHOD Or(o AS RecordBitmap) AS VOID
        IF o == NULL
            RETURN
        ENDIF

        // Ensure both arrays have the same length for operation
        LOCAL otherBits AS BitArray
        otherBits := BitArray{o:_bits}  // Clone the other to avoid modifying it

        // Extend both arrays to the same length if needed
        LOCAL maxLength AS LONG
        maxLength := Math.Max(_bits:Length, o:_bits:Length)
        IF _bits:Length < maxLength
            _bits:Length := maxLength
        ENDIF
        IF otherBits:Length < maxLength
            otherBits:Length := maxLength
        ENDIF

        // Perform the OR operation (modifies this instance's bits in place)
        _bits:Or(otherBits)
    END METHOD

    PUBLIC METHOD Not() AS VOID
        // Perform the NOT operation (modifies this instance's bits in place)
        _bits:Not()
    END METHOD

PUBLIC METHOD Clone() AS RecordBitmap
        RETURN RecordBitmap{SELF}
    END METHOD

    PUBLIC METHOD IsAllMatch() AS LOGIC
        LOCAL allSet := TRUE AS LOGIC
        FOR LOCAL i := 0 AS LONG TO _bits:Length - 1
            IF !_bits[i]
                allSet := FALSE
                EXIT
            ENDIF
        NEXT
        RETURN allSet

    PUBLIC METHOD CountMatches() AS LONG
        LOCAL count := 0 AS LONG
        FOR LOCAL i := 0 AS LONG TO _bits:Length - 1
            IF _bits[i]
                count++
            ENDIF
        NEXT
        RETURN count

END CLASS

END NAMESPACE // XSharp.Data
