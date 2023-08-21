//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Functions provided by Antonio Lopes
// make sure this is compiled early bound !
#pragma options ("lb", off)
#translate BETWEEN(<n1>, <n2>, <n3>)  => (<n1> >= <n2> .and. <n1> <= <n3>)
#define RANGE_ERROR "Value must be between 0 and 31"
#define TYPE_ERROR  "Value must be LONG or BINARY"
#define TYPE_ERROR2  "Value must be LONG"

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitand/*" />
FUNCTION BitAnd (Arg1 AS USUAL, Arg2 PARAMS USUAL[]) AS USUAL
    RETURN BitHelpers._BITANDORX(BitOperation.And, Arg1, Arg2)
END FUNC
/// <include file="VFPDocs.xml" path="Runtimefunctions/bitand/*" />
FUNCTION BitAnd (Arg1 AS INT, Arg2 PARAMS INT[]) AS INT
    RETURN BitHelpers._BITANDORX(BitOperation.And, Arg1, Arg2)
END FUNC
/// <include file="VFPDocs.xml" path="Runtimefunctions/bitand/*" />
FUNCTION BitAnd(Arg1 AS BINARY, Arg2 PARAMS BINARY[]) AS BINARY
    RETURN BitHelpers._BITANDORX(BitOperation.And, Arg1, Arg2)
END FUNC


/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (Arg1 AS USUAL) AS BINARY
    RETURN BitClear((BINARY)Arg1)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (Arg1 AS USUAL, Bit AS USUAL) AS USUAL
    IF ! IsNumeric(Bit)
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bit),TYPE_ERROR, 2, {Bit})
    ENDIF
    IF IsBinary(Arg1)
        VAR binValue := (BINARY) Arg1
        RETURN IIF(binValue.Length == 0, 0h, BitClear(binValue, (INT)Bit, 1))
    ELSEIF IsNumeric(Arg1)
        RETURN BitClear((INT)Arg1, (INT)Bit)
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Arg1),TYPE_ERROR, 1, {Arg1})
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (Arg1 AS USUAL, Arg2 AS USUAL, Arg3 AS USUAL) AS BINARY

    RETURN BitClear((BINARY)Arg1, (INT)Arg2, (INT)Arg3)

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (Num AS INT, Bit AS INT) AS INT

    IF BETWEEN(Bit, 0, 31)
        RETURN _AND(Num, _NOT(1 << Bit))
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bit),RANGE_ERROR, 2,{Bit})
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (BinString AS BINARY) AS BINARY

    RETURN IIF(BinString.Length == 0, 0h, BitClear(BinString, 0, BinString.Length * 8))

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (BinString AS BINARY, StartBit AS INT) AS BINARY
    RETURN IIF(BinString.Length == 0, 0h, BitClear(BinString, StartBit, 1))
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitclear/*" />
FUNCTION BitClear (BinString AS BINARY, StartBit AS INT, BitCount AS INT) AS BINARY

    LOCAL Result := BinString AS BYTE[]
    LOCAL ByteIndex AS INT
    LOCAL BitIndex := StartBit AS INT
    LOCAL BitCounter AS INT

    FOR BitCounter := 1 TO BitCount

        ByteIndex := BitIndex / 8 + 1
        IF BETWEEN(ByteIndex, 1, Result.Length)
            Result[ByteIndex] := (BYTE) _AND(Result[ByteIndex],_NOT(1 << BitIndex % 8))
            BitIndex++
        ELSE
            THROW Error.ArgumentError(__FUNCTION__, nameof(StartBit),2)
        ENDIF

    NEXT

    RETURN (BINARY)Result

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitlshift/*" />
FUNCTION BitLShift (Arg AS INT, Bits AS INT) AS INT
    IF BETWEEN(Bits, 0, 31)
        RETURN Arg << Bits
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bits),RANGE_ERROR, 2,{Bits})
    ENDIF

END FUNC
/// <include file="VFPDocs.xml" path="Runtimefunctions/bitlshift/*" />
FUNCTION BitLShift (Arg AS USUAL, Bits AS USUAL) AS INT
    RETURN BitLShift((INT)Arg, (INT)Bits)
END FUNC



/// <include file="VFPDocs.xml" path="Runtimefunctions/bitnot/*" />
FUNCTION BitNot (Arg1 AS USUAL) AS USUAL
    IF IsBinary(Arg1)
        RETURN BitNot((BINARY)Arg1)
    ELSEIF IsNumeric(Arg1)
        RETURN BitNot((INT)Arg1)
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Arg1),TYPE_ERROR, 1, {Arg1})
    ENDIF
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitnot/*" />
FUNCTION BitNot (nNumericExpression AS INT) AS INT
    RETURN ~nNumericExpression
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitnot/*" />
FUNCTION BitNot (BinString AS BINARY) AS BINARY
    RETURN BitNot(BinString, 0, BinString.Length * 8)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitnot/*" />
FUNCTION BitNot (BinString AS BINARY, StartBit AS INT, BitCount := 1 AS INT) AS BINARY
    LOCAL Result := 0h + BinString AS BYTE[]
    LOCAL ByteIndex AS INT
    LOCAL BitIndex := StartBit AS INT
    LOCAL BitCounter AS INT

    FOR BitCounter := 1 TO BitCount

        ByteIndex := BitIndex / 8 + 1
        IF BETWEEN(ByteIndex, 1, Result.Length)
            Result[ByteIndex] := (BYTE) _XOR(Result[ByteIndex], 1 << BitIndex % 8)
            BitIndex++
        ELSE
            THROW Error.ArgumentError(__FUNCTION__, nameof(StartBit),2)
        ENDIF

    NEXT
    RETURN (BINARY)Result
END FUNC


/// <include file="VFPDocs.xml" path="Runtimefunctions/bitor/*" />
FUNCTION BitOr (Arg1 AS USUAL, Arg2 PARAMS USUAL[]) AS USUAL
    RETURN BitHelpers._BITANDORX(BitOperation.Or, Arg1, Arg2)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitor/*" />
FUNCTION BitOr (Arg1 AS INT, Arg2 PARAMS INT[]) AS INT
    RETURN BitHelpers._BITANDORX(BitOperation.Or, Arg1, Arg2)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitor/*" />
FUNCTION BitOr (Arg1 AS BINARY, Arg2 PARAMS BINARY[]) AS BINARY
    RETURN BitHelpers._BITANDORX(BitOperation.Or, Arg1, Arg2)
END FUNC





/// <include file="VFPDocs.xml" path="Runtimefunctions/bitrshift/*" />
FUNCTION BitRShift (Arg AS USUAL, Bits AS USUAL) AS INT
   IF IsNumeric(Arg) .AND. IsNumeric(Bits)
      RETURN BitRShift((INT)Arg, (INT)Bits)
   ENDIF
   IF ! IsNumeric(Arg)
     THROW Error.ArgumentError(__FUNCTION__, nameof(Arg),TYPE_ERROR2, 1, {Arg})
   ENDIF
   THROW Error.ArgumentError(__FUNCTION__, nameof(Bits),TYPE_ERROR2, 2, {Bits})

END FUNC


/// <include file="VFPDocs.xml" path="Runtimefunctions/bitrshift/*" />
INTERNAL FUNCTION BitRShift (Arg AS INT, Bits AS INT) AS INT
    IF BETWEEN(Bits, 0, 31)
        IF !BitTest(Arg, 31) .OR. Bits == 0
            RETURN Arg >> Bits
        ELSE
            RETURN BitSet(BitClear(Arg, 31) >> Bits, 31 - Bits)
        ENDIF
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bits),RANGE_ERROR, 2,{Bits})
    ENDIF

END FUNC


/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (Arg1 AS USUAL) AS BINARY
    RETURN BitSet((BINARY)Arg1)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (Arg1 AS USUAL, Bit AS USUAL) AS USUAL

    IF IsBinary(Arg1)
        RETURN BitSet((BINARY)Arg1, (INT)Bit, 1)
    ELSE
        RETURN BitSet((INT)Arg1, (INT)Bit)
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (BinString AS USUAL, StartBit AS USUAL, BitCount AS USUAL) AS BINARY
    RETURN BitSet((BINARY)BinString, (INT)StartBit, (INT)BitCount)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (Num AS INT, Bit AS INT) AS INT
    IF BETWEEN(Bit, 0, 31)
        RETURN _OR(Num, 1 << Bit)
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bit),RANGE_ERROR, 2,{Bit})
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (BinString AS BINARY) AS BINARY

    RETURN IIF(BinString.Length == 0, 0h, BitSet(BinString, 0, BinString.Length * 8))

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (BinString AS BINARY, StartBit AS INT) AS BINARY

    RETURN IIF(BinString.Length == 0, 0h, BitSet(BinString, StartBit, 1))

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitset/*" />
FUNCTION BitSet (BinString AS BINARY, StartBit AS INT, BitCount AS INT) AS BINARY

    LOCAL Result := 0h + BinString AS BYTE[]
    LOCAL ByteIndex AS INT
    LOCAL BitIndex := StartBit AS INT
    LOCAL BitCounter AS INT

    FOR BitCounter := 1 TO BitCount

        ByteIndex := BitIndex / 8 + 1
        IF BETWEEN(ByteIndex, 1, Result.Length)
            Result[ByteIndex] := (BYTE) _OR(Result[ByteIndex], 1 << BitIndex % 8)
            BitIndex++
        ELSE
            THROW Error.ArgumentError(__FUNCTION__, nameof(StartBit),2)
        ENDIF

    NEXT

    RETURN (BINARY)Result

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bittest/*" />
FUNCTION BitTest (Arg AS USUAL, Bit AS USUAL) AS LOGIC
    IF IsBinary(Arg)
      RETURN BitTest((BINARY)Arg, (INT)Bit)
    ELSEIF IsLong(Arg)
        RETURN BitTest((INT)Arg, (INT)Bit)
    ELSE
         THROW Error.ArgumentError(__FUNCTION__, nameof(Arg),TYPE_ERROR, 1, {Arg})
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bittest/*" />
FUNCTION BitTest (Arg AS INT, Bit AS INT) AS LOGIC

    IF BETWEEN(Bit, 0, 31)
        RETURN _AND(Arg, 1 << Bit) != 0
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(Bit),RANGE_ERROR,2,{Bit})
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bittest/*" />
FUNCTION BitTest (BinString AS BINARY, BitNumber AS INT) AS LOGIC

    LOCAL Buff := BinString AS BYTE[]

    IF BETWEEN(BitNumber, 0, Buff.Length * 8 - 1)
        RETURN _AND(Buff[BitNumber / 8 + 1], 1 << BitNumber % 8) != 0
    ELSE
        THROW Error.ArgumentError(__FUNCTION__, nameof(BitNumber),2)
    ENDIF

END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitxor/*" />
FUNCTION BitXor (Arg1 AS USUAL, Arg2 PARAMS USUAL[]) AS USUAL
    RETURN BitHelpers._BITANDORX(BitOperation.Xor, Arg1, Arg2)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitxor/*" />
FUNCTION BitXor (Arg1 AS INT, Arg2 PARAMS INT[]) AS INT
    RETURN BitHelpers._BITANDORX(BitOperation.Xor, Arg1, Arg2)
END FUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/bitxor/*" />
FUNCTION BitXor (Arg1 AS BINARY, Arg2 PARAMS BINARY[]) AS BINARY
    RETURN BitHelpers._BITANDORX(BitOperation.Xor, Arg1, Arg2)
END FUNC


INTERNAL STATIC CLASS BitHelpers
STATIC METHOD _BITANDORX (LogicalOp AS BitOperation, Arg1 AS USUAL, Arg2 PARAMS USUAL[]) AS USUAL

    IF IsBinary(Arg1)
        VAR Args = BINARY[]{Arg2.Length}
        LOCAL ArgIndex AS INT

        FOR ArgIndex := 1 TO Args.Length
            Args[ArgIndex] := (BINARY)Arg2[ArgIndex]
        NEXT

        RETURN _BITANDORX(LogicalOp, (BINARY)Arg1, Args)
    ELSEIF IsLong(Arg1)
        VAR Args = INT[]{Arg2.Length}
        LOCAL ArgIndex AS INT

        FOR ArgIndex := 1 TO Args.Length
            Args[ArgIndex] := (INT)Arg2[ArgIndex]
        NEXT

        RETURN _BITANDORX(LogicalOp, (INT)Arg1, Args)
    ELSE
       THROW Error.ArgumentError(__FUNCTION__, nameof(Arg1),TYPE_ERROR,1, {Arg1})
    ENDIF
END METHOD

STATIC METHOD _BITANDORX (LogicalOp AS BitOperation, Arg1 AS INT, Arg2 PARAMS INT[]) AS INT

    LOCAL Result := Arg1 AS INT
    LOCAL ArgIndex AS INT

    SWITCH LogicalOp
        CASE BitOperation.And
            FOR ArgIndex := 1 TO Arg2.Length
                Result := _AND(Result, Arg2[ArgIndex])
            NEXT
        CASE BitOperation.Or
            FOR ArgIndex := 1 TO Arg2.Length
                Result := _OR(Result, Arg2[ArgIndex])
            NEXT
        CASE BitOperation.Xor
            FOR ArgIndex := 1 TO Arg2.Length
                Result := _XOR(Result, Arg2[ArgIndex])
            NEXT
        END

    RETURN Result

END METHOD

STATIC METHOD _BITANDORX (LogicalOp AS BitOperation, Arg1 AS BINARY, Arg2 PARAMS BINARY[]) AS BINARY

    LOCAL Result := 0h + Arg1 AS BYTE[]
    LOCAL ArgIndex AS INT

    FOR ArgIndex := 1 TO Arg2.Length
        LOCAL Arg := 0h + Arg2[ArgIndex] AS BYTE[]
        IF Result.Length < Arg.Length
            Array.Resize(REF Result, Arg.Length)
        ELSE
            IF Result.Length > Arg.Length
                Array.Resize(REF Arg, Result.Length)
            ENDIF
        ENDIF
        LOCAL ByteIndex AS INT
        SWITCH LogicalOp
            CASE BitOperation.And
                FOR ByteIndex := 1 TO Result.Length
                    Result[ByteIndex] := _AND(Result[ByteIndex], Arg[ByteIndex])
                NEXT
            CASE BitOperation.Or
                FOR ByteIndex := 1 TO Result.Length
                    Result[ByteIndex] := _OR(Result[ByteIndex], Arg[ByteIndex])
                NEXT
            CASE BitOperation.Xor
                FOR ByteIndex := 1 TO Result.Length
                    Result[ByteIndex] := _XOR(Result[ByteIndex], Arg[ByteIndex])
                NEXT
            END

    ENDFOR

    RETURN (BINARY)Result

END METHOD

END CLASS

INTERNAL ENUM BitOperation
   MEMBER And
   MEMBER Or
   MEMBER Xor
END ENUM

