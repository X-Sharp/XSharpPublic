//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX


INTERNAL ENUM FlexArrayTypes
    MEMBER NIL      := 0
    MEMBER UChar    := 1       // 1 byte
    MEMBER Char     := 2       // 1 byte
    MEMBER Short    := 3       // 2 bytes
    MEMBER UShort   := 4      // 2 bytes
    MEMBER Long     := 5      // 4 bytes
    MEMBER String32 := 6      // 4 bytes length followed by string
    MEMBER String16 := 7      // 2 bytes length followed by string
    MEMBER Float    := 8      // 10 bytes 
    MEMBER Double   := 9      // 8 bytes
    MEMBER Date     := 10     // 4 bytes
    MEMBER Logic    := 11     // 1 byte
    MEMBER Array    := 12     // 2 bytes length followed by array
    MEMBER CodeBlock:= 13     // ?
    MEMBER DateJ    := 14     // 4 bytes
    MEMBER Double2  := 15       // len, dec, 8 byte double
    MEMBER Cyclic   := 16       // Cyclic array, stored how ?
    MEMBER UCHar1   := 17       // byte, dec
    MEMBER Char1    := 18       // char, dec
    MEMBER Short1   := 19       // 2, followed by len
    MEMBER UShort1  := 20       // 2, followed by len
    MEMBER Long1    := 21       // 4, followed by len
    MEMBER Unused   := 22       // no data
    MEMBER Object   := 23       // ?
    MEMBER Null     := 24       // no data
    MEMBER True     := 25
    MEMBER False    := 26
    MEMBER LDouble  := 27       
    MEMBER UCHar2   := 28       // byte[1], len, dec 
    MEMBER CHar2    := 29       // byte[1], len, dec 
    MEMBER Short2   := 30       // short[2], len, dec 
    MEMBER UShort2  := 31       // ushort[2], len, dec 
    MEMBER Long2    := 32       // long[4], len, dec
    MEMBER ULong2   := 33       // ulong[4], len, dec
END ENUM

INTERNAL ENUM FlexFieldType
    MEMBER Picture      := 0
    MEMBER String       := 1
    MEMBER OleObject    := 2
    MEMBER IndexBlock   := 1000
    MEMBER Delete       := 1001
    MEMBER Array16      := 1002
    MEMBER Object16     := 1003
    MEMBER Array32      := 1004
    MEMBER Object32     := 1005
    MEMBER Nil          := 1006
    MEMBER LogicTrue    := 1007
    MEMBER LogicFalse   := 1008
    MEMBER JDate        := 1009
    MEMBER SByte        := 1010
    MEMBER Byte         := 1011
    MEMBER Short        := 1012
    MEMBER Word         := 1013
    MEMBER Long         := 1014
    MEMBER Dword        := 1015
    MEMBER Double       := 1016
    MEMBER Double10     := 1017
    MEMBER Compressed   := 1018
    MEMBER StringLong   := 1019
    MEMBER CompressedLong := 1020
    MEMBER ItemClipper  := 10000
    MEMBER LogicLong    := 10001
    MEMBER StringEmpty  := 10002
    MEMBER Illegal      := -1
END ENUM
