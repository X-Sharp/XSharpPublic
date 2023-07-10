//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Text


/// <summary>
/// Copy public and private memory variables visible within the current routine to a disk file.
/// </summary>
/// <param name="cFileName">The name of the file, including an optional drive, directory, and extension. The default extension is .MEM </param>
/// <param name="cSkel">Wildcard pattern to use when saving. This can include literal characters as well as the standard wildcard characters, * and ?. </param>
/// <param name="lLike">When set to TRUE then variables matching the pattern are saved. When set to FALSE then the variables that do NOT match the pattern are saved.</param>
/// <remarks>
/// An existing MEM file will be overwritten. When an existing file is ReadOnly then an exception will be thrown.
/// </remarks>
FUNCTION _MSave(cFileName AS STRING, cSkel AS STRING, lLike AS LOGIC) AS VOID
    LOCAL lAll AS LOGIC
    LOCAL oMemWriter AS MemWriter
    IF String.IsNullOrEmpty(System.IO.Path.GetExtension(cFileName))
        cFileName := System.IO.Path.ChangeExtension(cFileName, ".MEM")
    ENDIF
    IF cSkel == "*"
        lAll := TRUE
        IF ! lLike 			// NOT ALL = nothing, so create empty file and exit
            MemoWrit(cFileName,"")
            RETURN
        ENDIF
        ELSE
            lAll := FALSE
        cSkel := Upper(cSkel)
    ENDIF

    oMemWriter := MemWriter{cFileName}
    IF ! oMemWriter:Ok
        // Throw an error
        NOP

        ELSE
            LOCAL cVar AS STRING
            cVar := _PublicFirst()

            DO WHILE !String.IsNullOrEmpty(cVar)
                IF lAll .OR. _Like(cSkel, cVar) == lLike
                    oMemWriter:WriteValue(cVar, MemVarGet(cVar))
                ENDIF
                cVar := _PublicNext()
            ENDDO
            cVar := _PrivateFirst()

            DO WHILE !String.IsNullOrEmpty(cVar)
                IF lAll .OR. _Like(cSkel, cVar) == lLike
                    oMemWriter:WriteValue(cVar, MemVarGet(cVar))
                ENDIF
                cVar := _PrivateNext()
            ENDDO
        oMemWriter:Close()
    ENDIF
    RETURN

/// <summary>
/// Recreate public and private variables previously saved to a file and initialize them with their former values.
/// </summary>
/// <param name="cFileName">The name of the memory file to read. The default extension is .MEM </param>
/// <param name="lAdditive">When set to TRUE then the existing memory variables will be saved. Otherwise all memory variables will be deleted first.</param>
/// <param name="cSkel">(Optional) Wildcard pattern to use when saving. This can include literal characters as well as the standard wildcard characters, * and ?. </param>
/// <param name="lLike">(Optional) When set to TRUE then variables matching the pattern are restored. When set to FALSE then the variables that do NOT match the pattern are restored.</param>
/// <remarks>
/// When memory variables are restored, they are recreated as private variables with the scope of the current procedure or function
/// unless they exist as public variables and you specify the ADDITIVE clause .  If ADDITIVE is specified, public and private variables
/// with the same names are overwritten unless hidden with PRIVATE.  If ADDITIVE is not specified, all public and private variables are
/// released before the memory file is loaded. In that case all restored variables will become private variables.
/// </remarks>
FUNCTION _MRestore(cFileName AS STRING, lAdditive AS LOGIC, cSkel := NULL AS STRING, lInclude := TRUE AS LOGIC ) AS VOID
    LOCAL oMemReader AS MemReader
    IF .NOT. lAdditive
        _MClear()
    ENDIF
    IF String.IsNullOrEmpty(System.IO.Path.GetExtension(cFileName))
        cFileName := System.IO.Path.ChangeExtension(cFileName, ".MEM")
    ENDIF
    oMemReader := MemReader{cFileName}
    oMemReader:ReadFile(cSkel, lInclude)
    oMemReader:Close()
    RETURN

INTERNAL CLASS MemWriter
    PRIVATE hFile       AS IntPtr
    PRIVATE aArrayList  AS ARRAY	// to keep track of recursive arrays
    PRIVATE aObjectList AS ARRAY    // to keep track of recursive objects
    PRIVATE cFileName   AS STRING
    PROPERTY Ok         AS LOGIC GET hFile != F_ERROR

    CONSTRUCTOR (cFile AS STRING)
        cFileName := cFile
        hFile := FCreate(cFile)
        IF hFile == F_ERROR
            SELF:_Error()
        ENDIF
        aArrayList  := {}
        aObjectList := {}
        RETURN


    DESTRUCTOR
        SELF:Close()

    METHOD Close() AS VOID
        IF hFile != F_ERROR
            SELF:WriteByte(ASC_EOF)
            FClose(hFile)
            hFile := F_ERROR
            UnRegisterAxit(SELF)
        ENDIF
        RETURN

    METHOD _Error() AS VOID
        LOCAL oErr AS Error
        oErr := Error{FException()}
        oErr:Gencode := EG_READ
        oErr:SubCode := XSharp.VOErrors.E_OPEN_MSAVE
        oErr:FuncSym := "_MSAVE"
        oErr:FileName := cFileName
        oErr:OSCode   := FError()
        THROW oErr

    METHOD WriteStr(cVar AS STRING) AS VOID
        SELF:WriteBytes(W2Bin(WORD(SLen(cVar))))
        SELF:WriteBytes(cVar)
        RETURN

    METHOD WriteBytes(cVar AS STRING) AS VOID   PASCAL
        LOCAL nLen AS DWORD
        LOCAL nWritten AS DWORD
        LOCAL aBytes AS BYTE[]
        nLen   := SLen(cVar)
        aBytes := BYTE[] { nLen}
        FOR VAR nI := 1 TO nLen
            aBytes[nI] := (BYTE) cVar[nI-1]
        NEXT
        nWritten := FWrite3(hFile, aBytes, nLen)
        IF nWritten != nLen
            SELF:_Error()
        ENDIF


    METHOD WriteByte(nByte AS BYTE) AS VOID   PASCAL
        LOCAL nWritten AS DWORD
        nWritten := FWrite3(hFile, @nByte, 1)
        IF nWritten != 1
            SELF:_Error()
        ENDIF
        RETURN

    METHOD WriteValue(cVar AS STRING, uVal AS USUAL) AS VOID
        LOCAL wType 	AS DWORD
        LOCAL cVarName 	AS STRING
        LOCAL wArrayEl 	AS DWORD
        LOCAL WriteName AS Action
        wType    := UsualType(uVal)
        IF !String.IsNullOrEmpty(cVar)
            cVarName := cVar
            WriteName := { =>SELF:WriteStr(cVarName) }
        ELSE
            WriteName := { =>  }
        ENDIF

        SWITCH wType
            CASE __UsualType.String
                SELF:WriteByte(__UsualType.String)
                WriteName()
                SELF:WriteStr(uVal)

            CASE __UsualType.Long
                SELF:WriteByte(__UsualType.Long)
                WriteName()
                SELF:WriteBytes(L2Bin(uVal))

            CASE __UsualType.Date
            CASE __UsualType.DateTime
                SELF:WriteByte(__UsualType.Date)
                WriteName()
                SELF:WriteBytes(Date2Bin(uVal))

            CASE __UsualType.Float
            CASE __UsualType.Decimal
            CASE __UsualType.Currency
                SELF:WriteByte(__UsualType.Float)
                WriteName()
                SELF:WriteBytes(F2Bin(uVal))

            CASE __UsualType.Logic
                SELF:WriteByte(__UsualType.Logic)
                WriteName()
                SELF:WriteByte(IIF(uVal,1,0))

            CASE __UsualType.Symbol
                SELF:WriteByte(__UsualType.Symbol)
                WriteName()
                SELF:WriteStr(Symbol2String(uVal))

            CASE __UsualType.Array
                wArrayEl := AScan(aArrayList, uVal)
                IF wArrayEl =0
                    SELF:WriteByte(__UsualType.Array)
                    WriteName()
                    SELF:WriteArr(uVal)
                    ELSE
                        SELF:WriteByte(__UsualType.Array +0x80)
                        WriteName()
                    SELF:WriteBytes(W2Bin((WORD) wArrayEl))
                ENDIF

            CASE __UsualType.Object
                wArrayEl := AScan(aObjectList, uVal)
                IF wArrayEl =0
                    SELF:WriteByte(__UsualType.Object)
                    WriteName()
                    SELF:WriteObj(uVal)
                ELSE
                    SELF:WriteByte(__UsualType.Object +0x80)
                    WriteName()
                    SELF:WriteBytes(W2Bin((WORD) wArrayEl))
                ENDIF

            CASE __UsualType.Ptr
                SELF:WriteByte(__UsualType.Ptr)
                WriteName()
                SELF:WriteBytes(Ptr2Bin(uVal))
                SELF:WriteBytes(W2Bin(WORD(_GetMRandID())))

            OTHERWISE
                SELF:WriteByte(__UsualType.Void)
                WriteName()

        END SWITCH
        RETURN

    METHOD WriteObj  (oVar AS OBJECT) AS VOID
        LOCAL aIVarList AS ARRAY
        LOCAL nLen      AS DWORD
        LOCAL wI        AS DWORD
        LOCAL cIVar 	AS STRING
        IF oVar == NULL_OBJECT
            SELF:WriteStr("")
        ELSE
            SELF:WriteStr(ClassName(oVar))
            AAdd(SELF:aObjectList, oVar)

            aIVarList :=IvarList(oVar)
            nLen :=ALen(aIVarList)
            SELF:WriteBytes(W2Bin(WORD(nLen)))

            FOR wI := 1 UPTO nLen
                cIVar :=aIVarList[wI]
                IF IVarGetInfo(oVar, cIVar) > 0
                    SELF:WriteValue(cIVar, IVarGet(oVar, cIVar))
                ENDIF
            NEXT
        ENDIF
        RETURN

    METHOD WriteArr  (aVar AS ARRAY) AS VOID

        LOCAL wArrayLen 	AS DWORD
        LOCAL wI     	 	AS DWORD
        LOCAL uVal 			AS USUAL

        AAdd(aArrayList, aVar)

        wArrayLen :=ALen(aVar)
        SELF:WriteBytes(W2Bin(WORD(wArrayLen)))

        FOR wI := 1 UPTO wArrayLen

            uVal   :=aVar[wI]
            SELF:WriteValue(NULL_STRING, uVal)
        NEXT

        RETURN

END CLASS

FUNCTION _GetMRandID() AS WORD
    RETURN 0x4242



INTERNAL CLASS MemReader
    PRIVATE hFile AS IntPtr
    PRIVATE cFileName AS STRING
    PRIVATE aArrayList 	AS ARRAY		// to keep track of recursive arrays
    PRIVATE aObjectList AS ARRAY    // to keep track of recursive objects
    PRIVATE aBytes 		AS BYTE[]
    PRIVATE lUsemask	AS LOGIC
    PRIVATE cMask		AS STRING
    PRIVATE lInclude	AS LOGIC

    CONSTRUCTOR (cFile AS STRING)
        cFileName := cFile
        hFile := FOpen(cFile)
        IF hFile == F_ERROR
            SELF:_Error()
        ENDIF
        aArrayList  := {}
        aObjectList := {}
        aBytes      := BYTE[]{256}
        RETURN


    DESTRUCTOR
        SELF:Close()

    METHOD _Error() AS VOID
        LOCAL oErr AS Error
        oErr := Error{FException()}
        oErr:Gencode := EG_READ
        oErr:SubCode := XSharp.VOErrors.E_OPEN_MRESTORE
        oErr:FuncSym := "_MRESTORE"
        oErr:FileName := cFileName
        oErr:OSCode   := FError()
        THROW oErr

    METHOD Close() AS VOID
        IF hFile != F_ERROR
            FClose(hFile)
            hFile := F_ERROR
            UnRegisterAxit(SELF)
        ENDIF
        RETURN

    METHOD ReadBytes(nBytes AS DWORD) AS STRING
        IF nBytes > SELF:aBytes:Length
            SELF:aBytes := BYTE[]{nBytes}
        ENDIF
        IF FRead3(hFile, aBytes, nBytes) != nBytes
            SELF:_Error()
        ENDIF
        LOCAL sb AS StringBuilder
        sb := StringBuilder{}
        FOR VAR nI := 1 TO nBytes
            sb:Append( (CHAR) aBytes[nI])
        NEXT
        RETURN sb:ToString()

    METHOD ReadWord() AS WORD
        RETURN Bin2W(SELF:ReadBytes(2))

    METHOD ReadValue(wType AS DWORD) AS USUAL
        LOCAL uValue AS USUAL
        LOCAL wLen AS WORD
        SWITCH wType
        CASE __UsualType.String
            uValue := SELF:ReadString()

        CASE __UsualType.Long
            uValue :=Bin2L(SELF:ReadBytes(4))

        CASE __UsualType.Date
            uValue :=Bin2Date(SELF:ReadBytes(4))

        CASE __UsualType.Float
            uValue :=Bin2F(SELF:ReadBytes(12))

        CASE __UsualType.Logic
            uValue := SELF:ReadByte() != 0

        CASE __UsualType.Symbol
            uValue := String2Symbol(SELF:ReadString())

        CASE __UsualType.Array
            wLen := SELF:ReadWord()
            uValue := SELF:ReadArray(wLen)

        CASE __UsualType.Array +0x80
            wLen := SELF:ReadWord()
            uValue :=aArrayList[wLen]

        CASE __UsualType.Object
            uValue :=SELF:ReadObject(SELF:ReadString())

        CASE __UsualType.Object +0x80
            wLen := SELF:ReadWord()
            uValue :=aObjectList[wLen]

        CASE __UsualType.Ptr
            uValue :=Bin2Ptr(SELF:ReadBytes(4))
            IF SELF:ReadWord() <>_GetMRandID()
                uValue :=NULL_PTR
            ENDIF

        CASE __UsualType.Void
        OTHERWISE
                uValue :=NIL

        END SWITCH
        RETURN uValue

    METHOD ReadByte () AS BYTE
        IF FRead3(hFile, aBytes, 1) != 1
            SELF:_Error()
        ENDIF
        RETURN aBytes[1]

    METHOD ReadArray(wArrayLen AS DWORD) AS ARRAY
        LOCAL aVar AS ARRAY
        LOCAL wI    AS WORD
        LOCAL wType AS DWORD
        aVar := ArrayCreate(wArrayLen)
        AAdd(aArrayList, aVar)

        FOR wI := 1 UPTO wArrayLen
            wType    := SELF:ReadByte()
            aVar[wI] := SELF:ReadValue(wType)
        NEXT

        RETURN aVar
    METHOD ReadObject   (sObject AS STRING) AS OBJECT

        LOCAL oVar     AS OBJECT
        LOCAL nLen     AS DWORD
        LOCAL wI       AS DWORD
        LOCAL cVar		AS STRING
        LOCAL wType 	AS DWORD
        LOCAL uValue	AS USUAL
        IF String.IsNullOrEmpty(sObject)
            RETURN NULL_OBJECT
        ENDIF
        oVar :=CreateInstance(sObject)

        AAdd(aObjectList, oVar)

        nLen := SELF:ReadWord()

        FOR wI := 1 UPTO nLen

            wType  := SELF:ReadByte()

            cVar 	:= SELF:ReadString()
            uValue 	:= SELF:ReadValue(wType)
            IVarPut(oVar, cVar, uValue)

        NEXT

        RETURN oVar

    METHOD ReadFile(cMask AS STRING, lInclude AS LOGIC) AS VOID
        LOCAL wType AS BYTE
        aArrayList :={}
        aObjectList :={}

        IF cMask != NULL
            SELF:lUsemask := TRUE
            SELF:cMask    := Upper(cMask)
            SELF:lInclude := lInclude
        ENDIF

        wType  := SELF:ReadByte()

        IF wType< 65 // ASC_A //401@TR001
            SELF:ReadVOFile(wType)
        ELSE
            SELF:ReadClipperFile(wType)
        ENDIF

    METHOD ReadString() AS STRING
        LOCAL nLen AS WORD
        nLen := SELF:ReadWord()
        RETURN SELF:ReadBytes(nLen)

    METHOD ReadVOFile(wType AS BYTE) AS VOID
        LOCAL cVar AS STRING
        LOCAL uValue AS USUAL
        LOCAL lPut := TRUE AS LOGIC
        DO WHILE wType <>ASC_EOF
            cVar    := SELF:ReadString():ToUpper()
            uValue  := SELF:ReadValue(wType)
            IF lUsemask
                lPut := _Like(SELF:cMask, cVar) == SELF:lInclude
            ENDIF
            IF lPut
                MemVarPut(cVar, uValue)
            ENDIF
            wType  := SELF:ReadByte()
        ENDDO


    METHOD ReadClipperFile(wType AS BYTE) AS VOID
        LOCAL cType AS STRING
        LOCAL nLen AS DWORD
        LOCAL cVar AS STRING
        LOCAL nType AS BYTE
        LOCAL lPut := TRUE AS LOGIC

        LOCAL nALenX := 0 as DWORD
        local nALenY := 0 as DWORD
        local cArrayName := "" as STRING
        local aValues := NULL_ARRAY as ARRAY
        local nIndex AS usual
        local nX as usual
        local nY as usual
        local nLenCVar as word
        local longCVarName as logic
        nIndex:= -1
        local uValue := NIL  as USUAL

        // Clipper type mem file, this has a diffent file format
        // file format is as follows
        // name,  11 bytes nul terminated (Clipper var name is 10 chars max)
        // type,  1  byte   ox80 ored with either "C", "N", "D", "L" or "A". FoxPro has more types that are not supported yet.
        //                  lower case char means name is bigger than 10 bytes. See longCVarName flag below
        // pad,   4  bytes
        // len,   1  byte
        // dec,   1  byte
        // class, 1  byte  not used
        // pad,   13 bytes
        // value, if C length is dec*256+len, null terminated
        //        if N length is 8 bytes, real8 format
        //                  for display - len=number of digits
        //                                dec=number of decimal places
        //        if L length is 1
        //        if D length is 8, real8 format
        //        if A array is array length
        DO WHILE wType <>ASC_EOF
            cVar	:= Chr(wType)+SELF:ReadBytes(10)
            cVar 	:= Left(cVar,At(Chr(0),cVar)-1):ToUpper()
            nType   := SELF:ReadByte()
            cType	:= Chr(_AND(nType,127U))
            SELF:ReadBytes(4)
//            Console.WriteLine("mem file {0},{1} , {2} ",cVar, cType, nIndex)

            longCVarName:= ( cType!= Upper(cType)) // ctype in lower case means variable name bigger than 10 bytes
            cType:= Upper(cType)
            IF lUsemask
                lPut := _Like(SELF:cMask, cVar ) == SELF:lInclude
            ENDIF
            if nIndex>=0
                if cVar == cArrayName
                   nIndex ++
                else
                   nIndex := -1
                endif
            endif

            SWITCH cType
            CASE "C" //String
                nLen:=Bin2W(SELF:ReadBytes(2))
                SELF:ReadBytes(14) //Class and pad
                if longCVarName
                   nLenCVar:= self:ReadWord()
                   cVar:= self:ReadBytes(nLenCVar)
                endif
                IF lPut
                    uValue:= SELF:ReadBytes(nLen-1)
                ENDIF
                SELF:ReadBytes(1) //nul byte

            CASE "N" //Real8
                SELF:ReadBytes(16) //Len, Dec, Class and pad
                if longCVarName
                   nLenCVar:= self:ReadWord()
                   cVar:= self:ReadBytes(nLenCVar)
                endif
                IF lPut
                    uValue:=  Bin2Real8(SELF:ReadBytes(8))
                ENDIF

            CASE "L" //Logic
                SELF:ReadBytes(16) //Len, Dec, Class and pad
                if longCVarName
                   nLenCVar:= self:ReadWord()
                   cVar:= self:ReadBytes(nLenCVar)
                endif
                IF lPut
                    uValue:= SELF:ReadByte() != 0
                ENDIF

            CASE "D" //Date
                SELF:ReadBytes(16) //Len, Dec, Class and pad
                if longCVarName
                   nLenCVar:= self:ReadWord()
                   cVar:= self:ReadBytes(nLenCVar)
                endif
                IF lPut
                    VAR r8 := Bin2Real8(SELF:ReadBytes(8))
                    uValue:= Bin2Date(L2Bin(LONGINT(r8)))
                ENDIF
             CASE "A" // ARRAY
                self:ReadBytes(16)
                if longCVarName
                   nLenCVar:= self:ReadWord()
                   cVar:= self:ReadBytes(nLenCVar)
                endif
                nALenX:= SELF:ReadWord() // array X Rows number
                if nALenX = 0
                   VAR error := Error{"Unknown type '"+cType+"' for memory array variable '"+cVar+ "' format error x dimension" }
                   error:FileName := cFileName
                   throw error
                endif
                nALenY:= SELF:ReadWord() // array Y Columns number
                nIndex:= 0
            OTHERWISE
                VAR error := Error{"Unknown type '"+cType+"' for memory variable '"+cVar+"'. Expected types are 'CDLNA'"}
                error:FileName := cFileName
                THROW error
            END SWITCH
            wType  := SELF:ReadByte()
            if nIndex < 0
               MemVarPut(cVar,uValue )
            elseif nIndex == 0 // array declaration
                if nALenY == 0
                   aValues := ArrayNew(nALenX)
                else
                   aValues := ArrayNew(nALenX, nALenY)
                endif
                cArrayName := cVar
                MemVarPut(cArrayName,aValues)
            else
                if nALenY ==0
                   aValues[nIndex] := uValue
                else
                   if nIndex<=nALenY
                      nX := 1
                      nY := nIndex
                   else
                      nX :=  (int)nIndex/nALenY+ iif(Mod(nIndex,nALenY)>0,1,0)
                      nY := nIndex - (nX-1) * nALenY
                   endif
                   aValues[nX , nY]:= uValue
                endif
            endif
        ENDDO

        RETURN

END CLASS
