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
        ELSE
            LOCAL symVar AS SYMBOL
            symVar := _PublicFirst()
            
            DO WHILE symVar != NULL_SYMBOL
                IF lAll .OR. _Like(cSkel, Symbol2String(symVar)) == lLike
                    oMemWriter:WriteValue(symVar, MemVarGetSym(symVar))
                ENDIF
                symVar := _PublicNext()
            ENDDO  
            symVar := _PrivateFirst()
            
            DO WHILE symVar != NULL_SYMBOL
                IF lAll .OR. _Like(cSkel, Symbol2String(symVar)) == lLike
                    oMemWriter:WriteValue(symVar, MemVarGetSym(symVar))
                ENDIF
                symVar := _PrivateNext()
            ENDDO  
        oMemWriter:CLose()
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
            WriteByte(ASC_EOF)
            FClose(hFile)  
            hFile := F_ERROR
            UnRegisterAxit(SELF)
        ENDIF
        RETURN

    METHOD _Error() AS VOID
        LOCAL oErr AS Error
        oErr := Error{FException()}
        oErr:Gencode := EG_READ
        oErr:SubCode := 2005
        oErr:FuncSym := "_MRESTORE"
        oErr:FileName := cFileName
        oErr:OSCode   := FError()
        THROW oErr

    METHOD WriteStr(cVar AS STRING) AS VOID
        WriteBytes(W2Bin(WORD(SLen(cVar))))
        WriteBytes(cVar)
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
        
    METHOD WriteValue(symVar AS SYMBOL, uVal AS USUAL) AS VOID   
        LOCAL wType 	AS DWORD
        LOCAL cVarName 	AS STRING
        LOCAL wArrayEl 	AS DWORD             
        LOCAL writeName AS Action
        wType    := UsualType(uVal)
        IF symVar != NULL_SYMBOL
            cVarName := Symbol2String(symVar)
            WriteName := { =>WriteStr(cVarName) }
        ELSE
            WriteName := { =>  }
        ENDIF
        
        SWITCH wType
            CASE __UsualType.STRING
                WriteByte(__UsualType.STRING)
                WriteName()
                WriteStr(uVal)
                
            CASE __UsualType.LONG
                WriteByte(__UsualType.LONG)
                WriteName()
                WriteBytes(L2Bin(uVal))
                
        CASE __UsualType.DATE
            CASE __UsualType.DATETIME
                WriteByte(__UsualType.DATE)
                WriteName()
                WriteBytes(Date2Bin(uVal))
                
            CASE __UsualType.FLOAT
            CASE __UsualType.Decimal
                WriteByte(__UsualType.FLOAT)
                WriteName()
                WriteBytes(F2Bin(uVal))
                
            CASE __UsualType.LOGIC
                WriteByte(__UsualType.LOGIC)
                WriteName()
                WriteByte(IIF(uVal,1,0))
                
            CASE __UsualType.SYMBOL
                WriteByte(__UsualType.SYMBOL)
                WriteName()
                WriteStr(Symbol2String(uVal))
                
            CASE __UsualType.ARRAY
                wArrayEl := AScan(aArrayList, uVal)
                IF wArrayEl =0
                    WriteByte(__UsualType.ARRAY)
                    WriteName()
                    WriteArr(uVal)
                    ELSE
                        WriteByte(__UsualType.ARRAY +0x80)
                        WriteName()
                    WriteBytes(W2Bin((WORD) wArrayEl))
                ENDIF
                
            CASE OBJECT
                wArrayEl := AScan(aObjectList, uVal)
                IF wArrayEl =0
                    WriteByte(__UsualType.OBJECT)
                    WriteName()
                    WriteObj(uVal)
                    ELSE
                        WriteByte(__UsualType.OBJECT +0x80)
                        WriteName()
                    WriteBytes(W2Bin((WORD) wArrayEl))
                ENDIF
                
            CASE PTR
                WriteByte(__UsualType.PTR)
                WriteName()
                WriteBytes(Ptr2Bin(uVal))
                WriteBytes(W2Bin(WORD(_GetMRandID())))
                
            OTHERWISE
                WriteByte(__UsualType.VOID)
                WriteName()
                
        END SWITCH
        RETURN	
        
    METHOD WriteObj  (oVar AS OBJECT) AS VOID
        LOCAL aIVarList AS ARRAY
        LOCAL nLen      AS DWORD
        LOCAL wI        AS DWORD
        LOCAL symIvar 	AS SYMBOL
        IF oVar == NULL_OBJECT
            WriteStr("")
        ELSE
            WriteStr(Symbol2String(ClassName(oVar)))
            AAdd(SELF:aObjectList, oVar)
                
            aIVarList :=IVarList(oVar)
            nLen :=ALen(aIVarList)
            WriteBytes(W2Bin(WORD(nLen)))
                
            FOR wI := 1 UPTO nLen   
                symIvar :=aIVarList[wI]
                IF IVarGetInfo(oVar, symIVar) > 0
                    WriteValue(symIvar, IVarGet(oVar, symIvar))
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
        WriteBytes(W2Bin(WORD(wArrayLen)))
        
        FOR wI := 1 UPTO wArrayLen
        
            uVal   :=aVar[wI]        
            WriteValue(NULL_SYMBOL, uVal)
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
        oErr:SubCode := 2005
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
            CASE __UsualType.STRING
                uValue := SELF:ReadString()
                
            CASE __UsualType.LONG
                uValue :=Bin2L(SELF:ReadBytes(4))
                
            CASE __UsualType.DATE
                uValue :=Bin2Date(SELF:ReadBytes(4))
                
            CASE __UsualType.FLOAT
                uValue :=Bin2F(SELF:ReadBytes(12))
                
            CASE __UsualType.LOGIC
                uValue := ReadByte() != 0
                
            CASE __UsualType.SYMBOL
                uValue := String2Symbol(SELF:ReadString())
                
            CASE __UsualType.ARRAY     
                wLen := SELF:ReadWord()
                uValue := SELF:ReadArray(wLen)
                
            CASE __UsualType.ARRAY +0x80
                wLen := SELF:ReadWord()
                uValue :=aArrayList[wLen]
                
            CASE __UsualType.OBJECT
                uValue :=SELF:ReadObject(SELF:ReadString())
                
            CASE __UsualType.OBJECT +0x80    
                wLen := SELF:ReadWord()
                uValue :=aObjectList[wLen]
                
            CASE __UsualType.PTR
                uValue :=Bin2Ptr(SELF:ReadBytes(4))
                IF SELF:ReadWord() <>_GetMRandID()
                    uValue :=NULL_PTR
                ENDIF
                
        CASE __UsualType.VOID
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
            wType    := ReadByte()
            aVar[wI] := ReadValue(wType)
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
        
        nLen := ReadWord() 
        
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
            SELF:lUseMask := TRUE
            SELF:cMask    := Upper(cMask)
            SELF:lInclude := lInclude
        ENDIF
        
        wType  := SELF:ReadByte()
        
        IF wType<ASC_A //401@TR001
            ReadVOFile(wType)
            ELSE
            ReadClipperFile(wType)
        ENDIF
        
    METHOD ReadString() AS STRING
        LOCAL nLen AS WORD	
        nLen := SELF:ReadWord()
        RETURN SELF:ReadBytes(nLen)		
        
    METHOD ReadVOFile(wType AS BYTE) AS VOID
        LOCAL symVar AS SYMBOL
        LOCAL uValue AS USUAL
        LOCAL lPut := TRUE AS LOGIC
        DO WHILE wType <>ASC_EOF
            symVar := String2Symbol(ReadString())
            uValue := ReadValue(wType)   
            IF lUseMask    
                lPut := _Like(SELF:cMask, Symbol2String(symVar)) == SELF:lInclude
            ENDIF
            IF lPut        		
                MemVarPutSym(symVar, uValue)
            ENDIF
            wType  := SELF:ReadByte()
        ENDDO
        
        
    METHOD ReadClipperFile(wType AS BYTE) AS VOID    
        LOCAL symVar AS SYMBOL
        LOCAL cType AS STRING 
        LOCAL nLen AS DWORD   
        LOCAL cVar AS STRING 
        LOCAL nType AS BYTE
        LOCAL lPut := TRUE AS LOGIC
        
        //Clipper type mem file
        // file format is as follows
        // name,  11 bytes nul terminated (Clipper var name is 10 chars max)
        // type,  1  byte   ox80 ored with either "C", "N", "D" or "L"
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
        DO WHILE wType <>ASC_EOF
            cVar	:= Chr(wType)+SELF:ReadBytes(10)
            symVar 	:= String2Symbol(Left(cVar,At(Chr(0),cVar)-1))
            nType   := SELF:ReadByte()
            cType	:= Chr(_AND(nType,127))
            SELF:ReadBytes(4) // pad
            IF lUseMask    
                lPut := _Like(SELF:cMask, Symbol2String(symVar)) == SELF:lInclude
            ENDIF
            
            DO CASE
                CASE cType ="C" //String
                    nLen:=Bin2W(SELF:ReadBytes(2))
                    SELF:ReadBytes(14) //Class and pad
                    IF lPut
                        MemVarPutSym(symVar, SELF:ReadBytes(nLen-1))
                    ENDIF
                SELF:ReadBytes(1) //nul byte
                CASE cType ="N" //Real8
                    SELF:ReadBytes(16) //Len, Dec, Class and pad
                    IF lPut
                        MemVarPutSym(symVar, Bin2Real8(SELF:ReadBytes(8)))
                ENDIF
                CASE cType ="L" //Logic
                    SELF:ReadBytes(16) //Len, Dec, Class and pad
                    IF lPut
                        MemVarPutSym(symVar, ReadByte() != 0)  
                ENDIF
                CASE cType ="D" //Date
                    SELF:ReadBytes(16) //Len, Dec, Class and pad
                    IF lPut            
                        VAR r8 := Bin2Real8(SELF:ReadBytes(8))
                        MemVarPutSym(symVar, Bin2Date(L2Bin(LONGINT(r8))))
                ENDIF
                OTHERWISE
                BREAK
            ENDCASE
            wType  := SELF:ReadByte()
        ENDDO
        
        RETURN	
        
END CLASS
