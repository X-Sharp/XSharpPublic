// NtxHeader.prg
// Created by    : fabri
// Creation Date : 6/20/2018 5:04:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE XSharp.RDD

    /// <summary>
    /// The NtxHeader class.
    /// </summary>
    CLASS NtxHeader
        PROTECTED _hFile AS IntPtr
        
        PROPERTY Bytes AS DbfNtxHeader AUTO GET 
        
        
        METHOD Read() AS LOGIC
            LOCAL isOk AS LOGIC
            // Move to top
            FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
            // Read Buffer
            isOk := ( FRead3(SELF:_hFile, SELF:Bytes:Buffer, NTXOFFSETS.SIZE) == NTXOFFSETS.SIZE )
            //
            RETURN isOk
            
        METHOD Write() AS LOGIC
            LOCAL isOk AS LOGIC
            // Move to top
            FSeek3( SELF:_hFile, 0, SeekOrigin.Begin )
            // Write Buffer
            isOk := ( FWrite3(SELF:_hFile, SELF:Bytes:Buffer, NTXOFFSETS.SIZE) == NTXOFFSETS.SIZE )
            //
            RETURN isOk
            
            
            
        CONSTRUCTOR( fileHandle AS IntPtr )
            //
            SELF:_hFile := fileHandle
            
            RETURN
            
            
            /// <summary>NTX Header.</summary>        
        STRUCTURE DbfNtxHeader                     
            // Fixed Buffer of 1024 bytes
            // https://www.clicketyclick.dk/databases/xbase/format/ntx.html#NTX_STRUCT  
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            PUBLIC Buffer   AS BYTE[]
            // Hot ?  => Header has changed ?
            PUBLIC isHot	AS LOGIC
            
            PROPERTY Signature  AS WORD	;
            GET Buffer[NTXOFFSETS.SIG] ;
            SET Buffer[NTXOFFSETS.SIG] := (BYTE) VALUE
            
                PROPERTY IndexingVersion		AS WORD			;
                GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.INDEXING_VER);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.INDEXING_VER, SIZEOF(WORD))
                
                    PROPERTY FirstPageOffset		AS DWORD			;
                    GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.FPAGE_OFFSET);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.FPAGE_OFFSET, SIZEOF(WORD))
                    
                        PROPERTY NextUnusedPageOffset		AS DWORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.NUPAGE_OFFSET);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.NUPAGE_OFFSET, SIZEOF(WORD))
                        
                        // keysize + 2 longs. ie.e Left pointer + record no.
                        PROPERTY EntrySize		AS WORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.ENTRYSIZE);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.ENTRYSIZE, SIZEOF(WORD))
                        
                        PROPERTY KeySize		AS WORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.KEYSIZE);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.KEYSIZE, SIZEOF(WORD))
                        
                        PROPERTY KeyDecimals	AS WORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.KEYDECIMALS);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.KEYDECIMALS, SIZEOF(WORD))
                        
                        PROPERTY MaxItem	AS WORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.MAXITEM);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.MAXITEM, SIZEOF(WORD))
                        
                        PROPERTY HalfPage	AS WORD			;
                        GET BitConverter.ToUInt16(Buffer, NTXOFFSETS.HALFPAGE);
                        SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, NTXOFFSETS.HALFPAGE, SIZEOF(WORD))
                        
                        PROPERTY KeyExpression	 AS STRING
                        GET 
                            LOCAL fieldName := BYTE[]{NTXOFFSETS.EXPRESSION_SIZE} AS BYTE[]
                            Array.Copy( Buffer, NTXOFFSETS.KEYEXPRESSION, fieldName, 0, NTXOFFSETS.EXPRESSION_SIZE )
                            LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
                            IF count == -1
                                count := NTXOFFSETS.EXPRESSION_SIZE
                            ENDIF
                            LOCAL str := System.Text.Encoding.ASCII:GetString( fieldName,0, count ) AS STRING
                            IF ( str == NULL )
                                str := String.Empty
                            ENDIF
                            str := str:Trim()
                            RETURN str
                        END GET
                        SET
                            // Be sure to fill the Buffer with 0
                            Array.Clear( Buffer, NTXOFFSETS.KEYEXPRESSION, NTXOFFSETS.EXPRESSION_SIZE )
                            System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Min(NTXOFFSETS.EXPRESSION_SIZE,VALUE:Length), Buffer, NTXOFFSETS.KEYEXPRESSION )
                        END SET
                    END PROPERTY
                    
                    PROPERTY Unique	AS LOGIC  ;
                    GET Buffer[NTXOFFSETS.UNIQUE] != 0 ;
                    SET Buffer[ NTXOFFSETS.UNIQUE ] := IIF(VALUE,1,0), isHot := TRUE
                    
                    PROPERTY ForExpression	 AS STRING
                    GET 
                        LOCAL fieldName := BYTE[]{NTXOFFSETS.EXPRESSION_SIZE} AS BYTE[]
                        Array.Copy( Buffer, NTXOFFSETS.FOREXPRESSION, fieldName, 0, NTXOFFSETS.EXPRESSION_SIZE )
                        LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
                        IF count == -1
                            count := NTXOFFSETS.EXPRESSION_SIZE
                        ENDIF
                        LOCAL str := System.Text.Encoding.ASCII:GetString( fieldName,0, count ) AS STRING
                        IF ( str == NULL )
                            str := String.Empty
                        ENDIF
                        str := str:Trim()
                        RETURN str
                    END GET
                    SET
                        // Be sure to fill the Buffer with 0
                        Array.Clear( Buffer, NTXOFFSETS.FOREXPRESSION, NTXOFFSETS.EXPRESSION_SIZE )
                        System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Min(NTXOFFSETS.EXPRESSION_SIZE,VALUE:Length), Buffer, NTXOFFSETS.FOREXPRESSION )
                    END SET
                END PROPERTY
                
                PROPERTY OrdName	 AS STRING
                GET 
                    LOCAL fieldName := BYTE[]{NTXOFFSETS.EXPRESSION_SIZE} AS BYTE[]
                    Array.Copy( Buffer, NTXOFFSETS.ORDNAME, fieldName, 0, NTXOFFSETS.EXPRESSION_SIZE )
                    LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
                    IF count == -1
                        count := NTXOFFSETS.EXPRESSION_SIZE
                    ENDIF
                    LOCAL str := System.Text.Encoding.ASCII:GetString( fieldName,0, count ) AS STRING
                    IF ( str == NULL )
                        str := String.Empty
                    ENDIF
                    str := str:Trim()
                    RETURN str
                END GET
                SET
                    // Be sure to fill the Buffer with 0
                    Array.Clear( Buffer, NTXOFFSETS.ORDNAME, NTXOFFSETS.EXPRESSION_SIZE )
                    System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Min(NTXOFFSETS.EXPRESSION_SIZE,VALUE:Length), Buffer, NTXOFFSETS.ORDNAME )
                END SET
            END PROPERTY
            
            
            
            METHOD initialize() AS VOID STRICT
                Buffer := BYTE[]{NTXOFFSETS.SIZE}
                isHot  := FALSE
                RETURN
                
                END STRUCTURE
                
        PUBLIC ENUM NTXOFFSETS
            MEMBER SIG			:= 0
            MEMBER INDEXING_VER := 2
            MEMBER FPAGE_OFFSET := 4
            MEMBER NUPAGE_OFFSET := 8
            MEMBER ENTRYSIZE := 12
            MEMBER KEYSIZE := 14
            MEMBER KEYDECIMALS := 16
            MEMBER MAXITEM := 18
            MEMBER HALFPAGE := 20
            MEMBER KEYEXPRESSION := 22
            MEMBER EXPRESSION_SIZE := 256
            MEMBER UNIQUE := 258
            MEMBER FOREXPRESSION := 282
            MEMBER ORDNAME := 538
            
            MEMBER SIZE         := 1024
            
        END ENUM
        
    END CLASS
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx