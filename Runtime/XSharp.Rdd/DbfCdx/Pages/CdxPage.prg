// CdxBlock.prg
// Created by    : fabri
// Creation Date : 10/25/2018 10:43:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.RDD.CDX

 	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
	INTERNAL ABSTRACT CLASS CdxPage
	    PROTECTED _bag      AS CDXOrderBag
        PROTECTED _tag      AS CDXTag
        PROTECTED _nPage    AS Int32
		PROTECTED _buffer   AS BYTE[]
		PROTECTED _hot      AS LOGIC        // Hot ?  => Page has changed ?
        PROTECTED _dumped   AS LOGIC
        INTERNAL VIRTUAL PROPERTY PageType AS CdxPageType GET CdxPageType.Undefined SET


        INTERNAL PROPERTY Dumped AS LOGIC GET _dumped SET _dumped := VALUE
        INTERNAL PROPERTY IsHot  AS LOGIC GET _hot SET _hot := VALUE
        INTERNAL PROPERTY Tag    AS CDXTag GET _tag SET _setTag(VALUE)
        INTERNAL PROPERTY Buffer AS BYTE[] GET _buffer
        INTERNAL PROPERTY PageNo AS Int32 GET _nPage SET _nPage := VALUE


        PROTECTED VIRTUAL METHOD _setTag(newTag AS CdxTag) AS VOID
            _tag := newTag

        PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
			SELF:_bag    := bag

	    PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[])
			SELF(bag)
            SELF:_nPage  := nPage
			SELF:isHot  := FALSE
            IF nPage == -1
                _buffer := bag:AllocBuffer()
            ELSE
    			SELF:_buffer := buffer
            ENDIF
		    RETURN
        #region Read/Write

        PROTECTED METHOD SetBuffer(buffer AS BYTE[]) AS VOID
            _buffer := buffer

			
		PROTECTED INTERNAL VIRTUAL METHOD Write() AS LOGIC
			VAR lOk :=  _Bag:Write(SELF)
            SELF:IsHot := FALSE
            RETURN lOk

    	PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR lOk :=  _Bag:Read(SELF)
            SELF:IsHot := FALSE
            RETURN lOk

        INTERNAL METHOD Clear() AS VOID
            _buffer := BYTE[]{_buffer:Length}
            RETURN

        INTERNAL METHOD SetEmptyRoot() AS VOID
            SELF:Clear()
            SELF:PageType   := CdxPageType.Leaf
            VAR oLeaf       := CdxLeafPage{SELF:_bag,SELF}
            oLeaf:InitBlank(SELF:_tag)
            SELF:PageType  := CdxPageType.Leaf | CdxPageType.Root
            SELF:Write()

        #endregion
        #region Helper Methods to read/write numbers are strings out of the buffer
        
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetByte(nOffSet AS INT) AS BYTE
				RETURN Buffer[ nOffset]

            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetByte(nOffSet AS INT, bValue AS BYTE) AS VOID
				Buffer[ nOffset] := bValue
                _hot := TRUE

            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetWord(nOffSet AS INT) AS WORD
                LOCAL nValue := WordStruct{} AS WordStruct
	            nValue:b1 := buffer[nOffSet]
                nValue:b2 := buffer[nOffSet+1]
                RETURN nValue:wordValue

           [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetShort(nOffSet AS INT) AS SHORT
                LOCAL nValue := WordStruct{} AS WordStruct
	            nValue:b1 := buffer[nOffSet]
                nValue:b2 := buffer[nOffSet+1]
                RETURN nValue:ShortValue
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetWord(nOffSet AS INT, wValue AS WORD) AS VOID
                LOCAL nValue := WordStruct{} AS WordStruct
                nValue:wordValue := wValue
	            buffer[nOffSet]   := nValue:b1
                buffer[nOffSet+1] := nValue:b2
				_hot := TRUE
                RETURN 

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetShort(nOffSet AS INT, siValue AS SHORT) AS VOID
                LOCAL nValue := WordStruct{} AS WordStruct
                nValue:shortValue := siValue
	            buffer[nOffSet]   := nValue:b1
                buffer[nOffSet+1] := nValue:b2
				_hot := TRUE
                RETURN 

				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetDWord(nOffSet AS INT) AS DWORD
                LOCAL nValue := LongStruct{} AS LongStruct
	            nValue:b1 := buffer[nOffSet]
                nValue:b2 := buffer[nOffSet+1]
                nValue:b3 := buffer[nOffSet+2]
                nValue:b4 := buffer[nOffSet+3]
                RETURN nValue:dwordValue

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetDWordLE(nOffSet AS INT) AS DWORD
                LOCAL nValue := LongStruct{} AS LongStruct
	            nValue:b4 := buffer[nOffSet]
                nValue:b3 := buffer[nOffSet+1]
                nValue:b2 := buffer[nOffSet+2]
                nValue:b1 := buffer[nOffSet+3]
                RETURN nValue:dwordValue
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetDWord(nOffSet AS INT, dwValue AS DWORD) AS VOID
                LOCAL nValue  := LongStruct{} AS LongStruct
                nValue:dwordValue := dwValue
	            buffer[nOffSet]   :=  nValue:b1  
                buffer[nOffSet+1] :=  nValue:b2  
                buffer[nOffSet+2] :=  nValue:b3  
                buffer[nOffSet+3] :=  nValue:b4  
				_hot := TRUE

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetDWordLE(nOffSet AS INT, dwValue AS DWORD) AS VOID
                LOCAL nValue := LongStruct{} AS LongStruct
                nValue:dwordValue := dwValue
	            buffer[nOffSet]   :=  nValue:b4  
                buffer[nOffSet+1] :=  nValue:b3  
                buffer[nOffSet+2] :=  nValue:b2  
                buffer[nOffSet+3] :=  nValue:b1  
				_hot := TRUE
                
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetLong(nOffSet AS INT) AS Int32
                LOCAL nValue := LongStruct{} AS LongStruct
	            nValue:b1 := buffer[nOffSet]
                nValue:b2 := buffer[nOffSet+1]
                nValue:b3 := buffer[nOffSet+2]
                nValue:b4 := buffer[nOffSet+3]
                RETURN nValue:LongValue

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _GetLongLE(nOffSet AS INT) AS Int32
                LOCAL nValue := LongStruct{} AS LongStruct
	            nValue:b4 := buffer[nOffSet]
                nValue:b3 := buffer[nOffSet+1]
                nValue:b2 := buffer[nOffSet+2]
                nValue:b1 := buffer[nOffSet+3]
                RETURN nValue:LongValue
			
				
			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetLong(nOffSet AS INT, liValue AS Int32) AS VOID
                LOCAL nValue := LongStruct{} AS LongStruct
                nValue:LongValue := liValue
	            buffer[nOffSet]   :=  nValue:b1  
                buffer[nOffSet+1] :=  nValue:b2  
                buffer[nOffSet+2] :=  nValue:b3  
                buffer[nOffSet+3] :=  nValue:b4  
				_hot := TRUE

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			PROTECTED INTERNAL METHOD _SetLongLE(nOffSet AS INT, liValue AS Int32) AS VOID
                LOCAL nValue := LongStruct{} AS LongStruct
                nValue:LongValue := liValue
	            buffer[nOffSet]   :=  nValue:b4 
                buffer[nOffSet+1] :=  nValue:b3  
                buffer[nOffSet+2] :=  nValue:b2  
                buffer[nOffSet+3] :=  nValue:b1  
				_hot := TRUE


			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			INTERNAL METHOD _GetString( nOffSet AS INT, count AS INT) AS STRING
				LOCAL str := _bag:Encoding:GetString( _buffer,nOffSet, count ) AS STRING
				IF str == NULL 
					str := String.Empty
                ELSEIF str:EndsWith(e"\0")
                    str := str:TrimEnd(<CHAR>{'\0'})
				ENDIF
				RETURN str

			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
            INTERNAL METHOD _GetBytes( nOffSet AS INT, count AS INT) AS BYTE[]
                LOCAL result AS BYTE[]
                result := BYTE[]{count}
			    System.Array.Copy(_buffer, nOffSet, result, 0, count)
                RETURN result


			[MethodImpl(MethodImplOptions.AggressiveInlining)];        
			INTERNAL  METHOD _SetString(nOffSet AS INT, nSize AS INT, sValue AS STRING) AS VOID
				// Be sure to fill the Buffer with 0
				MemSet( _buffer, nOffSet, nSize , 0)
				_bag:Encoding:GetBytes( sValue, 0, Math.Min(nSize,sValue:Length), _buffer, nOffSet)
				_hot := TRUE
            #endregion


        STATIC METHOD MemSet(bytes AS BYTE[], start AS INT, length AS INT, bValue AS BYTE) AS VOID
            VAR finish := start+length-1
            FOR VAR i := start TO finish
                bytes[i] := bValue
            NEXT

		INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD

        INTERNAL VIRTUAL METHOD Dump AS STRING
            RETURN String.Empty

 
       
	END CLASS




END NAMESPACE 
