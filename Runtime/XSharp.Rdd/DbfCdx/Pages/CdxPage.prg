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
USING STATIC XSharp.Conversions
#include "CdxDebug.xh"
#pragma warnings (170, off)

#define USEATTRIB
#ifdef USEATTRIB
#XTRANSLATE \[HIDDEN\] => \[DebuggerBrowsable(DebuggerBrowsableState.Never)\]
#XTRANSLATE \[INLINE\] => \[MethodImpl(MethodImplOptions.AggressiveInlining)\]
#XTRANSLATE \[NODEBUG\] => \[DebuggerStepThroughAttribute\]
#else
#XTRANSLATE \[HIDDEN\] =>
#XTRANSLATE [\INLINE\] =>
#XTRANSLATE \[NODEBUG\] =>
#endif

BEGIN NAMESPACE XSharp.RDD.CDX

 	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
	INTERNAL ABSTRACT CLASS CdxPage
	    PROTECTED _bag      AS CdxOrderBag
        PROTECTED _tag      AS CdxTag
        PROTECTED _nPage    AS Int32
		PROTECTED _buffer   AS BYTE[]
		PROTECTED _hot      AS LOGIC        // Hot ?  => Page has changed ?
        PROTECTED _dumped   AS LOGIC
        INTERNAL PROPERTY Generation AS DWORD AUTO := 0
        INTERNAL VIRTUAL PROPERTY PageType AS CdxPageType GET CdxPageType.Undefined SET


        INTERNAL PROPERTY Dumped AS LOGIC  GET _dumped SET _dumped := VALUE
        INTERNAL PROPERTY IsHot  AS LOGIC  GET _hot SET _hot := VALUE
        INTERNAL PROPERTY Tag    AS CdxTag GET _tag SET SELF:_setTag(VALUE)
        INTERNAL PROPERTY Buffer AS BYTE[] GET _buffer
        INTERNAL PROPERTY PageNo AS Int32  GET _nPage SET _nPage := VALUE
        INTERNAL PROPERTY Bag    AS CdxOrderBag GET _bag
        // For debugging
        INTERNAL PROPERTY PageNoX AS STRING GET _nPage:ToString("X8")

        INTERNAL VIRTUAL METHOD _setTag(newTag AS CdxTag) AS VOID
            _tag := newTag

        PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag )
			SELF:_bag    := bag
            IF (bag:Root != NULL)
                SELF:Generation := bag:Root:RootVersion
            ELSE
                NOP
            ENDIF
	    PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[])
			SELF(bag)
            SELF:_nPage  := nPage
			SELF:IsHot  := FALSE
            IF nPage == -1
                _buffer := bag:AllocBuffer()
            ELSE
    			SELF:_buffer := buffer
            ENDIF
            SELF:Generation := bag:Root:RootVersion
		    RETURN
        #region Read/Write

        INTERNAL METHOD SetBuffer(buffer AS BYTE[]) AS VOID
            _buffer := buffer


		INTERNAL VIRTUAL METHOD Write() AS LOGIC
            IF ! Self:IsHot
                return TRUE
            ENDIF
			VAR lOk :=  _bag:Write(SELF)
            SELF:IsHot := FALSE
            RETURN lOk

        INTERNAL VIRTUAL METHOD Read() AS LOGIC
			VAR lOk :=  _bag:Read(SELF)
            SELF:IsHot := FALSE
            RETURN lOk

        INTERNAL VIRTUAL METHOD Clear() AS VOID
            _buffer := BYTE[]{_buffer:Length}
            _hot    := FALSE
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

			[INLINE];
			PROTECTED INTERNAL METHOD _GetByte(nOffset AS INT) AS BYTE
				RETURN Buffer[ nOffset]

            [INLINE];
			PROTECTED INTERNAL METHOD _SetByte(nOffset AS INT, bValue AS BYTE) AS VOID
				Buffer[ nOffset] := bValue
                _hot := TRUE

            [INLINE];
			PROTECTED INTERNAL METHOD _GetWord(nOffSet AS INT) AS WORD
                BEGIN UNCHECKED
                    LOCAL wValue AS __WordStruct
                    wValue:b1 := Buffer[nOffSet]
                    wValue:b2 := Buffer[nOffSet+1]
                    RETURN wValue:wordValue
                END UNCHECKED

           [INLINE];
			PROTECTED INTERNAL METHOD _GetShort(nOffSet AS INT) AS SHORT
                BEGIN UNCHECKED
                    LOCAL wValue AS __WordStruct
    	            wValue:b1 := Buffer[nOffSet]
                    wValue:b2 := Buffer[nOffSet+1]
                    RETURN wValue:shortValue
                END UNCHECKED

			[INLINE];
			PROTECTED INTERNAL METHOD _SetWord(nOffSet AS INT, _wValue AS WORD) AS VOID
                BEGIN UNCHECKED
                    LOCAL wValue AS __WordStruct
                    wValue:wordValue := _wValue
	                Buffer[nOffSet]   := wValue:b1
                    Buffer[nOffSet+1] := wValue:b2
				    _hot := TRUE
                END UNCHECKED
                RETURN

			[INLINE];
			PROTECTED INTERNAL METHOD _SetShort(nOffSet AS INT, siValue AS SHORT) AS VOID
                LOCAL wValue AS __WordStruct
                wValue:shortValue := siValue
	            Buffer[nOffSet]   := wValue:b1
                Buffer[nOffSet+1] := wValue:b2
				_hot := TRUE
                RETURN


			[INLINE];
			PROTECTED INTERNAL METHOD _GetDWord(nOffSet AS INT) AS DWORD
                LOCAL liValue AS __LongStruct
	            liValue:b1 := Buffer[nOffSet]
                liValue:b2 := Buffer[nOffSet+1]
                liValue:b3 := Buffer[nOffSet+2]
                liValue:b4 := Buffer[nOffSet+3]
                RETURN liValue:dwordValue

			[INLINE];
			PROTECTED INTERNAL METHOD _GetDWordLE(nOffSet AS INT) AS DWORD
                LOCAL liValue AS __LongStruct
	            liValue:b4 := Buffer[nOffSet]
                liValue:b3 := Buffer[nOffSet+1]
                liValue:b2 := Buffer[nOffSet+2]
                liValue:b1 := Buffer[nOffSet+3]
                RETURN liValue:dwordValue

			[INLINE];
			PROTECTED INTERNAL METHOD _SetDWord(nOffSet AS INT, dwValue AS DWORD) AS VOID
                LOCAL liValue AS __LongStruct
                liValue:dwordValue := dwValue
	            Buffer[nOffSet]   :=  liValue:b1
                Buffer[nOffSet+1] :=  liValue:b2
                Buffer[nOffSet+2] :=  liValue:b3
                Buffer[nOffSet+3] :=  liValue:b4
				_hot := TRUE

			[INLINE];
			PROTECTED INTERNAL METHOD _SetDWordLE(nOffSet AS INT, dwValue AS DWORD) AS VOID
                LOCAL liValue AS __LongStruct
                liValue:dwordValue := dwValue
	            Buffer[nOffSet]   :=  liValue:b4
                Buffer[nOffSet+1] :=  liValue:b3
                Buffer[nOffSet+2] :=  liValue:b2
                Buffer[nOffSet+3] :=  liValue:b1
				_hot := TRUE

			[INLINE];
			PROTECTED INTERNAL METHOD _GetLong(nOffSet AS INT) AS Int32
                LOCAL liValue AS __LongStruct
	            liValue:b1 := Buffer[nOffSet]
                liValue:b2 := Buffer[nOffSet+1]
                liValue:b3 := Buffer[nOffSet+2]
                liValue:b4 := Buffer[nOffSet+3]
                RETURN liValue:longValue

			[INLINE];
			PROTECTED INTERNAL METHOD _GetLongLE(nOffSet AS INT) AS Int32
                LOCAL liValue AS __LongStruct
	            liValue:b4 := Buffer[nOffSet]
                liValue:b3 := Buffer[nOffSet+1]
                liValue:b2 := Buffer[nOffSet+2]
                liValue:b1 := Buffer[nOffSet+3]
                RETURN liValue:longValue


			[INLINE];
			PROTECTED INTERNAL METHOD _SetLong(nOffSet AS INT, longValue AS Int32) AS VOID
                LOCAL liValue AS __LongStruct
                liValue:longValue := longValue
	            Buffer[nOffSet]   :=  liValue:b1
                Buffer[nOffSet+1] :=  liValue:b2
                Buffer[nOffSet+2] :=  liValue:b3
                Buffer[nOffSet+3] :=  liValue:b4
				_hot := TRUE

			[INLINE];
			PROTECTED INTERNAL METHOD _SetLongLE(nOffSet AS INT, longValue AS Int32) AS VOID
                LOCAL liValue AS __LongStruct
                liValue:longValue := longValue
	            Buffer[nOffSet]   :=  liValue:b4
                Buffer[nOffSet+1] :=  liValue:b3
                Buffer[nOffSet+2] :=  liValue:b2
                Buffer[nOffSet+3] :=  liValue:b1
				_hot := TRUE


			[INLINE];
			INTERNAL METHOD _GetString( nOffSet AS INT, count AS INT) AS STRING
				LOCAL str := _bag:Encoding:GetString( _buffer,nOffSet, count ) AS STRING
				IF str == NULL
					str := String.Empty
                ELSEIF str:EndsWith(e"\0")
                    str := str:TrimEnd(<CHAR>{'\0'})
				ENDIF
				RETURN str

			[INLINE];
            INTERNAL METHOD _GetBytes( nOffSet AS INT, count AS INT) AS BYTE[]
                LOCAL result AS BYTE[]
                result := BYTE[]{count}
			    System.Array.Copy(_buffer, nOffSet, result, 0, count)
                RETURN result


			[INLINE];
			INTERNAL  METHOD _SetString(nOffSet AS INT, nSize AS INT, sValue AS STRING) AS VOID
				// Be sure to fill the Buffer with 0
				MemSet( _buffer, nOffSet, nSize , 0)
				_bag:Encoding:GetBytes( sValue, 0, Math.Min(nSize,sValue:Length), _buffer, nOffSet)
				_hot := TRUE
            #endregion


        STATIC METHOD MemSet(bytes AS BYTE[], start AS INT, length AS INT, bValue AS BYTE) AS VOID
            BEGIN UNCHECKED
                VAR finish := start+length-1
                FOR VAR i := start TO finish
                    bytes[i] := bValue
                NEXT
            END UNCHECKED
		INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD

        INTERNAL VIRTUAL METHOD Dump AS STRING
            RETURN String.Empty



	END CLASS




END NAMESPACE
