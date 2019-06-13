
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Runtime.InteropServices
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE WordStruct
        [FieldOffset(0)]  INTERNAL shortValue AS Int16
        [FieldOffset(0)]  INTERNAL wordValue  AS UInt16
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        METHOD CLear() AS VOID
            shortValue := 0
            RETURN
    END STRUCTURE

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE LongStruct
        [FieldOffset(0)]  INTERNAL longValue  AS Int32
        [FieldOffset(0)]  INTERNAL dwordValue AS UInt32
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        [FieldOffset(2)]  INTERNAL b3 AS BYTE
        [FieldOffset(3)]  INTERNAL b4 AS BYTE
        METHOD CLear() AS VOID
            longValue := 0
            RETURN
    END STRUCTURE

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE DoubleStruct
        [FieldOffset(0)]  INTERNAL doubleValue  AS REAL8
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        [FieldOffset(2)]  INTERNAL b3 AS BYTE
        [FieldOffset(3)]  INTERNAL b4 AS BYTE
        [FieldOffset(4)]  INTERNAL b5 AS BYTE
        [FieldOffset(5)]  INTERNAL b6 AS BYTE
        [FieldOffset(6)]  INTERNAL b7 AS BYTE
        [FieldOffset(7)]  INTERNAL b8 AS BYTE
        METHOD Clear() AS VOID
            doubleValue := 0
            RETURN

        METHOD Reverse() AS VOID
            LOCAL copy := DoubleStruct{} AS DoubleStruct
            copy:doubleValue := SELF:doubleValue
            SELF:b1 := copy:B8
            SELF:b2 := copy:B7
            SELF:b3 := copy:B6
            SELF:b4 := copy:B5
            SELF:b5 := copy:B4
            SELF:b6 := copy:B3
            SELF:b7 := copy:B2
            SELF:b8 := copy:B1

        METHOD SaveToIndex(buffer AS BYTE[]) AS VOID
            SELF:Reverse()
            IF _AND(b1, 0x80) != 0
                SELF:B1 :=(BYTE) ~SELF:B1
                SELF:b2 :=(BYTE) ~SELF:b2
                SELF:b3 :=(BYTE) ~SELF:b3
                SELF:b4 :=(BYTE) ~SELF:b4
                SELF:b5 :=(BYTE) ~SELF:b5
                SELF:b6 :=(BYTE) ~SELF:b6
                SELF:b7 :=(BYTE) ~SELF:b7
                SELF:b8 :=(BYTE) ~SELF:b8
            ELSE
                b1 |= 0x80
           ENDIF
           buffer[0] := SELF:B1
           buffer[1] := SELF:b2
           buffer[2] := SELF:b3
           buffer[3] := SELF:b4
           buffer[4] := SELF:b5
           buffer[5] := SELF:b6
           buffer[6] := SELF:b7
           buffer[7] := SELF:b8
           RETURN

    END STRUCTURE

    INTERNAL STATIC CLASS CdxHelpers
        STATIC METHOD ToAscii (SELF bytes AS BYTE[]) AS STRING
            RETURN ToAscii(bytes, FALSE)

        STATIC METHOD ToAscii (SELF bytes AS BYTE[], lHex AS LOGIC) AS STRING
            VAR sb := System.Text.StringBuilder{}
            if bytes == NULL
                RETURN ""
            ENDIF
            IF lHex
                FOREACH VAR b IN bytes
                    //IF b > 0
                        sb:Append( String.Format("{0:X2}",b))
                    //ENDIF
                NEXT
                sb:Append(" ")
            ENDIF
            FOREACH VAR b IN bytes
                IF b > 31 .AND. b < 128
                    sb:Append( (CHAR) b)
                ELSE
                    sb:Append('.')
                ENDIF
            NEXT
            RETURN sb:ToString()
        STATIC CONSTRUCTOR
            KeyBitsTable := Dictionary <WORD, BYTE>{}

        INTERNAL STATIC KeyBitsTable AS Dictionary <WORD, BYTE>
        INTERNAL STATIC METHOD GetBits(wLength AS WORD) AS BYTE
            LOCAL bits AS BYTE
            
            IF KeyBitsTable:TryGetValue(wLength, OUT bits)
                RETURN bits
            ENDIF
            bits := 0
            LOCAL original := wLength AS WORD
            DO WHILE wLength > 0
                bits++
                wLength >>= 1
            ENDDO
            KeyBitsTable:Add(original, bits)
            RETURN bits
    END CLASS


    [DebuggerDisplay("{Page} {Pos}")];
    INTERNAL SEALED CLASS CdxStackEntry
        PROPERTY Page AS CdxTreePage AUTO
        PROPERTY Pos  AS WORD AUTO
    END CLASS

    [DebuggerDisplay("Stack: {Count}")];
    INTERNAL SEALED CLASS CdxPageStack
        PRIVATE _pages AS List<CdxStackEntry>
        CONSTRUCTOR(tag AS CdxTag)
            _pages := List<CdxStackEntry>{20}

        PROPERTY Count AS LONG GET _pages:Count

        METHOD Push(page AS CdxTreePage, nPos AS WORD)  AS LONG
            VAR entry := CdxStackEntry{}{ Page := page, Pos := nPos}
            _pages:Add(entry)
            RETURN _pages:Count

        METHOD Pop() AS LOGIC
            IF _pages:Count > 0
                _pages:RemoveAt(_pages:Count-1)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        PROPERTY Top AS CdxStackEntry
            GET
                IF _pages:Count > 0
                    RETURN _pages[_pages:Count-1]
                ENDIF
                RETURN NULL
            END GET
        END PROPERTY

        PROPERTY Empty AS LOGIC GET _pages:Count == 0

        PROPERTY Root AS CdxStackEntry
            GET
                IF _pages:Count > 0
                    RETURN _pages[0]
                ENDIF
                RETURN NULL
            END GET
        END PROPERTY

        METHOD FindPage(oPage AS CdxTreePage) AS LONG
            IF _Pages:Count > 0
                FOR VAR i := 0 TO _pages:Count -1
                    IF _pages[i]:Page:PageNo == oPage:PageNo
                        RETURN i
                    ENDIF
                NEXT
            ENDIF
            RETURN -1

        METHOD Replace(originalPage AS CdxTreePage, newPage AS CdxTreePage, nPos := 0 AS WORD) AS LOGIC
            VAR index := SELF:FindPage(originalPage)
            IF index >= 0
                _pages[index] := CdxStackEntry{}{Page := newPage, Pos := nPos}
                 RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD InsertOnTop(newPage AS CdxTreePage) AS LOGIC
            _pages:Insert(0,CdxStackEntry{}{Page := newPage, Pos := 0})
             RETURN TRUE
        METHOD SetPos(oPage AS CdxTreePage, nPos AS WORD) AS LOGIC
            VAR index := SELF:FindPage(oPage)
            IF index >= 0
                _pages[index]:Pos := nPos
                RETURN TRUE
            ENDIF
            RETURN FALSE
        METHOD GetParent(oPage AS CdxTreePage) AS CdxtreePage
            VAR index := SELF:FindPage(oPage)
            IF index > 0
                RETURN _pages[index-1]:Page
            ENDIF
            RETURN NULL
        METHOD Clear() AS VOID
            SELF:_pages:Clear()
            RETURN
    END CLASS


    [DebuggerDisplay("Action {Type}")];
    INTERNAL CLASS CdxAction
        INTERNAL PROPERTY Type  AS CdxActionType AUTO   
        INTERNAL PROPERTY Page  AS CdxTreePage   AUTO  := NULL
        INTERNAL PROPERTY Page2 AS CdxTreePage   AUTO  := NULL
        INTERNAL PROPERTY Pos   AS LONG AUTO           := 0
        INTERNAL PROPERTY Recno AS LONG AUTO           := -1
        INTERNAL PROPERTY Key   AS BYTE[] AUTO         := NULL
        INTERNAL PROPERTY ChildPage AS LONG AUTO       := 0
        PRIVATE STATIC _Ok AS CdxAction

        STATIC CONSTRUCTOR
            _Ok := CdxAction{CdxActionType.Ok}

        PRIVATE CONSTRUCTOR(ntype AS CdxActionType)
            Type   := nType


        STATIC PROPERTY Ok AS CdxAction GET _Ok

        METHOD IsOk AS LOGIC
            RETURN SELF:Type == CdxActionType.Ok

        INTERNAL STATIC METHOD AddKey( nRecno AS LONG, bKey AS BYTE[]) AS CdxAction
            RETURN CdxAction{CdxActionType.AddKey}{Recno := nRecno, Key := bKey}

        INTERNAL STATIC METHOD DeleteKey(oPage AS CdxtreePage, nPos AS LONG) AS CdxAction
            RETURN CdxAction{CdxActionType.DeleteKey}{Page := oPage, Pos := nPos}

        INTERNAL STATIC METHOD AddLeaf(oLeaf AS CdxtreePage, nRecno AS LONG, bKey AS BYTE[] ) AS CdxAction
            RETURN CdxAction{CdxActionType.AddLeaf}{ Page := oLeaf, Recno := nRecno, Key := bKey, Pos := -1}

        INTERNAL STATIC METHOD SplitLeaf(oLeaf AS CdxtreePage, nRecno AS LONG, bKey AS BYTE[], nPos AS LONG ) AS CdxAction
            RETURN CdxAction{CdxActionType.AddLeaf}{ Page := oLeaf, Recno := nRecno, Key := bKey, Pos := nPos}

        INTERNAL STATIC METHOD InsertKey(oPage AS CdxtreePage, nPos AS LONG, nRecno AS LONG, bKey AS BYTE[]) AS CdxAction
            RETURN CdxAction{CdxActionType.InsertKey}{Page := oPage, Pos := nPos,Recno := nRecno, Key := bKey}
            
        INTERNAL STATIC METHOD DeletePage(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.DeletePage}{Page := oPage}

        INTERNAL STATIC METHOD InsertParent(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.InsertParent}{Page := oPage}

        INTERNAL STATIC METHOD ChangeParent(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.ChangeParent}{Page := oPage}

        INTERNAL STATIC METHOD ChangeParent(oPage1 AS CdxTreePage, oPage2 as CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.ChangeParent}{Page := oPage1, Page2 := oPage2}

        INTERNAL STATIC METHOD AddBranch(oPage AS CdxTreePage,  nChild AS LONG, nRecno AS LONG, bKey AS BYTE[]) AS CdxAction
            RETURN CdxAction{CdxActionType.AddBranch}{Page := oPage, ChildPage := nChild, Recno := nRecno, Key := bKey, Pos := -1}

        INTERNAL STATIC METHOD SplitBranch(oPage AS CdxTreePage,  nChild AS LONG, nRecno AS LONG, bKey AS BYTE[], nPos AS INT) AS CdxAction
            RETURN CdxAction{CdxActionType.AddBranch}{Page := oPage, ChildPage := nChild, Recno := nRecno, Key := bKey, Pos := nPos}

        INTERNAL STATIC METHOD DeleteFromParent(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.DeleteFromParent}{Page := oPage}

        INTERNAL STATIC METHOD ExpandRecnos(oPage AS CdxLeafPage, nRecno as LONG, bKey as BYTE[], nPos as INT) AS CdxAction
            RETURN CdxAction{CdxActionType.ExpandRecnos}{Page := oPage, Recno := nRecno, Key := bKey, Pos := nPos}

        INTERNAL STATIC METHOD Balance(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.Balance}{Page := oPage}

        INTERNAL STATIC METHOD OutOfBounds(oPage AS CdxTreePage) AS CdxAction
            RETURN CdxAction{CdxActionType.OutOfBounds}{Page := oPage}
            
    END CLASS

END NAMESPACE
