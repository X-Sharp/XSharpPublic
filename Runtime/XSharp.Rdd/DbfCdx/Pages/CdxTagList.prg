//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxTagList class is a special CdxLeaf. Its NodeAttribute must be Root + Leaf = TagList
	/// </summary>
	INTERNAL SEALED CLASS CdxTagList INHERIT CdxLeafPage
        PRIVATE _tags AS List<CdxTag>
        INTERNAL PROPERTY Encoding as System.Text.Encoding GET _bag:Encoding

        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer, nKeyLen)

	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , page AS CdxPage, keyLen AS WORD)
            SUPER(bag  , page:PageNo, page:Buffer, keyLen)

        INTERNAL METHOD ReadTags() AS List<CdxTag>
            _tags := List<CdxTag>{}
            System.Diagnostics.Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf) .AND. SELF:PageType:HasFlag(CdxPageType.Root))
            local oError := NULL_OBJECT as Exception
            FOR VAR nI := 0 TO SELF:NumKeys-1
                LOCAL nRecno    := SELF:GetRecno(nI) AS Int32
                LOCAL bName     := SELF:GetKey(nI)  AS BYTE[]
                LOCAL cName     := SELF:Encoding:GetString( bName, 0, bName:Length) AS STRING
                cName           := cName:TrimEnd(<CHAR>{'\0'})
                VAR tag         := CdxTag{_bag,  nRecno, cName:Trim()}
                _tags:Add(tag)
                IF !tag:IsOpen
                    oError := RuntimeState.LastRddError
                ENDIF
            NEXT
            // default sort for tags in an orderbag is on pageno.
            _tags:Sort( { tagX, tagY => tagX:Page - tagY:Page} )
            if oError != NULL
                RuntimeState.LastRddError := oError
            ENDIF
            RETURN _tags

        INTERNAL PROPERTY Tags AS IList<CdxTag> GET _tags

        INTERNAL OVERRIDE METHOD Initialize(keyLength AS WORD) AS VOID
            SUPER:Initialize(keyLength)
            _tags := List<CdxTag>{}
            SELF:PageType := CdxPageType.Leaf + CdxPageType.Root
            SELF:TrailByte := 0

        INTERNAL METHOD Remove(oTag AS CdxTag) AS LOGIC
            LOCAL found := FALSE AS LOGIC
            IF _tags:Contains(oTag)
                _tags:Remove(oTag)
                found := TRUE
            ELSE
                FOREACH VAR tag IN _tags
                    IF String.Compare(tag:OrderName, oTag:OrderName, StringComparison.OrdinalIgnoreCase) == 0
                        _tags:Remove(tag)
                        found := TRUE
                        EXIT
                    ENDIF
                NEXT
            ENDIF
            IF found
                SELF:_WriteTags(_tags)
            ENDIF
            RETURN found

        PRIVATE METHOD _WriteTags(tags AS List<CdxTag>) AS LOGIC
            VAR aTags := tags:ToArray()
            IF aTags:Length > 1
                System.Array.Sort(aTags,  { x,y => IIF (x:OrderName < y:OrderName , -1, 1)})
            ENDIF
            VAR dbytes := SELF:DataBytes
            VAR rbits  := SELF:RecordBits
            VAR mask   := SELF:RecnoMask
            SELF:Initialize(KeyLength)
            SELF:TrailByte  := 0
            SELF:DataBytes  := dbytes
            SELF:RecordBits := rbits
            SELF:RecnoMask  := mask
            FOREACH VAR tag IN aTags
                VAR bytes := BYTE[]{ SELF:KeyLength}
                VAR name := tag:OrderName
            	// Be sure to fill the Buffer with 0
				MemSet( bytes, 0, KeyLength, 0)
                SELF:Encoding:GetBytes( name, 0, Math.Min(KeyLength,name:Length), bytes, 0)
				_hot := TRUE

                LOCAL action := SELF:Add(tag:Header:PageNo, bytes) AS CdxAction
                IF action:Type == CdxActionType.ExpandRecnos
                    VAR leaves := SELF:GetKeys()
                    LOCAL stream    AS Stream
                    LOCAL fileSize  AS LONG
                    stream   := FGetStream(_bag:_hFile)
                    fileSize := (LONG) stream:Length
                    SELF:SetRecordBits(fileSize)
                    SELF:SetKeys(leaves, 0, leaves:Count)
                    action := SELF:Add(tag:Header:PageNo, bytes)
                    Debug.Assert(action:IsOk)
                ENDIF
            NEXT
            SELF:Compress()
            SELF:Write()
            _tags:Clear()
            // sort by pageno.
            System.Array.Sort(aTags,  { x,y => x:Page - y:Page})
            _tags:AddRange(aTags)
            RETURN TRUE

        INTERNAL METHOD Add(oTag AS CdxTag) AS LOGIC
            SELF:_tags:Add(oTag)
            SELF:_WriteTags(_tags)
            RETURN TRUE
    END CLASS
END NAMESPACE
