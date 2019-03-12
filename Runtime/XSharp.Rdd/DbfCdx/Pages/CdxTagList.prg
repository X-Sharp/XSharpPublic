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
	INTERNAL CLASS CdxTagList INHERIT CdxLeafPage
        PROTECTED _tags AS List<CdxTag>
        
	    PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , page AS CdxPage, keyLen AS WORD)
            SUPER(bag  , page:PageNo, page:Buffer, keyLen)

        INTERNAL METHOD ReadTags() AS List<CdxTag>
            _tags := List<CdxTag>{}
            Debug.Assert (SELF:PageType:HasFlag(CdxPageType.Leaf) .and. SELF:PageType:HasFlag(CdxPageType.Root))
            FOR VAR nI := 0 TO SELF:NumKeys-1
                LOCAL nRecno    := SELF:GetRecno(nI) AS Int32
                LOCAL bName     := SELF:GetKey(nI)  AS BYTE[]
                LOCAL cName     := System.Text.Encoding.ASCII:GetString( bName, 0, bName:Length) AS STRING
                cName           := cName:TrimEnd(<CHAR>{'\0'})
                VAR tag         := CdxTag{_bag,  nRecno, cName:Trim()}
                _tags:Add(tag)
            NEXT
            // default sort for tags in an orderbag is on pageno. 
            _tags:Sort( { tagX, tagY => tagX:Page - tagY:Page} ) 
            RETURN _tags

        PROPERTY Tags AS IList<cdxTag> GET _tags

        INTERNAL VIRTUAL METHOD Initialize(keyLength AS WORD) AS VOID
            SUPER:Initialize(keyLength)
            _tags := List<CdxTag>{}
            SELF:PageType := CdxPageType.Leaf + CdxPageType.Root

        METHOD Remove(oTag AS CdxTag) AS LOGIC
            LOCAL found := FALSE AS LOGIC
            IF _Tags:Contains(oTag)
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
                System.Array.Sort(aTags,  { x,y => IIF (x:OrderName < y:Ordername , -1, 1)})
            ENDIF
            SELF:Initialize(KeyLength)
            FOREACH VAR tag IN aTags
                VAR bytes := BYTE[]{ keyLength}
                VAR name := tag:OrderName
				// Be sure to fill the Buffer with 0
				MemSet( bytes, 0, keyLength, 0)
				System.Text.Encoding.ASCII:GetBytes( name, 0, Math.Min(keyLength,name:Length), bytes, 0)
				_hot := TRUE

                SELF:Add(tag:Header:PageNo, bytes)
            NEXT
            SELF:Write()
            _tags:Clear()
            // sort by pageno.
            System.Array.Sort(aTags,  { x,y => x:Page - y:Page})
            _tags:AddRange(aTags)
            RETURN TRUE

        METHOD Add(oTag AS CdxTag) AS LOGIC
            SELF:_Tags:Add(oTag)
            SELF:_WriteTags(_Tags)
            RETURN TRUE
    END CLASS
END NAMESPACE 
