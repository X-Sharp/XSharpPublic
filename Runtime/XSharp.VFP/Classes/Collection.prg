//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Linq
USING System.Collections
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP

    CLASS Collection INHERIT Vfp.Abstract IMPLEMENTS  IEnumerable
        PRIVATE aItems AS List<OBJECT>
        PRIVATE aDict  AS SortedList<STRING, OBJECT>
        PRIVATE initialized AS LOGIC
        
        PROPERTY KeySort AS LONG AUTO
        
        CONSTRUCTOR()
            initialized := FALSE
            aItems      := List<OBJECT>{}
            KeySort     := 1
            
        METHOD AddObject(oValue AS OBJECT, cKey := "" AS STRING, oBefore := NIL AS USUAL, oAfter:= NIL AS USUAL) AS LOGIC
            LOCAL nPos      AS LONG
            IF Initialized
            IF aDict != NULL_OBJECT
                IF String.IsNullOrEmpty(cKey)
                    // Error
                    THROW Exception{"Collection is indexed on key, key parameter should be passed"}
                ELSE
                    // The dictionary is sorted, so forget about before or after
                    IF aDict:ContainsKey(cKey)
                        THROW Exception{"Collection already contains an item with key '"+cKey+"'"}
                    ELSE
                        aDict:Add(cKey, oValue)
                    ENDIF
                ENDIF
            ELSE
                IF !String.IsNullOrEmpty(cKey)
                    THROW Exception{"Collection is not indexed on key, key parameter should NOT be passed"}
                ELSE
                    DO CASE
                    CASE IsNil(oBefore) .AND. IsNil(oAfter)
                        aItems:Add(oValue)
                    CASE IsNumeric(oBefore) .AND. IsNil(oAfter)
                            nPos := (LONG) oBefore
                            IF nPos <= aItems:Count
                                aItems:Insert(nPos-1, oValue)
                            ELSE
                                aItems:Add(oValue)
                        ENDIF
                    CASE IsNil(oBefore) .AND. IsNumeric(oAfter)
                            nPos := (LONG) oAfter
                            IF nPos < aItems:Count
                                aItems:Insert(nPos+1, oValue)
                            ELSE
                                aItems:Add(oValue)
                        ENDIF
                    OTHERWISE
                            THROW Exception{"When you pass a before or after parameter you can only pass one argument and it has to be numeric"}                        
                    END CASE
                ENDIF
            ENDIF
        ELSE
            Initialized := TRUE
            IF ! String.IsNullOrEmpty(cKey)
                aDict   := SortedList<STRING, OBJECT>{}
                SELF:KeySort := 2
                aDict:Add(cKey, oValue)
            ENDIF
            ENDIF
    RETURN TRUE
    PROPERTY Count AS LONG GET aItems:Count
    
    METHOD Item(oKey AS USUAL) AS OBJECT
        LOCAL nPos AS LONG
        LOCAL cKey AS STRING
        IF IsNumeric(oKey)
            nPos := (LONG) oKey
            IF nPos > 0 .AND. nPos <= aItems:Count
                RETURN aItems[nPos-1]
            ELSE
                THROW Exception{"Index must be between 1 and the number of items in the collection"}
            ENDIF
        ELSEIF IsString(oKey)
            IF aDict != NULL
                cKey := (STRING) oKey
                IF aDict:ContainsKey(cKey)
                    RETURN aDict[cKey]
                ELSE
                    THROW Exception{"Index '"+cKey+"' not found in the collection"}
                ENDIF
            ELSE
                THROW Exception{"Collection is not indexed on key"}
            ENDIF
        ELSE
            THROW Exception{"Index parameter must be numeric or string"}
        ENDIF
        
    PUBLIC METHOD IEnumerable.GetEnumerator() AS IEnumerator
        VAR sort := (CollectionSort) SELF:KeySort
        IF aDict != NULL
            SWITCH sort
            CASE CollectionSort.IndexAscending
            CASE CollectionSort.IndexDescending
                THROW Exception{"Keysort on Index is not supported for collections ordered by Key"}
            CASE CollectionSort.KeyAscending
                RETURN CollectionEnumerator{aDict:GetEnumerator()}
            CASE CollectionSort.KeyDescending
                VAR aTemp := SortedList<STRING, OBJECT>{ ReversedComparer<STRING>{}}
                FOREACH VAR oItem IN SELF:aDict
                    aTemp:Add(oItem:Key, oItem:Value)
                NEXT
                RETURN CollectionEnumerator{aTemp:GetEnumerator()}
            END SWITCH
        ELSE
            SWITCH sort
            CASE CollectionSort.IndexAscending
                RETURN aItems:GetEnumerator()
            CASE CollectionSort.IndexDescending
                VAR aTemp := List<OBJECT>{}
                aTemp:AddRange(aItems)
                aTemp:Reverse()
                RETURN aTemp:GetEnumerator()
            CASE CollectionSort.KeyAscending
            CASE CollectionSort.KeyDescending
                THROW Exception{"Keysort on Key is not supported for collections order by index"}
            END SWITCH    
        ENDIF
        RETURN NULL
        
    PUBLIC METHOD GetKey(uKey AS USUAL) AS OBJECT
        LOCAL nPos AS LONG
        LOCAL oKey AS OBJECT
        oKey := uKey
        IF aDict != NULL_OBJECT
            VAR nHash := oKey:GetHashCode()
            FOREACH VAR oPair IN aDict
                VAR oItem := oPair:Value
                IF oItem:GetHashCode() == nHash .AND. Object.Equals(oItem, oKey)
                    RETURN oPair:Key
                ENDIF
            NEXT
        ELSE
            nPos := -1
            VAR nHash := oKey:GetHashCode()
            FOREACH VAR oItem IN aItems
                nPos++
                IF oItem:GetHashCode() == nHash .AND. Object.Equals(oItem, oKey)
                    RETURN nPos
                ENDIF
            NEXT
        ENDIF
        THROW Exception{"Item not found in the collection"}
        
        
    CLASS CollectionEnumerator  IMPLEMENTS  IEnumerator
        PRIVATE oEnum AS IEnumerator<KeyValuePair<STRING, OBJECT>>
        CONSTRUCTOR(oEnumerator AS IEnumerator<KeyValuePair<STRING, OBJECT> >)
            SELF:oEnum :=oEnumerator
            RETURN
        PROPERTY Current AS OBJECT
        GET
            VAR oValue := oEnum:Current
            RETURN oValue:Value
            END GET 
    END PROPERTY
        METHOD MoveNext AS LOGIC
            RETURN oEnum:MoveNext()
        METHOD Reset AS VOID
            oEnum:Reset()
            RETURN 
    END CLASS
    CLASS ReversedComparer<T> IMPLEMENTS IComparer<T>
        PRIVATE oCompare AS IComparer<T>
        CONSTRUCTOR()
            oCompare := System.Collections.Generic.Comparer<T>.Default
        METHOD Compare(x AS T, y AS T) AS LONG
            RETURN oCompare:Compare(x,y) * -1
    END CLASS
    
ENUM CollectionSort
    MEMBER IndexAscending := 0
    MEMBER IndexDescending := 1
    MEMBER KeyAscending := 2
    MEMBER KeyDescending := 3
    END ENUM
    
    
    END CLASS
END NAMESPACE 
