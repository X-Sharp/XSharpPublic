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
        PRIVATE aItems as List<Object>
        PRIVATE aDict  AS SortedList<String, object>
        private initialized as LOGIC
        
        PROPERTY KeySort as LONG AUTO
        
        CONSTRUCTOR()
            initialized := FALSE
            aItems      := List<Object>{}
            KeySort     := 1
            
        METHOD AddObject(oValue as object, cKey := NULL_STRING as STRING, oBefore := NIL as USUAL, oAfter:= NIL as USUAL) AS LOGIC
            LOCAL nPos      as LONG
            if Initialized
            if aDict != NULL_OBJECT
                IF String.IsNullOrEmpty(cKey)
                    // Error
                    Throw Exception{"Collection is indexed on key, key parameter should be passed"}
                ELSE
                    // The dictionary is sorted, so forget about before or after
                    if aDict:ContainsKey(cKey)
                        Throw Exception{"Collection already contains an item with key '"+cKey+"'"}
                    else
                        aDict:Add(cKey, oValue)
                    endif
                endif
            else
                IF !String.IsNullOrEmpty(cKey)
                    Throw Exception{"Collection is not indexed on key, key parameter should NOT be passed"}
                ELSE
                    DO CASE
                    CASE IsNil(oBefore) .and. IsNil(oAfter)
                        aItems:Add(oValue)
                    CASE IsNumeric(oBefore) .and. IsNil(oAfter)
                            nPos := (LONG) oBefore
                            IF nPos <= aItems:Count
                                aItems:Insert(nPos-1, oValue)
                            ELSE
                                aItems:Add(oValue)
                        ENDIF
                    CASE IsNil(oBefore) .and. IsNumeric(oAfter)
                            nPos := (LONG) oAfter
                            IF nPos < aItems:Count
                                aItems:Insert(nPos+1, oValue)
                            ELSE
                                aItems:Add(oValue)
                        ENDIF
                    OTHERWISE
                            Throw Exception{"When you pass a before or after parameter you can only pass one argument and it has to be numeric"}                        
                    END CASE
                ENDIF
            endif
        else
            Initialized := TRUE
            IF ! String.IsNullOrEmpty(cKey)
                aDict   := SortedList<string, object>{}
                SELF:KeySort := 2
                aDict:Add(cKey, oValue)
            ENDIF
            endif
    RETURN TRUE
    PROPERTY Count as LONG GET aItems:Count
    
    METHOD Item(oKey as USUAL) AS OBJECT
        LOCAL nPos as LONG
        LOCAL cKey as STRING
        IF IsNumeric(oKey)
            nPos := (LONG) oKey
            if nPos > 0 .and. nPos <= aItems:Count
                return aItems[nPos-1]
            else
                Throw Exception{"Index must be between 1 and the number of items in the collection"}
            endif
        ELSEIF IsString(oKey)
            if aDict != NULL
                cKey := (String) oKey
                if aDict:ContainsKey(cKey)
                    return aDict[cKey]
                ELSE
                    Throw Exception{"Index '"+cKey+"' not found in the collection"}
                endif
            else
                Throw Exception{"Collection is not indexed on key"}
            endif
        ELSE
            Throw Exception{"Index parameter must be numeric or string"}
        ENDIF
        
    PUBLIC METHOD IEnumerable.GetEnumerator() AS IEnumerator
        var sort := (CollectionSort) SELF:KeySort
        if aDict != NULL
            SWITCH sort
            CASE CollectionSort.IndexAscending
            CASE CollectionSort.IndexDescending
                Throw Exception{"Keysort on Index is not supported for collections ordered by Key"}
            CASE CollectionSort.KeyAscending
                RETURN CollectionEnumerator{aDict:GetEnumerator()}
            CASE CollectionSort.KeyDescending
                var aTemp := SortedList<String, object>{ ReversedComparer<String>{}}
                Foreach var oItem in SELF:aDict
                    aTemp:Add(oItem:Key, oItem:Value)
                NEXT
                RETURN CollectionEnumerator{aTemp:GetEnumerator()}
            END SWITCH
        ELSE
            SWITCH sort
            CASE CollectionSort.IndexAscending
                RETURN aItems:GetEnumerator()
            CASE CollectionSort.IndexDescending
                var aTemp := List<Object>{}
                aTemp:AddRange(aItems)
                aTemp:Reverse()
                return aTemp:GetEnumerator()
            CASE CollectionSort.KeyAscending
            CASE CollectionSort.KeyDescending
                Throw Exception{"Keysort on Key is not supported for collections order by index"}
            END SWITCH    
        ENDIF
        RETURN NULL
        
    PUBLIC METHOD GetKey(uKey as USUAL) AS OBJECT
        LOCAL nPos as LONG
        LOCAL oKey as OBJECT
        oKey := uKey
        IF aDict != NULL_OBJECT
            var nHash := oKey:GetHashCode()
            FOREACH VAR oPair in aDict
                var oItem := oPair:Value
                if oItem:GetHashCode() == nHash .and. Object.Equals(oItem, oKey)
                    return oPair:Key
                endif
            NEXT
        ELSE
            nPos := -1
            var nHash := oKey:GetHashCode()
            FOREACH VAR oItem in aItems
                nPos++
                if oItem:GetHashCode() == nHash .and. Object.Equals(oItem, oKey)
                    return nPos
                endif
            NEXT
        ENDIF
        Throw Exception{"Item not found in the collection"}
        
        
    Class CollectionEnumerator  IMPLEMENTS  IEnumerator
        PRIVATE oEnum as IEnumerator<KeyValuePair<String, object>>
        CONSTRUCTOR(oEnumerator as IEnumerator<KeyValuePair<String, object> >)
            SELF:oEnum :=oEnumerator
            RETURN
        PROPERTY Current as OBJECT
        GET
            var oValue := oEnum:Current
            return oValue:Value
            END GET 
    END PROPERTY
        METHOD MoveNext as LOGIC
            RETURN oEnum:MoveNext()
        METHOD Reset AS VOID
            oEnum:Reset()
            RETURN 
    END CLASS
    CLASS ReversedComparer<T> IMPLEMENTS IComparer<T>
        PRIVATE oCompare as IComparer<T>
        CONSTRUCTOR()
            oCompare := System.Collections.Generic.Comparer<T>.Default
        METHOD Compare(x as T, y as T) AS LONG
            return oCompare:Compare(x,y) * -1
    END CLASS
    
ENUM CollectionSort
    MEMBER IndexAscending := 0
    MEMBER IndexDescending := 1
    MEMBER KeyAscending := 2
    MEMBER KeyDescending := 3
    END ENUM
    
    
    END CLASS
END NAMESPACE 
