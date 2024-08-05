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

    CLASS Collection INHERIT VFP.Abstract IMPLEMENTS  IEnumerable
        PRIVATE aItems AS List<OBJECT>
        PRIVATE aDict  AS SortedList<STRING, OBJECT>
        PRIVATE nKeySort := CollectionSort.IndexAscending AS CollectionSort
        PRIVATE PROPERTY Initialized as LOGIC GET aItems != NULL_OBJECT .OR. aDict != NULL_OBJECT
        PROPERTY KeySort AS LONG
            GET
                RETURN nKeySort
            END GET
            SET
                IF VALUE >= 0 .AND. VALUE <= 3
                    nKeySort := (CollectionSort) VALUE
                ELSE
                    THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_INVALIDKEYSORT)}
                ENDIF

            END SET
        END PROPERTY


        CONSTRUCTOR()
            SUPER()

        PROTECTED OVERRIDE METHOD _InitProperties() AS VOID
            SUPER:_InitProperties()
            RETURN

        [Obsolete("Use Add() instead")] ;
        METHOD AddObject(oValue AS OBJECT, cKey := "" AS STRING, oBefore := NIL AS USUAL, oAfter:= NIL AS USUAL) AS LOGIC
            RETURN SELF:Add(oValue, cKey, oBefore, oAfter)

        METHOD Add(oValue AS OBJECT, cKey := "" AS STRING, oBefore := NIL AS USUAL, oAfter:= NIL AS USUAL) AS LOGIC
            LOCAL nPos      AS LONG
            IF !Initialized
                IF !String.IsNullOrEmpty(cKey)
                    aDict   := SortedList<STRING, OBJECT>{}
                    aDict:Add(cKey, oValue)
                ELSE
                    aItems := List<OBJECT>{}
                    aItems:Add(oValue)
                ENDIF
            ELSE
                IF aDict != NULL_OBJECT
                    IF String.IsNullOrEmpty(cKey)
                        // Error
                        THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_MISSING_KEY)}
                    ELSE
                        // The dictionary is sorted, so forget about before or after
                        IF aDict:ContainsKey(cKey)
                            THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_DUPLICATE_KEY, cKey)}
                        ELSE
                            aDict:Add(cKey, oValue)
                        ENDIF
                    ENDIF
                ELSE
                    IF !String.IsNullOrEmpty(cKey)
                        THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_NO_KEY_ALLOWED)}
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
                             THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_INVALID_BEFORE_AFTER)}
                        END CASE
                    ENDIF
                ENDIF
            ENDIF
    		RETURN TRUE
    	END METHOD

    PROPERTY Count AS LONG
        GET
            IF SELF:Initialized
			    IF SELF:aItems != NULL
				    RETURN aItems:Count
			    ELSE
				    RETURN aDict:Count
                ENDIF
            ENDIF
            RETURN 0
		END GET
    END PROPERTY

    METHOD Item(oKey AS USUAL) AS OBJECT
        LOCAL nPos AS LONG
        LOCAL cKey AS STRING
        IF !SELF:Initialized
            THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_MEMBER_NOT_FOUND,"Item",oKey)}
        ENDIF
        IF IsNumeric(oKey)
            nPos := (LONG) oKey
            IF nPos > 0 .AND. nPos <= aItems:Count
                RETURN aItems[nPos-1]
            ELSE
                THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_INCORRECT_INDEX)}
            ENDIF
        ELSEIF IsString(oKey)
            IF aDict != NULL
                cKey := (STRING) oKey
                IF aDict:ContainsKey(cKey)
                    RETURN aDict[cKey]
                ELSE
                    THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_MEMBER_NOT_FOUND, "Key", "'"+cKey+"'")}
                ENDIF
            ELSE
                THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_NOT_INDEXED_BY_KEY)}
            ENDIF
        ELSE
            THROW Exception{__VfpStr(VFPErrors.VFP_COLLECTION_INCORRECT_KEYTYPE)}
        ENDIF

    PUBLIC METHOD IEnumerable.GetEnumerator() AS IEnumerator
        IF aDict != NULL
            SWITCH SELF:nKeySort
            CASE CollectionSort.IndexAscending
                VAR aTemp := SortedList<OBJECT, OBJECT>{ }
                FOREACH VAR oItem IN SELF:aDict
                    aTemp:Add(oItem:Value, oItem:Value)
                NEXT
                RETURN CollectionEnumerator<OBJECT>{aTemp:GetEnumerator()}

            CASE CollectionSort.IndexDescending
                VAR aTemp := SortedList<OBJECT, OBJECT>{ ReversedComparer<OBJECT>{}}
                FOREACH VAR oItem IN SELF:aDict
                    aTemp:Add(oItem:Value, oItem:Value)
                NEXT
                RETURN CollectionEnumerator<OBJECT>{aTemp:GetEnumerator()}

            CASE CollectionSort.KeyAscending
                RETURN CollectionEnumerator<STRING>{aDict:GetEnumerator()}
            CASE CollectionSort.KeyDescending
                VAR aTemp := SortedList<STRING, OBJECT>{ ReversedComparer<STRING>{}}
                FOREACH VAR oItem IN SELF:aDict
                    aTemp:Add(oItem:Key, oItem:Value)
                NEXT
                RETURN CollectionEnumerator<STRING>{aTemp:GetEnumerator()}
            END SWITCH
        ELSE
            SWITCH SELF:nKeySort
            CASE CollectionSort.IndexAscending
                RETURN aItems:GetEnumerator()
            CASE CollectionSort.IndexDescending
                VAR aTemp := List<OBJECT>{}
                aTemp:AddRange(aItems)
                aTemp:Reverse()
                RETURN aTemp:GetEnumerator()
            CASE CollectionSort.KeyAscending
                VAR aTemp := List<Object>{ }
                FOREACH VAR oItem IN SELF:aItems
                    aTemp:Add(oItem)
                NEXT
                RETURN aTemp:GetEnumerator()
            CASE CollectionSort.KeyDescending
                VAR aTemp := List<Object>{}
                FOREACH VAR oItem IN SELF:aItems
                    aTemp:Add(oItem)
                NEXT
                aTemp:Reverse()
                RETURN aTemp:GetEnumerator()
            END SWITCH
        ENDIF
        RETURN NULL

    PUBLIC METHOD GetKey(uKey AS USUAL) AS OBJECT
        LOCAL nPos AS LONG
        LOCAL oKey AS OBJECT
        IF ! SELF:Initialized
            RETURN 0
        ENDIF
        oKey := uKey
        IF aDict != NULL_OBJECT
            VAR nHash := oKey:GetHashCode()
            FOREACH VAR oPair IN aDict
                VAR oItem := oPair:Value
                IF oItem:GetHashCode() == nHash .AND. Object.Equals(oItem, oKey)
                    RETURN oPair:Key
                ENDIF
            NEXT
        ELSEIF IsNumeric(uKey)
            nPos := -1
            VAR nHash := oKey:GetHashCode()
            FOREACH VAR oItem IN aItems
                nPos++
                IF oItem:GetHashCode() == nHash .AND. Object.Equals(oItem, oKey)
                    RETURN nPos
                ENDIF
            NEXT
        ENDIF
        RETURN 0


    INTERNAL CLASS CollectionEnumerator<T>  IMPLEMENTS  IEnumerator
        PRIVATE oEnum AS IEnumerator<KeyValuePair<T, OBJECT>>
        CONSTRUCTOR(oEnumerator AS IEnumerator<KeyValuePair<T, OBJECT> >)
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
    INTERNAL CLASS ReversedComparer<T> IMPLEMENTS IComparer<T>
        PRIVATE oCompare AS IComparer<T>
        CONSTRUCTOR()
            oCompare := System.Collections.Generic.Comparer<T>.Default
        METHOD Compare(x AS T, y AS T) AS LONG
            RETURN oCompare:Compare(x,y) * -1
    END CLASS

    INTERNAL ENUM CollectionSort
        MEMBER IndexAscending := 0
        MEMBER IndexDescending := 1
        MEMBER KeyAscending := 2
        MEMBER KeyDescending := 3
    END ENUM


    END CLASS
END NAMESPACE
