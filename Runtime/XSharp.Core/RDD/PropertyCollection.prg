//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING System.IO
USING STATIC XSharp.Conversions

/// <summary>This collection is used to store additional properties for fields and servers
/// such as captions, descriptions etc.<summary>
[DebuggerDisplay("Properties: {Count}")];
CLASS XSharp.RDD.PropertyCollection INHERIT Dictionary<DatabasePropertyType, OBJECT>
    CONSTRUCTOR()
        SUPER()

    NEW METHOD Add(key as DatabasePropertyType, val as OBJECT) AS VOID
        // Duplicate keys simply replace the existing value
        SELF[key] := val
        RETURN

    METHOD GetValue<T> (key as DatabasePropertyType) AS T
        IF SELF:ContainsKey(key)
            return (T) SELF[key]
        ENDIF
        RETURN default(T)
        
    METHOD GetValue(cProp as STRING) AS OBJECT
        var prop := GetDatabasePropertyNumber(cProp)
        if prop != 0
            var type := DatabasePropertyValType(prop)
            SWITCH type
                CASE "L"
                    return SELF:GetValue<LOGIC>((DatabasePropertyType)prop)
                CASE "C"
                    var result := SELF:GetValue<STRING>((DatabasePropertyType)prop)
                    if result == null
                        result := String.Empty
                    endif
                    return result
                CASE "N"
                    return SELF:GetValue<LONG>((DatabasePropertyType)prop)
            END SWITCH
        ENDIF
        RETURN NULL

    METHOD FillFromMemo(bProps as BYTE[]) AS VOID
        TRY
            SELF:Clear()
            VAR pos := 8
            DO WHILE pos < bProps:Length-10 // 4 for length, 4 for type, 2 for prop#
                VAR len     := BitConverter.ToInt32(bProps, pos)
                //VAR type := (Int) bProps[4]
                VAR prop    := BuffToShortFox(bProps, pos+5)
                var dbProp  := (DatabasePropertyType) prop
                var expectedType := DatabasePropertyValType(prop)
                SWITCH expectedType
                CASE "L"
                    var lValue   := bProps[pos+7] != 0
                    SELF:Add(dbProp, lValue)
                CASE "N"
                    local nValue as LONG
                    if len == 11
                        nValue   := BuffToLongFox(bProps, pos+7)
                    elseif len == 8
                        nValue := bProps[pos+7]
                    elseif len == 9
                        nValue   := BuffToShortFox(bProps, pos+7)
                    else
                        nValue := 0
                    endif
                    SELF:Add(dbProp, nValue)
                CASE "C"
                    VAR strValue := RuntimeState:WinEncoding:GetString(bProps, pos+7, len-8)
                    SELF:Add(dbProp, strValue)
                OTHERWISE
                    VAR strValue := RuntimeState:WinEncoding:GetString(bProps, pos+7, len-8)
                    SELF:Add(dbProp, strValue)
                END SWITCH                                
                pos += len
            ENDDO
        CATCH
            THROW
        END TRY

        METHOD SaveToMemo() AS BYTE[]
            // starts with normal 8 byte header containing 1 and length of byte []
            // then for each property:
            // 4 byte length
            // 2 bytes property number
            // data:
            // 1 byte for LOGIC
            // 1, 2 or 4 bytes for numbers
            // n bytes Ansi bytes for strings
            // problem is that when we start we don't know how much memory to allocate, so we should use a memory stream and write to that
            // and then at the end we know the size and we can the header for the block
            LOCAL oStream := MemoryStream{1024} as MemoryStream
            LOCAL oWriter := BinaryWriter{oStream} AS BinaryWriter
            LOCAL nTotalLen AS LONG
            LOCAL bTrue := 1 as BYTE
            LOCAL bFalse := 0 as BYTE
            oWriter:Write( SwapBytes(1L))
            oWriter:Write( (LONG) 0)
            nTotalLen := 0
            FOREACH var key in SELF:Keys
                LOCAL nType := (SHORT) key AS SHORT
                LOCAL nLen  := 7 AS LONG
                LOCAL oData := SELF[key] AS OBJECT
                IF oData IS STRING var sData
                    nLen  += sData:Length
                    oWriter:Write(nLen)
                    oWriter:Write(bTrue)
                    oWriter:Write(SwapBytes(nType))
                    oWriter:Write(RuntimeState:WinEncoding:GetBytes(sData))
                    
                ELSEIF oData IS LONG VAR liValue
                    nLen    += 4
                    oWriter:Write(nLen)
                    oWriter:Write(bTrue)
                    oWriter:Write(SwapBytes(nType))
                    oWriter:Write(SwapBytes(liValue))
                    
                ELSEIF oData IS SHORT VAR siData
                    nLen    += 2
                    oWriter:Write(nLen)
                    oWriter:Write(bTrue)
                    oWriter:Write(SwapBytes(nType))
                    oWriter:Write(SwapBytes(siData))
                    
                ELSEIF oData IS BYTE VAR bValue
                    nLen    += 1
                    oWriter:Write(nLen)
                    oWriter:Write(bTrue)
                    oWriter:Write(SwapBytes(nType))
                    oWriter:Write(bValue)
                    
                ELSEIF oData IS LOGIC VAR lValue
                    nLen    += 1
                    oWriter:Write(nLen)
                    oWriter:Write(bTrue)
                    oWriter:Write(SwapBytes(nType))
                    oWriter:Write(iif(lValue,bTrue,bFalse))
                ELSE
                    nLen := 0
                ENDIF
                nTotalLen += nLen
            NEXT
            oWriter:Flush()
            oWriter:Dispose()
            var result := oStream:ToArray()
            nTotalLen := SwapBytes(nTotalLen)
            System.Array.Copy(BitConverter.GetBytes(nTotalLen), 0, result, 4, 4)
            oStream:Close()
            RETURN result

            
END CLASS
