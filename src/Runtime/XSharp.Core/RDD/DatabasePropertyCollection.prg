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
/// such as captions, descriptions etc.</summary>
/// <seealso cref="O:XSharp.VFP.Functions.DbGetProp" />
/// <seealso cref="O:XSharp.VFP.Functions.DbSetProp" />
/// <seealso cref="T:XSharp.RDD.DatabasePropertyType" />

[DebuggerDisplay("Properties: {Count}")];
CLASS XSharp.RDD.DatabasePropertyCollection INHERIT Dictionary<DatabasePropertyType, OBJECT>
    CONSTRUCTOR()
        SUPER()

    /// <summary>Add a property - value pair to the collection.</summary>
    /// <param name="key">Propery to store.</param>
    /// <param name="val">Value to store</param>
    NEW METHOD Add(key as DatabasePropertyType, val as OBJECT) AS VOID
        // Duplicate keys simply replace the existing value
        SELF[key] := val
        RETURN

    /// <summary>Get the value for a property or an empty value when the property has not been defined </summary>
    /// <param name="key">Property to look for.</param>
    /// <typeparam name="T">Expected return type for the value</typeparam>
    /// <returns>The value from the collection or an empty value of the right type.</returns>
    METHOD GetValue<T> (key as DatabasePropertyType) AS T
        if self:TryGetValue(key, out var result)
            return (T) result
        ENDIF
        RETURN default(T)

    /// <summary>Get the value for a property</summary>
    /// <param name="cProp">Property name to look for.</param>
    /// <returns>The value from the collection or an empty value of the right type.</returns>
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

    INTERNAL METHOD FillFromMemo(bProps as BYTE[]) AS VOID
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
                    if pos+len <= bProps:Length
                        VAR strValue := RuntimeState.WinEncoding:GetString(bProps, pos+7, len-8)
                        SELF:Add(dbProp, strValue)
                    endif
                OTHERWISE
                    if pos+len <= bProps:Length
                        VAR strValue := RuntimeState.WinEncoding:GetString(bProps, pos+7, len-8)
                        SELF:Add(dbProp, strValue)
                    endif
                END SWITCH
                pos += len
            ENDDO
        CATCH
            THROW
        END TRY

        INTERNAL METHOD SaveToMemo() AS BYTE[]
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
                    oWriter:Write(RuntimeState.WinEncoding:GetBytes(sData))

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

INTERNAL STATIC databasePropertyNames AS Dictionary<STRING, LONG>

INTERNAL STATIC METHOD GetDatabasePropertyNumber(propertyName as STRING) AS LONG
    IF databasePropertyNames == NULL
        databasePropertyNames := Dictionary<STRING, LONG>{StringComparer.OrdinalIgnoreCase}
        var values := System.Enum.GetValues(typeof(XSharp.RDD.DatabasePropertyType))
        FOREACH var enumvalue in values
            var name := System.Enum.GetName(typeof(XSharp.RDD.DatabasePropertyType), enumvalue)
            databasePropertyNames:Add(name, (LONG) enumvalue)
        NEXT
    ENDIF
    if databasePropertyNames:TryGetValue(propertyName, out var result)
        return result
    ENDIF
    RETURN -1


INTERNAL STATIC METHOD DatabasePropertyValType(nType as LONG) AS STRING
    SWITCH  (DatabasePropertyType) nType
    CASE DatabasePropertyType.Comment
    CASE DatabasePropertyType.ConnectString
    CASE DatabasePropertyType.Database
    CASE DatabasePropertyType.DataSource
    CASE DatabasePropertyType.Password
    CASE DatabasePropertyType.UserId
    CASE DatabasePropertyType.DBCEventFileName
    CASE DatabasePropertyType.Caption
    CASE DatabasePropertyType.DisplayClass
    CASE DatabasePropertyType.DisplayClassLibrary
    CASE DatabasePropertyType.InputMask
    CASE DatabasePropertyType.Format
    CASE DatabasePropertyType.RuleExpression
    CASE DatabasePropertyType.RuleText
    CASE DatabasePropertyType.DataType
    CASE DatabasePropertyType.DefaultValue
    CASE DatabasePropertyType.UpdateName
    CASE DatabasePropertyType.Path
    CASE DatabasePropertyType.InsertTrigger
    CASE DatabasePropertyType.UpdateTrigger
    CASE DatabasePropertyType.DeleteTrigger
    CASE DatabasePropertyType.PrimaryKey
    CASE DatabasePropertyType.ConnectName
    CASE DatabasePropertyType.ParameterList
    CASE DatabasePropertyType.SQL
    CASE DatabasePropertyType.Tables
        RETURN "C"
    CASE DatabasePropertyType.ConnectTimeout
    CASE DatabasePropertyType.DispLogin
    CASE DatabasePropertyType.IdleTimeout
    CASE DatabasePropertyType.PacketSize
    CASE DatabasePropertyType.QueryTimeOut
    CASE DatabasePropertyType.Transactions
    CASE DatabasePropertyType.WaitTime
    CASE DatabasePropertyType.Version
    CASE DatabasePropertyType.BatchUpdateCount
    CASE DatabasePropertyType.FetchSize
    CASE DatabasePropertyType.MaxRecords
    CASE DatabasePropertyType.SourceType
    CASE DatabasePropertyType.UpdateType
    CASE DatabasePropertyType.UseMemoSize
    CASE DatabasePropertyType.WhereType
    CASE DatabasePropertyType.OfflineRecs
    CASE DatabasePropertyType.OfflineRemRecs
        RETURN "N"
    CASE DatabasePropertyType.Asynchronous
    CASE DatabasePropertyType.BatchMode
    CASE DatabasePropertyType.DisconnectRollback
    CASE DatabasePropertyType.DispWarnings
    CASE DatabasePropertyType.DBCEvents
    CASE DatabasePropertyType.KeyField
    CASE DatabasePropertyType.Updatable
    CASE DatabasePropertyType.AllowSimultaneousFetch
    CASE DatabasePropertyType.CompareMemo
    CASE DatabasePropertyType.FetchAsNeeded
    CASE DatabasePropertyType.FetchMemo
    CASE DatabasePropertyType.Prepared
    CASE DatabasePropertyType.SendUpdates
    CASE DatabasePropertyType.ShareConnection
    CASE DatabasePropertyType.IsUnique
        RETURN "L"

    // we have added this one
    CASE DatabasePropertyType.ColumnName
        RETURN "C"
    // internal for foxpro
    CASE DatabasePropertyType.Class
        RETURN "C"
    // no idea
    CASE DatabasePropertyType.TimeStamp
        RETURN "L"
    // Unknown numbers for now
    END SWITCH
    RETURN "C"
    STATIC METHOD IsValidProperty(cProp as STRING) AS LOGIC
        RETURN GetDatabasePropertyNumber(cProp) > 0

END CLASS
