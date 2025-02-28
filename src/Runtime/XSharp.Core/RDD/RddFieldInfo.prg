﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD
// The classes below are simple. No properties, but all public fields.

BEGIN NAMESPACE XSharp.RDD.Support

/// <summary>Helper class for the RDD system to store field information</summary>
CLASS RddFieldInfo
#region static
    static private TypeMap as Dictionary<STRING, DbFieldType>
    static constructor()
        TypeMap := Dictionary<STRING, DbFieldType>{StringComparer.OrdinalIgnoreCase}
        TypeMap:Add("blob", DbFieldType.Blob )
        TypeMap:Add("char", DbFieldType.Character )
        TypeMap:Add("character", DbFieldType.Character )
        TypeMap:Add("currency", DbFieldType.Currency )
        TypeMap:Add("date", DbFieldType.Date )
        TypeMap:Add("datetime", DbFieldType.DateTime )
        TypeMap:Add("double", DbFieldType.Double )
        TypeMap:Add("general", DbFieldType.General )
        TypeMap:Add("int", DbFieldType.Integer )
        TypeMap:Add("integer", DbFieldType.Integer )
        TypeMap:Add("logical", DbFieldType.Logic )
        TypeMap:Add("memo", DbFieldType.Memo )
        TypeMap:Add("num", DbFieldType.Integer )
        TypeMap:Add("numeric", DbFieldType.Number )
        TypeMap:Add("float", DbFieldType.Float )
        TypeMap:Add("varbinary", DbFieldType.VarBinary )
        TypeMap:Add("varchar", DbFieldType.VarChar )
        RETURN
    STATIC METHOD FindType(cType as STRING) as DbFieldType
        cType := cType:ToUpper()
        if cType:Length == 1
            RETURN (DbFieldType) (byte) cType[0]
        elseif TypeMap:TryGetValue(cType, out var dbType)
            RETURN dbType
        endif
        RETURN DbFieldType.Unknown
#endregion

    /// <summary>Name, normally max 10 characters</summary>
    PUBLIC Name 		AS STRING
    /// <summary>Field Type</summary>
    PUBLIC FieldType 	AS DbFieldType
    /// <summary>Physical length in the table</summary>
    PUBLIC Length 		AS LONG
    /// <summary>Decimal positions</summary>
    PUBLIC Decimals 	AS LONG
    /// <summary>Alternative name, no length limit. This is the Caption for VFP fields</summary>
    PUBLIC Alias 		AS STRING
    /// <summary>Flags, such as Nullable, AutoIncrement, Binary etc.</summary>
    PUBLIC Flags        AS DBFFieldFlags
    /// <summary>Offset in the record buffer for DBF fields.</summary>
    PUBLIC Offset       AS LONG
    /// <summary>1 based Ordinal position in the RDD.</summary>
    PUBLIC Ordinal      AS LONG
    /// <summary>Next key for autoincrement columns.</summary>
    PUBLIC NextValue    AS LONG
    /// <summary>Step value for autoincrement columns.</summary>
    PUBLIC StepValue    AS LONG

    /// <summary>Dynamic list of optional properties, such as Caption, Description.</summary>
    /// <remarks>These properties are used to store VFP specific properties and are read from the DBC file.</remarks>
    PUBLIC PROPERTY Properties AS DatabasePropertyCollection
        GET
            if _lazyProperties == NULL
                _lazyProperties := DatabasePropertyCollection{}
            endif
            return _lazyProperties
        END GET
        SET
            _lazyProperties := value
        END SET
    END PROPERTY
    PRIVATE _lazyProperties  := NULL as DatabasePropertyCollection
    /// <summary>Does the field have properties.</summary>
    property HasProperties as logic get _lazyProperties != null
    /// <summary>FieldType as a string</summary>
    property FieldTypeFlags as string
        get
            if self:Flags == DBFFieldFlags.None
                return ((char) self:FieldType):ToString()
            endif
            var sb := System.Text.StringBuilder{}
            sb:Append((char) self:FieldType)
            sb:Append(':')
            if self:Flags:HasFlag(DBFFieldFlags.Nullable)
                sb:Append('0')
            endif
            if self:Flags:HasFlag(DBFFieldFlags.Binary)
                sb:Append('B')
            endif
            if self:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                sb:Append('+')
            endif
            if self:Flags:HasFlag(DBFFieldFlags.Compressed)
                sb:Append('Z')
            endif
            if self:Flags:HasFlag(DBFFieldFlags.Encrypted)
                sb:Append('E')
            endif
            if self:Flags:HasFlag(DBFFieldFlags.Unicode)
                sb:Append('U')
            endif
            return sb:ToString()
        end get

    end property
    /// <summary>Construct a RddFieldInfo object.</summary>
    /// <param name="sName">Name</param>
    /// <param name="sType">Type, may also contain flags in the form of a colon follwed by N,0,B,+,Z,E,U</param>
    /// <param name="nLength">Length 'DBF style', so length in Buffer</param>
    /// <param name="nDecimals">Number of decimals. </param>
    /// <param name="nOffSet">Offset in record buffer (optional).</param>
    CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG, nOffSet := -1 AS LONG)
        Name 		:= sName
        Length 		:= nLength
        Decimals 	:= nDecimals
        Flags       := DBFFieldFlags.None
        FieldType := (DbFieldType) Char.ToUpper(sType[0])
        if FieldType:IsBinary()
            Flags |= DBFFieldFlags.Binary
        ENDIF
        IF sType:IndexOf(":") >= 0
            sType := sType:Substring(1)
            FOREACH VAR cChar IN sType
                SWITCH cChar
                CASE 'N'
                CASE '0'
                    Flags |= DBFFieldFlags.Nullable
                CASE 'B'
                    Flags |= DBFFieldFlags.Binary
                CASE '+'
                    Flags |= DBFFieldFlags.AutoIncrement
                CASE 'Z'
                    Flags |= DBFFieldFlags.Compressed
                CASE 'E'
                    Flags |= DBFFieldFlags.Encrypted
                CASE 'U'
                    Flags |= DBFFieldFlags.Unicode
                END SWITCH
            NEXT
        ENDIF
        IF String.Compare(Name, _NULLFLAGS, TRUE) == 0
            Flags |= DBFFieldFlags.System
            Flags |= DBFFieldFlags.Binary
        ENDIF
        Alias       := sName
        SELF:Offset := nOffSet
        SELF:Validate()
        RETURN
    /// <summary>Construct a RddFieldInfo object.</summary>
    /// <param name="sName">Name</param>
    /// <param name="nType">Type</param>
    /// <param name="nLength">Length 'DBF style', so length in Buffer</param>
    /// <param name="nDecimals">Number of decimals. </param>
    /// <param name="nOffSet">Offset in record buffer (optional)</param>
    /// <param name="nFlags">Flags (optional)</param>
    CONSTRUCTOR(sName AS STRING, nType AS DbFieldType, nLength AS LONG, nDecimals AS LONG, nOffSet := -1 AS LONG, nFlags := DBFFieldFlags.None AS DBFFieldFlags)
        self:Name 		:= sName
        SELF:FieldType 	:= nType
        SELF:Length 	:= nLength
        SELF:Decimals 	:= nDecimals
        SELF:Alias      := sName
        SELF:Offset     := nOffSet
        SELF:Flags      := nFlags
        SELF:Validate()
        RETURN

    /// <summary>Construct a RddFieldInfo object.</summary>
    /// <param name="oInfo">Object to copy values from.</param>
    CONSTRUCTOR(oInfo AS RddFieldInfo)
        SUPER()
        SELF:CopyValues(oInfo)
        SELF:Validate()

    /// <summary>Copy values from one object to another.</summary>
    /// <param name="oInfo">Object to copy values to.</param>
    /// <remarks>Only the fields will be copied.</remarks>
    METHOD CopyValues(oInfo AS RddFieldInfo) AS VOID
        VAR oFields    := typeof(RddFieldInfo):GetFields()
        foreach var oField in oFields
            oField:SetValue(SELF, oField:GetValue(oInfo))
        NEXT
        IF SELF:FieldType:HasDecimals()  .OR. SELF:FieldType == DbFieldType.Character  // Support for char fields > 255 characters
            SELF:Decimals 	:= oInfo:Decimals
        ENDIF

        foreach var prop in oInfo:Properties
            SELF:Properties:Add(prop:Key, prop:Value)
        next
        SELF:Name := SELF:Name:ToUpper()
        return
    /// <summary>Return the blank (non null) value of the column.</summary>
    METHOD BlankValue() AS OBJECT
        SWITCH SELF:FieldType
        CASE DbFieldType.Character
        case DbFieldType.VarChar
        CASE DbFieldType.Memo
            RETURN String.Empty
        CASE DbFieldType.Date
        CASE DbFieldType.DateTime
            RETURN DateTime.MinValue
        CASE DbFieldType.Number
        CASE DbFieldType.Integer
            RETURN 0
        CASE DbFieldType.Logic
            RETURN FALSE
        CASE DbFieldType.Blob
        CASE DbFieldType.General
        CASE DbFieldType.Picture
        CASE DbFieldType.VarBinary
            RETURN <BYTE>{}
        CASE DbFieldType.Currency
            RETURN 0.0m
        CASE DbFieldType.Double
        CASE DbFieldType.Float
            RETURN 0.0
        END SWITCH
        RETURN NULL
    /// <summary>Clone a RddFieldInfo object.</summary>
    METHOD Clone() AS RddFieldInfo
        VAR info := (RddFieldInfo) SELF:MemberwiseClone()
        RETURN info

    /// <summary>Check if two fields match in type, length and decimals.</summary>
    METHOD SameType(oFld AS RddFieldInfo) AS LOGIC
        RETURN SELF:FieldType == oFld:FieldType .AND. SELF:Length == oFld:Length .AND. SELF:Decimals == oFld:Decimals

    /// <summary>Validate combinations of type, length and decimals.</summary>
    VIRTUAL METHOD Validate() AS LOGIC
        SWITCH SELF:FieldType
        CASE DbFieldType.Date
            SELF:Length := 8
        CASE DbFieldType.DateTime
            SELF:Length := 8
        CASE DbFieldType.Logic
            SELF:Length := 1
        CASE DbFieldType.Integer
            SELF:Length := 4
        CASE DbFieldType.Currency
            SELF:Length   := 8
            SELF:Decimals := 4
        CASE DbFieldType.Double
            SELF:Length := 8
        CASE DbFieldType.Memo
            IF SELF:Length != 10 .and. SELF:Length != 4
                SELF:Length := 10
            ENDIF
        CASE DbFieldType.Blob
        CASE DbFieldType.General
            SELF:Length := 4
        END SWITCH
        IF !SELF:FieldType:HasDecimals()
            SELF:Decimals := 0
        ENDIF
        RETURN TRUE

    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Name+" ('"+SELF:FieldTypeStr+"',"+SELF:Length:ToString()+","+SELF:Decimals:ToString()+","+SELF:Flags:ToString("G")+")"

    /// <summary>Field type as 1 character string.</summary>
    PROPERTY FieldTypeStr       AS STRING GET ((CHAR) SELF:FieldType):ToString()
    /// <summary>Is it a memo ?</summary>
    PROPERTY IsMemo             AS LOGIC GET SELF:FieldType:IsMemo()
    /// <summary>Is it binary ?</summary>
    PROPERTY IsBinary           AS LOGIC GET SELF:FieldType:IsBinary() .OR. SELF:Flags:HasFlag(DBFFieldFlags.Binary)
    /// <summary>Is it nullable ?</summary>
    PROPERTY IsNullable         AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Nullable)
    /// <summary>Is it an autoincrement ?</summary>
    PROPERTY IsAutoIncrement    AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
    /// <summary>Is it a standard Dbase 3 field (CDLMN) ?</summary>
    PROPERTY IsStandard         AS LOGIC GET SELF:FieldType:IsStandard()
    /// <summary>Is it a VFP extended field ?</summary>
    PROPERTY IsVfp              AS LOGIC GET SELF:FieldType:IsVfp()
    /// <summary>Is it a variable length field ?</summary>
    PROPERTY IsVarLength        AS LOGIC GET SELF:FieldType:IsVarLength()
    /// <summary>Is it a unicode text ?</summary>
    PROPERTY IsUnicode          AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Unicode)
    /// <summary>Is it an encryped field (not implemented yet)?</summary>
    PROPERTY IsEncrypted        AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Encrypted)
    /// <summary>Is it a  compressed field (not implemented yet) ?</summary>
    PROPERTY IsCompressed       AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Compressed)
    /// <summary>Can the field be sorted?</summary>
    PROPERTY CanSort            AS LOGIC
        GET
            SWITCH SELF:FieldType
            CASE DbFieldType.Character
            CASE DbFieldType.Date
            CASE DbFieldType.DateTime
            CASE DbFieldType.Number
            CASE DbFieldType.Logic
            CASE DbFieldType.Currency
            CASE DbFieldType.Integer
            CASE DbFieldType.Float
            CASE DbFieldType.Double
                RETURN TRUE
            OTHERWISE
                IF SELF:IsBinary .OR. SELF:IsVarLength
                    RETURN FALSE
                ENDIF
            END SWITCH
            RETURN FALSE

        END GET
    END PROPERTY

    PRIVATE METHOD _GetPropertyValue<T> (Key as DatabasePropertyType) AS T
        if self:_lazyProperties != NULL
            return SELF:_lazyProperties:GetValue<T>(Key)
        endif
        return default(T)

    // These dynamic properties can be used to automatically generate databrowsers etc.
    /// <summary>Dynamic property for the Caption. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY Caption      AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Caption) DEFAULT SELF:Name;
        SET SELF:Properties:Add(DatabasePropertyType.Caption, value)

    /// <summary>Dynamic property for the Description. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY Description  AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Comment)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.Comment, value)

    /// <summary>Dynamic property for the InputMask. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY InputMask    AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.InputMask)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.InputMask, value)

    /// <summary>Dynamic property for the Format. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY Format       AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Format)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.Format, value)

    /// <summary>Dynamic property for the ColumnName. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY ColumnName   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.ColumnName)  DEFAULT SELF:Name;
        SET SELF:Properties:Add(DatabasePropertyType.ColumnName, value)

    /// <summary>Dynamic property for the PrimaryKey. Read from the VFP properties collection.</summary>
    PROPERTY PrimaryKey   AS LOGIC ;
        GET SELF:_GetPropertyValue<LOGIC>(DatabasePropertyType.KeyField)   ;
        SET SELF:Properties:Add(DatabasePropertyType.KeyField, value)

    /// <summary>Dynamic property for the DefaultValue. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY DefaultValue   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.DefaultValue)  DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.DefaultValue, value)

    /// <summary>Dynamic property for the IsUnique. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY IsUnique   AS LOGIC ;
        GET SELF:_GetPropertyValue<LOGIC>(DatabasePropertyType.IsUnique)  ;
        SET SELF:Properties:Add(DatabasePropertyType.IsUnique, value)

    /// <summary>Dynamic property for the RuleExpression. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY RuleExpression   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.RuleExpression)    DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.RuleExpression, value)

    /// <summary>Dynamic property for the RuleText. Read from the VFP properties collection.</summary>
    /// <seealso cref="P:Properties"/>
    PROPERTY RuleText   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.RuleText)    DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.RuleText, value)

END CLASS

END NAMESPACE
