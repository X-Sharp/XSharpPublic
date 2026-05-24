//
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

/// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Name/*" />
    PUBLIC Name 		AS STRING
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.FieldType/*" />
    PUBLIC FieldType 	AS DbFieldType
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Length/*" />
    PUBLIC Length 		AS LONG
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Decimals/*" />
    PUBLIC Decimals 	AS LONG
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Alias/*" />
    PUBLIC Alias 		AS STRING
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Flags/*" />
    PUBLIC Flags        AS DBFFieldFlags
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Offset/*" />
    PUBLIC Offset       AS LONG
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Ordinal/*" />
    PUBLIC Ordinal      AS LONG
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.NextValue/*" />
    PUBLIC NextValue    AS LONG
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.StepValue/*" />
    PUBLIC StepValue    AS LONG

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Properties/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.HasProperties/*" />
    property HasProperties as logic get _lazyProperties != null
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.FieldTypeFlags/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.ctor/*" />
    CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG, nOffSet := -1 AS LONG)
        Name 		:= sName:Trim()
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.ctor_2/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.ctor_3/*" />
    CONSTRUCTOR(oInfo AS RddFieldInfo)
        SUPER()
        SELF:CopyValues(oInfo)
        SELF:Validate()

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.CopyValues/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.BlankValue/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Clone/*" />
    METHOD Clone() AS RddFieldInfo
        VAR info := (RddFieldInfo) SELF:MemberwiseClone()
        RETURN info

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.SameType/*" />
    METHOD SameType(oFld AS RddFieldInfo) AS LOGIC
        RETURN SELF:FieldType == oFld:FieldType .AND. SELF:Length == oFld:Length .AND. SELF:Decimals == oFld:Decimals

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Validate/*" />
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
        CASE DbFieldType.Character
            IF SELF:Decimals != 0
                SELF:Length := SELF:Decimals * 256 + SELF:Length
            ENDIF
        END SWITCH
        IF !SELF:FieldType:HasDecimals()
            SELF:Decimals := 0
        ENDIF
        RETURN TRUE

    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Name+" ('"+SELF:FieldTypeStr+"',"+SELF:Length:ToString()+","+SELF:Decimals:ToString()+","+SELF:Flags:ToString("G")+")"

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.FieldTypeStr/*" />
    PROPERTY FieldTypeStr       AS STRING GET ((CHAR) SELF:FieldType):ToString()
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsMemo/*" />
    PROPERTY IsMemo             AS LOGIC GET SELF:FieldType:IsMemo()
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsBinary/*" />
    PROPERTY IsBinary           AS LOGIC GET SELF:FieldType:IsBinary() .OR. SELF:Flags:HasFlag(DBFFieldFlags.Binary)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsNullable/*" />
    PROPERTY IsNullable         AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Nullable)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsAutoIncrement/*" />
    PROPERTY IsAutoIncrement    AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsStandard/*" />
    PROPERTY IsStandard         AS LOGIC GET SELF:FieldType:IsStandard()
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsVfp/*" />
    PROPERTY IsVfp              AS LOGIC GET SELF:FieldType:IsVfp()
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsVarLength/*" />
    PROPERTY IsVarLength        AS LOGIC GET SELF:FieldType:IsVarLength()
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsUnicode/*" />
    PROPERTY IsUnicode          AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Unicode)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsEncrypted/*" />
    PROPERTY IsEncrypted        AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Encrypted)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsCompressed/*" />
    PROPERTY IsCompressed       AS LOGIC GET SELF:Flags:HasFlag(DBFFieldFlags.Compressed)
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.CanSort/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Caption/*" />
    PROPERTY Caption      AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Caption) DEFAULT SELF:Name;
        SET SELF:Properties:Add(DatabasePropertyType.Caption, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Description/*" />
    PROPERTY Description  AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Comment)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.Comment, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.InputMask/*" />
    PROPERTY InputMask    AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.InputMask)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.InputMask, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.Format/*" />
    PROPERTY Format       AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.Format)  DEFAULT String.Empty;
        SET SELF:Properties:Add(DatabasePropertyType.Format, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.ColumnName/*" />
    PROPERTY ColumnName   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.ColumnName)  DEFAULT SELF:Name;
        SET SELF:Properties:Add(DatabasePropertyType.ColumnName, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.PrimaryKey/*" />
    PROPERTY PrimaryKey   AS LOGIC ;
        GET SELF:_GetPropertyValue<LOGIC>(DatabasePropertyType.KeyField)   ;
        SET SELF:Properties:Add(DatabasePropertyType.KeyField, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.DefaultValue/*" />
    PROPERTY DefaultValue   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.DefaultValue)  DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.DefaultValue, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.IsUnique/*" />
    PROPERTY IsUnique   AS LOGIC ;
        GET SELF:_GetPropertyValue<LOGIC>(DatabasePropertyType.IsUnique)  ;
        SET SELF:Properties:Add(DatabasePropertyType.IsUnique, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.RuleExpression/*" />
    PROPERTY RuleExpression   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.RuleExpression)    DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.RuleExpression, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RddFieldInfo.RuleText/*" />
    PROPERTY RuleText   AS STRING ;
        GET SELF:_GetPropertyValue<STRING>(DatabasePropertyType.RuleText)    DEFAULT String.Empty ;
        SET SELF:Properties:Add(DatabasePropertyType.RuleText, value)

END CLASS

END NAMESPACE
