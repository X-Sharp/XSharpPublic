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

/// <include file="XSharp.Core.Docs.xml" path="doc/DbEvalInfo/*" />
CLASS DbEvalInfo

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbEvalInfo.Block/*" />
	PUBLIC Block	 AS ICodeblock

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbEvalInfo.ScopeInfo/*" />
	PUBLIC ScopeInfo AS DbScopeInfo
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbEvalInfo.ctor/*" />
    CONSTRUCTOR()
        SELF:ScopeInfo := DbScopeInfo{}
        RETURN
END CLASS


/// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo/*" />
CLASS DbFilterInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.FilterText/*" />
	PUBLIC FilterText	AS STRING

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.FilterBlock/*" />
	PUBLIC FilterBlock	AS ICodeblock

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.Active/*" />
	PUBLIC Active		AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.Optimized/*" />
	PUBLIC Optimized	AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.Clear/*" />
	METHOD Clear() AS VOID
		SELF:FilterBlock := NULL
		SELF:FilterText	 := String.Empty
		SELF:Active		 := FALSE
		SELF:Optimized	 := FALSE
		RETURN

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.Clone/*" />
	METHOD Clone AS DbFilterInfo
		LOCAL oClone AS DbFilterInfo
		oClone := DbFilterInfo{}
		oClone:FilterBlock := SELF:FilterBlock
		oClone:FilterText  := SELF:FilterText
		oClone:Optimized   := SELF:Optimized
		oClone:Active	   := FALSE
		RETURN oClone

    /// <include file="XSharp.Core.Docs.xml" path="doc/DbFilterInfo.ctor/*" />
    CONSTRUCTOR
        SELF:Clear()
        RETURN

	METHOD Compile(oRDD AS IRdd) AS VOID
        IF SELF:FilterBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:FilterText)
            SELF:FilterBlock := oRDD:Compile(SELF:FilterText)
        ENDIF
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbLockInfo/*" />
STRUCTURE DbLockInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbLockInfo.RecId/*" />
	PUBLIC RecId		AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbLockInfo.Method/*" />
	PUBLIC @@Method AS LockMethod

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbLockInfo.Result/*" />
	PUBLIC Result		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/LockMethod/*" />
	ENUM LockMethod
  /// <include file="XSharp.Core.Docs.xml" path="doc/LockMethod.Exclusive/*" />
		MEMBER Exclusive := 1
  /// <include file="XSharp.Core.Docs.xml" path="doc/LockMethod.Multiple/*" />
		MEMBER Multiple  := 2
  /// <include file="XSharp.Core.Docs.xml" path="doc/LockMethod.File/*" />
		MEMBER File 	 := 3
	END ENUM
END STRUCTURE

/// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo/*" />
CLASS DbOpenInfo
	/// <summary>Should the table be opened Readonly?</summary>
	PUBLIC ReadOnly 	AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.Shared/*" />
	PUBLIC Shared		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.Alias/*" />
	PUBLIC Alias		AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.Extension/*" />
	PUBLIC Extension    AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.FileName/*" />
	PUBLIC FileName		AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.Workarea/*" />
	PUBLIC Workarea		AS DWORD

    /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.FullName/*" />
    PUBLIC PROPERTY FullName AS STRING
        GET
            RETURN FileName + Extension
        END GET
        SET
            SELF:FileName  := Path.ChangeExtension(value,null)
            SELF:Extension := Path.GetExtension( value )
            // Make sure that a filename that ends with a '.' like "Test." does not get changed to "Test.Dbf"
            if String.IsNullOrEmpty(SELF:Extension) .and. value:EndsWith(".")
                SELF:Extension := "."
            ENDIF
        END SET
    END PROPERTY

	CONSTRUCTOR()
        SUPER()
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.ctor/*" />
	CONSTRUCTOR(sFileName AS STRING, sAlias AS STRING, dwWorkarea AS DWORD, lShared AS LOGIC, lReadOnly AS LOGIC)
		SELF:FullName := sFileName
		IF String.IsNullOrEmpty( sAlias )
			Alias := Path.GetFileNameWithoutExtension( sFileName )
		ELSE
			Alias	 	:= sAlias
		ENDIF
		Workarea	:= dwWorkarea
		Shared		:= lShared
		ReadOnly	:= lReadOnly
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOpenInfo.FileMode/*" />
	PUBLIC PROPERTY FileMode AS DWORD
	GET
		LOCAL nMode AS DWORD
		nMode := FO_COMPAT
		IF (Shared)
			nMode |= FO_SHARED
		ELSE
			nMode |= FO_EXCLUSIVE
		ENDIF
		IF (ReadOnly)
			nMode |= FO_READ
		ELSE
			nMode |= FO_READWRITE
		ENDIF
		RETURN nMode
	END GET
	END PROPERTY
    PUBLIC METHOD Clone() AS DbOpenInfo
        RETURN (DbOpenInfo) SELF:MemberwiseClone()
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo/*" />
CLASS DbOrderCondInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Active/*" />
	PUBLIC Active			AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Additive/*" />
	PUBLIC Additive			AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.All/*" />
	PUBLIC All				AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Custom/*" />
	PUBLIC Custom			AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Descending/*" />
	PUBLIC @@Descending     AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.EvalBlock/*" />
	PUBLIC EvalBlock		AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.ForBlock/*" />
	PUBLIC ForBlock			AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.ForExpression/*" />
	PUBLIC ForExpression	AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.NextCount/*" />
	PUBLIC NextCount		AS DWORD
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.NoOptimize/*" />
	PUBLIC NoOptimize		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.RecNo/*" />
	PUBLIC RecNo			AS DWORD
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Rest/*" />
	PUBLIC Rest				AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Scoped/*" />
	PUBLIC Scoped			AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.StartRecNo/*" />
	PUBLIC StartRecNo		AS DWORD
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.StepSize/*" />
	PUBLIC StepSize			AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.UseCurrent/*" />
	PUBLIC UseCurrent		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.WhileBlock/*" />
	PUBLIC WhileBlock		AS ICodeblock
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.WhileExpression/*" />
    PUBLIC WhileExpression	AS STRING

    /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.Collation/*" />
    PUBLIC Collation        AS STRING
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.IsBinary/*" />
    PUBLIC IsBinary         AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCondInfo.IsCandidate/*" />
    PUBLIC IsCandidate      AS LOGIC
    /// <summary>
    /// Compact option for the FoxPro INDEX ON command
    /// </summary>
    PUBLIC IsCompact      := TRUE  AS LOGIC


    METHOD Compile(oRDD AS IRdd) AS VOID
        IF SELF:WhileBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:WhileExpression)
            SELF:WhileBlock := oRDD:Compile(SELF:WhileExpression)
        ENDIF
        IF SELF:ForBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:ForExpression)
            SELF:ForBlock := oRDD:Compile(SELF:ForExpression)
        ENDIF
    METHOD Validate() AS VOID
        SELF:Active := FALSE
        SELF:Scoped := FALSE
        IF SELF:All .OR. SELF:ForBlock != NULL .OR. SELF:WhileBlock != NULL .OR.  ! String.IsNullOrEmpty(SELF:ForExpression ) ;
            .OR. SELF:NextCount != 0 .OR. SELF:RecNo != 0 .OR. SELF:Rest  .OR. SELF:Descending .OR. ! String.IsNullOrEmpty(SELF:WhileExpression)
            SELF:Active := TRUE
        ENDIF
        IF SELF:EvalBlock != NULL .AND. SELF:StepSize == 0
            SELF:StepSize := 1
        ENDIF
        IF SELF:Active
            IF SELF:All .OR. SELF:WhileBlock != NULL .OR. SELF:NextCount != 0 .OR. SELF:RecNo != 0 .OR. SELF:Rest
                SELF:Scoped := TRUE
            ENDIF
        ENDIF

    METHOD Clone() AS DbOrderCondInfo
        RETURN (DbOrderCondInfo) SELF:MemberwiseClone()

END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo/*" />
CLASS DbOrderCreateInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.BagName/*" />
	PUBLIC BagName		AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.Order/*" />
	PUBLIC Order		AS OBJECT
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.Expression/*" />
	PUBLIC Expression	AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.Unique/*" />
	PUBLIC Unique		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.Block/*" />
	PUBLIC Block		AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderCreateInfo.OrdCondInfo/*" />
	PUBLIC OrdCondInfo	AS DbOrderCondInfo

    METHOD Compile(oRDD AS IRdd) AS VOID
        IF SELF:Block == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:Expression)
            SELF:Block := oRDD:Compile(SELF:Expression)
        ENDIF
        IF SELF:OrdCondInfo != NULL
            SELF:OrdCondInfo:Compile(oRDD)
        ENDIF

   METHOD Clone() AS DbOrderCreateInfo
        RETURN (DbOrderCreateInfo) SELF:MemberwiseClone()

END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo/*" />
CLASS DbOrderInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo.AllTags/*" />
	PUBLIC AllTags		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo.Expression/*" />
	PUBLIC Expression	AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo.Order/*" />
	PUBLIC Order		AS OBJECT
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo.BagName/*" />
	PUBLIC BagName		AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbOrderInfo.Result/*" />
	PUBLIC Result		AS OBJECT

    METHOD Clone() AS DbOrderInfo
        RETURN (DbOrderInfo) SELF:MemberwiseClone()
    PROPERTY IsEmpty AS LOGIC
        GET
            IF String.IsNullOrEmpty(BagName)
                IF Order == NULL
                    RETURN TRUE
                ELSEIF Order IS LONG VAR nOrder
                    RETURN FALSE
                ELSEIF Order IS STRING VAR cOrder
                    RETURN String.IsNullOrEmpty(cOrder)
                ELSEIF RuntimeState.Dialect == XSharpDialect.FoxPro .AND. Order IS LOGIC VAR lValue
                    // VFP NIL is represented by a logic FALSE
                    RETURN ! lValue
                ENDIF
            ENDIF
            RETURN FALSE
        END GET
    END PROPERTY
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo/*" />
CLASS DbRelInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo.Key/*" />
	PUBLIC Key			AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo.Block/*" />
	PUBLIC Block		AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo.Child/*" />
	PUBLIC Child		AS IRdd
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo.Parent/*" />
	PUBLIC Parent		AS IRdd

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbRelInfo.Name/*" />
	PUBLIC Name         AS STRING
    METHOD Compile() AS VOID
        IF SELF:Block == NULL .AND. SELF:Parent != NULL .AND. ! String.IsNullOrWhiteSpace(SELF:Key)
            SELF:Block := SELF:Parent:Compile(SELF:Key)
        ENDIF

    METHOD Clone() AS DbRelInfo
        RETURN (DbRelInfo) SELF:MemberwiseClone()

END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo/*" />
CLASS DbScopeInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.IgnoreDuplicates/*" />
	PUBLIC IgnoreDuplicates AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.IgnoreFilter/*" />
	PUBLIC IgnoreFilter		AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.IncludeDeleted/*" />
	PUBLIC IncludeDeleted	AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.ForBlock/*" />
	PUBLIC ForBlock			AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.ForExpression/*" />
	PUBLIC ForExpression	AS STRING
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.Last/*" />
	PUBLIC Last				AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.NextCount/*" />
	PUBLIC NextCount		AS DWORD
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.RecId/*" />
	PUBLIC RecId			AS OBJECT
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.Rest/*" />
	PUBLIC Rest				AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.WhileBlock/*" />
	PUBLIC WhileBlock		AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.WhileExpression/*" />
	PUBLIC WhileExpression	AS STRING

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.ctor/*" />
	CONSTRUCTOR()
		SELF:Clear()

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.Clear/*" />
	PUBLIC METHOD Clear() AS VOID
		IgnoreDuplicates := FALSE
		IgnoreFilter	 := FALSE
		IncludeDeleted 	 := FALSE
		ForBlock		 := NULL
		ForExpression	 := String.Empty
		Last			 := FALSE
		NextCount		 := 0
		RecId			 := NULL
		Rest			 := FALSE
		WhileBlock		 := NULL
		WhileExpression  := String.Empty

 /// <include file="XSharp.Core.Docs.xml" path="doc/DbScopeInfo.Clone/*" />
	METHOD Clone AS DbScopeInfo
		RETURN (DbScopeInfo) SELF:MemberwiseClone()

	METHOD Compile(oRDD AS IRdd) AS VOID
        IF SELF:WhileBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:WhileExpression)
            SELF:WhileBlock := oRDD:Compile(SELF:WhileExpression)
        ENDIF
        IF SELF:ForBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:ForExpression)
            SELF:ForBlock := oRDD:Compile(SELF:ForExpression)
        ENDIF
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbSeekInfo/*" />
STRUCTURE DbSeekInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSeekInfo.Last/*" />
	PUBLIC Last 	AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSeekInfo.SoftSeek/*" />
	PUBLIC SoftSeek AS LOGIC
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSeekInfo.Value/*" />
	PUBLIC Value AS OBJECT
END STRUCTURE

/// <include file="XSharp.Core.Docs.xml" path="doc/DbSortInfo/*" />
CLASS DbSortInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortInfo.TransInfo/*" />
	PUBLIC TransInfo  AS DbTransInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortInfo.Items/*" />
	PUBLIC Items	  AS DbSortItem[]
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortInfo.ItemCount/*" />
    PROPERTY ItemCount  AS LONG GET Items:Length
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortInfo.ctor/*" />
    CONSTRUCTOR(transItemCount AS LONG, sortItemCount AS LONG)
        SELF:TransInfo := DbTransInfo{transItemCount}
        SELF:Items     := DbSortItem[]{sortItemCount}
        RETURN
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbSortItem/*" />
STRUCTURE DbSortItem
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortItem.FieldNo/*" />
	PUBLIC FieldNo 	AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortItem.OffSet/*" />
	PUBLIC OffSet	AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortItem.Length/*" />
	PUBLIC Length	AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbSortItem.Flags/*" />
	PUBLIC Flags	AS  DbSortFlags

END STRUCTURE


/// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo/*" />
CLASS DbTransInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.Scope/*" />
	PUBLIC Scope		AS DbScopeInfo
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.Source/*" />
	PUBLIC Source		AS IRdd
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.Destination/*" />
	PUBLIC Destination 	AS IRdd
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.Items/*" />
	PUBLIC Items		AS DbTransItem[]
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.Flags/*" />
	PUBLIC Flags		AS DbTransInfoFlags
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.ItemCount/*" />
    PUBLIC PROPERTY ItemCount AS LONG AUTO
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransInfo.ctor/*" />
    CONSTRUCTOR(itemCount AS LONG)
        SELF:Items := DbTransItem[]{itemCount}
        SELF:Scope := DbScopeInfo{}
        SELF:ItemCount := itemCount
        SELF:Flags := DbTransInfoFlags.None
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/DbTransItem/*" />
STRUCTURE DbTransItem
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransItem.Source/*" />
	PUBLIC Source 		AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbTransItem.Destination/*" />
	PUBLIC Destination 	AS LONG
END STRUCTURE



/// <exclude/>
STATIC CLASS RDDExtensions
    STATIC METHOD IsMemo(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Memo
        CASE DbFieldType.Picture
        CASE DbFieldType.General
            RETURN TRUE
        END SWITCH
        RETURN FALSE

    STATIC METHOD IsVarLength(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.VarBinary
        CASE DbFieldType.VarChar
            RETURN TRUE
        END SWITCH
        RETURN FALSE

    STATIC METHOD IsStandard(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Character
        CASE DbFieldType.Date
        CASE DbFieldType.Logic
        CASE DbFieldType.Memo
        CASE DbFieldType.Number
            RETURN TRUE
        END SWITCH
        RETURN FALSE

    STATIC METHOD IsBinary(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Integer
        CASE DbFieldType.Currency
        CASE DbFieldType.Double
        CASE DbFieldType.Picture
        CASE DbFieldType.Blob
        CASE DbFieldType.VarBinary
        CASE DbFieldType.DateTime
            RETURN TRUE
        END SWITCH
        RETURN FALSE
    STATIC METHOD HasDecimals(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Double
        CASE DbFieldType.Float
        CASE DbFieldType.Number
        CASE DbFieldType.Currency
            RETURN TRUE
        END SWITCH
        RETURN FALSE
    STATIC METHOD IsVfp(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Character
        CASE DbFieldType.Blob
        CASE DbFieldType.Currency
        CASE DbFieldType.Date
        CASE DbFieldType.DateTime
        CASE DbFieldType.Double
        CASE DbFieldType.Float
        CASE DbFieldType.General
        CASE DbFieldType.Integer
        CASE DbFieldType.Logic
        CASE DbFieldType.Memo
        CASE DbFieldType.Number
        CASE DbFieldType.Picture
        CASE DbFieldType.VarBinary
        CASE DbFieldType.VarChar
        CASE DbFieldType.NullFlags
            RETURN TRUE
        END SWITCH
        RETURN FALSE
END CLASS
END NAMESPACE


BEGIN NAMESPACE XSharp

/// <include file="XSharp.Core.Docs.xml" path="doc/_FieldNames/*" />
CLASS _FieldNames
    /// <include file="XSharp.Core.Docs.xml" path="doc/_FieldNames.Fields/*" />
    PUBLIC Fields AS STRING[]
    /// <include file="XSharp.Core.Docs.xml" path="doc/_FieldNames.FieldCount/*" />
    PROPERTY FieldCount AS LONG GET Fields:Length
    /// <include file="XSharp.Core.Docs.xml" path="doc/_FieldNames.ctor/*" />
    CONSTRUCTOR (aFields AS IList<STRING>)
        SELF:Fields := aFields:ToArray()
        RETURN
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/_JoinList/*" />
CLASS _JoinList
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinList.uiDestSel/*" />
    PUBLIC uiDestSel AS DWORD
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinList.Fields/*" />
    PUBLIC Fields AS _JoinField[]
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinList.Count/*" />
    PUBLIC PROPERTY Count AS LONG GET Fields:Length
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinList.ctor/*" />
    PUBLIC CONSTRUCTOR(nFields AS LONG)
        SELF:Fields := _JoinField[]{nFields}
        RETURN
END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/_JoinField/*" />
STRUCTURE _JoinField
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinField.Area/*" />
    PUBLIC Area AS DWORD
    /// <include file="XSharp.Core.Docs.xml" path="doc/_JoinField.Pos/*" />
    PUBLIC Pos  AS DWORD
END STRUCTURE

/// <include file="XSharp.Core.Docs.xml" path="doc/_RddList/*" />
STRUCTURE _RddList
    /// <summary>List of RDD names.</summary>
    EXPORT atomRddName AS STRING[]
    /// <include file="XSharp.Core.Docs.xml" path="doc/_RddList.uiRddCount/*" />
    PROPERTY uiRddCount AS DWORD GET (DWORD) atomRddName:Length

    /// <include file="XSharp.Core.Docs.xml" path="doc/_RddList.ctor/*" />
    CONSTRUCTOR(oRDD AS Workarea)
        VAR names := List<STRING>{}
        VAR type  := oRDD:GetType()
        DO WHILE type != typeof(Workarea)
            VAR name := type:Name:ToUpper()
            // map names to VO compatible names
            IF name == "DBF"
                names:Add("CAVODBF")
            ELSE
                names:Add(name)
            ENDIF
            type := type:BaseType
        ENDDO
        names:Reverse()
        atomRddName := names:ToArray()
    /// <include file="XSharp.Core.Docs.xml" path="doc/_RddList.ctor_2/*" />
    CONSTRUCTOR(aNames AS STRING[])
        atomRddName := aNames
        RETURN

END STRUCTURE
END NAMESPACE
