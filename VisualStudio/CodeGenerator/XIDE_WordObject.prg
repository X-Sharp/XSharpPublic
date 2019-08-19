BEGIN NAMESPACE Xide

ENUM WordStatus
	MEMBER Text
	MEMBER Literal
	MEMBER Comment
END ENUM

ENUM WordSubStatus
	MEMBER Text
	MEMBER TextReserved
	MEMBER TextFunction
	MEMBER TextDirective
	MEMBER TextOperator
	MEMBER LiteralInt
	MEMBER LiteralUInt
	MEMBER LiteralFloat
	MEMBER LiteralDecimal
	MEMBER LiteralSingle
	MEMBER LiteralDouble
	MEMBER LiteralSymbol
	MEMBER LiteralString
	MEMBER LiteralChar
	MEMBER CommentBlock
	MEMBER CommentLine
	MEMBER CommentRegion
END ENUM

[Flags];
ENUM WordStyle AS INT
	MEMBER None := 0
	MEMBER EscapedLiteral := 1
	MEMBER InAttribute := 2
END ENUM

CLASS WordObject
	EXPORT cWord AS STRING
	EXPORT nStart,nEnd AS INT
	EXPORT oInfo AS WordInfo
	EXPORT eStatus AS WordStatus
	EXPORT eSubStatus AS WordSubStatus
	PROTECT eStyle AS WordStyle
	CONSTRUCTOR(_cWord AS STRING)
		SELF:cWord := _cWord
	RETURN
	METHOD SetStyle(_eStyle AS WordStyle) AS VOID
		SELF:eStyle := SELF:eStyle | _eStyle
	RETURN
	METHOD HasStyle(_eStyle AS WordStyle) AS LOGIC
	RETURN (SELF:eStyle & _eStyle) == _eStyle
	ACCESS IsOneChar AS LOGIC
	RETURN SELF:cWord:Length==1 .and. !SELF:cWord==" " .and. !SELF:cWord == e"\t" .and. SELF:eStatus == WordStatus.Text
	ACCESS IsWhiteSpace AS LOGIC
	RETURN (SELF:cWord==" " .or. SELF:cWord == e"\t") .and. SELF:eStatus == WordStatus.Text
	VIRTUAL METHOD ToString() AS STRING
	RETURN "Word:" + iif(SELF:cWord != NULL , SELF:cWord , "")

END CLASS



ENUM WordType AS INT
	MEMBER Unknown := 0
	MEMBER Type := 1
	MEMBER Variable := 2
	MEMBER @@Member := 4
	MEMBER @@Method := 8
	MEMBER @@Operator := 16
END ENUM
ENUM WordSubType
	MEMBER Unknown

	MEMBER @@Local
	MEMBER @@Param
	MEMBER @@Global

	MEMBER @@Function
	MEMBER @@Method

	MEMBER @@Field
	MEMBER @@Property
	MEMBER @@Event

	MEMBER @@Self
	MEMBER @@Super
	MEMBER @@Type
END ENUM

ENUM OperatorType
	MEMBER None
	MEMBER @@Array
	MEMBER IndexedProperty
	MEMBER MethodCall
	MEMBER ConstructorCall
END ENUM

CLASS WordInfo
	EXPORT cName AS STRING
	EXPORT eType AS WordType
	EXPORT eSubType AS WordSubType
	EXPORT eMember AS System.Reflection.MemberTypes
	EXPORT oReturnType AS Type
	EXPORT oDeclaringType AS Type
	EXPORT oMember AS System.Reflection.MemberInfo
	EXPORT eOperator AS OperatorType
	EXPORT cParams AS STRING
	EXPORT nParams AS INT
	EXPORT lArrayElement AS LOGIC

	EXPORT lFromSourceParser AS LOGIC
	EXPORT _cRetType AS STRING // otan apo source parser mono
	EXPORT _cDeclaringType AS STRING // otan apo source parser mono
	CONSTRUCTOR()
		SUPER()
	RETURN
	CONSTRUCTOR(_oType AS Type)
		SUPER()
		SELF:eMember := _oType:MemberType
		SELF:cName := _oType:FullName
		SELF:oReturnType := _oType
		SELF:oDeclaringType := _oType
		SELF:oMember := _oType
		SELF:eType := WordType.Type
	RETURN
	CONSTRUCTOR(oInfo AS System.Reflection.PropertyInfo)
		SUPER()
		SELF:eMember := oInfo:MemberType
		SELF:cName := oInfo:Name
		SELF:oReturnType := oInfo:PropertyType
		SELF:oDeclaringType := oInfo:DeclaringType
		SELF:oMember := oInfo
		SELF:eType := WordType.Member
		SELF:eSubType := WordSubType.Property
	RETURN
	CONSTRUCTOR(oInfo AS System.Reflection.FieldInfo)
		SUPER()
		SELF:eMember := oInfo:MemberType
		SELF:cName := oInfo:Name
		SELF:oReturnType := oInfo:FieldType
		SELF:oDeclaringType := oInfo:DeclaringType
		SELF:oMember := oInfo
		SELF:eType := WordType.Member
		SELF:eSubType := WordSubType.Field
	RETURN
	CONSTRUCTOR(oInfo AS System.Reflection.MethodInfo)
		SUPER()
		SELF:eMember := oInfo:MemberType
//		SELF:cName := oInfo:Name + "(...)"
		SELF:cName := oInfo:Name
		SELF:oReturnType := oInfo:ReturnType
		SELF:oDeclaringType := oInfo:DeclaringType
		SELF:oMember := oInfo
		SELF:eType := WordType.Method
	RETURN
	CONSTRUCTOR(oInfo AS System.Reflection.EventInfo)
		SUPER()
		SELF:eMember := oInfo:MemberType
		SELF:cName := oInfo:Name
		SELF:oReturnType := oInfo:EventHandlerType
		SELF:oDeclaringType := oInfo:DeclaringType
		SELF:oMember := oInfo
		SELF:eType := WordType.Member
		SELF:eSubType := WordSubType.Event
	RETURN
	
END CLASS

END NAMESPACE
