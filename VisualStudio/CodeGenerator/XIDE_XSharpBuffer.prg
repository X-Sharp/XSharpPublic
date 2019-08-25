#using System.Collections.Generic
#using System.Reflection

BEGIN NAMESPACE Xide

CLASS XSharpBuffer INHERIT BaseBuffer

	METHOD GetStringLines() AS List<STRING>
		LOCAL aNew AS List<STRING>
		aNew := List<STRING>{SELF:aLines:Count}
		FOREACH oLine AS LineObject IN SELF:aLines
			aNew:Add(oLine:LineText)
		NEXT
	RETURN aNew
	
	CONSTRUCTOR(_aLines AS List<LineObject>)
		SUPER(FileType.XSharp , _aLines)
	RETURN
	STATIC METHOD Create(_aLines AS List<STRING>) AS XSharpBuffer
		LOCAL aNew AS List<LineObject>
		aNew := List<LineObject>{_aLines:Count}
		FOREACH cLine AS STRING IN _aLines
			aNew:Add(LineObject{cLine})
		NEXT
	RETURN XSharpBuffer{aNew}

	STATIC PROTECT oXSharpParsingOptions AS ParsingOptions
	STATIC ACCESS XSharpParsingOptions AS ParsingOptions
	RETURN XSharpBuffer.oXSharpParsingOptions
	
	STATIC EXPORT oLinqKeywords AS STRING[]
	
	STATIC CONSTRUCTOR()
		LOCAL aWords AS STRING[]
		LOCAL n AS INT
		
		XSharpBuffer.oXSharpParsingOptions := ParsingOptions{}

		aWords := <STRING>{;
		"Byte","Short","ShortInt","Word","Int","Long","LongInt","DWord","Int64",;
		"Real4","Real8","Float",  "Logic",  "String","Psz","Usual","Array","Date",;
		"Object","Symbol","CodeBlock","Ptr","Void","Nil","Null","True","False",;
		"Structure","Struct","VOStruct","Align","Union","Member","Enum","Event",;
		"Delegate","Global","Using","NameSpace","Class","Inherit","Hidden","Self",;
		"Super","Partial","Interface","Function","Func","Procedure","Proc","Return",;
		"Method","Access","Assign","Constructor","Destructor","Local","Static","Protect",;
		"Instance","Export","As","Is","_Cast","Ref","Dim","Clipper","Pascal","CDecl",;
		"Strict","WinApi","For","ForEach","Next","To","UpTo","DownTo","Step","Loop","Exit",;
		"If","Else","ElseIf","EndIf","Do","Case","Otherwise","While","EndDo","EndCase","Repeat","Until",;
		"Begin","Sequence","End","Recover","Finally","Break","Lock","Try","Catch","Throw",;
		"Private","Protected","Implements","Sealed","Abstract","Public","Field","_Dll","Internal",;
		"Const","InitOnly","Virtual","New","Operator","Explicit","Implicit","Declare","_Init1","_Init2","_Init3",;
		"Property","Set","Get","Value","Auto","In","Out","Implied","Scope" , ;
		;
		"Var", "Switch", "Define", "Volatile", "Checked", "Unchecked", "Unsafe", "Fixed", ;
		"From", "Where", "OrderBy", "Group", "Select", "Descending", "Ascending", "By", "Into", "Join", "Let", "Equals", "On",;
		"Yield", "Async", "Await", "Extern", "Nop", "Override", "Dynamic"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oReserved:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT
		
		oLinqKeywords := <STRING>{"WHERE", "ORDERBY", "GROUP", "SELECT", "DESCENDING", "ASCENDING", "BY", "INTO", "JOIN", "LET", "EQUALS", "ON"}
		
		aWords := <STRING>{;
		"CLASS","METHOD","FUNCTION","PROCEDURE","FUNC","PROC","ACCESS","ASSIGN","OPERATOR","DELEGATE",;
		"GLOBAL","CONSTRUCTOR","DESTRUCTOR","STRUCTURE","STRUCT","VOSTRUCT","UNION","ENUM","INTERFACE","PROPERTY","DEFINE"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oEntityMarkers:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"VIRTUAL", "PARTIAL", "_DLL", "ABSTRACT", "SEALED", ;
		"INTERNAL", "HIDDEN", "STATIC", "PROTECTED", "INSTANCE", ;
		"PROTECT", "PRIVATE", "PUBLIC", "EXPORT", "CONST", ;
		"INITONLY", "MEMBER", "NEW", "ASYNC", "EXTERN", "UNSAFE", "OVERRIDE"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oEntityVisibility:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"ifdef","ifndef","endif","else","define","undef","command","translate",;
		"region","endregion","using","include","line","pragma","error","warning"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oDirectives:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"DO","CASE","OTHERWISE","FOR","FOREACH","WHILE","IF","ELSE","ELSEIF",;
		"BEGIN","RECOVER","FINALLY","TRY","CATCH","REPEAT","SET","GET"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oIndent:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"END","ENDIF","ENDCASE","ENDDO","NEXT","ELSE","ELSEIF",;
		"OTHERWISE","BEGIN","RECOVER","FINALLY","CATCH","UNTIL"}
		FOR n := 1 UPTO aWords:Length
			XSharpBuffer.oXSharpParsingOptions:oOutdent:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

	RETURN

END CLASS

END NAMESPACE

