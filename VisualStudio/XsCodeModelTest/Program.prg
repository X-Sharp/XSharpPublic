USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharpModel
BEGIN NAMESPACE XsCodeModelTest


FUNCTION Start() AS VOID
LOCAL cFileName AS STRING
	cFileName := "C:\Test\test.prg"
	cFileName := "C:\VIDE\Projects\XIDE\VIDE\PIDE.prg"
	cFileName := "c:\temp\InventoryRepILst.prg"
	ParseAndDisplay(System.IO.File.ReadAllLines(cFileName))
	Console.ReadLine()
RETURN	


		FUNCTION ParseAndDisplay(aLineCollection AS IList<STRING>) AS VOID
		LOCAL oInfo AS ParseResult
		LOCAL d AS DateTime
		
		? "Starting parsing..."
		d := DateTime.Now
		//LineObject.LinesWithSpecialStuff:Clear()
		VAR parser := Parser{}
		oInfo   := parser:Parse(aLineCollection, true)
		? "Parsing completed!"
		?
		? "Time elapsed:" , DateTime.Now - d
		?
		? "Total Lines:" , oInfo:LineCount
		//? "Entities:" , Parser:Entities:Count
		? "Types:" , oInfo:Types:Count
		//? "Directives, block commands etc:" , LineObject.LinesWithSpecialStuff:Count
		?
		? "Press enter to list info"
		Console.ReadLine()
		?
		? "Types:"
		FOREACH oEntity AS EntityObject IN oInfo:Types
			? "Line:" , oEntity:nStartLine ,"Type:" , oEntity:eType , "Name:" , oEntity:cName ,  "Children", oEntity:aChildren:Count
			? "Children:"
			FOREACH oChild AS EntityObject IN oEntity:aChildren
				? "  line:" , oChild:nStartLine , "Type:" , oChild:eType, "Name:" , oChild:cName ,  "Return Type =", oChild:cRetType
				IF oChild:aChildren:Count > 0
					?? "  Locals: ", oChild:aChildren:Count
				ENDIF
				FOREACH oLocal AS EntityObject IN oChild:aChildren
					? "      Line:" , oLocal:nStartLine , "Type:" , oLocal:eType , "Name:" , oLocal:cName ,  "Return Type =", oLocal:cRetType
				NEXT
				IF oChild:aParams != null
					? "  Parameters: ", oChild:aParams:Count
					FOREACH oParam AS EntityParamsObject IN oChild:aParams
						? "      Parameter:" , oParam:cName, oParam:cType, oParam:nParamType
					NEXT
				ENDIF
			NEXT
			Console.ReadLine()
		NEXT
		//Console.ReadLine()

		//? "Entities:"
		//foreach oEntity as EntityObject in Parser:Entities
		//? "Line:" , oEntity:nLine , "Name:" , oEntity:cName , "Type:" , oEntity:eType , "Return Type =", oEntity:cRetType
		//next
		//Console.ReadLine()
		//? "Locals:"
		//foreach oLocal as EntityObject in Parser:Locals
		//? "Line:" , oLocal:nLine , "Name:" , oLocal:cName , "Type =", oLocal:cRetType
		//next
//
		//Console.ReadLine()
		/*
		?
		? "Directives, Block commands etc:"
		FOREACH oLine AS LineObject IN Parser:LineObjects
			IF oLine:eType == LineType.EndClass .or. oLine:eType == LineType.Return .or. oLine:eType == LineType.Define
				//loop
			END IF
			? "Line:" , oLine:Line , "OffSet:", oLine:OffSet,  "Type:" , oLine:eType , ":" , oLine:cArgument
		NEXT
		*/
		
		RETURN	
END NAMESPACE
