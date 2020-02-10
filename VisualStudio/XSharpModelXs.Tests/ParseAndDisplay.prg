// ParseAndDisplay.prg
// Created by    : fabri
// Creation Date : 2/4/2020 1:30:59 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING XSharpModel
USING Xunit.Abstractions


FUNCTION ParseAndDisplay(aLineCollection AS IList<STRING>, output AS ITestOutputHelper) AS VOID
	LOCAL oInfo AS ParseResult
	LOCAL d AS DateTime
	
	output:WriteLine( "Starting parsing...")
	d := DateTime.Now
		//LineObject.LinesWithSpecialStuff:Clear()
	VAR parser := Parser{}
	oInfo   := parser:Parse(aLineCollection, TRUE)
	output:WriteLine( "Parsing completed!")
	output:WriteLine( String.Format( "Time elapsed: {0}",  DateTime.Now - d) )
	output:WriteLine( "Total Lines:" + oInfo:LineCount:ToString() )
		//? "Entities:" , Parser:Entities:Count
	output:WriteLine( String.Format( "Types: {0}" , oInfo:Types:Count ) )
		//? "Directives, block commands etc:" , LineObject.LinesWithSpecialStuff:Count
	output:WriteLine( "Types:")
	FOREACH oEntity AS EntityObject IN oInfo:Types
		output:WriteLine( String.Format( "Line:{0}  Type:{1}  Name:{2} Children:{3} " , oEntity:nStartLine , oEntity:eType , oEntity:cName , oEntity:aChildren:Count))
		output:WriteLine( "Children:")
		FOREACH oChild AS EntityObject IN oEntity:aChildren
			output:WriteLine(  String.Format( "  line:{0} Type:{1} Name:{2} Return Type ={3}" , oChild:nStartLine ,oChild:eType, oChild:cName , oChild:cRetType))
			IF oChild:aChildren:Count > 0
				output:WriteLine( String.Format("  Locals:{0} ", oChild:aChildren:Count))
			ENDIF
			FOREACH oLocal AS EntityObject IN oChild:aChildren
				output:WriteLine( String.Format("      Line:{0} Type:{1} Name:{2} Return Type ={3}" , oLocal:nStartLine , oLocal:eType , oLocal:cName , oLocal:cRetType))
			NEXT
			IF oChild:aParams != NULL
				output:WriteLine( String.Format("  Parameters:{0} ", oChild:aParams:Count))
				FOREACH oParam AS EntityParamsObject IN oChild:aParams
					output:WriteLine( String.Format("      Parameter: {0} {1} {2}" , oParam:cName, oParam:nParamType, oParam:cType) )
				NEXT
			ENDIF
		NEXT
		//Console.ReadLine()
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