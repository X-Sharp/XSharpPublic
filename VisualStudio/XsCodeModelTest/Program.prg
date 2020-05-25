USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharpModel
USING System.Threading
USING System.Threading.Tasks
USING LanguageService.CodeAnalysis.XSharp
BEGIN NAMESPACE XsCodeModelTest

//#define OLD
FUNCTION Start() AS VOID

    VAR aFiles := System.IO.Directory.GetFiles("c:\XSharp\DevRt\","*.prg",System.IO.SearchOption.AllDirectories)
    //VAR aFiles := System.IO.Directory.GetFiles("c:\XSharp\DevRt\Runtime\VOSDK\Source\VOSDK\SQL_Unicode_SDK\","SQLFunctions.prg",System.IO.SearchOption.AllDirectories)
    VAR d := DateTime.Now
    LOCAL size := 0 AS INT64
    VAR entities := 0
    VAR gate := OBJECT{}
    VAR files := List<XFile>{aFiles:Length}
    VAR options := XSharpParseOptions.Default
    VAR result := List<STRING>{}
    
    //Parallel.Foreach (aFiles,  { cFileName =>
    

    FOREACH VAR cFileName IN aFiles
	    VAR oFile := XFile{ cFileName }
#ifdef OLD       	
       	LOCAL oInfo AS ParseResult
        VAR aLineCollection := System.IO.File.ReadAllLines(cFileName)
        oInfo   := parser{}:Parse(aLineCollection, TRUE)
        result:Add(cFileName)
        FOREACH entity AS EntityObject IN oInfo:Entities
            result:Add(entity:cName)
        NEXT
#else        

      VAR parse := Parserv2{ }
       
        XSharp.Parser.VsParser.Lex(System.IO.File.ReadAllText(cFileName), cFileName, options, NULL, OUT VAR stream)
	    parse:Parse( stream, TRUE , oFile, FALSE,FALSE)
        result:Add(cFileName)
        FOREACH element AS XElement IN parse:EntityList
            IF element:Kind != Kind.Namespace
                result:Add(element:Name)
            ENDIF
        NEXT            
#endif        
        BEGIN LOCK gate
	        size += System.IO.FileInfo{cFileName}:Length
#ifdef OLD
            entities += oInfo:Entities:Count
#else            
            entities += oFile:EntityList:Count
#endif            
            files:Add(oFile)
            IF files:Count % 100 == 0
                ? files:Count:ToString("#,##0")
            ENDIF
        END LOCK

    //} )
    NEXT
    ? "Total Time :" , DateTime.Now - d
    ? "Files", aFiles:Length:ToString("#,##0")
    ? "Length", size:ToString("#,##0")
    ? "Entities", entities:ToString("#,##0")
#ifdef OLD
    System.IO.File.WriteAllLines("C:\test\oldent.txt", result)
#else
System.IO.File.WriteAllLines("C:\test\newent.txt", result)
#endif

	Console.ReadLine()
RETURN	


FUNCTION ParseAndDisplay(aLineCollection AS IList<STRING>) AS VOID
	LOCAL oInfo AS ParseResult
	LOCAL d AS DateTime
		
	? "Starting parsing..."
	d := DateTime.Now
	//LineObject.LinesWithSpecialStuff:Clear()
	VAR parser := Parser{}
	oInfo   := parser:Parse(aLineCollection, TRUE)
	? "Parsing completed!"
	?
	? "Time elapsed:" , DateTime.Now - d
	?
	? "Total Lines:" , oInfo:LineCount
	//? "Entities:" , Parser:Entities:Count
	? "Types:" , oInfo:Types:Count
	//? "Directives, block commands etc:" , LineObject.LinesWithSpecialStuff:Count
	?
    ? "Entities ",oInfo:Entities:Count
		
    ? "Press enter to list info"
	Console.ReadLine()
    ?
	/*? "Types:"
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
    */
    
		
    RETURN	
END NAMESPACE
