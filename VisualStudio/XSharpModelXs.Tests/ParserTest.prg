USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING Xunit
USING Xunit.Abstractions
USING XSharpModel


BEGIN NAMESPACE XSharpModel.Tests
	CLASS ParserTest
		PRIVATE output AS ITestOutputHelper

		CONSTRUCTOR( output AS ITestOutputHelper )
			SELF:output := output
		
		[Fact, Trait("Parser", "Parse_1")];
		METHOD Parse_1() AS VOID
			LOCAL cFileName AS STRING
			cFileName := "..\..\TestFiles\CodeFile1.prg"
			ParseAndDisplay(System.IO.File.ReadAllLines(cFileName), output)

		[Fact, Trait("Parser", "Parse_2")];
		METHOD Parse_2() AS VOID
			LOCAL cFileName AS STRING
			cFileName := "..\..\TestFiles\CodeFile1.prg"
			//
			VAR walker := SourceWalker{ XFile{ cFileName } }
			Parse2( walker:Lex( System.IO.File.ReadAllText( cFileName )), output )


	END CLASS
END NAMESPACE
