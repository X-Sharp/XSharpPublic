// Please read the comments in Readme.txt !
using System
using System.Collections.Generic
using System.Linq
using System.Text


Function Start() as void

	LOCAL startingColor AS ConsoleColor
	startingColor := Console.ForegroundColor

	ConsoleHeading("Bring Your Own Runtime (BYOR) Sample",ConsoleColor.Magenta)
	Console.ForegroundColor := ConsoleColor.Gray

	DateSamples()
	Conversion()
	Strings()
	Numeric()
	DoWait() 
	Arrays()
	Symbols()
	LateBinding()
	DoWait()

	Workarea()
	DoWait()
	Macros()
	DoWait()
	CodeBlocks()
	DoWait()

	Console.ForegroundColor := startingColor

FUNCTION ConsoleHeading(s AS STRING, c := ConsoleColor.Yellow AS ConsoleColor) AS VOID
	LOCAL originalColor AS ConsoleColor
	originalColor := Console.ForegroundColor

	Console.ForegroundColor := c
	Console.WriteLine(s)
	Console.ForegroundColor := originalColor

FUNCTION DoWait() AS VOID
	ConsoleHeading("Press any key to continue...",ConsoleColor.Green)
	Console.ReadKey()


FUNCTION DateSamples() AS VOID
	ConsoleHeading("Dates")
	LOCAL dToday := Today() AS DATE 
	Console.WriteLine( i"Today is {dToday:d}"	)	// Interpolated string, short Date Format
	Console.WriteLine( i"Today is {dToday:G}"	)	// Interpolated string, General Date Format

	LOCAL dTomorrow AS __VODATE
	dTomorrow := dToday + 1  // can perform VO date arithmetic
	Console.WriteLine( i"Tomorrow is {dTomorrow:D}") // Long date format   

	LOCAL dNewYear AS DATE
    dNewYear := 2016.01.01

    Console.WriteLine("New year was " +AsString(dNewYear)+ " on a "+CDOW(dNewyear))

FUNCTION Conversion() AS VOID
	ConsoleHeading("Conversion")
	LOCAL sToday AS STRING
	sToday := DTOS(Today()) // Convert date using VO function
	Console.WriteLine(i"Today is {sToday}") 
	
FUNCTION Strings() AS VOID
	ConsoleHeading("Strings")
	LOCAL s AS STRING
	LOCAL sToday := DTOS(Today())
	s := SubStr(sToday,1,4) // string manipulation
	Console.WriteLine(String.Format("SubStr is {0}",s))

	LOCAL n AS DWORD
	n := At("0",s)
	Console.WriteLine(String.Format("At is {0}",n))

	LOCAL c := "" AS STRING
	Console.WriteLine(String.Format("c is {0}",IIF(Empty(c),"Empty","Not empty")))
	c := "x"
	Console.WriteLine(String.Format("c is {0}",IIF(Empty(c),"Empty","Not empty")))
	RETURN

FUNCTION Numeric() as VOID
	ConsoleHeading("Numeric")
	LOCAL r AS REAL8
	r := Pow(2,3)
	Console.WriteLine(String.Format("Pow is {0}",r))
	LOCAL f AS FLOAT
	f := Sqrt(10)
	Console.WriteLine(String.Format("The root of 10 is {0}",f))
	RETURN

FUNCTION Arrays() AS VOID
	ConsoleHeading("Array")
	LOCAL a AS ARRAY
	a := {1,2,3}
	Console.WriteLine(String.Format("Original Array length is {0}",ALen(a)))
	AAdd(a,4)
	AAdd(a,5)
	AAdd(a,6)
	Console.WriteLine(String.Format("Array length after adding 3 elements is {0}",ALen(a)))
	FOR VAR i := 1 TO ALen(a)
		Console.Writeline(String.Format("{0} {1}", i, a[i]))
	NEXT
	ADel(a, 1)
	Asize(a, Alen(a) -1)
	Console.WriteLine(String.Format("Array length after deleting 1 elements is {0}",ALen(a)))
	RETURN

FUNCTION Symbols() AS VOID
	LOCAL s as Symbol
	LOCAL c as STRING
	ConsoleHeading("Symbol")
	s := #LastName
	Console.WriteLine("Symbol s: "+s:ToString())
	c := Symbol2String(s)
	Console.WriteLine("Symbol converted to string : "+c)

	RETURN

FUNCTION LateBinding() AS VOID
	LOCAL e as Object
	ConsoleHeading("Late binding")
	e := Error{0, "Message"}
	Console.WriteLine("Reading property from a untype variable")
	Console.WriteLine( (String) e:Message)

	RETURN

Function Workarea() AS VOID
	LOCAL cWorkDir as STRING
	FIELD LASTNAME, FIRSTNAME in CUSTOMER
	ConsoleHeading("DBF Access")
	cWorkDir := "..\..\"
	SetPath(cWorkDir)
	DbUseArea(TRUE, "DBFNTX","Customer")
	DbSetIndex("CustNum.NTX")
	DbSetIndex("CustName.NTX")
	DbGoTop()
	DO WHILE ! EOF()
		? Str(Recno(),3), LASTNAME, FIRSTNAME, _FIELD->CITY
		DbSkip(1)
	ENDDO
	DbCloseArea()

FUNCTION Macros() AS VOID
	LOCAL cMacro as STRING
	LOCAL cbMacro as CodeBlock
	ConsoleHeading("Macros")
	cMacro := "{||Today()}"
	cbMacro := &(cMacro)
	? cMacro, Eval(cbMacro)
	cMacro := "1+2+3"
	? cMacro, &cMacro
	cMacro := "System.Int32.MaxValue"
	cbMacro := MCompile(cMacro)
	? cMacro, MExec(cbMacro)
	cMacro := "{|a,b,c| a*b*c}"
	cbMacro := &(cMacro)
	? cMacro, Eval(cbMacro, 2,3,4)

	?
	RETURN

FUNCTION CodeBlocks() AS VOID
	LOCAL oBlock as CodeBlock
	ConsoleHeading("Codeblocks")
	oBlock := {||Today()}
	? oBlock, eval(oBlock)
	oBlock := {||System.Math.Pow(2,3)}
	? oBlock, eval(oBlock)
	oBlock := {|a,b,c|a*b*c}
	? oBlock, eval(oBlock,2,3,4)
	?
	RETURN


