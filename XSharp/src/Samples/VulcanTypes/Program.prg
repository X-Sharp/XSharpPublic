using System
using System.Collections.Generic
using System.Linq
using System.Text

using Vulcan
using VulcanRTFuncs
using XSharpVulcanTest

using static VulcanRTFuncs.Functions // allows us to use methods of this class as if they were functions

Begin Namespace XSharpVulcanTest

	Function Start() as void
		VulcanLoader.InitVulcan() // required for VO/Vulcan database support

		LOCAL startingColor AS ConsoleColor
		startingColor := Console.ForegroundColor

		ConsoleHeading("Bring Your Own Runtime (BYOR) Sample",ConsoleColor.Magenta)
		Console.ForegroundColor := ConsoleColor.Gray

		ConsoleHeading("Dates")
		LOCAL dToday := Today() AS __VODATE // Note: __VODATE's value is a DateTime
		Console.WriteLine(String.Format("Today is {0}",dToday))  // displays as a DateTime
		Console.WriteLine(String.Format("Today is {0}",((DateTime)dToday):ToShortDateString())) // Use DateTime method to change output

		LOCAL dTomorrow AS __VODATE
		dTomorrow := dToday + 1  // can perform VO date arithmetic
		Console.WriteLine(String.Format("Tomorrow is {0}",dTomorrow))  // displays as a DateTime

		ConsoleHeading("Conversion")
		LOCAL sToday AS STRING
		sToday := DTOS(Today()) // Convert date using VO function
		Console.WriteLine(String.Format("Today is {0}",sToday)) 

		ConsoleHeading("Strings")
		LOCAL s AS STRING
		s := SubStr(sToday,1,4) // string manipulation
		Console.WriteLine(String.Format("SubStr is {0}",s))

		LOCAL n AS DWORD
		n := At("0",s)
		Console.WriteLine(String.Format("At is {0}",n))

		LOCAL c := "" AS STRING
		Console.WriteLine(String.Format("c is {0}",IIF(Empty(c),"Empty","Not empty")))
		c := "x"
		Console.WriteLine(String.Format("c is {0}",IIF(Empty(c),"Empty","Not empty")))

		ConsoleHeading("Numeric")
		LOCAL f AS Double
		f := Pow(2,3)
		Console.WriteLine(String.Format("Pow is {0}",f))

		ConsoleHeading("Array")
		LOCAL a AS __Array
		a := ArrayNew()
		AAdd(a,1)
		AAdd(a,2)
		AAdd(a,3)
		Console.WriteLine(String.Format("Array length is {0}",ALen(a)))

		LOCAL e AS DWORD
		e := a:__GetElement(1) // note: zero-based, 2nd element is numbered 1
		Console.WriteLine(String.Format("2nd element is {0}",e))


		ConsoleHeading("Press any key to continue...",ConsoleColor.Green)
		Console.ReadKey()

		Console.ForegroundColor := startingColor

	FUNCTION ConsoleHeading(s AS STRING, c := ConsoleColor.Yellow AS ConsoleColor) AS VOID
		LOCAL originalColor AS ConsoleColor
		originalColor := Console.ForegroundColor

		Console.ForegroundColor := c
		Console.WriteLine(s)

		Console.ForegroundColor := originalColor

	CLASS VulcanLoader
		STATIC METHOD InitVulcan() AS VOID
			LOCAL t AS Type

			t := typeof(VulcanLoader)

			LOCAL mi AS System.Reflection.MethodInfo
			mi := t:GetMethod( "InitVulcan" )
			Vulcan.Runtime.State.AppModule := mi:Module
	END CLASS
	
End Namespace
