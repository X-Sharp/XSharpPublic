USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


	FUNCTION Start() AS VOID
		LOCAL oList AS List<Int>
		// The next line creates the collection and adds 5 elements
		// Note the double curly braces:
		// The first pair calls the default constructor of the List<> Class
		// The second pair of curly braces surrounds the list of values
		Console.WriteLine("Collection Initializers")
		oList := List<Int>{} {1,2,3,4,5}
		FOREACH VAR i IN oLIst
			Console.WriteLine(i)
		NEXT
		VAR oCompass := List<String>{}{"North", "East", "South", "West"}
		FOREACH VAR sDirection in oCompass
			Console.WriteLine(sDirection)
		NEXT
		Console.ReadLine()
		// Now an example of an Object Initializer
		// Note that the object has no constructor
		// We are assigning the values directly to the properties
		// This will only work if there are public properties
		// Again there are double curly braces:
		// The first pair calls the default constructor of the Person class
		// The second pair of curly braces surrounds the list of name-value pairs

		Console.WriteLine("Object Initializer")
		VAR oPerson := Person{}{FirstName := "John", LastName := "Smith"}
		? oPerson:Name
		Console.ReadLine()
		// Combine the two
		Var oPeople := List<Person> {} {;
											Person{}{FirstName := "John", LastName := "Smith"}, ;
											Person{}{FirstName := "Jane", LastName := "Doe"} ;
										}
		Console.WriteLine("Collection and Object Initializers")							
		FOREACH var oP in oPeople
			Console.WriteLine(oP:Name)
		NEXT
		Console.ReadLine()			
		RETURN


	
PUBLIC CLASS Person
	PROPERTY FirstName  AS STRING AUTO
	PROPERTY LastName   AS STRING AUTO
	PROPERTY Name		AS STRING GET FirstName+" "+LastName
END CLASS
