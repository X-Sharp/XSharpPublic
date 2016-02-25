USING System.Collections.Generic    
USING System.Linq
USING STATIC System.Console

FUNCTION Start AS VOID
	VAR oList := GetDevelopers()
	VAR oAll   := FROM Developer IN oList ORDERBY Developer:Country, Developer:LastName SELECT Developer    
	VAR oGreek := FROM Developer IN oList WHERE Developer:Country == "Greece" ORDERBY Developer:LastName DESCENDING SELECT Developer 
	VAR oCount := FROM Developer IN oList GROUP Developer BY Developer:Country INTO NewGroup ORDERBY NewGroup:Key SELECT NewGroup
	WriteLine(e"X# does LINQ!\n")
	WriteLine(e"All X# developers (country+lastname order)\n")    
	FOREACH oDeveloper AS Developer IN oAll
		WriteLine(e"{0} in {1}",oDeveloper:Name, oDeveloper:Country)
	NEXT                                  
    
	WriteLine(e"\nGreek X# Developers (descending lastname)\n")    
	FOREACH oDeveloper AS Developer IN oGreek
		WriteLine(oDeveloper:Name)
	NEXT                         
	
	WriteLine(e"\nDevelopers grouped per country\n")

	FOREACH VAR country IN oCount
		WriteLine("{0}, {1} developer(s)", country:Key, country:Count())
		FOREACH VAR oDeveloper IN country
			WriteLine("  " + oDeveloper:Name)
		NEXT
	NEXT                         
	WriteLine("Enter to continue")
	ReadLine()
	RETURN
	

FUNCTION GetDevelopers AS IList<Developer>
	VAR oList := List<Developer>{}
	oList:Add(Developer{ "Robert", "van der Hulst", "The Netherlands"})
	oList:Add(Developer{ "Chris", "Pyrgas", "Greece"})
	oList:Add(Developer{ "Fabrice", "Foray", "France"})
	oList:Add(Developer{ "Nikos", "Kokkalis", "Greece"})
	RETURN oList
	
			
CLASS Developer	                
	PROPERTY Name 		AS STRING GET FirstName + " " + LastName
	PROPERTY FirstName 	AS STRING AUTO
	PROPERTY LastName 	AS STRING AUTO
	PROPERTY Country 	AS STRING AUTO
	CONSTRUCTOR(cFirst AS STRING, cLast AS STRING, cCountry AS STRING)
		FirstName := cFirst
		LastName := cLast  
		Country := cCountry         
END CLASS
