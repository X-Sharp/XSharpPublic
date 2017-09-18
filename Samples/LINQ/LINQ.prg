USING System.Collections.Generic    
USING System.Linq
USING STATIC System.Console

FUNCTION Start AS VOID
	VAR oDev   := GetDevelopers()
	VAR oC     := GetCountries()
	VAR oAll   := 	FROM D 		 IN oDev  ;
					JOIN C 		 IN oC ON D:Country EQUALS C:Name   ;
					ORDERBY D:LastName  ;
					SELECT CLASS {D:Name, D:Country, C:Region}	// Anonymous class !
	
	// The type of oAll is WhereSelectEnumerableIterator<<>f__AnonymousType0<Developer,Country>>
	// We prefer the VAR keyword!
			
	VAR oGreek := 	FROM Developer IN oDev ;
					WHERE Developer:Country == "Greece" ;
					ORDERBY Developer:LastName DESCENDING ;
					SELECT Developer 						
	
	// The type of oGreek is IOrderedEnumerable<Developer>
	// We prefer the VAR keyword!

	VAR oCount := 	FROM Developer IN oDev ;
					GROUP Developer BY Developer:Country INTO NewGroup ;
					ORDERBY NewGroup:Key SELECT NewGroup      
	
	// The type of oCount is IOrderedEnumerable<<IGrouping<string,Developer>>
	// We prefer the VAR keyword!

	WriteLine(e"X# does LINQ!\n")
	WriteLine(e"All X# developers (country+lastname order)\n")    
	FOREACH VAR oDeveloper IN oAll
		WriteLine("{0} in {1}, {2}",oDeveloper:Name,oDeveloper:Country,oDeveloper:Region)
	NEXT                                  
    
	WriteLine(e"\nGreek X# Developers (descending lastname)\n")    
	FOREACH oDeveloper AS Developer IN oGreek
		WriteLine(oDeveloper:Name)
	NEXT                         
	
	WriteLine(e"\nDevelopers grouped per country\n")

	FOREACH VAR country IN oCount
		WriteLine(country:Key+" " +country:Count():ToString()+" developer(s)") 
		FOREACH VAR oDeveloper IN country
			WriteLine("  " + oDeveloper:Name)
		NEXT
	NEXT                         
	WriteLine("Enter to continue")
	ReadLine()
	RETURN
	

FUNCTION GetDevelopers AS IList<Developer>        
	// This function uses a collection initializer for the List of Developers
	// and Object initializers for the Developer Objects
	VAR oList := List<Developer>{} 	{ ;
										Developer{}{ FirstName  := "Chris", LastName := "Pyrgas", Country := "Greece"},;
										Developer{}{ FirstName  := "Robert", LastName := "van der Hulst", Country := "The Netherlands"},;
										Developer{}{ FirstName  := "Fabrice", LastName := "Foray", Country := "France"},;
										Developer{}{ FirstName  := "Nikos", LastName := "Kokkalis", Country := "Greece"} ;
									}
	RETURN oList

FUNCTION GetCountries AS IList<Country>
	// This function uses a collection initializer for the List of Counties
	// and Object initializers for the Country Objects
	VAR oList := List<Country>{}{ ;
									Country{} {Name := "Greece", 			Region := "South East Europe"},;
									Country{} {Name := "France", 			Region := "West Europe"},;
									Country{} {Name := "The Netherlands", 	Region := "North West Europe"} ;
								}
	RETURN oList			
CLASS Developer	                
	PROPERTY Name 		AS STRING GET FirstName + " " + LastName
	PROPERTY FirstName 	AS STRING AUTO
	PROPERTY LastName 	AS STRING AUTO
	PROPERTY Country 	AS STRING AUTO
END CLASS

CLASS Country
	PROPERTY Name 		AS STRING AUTO
	PROPERTY Region 	AS STRING AUTO
END CLASS

		