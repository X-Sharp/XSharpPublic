#ifndef __XSHARP__
#translate AS <type> => 
#endif

CLASS Developer
	CLASS VAR list    AS ARRAY
	CLASS VAR nextid  AS INT
	CLASS VAR random  AS Random
PROTECTED:
	VAR id AS INT NOSAVE     
EXPORTED:
	VAR FirstName     AS STRING
	VAR LastName      AS STRING
	VAR Country		  AS STRING
	INLINE METHOD initClass()
		list := {}
		nextid := 1
		random := Random{}
		RETURN
	INLINE METHOD SayHello() AS STRING
		IF ::Age < 40
			RETURN "Hello, I am " + ::FirstName+ " from "+::Country
		ELSE
			RETURN "Hello, I am mr " + ::LastName+ " from "+::Country+" but you can call me "+::FirstName
		ENDIF
	INLINE METHOD Init(cFirst AS STRING , cLast AS STRING, cCountry AS STRING)
		::FirstName := cFirst
		::LastName := cLast
		::Country  := cCountry
		::id := nextid
		nextid += 1
		AAdd(list, SELF)
		RETURN 
	INLINE METHOD FullName() AS STRING
		RETURN ::FirstName + " " + ::LastName

  INLINE METHOD Fire() AS LOGIC
    LOCAL nPos AS DWORD
    nPos := AScan(list, SELF)
    IF nPos > 0	
        ADel(list, nPos)
        ASize(list, ALen(list)-1)
        RETURN TRUE
    ENDIF
    RETURN FALSE
	SYNC METHOD LivesIn 
	ACCESS CLASS METHOD Length AS DWORD
	ACCESS METHOD Age AS INT
ENDCLASS

METHOD LivesIn(cCountry AS STRING) AS LOGIC
  RETURN Lower(::Country) == Lower(cCountry)

METHOD Age() AS INT
	LOCAL nAge AS INT
	nAge  := random:@@Next(25,60)
	RETURN nAge
	
CLASS METHOD Length AS DWORD
	RETURN ALen(list)
	
FUNCTION Main(a) AS INT
  LOCAL oDeveloper  AS Developer
  LOCAL aDevs := {} AS ARRAY
  LOCAL i AS INT
  IF PCount() > 0
	  ? "Parameters"
	  FOR i := 1 TO PCount()
		? _GetFParam(i)
	  NEXT
  ENDIF
  AAdd(aDevs, Developer():NEW("Chris", "Pyrgas", "Greece"))
  AAdd(aDevs, Developer():NEW("Nikos", "Kokkalis","Greece"))
  AAdd(aDevs, Developer():NEW("Fabrice", "Foray","France"))
  AAdd(aDevs, Developer():NEW("Robert", "van der Hulst","The Netherlands"))
  ? "# of devs before", Developer.Length
  FOR i := 1 TO ALen(aDevs)
    oDeveloper := aDevs[i]
	? "Fields", oDeveloper:FirstName, oDeveloper:LastName, oDeveloper:Country
	? "FullName",oDeveloper:FullName()
	? oDeveloper:SayHello()
	? "Greece ?", oDeveloper:LivesIn("Greece")
    ? "Fired", oDeveloper:Fire()
    ? "# of devs after firing", oDeveloper:FirstName, Developer.Length
  NEXT
  _wait()
RETURN  PCount()


