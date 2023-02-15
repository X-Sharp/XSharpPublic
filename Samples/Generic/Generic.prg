/*
Stack Example - Written by Robert van der Hulst
This example shows that we can now create generic classes with X# !
Note: Compile with the /AZ option
*/
USING System.Collections.Generic
USING STATIC System.Console
FUNCTION Start AS VOID
	LOCAL oStack AS Stack<INT>                    
	LOCAL i AS LONG                      
	TRY
	oStack := Stack<INT>{25}
   	WriteLine("Created a stack with {0} items",oStack:Capacity)
   	WriteLine("Pushing 10 items")
   	FOR I := 1 TO 10
   		oStack:Push(i)
   	NEXT
   	WriteLine("Popping the stack until it is empty")
	i := 0
	WHILE oStack:Size > 0
	    i += 1
		WriteLine(oStack:Pop())
	END
	WriteLine("{0} Items popped from the stack",i)
    WriteLine("Press Enter")
	ReadLine()
	WriteLine("The next line pops from an empty stack and throws an exception")	
	ReadLine()
    WriteLine(oStack:Pop())
	CATCH e AS Exception
		WriteLine("An exception was catched: {0}", e:Message)
	END TRY
    WriteLine("Press Enter to Exit")
	ReadLine()
	RETURN                             


CLASS Stack<T>  WHERE T IS STRUCT
	PROTECT _Items 		AS T[] 
	PROTECT _Size 		AS INT
	PROTECT _Capacity 	AS INT  
	PROPERTY Size 		AS INT GET _Size 
	PROPERTY Capacity 	AS INT GET _Capacity
	
	CONSTRUCTOR()
		SELF(100)     
		
	CONSTRUCTOR(nCapacity AS INT)
		_Capacity := nCapacity
		_Items := T[]{nCapacity}
		_Size  := 0
		RETURN

	PUBLIC METHOD Push( item AS T) AS VOID
		IF _Size >= _Capacity
			THROW StackOverFlowException{}
		ENDIF
		_Items[_Size] := item
		_Size++              
		RETURN

	PUBLIC METHOD Pop( ) AS T
		 _Size--
		 IF _Size >= 0
		 	RETURN _Items[_Size]
		 ELSE 
		 	_Size := 0
		 	THROW Exception{"Cannot pop from an empty stack"}
		 ENDIF 
END CLASS
