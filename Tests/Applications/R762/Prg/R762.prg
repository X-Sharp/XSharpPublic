// R762 Make sure that overloaded constructors are not seen as "changed" by the new [TypesChanged] code.
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
RETURN


[XSharp.Internal.TypesChanged];
CLASS DataField
	PROTECT oFieldSpec		AS STRING
	PROTECT oHyperLabel	    AS LONG

CONSTRUCTOR()
    
CONSTRUCTOR( oHLName AS STRING)


CONSTRUCTOR( oHLName AS LONG) 
	RETURN 

END CLASS





