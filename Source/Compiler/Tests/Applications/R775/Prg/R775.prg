// R775 Bug found with Xs2Ado: Using Defaultmember with Indexed Property with name Item on a subclass
// generates a stackoverflow exception
USING System.Reflection
FUNCTION Start( ) AS VOID
    LOCAL oTest AS Parent
    oTest := Parent{}
    ? otest[1]
    ? oTest["name"]
RETURN


[DefaultMember("Item")];
CLASS Parent
    VIRTUAL PROPERTY Item[uKey AS USUAL] AS USUAL
    GET           
        ? uKey 
        RETURN "Parent"
    END GET
    END PROPERTY
END CLASS    

[DefaultMember("Item")];
CLASS Child INHERIT Parent
    OVERRIDE PROPERTY Item[uKey AS USUAL] AS USUAL
    GET           
        ? uKey 
        RETURN "Child"
    END GET
    END PROPERTY
END CLASS     

