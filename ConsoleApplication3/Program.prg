USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION MyFile(cFile AS STRING) AS STRING
? cFile
RETURN cFile

FUNCTION Start( ) AS VOID
    VAR aList := OrdList()
    RegisterFileSearch(MyFile)
    ? "No open table"
    FOREACH name AS STRING IN aList
        ? name
    NEXT
    ? DbSetScope(SCOPE_BOTH,"C")
    DBUseArea(TRUE,"DBFNTX","C:\Cavo28sp3\Samples\gsTutor\Customer","CustName",FALSE,FALSE)
    DBSetIndex("CustName")
    OrdListAdd("CustNum")
    OrdListAdd("")
    OrdListAdd("   ")
    OrdListAdd(null)
    aList := OrdList()
    ? "Customer with 2 indexes"
    FOREACH name AS STRING IN aList
        ? name
    NEXT
    OrdSetFocus("CustName")
    DbSetScope(SCOPE_TOP,"A")
    DbSetScope(SCOPE_BOTTOM,"B")
    ? "Has Scope", DbScope()
    ? "Top", DbScope(SCOPE_TOP)
    ? "Bottom",DbScope(SCOPE_BOTTOM)
    ? "Both", DbScope(SCOPE_BOTH)[1], DbScope(SCOPE_BOTH)[2]
    DbSetScope(SCOPE_BOTH,"C")
    ? "Top", DbScope(SCOPE_TOP)
    ? "Bottom",DbScope(SCOPE_BOTTOM)
    ? "Both", DbScope(SCOPE_BOTH)[1], DbScope(SCOPE_BOTH)[2]
    Wait
    
    RETURN

    /// <summary>Return a list of all tag names for the current work area. </summary>
    /// <returns>OrdList() returns a one dimensional array holding strings with the tag names of all open indexes.
    /// When no index is open, an empty array is returned. </returns>
    FUNCTION OrdList() AS ARRAY STRICT 
    LOCAL aResult AS ARRAY        
    LOCAL nIndex, nCount AS DWORD
    aResult := {}                
    nCount := OrdCount()
    FOR nIndex := 1 UPTO nCount
    AAdd(aResult, DBOrderInfo(DBOI_NAME, ,nIndex) )
    NEXT
    RETURN aResult
    
    /// <summary>Determines the number of orders for the current work area.  </summary>
    /// <returns>OrdCount() returns the number of open indexes as a numeric value.
    /// When no index is open or when no file is open in the current workarea, the return value is 0. </returns>
    FUNCTION OrdCount() AS DWORD
    RETURN IIF (Used(), DBOrderInfo(DBOI_ORDERCOUNT),0)

    
    /// <summary>Checks if a scope is set in a work area.</summary>
    /// <param name="uScope">A constant that indicates which scope to test.
    /// <include file="RTComments.xml" path="path="Comments/ScopeParams/*"  />
    /// </param>
    /// <returns>
    /// The return value depends on the parameter that is passed in:<br/>
    ///   <list type="table">
    ///   <item>
    ///   <term>no parameter</term>
    ///   <description>Returns .T. (true) if a scope is defined, and .F. when no scope is defined</description>
    ///   </item>
    ///   <item>
    ///   <term>SCOPE_TOP</term>
    ///   <description>Returns the top scope value, or NIL if no top scope is set</description>
    ///   </item>
    ///   <item>
    ///     <term>SCOPE_BOTTOM</term>
    ///   <description>Returns the bottom scope value, or NIL if no bottom scope is set</description>
    ///   </item>
    ///   <item>
    ///     <term>SCOPE_BOTH</term>
    ///     <description>Returns an array with 2 elements with the Top scope in element 1 and the Bottom scope in element 2. When a scope is not set then the value NIL is stored in the array</description>
    ///   </item>
    /// </list>
/// </returns>
FUNCTION DbScope(uScope) AS USUAL
    LOCAL nScope AS LONG
    IF IsNil(uScope)
        uScope := OrdScope(TOPSCOPE)
        IF isNil(uScope)
            uScope := OrdScope(BOTTOMSCOPE)
        ENDIF
        RETURN ! IsNil(uScope)
    ENDIF
    EnForceNumeric(uScope)
    nScope := uScope
    SWITCH nScope
    CASE SCOPE_TOP
        uScope := OrdScope(TOPSCOPE)
    CASE SCOPE_BOTTOM
        uScope := OrdScope(BOTTOMSCOPE)
    CASE SCOPE_BOTH
        uScope := {OrdScope(TOPSCOPE),OrdScope(BOTTOMSCOPE)}
    OTHERWISE
        uScope := NIL
    END SWITCH
    RETURN uScope
    
    /// <summary>Sets scope values.</summary>
    /// <param name="uScope">A constant that indicates which scope needs to be set.
    /// <include file="RTComments.xml" path="path="Comments/ScopeParams/*"  />
    /// </param>
    /// <param name="uValue">The value that needs to be set. The type of the value must match the type of the index expression.</param>
    /// <returns>TRUE when the scope was set succesfully, otherwise FALSE.</returns>
FUNCTION DbSetScope(nScope AS LONG, uValue AS USUAL) AS LOGIC
    LOCAL lResult := TRUE AS LOGIC
    TRY
        SWITCH nScope
        CASE SCOPE_TOP
            OrdScope(TOPSCOPE,uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
                
        CASE SCOPE_BOTTOM
            OrdScope(BOTTOMSCOPE, uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
                
        CASE SCOPE_BOTH
            OrdScope(TOPSCOPE,uValue)
            OrdScope(BOTTOMSCOPE, uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
        
        OTHERWISE
            lResult := FALSE
        END SWITCH
    CATCH AS Exception
        lResult := FALSE
    END TRY
    RETURN lResult
