// Aliases to functions in XSharp.RT
USING System
USING XSharp.RDD

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/orddescend/*" />
FUNCTION DbDescend() AS LOGIC
    RETURN OrdDescend()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/orddescend/*" />
FUNCTION DbSetDescend(lNewDescend AS LOGIC) AS LOGIC
    RETURN OrdDescend(NIL,NIL, lNewDescend)


/// <summary>Attaches an arbitrary value to a used work area </summary>
/// <param name="xNewValue">the value to attach to a work area. </param>
/// <returns>The function returns the value attached to a work area before the function is called. </returns>
/// <remarks>The function DbCargo() attaches an arbitrary value to a used work area. The value remains in the work area,
/// In this way, user defined data can be bound to a particular work area. Attached data is discarded when the workarea
/// is closed.</remarks>

FUNCTION DbCargo(xNewValue) AS USUAL
    LOCAL nArea AS DWORD
    LOCAL old   AS OBJECT
    LOCAL wa := XSharp.RuntimeState.Workareas AS Workareas
    nArea   := wa:CurrentWorkAreaNO
    old     := wa:GetCargo(nArea)
    IF ! IsNil(xNewValue)
        wa:SetCargo(nArea, xNewValue)
    ENDIF
    RETURN old
    
