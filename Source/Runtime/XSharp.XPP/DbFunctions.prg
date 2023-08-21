//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Aliases to functions in XSharp.RT
USING System

/// <summary>Checks if the order for navigation is descending </summary>
/// <returns>The return value of DbDescend() is TRUE when the in the current work area is descending, otherwise FALSE is returned. </returns>
/// <remarks>The function DbDescend() tests if the order in the current work area is descending.
/// Refer to DbSetDescend() for more information about reversing the navigational order. 
/// </remarks>
/// <seealso cref='DbSetDescend' >DbSetDescend()</seealso>
FUNCTION DbDescend() AS LOGIC
    RETURN OrdDescend()

/// <summary>Reverses the navigational order of a work area.</summary>
/// <param name="lNewDescend">A logical value. When TRUE the navigational order in the current work area is set to descending, FALSE sets the order to ascending. </param>
/// <returns>The return value of DbSetDescend() is the previous setting for DbSetDescend().</returns>
/// <remarks>The function DbSetDescend() is used to quickly change the order in a work area from ascending to descending and vice versa
/// without the need to create a corresponding index. When TRUE is passed to the function, the order is reversed, i.e. all functions and
/// commands that move the record pointer are inverted. DbGoTop() becomes DbGoBottom(), DbSkip(1) becomes DbSkip(-1), EOF becomes BOF() etc.
/// <note type="tip"> Not all RDDs support switching the order at runtime </note></remarks>
/// <seealso cref='DbDescend' >DbDescend()</seealso>
FUNCTION DbSetDescend(lNewDescend AS LOGIC) AS LOGIC
    LOCAL old := OrdDescend() AS LOGIC
    OrdDescend(NIL,NIL, lNewDescend)
    RETURN old


/// <summary>Attaches an arbitrary value to a used work area </summary>
/// <param name="xNewValue">the value to attach to a work area. </param>
/// <returns>The function returns the value attached to a work area before the function is called. </returns>
/// <remarks>The function DbCargo() attaches an arbitrary value to a used work area. The value remains in the work area,
/// In this way, user defined data can be bound to a particular work area. Attached data is discarded when the workarea
/// is closed.</remarks>

FUNCTION DbCargo(xNewValue) AS USUAL CLIPPER
    LOCAL nArea AS DWORD
    LOCAL old   AS OBJECT
    var wa := XSharp.RuntimeState.Workareas 
    nArea   := wa:CurrentWorkareaNO
    old     := wa:GetCargo(nArea) 
    IF PCOUNT() != 0
        wa:SetCargo(nArea, xNewValue)
    ENDIF
    RETURN old
    
