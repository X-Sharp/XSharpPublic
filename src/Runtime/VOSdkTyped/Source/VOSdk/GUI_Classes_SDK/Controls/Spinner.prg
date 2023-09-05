//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Spinner control does not exist in .NET
// There is a NumericUpdown control that combines an edit and a spinner
// To make our code happy I have added the classes as subclasses from ScrollBar

//Todo Implement Spinner
ABSTRACT CLASS Spinner INHERIT Control
    PROTECT oClient as Control

    /// <include file="Gui.xml" path="doc/Spinner.Client/*" />
    PROPERTY Client AS Control GET oClient SET oClient := VALUE
    /// <exclude />
    PROPERTY __UpDown AS System.Windows.Forms.NumericUpDown GET (System.Windows.Forms.NumericUpDown) oCtrl

    /// <include file="Gui.xml" path="doc/Spinner.Position/*" />
	PROPERTY Position AS INT GET Convert.ToInt32(__UpDown:Value) SET __UpDown:Value := VALUE

    /// <include file="Gui.xml" path="doc/Spinner.Range/*" />
    PROPERTY Range AS Range
        GET
           RETURN Range{__UpDown:Minimum, __UpDown:Maximum}
        END GET
        SET
            __UpDown:Minimum := VALUE:Min
            __UpDown:Maximum := VALUE:Max
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Spinner.ThumbPosition/*" />
    PROPERTY ThumbPosition AS LONG GET Convert.ToInt32(__UpDown:Value) SET __UpDown:Value := VALUE

    /// <include file="Gui.xml" path="doc/Spinner.UnitSize/*" />
    PROPERTY UnitSize AS LONG GET Convert.ToInt32(__UpDown:Increment) SET __UpDown:Increment := Value

    /// <include file="Gui.xml" path="doc/Spinner.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,kStyle)
END CLASS

CLASS HorizontalSpinner INHERIT Spinner
    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.HorizontalSpinner
    /// <include file="Gui.xml" path="doc/HorizontalSpinner.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,kStyle)

END CLASS

CLASS VerticalSpinner INHERIT Spinner
    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.VerticalSpinner
    /// <include file="Gui.xml" path="doc/VerticalSpinner.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,kStyle)

END CLASS
