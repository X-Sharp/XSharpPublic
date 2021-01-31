// Spinner control does not exist in .NET
// There is a NumericUpdown control that combines an edit and a spinner
// To make our code happy I have added the classes as subclasses from ScrollBar

//Todo Implement Spinner
CLASS Spinner INHERIT Control
	PROTECT oClient as Control
	PROPERTY Client AS Control GET oClient SET oClient := VALUE
    ACCESS __UpDown AS System.Windows.Forms.NumericUpDown
		RETURN (System.Windows.Forms.NumericUpDown) oCtrl        
	PROPERTY Position AS INT GET __UpDown:Value SET __UpDown:Value := VALUE

    PROPERTY Range AS Range
        GET
           RETURN Range{__UpDown:Minimum, __UpDown:Maximum}
        END GET
        SET
            __UpDown:Minimum := VALUE:Min
            __UpDown:Maximum := VALUE:Max
        END SET
    END PROPERTY
    PROPERTY ThumbPosition AS LONG GET __UpDown:Value SET __UpDown:Value := VALUE
    PROPERTY UnitSize AS LONG GET __UpDown:Increment SET __UpDown:Increment := Value

    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware)
END CLASS

CLASS HorizontalSpinner INHERIT Spinner
    PROPERTY ControlType AS ControlType GET ControlType.HorizontalSpinner
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware)
        
END CLASS

CLASS VerticalSpinner INHERIT Spinner
    PROPERTY ControlType AS ControlType GET ControlType.VerticalSpinner
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware) 

END CLASS
