// Spinner control does not exist in .NET
// There is a NumericUpdown control that combines an edit and a spinner
// To make our code happy I have added the classes as subclasses from ScrollBar

//Todo Implement Spinner
CLASS Spinner INHERIT ScrollBar
	PROTECT oClient as Control
	PROPERTY Client as Control GET oClient SET oClient := value
	PROPERTY Position AS INT GET __ScrollBar:Value SET __ScrollBar:Value := Value
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware)
END CLASS

CLASS HorizontalSpinner INHERIT Spinner

    PROPERTY ControlType AS ControlType GET ControlType.HorizontalScrollBar
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware)
        
END CLASS

CLASS VerticalSpinner INHERIT Spinner
    PROPERTY ControlType AS ControlType GET ControlType.VerticalScrollBar

    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) CLIPPER
	   SUPER(oOwner,xID,oPoint,oDimension,lDataAware) 
	
	

END CLASS
