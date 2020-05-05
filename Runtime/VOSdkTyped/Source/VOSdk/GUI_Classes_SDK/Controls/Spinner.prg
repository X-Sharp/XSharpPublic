// Spinner control does not exist in .NET
// There is a NumericUpdown control that combines an edit and a spinner
// To make our code happy I have added the classes as subclasses from ScrollBar

//Todo Implement Spinner
CLASS Spinner INHERIT ScrollBar
	PROTECT oClient as Control
	PROPERTY Client as Control GET oClient SET oClient := value
	PROPERTY Position AS INT GET __ScrollBar:Value SET __ScrollBar:Value := Value
END CLASS

CLASS HorizontalSpinner INHERIT Spinner

    PROPERTY ControlType AS ControlType GET ControlType.HorizontalScrollBar
        
END CLASS

CLASS VerticalSpinner INHERIT Spinner
    PROPERTY ControlType AS ControlType GET ControlType.VerticalScrollBar

	
	

END CLASS
