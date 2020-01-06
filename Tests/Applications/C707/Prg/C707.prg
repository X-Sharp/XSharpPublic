// 707. Name conflict between memvar and class
// slightly diffrnt case than #703
FUNCTION Start() AS VOID
	PushButton := PushButton{}
	// error XS0120: An object reference is required for the non-static field, method, or property 'PushButton.HyperLabel'
	? PushButton:HyperLabel
RETURN

CLASS PushButton
	EXPORT HyperLabel AS INT
END CLASS
