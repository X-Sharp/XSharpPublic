CLASS TwoBmpButton INHERIT PushButton
	PROTECT oBmpUnpressed AS Bitmap
	PROTECT oBmpPressed AS Bitmap

    CONSTRUCTOR() CLIPPER
        SUPER(_Args())
        SELF:EnableDispatch := TRUE


ASSIGN BmpPressed(oNewBmp)
	oBmpPressed := oNewBmp
	RETURN NIL

ASSIGN BmpUnpressed(oNewBmp)
	oBmpUnpressed := oNewBmp
	SELF:Image := oBmpUnpressed
	RETURN NIL

METHOD Dispatch(oEvt  AS @@Event)  AS INT
  IF oEvt:uMsg == WM_LBUTTONDOWN
    SELF:Image := oBmpPressed
  ELSEIF oEvt:uMsg == WM_LBUTTONUP
  	SELF:Image := oBmpUnPressed
  ENDIF

  RETURN SUPER:Dispatch(oEvt)



END CLASS
