class TwoBmpButton inherit PushButton
	protect oBmpUnpressed as Bitmap
	protect oBmpPressed as Bitmap
	

assign BmpPressed(oNewBmp) 
	oBmpPressed := oNewBmp
	return nil

assign BmpUnpressed(oNewBmp) 
	oBmpUnpressed := oNewBmp
	self:Image := oBmpUnpressed
	return nil

METHOD Dispatch(oEvt AS Event) AS LONG

  if oEvt:uMsg == WM_LBUTTONDOWN
    self:Image := oBmpPressed
  elseif oEvt:uMsg == WM_LBUTTONUP
  	self:Image := oBmpUnPressed
  endif

  return super:Dispatch(oEvt)	
	


END CLASS
