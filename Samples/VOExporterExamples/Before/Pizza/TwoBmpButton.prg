class TwoBmpButton inherit PushButton
	protect oBmpUnpressed as Bitmap
	protect oBmpPressed as Bitmap
	

assign BmpUnpressed(oNewBmp) 
	oBmpUnpressed := oNewBmp
	self:Image := oBmpUnpressed
	return nil

assign BmpPressed(oNewBmp) 
	oBmpPressed := oNewBmp
	return nil

method Dispatch(oEvt) 

  if oEvt:uMsg == WM_LBUTTONDOWN
    self:Image := oBmpPressed
  elseif oEvt:uMsg == WM_LBUTTONUP
  	self:Image := oBmpUnPressed
  endif

  return super:Dispatch(oEvt)	
	


END CLASS
