class TwoBmpButton inherit PushButton
	protect oBmpUnpressed as Bitmap
	protect oBmpPressed as Bitmap
	

// constructor inserted by xPorter, remove superfluous arguments
CONSTRUCTOR(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) CLIPPER
SUPER(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
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
