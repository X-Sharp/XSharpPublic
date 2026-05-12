
// Class OptionButton  BaseClass   Optionbutton Class  Optionbutton
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS OptionButton  IMPLEMENTS IVFPControl, IVFPImage
#include "VFPControl.xh"
#define NO_PICTUREPOSITION
#include "VFPImage.xh"
		// Style, WordWrap, Alignment, PicturePosition — implemented via VFPButtonImage.xh in OptionButton.prg
		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow  AS LOGIC AUTO
	END CLASS
	END NAMESPACE