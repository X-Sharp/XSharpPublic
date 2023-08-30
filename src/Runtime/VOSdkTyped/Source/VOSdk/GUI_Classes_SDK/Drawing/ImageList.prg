//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



/// <include file="Gui.xml" path="doc/ImageList/*" />

CLASS ImageList INHERIT VObject
	PROTECT hDragWindow AS IntPtr
	PROTECT iDragYOffset AS INT
	PROTECT oImageList AS System.Windows.Forms.ImageList

    INTERNAL PROPERTY __ImageList AS System.Windows.Forms.ImageList GET oImageList
/// <include file="Gui.xml" path="doc/ImageList.Add/*" />
	METHOD Add(oImage AS OBJECT)  AS LONG
		LOCAL nReturnValue AS INT

		IF (oImageList != NULL_OBJECT)
			IF oImage IS VO.SDK.Bitmap VAR oBM
				oImageList:Images:Add( oBM)
				nReturnValue :=  oImageList:Images:Count
			ELSEIF oImage IS VO.SDK.Icon var oIcon
				oImageList:Images:Add(oIcon)
				nReturnValue :=  oImageList:Images:Count
			ELSE
				WCError{#Add, #ImageList, __WCSTypeError, oImage, 1}:Throw()
			ENDIF
		ENDIF

		RETURN nReturnValue

/// <include file="Gui.xml" path="doc/ImageList.AddMask/*" />
	METHOD AddMask(oBitmap, oMaskColor)
		//Todo AddMask
		//LOCAL dwColor AS DWORD

		//IF (oImageList != NULL_OBJECT)
		//	IF IsInstanceOfUsual(oMaskColor,#color)
		//		dwColor := oMaskColor:ColorRef
		//	ENDIF
		//	RETURN ImageList_AddMasked(hImageList, oBitmap:Handle(), dwColor) + 1
		//ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ImageList.BeginDrag/*" />
	METHOD BeginDrag(nIndex)
		//DEFAULT( REF nIndex, 1)
		//RETURN ImageList_BeginDrag(hImageList, nIndex - 1, 1, 1)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ImageList.CreateOverlayImage/*" />
	METHOD CreateOverlayImage(nImageIndex, nListIndex)
		//Todo CreateOverlayImage
		//DEFAULT( REF nListIndex, 1)

		//IF nListIndex < 1 .or. nListIndex > 4
		//	RETURN FALSE
		//ENDIF

		//RETURN ImageList_SetOverlayImage(hImageList, nImageIndex - 1, nListIndex)
		RETURN TRUE

	//METHOD Destroy() AS USUAL STRICT
		//IF (oImageList != NULL_PTR)
		//	oImageList:Dispose()
		//	oImageList := NULL_OBJECT
		//ENDIF

		//SUPER:Destroy()

		//RETURN NIL

/// <include file="Gui.xml" path="doc/ImageList.DragEnter/*" />
	METHOD DragEnter(oPoint, oWindow)
		////SE-080520
		//LOCAL liPointY AS LONG

		//liPointY     := oPoint:Y
		//iDragYOffset := oWindow:Size:Height


		//hDragWindow  := oWindow:Handle()

		//RETURN ImageList_DragEnter(hDragWindow, oPoint:X, liPointY)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ImageList.DragLeave/*" />
	METHOD DragLeave()
		//RETURN ImageList_DragLeave(hDragWindow)
		RETURN TRUE
/// <include file="Gui.xml" path="doc/ImageList.DragMove/*" />
	METHOD DragMove(oPoint)
		////SE-080520
		//LOCAL liPointY AS LONG

		//liPointY := oPoint:Y


		//RETURN ImageList_DragMove(oPoint:X, liPointY)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ImageList.EndDrag/*" />
	METHOD EndDrag()
		//ImageList_EndDrag()

		//hDragWindow := NULL_PTR
		//iDragYOffset := 0

		RETURN TRUE

/// <include file="Gui.xml" path="doc/ImageList.Handle/*" />
	METHOD Handle() AS IntPtr STRICT
		RETURN oImageList:Handle

/// <include file="Gui.xml" path="doc/ImageList.ImageCount/*" />
	ACCESS ImageCount AS LONG
		IF oImageList != NULL_OBJECT
			RETURN oImageList:Images:Count
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ImageList.ImageSize/*" />
	ACCESS ImageSize AS Dimension
		IF oImageList != NULL_OBJECT
			RETURN (Dimension) oImageList:ImageSize
		ENDIF

		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/ImageList.ctor/*" />
	CONSTRUCTOR(nImages, oDimension, oImage, wColor, nGrow)
		//LOCAL dwCol AS DWORD

		SUPER()

		IF IsObject (nImages)
			oImageList := nImages
			//hImageList := nImages
		ELSE
			//PP-031129 Initialisation of wCol changed
			//IF IsNumeric(wColor)
			//	dwCol := wColor
			//ELSE
			//	dwCol := _Or(ILC_COLOR4, ILC_MASK)
			//ENDIF

			//IF ! IsNumeric(nGrow)
			//	nGrow := 1L
			//ENDIF
			//PP-031115 Allow imagelist without a mask if appropriate wColor specified
			//hImageList := ImageList_Create(oDimension:Width, oDimension:Height, dwCol, nImages, nGrow)
			oImageList := System.Windows.Forms.ImageList{}
			if oDimension is DImension var oDim
				oImageList:ImageSize := oDim
			ENDIF
		ENDIF

		IF oImage IS Bitmap .or. oImage IS Icon
			SELF:Add(oImage)
		ENDIF

		RETURN

	OPERATOR IMPLICIT ( i AS System.Windows.Forms.ImageList) AS ImageList
		RETURN ImageList{i}

	OPERATOR IMPLICIT ( i AS ImageList ) AS System.Windows.Forms.ImageList
		RETURN i:oImageList

END CLASS

