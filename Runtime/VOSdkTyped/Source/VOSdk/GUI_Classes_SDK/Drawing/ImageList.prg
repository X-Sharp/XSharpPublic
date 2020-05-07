



CLASS ImageList INHERIT VObject
	PROTECT hDragWindow AS IntPtr
	PROTECT iDragYOffset AS INT
	PROTECT oImageList AS System.Windows.Forms.ImageList
	
	ACCESS __ImageList AS System.Windows.Forms.ImageList
		RETURN oImageList
	
	METHOD Add(oImage AS OBJECT)  AS LONG
		LOCAL nReturnValue AS INT

		IF (oImageList != NULL_OBJECT)
			IF IsInstanceOfUsual(oImage, #Bitmap)
				oImageList:Images:Add( ((Bitmap) oImage):__Image)
				nReturnValue :=  oImageList:Images:Count
			ELSEIF IsInstanceOfUsual(oImage, #Icon)
				oImageList:Images:Add(((Icon) oImage):__Icon)
				nReturnValue :=  oImageList:Images:Count
			ELSE
				WCError{#Add, #ImageList, __WCSTypeError, oImage, 1}:@@Throw()
			ENDIF
		ENDIF

		RETURN nReturnValue

	METHOD AddMask(oBitmap, oMaskColor) 
		//Todo
		//LOCAL dwColor AS DWORD

		//IF (oImageList != NULL_OBJECT)
		//	IF IsInstanceOfUsual(oMaskColor,#color)
		//		dwColor := oMaskColor:ColorRef
		//	ENDIF
		//	RETURN ImageList_AddMasked(hImageList, oBitmap:Handle(), dwColor) + 1
		//ENDIF
		RETURN 0

	METHOD BeginDrag(nIndex) 
		//DEFAULT(@nIndex, 1)
		//RETURN ImageList_BeginDrag(hImageList, nIndex - 1, 1, 1)
		RETURN TRUE

	METHOD CreateOverlayImage(nImageIndex, nListIndex) 
		//Todo
		//Default(@nListIndex, 1)

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

	METHOD DragEnter(oPoint, oWindow) 
		////SE-080520
		//LOCAL liPointY AS LONG
		
		//liPointY     := oPoint:Y 
		//iDragYOffset := oWindow:Size:Height
		
		
		//hDragWindow  := oWindow:Handle()

		//RETURN ImageList_DragEnter(hDragWindow, oPoint:X, liPointY)
		RETURN TRUE

	METHOD DragLeave() 
		//RETURN ImageList_DragLeave(hDragWindow)
		RETURN TRUE
	METHOD DragMove(oPoint) 
		////SE-080520 
		//LOCAL liPointY AS LONG
		
		//liPointY := oPoint:Y 
		
	
		//RETURN ImageList_DragMove(oPoint:X, liPointY)
		RETURN TRUE

	METHOD EndDrag() 
		//ImageList_EndDrag()

		//hDragWindow := NULL_PTR
		//iDragYOffset := 0

		RETURN TRUE

	METHOD Handle() AS IntPtr STRICT
		RETURN oImageList:Handle

	ACCESS ImageCount AS LONG
		IF oImageList != NULL_OBJECT
			RETURN oImageList:Images:Count
		ENDIF
		RETURN 0

	ACCESS ImageSize AS Dimension
		IF oImageList != NULL_OBJECT
			RETURN (Dimension) oImageList:ImageSize
		ENDIF

		RETURN NULL_OBJECT


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
			IF IsInstanceOfUsual(oDimension, #DImension)
				oImageList:ImageSize := (Dimension) oDimension
			ENDIF
		ENDIF

		IF IsInstanceOfUsual(oImage, #Bitmap) .or. IsInstanceOfUsual(oImage, #Icon)
			SELF:Add(oImage)
		ENDIF

		

		RETURN 

	OPERATOR IMPLICIT ( i AS System.Windows.Forms.ImageList) AS ImageList
		RETURN ImageList{i}

	OPERATOR IMPLICIT ( i AS ImageList ) AS System.Windows.Forms.ImageList
		RETURN i:__ImageList
	
END CLASS

