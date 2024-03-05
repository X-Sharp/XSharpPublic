/// <include file="Gui.xml" path="doc/ImageList/*" />
CLASS ImageList INHERIT VObject
	PROTECT hImageList AS PTR
	PROTECT hDragWindow AS PTR
	PROTECT iDragYOffset AS INT


/// <include file="Gui.xml" path="doc/ImageList.Add/*" />
METHOD Add(oImage)
	LOCAL nReturnValue AS INT






	IF (hImageList != NULL_PTR)
		IF oImage IS Bitmap var oBmp
			nReturnValue := ImageList_Add(hImageList, oBmp:Handle(), NULL_PTR) + 1
		ELSEIF oImage IS Icon var oIcon
			nReturnValue := ImageList_AddIcon(hImageList, oIcon:Handle()) + 1
		ELSE
			WCError{#Add, #ImageList, __WCSTypeError, oImage, 1}:Throw()
		ENDIF
	ENDIF


	RETURN nReturnValue


/// <include file="Gui.xml" path="doc/ImageList.AddMask/*" />
METHOD AddMask(oBitmap, oMaskColor)
	//PP-031115 Allow specification of mask color
	LOCAL dwColor AS DWORD






	IF (hImageList != NULL_PTR)
		IF oMaskColor IS color var oCol
			dwColor := oCol:ColorRef
		ENDIF
		RETURN ImageList_AddMasked(hImageList, oBitmap:Handle(), dwColor) + 1
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/ImageList.BeginDrag/*" />
METHOD BeginDrag(nIndex)






	Default(@nIndex, 1)


	RETURN ImageList_BeginDrag(hImageList, nIndex - 1, 1, 1)


/// <include file="Gui.xml" path="doc/ImageList.CreateOverlayImage/*" />
METHOD CreateOverlayImage(nImageIndex, nListIndex)






	Default(@nListIndex, 1)


	IF nListIndex < 1 .or. nListIndex > 4
		RETURN FALSE
	ENDIF


	RETURN ImageList_SetOverlayImage(hImageList, nImageIndex - 1, nListIndex)


/// <include file="Gui.xml" path="doc/ImageList.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	IF (hImageList != NULL_PTR)
		ImageList_Destroy(hImageList)
	ENDIF

	SUPER:Destroy()
	//RvdH 050307 Fix for bug #  13023
	IF ! InCollect()
		UnregisterAxit(SELF)
		SELF:hImageList := NULL_PTR
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/ImageList.DragEnter/*" />
METHOD DragEnter(oPoint, oWindow)
	//SE-080520
	LOCAL liPointY AS LONG


	liPointY     := oPoint:Y
	iDragYOffset := oWindow:Size:Height


   IF WCGetCoordinateSystem() = WCCartesianCoordinates // Cartesian Coordinate System
	   liPointY := iDragYOffset - liPointY
   ENDIF


	hDragWindow  := oWindow:Handle()


	RETURN ImageList_DragEnter(hDragWindow, oPoint:X, liPointY)




/// <include file="Gui.xml" path="doc/ImageList.DragLeave/*" />
METHOD DragLeave()




	RETURN ImageList_DragLeave(hDragWindow)


/// <include file="Gui.xml" path="doc/ImageList.DragMove/*" />
METHOD DragMove(oPoint)
	//SE-080520
	LOCAL liPointY AS LONG


	liPointY := oPoint:Y


   IF WCGetCoordinateSystem() = WCCartesianCoordinates // Cartesian Coordinate System
	   liPointY := iDragYOffset - liPointY
   ENDIF


	RETURN ImageList_DragMove(oPoint:X, liPointY)


/// <include file="Gui.xml" path="doc/ImageList.EndDrag/*" />
METHOD EndDrag()






	ImageList_EndDrag()


	hDragWindow := NULL_PTR
	iDragYOffset := 0


	RETURN NIL


/// <include file="Gui.xml" path="doc/ImageList.Handle/*" />
METHOD Handle() AS PTR
	RETURN hImageList


/// <include file="Gui.xml" path="doc/ImageList.ImageCount/*" />
ACCESS ImageCount
	//PP-031115
	IF hImageList != Null_Ptr
		RETURN ImageList_GetImageCount(hImageList)
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/ImageList.ImageSize/*" />
ACCESS ImageSize
LOCAL cx	AS INT
LOCAL cy	AS INT


	// DHer: 18/12/2008
	IF SELF:hImageList<>NULL_PTR
		IF ImageList_GetIconSize(SELF:hImageList,@cx,@cy)=TRUE
			RETURN Dimension{cx,cy}
		ENDIF
	ENDIF


RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/ImageList.ctor/*" />
CONSTRUCTOR(nImages, oDimension, oImage, wColor, nGrow)
	LOCAL dwCol AS DWORD
	//PP-040416 Update from S Ebert




	SUPER()


	IF IsPtr(nImages) .and. nImages != NULL_PTR
		hImageList := nImages
	ELSE
		//PP-031129 Initialisation of wCol changed
		IF IsNumeric(wColor)
			dwCol := wColor
		ELSE
			dwCol := _Or(ILC_COLOR4, ILC_MASK)
		ENDIF


		IF ! IsNumeric(nGrow)
			nGrow := 1l
		ENDIF
		//PP-031115 Allow imagelist without a mask if appropriate wColor specified
		hImageList := ImageList_Create(oDimension:Width, oDimension:Height, dwCol, nImages, nGrow)
	ENDIF


	IF (oImage IS Bitmap) .or. (oImage IS Icon)
		SELF:Add(oImage)
	ENDIF






	RETURN




END CLASS


