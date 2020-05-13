


CLASS Bitmap INHERIT VObject
	PROTECT oImage AS System.Drawing.Image


	ACCESS __Image AS System.Drawing.Image
		RETURN oImage		

	ACCESS __Bitmap AS System.Drawing.Bitmap
		RETURN (System.Drawing.Bitmap) oImage		
	
	METHOD __SetImage(oNewImage AS System.Drawing.Image) AS VOID
		IF (oImage != NULL_OBJECT)
			oImage:Dispose()
		ENDIF
		oImage := oNewImage
		RETURN 
		

	METHOD Destroy() AS USUAL CLIPPER
		
		IF (oImage != NULL_OBJECT)
			oImage:Dispose()
			oImage := NULL_OBJECT
		ENDIF
		SUPER:Destroy()
		RETURN NIL
	

	METHOD Handle() AS IntPtr STRICT
		//Todo ?
		RETURN IntPtr.Zero
		
	CONSTRUCTOR(xResourceID, kLoadOption, iWidth, iHeight) 
		//Todo handle various options
		LOCAL hInst AS PTR
		LOCAL lpszBitmap AS PTR
		LOCAL hBitMap as PTR
		LOCAL oResourceID as ResourceID
		
		SUPER()
		
		IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
			oResourceID := ResourceID{xResourceID}
		ELSEIF IsPtr(xResourceID) //
			oImage := System.Drawing.Image.FromHbitmap((IntPtr) xResourceID)
			RETURN 
		ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
			WCError{#Init, #Bitmap, __WCSTypeError, xResourceID, 1}:@@Throw()
			GC.SuppressFinalize( SELF )
		ELSE
			oResourceID := xResourceID
		ENDIF

		DEFAULT(@kLoadOption, LR_DEFAULTCOLOR)
		IF ! IsLong(iWidth)
			iWidth := 0
		ENDIF
		IF ! IsLong(iHeight)
			iHeight := 0
		ENDIF

		IF IsString(xResourceID) .and. File(xResourceID)
			oImage := System.Drawing.Image.FromFile(xResourceID)
		ELSE
			hInst := oResourceID:Handle() 
			lpszBitmap := oResourceID:Address()
		
			hBitMap := Win32.LoadImage(hInst, lpszBitmap, IMAGE_BITMAP, iWidth, iHeight, kLoadOption)
			IF hBitMap != NULL_PTR
				oImage  := System.Drawing.Image.FromHbitmap(hBitMap)
			ENDIF
		ENDIF		
		
		
		RETURN 
	

	ACCESS Size AS Dimension
		IF oImage != NULL_OBJECT
			RETURN (Dimension) oImage:Size
		ENDIF
		RETURN Dimension{}
		
	METHOD FromFile(cFile AS STRING	) AS LOGIC
		IF File(cFile)
			IF ! oImage == NULL_OBJECT
				oImage:Dispose()
			ENDIF

			oImage := System.Drawing.Image.FromFile(cFile)
			RETURN oImage != NULL_OBJECT
		ENDIF
		RETURN FALSE
END CLASS

