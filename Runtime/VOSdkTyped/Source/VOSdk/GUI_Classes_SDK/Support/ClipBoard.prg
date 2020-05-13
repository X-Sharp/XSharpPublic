


#USING System.Collections.Specialized

CLASS Clipboard INHERIT VObject

	METHOD Clear() AS VOID
		System.Windows.Forms.Clipboard.Clear()
		RETURN 

	ACCESS FileCount AS LONG
		LOCAL nCount AS LONG
		LOCAL aFiles AS StringCollection
		IF System.Windows.Forms.Clipboard.ContainsFileDropList()
			aFiles := System.Windows.Forms.Clipboard.GetFileDropList()
			nCount := aFiles:Count
		ENDIF
		RETURN nCount

	METHOD GetItemSize(kFormat AS LONG) AS LONG 
		LOCAL nSize AS LONG
		LOCAL oImage AS System.Drawing.Image
		LOCAL oStream AS System.IO.MemoryStream
		LOCAL bytes AS BYTE[]
		DO CASE
		CASE (kFormat == STRINGFORMAT)
			IF System.Windows.Forms.Clipboard.ContainsText()
				nSize := System.Windows.Forms.Clipboard.GetText():Length
			ENDIF
		CASE (kFormat == BITMAPFORMAT)
			IF System.Windows.Forms.Clipboard.ContainsImage()
				oImage := System.Windows.Forms.Clipboard.GetImage()
				oStream := System.IO.MemoryStream{}
				oImage:Save(oStream,System.Drawing.Imaging.ImageFormat.Bmp)
				bytes := oStream:GetBuffer()
				nSize := bytes:Length
				oStream:Close()
			ENDIF

		OTHERWISE
			nSize := 0
		ENDCASE
		RETURN nSize

	
		

	METHOD Insert(xType AS USUAL)  AS LOGIC
		LOCAL lOk AS LOGIC
		IF IsString(xType)
			LOCAL cString AS STRING
			cString := xType
			IF !STRING.IsNullOrEmpty(cString)
				System.Windows.Forms.Clipboard.SetText( cString)
			ENDIF
			lOk := TRUE
		ELSEIF IsInstanceOfUsual(xType,#Bitmap)
			LOCAL oBitMap AS BitMap
			oBitMap := xType
			System.Windows.Forms.Clipboard.SetImage( (System.Drawing.Image) oBitMap:__Image)
			lOk := TRUE
		ELSE
			WCError{#Insert,#Clipboard,__WCSTypeError,xType,1}:@@Throw()
			lOk := FALSE
		ENDIF
		RETURN lOk

	METHOD InsertRTF(cText AS STRING)  AS LOGIC
		LOCAL lOk AS LOGIC
		System.Windows.Forms.Clipboard.SetText((STRING) cText, System.Windows.Forms.TextDataFormat.Rtf)
		lOk := TRUE
		RETURN lOk

	METHOD RetrieveBitmap(oBitmap AS Bitmap) as LOGIC
		LOCAL lRetVal AS LOGIC
		LOCAL oImage AS System.Drawing.Image
		IF System.Windows.Forms.Clipboard.ContainsImage()
			oImage := System.Windows.Forms.Clipboard.GetImage()
			oBitmap:__SetImage(oImage)
			lRetVal := TRUE			
		ENDIF
		
		RETURN lRetVal

	METHOD RetrieveFiles(lMustExist AS LOGIC)  AS ARRAY
		LOCAL aFiles AS ARRAY
		LOCAL aCollection AS StringCollection
		aFiles := {}
		IF System.Windows.Forms.Clipboard.ContainsFileDropList()
			aCollection := System.Windows.Forms.Clipboard.GetFileDropList()
			FOREACH IMPLIED s IN aCollection
				AADD(aFiles,s)
			NEXT
		ENDIF
		RETURN aFiles

	METHOD RetrieveRTF(nStringLength := -1 AS LONG)  AS STRING
		LOCAL cRetVal AS STRING
		IF System.Windows.Forms.Clipboard.ContainsText(System.Windows.Forms.TextDataFormat.Rtf)
			cRetVal := System.Windows.Forms.Clipboard.GetText(System.Windows.Forms.TextDataFormat.Rtf)
			IF nStringLength >= 0
				cRetVal := cRetVal:Substring(0, nStringLength)
			ENDIF
		ENDIF
		RETURN cRetVal

	METHOD RetrieveString(nStringLength := -1 AS LONG)  AS STRING
		LOCAL cRetVal AS STRING
		IF System.Windows.Forms.Clipboard.ContainsText()
			cRetVal := System.Windows.Forms.Clipboard.GetText(System.Windows.Forms.TextDataFormat.UnicodeText)
			IF nStringLength >= 0
				cRetVal := cRetVal:Substring(0, nStringLength)
			ENDIF
		ENDIF
		RETURN cRetVal

END CLASS

