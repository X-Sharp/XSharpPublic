//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System.Collections.Specialized
/// <include file="Gui.xml" path="doc/Clipboard/*" />

CLASS Clipboard INHERIT VObject

/// <include file="Gui.xml" path="doc/Clipboard.Clear/*" />
	METHOD Clear() AS VOID STRICT
		System.Windows.Forms.Clipboard.Clear()
		RETURN
/// <include file="Gui.xml" path="doc/Clipboard.FileCount/*" />
	ACCESS FileCount AS LONG
		LOCAL nCount AS LONG
		LOCAL aFiles AS StringCollection
		IF System.Windows.Forms.Clipboard.ContainsFileDropList()
			aFiles := System.Windows.Forms.Clipboard.GetFileDropList()
			nCount := aFiles:Count
		ENDIF
		RETURN nCount

/// <include file="Gui.xml" path="doc/Clipboard.GetItemSize/*" />
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


/// <include file="Gui.xml" path="doc/Clipboard.ctor/*" />
CONSTRUCTOR()  STRICT

    SUPER()

RETURN
/// <include file="Gui.xml" path="doc/Clipboard.Insert/*" />
	METHOD Insert(xType AS USUAL)  AS LOGIC
		LOCAL lOk AS LOGIC
		IF IsString(xType)
			LOCAL cString AS STRING
			cString := xType
			IF !STRING.IsNullOrEmpty(cString)
				System.Windows.Forms.Clipboard.SetText( cString)
			ENDIF
			lOk := TRUE
		elseif xType is Bitmap var oBitMap
			System.Windows.Forms.Clipboard.SetImage( (System.Drawing.Image) oBitMap)
			lOk := TRUE
		ELSE
			WCError{#Insert,#Clipboard,__WCSTypeError,xType,1}:Throw()
			lOk := FALSE
		ENDIF
		RETURN lOk

/// <include file="Gui.xml" path="doc/Clipboard.InsertRTF/*" />
	METHOD InsertRTF(cText AS STRING)  AS LOGIC
		LOCAL lOk AS LOGIC
		System.Windows.Forms.Clipboard.SetText((STRING) cText, System.Windows.Forms.TextDataFormat.Rtf)
		lOk := TRUE
		RETURN lOk

/// <include file="Gui.xml" path="doc/Clipboard.RetrieveBitmap/*" />
	METHOD RetrieveBitmap(oBitmap AS Bitmap) as LOGIC
		LOCAL lRetVal AS LOGIC
		LOCAL oImage AS System.Drawing.Image
		IF System.Windows.Forms.Clipboard.ContainsImage()
			oImage := System.Windows.Forms.Clipboard.GetImage()
			oBitmap:__SetImage(oImage)
			lRetVal := TRUE
		ENDIF

		RETURN lRetVal

/// <include file="Gui.xml" path="doc/Clipboard.RetrieveFiles/*" />
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

/// <include file="Gui.xml" path="doc/Clipboard.RetrieveRTF/*" />
	METHOD RetrieveRTF(nStringLength := -1 AS LONG)  AS STRING
		LOCAL cRetVal AS STRING
		IF System.Windows.Forms.Clipboard.ContainsText(System.Windows.Forms.TextDataFormat.Rtf)
			cRetVal := System.Windows.Forms.Clipboard.GetText(System.Windows.Forms.TextDataFormat.Rtf)
			IF nStringLength >= 0
				cRetVal := cRetVal:Substring(0, nStringLength)
			ENDIF
		ENDIF
		RETURN cRetVal

/// <include file="Gui.xml" path="doc/Clipboard.RetrieveString/*" />
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

