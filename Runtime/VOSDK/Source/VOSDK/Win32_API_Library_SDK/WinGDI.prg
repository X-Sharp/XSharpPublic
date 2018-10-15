VOSTRUCT _winXFORM
	MEMBER eM11 AS REAL4
	MEMBER eM12 AS REAL4
	MEMBER eM21 AS REAL4
	MEMBER eM22 AS REAL4
	MEMBER eDx  AS REAL4
	MEMBER eDy AS REAL4


/* Bitmap Header Definition */
VOSTRUCT _WINBITMAP
	MEMBER		  bmType AS LONGINT
	MEMBER		  bmWidth AS LONGINT
	MEMBER		  bmHeight AS LONGINT
	MEMBER		  bmWidthBytes AS LONGINT
	MEMBER		  bmPlanes AS WORD
	MEMBER		  bmBitsPixel AS WORD
	MEMBER		  bmBits AS PTR

VOSTRUCT _WINRGBTRIPLE   ALIGN 1
	MEMBER  rgbtBlue AS BYTE
	MEMBER  rgbtGreen AS BYTE
	MEMBER  rgbtRed AS BYTE

VOSTRUCT _WINRGBQUAD
	MEMBER  rgbBlue AS BYTE
	MEMBER  rgbGreen AS BYTE
	MEMBER  rgbRed AS BYTE
	MEMBER  rgbReserved AS BYTE




VOSTRUCT _winCIEXYZ

	MEMBER ciexyzX AS LONGINT
	MEMBER ciexyzY AS LONGINT
	MEMBER ciexyzZ AS LONGINT

VOSTRUCT _WINICEXYZTRIPLE
	MEMBER  ciexyzRed IS _winCIEXYZ
	MEMBER  ciexyzGreen IS _winCIEXYZ
	MEMBER  ciexyzBlue IS _winCIEXYZ



VOSTRUCT _winLOGCOLORSPACE
	MEMBER lcsSignature AS DWORD
	MEMBER lcsVersion AS DWORD
	MEMBER lcsSize AS DWORD
	MEMBER lcsCSType AS  LONGINT
	MEMBER lcsIntent	  AS LONGINT
	MEMBER lcsEndpoints IS _WINICEXYZTRIPLE
	MEMBER lcsGammaRed AS DWORD
	MEMBER lcsGammaGreen AS DWORD
	MEMBER lcsGammaBlue AS DWORD
	MEMBER  DIM lcsFilename[MAX_PATH] AS BYTE





VOSTRUCT _WINBITMAPCOREHEADER
	MEMBER bcSize AS DWORD
   MEMBER bcWidth AS WORD     //RvdH 040122 Changed from DWORD to WORD. Definition was invalid !
	MEMBER bcHeight AS WORD
	MEMBER bcPlanes AS WORD
	MEMBER bcBitCount AS WORD


VOSTRUCT _WINBITMAPINFOHEADER
	MEMBER	   biSize AS DWORD
	MEMBER	   biWidth AS LONGINT
	MEMBER	   biHeight AS LONGINT
	MEMBER	   biPlanes AS WORD
	MEMBER	   biBitCount AS WORD
	MEMBER	   biCompression AS DWORD
	MEMBER	   biSizeImage AS DWORD
	MEMBER	   biXPelsPerMeter AS LONGINT
	MEMBER	   biYPelsPerMeter AS LONGINT
	MEMBER	   biClrUsed AS DWORD
	MEMBER	   biClrImportant AS DWORD


VOSTRUCT _WINBITMAPV4HEADER
	MEMBER		   bV4Size AS DWORD
	MEMBER		   bV4Width AS LONGINT
	MEMBER		   bV4Height AS LONGINT
	MEMBER		   bV4Planes AS WORD
	MEMBER		   bV4BitCount AS WORD
	MEMBER		   bV4V4Compression AS DWORD
	MEMBER		   bV4SizeImage AS DWORD
	MEMBER		   bV4XPelsPerMeter AS LONGINT
	MEMBER		   bV4YPelsPerMeter AS LONGINT
	MEMBER		   bV4ClrUsed AS DWORD
	MEMBER		   bV4ClrImportant AS DWORD
	MEMBER		   bV4RedMask AS DWORD
	MEMBER		   bV4GreenMask AS DWORD
	MEMBER		   bV4BlueMask AS DWORD
	MEMBER		   bV4AlphaMask AS DWORD
	MEMBER		   bV4CSType AS DWORD
	MEMBER		   bV4Endpoints IS _WINICEXYZTRIPLE
	MEMBER		   bV4GammaRed AS DWORD
	MEMBER		   bV4GammaGreen AS DWORD
	MEMBER		   bV4GammaBlue AS DWORD

VOSTRUCT _WINBITMAPV5HEADER  
	MEMBER		   bV5Size AS DWORD
	MEMBER		   bV5Width AS LONGINT
	MEMBER		   bV5Height AS LONGINT
	MEMBER		   bV5Planes AS WORD
	MEMBER		   bV5BitCount AS WORD
	MEMBER		   bV5V4Compression AS DWORD
	MEMBER		   bV5SizeImage AS DWORD
	MEMBER		   bV5XPelsPerMeter AS LONGINT
	MEMBER		   bV5YPelsPerMeter AS LONGINT
	MEMBER		   bV5ClrUsed AS DWORD
	MEMBER		   bV5ClrImportant AS DWORD
	MEMBER		   bV5RedMask AS DWORD
	MEMBER		   bV5GreenMask AS DWORD
	MEMBER		   bV5BlueMask AS DWORD
	MEMBER		   bV5AlphaMask AS DWORD
	MEMBER		   bV5CSType AS DWORD
	MEMBER		   bV5Endpoints IS _WINICEXYZTRIPLE
	MEMBER		   bV5GammaRed AS DWORD
	MEMBER		   bV5GammaGreen AS DWORD
	MEMBER		   bV5GammaBlue AS DWORD
   MEMBER		   bV5Intent AS DWORD
   MEMBER		   bV5ProfileData AS DWORD
   MEMBER		   bV5ProfileSize AS DWORD
   MEMBER		   bV5Reserved AS DWORD


/* constants for the biCompression field */
VOSTRUCT _WINBITMAPINFO
	MEMBER						  bmiHeader IS _WINBITMAPINFOHEADER
	MEMBER			  DIM	  bmiColors[1] IS _WINRGBQUAD

VOSTRUCT _WINBITMAPCOREINFO
	MEMBER			  bmciHeader IS _WINBITMAPCOREHEADER
	MEMBER	  DIM bmciColors[1] IS _WINRGBTRIPLE

VOSTRUCT _WINBITMAPFILEHEADER  ALIGN 2
	MEMBER  bfType 		AS WORD
	MEMBER  bfSize 		AS DWORD
	MEMBER  bfReserved1 	AS WORD
	MEMBER  bfReserved2 	AS WORD
	MEMBER  bfOffBits 	AS DWORD



VOSTRUCT _WINFONTSIGNATURE
	MEMBER   DIM fsUsb[4] AS DWORD
	MEMBER   DIM fsCsb[2] AS DWORD

VOSTRUCT _WINCHARSETINFO
	MEMBER ciCharset AS DWORD
	MEMBER ciACP AS DWORD
	MEMBER fs IS _WINFONTSIGNATURE

VOSTRUCT _WINLOCALESIGNATURE
	MEMBER DIM  lsUsb[4] AS DWORD
	MEMBER  DIM lsCsbDefault[2] AS DWORD
	MEMBER  DIM lsCsbSupported[2] AS DWORD



VOSTRUCT _WINHANDLETABLE
	MEMBER	   DIM objectHandle[1] AS PTR

VOSTRUCT _WINMETARECORD
	MEMBER		   rdSize AS DWORD
	MEMBER		   rdFunction AS WORD
	MEMBER		   DIM rdParm[1] AS WORD

VOSTRUCT _WINMETAFILEPICT
	MEMBER	   mm AS LONGINT
	MEMBER		  xExt AS LONGINT
	MEMBER		  yExt AS LONGINT
	MEMBER		  hMF AS PTR

VOSTRUCT _WINMETAHEADER ALIGN 2
	MEMBER	  mtType AS WORD
	MEMBER	  mtHeaderSize AS WORD
	MEMBER	  mtVersion AS WORD
	MEMBER	  mtSize AS DWORD
	MEMBER	  mtNoObjects AS WORD
	MEMBER	  mtMaxRecord AS DWORD
	MEMBER	  mtNoParameters AS WORD



VOSTRUCT _WINENHMETARECORD
	MEMBER  iType AS DWORD
	MEMBER  nSize AS DWORD
	MEMBER  DIM dParm[1] AS DWORD

VOSTRUCT _WINENHMETAHEADER
	MEMBER  iType AS DWORD
	MEMBER  nSize AS DWORD

	MEMBER  rclBounds IS _WINRECTL
	MEMBER  rclFrame IS _WINRECTL
	MEMBER  dSignature AS DWORD
	MEMBER  nVersion AS DWORD
	MEMBER  nBytes AS DWORD
	MEMBER  nRecords AS DWORD
	MEMBER  nHandles AS WORD

	MEMBER  sReserved AS WORD
	MEMBER  nDescription AS DWORD

	MEMBER  offDescription AS DWORD

	MEMBER  nPalEntries AS DWORD
	MEMBER  szlDevice IS _winSIZE
	MEMBER  szlMillimeters IS _winSIZE
	MEMBER  cbPixelFormat AS DWORD    // Size of PIXELFORMATDESCRIPTOR information
                                			// This is 0 if no pixel format is set
   MEMBER offPixelFormat AS DWORD     // Offset to PIXELFORMATDESCRIPTOR
                                			// This is 0 if no pixel format is set
   MEMBER bOpenGL AS DWORD            // TRUE if OpenGL commands are present in
                                			// the metafile, otherwise FALSE
   MEMBER szlMicrometers IS _winSIZE  // Size of the reference device in micrometers





/* tmPitchAndFamily flags */
VOSTRUCT _WINTEXTMETRIC
	MEMBER		  tmHeight AS LONGINT
	MEMBER		  tmAscent AS LONGINT
	MEMBER		  tmDescent AS LONGINT
	MEMBER		  tmInternalLeading AS LONGINT
	MEMBER		  tmExternalLeading AS LONGINT
	MEMBER		  tmAveCharWidth AS LONGINT
	MEMBER		  tmMaxCharWidth AS  LONGINT
	MEMBER		  tmWeight AS LONGINT
	MEMBER		  tmOverhang AS LONGINT
	MEMBER		  tmDigitizedAspectX AS LONGINT
	MEMBER		  tmDigitizedAspectY AS LONGINT
	MEMBER		  tmFirstChar AS BYTE
	MEMBER		  tmLastChar AS BYTE
	MEMBER		  tmDefaultChar AS BYTE
	MEMBER		  tmBreakChar AS BYTE
	MEMBER		  tmItalic AS BYTE
	MEMBER		  tmUnderlined AS BYTE
	MEMBER		  tmStruckOut AS BYTE
	MEMBER		  tmPitchAndFamily AS BYTE
	MEMBER		  tmCharSet AS BYTE





/* ntmFlags field flags */
VOSTRUCT _WINNEWTEXTMETRIC    	// RvdH 070411 removed alignment
	MEMBER		  tmHeight AS LONGINT
	MEMBER		  tmAscent AS LONGINT
	MEMBER		  tmDescent AS LONGINT
	MEMBER		  tmInternalLeading AS LONGINT
	MEMBER		  tmExternalLeading AS LONGINT
	MEMBER		  tmAveCharWidth AS LONGINT
	MEMBER		  tmMaxCharWidth AS LONGINT
	MEMBER		  tmWeight AS LONGINT
	MEMBER		  tmOverhang AS LONGINT
	MEMBER		  tmDigitizedAspectX AS LONGINT
	MEMBER		  tmDigitizedAspectY AS LONGINT
	MEMBER		  tmFirstChar AS  BYTE
	MEMBER		  tmLastChar AS BYTE
	MEMBER		  tmDefaultChar AS BYTE
	MEMBER		  tmBreakChar AS BYTE
	MEMBER		  tmItalic AS BYTE
	MEMBER		  tmUnderlined AS BYTE
	MEMBER		  tmStruckOut AS BYTE
	MEMBER		  tmPitchAndFamily AS BYTE
	MEMBER		  tmCharSet AS BYTE
	MEMBER		  ntmFlags	  AS DWORD
	MEMBER		  ntmSizeEM AS DWORD
	MEMBER		  ntmCellHeight AS DWORD
	MEMBER		  ntmAvgWidth AS DWORD




VOSTRUCT _winNEWTEXTMETRICEX
	MEMBER  ntmTm   IS _WINNEWTEXTMETRIC
	MEMBER  ntmFontSig IS _WINFONTSIGNATURE






/* Pel Array */
VOSTRUCT _winPELARRAY
	MEMBER		  paXCount AS LONGINT
	MEMBER		  paYCount AS LONGINT
	MEMBER		  paXExt AS LONGINT
	MEMBER		  paYExt AS LONGINT
	MEMBER		  paRGBs AS BYTE

/* Logical Brush (or Pattern) */




VOSTRUCT _winLOGBRUSH
	MEMBER			  lbStyle AS DWORD
	MEMBER			  lbColor AS DWORD
	MEMBER			  lbHatch AS LONGINT


/* Logical Pen */

VOSTRUCT _winLOGPEN
	MEMBER		  lopnStyle AS DWORD
	MEMBER		  lopnWidth IS _winPOINT
	MEMBER		  lopnColor AS DWORD


VOSTRUCT _winEXTLOGPEN
	MEMBER		  elpPenStyle AS DWORD
	MEMBER		  elpWidth AS DWORD
	MEMBER		  elpBrushStyle AS DWORD
	MEMBER		  elpColor AS DWORD
	MEMBER		  elpHatch AS LONGINT
	MEMBER		  elpNumEntries AS DWORD
	MEMBER		  DIM elpStyleEntry[1] AS DWORD


VOSTRUCT _winPALETTEENTRY
	MEMBER		  peRed AS BYTE
	MEMBER		  peGreen AS BYTE
	MEMBER		  peBlue AS BYTE
	MEMBER		  peFlags AS BYTE



/* Logical Palette */
VOSTRUCT _winLOGPALETTE
	MEMBER			  palVersion AS WORD
	MEMBER			  palNumEntries AS WORD
	MEMBER			  DIM palPalEntry[1] IS _winPALETTEENTRY


/* Logical Font */
VOSTRUCT _winLOGFONT ALIGN 1
	MEMBER	  lfHeight AS LONGINT
	MEMBER	  lfWidth AS LONGINT
	MEMBER	  lfEscapement AS LONGINT
	MEMBER	  lfOrientation AS LONGINT
	MEMBER	  lfWeight AS LONGINT
	MEMBER	  lfItalic AS BYTE
	MEMBER	  lfUnderline AS BYTE
	MEMBER	  lfStrikeOut AS BYTE
	MEMBER	  lfCharSet AS BYTE
	MEMBER	  lfOutPrecision AS BYTE
	MEMBER	  lfClipPrecision AS BYTE
	MEMBER	  lfQuality AS BYTE
	MEMBER	  lfPitchAndFamily AS BYTE
	MEMBER	  DIM lfFaceName[LF_FACESIZE] AS BYTE




VOSTRUCT _winENUMLOGFONT
	MEMBER   elfLogFont IS _winLOGFONT
	MEMBER   DIM elfFullName[LF_FULLFACESIZE] AS BYTE
	MEMBER   DIM elfStyle[LF_FACESIZE] AS BYTE





VOSTRUCT _winENUMLOGFONTEX
	MEMBER		  elfLogFont IS _winLOGFONT
	MEMBER		  DIM elfFullName[LF_FULLFACESIZE] AS BYTE
	MEMBER		  DIM elfStyle[LF_FACESIZE] AS BYTE
	MEMBER		  DIM elfScript[LF_FACESIZE] AS BYTE


VOSTRUCT _winPANOSE
	MEMBER	  bFamilyType AS BYTE
	MEMBER	  bSerifStyle AS BYTE
	MEMBER	  bWeight AS BYTE
	MEMBER	  bProportion AS BYTE
	MEMBER   bContrast AS BYTE
	MEMBER   bStrokeVariation AS BYTE
	MEMBER   bArmStyle AS BYTE
	MEMBER   bLetterform AS BYTE
	MEMBER   bMidline AS BYTE
	MEMBER  bXHeight AS BYTE


VOSTRUCT _winEXTLOGFONT
	MEMBER	   elfLogFont IS _winLOGFONT
	MEMBER	   DIM elfFullName[LF_FULLFACESIZE] AS BYTE
	MEMBER		  DIM elfStyle[LF_FACESIZE] AS BYTE
	MEMBER		  elfVersion AS DWORD
	MEMBER		  elfStyleSize AS DWORD
	MEMBER		  elfMatch AS DWORD
	MEMBER		  elfReserved AS DWORD
	MEMBER		  DIM elfVendorId[ELF_VENDOR_SIZE] AS BYTE
	MEMBER		  elfCulture AS DWORD
	MEMBER	   elfPanose IS _winPANOSE




VOSTRUCT _WINDEVMODE
	MEMBER   DIM dmDeviceName[CCHDEVICENAME] AS BYTE
	MEMBER   dmSpecVersion AS WORD
	MEMBER   dmDriverVersion AS WORD
	MEMBER   dmSize AS WORD
	MEMBER   dmDriverExtra AS WORD
	MEMBER   dmFields AS DWORD
	MEMBER   dmOrientation AS SHORTINT
	MEMBER   dmPaperSize AS SHORTINT
	MEMBER   dmPaperLength AS SHORTINT
	MEMBER   dmPaperWidth AS SHORTINT
	MEMBER   dmScale AS SHORTINT
	MEMBER   dmCopies AS SHORTINT
	MEMBER   dmDefaultSource AS SHORTINT
	MEMBER   dmPrintQuality AS SHORTINT
	MEMBER   dmColor AS SHORTINT
	MEMBER   dmDuplex AS SHORTINT
	MEMBER   dmYResolution AS SHORTINT
	MEMBER   dmTTOption AS SHORTINT
	MEMBER   dmCollate AS SHORTINT
	MEMBER   DIM dmFormName[CCHFORMNAME] AS BYTE
	MEMBER   dmLogPixels AS WORD
	MEMBER   dmBitsPerPel AS DWORD
	MEMBER   dmPelsWidth AS DWORD
	MEMBER   dmPelsHeight AS DWORD
	MEMBER   dmDisplayFlags AS DWORD
	MEMBER   dmDisplayFrequency AS DWORD
	MEMBER   dmICMMethod AS DWORD
	MEMBER   dmICMIntent AS DWORD
	MEMBER   dmMediaType AS DWORD
	MEMBER   dmDitherType AS DWORD
	MEMBER   dmReserved1 AS DWORD
	MEMBER   dmReserved2 AS DWORD
	//RvdH 070412 Added  
	MEMBER dmPanningWidth AS DWORD
   MEMBER dmPanningHeight AS DWORD


VOSTRUCT _winRGNDATAHEADER
	MEMBER  dwSize AS DWORD
	MEMBER  iType AS DWORD
	MEMBER  nCount AS DWORD
	MEMBER  nRgnSize AS DWORD
	MEMBER  rcBound IS _winRECT

VOSTRUCT _winRGNDATA
	MEMBER   rdh	  IS  _winRGNDATAHEADER
	MEMBER   DIM	  Buffer[1] AS BYTE


VOSTRUCT _winABC
	MEMBER  abcA AS INT
	MEMBER  abcB AS DWORD
	MEMBER  abcC AS INT



VOSTRUCT _winABCFLOAT
	MEMBER  abcfA AS REAL4
	MEMBER  abcfB AS REAL4
	MEMBER  abcfC AS REAL4



VOSTRUCT _winOUTLINETEXTMETRIC
	MEMBER  otmSize AS DWORD
	MEMBER  otmTextMetrics IS _WINTEXTMETRIC
	MEMBER  otmFiller AS BYTE
	MEMBER  otmPanoseNumber IS _winPANOSE
	MEMBER  otmfsSelection AS DWORD
	MEMBER  otmfsType AS DWORD
	MEMBER  otmsCharSlopeRise AS INT
	MEMBER  otmsCharSlopeRun AS INT
	MEMBER  otmItalicAngle AS INT
	MEMBER  otmEMSquare AS DWORD
	MEMBER  otmAscent AS INT
	MEMBER  otmDescent AS INT
	MEMBER  otmLineGap AS DWORD
	MEMBER  otmsCapEmHeight AS DWORD
	MEMBER  otmsXHeight AS DWORD
	MEMBER  otmrcFontBox IS _winRECT
	MEMBER  otmMacAscent AS INT
	MEMBER  otmMacDescent AS INT
	MEMBER  otmMacLineGap AS DWORD
	MEMBER  otmusMinimumPPEM AS DWORD
	MEMBER  otmptSubscriptSize IS _WINPOINT
	MEMBER  otmptSubscriptOffset IS _WINPOINT
	MEMBER  otmptSuperscriptSize IS _WINPOINT
	MEMBER  otmptSuperscriptOffset IS _WINPOINT
	MEMBER  otmsStrikeoutSize AS DWORD
	MEMBER  otmsStrikeoutPosition AS INT
	MEMBER  otmsUnderscoreSize AS INT
	MEMBER  otmsUnderscorePosition AS INT
	MEMBER  otmpFamilyName AS PTR
	MEMBER  otmpFaceName AS PTR
	MEMBER  otmpStyleName AS PTR
	MEMBER  otmpFullName AS PTR





VOSTRUCT _winPOLYTEXT
	MEMBER	  x AS INT
	MEMBER	  y AS INT
	MEMBER	  n AS DWORD
	MEMBER	  lpstr AS PSZ
	MEMBER	  uiFlags AS DWORD
	MEMBER	  rcl IS _winRECT
	MEMBER	  pdx AS PTR



VOSTRUCT _winFIXED
	MEMBER	  fract AS WORD
	MEMBER	  value AS SHORTINT


VOSTRUCT _winMAT2
	MEMBER eM11 IS _winFIXED
	MEMBER eM12 IS _winFIXED
	MEMBER eM21 IS _winFIXED
	MEMBER eM22 IS _winFIXED


VOSTRUCT _winGLYPHMETRICS
	MEMBER gmBlackBoxX AS DWORD
	MEMBER gmBlackBoxY AS DWORD
	MEMBER gmptGlyphOrigin IS _winPOINT
	MEMBER gmCellIncX AS SHORTINT
	MEMBER gmCellIncY AS SHORTINT

//  GetGlyphOutline constants

VOSTRUCT _WINPOINTFX
	MEMBER  x IS _winFIXED
	MEMBER  y IS _winFIXED



VOSTRUCT _winTTPOLYCURVE
	MEMBER	  wType AS WORD
	MEMBER   cpfx AS WORD
	MEMBER   DIM apfx[1] IS _winPOINTFX



VOSTRUCT _winTTPOLYGONHEADER
	MEMBER  cb AS DWORD
	MEMBER  dwType AS DWORD
	MEMBER  pfxStart IS _winPOINTFX

VOSTRUCT _winGCP_RESULTS
	MEMBER	  lStructSize AS DWORD
	MEMBER	  lpOutString AS PSZ
	MEMBER	  lpOrder AS DWORD PTR
	MEMBER	  lpDx AS INT PTR
	MEMBER	  lpCaretPos AS INT PTR
	MEMBER	  lpClass AS PSZ
	MEMBER	  lpGlyphs	  AS PSZ
	MEMBER	  nGlyphs	  AS DWORD
	MEMBER	  nMaxFit	  AS INT



VOSTRUCT _winRASTERIZER_STATUS
	MEMBER  nSize AS SHORTINT
	MEMBER  wFlags AS SHORTINT
	MEMBER  nLanguageID AS SHORTINT



VOSTRUCT _winPIXELFORMATDESCRIPTOR
	MEMBER  nSize AS WORD
	MEMBER  nVersion AS WORD
	MEMBER  dwFlags AS DWORD
	MEMBER  iPixelType AS BYTE
	MEMBER  cColorBits AS BYTE
	MEMBER  cRedBits AS BYTE
	MEMBER  cRedShift AS BYTE
	MEMBER  cGreenBits AS BYTE
	MEMBER  cGreenShift AS BYTE
	MEMBER  cBlueBits AS BYTE
	MEMBER  cBlueShift AS BYTE
	MEMBER  cAlphaBits AS BYTE
	MEMBER  cAlphaShift AS BYTE
	MEMBER  cAccumBits AS BYTE
	MEMBER  cAccumRedBits AS BYTE
	MEMBER  cAccumGreenBits AS BYTE
	MEMBER  cAccumBlueBits AS BYTE
	MEMBER  cAccumAlphaBits AS BYTE
	MEMBER  cDepthBits AS BYTE
	MEMBER  cStencilBits AS BYTE
	MEMBER  cAuxBuffers AS BYTE
	MEMBER  iLayerType AS BYTE
	MEMBER  bReserved AS BYTE
	MEMBER  dwLayerMask AS DWORD
	MEMBER  dwVisibleMask AS DWORD
	MEMBER  dwDamageMask AS DWORD

/* pixel types */
VOSTRUCT _winDIBSECTION
	MEMBER   dsBm IS _winBITMAP
	MEMBER   dsBmih IS _winBITMAPINFOHEADER
	MEMBER   DIM dsBitfields[3] AS DWORD
	MEMBER   dshSection AS PTR
	MEMBER   dsOffset AS DWORD



VOSTRUCT _winCOLORADJUSTMENT
	MEMBER   caSize AS WORD
	MEMBER   caFlags AS WORD
	MEMBER   caIlluminantIndex AS WORD
	MEMBER  caRedGamma AS WORD
	MEMBER  caGreenGamma AS WORD
	MEMBER  caBlueGamma AS WORD
	MEMBER  caReferenceBlack AS WORD
	MEMBER  caReferenceWhite AS WORD
	MEMBER  caContrast AS SHORTINT
	MEMBER  caBrightness AS SHORTINT
	MEMBER  caColorfulness AS SHORTINT
	MEMBER caRedGreenTint AS SHORTINT



VOSTRUCT _winDOCINFO ALIGN 1
	MEMBER	  cbSize AS INT
	MEMBER  lpszDocName AS PSZ
	MEMBER  lpszOutput AS PSZ
	MEMBER  lpszDatatype AS PSZ
	MEMBER	  fwType AS DWORD



VOSTRUCT _winKERNINGPAIR
	MEMBER wFirst AS WORD
	MEMBER wSecond AS WORD
	MEMBER iKernAmount AS INT



VOSTRUCT _winEMR
	MEMBER iType AS DWORD
	MEMBER nSize AS DWORD





VOSTRUCT _winEMRTEXT
	MEMBER  ptlReference IS _winPOINTL
	MEMBER  nChars AS DWORD
	MEMBER  offString AS DWORD
	MEMBER  fOptions AS DWORD
	MEMBER  rcl IS _winRECTL
	MEMBER  offDx AS DWORD




VOSTRUCT _winEMRABORTPATH       // RvdH 070411 changed name (added EMR) 
	MEMBER  emr IS _winEMR

VOSTRUCT _winEMRSELECTCLIPPATH
	MEMBER	  emr IS _winEMR
	MEMBER  iMode AS DWORD


VOSTRUCT _winEMRSETMITERLIMIT
	MEMBER  emr IS _winEMR
	MEMBER  eMiterLimit AS REAL4


VOSTRUCT _winEMRRESTOREDC
	MEMBER   emr IS _winEMR
	MEMBER   iRelative AS LONGINT





VOSTRUCT _winEMRSETARCDIRECTION
	MEMBER bemr IS _winEMR
	MEMBER irection AS DWORD





VOSTRUCT _winEMRSETMAPPERFLAGS
	MEMBER	   emr IS _winEMR
	MEMBER  dwFlags AS DWORD



VOSTRUCT _winEMRSETTEXTCOLOR
	MEMBER  emr IS _winEMR
	MEMBER  crColor AS DWORD




VOSTRUCT _winEMRSELECTOBJECT
	MEMBER   bemr IS _winEMR
	MEMBER   ihObject AS DWORD


VOSTRUCT _winEMRSELECTCOLORSPACE
	MEMBER  emr IS _winEMR
	MEMBER  ihCS AS DWORD


VOSTRUCT _winEMRSELECTPALETTE
	MEMBER  bemr IS _winEMR
	MEMBER   ihPal AS DWORD

VOSTRUCT _winEMRRESIZEPALETTE
	MEMBER  emr IS _winEMR
	MEMBER  ihPal AS DWORD
	MEMBER  cEntries AS DWORD

VOSTRUCT _winEMRSETPALETTEENTRIES
	MEMBER  emr IS _winEMR
	MEMBER  ihPal AS DWORD
	MEMBER  iStart AS DWORD
	MEMBER  cEntries AS DWORD
	MEMBER  DIM aPalEntries[1] IS _winPALETTEENTRY

VOSTRUCT _winEMRSETCOLORADJUSTMENT
	MEMBER emr 				  IS _winEMR
	MEMBER ColorAdjustment IS _winCOLORADJUSTMENT	//RvdH 070412 changed from AS to IS
                          	
VOSTRUCT _winEMRGDICOMMENT
	MEMBER  emr IS _winEMR
	MEMBER  cbData AS DWORD
	MEMBER  DIM Data[1] AS BYTE

VOSTRUCT _winEMREOF
	MEMBER   emr IS _winEMR
	MEMBER   nPalEntries AS DWORD
	MEMBER   offPalEntries AS DWORD
	MEMBER   nSizeLast AS DWORD

VOSTRUCT _winEMRLINETO
	MEMBER  emr IS _winEMR
	MEMBER  ptl IS _winPOINTL

VOSTRUCT _winEMROFFSETCLIPRGN
	MEMBER	  Bemr IS _winEMR
	MEMBER  ptlOffset  IS _winPOINT

VOSTRUCT _winEMRFILLPATH
	MEMBER   emr IS _winEMR
	MEMBER   rclBounds IS _winRECT



VOSTRUCT _winEMREXCLUDECLIPRECT
	MEMBER  emr IS _winEMR
	MEMBER  rclClip IS _winRECT



VOSTRUCT _winEMRSETVIEWPORTORGEX
	MEMBER  emr IS _winEMR
	MEMBER  ptlOrigin IS _winpoint



VOSTRUCT _winEMRSETVIEWPORTEXTEX
	MEMBER  emr IS _winEMR
	MEMBER  szlExtent IS _winSIZE

VOSTRUCT _winEMRSCALEVIEWPORTEXTEX
	MEMBER   emr IS _winEMR
	MEMBER   xNum AS LONGINT
	MEMBER   xDenom AS LONGINT
	MEMBER   yNum AS LONGINT
	MEMBER   yDenom AS LONGINT

VOSTRUCT _winEMRSETWORLDTRANSFORM
	MEMBER  emr IS _winEMR
	MEMBER  xform IS _winXFORM

VOSTRUCT _winEMRMODIFYWORLDTRANSFORM
	MEMBER  emr IS _winEMR
	MEMBER  xform IS _winXFORM
	MEMBER  iMode AS DWORD



VOSTRUCT _winEMRSETPIXELV
	MEMBER  emr IS _winEMR
	MEMBER  ptlPixel IS _winPOINT
	MEMBER  crColor AS DWORD



VOSTRUCT _winEMREXTFLOODFILL
	MEMBER  emr IS _winEMR
	MEMBER  ptlStart IS _winPOINT
	MEMBER  crColor AS DWORD
	MEMBER iMode AS DWORD


VOSTRUCT _winEMRELLIPSE
	MEMBER	   emr IS _winEMR
	MEMBER  rclBox IS _winRECT





VOSTRUCT _winEMRROUNDRECT
	MEMBER   emr IS _winEMR
	MEMBER   rclBox IS _winRECTL
	MEMBER   szlCorner IS _winSIZE



VOSTRUCT _winEMRARC
	MEMBER   emr IS _winEMR
	MEMBER   rclBox IS _winRECTL
	MEMBER  ptlStart IS _winPOINTL
	MEMBER  ptlEnd IS _winPOINTL


VOSTRUCT _winEMRANGLEARC
	MEMBER  emr IS _winemr
	MEMBER  ptlCenter IS _winPOINTL
	MEMBER  nRadius AS DWORD
	MEMBER  eStartAngle AS REAL4
	MEMBER  eSweepAngle AS REAL4



VOSTRUCT _winEMRPOLYLINE
	MEMBER   emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  cptl AS DWORD
	MEMBER  DIM aptl[1] IS _winPOINTL



VOSTRUCT _winEMRPOLYLINE16
	MEMBER   emr IS _winEMR
	MEMBER   rclBounds IS _winRECTL
	MEMBER   cpts AS DWORD
	MEMBER   DIM apts[1] IS _winPOINTs



VOSTRUCT _winEMRPOLYDRAW
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  cptl AS DWORD
	MEMBER  DIM aptl[1] IS _winPOINTL
	MEMBER  DIM abTypes[1] AS BYTE


VOSTRUCT _winEMRPOLYDRAW16
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  cpts AS DWORD
	MEMBER  DIM apts[1] IS _winPOINTS
	MEMBER  DIM abTypes[1] AS BYTE


VOSTRUCT _winEMRPOLYPOLYLINE
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  nPolys AS DWORD
	MEMBER  cptl AS DWORD
	MEMBER  DIM aPolyCounts[1] AS DWORD
	MEMBER  DIM aptl[1] IS _winPOINTL



VOSTRUCT _winEMRPOLYPOLYLINE16
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  nPolys AS DWORD
	MEMBER  cpts AS DWORD
	MEMBER  DIM aPolyCounts[1] AS DWORD
	MEMBER  DIM apts[1] IS _winPOINTS


VOSTRUCT _winEMRINVERTRGN
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  cbRgnData AS DWORD
	MEMBER  DIM RgnData[1] AS BYTE


VOSTRUCT _winEMRFILLRGN
	MEMBER   emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  cbRgnData AS DWORD
	MEMBER  ihBrush AS DWORD
	MEMBER  DIM RgnData[1] AS BYTE



VOSTRUCT _winEMRFRAMERGN
	MEMBER   emr IS _winEMR
	MEMBER   rclBounds IS _winRECTL
	MEMBER   cbRgnData AS DWORD
	MEMBER   ihBrush AS DWORD
	MEMBER   szlStroke IS _winSIZE
	MEMBER   DIM RgnData[1] AS BYTE



VOSTRUCT _winEMREXTSELECTCLIPRGN
	MEMBER  emr IS _winEMR
	MEMBER  cbRgnData AS DWORD
	MEMBER  iMode AS DWORD
	MEMBER  DIM RgnData[1] AS BYTE


VOSTRUCT _winEMREXTTEXTOUT
	MEMBER	   emr IS _winEMR
	MEMBER	   rclBounds IS _winRECTL
	MEMBER	   iGraphicsMode AS DWORD
	MEMBER	   exScale AS REAL4
	MEMBER	   eyScale AS REAL4
	MEMBER	   emrtext IS _winEMRTEXT




VOSTRUCT _winEMRPOLYTEXTOUT
	MEMBER   emr IS _winEMR
	MEMBER   rclBounds IS _winRECTL
	MEMBER   iGraphicsMode AS DWORD
	MEMBER   exScale AS REAL4
	MEMBER   eyScale AS REAL4
	MEMBER   cStrings AS LONGINT
	MEMBER   DIM aemrtext[1] IS _winEMRTEXT




VOSTRUCT _winEMRBITBLT
	MEMBER emr IS _winEMR
	MEMBER rclBounds IS _winRECTL
	MEMBER xDest AS LONGINT
	MEMBER yDest AS LONGINT
	MEMBER cxDest AS LONGINT
	MEMBER cyDest AS LONGINT
	MEMBER dwRop AS DWORD
	MEMBER xSrc AS LONGINT
	MEMBER ySrc AS LONGINT
	MEMBER xformSrc IS _winXFORM
	MEMBER crBkColorSrc AS DWORD
	MEMBER iUsageSrc AS DWORD

	MEMBER offBmiSrc AS DWORD
	MEMBER cbBmiSrc AS DWORD
	MEMBER offBitsSrc AS DWORD
	MEMBER cbBitsSrc AS DWORD
   


VOSTRUCT _winEMRSTRETCHBLT
	MEMBER  emr IS _winEMR
	MEMBER rclBounds IS _winRECTL
	MEMBER xDest AS LONGINT
	MEMBER yDest AS LONGINT
	MEMBER cxDest AS LONGINT
	MEMBER cyDest AS LONGINT
	MEMBER dwRop AS DWORD
	MEMBER xSrc AS LONGINT
	MEMBER ySrc AS LONGINT
	MEMBER xformSrc IS _winXFORM
	MEMBER crBkColorSrc AS DWORD
	MEMBER iUsageSrc AS DWORD

	MEMBER offBmiSrc AS DWORD
	MEMBER cbBmiSrc AS DWORD
	MEMBER offBitsSrc AS DWORD
	MEMBER cbBitsSrc AS DWORD
	MEMBER cxSrc AS LONGINT
	MEMBER cySrc AS LONGINT


VOSTRUCT _winEMRMASKBLT
	MEMBER  emr IS _winEMR
	MEMBER rclBounds IS _winRECTL
	MEMBER xDest AS LONGINT
	MEMBER yDest AS LONGINT
	MEMBER cxDest AS LONGINT
	MEMBER cyDest AS LONGINT
	MEMBER dwRop	  AS DWORD
	MEMBER xSrc AS LONGINT
	MEMBER ySrc AS LONGINT
	MEMBER xformSrc IS _winXFORM
	MEMBER crBkColorSrc AS DWORD
	MEMBER iUsageSrc AS DWORD

	MEMBER offBmiSrc AS DWORD
	MEMBER cbBmiSrc AS DWORD
	MEMBER offBitsSrc AS DWORD
	MEMBER cbBitsSrcAS AS DWORD
	MEMBER xMask AS LONGINT
	MEMBER yMask AS LONGINT
	MEMBER iUsageMask AS DWORD
	MEMBER offBmiMask AS DWORD
	MEMBER cbBmiMask AS DWORD
	MEMBER offBitsMask AS DWORD
	MEMBER cbBitsMask AS DWORD


VOSTRUCT _winEMRPLGBLT
	MEMBER   emr IS _winEMR
	MEMBER   rclBounds IS _winRECTL
	MEMBER   DIM aptlDest[3] IS _winPOINTL
	MEMBER   xSrc  AS LONGINT
	MEMBER   ySrc  AS LONGINT
	MEMBER   cxSrc  AS LONGINT
	MEMBER   cySrc  AS LONGINT
	MEMBER   xformSrc IS _winXFORM
	MEMBER   crBkColorSrc AS DWORD
	MEMBER   iUsageSrc AS DWORD

	MEMBER   offBmiSrc AS DWORD
	MEMBER   cbBmiSrc AS DWORD
	MEMBER   offBitsSrc AS DWORD
	MEMBER   cbBitsSrc AS DWORD
	MEMBER   xMask AS LONGINT
	MEMBER   yMask AS LONGINT
	MEMBER   iUsageMask AS DWORD
	MEMBER   offBmiMask AS DWORD
	MEMBER   cbBmiMask AS DWORD
	MEMBER   offBitsMask AS DWORD
	MEMBER   cbBitsMask AS DWORD


VOSTRUCT _winEMRSETDIBITSTODEVICE
	MEMBER  emr IS _winEMR
	MEMBER  rclBounds IS _winRECTL
	MEMBER  xDest AS LONGINT
	MEMBER  yDest AS LONGINT
	MEMBER  xSrc AS LONGINT
	MEMBER  ySrc AS LONGINT
	MEMBER  cxSrc AS LONGINT
	MEMBER  cySrc AS LONGINT
	MEMBER  offBmiSrc AS DWORD
	MEMBER  cbBmiSrc AS DWORD
	MEMBER  offBitsSrc AS DWORD
	MEMBER  cbBitsSrc AS DWORD
	MEMBER  iUsageSrc AS DWORD
	MEMBER  iStartScan AS DWORD
	MEMBER  cScans AS DWORD


VOSTRUCT _winEMRSTRETCHDIBITS
	MEMBER  emr IS _winEMR
	MEMBER rclBounds IS _winRECTL
	MEMBER xDest AS LONGINT
	MEMBER yDest AS LONGINT
	MEMBER xSrc AS LONGINT
	MEMBER ySrc AS LONGINT
	MEMBER cxSrc AS LONGINT
	MEMBER cySrc AS LONGINT
	MEMBER offBmiSrc AS DWORD
	MEMBER cbBmiSrc AS DWORD
	MEMBER offBitsSrc AS DWORD
	MEMBER cbBitsSrc AS DWORD
	MEMBER iUsageSrc AS DWORD
	MEMBER dwRop AS DWORD
	MEMBER cxDest AS LONGINT
	MEMBER cyDest AS LONGINT




VOSTRUCT _winEMRCREATEPALETTE
	MEMBER	   emr IS _winEMR
	MEMBER  ihPal AS DWORD
	MEMBER lgpl IS _winLOGPALETTE





VOSTRUCT _winEMRCREATEPEN
	MEMBER  emr IS _winEMR
	MEMBER ihPen AS DWORD
	MEMBER lopn IS _winLOGPEN


VOSTRUCT _winEMREXTCREATEPEN
	MEMBER  emr IS _winEMR
	MEMBER  ihPen AS DWORD
	MEMBER  offBmi AS DWORD
	MEMBER  cbBmi AS DWORD


	MEMBER  offBits AS DWORD
	MEMBER  cbBits AS DWORD
	MEMBER  elp IS _winEXTLOGPEN



VOSTRUCT _winEMRCREATEBRUSHINDIRECT
	MEMBER  emr IS _winEMR
	MEMBER ihBrush AS DWORD
	MEMBER lb IS _winLOGBRUSH



VOSTRUCT _winEMRCREATEMONOBRUSH
	MEMBER  emr IS _winEMR
	MEMBER ihBrush AS DWORD
	MEMBER iUsage AS DWORD
	MEMBER offBmi AS DWORD
	MEMBER cbBmi AS DWORD
	MEMBER offBits AS DWORD
	MEMBER cbBits AS DWORD




VOSTRUCT _winEMRCREATEDIBPATTERNBRUSHPT
	MEMBER  emr IS _winEMR
	MEMBER ihBrush AS DWORD
	MEMBER iUsage AS DWORD
	MEMBER offBmi AS DWORD
	MEMBER cbBmi AS DWORD


	MEMBER offBits AS DWORD
	MEMBER cbBits AS DWORD





VOSTRUCT _winEMRFORMAT
	MEMBER   dSignature AS DWORD
	MEMBER   nVersion AS DWORD
	MEMBER   cbData AS DWORD
	MEMBER  offData AS DWORD



VOSTRUCT _winPOINTFLOAT
	MEMBER   x AS REAL4
	MEMBER   y AS REAL4



VOSTRUCT _winGLYPHMETRICSFLOAT
	MEMBER gmfBlackBoxX AS REAL4
	MEMBER gmfBlackBoxY AS REAL4
	MEMBER gmfptGlyphOrigin IS _winPOINTFLOAT
	MEMBER gmfCellIncX AS REAL4
	MEMBER gmfCellIncY AS REAL4


FUNCTION MAKEROP4(fore AS WORD, back AS WORD) AS DWORD
	LOCAL val1 AS WORD
	LOCAL val2 AS DWORD
	LOCAL val3 AS DWORD

	val1 := WORD(back) << 8
	val2 := DWORD(_And(val1, 0xFF000000))
	val3 := DWORD(fore)
	RETURN (DWORD(_Or(val2, val3)))


FUNCTION GetCValue(cmyk AS DWORD) AS BYTE STRICT
	RETURN (BYTE(cmyk))

FUNCTION GetMValue(cmyk AS DWORD) AS BYTE
	RETURN (BYTE( WORD(cmyk) >> 8))

FUNCTION GetYValue(cmyk) AS BYTE
	RETURN(BYTE(DWORD(cmyk) >>16))


FUNCTION CMYK(c AS DWORD, m AS DWORD, y AS DWORD, k AS DWORD)  AS DWORD
	LOCAL val0 AS WORD
	LOCAL val1 AS DWORD
	LOCAL val2 AS DWORD
	LOCAL val3 AS DWORD
	LOCAL val4 AS DWORD

	val0	  := WORD(BYTE(M)) << 8
	val1 := (DWORD (BYTE(y))) << 16
	val2 := val1
	val3 := DWORD(_Or(BYTE(C), val0))
	val4 := DWORD(_Or(val1, val2))
	RETURN (_Or(val3, val4))




FUNCTION GetRValue(rgb AS DWORD) AS BYTE
	LOCAL val AS BYTE

	val:=BYTE(_CAST,rgb)
	RETURN val
FUNCTION GetGValue(rgb AS DWORD) AS BYTE
	LOCAL val AS WORD
	LOCAL retval AS BYTE

	val := (WORD(_CAST,rgb))>>8
	retval := BYTE(_CAST,val)
	RETURN retval

FUNCTION GetBValue(rgb AS DWORD) AS BYTE
	LOCAL val AS DWORD
	LOCAL retval AS BYTE

	val := rgb>>16
	retval := BYTE(_CAST,val)
	RETURN retval



_DLL FUNC AddFontResource(lpFontResource AS PSZ) AS INT PASCAL:GDI32.AddFontResourceA



_DLL FUNC AnimatePalette(hPalette AS PTR, aUnit AS DWORD, bUnit AS DWORD,;
	lpPaletteentry AS _winPALETTEENTRY);
	AS LOGIC PASCAL:GDI32.AnimatePalette


_DLL FUNC Arc(hdc AS PTR, nLeftRect AS INT, nTopRect AS INT,;
	nRightRect AS INT, nBottomRect AS INT, nXStartArc AS INT,;
	nYAtartArc AS INT, nXEndArc AS INT, nYEnDArc AS INT);
	AS LOGIC PASCAL:GDI32.Arc


_DLL FUNC BitBlt(hdcDest AS PTR, nXDest AS INT, nYDest AS INT,;
	nWidth AS INT, nHeight AS INT, hdcSrc AS PTR,;
	nXSrc AS INT, nYSrc AS INT, dwRop AS DWORD);
	AS LOGIC PASCAL:GDI32.BitBlt


_DLL FUNC CancelDC(hdc AS PTR) AS LOGIC PASCAL:GDI32.CancelDC


_DLL FUNC Chord(hdc AS PTR, nLeftRect AS INT, nTopRect AS INT,;
	nRightRect AS INT, nBottomRect AS INT,;
	nXStartLine AS INT, nYStartLine AS INT,;
	nXEndLine AS INT, nYEndLine AS INT);
	AS LOGIC PASCAL:GDI32.Chord


_DLL FUNC ChoosePixelFormat(hdc AS PTR, lpPIXELFORMATDESCRIPTOR AS _winPIXELFORMATDESCRIPTOR);
	AS INT PASCAL:GDI32.ChoosePixelFormat


_DLL FUNC CloseMetaFile(hdc AS PTR) AS PTR PASCAL:GDI32.CloseMetaFile


_DLL FUNC CombineRgn(hrgnDest AS PTR, hrgnSrc1 AS PTR, hrgnSrc2 AS PTR,;
	fnCombineMode AS INT) AS INT PASCAL:GDI32.CombineRgn



_DLL FUNC CopyMetaFile(hmfSrc AS PTR, lpszFile AS PSZ) AS PTR PASCAL:GDI32.CopyMetaFileA



_DLL FUNC CreateBitmap(nWidth AS INT, nHeight AS INT, cPlanes AS DWORD,;
	cBitsPerPel AS DWORD, lpvBits AS PTR);
	AS PTR PASCAL:GDI32.CreateBitmap


_DLL FUNC CreateBitmapIndirect(lpbm AS _WINBITMAP);
	AS PTR PASCAL:GDI32.CreateBitmapIndirect


_DLL FUNC  CreateBrushIndirect(lplb AS _winLOGBRUSH) AS PTR PASCAL:GDI32.CreateBrushIndirect


_DLL FUNC CreateCompatibleBitmap(hdc AS PTR, nWidth AS INT, nHeight AS INT);
	AS PTR PASCAL:GDI32.CreateCompatibleBitmap


_DLL FUNC CreateDiscardableBitmap(hDc AS PTR, nWidth AS INT,nHeight AS INT);
	AS PTR PASCAL:GDI32.CreateDiscardableBitmap

_DLL FUNC CreateCompatibleDC(hDc AS PTR) AS PTR PASCAL:GDI32.CreateCompatibleDC


_DLL FUNC CreateDC(lpszDriver AS PSZ, lpszDevice AS PSZ, lpszOutput AS PSZ,;
	lpInitData AS _WINDEVMODE) AS PTR PASCAL:GDI32.CreateDCA



_DLL FUNC CreateDIBitmap (hdc AS PTR,  lpbmih AS _WINBITMAPINFOHEADER, fdwInit AS DWORD,;
	lpbInit AS PTR, lpbmt AS _winBITMAPINFO,fuUsage AS DWORD);
	AS PTR PASCAL:GDI32.CreateDIBitmap


_DLL FUNC  CreateDIBPatternBrush(hglbDIBPacked AS PTR, fuColorSpec AS DWORD) AS PTR PASCAL:GDI32.CreateDIBPatternBrush


_DLL FUNC  CreateDIBPatternBrushPt( lpPackedDIB AS PTR, iUsage AS DWORD) AS PTR PASCAL:GDI32.CreateDIBPatternBrushPt


_DLL FUNC  CreateEllipticRgn(nLefteRect AS INT, nTopRect AS INT,nRightRect AS INT, nBottomRect AS INT);
	AS PTR PASCAL:GDI32.CreateEllipticRgn


_DLL FUNC CreateEllipticRgnIndirect(lprc AS _winRECT) AS PTR PASCAL:GDI32.CreateEllipticRgnIndirect



_DLL FUNC CreateFontIndirect (lplf AS _winLOGFONT) AS PTR PASCAL:GDI32.CreateFontIndirectA



_DLL FUNC CreateFont (nHeight AS INT, nWidth AS INT, nEscapement AS INT, nOrientation AS INT,;
	fnWeight AS INT,fdwItalic AS DWORD, fdwUnderline AS DWORD, fdwStrikeOut AS DWORD,;
	fdwCharSet AS DWORD, fdwOutputPrecision AS DWORD, fdwClipPrecision AS DWORD,;
	fdwQuality AS DWORD, fdwPitchAndFamily AS DWORD, lpszFace AS PSZ);
	AS PTR PASCAL:GDI32.CreateFontA



_DLL FUNC CreateHatchBrush (fnStyle AS INT, clref AS DWORD) AS PTR PASCAL:GDI32.CreateHatchBrush


_DLL FUNC  CreateIC(lpszDriver AS PSZ, lpszDevice AS PSZ, lpszOutput AS PSZ, lpdvmInit AS _winDEVMODE );
	AS PTR PASCAL:GDI32.CreateICA


_DLL FUNC CreateMetaFile(lpszFile AS PSZ) AS PTR PASCAL:GDI32.CreateMetaFileA


_DLL FUNC CreatePalette( lplgpl AS _winLOGPALETTE) AS PTR PASCAL:GDI32.CreatePalette


_DLL FUNC CreatePen(fnPenStyle AS INT, nWidth AS INT, clrref AS DWORD) AS PTR PASCAL:GDI32.CreatePen


_DLL FUNC CreatePenIndirect(lplgpn AS _winLOGPEN ) AS PTR PASCAL:GDI32.CreatePenIndirect


_DLL FUNC  CreatePolyPolygonRgn(lppt AS _winPOINT, lpPolyCount AS INT PTR, nCount AS INT,;
	iPolyFillMode AS INT) AS PTR PASCAL:GDI32.CreatePolyPolygonRgn


_DLL FUNC CreatePatternBrush(hbmp AS PTR) AS PTR PASCAL:GDI32.CreatePatternBrush


_DLL FUNC  CreateRectRgn (X1 AS INT, X2 AS INT, X3 AS INT, X4 AS INT) AS PTR PASCAL:GDI32.CreateRectRgn


_DLL FUNC  CreateRectRgnIndirect(lprc AS _winRECT) AS PTR PASCAL:GDI32.CreateRectRgnIndirect


_DLL FUNC CreateRoundRectRgn(nLeftRect AS INT, nTopRect AS INT, nRightRect AS INT,;
	nBottomRect AS INT, nWidthEllipse AS INT, nHeightEllipse AS INT);
	AS PTR PASCAL:gdi32.CreateRoundRectRgn


_DLL FUNC CreateScalableFontResource(fulHidden AS DWORD, lpszResourceFile AS PSZ,;
	lpszFontFile AS PSZ, lpszCurrentPath AS PSZ);
	AS LOGIC PASCAL:GDI32.CreateScalableFontResourceA


_DLL FUNC CreateSolidBrush(crColor AS DWORD) AS PTR PASCAL:GDI32.CreateSolidBrush


_DLL FUNC DeleteDC(hdc AS PTR) AS LOGIC PASCAL:GDI32.DeleteDC


_DLL FUNC DeleteMetaFile(hmf AS PTR ) AS LOGIC PASCAL:GDI32.DeleteMetaFile


_DLL FUNC DeleteObject (hObject AS PTR) AS LOGIC PASCAL:GDI32.DeleteObject


_DLL FUNC DescribePixelFormat( hDc AS PTR, bInt AS INT, dwfmt AS DWORD, lpPfd AS _winPIXELFORMATDESCRIPTOR);
	AS INT PASCAL:GDI32.DescribePixelFormat






/* mode selections for the device mode function */
_DLL FUNC  DrawEscape(hdc AS PTR, nEscape AS INT, cbInput AS INT, lpszInData AS PSZ);
	AS INT PASCAL:GDI32.DrawEscape


_DLL FUNC Ellipse(hdc AS PTR, nLeftRect AS INT, nTopRect AS INT, nRightRect AS INT,;
	nBottomRect AS INT) AS LOGIC PASCAL:GDI32.Ellipse






_DLL FUNC EqualRgn(hSrcRgn1 AS PTR, hSrcRgn2 AS PTR) AS LOGIC PASCAL:GDI32.EqualRgn


_DLL FUNC Escape(hDC AS PTR,   nEscape AS INT, cbInput AS INT, lpvInData AS PSZ,;
	lpvOutData AS PTR) AS INT PASCAL:gdi32.Escape


_DLL FUNC ExtEscape(hdc AS PTR, nEscape AS INT, vbInput AS INT, lpszInData AS PSZ,;
	cbOutput AS INT, lpszOutData AS PSZ) AS INT PASCAL:GDI32.ExtEscape


_DLL FUNC ExcludeClipRect(hdc AS PTR, nLeftRect AS INT , nTopRect AS INT,;
	nRightRecr AS INT, nBottomrECT AS INT);
	AS INT PASCAL:GDI32.ExcludeClipRect


_DLL FUNC ExtCreateRegion(lpXform AS _winXFORM, Count AS DWORD,;
	lpRgnData AS _winRGNDATA) AS PTR PASCAL:GDI32.ExtCreateRegion


_DLL FUNC ExtFloodFill(hdc AS PTR, nXStart AS INT, nYStart AS INT,;
	clrref AS DWORD, fuFillType AS DWORD);
	AS LOGIC PASCAL:GDI32.ExtFloodFill


_DLL FUNC FillRgn(hdc AS PTR, hrgn AS PTR, hbr AS PTR) AS LOGIC PASCAL:GDI32.FillRgn


_DLL FUNC FloodFill(hdc AS PTR, nXStart AS INT, nYStart AS INT,;
	clrref AS DWORD) AS LOGIC PASCAL:GDI32.FloodFill


_DLL FUNC FrameRgn(hdc AS PTR, hrgn AS PTR, hbr AS PTR, nWidth AS INT,;
	nHeight AS INT) AS LOGIC PASCAL:GDI32.FrameRgn


_DLL FUNC GetROP2( hdc AS PTR) AS INT PASCAL:GDI32.GetROP2


_DLL FUNC GetAspectRatioFilterEx(hdc AS PTR, lpAspectRation AS _winSIZE);
	AS LOGIC PASCAL:GDI32.GetAspectRatioFilterEx



_DLL FUNC GetBkColor(hdc AS PTR) AS DWORD PASCAL:gdi32.GetBkColor


_DLL FUNC GetBkMode(hdc AS PTR) AS INT PASCAL:gdi32.GetBkMode


_DLL FUNC GetBitmapBits(hbmP AS PTR, cbBuffer AS LONG,;
	lpvBits AS PTR) AS LONG PASCAL:GDI32.GetBitmapBits


_DLL FUNC GetBitmapDimensionEx(hBitmap AS PTR, lpDimension AS _winSIZE);
	AS LOGIC PASCAL:GDI32.GetBitmapDimensionEx


_DLL FUNC GetBoundsRect( hdc AS PTR, lptcBounds AS _winRECT,;
	flags AS INT) AS DWORD PASCAL:GDI32.GetBoundsRect


_DLL FUNC GetBrushOrgEx(hdc AS PTR, lppt AS _winPOINT) AS LOGIC PASCAL:GDI32.GetBrushOrgEx



_DLL FUNC GetCharWidth(hdc AS PTR, iFirstChar AS DWORD, iLastChar AS DWORD,;
	lpBuffer AS INT PTR) AS LOGIC PASCAL:GDI32.GetCharWidthA





_DLL FUNC GetCharWidth32(hdc AS PTR, iFirstChar AS DWORD, iLastChar AS DWORD, ;
	lpBuffer AS INT PTR) AS LOGIC PASCAL:GDI32.GetCharWidth32A




_DLL FUNC GetCharWidthFloat(hdc AS PTR, iFirstChar AS DWORD, iLastChar AS DWORD,;
	pxBuffer AS REAL4 PTR) AS LOGIC PASCAL:GDI32.GetCharWidthFloatA




_DLL FUNC GetCharABCWidths(hdc AS PTR, uFirstChar AS DWORD, uLastChar AS DWORD,;
	lpabc AS _winABC ) AS LOGIC PASCAL:gdi32.GetCharABCWidthsA



_DLL FUNC GetCharABCWidthsFloat(hdc AS PTR, uFirstChar AS DWORD, uLastChar AS DWORD,;
	lpABCF AS _winABCFLOAT) AS LOGIC PASCAL:GDI32.GetCharABCWidthsFloatA



_DLL FUNC GetClipBox( hdc AS PTR, lprc AS _winRECT) AS INT PASCAL:GDI32.GetClipBox


_DLL FUNC GetClipRgn( hdc AS PTR, hrgn AS PTR ) AS INT PASCAL:GDI32.GetClipRgn


_DLL FUNC GetMetaRgn(hdc AS PTR, hrgn AS PTR ) AS INT PASCAL:GDI32.GetMetaRgn


_DLL FUNC GetCurrentObject (hdc AS PTR, uObjectType AS DWORD) AS PTR PASCAL:GDI32.GetCurrentObject


_DLL FUNC GetCurrentPositionEx ( hdc AS PTR, lppt AS _winPOINT);
	AS LOGIC PASCAL:GDI32.GetCurrentPositionEx


_DLL FUNC GetDeviceCaps( hdc AS PTR, nIndex AS INT) AS INT PASCAL:GDI32.GetDeviceCaps


_DLL FUNC GetDIBits(hdc AS PTR, hbmp AS PTR, uStartScan AS DWORD, cScanLines AS DWORD,;
	lpvBits AS PTR, lpbi AS _winBITMAPINFO, uUsage AS DWORD) AS INT PASCAL:GDI32.GetDIBits


_DLL FUNC GetFontData( hdc AS PTR, dwTable AS DWORD, dwOffset AS DWORD,;
	lpvBuffer AS PTR, cbData AS DWORD) AS DWORD PASCAL:GDI32.GetFontData


_DLL FUNC GetGlyphOutline (hdc AS PTR, uChar AS DWORD, uFormat AS DWORD,;
	lpMetrics AS _winGLYPHMETRICS ,cbBuffer AS DWORD, lpvBuffer AS PTR,;
	lpMetrix AS _winMAT2) AS DWORD PASCAL:GDI32.GetGlyphOutlineA



_DLL FUNC GetGraphicsMode(hdc AS PTR) AS INT PASCAL:GDI32.GetGraphicsMode


_DLL FUNC GetMapMode(hdc AS PTR) AS INT PASCAL:GDI32.GetMapMode


_DLL FUNC GetMetaFileBitsEx(hmf AS PTR, nSize AS DWORD, lpvData AS PTR);
	AS DWORD PASCAL:GDI32.GetMetaFileBitsEx



_DLL FUNC GetMetaFile(lpszFile AS PSZ) AS PTR PASCAL:gdi32.GetMetaFileA



_DLL FUNC GetNearestColor( hdc AS PTR, clrref AS DWORD) AS DWORD PASCAL:gdi32.GetNearestColor


_DLL FUNC GetNearestPaletteIndex(hdc AS PTR, crColor AS DWORD);
	AS DWORD PASCAL:gdi32.GetNearestPaletteIndex


_DLL FUNC GetObjectType( hgdiobj AS PTR) AS DWORD PASCAL:GDI32.GetObjectType



_DLL FUNC GetOutlineTextMetrics(hdc AS PTR, cbData AS DWORD,;
	lpOTM AS _winOUTLINETEXTMETRIC);
	AS DWORD PASCAL:GDI32.GetOutlineTextMetricsA




_DLL FUNC GetPaletteEntries(hpal AS PTR,   iStartIndex AS DWORD,;
	nEntries AS DWORD, lppe AS _winPALETTEENTRY);
	AS DWORD PASCAL:GDI32.GetPaletteEntries


_DLL FUNC GetPixel(hdc AS PTR, nXPos AS INT, nYPos AS INT) AS DWORD PASCAL:GDI32.GetPixel


_DLL FUNC GetPixelFormat(hdc AS PTR) AS INT PASCAL:gdi32.GetPixelFormat


_DLL FUNC GetPolyFillMode(hdc AS PTR) AS INT PASCAL:GDI32.GetPolyFillMode


_DLL FUNC GetRasterizerCaps(lpStatus AS _winRASTERIZER_STATUS, cb AS DWORD);
	AS LOGIC PASCAL:GDI32.GetRasterizerCaps


_DLL FUNC GetRegionData( hRgn AS PTR, nCount AS DWORD, lpRgnData AS _winRGNDATA);
	AS DWORD PASCAL:GDI32.GetRegionData


_DLL FUNC GetRgnBox( hRgn AS PTR, lpRect AS _winRECT) AS INT PASCAL:GDI32.GetRgnBox


_DLL FUNC GetStockObject(fnObject AS INT ) AS PTR PASCAL:GDI32.GetStockObject


_DLL FUNC GetStretchBltMode(hdc AS PTR) AS INT PASCAL:GDI32.GetStretchBltMode


_DLL FUNC GetSystemPaletteEntries( hdc AS PTR, iStartIndex AS DWORD, nEntries AS DWORD,;
	lppe AS _winPALETTEENTRY);
	AS DWORD PASCAL:GDI32.GetSystemPaletteEntries


_DLL FUNC GetSystemPaletteUse(hdc AS PTR) AS DWORD PASCAL:gdi32.GetSystemPaletteUse


_DLL FUNC GetTextCharacterExtra(hdc AS PTR) AS INT PASCAL:GDI32.GetTextCharacterExtra


_DLL FUNC GetTextAlign(hdc AS PTR) AS DWORD PASCAL:GDI32.GetTextAlign


_DLL FUNC GetTextColor( hdc AS PTR) AS DWORD PASCAL:GDI32.GetTextColor



_DLL FUNC  GetTextExtentPoint( hdc AS PTR, lpsz AS PSZ, cbString AS INT,;
	lpSize AS _winSIZE) AS LOGIC PASCAL:gdi32.GetTextExtentPointA




_DLL FUNC GetTextExtentPoint32( hdC AS PTR, lpsz AS PSZ, cbString AS INT,;
	lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.GetTextExtentPoint32A




_DLL FUNC GetTextExtentExPoint(hdc AS PTR, lpszString AS PSZ, cchString AS INT,;
	nMaxExtent AS INT, lpnFit AS INT PTR, alpDx AS INT PTR,;
	lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.GetTextExtentExPointA



_DLL FUNC GetTextCharset(hdc AS PTR) AS INT PASCAL:GDI32.GetTextCharset


_DLL FUNC GetTextCharsetInfo( hdc AS PTR, lpSing AS _winFONTSIGNATURE,;
	dwFlags AS DWORD) AS INT PASCAL:GDI32.GetTextCharsetInfo


_DLL FUNC TranslateCharsetInfo( lpSrc AS DWORD PTR,lpCs AS _winCHARSETINFO,;
	dwFlags AS DWORD) AS LOGIC PASCAL:GDI32.TranslateCharsetInfo


_DLL FUNC  GetFontLanguageInfo(hdc AS PTR) AS DWORD PASCAL:GDI32.GetFontLanguageInfo



_DLL FUNC GetCharacterPlacement(hdc AS PTR, lpsz AS PSZ, bint1 AS INT, bint2 AS INT,;
	lpgcpr AS  _winGCP_RESULTS, dwFlags AS DWORD);
	AS DWORD PASCAL:GDI32.GetCharacterPlacementA



_DLL FUNC GetViewportExtEx(hdc AS PTR, lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.GetViewportExtEx


_DLL FUNC GetViewportOrgEx(hdc AS PTR, lppt AS _winPOINT) AS LOGIC PASCAL:GDI32.GetViewportOrgEx


_DLL FUNC GetWindowExtEx(hdc AS PTR, lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.GetWindowExtEx


_DLL FUNC GetWindowOrgEx(hdc AS PTR,lppt AS _winPOINT) AS LOGIC PASCAL:GDI32.GetWindowOrgEx


_DLL FUNC IntersectClipRect( hdc AS PTR, nLeftRect AS INT, nTopRect AS INT,;
	nRightRect AS INT, nBottomRect AS INT);
	AS INT PASCAL:GDI32.IntersectClipRect


_DLL FUNC InvertRgn( hdc AS PTR, hRgn AS PTR) AS LOGIC PASCAL:GDI32.InvertRgn


_DLL FUNC LineDDA( X1 AS INT, Y1 AS INT, X2 AS INT, Y2 AS INT,;
	lpLineFunc AS PTR, lpData AS PTR) AS LOGIC PASCAL:GDI32.LineDDA


_DLL FUNC LineTo(hdc AS PTR, nXEnd AS INT, nYEnd AS INT) AS LOGIC PASCAL:GDI32.LineTo


_DLL FUNC MaskBlt(hdcDest AS PTR, nXDest AS INT, nYDest AS INT, nWidth AS INT,;
	nHeight AS INT, hdcSrc AS PTR, nXSrc AS INT, nYSrc AS INT,;
	hbmMask AS PTR, xMask AS INT, yMask AS INT, dwRop AS DWORD);
	AS LOGIC PASCAL:GDI32.MaskBlt


_DLL FUNC PlgBlt(hdcDest AS PTR, lpPoint AS _winPOINT, hdcSrc AS PTR,;
	nXSrc AS INT, nYSrc AS INT, nWidth AS INT, nHeight AS INT,;
	hbmMask AS PTR, xMask AS INT, YMask AS INT);
	AS LOGIC PASCAL:GDI32.PlgBlt



_DLL FUNC OffsetClipRgn( hdc AS PTR, nXOffset AS INT, nYOffset AS INT);
	AS INT PASCAL:gdi32.OffsetClipRgn


_DLL FUNC OffsetRgn( hrg AS PTR, nXOffset AS INT, nYOffset AS INT);
	AS INT PASCAL:GDI32.OffsetRgn


_DLL FUNC PatBlt( hdc AS PTR, nXLeft AS INT, nYLeft AS INT, nWidth AS INT,;
	nHeight AS INT, Rop AS DWORD) AS LOGIC PASCAL:GDI32.PatBlt


_DLL FUNC  Pie(hdc AS PTR, X1 AS INT, Y1 AS INT, X2 AS INT, Y2 AS INT,;
	X3 AS INT, Y3 AS INT, X4 AS INT, Y4 AS INT);
	AS LOGIC PASCAL:GDI32.Pie


_DLL FUNC PlayMetaFile(hdc AS PTR, hmf AS PTR) AS LOGIC PASCAL:GDI32.PlayMetaFile


_DLL FUNC PaintRgn( hdc AS PTR, hrgn AS PTR) AS LOGIC PASCAL:GDI32.PaintRgn


_DLL FUNC PolyPolygon(hdc AS PTR, lpPoints AS _winPOINT, lpPolyCounts AS INT PTR,;
	nCount AS INT) AS LOGIC PASCAL:GDI32.PolyPolygon


_DLL FUNC PtInRegion( hrgn AS PTR, X AS INT, y AS INT) AS LOGIC PASCAL:GDI32.PtInRegion


_DLL FUNC PtVisible(hdc AS PTR, X AS INT, Y AS INT) AS LOGIC PASCAL:GDI32.PtVisible


_DLL FUNC RectInRegion( hrgn AS PTR, lpRect AS _winRECT) AS LOGIC PASCAL:GDI32.RectInRegion


_DLL FUNC RectVisible(hdc AS PTR, lprc AS _winRECT) AS LOGIC PASCAL:GDI32.RectVisible



_DLL FUNC Rectangle(hdc AS PTR, X1 AS INT, Y1 AS INT, X2 AS INT, Y2 AS INT);
	AS LOGIC PASCAL:GDI32.Rectangle


_DLL FUNC RestoreDC(hdc AS PTR, nSavedDc AS INT) AS LOGIC PASCAL:GDI32.RestoreDC


_DLL FUNC ResetDC(hdc AS PTR, lpInitData AS _winDEVMODE) AS PTR PASCAL:GDI32.ResetDCA



_DLL FUNC RealizePalette( hdc AS PTR) AS DWORD PASCAL:GDI32.RealizePalette


_DLL FUNC RemoveFontResource(lpFileName AS PSZ) AS LOGIC PASCAL:GDI32.RemoveFontResourceA



_DLL FUNC RoundRect(hdc AS PTR, X1 AS INT, Y1 AS INT, X2 AS INT, Y2 AS INT,;
	X3 AS INT, X4 AS INT) AS LOGIC PASCAL:GDI32.RoundRect


_DLL FUNC ResizePalette( hpal AS PTR, nEntries AS DWORD) AS LOGIC PASCAL:GDI32.ResizePalette


_DLL FUNC SaveDC(hdc AS PTR) AS INT PASCAL:GDI32.SaveDC


_DLL FUNC SelectClipRgn( hdc AS PTR, hrgn AS PTR) AS INT PASCAL:GDI32.SelectClipRgn


_DLL FUNC ExtSelectClipRgn( hdc AS PTR, hrgn AS PTR, iMode AS INT);
	AS INT PASCAL:GDI32.ExtSelectClipRgn


_DLL FUNC SetMetaRgn( hdc AS PTR) AS INT PASCAL:GDI32.SetMetaRgn


_DLL FUNC SelectObject(hdc AS PTR, hgdiobj AS PTR) AS PTR PASCAL:GDI32.SelectObject


_DLL FUNC SelectPalette( hdc AS PTR, hpal AS PTR, bForceBackground AS LOGIC);
	AS PTR PASCAL:GDI32.SelectPalette


_DLL FUNC SetBkColor(hdc AS PTR, clrref AS DWORD) AS DWORD PASCAL:GDI32.SetBkColor


_DLL FUNC SetBkMode(hdc AS PTR, iBkMOde AS PTR) AS INT PASCAL:GDI32.SetBkMode


_DLL FUNC SetBitmapBits(hbmp AS PTR, cBytes AS DWORD, lpBits AS PTR);
	AS LONG PASCAL:GDI32.SetBitmapBits


_DLL FUNC SetBoundsRect(hdc AS PTR, lprcBounds AS _winRECT, flags AS DWORD);
	AS DWORD PASCAL:GDI32.SetBoundsRect


_DLL FUNC SetDIBits( hdc AS PTR, hbmp AS PTR, uStartScan AS DWORD, cScanLines AS DWORD,;
	lpvBits AS PTR, lpBmi AS _winBITMAPINFO, fuColorUse AS DWORD);
	AS INT PASCAL:GDI32.SetDIBits


_DLL FUNC SetDIBitsToDevice(hdc AS PTR, xDest AS INT, YDest AS INT, dwWidth AS DWORD,;
	deHeight AS DWORD, XSrc AS INT, YSrc AS INT,;
	uStartScan AS DWORD, cScanLines AS DWORD,lpvBits AS PTR,;
	lpbmi AS _winBITMAPINFO, fuColorUse AS DWORD);
	AS INT PASCAL:GDI32.SetDIBitsToDevice


_DLL FUNC SetMapperFlags(hdc AS PTR, f AS DWORD ) AS DWORD PASCAL:GDI32.SetMapperFlags


_DLL FUNC SetGraphicsMode(hdc AS PTR, iMode AS INT) AS INT PASCAL:GDI32.SetGraphicsMode


_DLL FUNC SetMapMode(hdc AS PTR, fnMapMode AS INT) AS INT PASCAL:GDI32.SetMapMode


_DLL FUNC SetMetaFileBitsEx(nSize AS DWORD, lpData AS BYTE PTR) AS PTR PASCAL:GDI32.SetMetaFileBitsEx


_DLL FUNC SetPaletteEntries(hpal AS PTR, iStart AS DWORD, cEntries AS DWORD,;
	lppe AS _winPALETTEENTRY) AS DWORD PASCAL:GDI32.SetPaletteEntries


_DLL FUNC SetPixel(hdc AS PTR, X AS INT, Y AS INT, crColor AS DWORD);
	AS DWORD PASCAL:GDI32.SetPixel


_DLL FUNC SetPixelV(hdc AS PTR, X AS INT, Y AS INT, crColor AS DWORD);
	AS LOGIC PASCAL:GDI32.SetPixelV


_DLL FUNC SetPixelFormat(hdc AS PTR, iPixelFormat AS INT,;
	ppfd AS _winPIXELFORMATDESCRIPTOR) AS LOGIC PASCAL:GDI32.SetPixelFormat


_DLL FUNC SetPolyFillMode(hdc AS PTR, iPolyFillMode AS INT) AS INT PASCAL:GDI32.SetPolyFillMode



_DLL FUNC StretchBlt(hdc AS PTR, nXOriginDest AS INT, nYOriginDest AS INT,;
	nWidthDest AS INT, nHeihtDest AS INT, hdcSRC AS PTR,;
	nXOriginSrc AS INT, nYOriginSrc AS INT, nWidthSrc AS INT,;
	nHeightSrc AS INT, dwRop AS DWORD) AS LOGIC PASCAL:GDI32.StretchBlt


_DLL FUNC SetRectRgn(hrgn AS PTR, nLeftRect AS INT, nTopRect AS INT,nRightRect AS INT,;
	nBottomRect AS INT) AS LOGIC PASCAL:GDI32.SetRectRgn


_DLL FUNC StretchDIBits(hdc AS PTR, XDest AS INT, yDest AS INT, nDestWidth AS INT,;
	nDestHeight AS INT, XSrc AS INT, YSrc AS INT, nSrcWidth AS INT,;
	nSrcHeight AS INT, lpBits AS PTR, lpBitsInfo AS _winBITMAPINFO,;
	iUsage AS DWORD, Rop AS DWORD) AS INT PASCAL:GDI32.StretchDIBits


_DLL FUNC SetROP2(hdc AS PTR, fnDrawMode AS INT) AS INT PASCAL:GDI32.SetROP2


_DLL FUNC SetStretchBltMode(hdc AS PTR, iStretchMode AS INT) AS INT PASCAL:GDI32.SetStretchBltMode


_DLL FUNC SetSystemPaletteUse(hdc AS PTR, uUsage AS DWORD) AS DWORD PASCAL:GDI32.SetSystemPaletteUse


_DLL FUNC SetTextCharacterExtra(hdc AS PTR, nCharxtra AS INT) AS INT PASCAL:GDI32.SetTextCharacterExtra


_DLL FUNC SetTextColor(hdc AS PTR, crColor AS DWORD) AS DWORD PASCAL:GDI32.SetTextColor


_DLL FUNC SetTextAlign(hdc AS PTR, fMode AS DWORD) AS DWORD PASCAL:GDI32.SetTextAlign


_DLL FUNC SetTextJustification( hdc AS PTR, nBreakExtra AS INT, nBreakCount AS INT);
	AS LOGIC PASCAL:GDI32.SetTextJustification


_DLL FUNC UpdateColors(hdc AS PTR) AS LOGIC PASCAL:GDI32.UpdateColors



_DLL FUNC PlayMetaFileRecord(hdc AS PTR, lpHandletable AS _winHANDLETABLE,;
	lpMetaRecord AS _WINMETARECORD , nHabdles AS DWORD);
	AS LOGIC PASCAL:GDI32.PlayMetaFileRecord



_DLL FUNC EnumMetaFile(hdc AS PTR, hmf AS PTR, lpMetaFunc AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:GDI32.EnumMetaFile




_DLL FUNC CloseEnhMetaFile( hdc AS PTR) AS PTR PASCAL:GDI32.CloseEnhMetaFile


_DLL FUNC CopyEnhMetaFile( hemfSrc AS PTR, lpszFile AS PTR) AS PTR PASCAL:GDI32.CopyEnhMetaFileA




_DLL FUNC CreateEnhMetaFile(hdcRef AS PTR, lpFilename AS PSZ, lpRect AS _winRECT,;
	lpDestcription AS PSZ) AS PTR PASCAL:GDI32.CreateEnhMetaFileA



_DLL FUNC DeleteEnhMetaFile(hemf AS PTR) AS LOGIC PASCAL:GDI32.DeleteEnhMetaFile


_DLL FUNC EnumEnhMetaFile(hdc AS PTR, hemf AS PTR, lpEnHMetaFunc AS PTR,;
	lpData AS PTR, lpRect AS _winRECT);
	AS LOGIC PASCAL:GDI32.EnumEnhMetaFile


_DLL FUNC GetEnhMetaFile(lpszMetaFile AS PSZ) AS PTR PASCAL:GDI32.GetEnhMetaFileA



_DLL FUNC GetEnhMetaFileBits( hemf AS PTR, cbBuffer AS DWORD, lpbBuffer AS BYTE PTR);
	AS DWORD PASCAL:GDI32.GetEnhMetaFileBits



_DLL FUNC GetEnhMetaFileDescription(hemf AS PTR, cchBuffer AS DWORD,;
	lpszDescription AS PSZ);
	AS DWORD PASCAL:GDI32.GetEnhMetaFileDescriptionA



_DLL FUNC GetEnhMetaFileHeader(hemf AS PTR, cbBuffer AS DWORD, lpemh AS _winENHMETAHEADER);
	AS DWORD PASCAL:GDI32.GetEnhMetaFileHeader


_DLL FUNC GetEnhMetaFilePaletteEntries(hemf AS PTR, cEntries AS DWORD,;
	lppe AS _winPALETTEENTRY);
	AS DWORD PASCAL:GDI32.GetEnhMetaFilePaletteEntries


_DLL FUNC GetWinMetaFileBits(hemf AS PTR, cbBuffer AS DWORD, lpbBuffer AS BYTE PTR,;
	fnMapMode AS INT, hdcRef AS PTR);
	AS DWORD PASCAL:GDI32.GetWinMetaFileBits



_DLL FUNC PlayEnhMetaFile(hdc AS PTR, hemf AS PTR, lpRect AS _winRECT);
	AS LOGIC PASCAL:GDI32.PlayEnhMetaFile


_DLL FUNC PlayEnhMetaFileRecord(hdc AS PTR, lpHandleTable AS _winHANDLETABLE,;
	lpEnhMetaRecord AS _winENHMETARECORD, nHandle AS DWORD);
	AS LOGIC PASCAL:gdi32.PlayEnhMetaFileRecord


_DLL FUNC SetEnhMetaFileBits(cbBuffer AS DWORD, lpData AS BYTE PTR);
	AS PTR PASCAL:GDI32.SetEnhMetaFileBits


_DLL FUNC SetWinMetaFileBits(cbBuffer AS DWORD, lpbBuffer AS BYTE PTR, hdcRef AS PTR,;
	lpmfp AS _winMETAFILEPICT) AS PTR PASCAL:gdi32.SetWinMetaFileBits


_DLL FUNC GdiComment(hdc AS PTR, nSize AS DWORD, lpData AS BYTE PTR);
	AS LOGIC PASCAL:GDI32.GdiComment




_DLL FUNC GetTextMetrics(hdc AS PTR, lpMetries AS _winTEXTMETRIC);
	AS LOGIC PASCAL:GDI32.GetTextMetricsA








_DLL FUNC AngleArc(hdc AS PTR, X AS INT, Y AS INT, nRadius AS DWORD,;
	eStartAngle AS REAL4, eSweepAngle AS REAL4);
	AS LOGIC PASCAL:GDI32.AngleArc


_DLL FUNC PolyPolyline(hdc AS PTR, lpPoint AS _winPOINT, lpPolyCount AS DWORD PTR,;
	nCount AS DWORD) AS LOGIC PASCAL:GDI32.PolyPolyline


_DLL FUNC GetWorldTransform(hdc AS PTR, lpXform AS _winXFORM);
	AS LOGIC PASCAL:GDI32.GetWorldTransform


_DLL FUNC SetWorldTransform( hdc AS PTR, lpXform AS _winXFORM);
	AS LOGIC PASCAL:GDI32.SetWorldTransform


_DLL FUNC ModifyWorldTransform(hdc AS PTR, lpXform AS _winXFORM, iMOde AS DWORD);
	AS LOGIC PASCAL:GDI32.ModifyWorldTransform


_DLL FUNC CombineTransform(lpxformResult AS _winXFORM, lpXForm1 AS _winXFORM,;
	lpXform2 AS _winXFORM) AS LOGIC PASCAL:GDI32.CombineTransform


_DLL FUNC CreateDIBSection(hdc AS PTR, pbmi AS _winBITMAPINFO, iUsage AS DWORD,;
	ppvBits AS PTR, hSection AS PTR, dwOffset AS DWORD);
	AS PTR PASCAL:GDI32.CreateDIBSection


_DLL FUNC GetDIBColorTable(hdc AS PTR, uStartIndex AS DWORD, cEntries AS DWORD,;
	pColors AS _winRGBQUAD) AS DWORD PASCAL:GDI32.GetDIBColorTable


_DLL FUNC SetDIBColorTable(hdc AS PTR, uStartIndex AS DWORD, cEntries AS DWORD,;
	pColors AS _winRGBQUAD) AS DWORD PASCAL:GDI32.SetDIBColorTable


/* Flags value for COLORADJUSTMENT */
_DLL FUNC SetColorAdjustment(hdc AS PTR, lpca AS _winCOLORADJUSTMENT);
	AS LOGIC PASCAL:GDI32.SetColorAdjustment


_DLL FUNC GetColorAdjustment(hdc AS PTR, lpca AS _winCOLORADJUSTMENT);
	AS LOGIC PASCAL:GDI32.GetColorAdjustment


_DLL FUNC CreateHalftonePalette(hdc AS PTR) AS PTR PASCAL:GDI32.CreateHalftonePalette



_DLL FUNC StartDoc (_Hdc AS PTR, lpdi AS _winDocInfo) AS INT PASCAL:GDI32.StartDocA



_DLL FUNC EndDoc(hdc AS PTR) AS INT PASCAL:GDI32.EndDoc


_DLL FUNC StartPage(hdc AS PTR ) AS INT PASCAL:GDI32.StartPage


_DLL FUNC EndPage (hdc AS PTR) AS INT PASCAL:GDI32.EndPage


_DLL FUNC AbortDoc(hdc AS PTR) AS INT PASCAL:GDI32.AbortDoc



_DLL FUNC SetAbortProc(hdc AS PTR, lpAbortproc AS PTR) AS INT PASCAL:GDI32.SetAbortProc


_DLL FUNC AbortPath( hdc AS PTR) AS LOGIC PASCAL:GDI32.AbortPath


_DLL FUNC ArcTo(hdc AS PTR, X1 AS INT, Y1 AS INT, X2 AS INT, Y2 AS INT,;
	X3 AS INT, Y3 AS INT, X4 AS INT, Y4 AS INT) AS LOGIC PASCAL:GDI32.ArcTo



_DLL FUNC BeginPath(hdc AS PTR) AS LOGIC PASCAL:GDI32.BeginPath


_DLL FUNC CloseFigure(hdc AS PTR) AS LOGIC PASCAL:GDI32.CloseFigure


_DLL FUNC EndPath(hdc AS PTR) AS LOGIC PASCAL:GDI32.EndPath


_DLL FUNC FillPath(hdc AS PTR) AS LOGIC PASCAL:GDI32.FillPath


_DLL FUNC FlattenPath( hdc AS PTR) AS LOGIC PASCAL:GDI32.FlattenPath


_DLL FUNC GetPath(hdc AS PTR, lpPoints AS _winPOINT, lpType AS BYTE PTR,;
	nSize AS INT) AS INT PASCAL:GDI32.GetPath


_DLL FUNC PathToRegion( hdc AS PTR) AS PTR PASCAL:GDI32.PathToRegion


_DLL FUNC PolyDraw( hdc AS PTR,  lpPoints AS _winPOINT,;
	lpType AS BYTE PTR, nCount AS INT) AS LOGIC PASCAL:GDI32.PolyDraw


_DLL FUNC SelectClipPath(hdc AS PTR, iMOde AS INT) AS LOGIC PASCAL:GDI32.SelectClipPath


_DLL FUNC SetArcDirection(hdc AS PTR, ArcDirection AS INT) AS INT PASCAL:GDI32.SetArcDirection


_DLL FUNC SetMiterLimit(hdc AS PTR,  eNewLimit AS REAL4,   peOldLimit AS REAL4 PTR);
	AS LOGIC PASCAL:GDI32.SetMiterLimit



_DLL FUNC StrokeAndFillPath(hdc AS PTR ) AS LOGIC PASCAL:GDI32.StrokeAndFillPath


_DLL FUNC StrokePath(hdc AS PTR) AS LOGIC PASCAL:GDI32.StrokePath


_DLL FUNC WidenPath( hdc AS PTR) AS LOGIC PASCAL:GDI32.WidenPath


_DLL FUNC ExtCreatePen(dwPenStyle AS DWORD, dwWidth AS DWORD, lplb AS _winLOGBRUSH,;
	dwStyleCount AS DWORD, lpStyle AS DWORD PTR)AS PTR PASCAL:GDI32.ExtCreatePen


_DLL FUNC GetMiterLimit( hdc AS PTR, peLimit AS REAL4 PTR) AS LOGIC PASCAL:GDI32.GetMiterLimit


_DLL FUNC GetArcDirection( hdc AS PTR) AS INT PASCAL:GDI32.GetArcDirection


_DLL FUNC GetObject(hgdiobj AS PTR, cbBuffer AS INT, lpvObject AS PTR);
	AS INT PASCAL:GDI32.GetObjectA


_DLL FUNC MoveToEx(hdc AS PTR, X AS INT, y AS INT, lpPoint AS _winPOINT);
	AS LOGIC PASCAL:GDI32.MoveToEx


_DLL FUNC TextOut(hdc AS PTR, nXStart AS INT, nYStart AS INT, lpszString AS PSZ,;
	cbString AS INT) AS LOGIC PASCAL:GDI32.TextOutA



_DLL FUNC ExtTextOut(hdc AS PTR,   X AS INT, Y AS INT, fOptions AS DWORD,;
	lprc AS _winRECT,  lpString AS PSZ,	  nCount AS DWORD,;
	lpDx AS INT PTR) AS LOGIC PASCAL:GDI32.ExtTextOutA


_DLL FUNC PolyTextOut(hdc AS PTR, pptxt AS _winPOLYTEXT, cStrings AS INT);
	AS LOGIC PASCAL:GDI32.PolyTextOutA



_DLL FUNC CreatePolygonRgn(lppt AS _winPOINT, cPoints AS INT, fnPolyFillMode AS INT);
	AS PTR PASCAL:GDI32.CreatePolygonRgn


_DLL FUNC DPtoLP( hdc AS PTR, lpPoins AS _winPOINT, nCount AS INT);
	AS LOGIC PASCAL:GDI32.DPtoLP


_DLL FUNC LPtoDP(hdc AS PTR, lpPoints AS _winPoint, nCount AS INT);
	AS LOGIC PASCAL:GDI32.LPtoDP


_DLL FUNC Polygon(hdc AS PTR, lpPoints AS _winPOINT, nCOunt AS INT);
	AS LOGIC PASCAL:GDI32.Polygon


_DLL FUNC  POlyline( hdc AS PTR, lpPoints AS _winPOINT, nPoints AS INT);
	AS LOGIC PASCAL:GDI32.Polyline


_DLL FUNC PolyBezier (hdc AS PTR, lpPoints AS _winPOINT, nCount AS DWORD);
	AS LOGIC PASCAL:GDI32.PolyBezier


_DLL FUNC PolyBezierTo( hdc AS PTR, lpPoints AS _winPOINT, nCount AS DWORD);
	AS LOGIC PASCAL:GDI32.PolyBezierTo


_DLL FUNC PolylineTo( hdc AS PTR , lpPoints AS _winPOINT, nCOunt AS DWORD);
	AS LOGIC PASCAL:GDI32.PolylineTo


_DLL FUNC SetViewportExtEx( hdc AS PTR, nXExtent AS INT, nYExtent AS INT,;
	lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.SetViewportExtEx


_DLL FUNC SetViewportOrgEx(hdc AS PTR, X AS INT, Y AS INT, lpPoints AS _winPOINT);
	AS LOGIC PASCAL:GDI32.SetViewportOrgEx


_DLL FUNC SetWindowExtEx( hdc AS PTR, nXExtent AS INT, nYExtent AS INT,;
	lpSize AS _winSIZE) AS LOGIC PASCAL:GDI32.SetWindowExtEx


_DLL FUNC SetWindowOrgEx(hdc AS PTR, X AS INT, Y AS INT, lpPoint AS _winPOINT);
	AS  LOGIC PASCAL:GDI32.SetWindowOrgEx


_DLL FUNC OffsetViewportOrgEx( hdc AS PTR, nXOffset AS INT, nYOffset AS INT,;
	lpPoint AS _winPOINT) AS LOGIC PASCAL:GDI32.OffsetViewportOrgEx


_DLL FUNC OffsetWindowOrgEx(hdc AS PTR, nXOffset AS INT, nYOffset AS INT,;
	lpPoint AS _winPOINT) AS LOGIC PASCAL:GDI32.OffsetWindowOrgEx


_DLL FUNC ScaleViewportExtEx( hdc AS PTR, Xnum AS INT, Xdenom AS INT, Ynum AS INT,;
	Ydenom AS INT, lpSize AS _winSIZE);
	AS LOGIC PASCAL:GDI32.ScaleViewportExtEx


_DLL FUNC ScaleWindowExtEx( hdc AS PTR, Xnum AS INT, Xdenom AS INT, Ynum AS INT,;
	Ydenom AS INT, lpSize AS _winSIZE);
	AS LOGIC PASCAL:GDI32.ScaleWindowExtEx


_DLL FUNC SetBitmapDimensionEx(hBitmap AS PTR, nWidth AS INT,  nHeight AS INT,;
	lpSize AS _winSIZE);
	AS LOGIC PASCAL:GDI32.SetBitmapDimensionEx


_DLL FUNC SetBrushOrgEx( hdc AS PTR, nXOrg AS INT, nYOrg AS INT,;
	lppt AS _winPOINT) AS LOGIC PASCAL:GDI32.SetBrushOrgEx


_DLL FUNC GetTextFace(hdc AS PTR,nCount AS INT, lpFaceName AS PSZ) AS INT PASCAL:GDI32.GetTextFaceA


_DLL FUNC GetKerningPairs( hdc AS PTR, nNumberPairs AS DWORD,;
	lpKernPairEntries AS _winKERNINGPAIR);
	AS DWORD PASCAL:GDI32.GetKerningPairsA



_DLL FUNC GetDCOrgEx( hdc AS PTR, lppt AS _winPOINT) AS LOGIC PASCAL:GDI32.GetDCOrgEx


_DLL FUNC FixBrushOrgEx( hdc AS PTR, bint1 AS INT, bint2 AS INT, lppt AS _winPOINT);
	AS LOGIC PASCAL:GDI32.FixBrushOrgEx


_DLL FUNC UnrealizeObject(hgdiobj AS PTR) AS LOGIC PASCAL:GDI32.UnrealizeObject


_DLL FUNC GdiFlush() AS LOGIC PASCAL:GDI32.GdiFlush


_DLL FUNC GdiSetBatchLimit(cLimit AS DWORD) AS DWORD PASCAL:GDI32.GdiSetBatchLimit


_DLL FUNC GdiGetBatchLimit() AS DWORD PASCAL:GDI32.GdiGetBatchLimit


_DLL FUNC SetICMMode( hdc AS PTR, fICM AS INT) AS INT PASCAL:GDI32.SetICMMode


_DLL FUNC CheckColorsInGamut( hdc AS PTR,lpaRGBQuad AS PTR, lpResult AS PTR,;
	nCount AS DWORD) AS LOGIC PASCAL:GDI32.CheckColorsInGamut




_DLL FUNC GetLogColorSpace(hColorSpace AS PTR, lpbuffer AS _winLOGCOLORSPACE,;
	nSize AS DWORD) AS LOGIC PASCAL:GDI32.GetLogColorSpaceA


_DLL FUNC CreateColorSpace(lpLogColorSpace AS _winLOGCOLORSPACE);
	AS PTR PASCAL:GDI32.CreateColorSpaceA


_DLL FUNC SetColorSpace( hdc AS PTR, hColorSpace AS PTR) AS LOGIC PASCAL:GDI32.SetColorSpace


_DLL FUNC DeleteColorSpace(hColorSpace AS PTR) AS LOGIC PASCAL:GDI32.DeleteColorSpace


_DLL FUNC GetICMProfile(hdc AS PTR, cbName AS DWORD, lpszFilename AS PSZ);
	AS LOGIC PASCAL:GDI32.GetICMProfileA




_DLL FUNC SetICMProfile( hdc AS PTR, lpFileName AS PSZ) AS LOGIC PASCAL:GDI32.SetICMProfileA



_DLL FUNC GetDeviceGammaRamp(hdc AS PTR, lpRamp AS PTR) AS LOGIC PASCAL:gdi32.GetDeviceGammaRamp


_DLL FUNC SetDeviceGammaRamp(hdc AS PTR, lpRamp AS PTR) AS LOGIC PASCAL:GDI32.SetDeviceGammaRamp



_DLL FUNC ColorMatchToTarget(hdc AS PTR, hdcTarget AS PTR, uiAction AS DWORD);
	AS LOGIC PASCAL:GDI32.ColorMatchToTarget




_DLL FUNC EnumICMProfiles( hdc AS PTR, lpICMEnumFunc AS PTR, lParam AS PTR);
	AS INT PASCAL:GDI32.EnumICMProfilesA




_DLL FUNC SwapBuffers(hdc AS PTR) AS LOGIC PASCAL:GDI32.SwapBuffers


_DLL FUNC DeviceCapabilities(lpszDevice AS PSZ, lpszPort AS PSZ, wCapability AS WORD,;
	lpszOutput AS PTR, pDevMode AS PTR);
	AS DWORD PASCAL:WINSPOOL.DRV.DeviceCapabilitiesA
FUNCTION PALETTEINDEX(i) AS DWORD
	LOCAL val1 AS DWORD
	val1 := DWORD(WORD(i))
	//PP-030924 correct 51422
	RETURN (_Or(0X01000000, LONGINT(_CAST,val1)))




#region defines
DEFINE R2_BLACK 	   := 1                /*  0       */   
DEFINE R2_NOTMERGEPEN	   := 2          /* DPon     */   
DEFINE R2_MASKNOTPEN	   := 3             /* DPna     */   
DEFINE R2_NOTCOPYPEN	   := 4             /* PN       */   
DEFINE R2_MASKPENNOT	   := 5             /* PDna     */   
DEFINE R2_NOT		   := 6                /* Dn       */   
DEFINE R2_XORPEN	   := 7                /* DPx      */   
DEFINE R2_NOTMASKPEN	   := 8             /* DPan     */   
DEFINE R2_MASKPEN	   := 9                /* DPa      */   
DEFINE R2_NOTXORPEN 	   := 10            /* DPxn     */   
DEFINE R2_NOP		   := 11               /* D        */   
DEFINE R2_MERGENOTPEN	   := 12         /* DPno     */   
DEFINE R2_COPYPEN	   := 13               /* P        */   
DEFINE R2_MERGEPENNOT	   := 14         /* PDno     */   
DEFINE R2_MERGEPEN	   := 15            /* DPo      */   
DEFINE R2_WHITE 	   := 16               /*  1       */   
DEFINE R2_LAST		   := 16
/* Ternary raster operations */
DEFINE SRCCOPY		   :=  0x00CC0020U      /* dest = source                   */  
DEFINE SRCPAINT 	   :=  0x00EE0086U      /* dest = source OR dest           */  
DEFINE SRCAND		   :=  0x008800C6U      /* dest = source AND dest          */  
DEFINE SRCINVERT	   :=  0x00660046U      /* dest = source XOR dest          */  
DEFINE SRCERASE 	   :=  0x00440328U      /* dest = source AND (NOT dest )   */  
DEFINE NOTSRCCOPY	   :=  0x00330008U      /* dest = (NOT source)             */  
DEFINE NOTSRCERASE	:=  0x001100A6U      /* dest = (NOT src) AND (NOT dest) */  
DEFINE MERGECOPY	   :=  0x00C000CAU      /* dest = (source AND pattern)     */  
DEFINE MERGEPAINT	   :=  0x00BB0226U      /* dest = (NOT source) OR dest     */  
DEFINE PATCOPY		   :=  0x00F00021U      /* dest = pattern                  */  
DEFINE PATPAINT 	   :=  0x00FB0A09U      /* dest = DPSnoo                   */  
DEFINE PATINVERT	   :=  0x005A0049U      /* dest = pattern XOR dest         */  
DEFINE DSTINVERT	   :=  0x00550009U      /* dest = (NOT dest)               */  
DEFINE BLACKNESS	   :=  0x00000042U      /* dest = BLACK                    */  
DEFINE WHITENESS	   :=  0x00FF0062U      /* dest = WHITE                    */  
DEFINE NOMIRRORBITMAP  := 0x80000000U /* Do not Mirror the bitmap in this call */
DEFINE CAPTUREBLT      := 0x40000000U /* Include layered windows */
DEFINE GDI_ERROR   := 0xFFFFFFFFL
DEFINE HGDI_ERROR := DWORD(_CAST,0xFFFFFFFFL)
/* Region Flags */
DEFINE NULLREGION			   := 1
DEFINE SIMPLEREGION 		   := 2
DEFINE COMPLEXREGION		   := 3
DEFINE RGN_ERROR				   := 0
/* CombineRgn() Styles */
DEFINE RGN_AND					   := 1
DEFINE RGN_OR					   := 2
DEFINE RGN_XOR					   := 3
DEFINE RGN_DIFF 				   := 4
DEFINE RGN_COPY 				   := 5
DEFINE RGN_MIN					   := RGN_AND
DEFINE RGN_MAX					   := RGN_COPY
/* StretchBlt() Modes */
DEFINE BLACKONWHITE 							  := 1
DEFINE WHITEONBLACK 							  := 2
DEFINE COLORONCOLOR 							  := 3
DEFINE HALFTONE 									  := 4
DEFINE MAXSTRETCHBLTMODE					  := 4
/* New StretchBlt() Modes */
DEFINE STRETCH_ANDSCANS    := BLACKONWHITE
DEFINE STRETCH_ORSCANS				   := WHITEONBLACK
DEFINE STRETCH_DELETESCANS := COLORONCOLOR
DEFINE STRETCH_HALFTONE 		  := HALFTONE
/* PolyFill() Modes */
DEFINE ALTERNATE									  := 1
DEFINE WINDING										  := 2
DEFINE POLYFILL_LAST							  := 2
/* Layout Orientation Options */
DEFINE LAYOUT_RTL                         := 0x00000001 // Right to left
DEFINE LAYOUT_BTT                         := 0x00000002 // Bottom to top
DEFINE LAYOUT_VBH                         := 0x00000004 // Vertical before horizontal
DEFINE LAYOUT_ORIENTATIONMASK             := (LAYOUT_RTL | LAYOUT_BTT | LAYOUT_VBH)
DEFINE LAYOUT_BITMAPORIENTATIONPRESERVED  := 0x00000008
/* Text Alignment Options */
DEFINE TA_NOUPDATECP							  := 0
DEFINE TA_UPDATECP								  := 1
DEFINE TA_LEFT										  := 0
DEFINE TA_RIGHT 									  := 2
DEFINE TA_CENTER									  := 6
DEFINE TA_TOP										  := 0
DEFINE TA_BOTTOM									  := 8
DEFINE TA_BASELINE								  := 24
DEFINE TA_RTLREADING							  := 256
DEFINE TA_MASK       := (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)
DEFINE VTA_BASELINE    := TA_BASELINE
DEFINE VTA_LEFT 		  := TA_BOTTOM
DEFINE VTA_RIGHT		  := TA_TOP
DEFINE VTA_CENTER	  := TA_CENTER
DEFINE VTA_BOTTOM	  := TA_RIGHT
DEFINE VTA_TOP			  := TA_LEFT
DEFINE ETO_OPAQUE								  := 0x0002
DEFINE ETO_CLIPPED								  := 0x0004
DEFINE ETO_GLYPH_INDEX					   :=  0x0010
DEFINE ETO_RTLREADING					   :=  0x0080
DEFINE ETO_NUMERICSLOCAL            := 0x0400
DEFINE ETO_NUMERICSLATIN            := 0x0800
DEFINE ETO_IGNORELANGUAGE           := 0x1000
DEFINE ETO_PDY                      := 0x2000
DEFINE ASPECT_FILTERING 				   :=  0x0001
/* Bounds Accumulation APIs */
DEFINE DCB_RESET		   := 0x0001
DEFINE DCB_ACCUMULATE  := 0x0002
DEFINE DCB_DIRTY		   := DCB_ACCUMULATE
DEFINE DCB_SET			   := 0X0003
DEFINE DCB_ENABLE	   := 0x0004
DEFINE DCB_DISABLE	   := 0x0008
/* Metafile Functions */
DEFINE META_SETBKCOLOR						  := 0x0201
DEFINE META_SETBKMODE						  := 0x0102
DEFINE META_SETMAPMODE						  := 0x0103
DEFINE META_SETROP2 							  := 0x0104
DEFINE META_SETRELABS						  := 0x0105
DEFINE META_SETPOLYFILLMODE 			  := 0x0106
DEFINE META_SETSTRETCHBLTMODE		  := 0x0107
DEFINE META_SETTEXTCHAREXTRA			  := 0x0108
DEFINE META_SETTEXTCOLOR					  := 0x0209
DEFINE META_SETTEXTJUSTIFICATION	  := 0x020A
DEFINE META_SETWINDOWORG					  := 0x020B
DEFINE META_SETWINDOWEXT					  := 0x020C
DEFINE META_SETVIEWPORTORG				  := 0x020D
DEFINE META_SETVIEWPORTEXT				  := 0x020E
DEFINE META_OFFSETWINDOWORG 			  := 0x020F
DEFINE META_SCALEWINDOWEXT				  := 0x0410
DEFINE META_OFFSETVIEWPORTORG		  := 0x0211
DEFINE META_SCALEVIEWPORTEXT			  := 0x0412
DEFINE META_LINETO								  := 0x0213
DEFINE META_MOVETO								  := 0x0214
DEFINE META_EXCLUDECLIPRECT 			  := 0x0415
DEFINE META_INTERSECTCLIPRECT		  := 0x0416
DEFINE META_ARC 									  := 0x0817
DEFINE META_ELLIPSE 							  := 0x0418
DEFINE META_FLOODFILL						  := 0x0419
DEFINE META_PIE 									  := 0x081A
DEFINE META_RECTANGLE						  := 0x041B
DEFINE META_ROUNDRECT						  := 0x061C
DEFINE META_PATBLT								  := 0x061D
DEFINE META_SAVEDC								  := 0x001E
DEFINE META_SETPIXEL							  := 0x041F
DEFINE META_OFFSETCLIPRGN				  := 0x0220
DEFINE META_TEXTOUT 							  := 0x0521
DEFINE META_BITBLT								  := 0x0922
DEFINE META_STRETCHBLT						  := 0x0B23
DEFINE META_POLYGON 							  := 0x0324
DEFINE META_POLYLINE							  := 0x0325
DEFINE META_ESCAPE								  := 0x0626
DEFINE META_RESTOREDC						  := 0x0127
DEFINE META_FILLREGION						  := 0x0228
DEFINE META_FRAMEREGION 					  := 0x0429
DEFINE META_INVERTREGION					  := 0x012A
DEFINE META_PAINTREGION 					  := 0x012B
DEFINE META_SELECTCLIPREGION			  := 0x012C
DEFINE META_SELECTOBJECT					  := 0x012D
DEFINE META_SETTEXTALIGN					  := 0x012E
DEFINE META_CHORD								  := 0x0830
DEFINE META_SETMAPPERFLAGS				  := 0x0231
DEFINE META_EXTTEXTOUT						  := 0x0a32
DEFINE META_SETDIBTODEV 					  := 0x0d33
DEFINE META_SELECTPALETTE				  := 0x0234
DEFINE META_REALIZEPALETTE				  := 0x0035
DEFINE META_ANIMATEPALETTE				  := 0x0436
DEFINE META_SETPALENTRIES				  := 0x0037
DEFINE META_POLYPOLYGON 					  := 0x0538
DEFINE META_RESIZEPALETTE				  := 0x0139
DEFINE META_DIBBITBLT						  := 0x0940
DEFINE META_DIBSTRETCHBLT				  := 0x0b41
DEFINE META_DIBCREATEPATTERNBRUSH  := 0x0142
DEFINE META_STRETCHDIB						  := 0x0f43
DEFINE META_EXTFLOODFILL					  := 0x0548
DEFINE META_SETLAYOUT               := 0x0149
DEFINE META_DELETEOBJECT					  := 0x01f0
DEFINE META_CREATEPALETTE				  := 0x00f7
DEFINE META_CREATEPATTERNBRUSH		  := 0x01F9
DEFINE META_CREATEPENINDIRECT		  := 0x02FA
DEFINE META_CREATEFONTINDIRECT		  := 0x02FB
DEFINE META_CREATEBRUSHINDIRECT 	  := 0x02FC
DEFINE META_CREATEREGION					  := 0x06FF
/* GDI Escapes */
DEFINE NEWFRAME 									  := 1
DEFINE ABORTDOC_									  := 2
DEFINE NEXTBAND 									  := 3
DEFINE SETCOLORTABLE							  := 4
DEFINE GETCOLORTABLE							  := 5
DEFINE FLUSHOUTPUT								  := 6
DEFINE DRAFTMODE									  := 7
DEFINE QUERYESCSUPPORT						  := 8
DEFINE SETABORTPROC_							  := 9
DEFINE STARTDOC_									  := 10
DEFINE ENDDOC_											  := 11
DEFINE GETPHYSPAGESIZE						  := 12
DEFINE GETPRINTINGOFFSET					  := 13
DEFINE GETSCALINGFACTOR 					  := 14
DEFINE MFCOMMENT									  := 15
DEFINE GETPENWIDTH								  := 16
DEFINE SETCOPYCOUNT 							  := 17
DEFINE SELECTPAPERSOURCE					  := 18
DEFINE DEVICEDATA								  := 19
DEFINE PASSTHROUGH								  := 19
DEFINE GETTECHNOLGY 							  := 20
DEFINE GETTECHNOLOGY							  := 20
DEFINE SETLINECAP								  := 21
DEFINE SETLINEJOIN								  := 22
DEFINE bSETMITERLIMIT						   := 23
DEFINE BANDINFO 									  := 24
DEFINE DRAWPATTERNRECT						  := 25
DEFINE GETVECTORPENSIZE 					  := 26
DEFINE GETVECTORBRUSHSIZE				  := 27
DEFINE ENABLEDUPLEX 							  := 28
DEFINE GETSETPAPERBINS						  := 29
DEFINE GETSETPRINTORIENT					  := 30
DEFINE ENUMPAPERBINS							  := 31
DEFINE SETDIBSCALING							  := 32
DEFINE EPSPRINTING								  := 33
DEFINE ENUMPAPERMETRICS 					  := 34
DEFINE GETSETPAPERMETRICS				  := 35
DEFINE POSTSCRIPT_DATA						  := 37
DEFINE POSTSCRIPT_IGNORE					  := 38
DEFINE MOUSETRAILS								  := 39
DEFINE GETDEVICEUNITS						  := 42
DEFINE GETEXTENDEDTEXTMETRICS		  := 256
DEFINE GETEXTENTTABLE						  := 257
DEFINE GETPAIRKERNTABLE 					  := 258
DEFINE GETTRACKKERNTABLE					  := 259
DEFINE bEXTTEXTOUT								   := 512
DEFINE GETFACENAME								  := 513
DEFINE DOWNLOADFACE 							  := 514
DEFINE ENABLERELATIVEWIDTHS 			  := 768
DEFINE ENABLEPAIRKERNING					  := 769
DEFINE SETKERNTRACK 							  := 770
DEFINE SETALLJUSTVALUES 					  := 771
DEFINE SETCHARSET								  := 772
DEFINE _STRETCHBLT								  := 2048
DEFINE METAFILE_DRIVER              := 2049
DEFINE GETSETSCREENPARAMS				  := 3072
DEFINE QUERYDIBSUPPORT						  := 3073
DEFINE BEGIN_PATH								  := 4096
DEFINE CLIP_TO_PATH 							  := 4097
DEFINE END_PATH 									  := 4098
DEFINE EXT_DEVICE_CAPS						  := 4099
DEFINE RESTORE_CTM								  := 4100
DEFINE SAVE_CTM 									  := 4101
DEFINE SET_ARC_DIRECTION					  := 4102
DEFINE SET_BACKGROUND_COLOR 			  := 4103
DEFINE SET_POLY_MODE							  := 4104
DEFINE SET_SCREEN_ANGLE 					  := 4105
DEFINE SET_SPREAD								  := 4106
DEFINE TRANSFORM_CTM							  := 4107
DEFINE SET_CLIP_BOX 							  := 4108
DEFINE SET_BOUNDS								  := 4109
DEFINE SET_MIRROR_MODE						  := 4110
DEFINE OPENCHANNEL								  := 4110
DEFINE DOWNLOADHEADER						  := 4111
DEFINE CLOSECHANNEL 							  := 4112
DEFINE POSTSCRIPT_PASSTHROUGH		  := 4115
DEFINE ENCAPSULATED_POSTSCRIPT		  := 4116
DEFINE POSTSCRIPT_IDENTIFY          := 4117   /* new escape for NT5 pscript driver */
DEFINE POSTSCRIPT_INJECTION         := 4118   /* new escape for NT5 pscript driver */
DEFINE CHECKJPEGFORMAT              := 4119
DEFINE CHECKPNGFORMAT               := 4120
DEFINE GET_PS_FEATURESETTING        := 4121   /* new escape for NT5 pscript driver */
DEFINE SPCLPASSTHROUGH2             := 4568   /* new escape for NT5 pscript driver */
/*
 * Parameters for POSTSCRIPT_IDENTIFY escape
 */
DEFINE PSIDENT_GDICENTRIC    := 0
DEFINE PSIDENT_PSCENTRIC     := 1
/*
 * Header structure for the input buffer to POSTSCRIPT_INJECTION escape
 */
/* Value returned for FEATURESETTING_PROTOCOL */
DEFINE PSPROTOCOL_ASCII             := 0
DEFINE PSPROTOCOL_BCP               := 1
DEFINE PSPROTOCOL_TBCP              := 2
DEFINE PSPROTOCOL_BINARY            := 3
/* Flag returned from QUERYDIBSUPPORT */
DEFINE QDI_SETDIBITS							  := 1
DEFINE QDI_GETDIBITS							  := 2
DEFINE QDI_DIBTOSCREEN						  := 4
DEFINE QDI_STRETCHDIB						  := 8
/* Spooler Error Codes */
DEFINE SP_NOTREPORTED						  := 0x4000
DEFINE SP_ERROR 									  := (-1)
DEFINE SP_APPABORT								  := (-2)
DEFINE SP_USERABORT 							  := (-3)
DEFINE SP_OUTOFDISK 							  := (-4)
DEFINE SP_OUTOFMEMORY						  := (-5)
DEFINE PR_JOBSTATUS 							  := 0x0000
/* Object Definitions for EnumObjects() */
DEFINE OBJ_PEN					   := 1
DEFINE OBJ_BRUSH				   := 2
DEFINE OBJ_DC					   := 3
DEFINE OBJ_METADC			   := 4
DEFINE OBJ_PAL					   := 5
DEFINE OBJ_FONT 				   := 6
DEFINE OBJ_BITMAP			   := 7
DEFINE OBJ_REGION			   := 8
DEFINE OBJ_METAFILE 		   := 9
DEFINE OBJ_MEMDC				   := 10
DEFINE OBJ_EXTPEN			   := 11
DEFINE OBJ_ENHMETADC		   := 12
DEFINE OBJ_ENHMETAFILE	   := 13
/* xform stuff */
DEFINE MWT_IDENTITY 		   := 1
DEFINE MWT_LEFTMULTIPLY    := 2
DEFINE MWT_RIGHTMULTIPLY   := 3
DEFINE MWT_MIN					   := MWT_IDENTITY
DEFINE MWT_MAX					   := MWT_RIGHTMULTIPLY
DEFINE LCS_CALIBRATED_RGB  := 0x00000000L
DEFINE LCS_DEVICE_RGB	  := 0x00000001L
DEFINE LCS_DEVICE_CMYK		  := 0x00000002L
DEFINE LCS_GM_BUSINESS		  := 0x00000001L
DEFINE LCS_GM_GRAPHICS		  := 0x00000002L
DEFINE LCS_GM_IMAGES		  := 0x00000004L
DEFINE CM_OUT_OF_GAMUT					  := 255
DEFINE CM_IN_GAMUT		  := 0
DEFINE BI_RGB		   := 0L
DEFINE BI_RLE8		   := 1L
DEFINE BI_RLE4		   := 2L
DEFINE BI_BITFIELDS  := 3L
DEFINE BI_JPEG       := 4L
DEFINE BI_PNG        := 5L
DEFINE TCI_SRCCHARSET := 1
DEFINE TCI_SRCCODEPAGE := 2
DEFINE TCI_SRCFONTSIG := 3
DEFINE TMPF_FIXED_PITCH 	  := 0x01
DEFINE TMPF_VECTOR				  := 0x02
DEFINE TMPF_DEVICE				  := 0x08
DEFINE TMPF_TRUETYPE			  := 0x04
DEFINE NTM_REGULAR	   := 0x00000040L
DEFINE NTM_BOLD 		   := 0x00000020L
DEFINE NTM_ITALIC	   := 0x00000001L
/* new in NT 5.0 */
DEFINE NTM_NONNEGATIVE_AC  := 0x00010000
DEFINE NTM_PS_OPENTYPE     := 0x00020000
DEFINE NTM_TT_OPENTYPE     := 0x00040000
DEFINE NTM_MULTIPLEMASTER  := 0x00080000
DEFINE NTM_TYPE1           := 0x00100000
DEFINE NTM_DSIG            := 0x00200000
DEFINE LF_FACESIZE			   := 32
DEFINE LF_FULLFACESIZE	   := 64
/* Structure passed to FONTENUMPROC */
DEFINE OUT_DEFAULT_PRECIS	   :=  0
DEFINE OUT_STRING_PRECIS		   :=  1
DEFINE OUT_CHARACTER_PRECIS    :=  2
DEFINE OUT_STROKE_PRECIS		   :=  3
DEFINE OUT_TT_PRECIS				   :=  4
DEFINE OUT_DEVICE_PRECIS		   :=  5
DEFINE OUT_RASTER_PRECIS		   :=  6
DEFINE OUT_TT_ONLY_PRECIS	   :=  7
DEFINE OUT_OUTLINE_PRECIS	   :=  8
DEFINE OUT_SCREEN_OUTLINE_PRECIS   := 9
DEFINE OUT_PS_ONLY_PRECIS          := 10
DEFINE CLIP_DEFAULT_PRECIS	   :=  0
DEFINE CLIP_CHARACTER_PRECIS   :=  1
DEFINE CLIP_STROKE_PRECIS	   :=  2
DEFINE CLIP_MASK						   :=  0xf
DEFINE CLIP_LH_ANGLES			   :=(1<<4)
DEFINE CLIP_TT_ALWAYS			   :=(2<<4)
DEFINE CLIP_DFA_DISABLE        	:= (4<<4)
DEFINE CLIP_EMBEDDED				   :=(8<<4)
DEFINE DEFAULT_QUALITY			   :=  0
DEFINE DRAFT_QUALITY				   :=  1
DEFINE PROOF_QUALITY				   :=  2
DEFINE NONANTIALIASED_QUALITY  :=  3
DEFINE ANTIALIASED_QUALITY	   :=  4
DEFINE CLEARTYPE_QUALITY       := 5
DEFINE CLEARTYPE_NATURAL_QUALITY       := 6
DEFINE DEFAULT_PITCH				   :=  0
DEFINE FIXED_PITCH					   :=  1
DEFINE VARIABLE_PITCH			   :=  2
DEFINE MONO_FONT						   :=  8
DEFINE ANSI_CHARSET 				   :=  0
DEFINE DEFAULT_CHARSET			   :=  1
DEFINE SYMBOL_CHARSET			   :=  2
DEFINE SHIFTJIS_CHARSET 		   :=  128
DEFINE HANGEUL_CHARSET			   :=  129
DEFINE HANGUL_CHARSET          := 129
DEFINE GB2312_CHARSET			   :=  134
DEFINE CHINESEBIG5_CHARSET	   :=  136
DEFINE OEM_CHARSET					   :=  255
DEFINE JOHAB_CHARSET				   :=  130
DEFINE HEBREW_CHARSET			   :=  177
DEFINE ARABIC_CHARSET			   :=  178
DEFINE GREEK_CHARSET				   :=  161
DEFINE TURKISH_CHARSET			   :=  162
DEFINE VIETNAMESE_CHARSET      := 163
DEFINE THAI_CHARSET 				   :=  222
DEFINE EASTEUROPE_CHARSET	   :=  238
DEFINE RUSSIAN_CHARSET			   :=  204
DEFINE MAC_CHARSET					   :=  77
DEFINE BALTIC_CHARSET			   :=  186
DEFINE FS_LATIN1			   :=	  0x00000001L
DEFINE FS_LATIN2			   :=	  0x00000002L
DEFINE FS_CYRILLIC			   :=	  0x00000004L
DEFINE FS_GREEK 	:=	  0x00000008L
DEFINE FS_TURKISH	:=	  0x00000010L
DEFINE FS_HEBREW	:=	  0x00000020L
DEFINE FS_ARABIC	:=	  0x00000040L
DEFINE FS_BALTIC	:=	  0x00000080L
DEFINE FS_VIETNAMESE    :=     0x00000100L
DEFINE FS_THAI		:=	  0x00010000L
DEFINE FS_JISJAPAN	:=	  0x00020000L
DEFINE FS_CHINESESIMP	:=	  0x00040000L
DEFINE FS_WANSUNG	:=	  0x00080000L
DEFINE FS_CHINESETRAD	:=	  0x00100000L
DEFINE FS_JOHAB 	:=	  0x00200000L
DEFINE FS_SYMBOL	:=	  0x80000000L
/* Font Families */
DEFINE FF_DONTCARE	:= (0<<4)
DEFINE FF_ROMAN 	:= (1<<4)
DEFINE FF_SWISS 	:= (2<<4)
DEFINE FF_MODERN	:= (3<<4)
DEFINE FF_SCRIPT	:= (4<<4)
DEFINE FF_DECORATIVE	:= (5<<4)
/* Font Weights */
DEFINE FW_DONTCARE			   :=  0
DEFINE FW_THIN					   :=  100
DEFINE FW_EXTRALIGHT		   :=  200
DEFINE FW_LIGHT 				   :=  300
DEFINE FW_NORMAL				   :=  400
DEFINE FW_MEDIUM				   :=  500
DEFINE FW_SEMIBOLD			   :=  600
DEFINE FW_BOLD					   :=  700
DEFINE FW_EXTRABOLD 		   :=  800
DEFINE FW_HEAVY 				   :=  900
DEFINE FW_ULTRALIGHT		   :=  FW_EXTRALIGHT
DEFINE FW_REGULAR			   :=  FW_NORMAL
DEFINE FW_DEMIBOLD			   :=  FW_SEMIBOLD
DEFINE FW_ULTRABOLD 		   :=  FW_EXTRABOLD
DEFINE FW_BLACK 				   :=  FW_HEAVY
DEFINE PANOSE_COUNT 						  :=  10
DEFINE PAN_FAMILYTYPE_INDEX 		  :=   0
DEFINE PAN_SERIFSTYLE_INDEX 		  :=   1
DEFINE PAN_WEIGHT_INDEX 				  :=   2
DEFINE PAN_PROPORTION_INDEX 		  :=   3
DEFINE PAN_CONTRAST_INDEX			  :=   4
DEFINE PAN_STROKEVARIATION_INDEX   :=  5
DEFINE PAN_ARMSTYLE_INDEX			  :=   6
DEFINE PAN_LETTERFORM_INDEX 		  :=   7
DEFINE PAN_MIDLINE_INDEX				  :=   8
DEFINE PAN_XHEIGHT_INDEX				  :=   9
DEFINE PAN_CULTURE_LATIN				  :=   0
DEFINE PAN_ANY											   := 0
DEFINE PAN_NO_FIT									   := 1
DEFINE PAN_FAMILY_TEXT_DISPLAY			   := 2
DEFINE PAN_FAMILY_SCRIPT						   := 3
DEFINE PAN_FAMILY_DECORATIVE				   := 4
DEFINE PAN_FAMILY_PICTORIAL 				   := 5
DEFINE PAN_SERIF_COVE							   := 2
DEFINE PAN_SERIF_OBTUSE_COVE				   := 3
DEFINE PAN_SERIF_SQUARE_COVE				   := 4
DEFINE PAN_SERIF_OBTUSE_SQUARE_COVE    := 5
DEFINE PAN_SERIF_SQUARE 						   := 6
DEFINE PAN_SERIF_THIN							   := 7
DEFINE PAN_SERIF_BONE							   := 8
DEFINE PAN_SERIF_EXAGGERATED				   := 9
DEFINE PAN_SERIF_TRIANGLE					   := 10
DEFINE PAN_SERIF_NORMAL_SANS				   := 11
DEFINE PAN_SERIF_OBTUSE_SANS				   := 12
DEFINE PAN_SERIF_PERP_SANS					   := 13
DEFINE PAN_SERIF_FLARED 						   := 14
DEFINE PAN_SERIF_ROUNDED						   := 15
DEFINE PAN_WEIGHT_VERY_LIGHT				   := 2
DEFINE PAN_WEIGHT_LIGHT 						   := 3
DEFINE PAN_WEIGHT_THIN							   := 4
DEFINE PAN_WEIGHT_BOOK							   := 5
DEFINE PAN_WEIGHT_MEDIUM						   := 6
DEFINE PAN_WEIGHT_DEMI							   := 7
DEFINE PAN_WEIGHT_BOLD							   := 8
DEFINE PAN_WEIGHT_HEAVY 						   := 9
DEFINE PAN_WEIGHT_BLACK 						   := 10
DEFINE PAN_WEIGHT_NORD							   := 11
DEFINE PAN_PROP_OLD_STYLE					   := 2
DEFINE PAN_PROP_MODERN							   := 3
DEFINE PAN_PROP_EVEN_WIDTH					   := 4
DEFINE PAN_PROP_EXPANDED						   := 5
DEFINE PAN_PROP_CONDENSED					   := 6
DEFINE PAN_PROP_VERY_EXPANDED			   := 7
DEFINE PAN_PROP_VERY_CONDENSED			   := 8
DEFINE PAN_PROP_MONOSPACED					   := 9
DEFINE PAN_CONTRAST_NONE						   := 2
DEFINE PAN_CONTRAST_VERY_LOW				   := 3
DEFINE PAN_CONTRAST_LOW 						   := 4
DEFINE PAN_CONTRAST_MEDIUM_LOW			   := 5
DEFINE PAN_CONTRAST_MEDIUM					   := 6
DEFINE PAN_CONTRAST_MEDIUM_HIGH 		   := 7
DEFINE PAN_CONTRAST_HIGH						   := 8
DEFINE PAN_CONTRAST_VERY_HIGH			   := 9
DEFINE PAN_STROKE_GRADUAL_DIAG			   := 2
DEFINE PAN_STROKE_GRADUAL_TRAN			   := 3
DEFINE PAN_STROKE_GRADUAL_VERT			   := 4
DEFINE PAN_STROKE_GRADUAL_HORZ			   := 5
DEFINE PAN_STROKE_RAPID_VERT				   := 6
DEFINE PAN_STROKE_RAPID_HORZ				   := 7
DEFINE PAN_STROKE_INSTANT_VERT			   := 8
DEFINE PAN_STRAIGHT_ARMS_HORZ			   := 2
DEFINE PAN_STRAIGHT_ARMS_WEDGE			   := 3
DEFINE PAN_STRAIGHT_ARMS_VERT			   := 4
DEFINE PAN_STRAIGHT_ARMS_SINGLE_SERIF  := 5
DEFINE PAN_STRAIGHT_ARMS_DOUBLE_SERIF  := 6
DEFINE PAN_BENT_ARMS_HORZ					   := 7
DEFINE PAN_BENT_ARMS_WEDGE					   := 8
DEFINE PAN_BENT_ARMS_VERT					   := 9
DEFINE PAN_BENT_ARMS_SINGLE_SERIF	   := 10
DEFINE PAN_BENT_ARMS_DOUBLE_SERIF	   := 11
DEFINE PAN_LETT_NORMAL_CONTACT			   := 2
DEFINE PAN_LETT_NORMAL_WEIGHTED 		   := 3
DEFINE PAN_LETT_NORMAL_BOXED				   := 4
DEFINE PAN_LETT_NORMAL_FLATTENED		   := 5
DEFINE PAN_LETT_NORMAL_ROUNDED			   := 6
DEFINE PAN_LETT_NORMAL_OFF_CENTER	   := 7
DEFINE PAN_LETT_NORMAL_SQUARE			   := 8
DEFINE PAN_LETT_OBLIQUE_CONTACT 		   := 9
DEFINE PAN_LETT_OBLIQUE_WEIGHTED		   := 10
DEFINE PAN_LETT_OBLIQUE_BOXED			   := 11
DEFINE PAN_LETT_OBLIQUE_FLATTENED	   := 12
DEFINE PAN_LETT_OBLIQUE_ROUNDED 		   := 13
DEFINE PAN_LETT_OBLIQUE_OFF_CENTER	   := 14
DEFINE PAN_LETT_OBLIQUE_SQUARE			   := 15
DEFINE PAN_MIDLINE_STANDARD_TRIMMED    := 2
DEFINE PAN_MIDLINE_STANDARD_POINTED    := 3
DEFINE PAN_MIDLINE_STANDARD_SERIFED    := 4
DEFINE PAN_MIDLINE_HIGH_TRIMMED 		   := 5
DEFINE PAN_MIDLINE_HIGH_POINTED 		   := 6
DEFINE PAN_MIDLINE_HIGH_SERIFED 		   := 7
DEFINE PAN_MIDLINE_CONSTANT_TRIMMED    := 8
DEFINE PAN_MIDLINE_CONSTANT_POINTED    := 9
DEFINE PAN_MIDLINE_CONSTANT_SERIFED    := 10
DEFINE PAN_MIDLINE_LOW_TRIMMED			   := 11
DEFINE PAN_MIDLINE_LOW_POINTED			   := 12
DEFINE PAN_MIDLINE_LOW_SERIFED			   := 13
DEFINE PAN_XHEIGHT_CONSTANT_SMALL	   := 2
DEFINE PAN_XHEIGHT_CONSTANT_STD 		   := 3
DEFINE PAN_XHEIGHT_CONSTANT_LARGE	   := 4
DEFINE PAN_XHEIGHT_DUCKING_SMALL		   := 5
DEFINE PAN_XHEIGHT_DUCKING_STD			   := 6
DEFINE PAN_XHEIGHT_DUCKING_LARGE		   := 7
DEFINE ELF_VENDOR_SIZE	   := 4
DEFINE ELF_VERSION			   := 0
DEFINE ELF_CULTURE_LATIN   := 0
DEFINE RASTER_FONTTYPE	   := 0x0001
DEFINE DEVICE_FONTTYPE	   := 0x002
DEFINE TRUETYPE_FONTTYPE   := 0x004
/*
function RGB(r as byte,g as byte,b as byte) as dword
	local val1 as dword
	local val2 as dword
	local val3 as dword
	local retval as dword
	val1 := DWORD(_CAST,r)
	val2 := (DWORD(_CAST,g)) <<8
	val3 := (DWORD(_CAST,b))<<16
	retval:= _oR(_Or(val1,val2),val3)
	RETURN retval
FUNCTION PaletteRGB(r AS BYTE,g AS BYTE, b AS BYTE) AS DWORD
	LOCAL val AS DWORD
	LOCAL retval AS DWORD
	val:= RGB(r,g,b)
	//PP-030924 correct 51422
	retval:=_oR(LONG(_CAST,val),0x02000000)
	RETURN retval
*/
DEFINE PC_RESERVED	   := 0x01
DEFINE PC_EXPLICIT	   := 0x02
DEFINE PC_NOCOLLAPSE   := 0x04
DEFINE TRANSPARENT			   := 1
DEFINE OPAQUE					   := 2
DEFINE BKMODE_LAST			   := 2
/* Graphics Modes */
DEFINE GM_COMPATIBLE		   := 1
DEFINE GM_ADVANCED			   := 2
DEFINE GM_LAST					   := 2
/* PolyDraw and GetPath point types */
DEFINE PT_CLOSEFIGURE	   := 0x01
DEFINE PT_LINETO				   := 0x02
DEFINE PT_BEZIERTO			   := 0x04
DEFINE PT_MOVETO				   := 0x06
/* Mapping Modes */
DEFINE MM_TEXT					   := 1
DEFINE MM_LOMETRIC			   := 2
DEFINE MM_HIMETRIC			   := 3
DEFINE MM_LOENGLISH 		   := 4
DEFINE MM_HIENGLISH 		   := 5
DEFINE MM_TWIPS 				   := 6
DEFINE MM_ISOTROPIC 		   := 7
DEFINE MM_ANISOTROPIC	   := 8
/* Min and Max Mapping Mode values */
DEFINE MM_MIN					   := MM_TEXT
DEFINE MM_MAX					   := MM_ANISOTROPIC
DEFINE MM_MAX_FIXEDSCALE   := MM_TWIPS
/* Coordinate Modes */
DEFINE ABSOLUTE 				   := 1
DEFINE RELATIVE 				   := 2
/* Stock Logical Objects */
DEFINE WHITE_BRUSH			   := 0
DEFINE LTGRAY_BRUSH 		   := 1
DEFINE GRAY_BRUSH			   := 2
DEFINE DKGRAY_BRUSH 		   := 3
DEFINE BLACK_BRUSH			   := 4
DEFINE NULL_BRUSH			   := 5
DEFINE HOLLOW_BRUSH 		   := NULL_BRUSH
DEFINE WHITE_PEN				   := 6
DEFINE BLACK_PEN				   := 7
DEFINE NULL_PEN 				   := 8
DEFINE OEM_FIXED_FONT	   := 10
DEFINE ANSI_FIXED_FONT	   := 11
DEFINE ANSI_VAR_FONT		   := 12
DEFINE SYSTEM_FONT			   := 13
DEFINE DEVICE_DEFAULT_FONT := 14
DEFINE DEFAULT_PALETTE	   := 15
DEFINE SYSTEM_FIXED_FONT   := 16
DEFINE DEFAULT_GUI_FONT    := 17
DEFINE DC_BRUSH            := 18
DEFINE DC_PEN              := 19
DEFINE STOCK_LAST          := 19
DEFINE CLR_INVALID	   := 0xFFFFFFFF
DEFINE BS_SOLID 				   := 0
DEFINE BS_NULL					   := 1
DEFINE BS_HOLLOW				   := BS_NULL
DEFINE BS_HATCHED			   := 2
DEFINE BS_PATTERN			   := 3
DEFINE BS_INDEXED			   := 4
DEFINE BS_DIBPATTERN		   := 5
DEFINE BS_DIBPATTERNPT	   := 6
DEFINE BS_PATTERN8X8		   := 7
DEFINE BS_DIBPATTERN8X8    := 8
DEFINE BS_MONOPATTERN      := 9
/* Hatch Styles */
DEFINE HS_HORIZONTAL		   := 0
DEFINE HS_VERTICAL			   := 1
DEFINE HS_FDIAGONAL 		   := 2
DEFINE HS_BDIAGONAL 		   := 3
DEFINE HS_CROSS 				   := 4
DEFINE HS_DIAGCROSS 		   := 5
/* Pen Styles */
DEFINE PS_SOLID 				   := 0
DEFINE PS_DASH					   := 1
DEFINE PS_DOT					   := 2
DEFINE PS_DASHDOT			   := 3
DEFINE PS_DASHDOTDOT		   := 4
DEFINE PS_NULL					   := 5
DEFINE PS_INSIDEFRAME	   := 6
DEFINE PS_USERSTYLE 		   := 7
DEFINE PS_ALTERNATE 		   := 8
DEFINE PS_STYLE_MASK		   := 0x0000000F
DEFINE PS_ENDCAP_ROUND	   := 0x00000000
DEFINE PS_ENDCAP_SQUARE    := 0x00000100
DEFINE PS_ENDCAP_FLAT	   := 0x00000200
DEFINE PS_ENDCAP_MASK	   := 0x00000F00
DEFINE PS_JOIN_ROUND		   := 0x00000000
DEFINE PS_JOIN_BEVEL		   := 0x00001000
DEFINE PS_JOIN_MITER		   := 0x00002000
DEFINE PS_JOIN_MASK 		   := 0x0000F000
DEFINE PS_COSMETIC			   := 0x00000000
DEFINE PS_GEOMETRIC 		   := 0x00010000
DEFINE PS_TYPE_MASK 		   := 0x000F0000
DEFINE AD_COUNTERCLOCKWISE := 1
DEFINE AD_CLOCKWISE 		   := 2
/* Device Parameters for GetDeviceCaps() */
DEFINE DRIVERVERSION := 0
DEFINE TECHNOLOGY  := 2
DEFINE HORZSIZE 	   := 4
DEFINE VERTSIZE 	   := 6
DEFINE HORZRES		   := 8
DEFINE VERTRES		   := 10
DEFINE BITSPIXEL	   := 12
DEFINE PLANES		   := 14
DEFINE NUMBRUSHES  := 16
DEFINE NUMPENS		   := 18
DEFINE NUMMARKERS  := 20
DEFINE NUMFONTS 	   := 22
DEFINE NUMCOLORS	   := 24
DEFINE PDEVICESIZE := 26
DEFINE CURVECAPS	   := 28
DEFINE LINECAPS 	   := 30
DEFINE POLYGONALCAPS := 32
DEFINE TEXTCAPS 	   := 34
DEFINE CLIPCAPS 	   := 36
DEFINE RASTERCAPS  := 38
DEFINE ASPECTX		   := 40
DEFINE ASPECTY		   := 42
DEFINE ASPECTXY 	   := 44
DEFINE LOGPIXELSX  := 88
DEFINE LOGPIXELSY  := 90
DEFINE SIZEPALETTE := 104
DEFINE NUMRESERVED := 106
DEFINE COLORRES 	  := 108
// Printing related DeviceCaps. These replace the appropriate Escapes
DEFINE PHYSICALWIDTH   := 110
DEFINE PHYSICALHEIGHT  := 111
DEFINE PHYSICALOFFSETX := 112
DEFINE PHYSICALOFFSETY := 113
DEFINE SCALINGFACTORX  := 114
DEFINE SCALINGFACTORY  := 115
// Display driver specific
DEFINE VREFRESH 		   := 116
DEFINE DESKTOPVERTRES  := 117
DEFINE DESKTOPHORZRES  := 118
DEFINE BLTALIGNMENT    := 119
DEFINE SHADEBLENDCAPS  := 120  /* Shading and blending caps               */
DEFINE COLORMGMTCAPS   := 121  /* Color Management caps                   */
/* Device Capability Masks: */
/* Device Technologies */
DEFINE DT_PLOTTER			   := 0
DEFINE DT_RASDISPLAY		   := 1
DEFINE DT_RASPRINTER		   := 2
DEFINE DT_RASCAMERA 		   := 3
DEFINE DT_CHARSTREAM		   := 4
DEFINE DT_METAFILE			   := 5
DEFINE DT_DISPFILE			   := 6
/* Curve Capabilities */
DEFINE CC_NONE					   := 0
DEFINE CC_CIRCLES			   := 1
DEFINE CC_PIE					   := 2
DEFINE CC_CHORD 				   := 4
DEFINE CC_ELLIPSES			   := 8
DEFINE CC_WIDE					   := 16
DEFINE CC_STYLED				   := 32
DEFINE CC_WIDESTYLED		   := 64
DEFINE CC_INTERIORS 		   := 128
DEFINE CC_ROUNDRECT 		   := 256
/* Line Capabilities */
DEFINE LC_NONE					   := 0
DEFINE LC_POLYLINE			   := 2
DEFINE LC_MARKER				   := 4
DEFINE LC_POLYMARKER		   := 8
DEFINE LC_WIDE					   := 16
DEFINE LC_STYLED				   := 32
DEFINE LC_WIDESTYLED		   := 64
DEFINE LC_INTERIORS 		   := 128
/* Polygonal Capabilities */
DEFINE PC_NONE					   := 0
DEFINE PC_POLYGON			   := 1
DEFINE PC_RECTANGLE 		   := 2
DEFINE PC_WINDPOLYGON	   := 4
DEFINE PC_TRAPEZOID 		   := 4
DEFINE PC_SCANLINE			   := 8
DEFINE PC_WIDE					   := 16
DEFINE PC_STYLED				   := 32
DEFINE PC_WIDESTYLED		   := 64
DEFINE PC_INTERIORS 		   := 128
DEFINE PC_POLYPOLYGON	   := 256
DEFINE PC_PATHS 				   := 512
/* Clipping Capabilities */
DEFINE CP_NONE					   := 0
DEFINE CP_RECTANGLE 		   := 1
DEFINE CP_REGION				   := 2
/* Text Capabilities */
DEFINE TC_OP_CHARACTER	   := 0x00000001
DEFINE TC_OP_STROKE 		   := 0x00000002
DEFINE TC_CP_STROKE 		   := 0x00000004
DEFINE TC_CR_90 				   := 0x00000008
DEFINE TC_CR_ANY				   := 0x00000010
DEFINE TC_SF_X_YINDEP	   := 0x00000020
DEFINE TC_SA_DOUBLE 		   := 0x00000040
DEFINE TC_SA_INTEGER		   := 0x00000080
DEFINE TC_SA_CONTIN 		   := 0x00000100
DEFINE TC_EA_DOUBLE 		   := 0x00000200
DEFINE TC_IA_ABLE			   := 0x00000400
DEFINE TC_UA_ABLE			   := 0x00000800
DEFINE TC_SO_ABLE			   := 0x00001000
DEFINE TC_RA_ABLE			   := 0x00002000
DEFINE TC_VA_ABLE			   := 0x00004000
DEFINE TC_RESERVED			   := 0x00008000
DEFINE TC_SCROLLBLT 		   := 0x00010000
/* Raster Capabilities */
DEFINE RC_BITBLT						  := 1
DEFINE RC_BANDING			   := 2
DEFINE RC_SCALING				  := 4
DEFINE RC_BITMAP64			   := 8
DEFINE RC_GDI20_OUTPUT	   := 0x0010
DEFINE RC_GDI20_STATE	   := 0x0020
DEFINE RC_SAVEBITMAP		   := 0x0040
DEFINE RC_DI_BITMAP 		   := 0x0080
DEFINE RC_PALETTE			   := 0x0100
DEFINE RC_DIBTODEV			   := 0x0200
DEFINE RC_BIGFONT			   := 0x0400
DEFINE RC_STRETCHBLT		   := 0x0800
DEFINE RC_FLOODFILL 		   := 0x1000
DEFINE RC_STRETCHDIB		   := 0x2000
DEFINE RC_OP_DX_OUTPUT	   := 0x4000
DEFINE RC_DEVBITS			   := 0x8000
/* Shading and blending caps */
DEFINE SB_NONE             := 0x00000000
DEFINE SB_CONST_ALPHA      := 0x00000001
DEFINE SB_PIXEL_ALPHA      := 0x00000002
DEFINE SB_PREMULT_ALPHA    := 0x00000004
DEFINE SB_GRAD_RECT        := 0x00000010
DEFINE SB_GRAD_TRI         := 0x00000020
/* Color Management caps */
DEFINE CM_NONE            := 0x00000000
DEFINE CM_DEVICE_ICM      := 0x00000001
DEFINE CM_GAMMA_RAMP      := 0x00000002
DEFINE CM_CMYK_COLOR      := 0x00000004
/* DIB color table identifiers */
DEFINE DIB_RGB_COLORS	   := 0
DEFINE DIB_PAL_COLORS	   := 1
/* constants for Get/SetSystemPaletteUse() */
DEFINE SYSPAL_ERROR    := 0
DEFINE SYSPAL_STATIC   := 1
DEFINE SYSPAL_NOSTATIC := 2
DEFINE SYSPAL_NOSTATIC256 := 3
/* constants for CreateDIBitmap */
DEFINE CBM_INIT 		   := 0x04L
/* ExtFloodFill style flags */
DEFINE FLOODFILLBORDER	  := 0
DEFINE FLOODFILLSURFACE    := 1
/* size of a device name string */
DEFINE CCHDEVICENAME := 32
/* size of a form name string */
DEFINE CCHFORMNAME := 32
DEFINE DM_SPECVERSION := 0x0400
DEFINE DM_ORIENTATION	   := 0x00000001L
DEFINE DM_PAPERSIZE 		   := 0x00000002L
DEFINE DM_PAPERLENGTH	   := 0x00000004L
DEFINE DM_PAPERWIDTH		   := 0x00000008L
DEFINE DM_SCALE 				   := 0x00000010L
DEFINE DM_POSITION            :=  0x00000020L
DEFINE DM_NUP                 :=  0x00000040L
DEFINE DM_DISPLAYORIENTATION  :=  0x00000080L
DEFINE DM_COPIES				   := 0x00000100L
DEFINE DM_DEFAULTSOURCE    := 0x00000200L
DEFINE DM_PRINTQUALITY	   := 0x00000400L
DEFINE DM_COLOR 				   := 0x00000800L
DEFINE DM_DUPLEX				   := 0x00001000L
DEFINE DM_YRESOLUTION	   := 0x00002000L
DEFINE DM_TTOPTION			   := 0x00004000L
DEFINE DM_COLLATE			   := 0x00008000L
DEFINE DM_FORMNAME			   := 0x00010000L
DEFINE DM_LOGPIXELS 		   := 0x00020000L
DEFINE DM_BITSPERPEL		   := 0x00040000L
DEFINE DM_PELSWIDTH 		   := 0x00080000L
DEFINE DM_PELSHEIGHT		   := 0x00100000L
DEFINE DM_DISPLAYFLAGS	   := 0x00200000L
DEFINE DM_DISPLAYFREQUENCY := 0x00400000L
DEFINE DM_ICMMETHOD    :=  0x00800000L
DEFINE DM_ICMINTENT    :=  0x01000000L
DEFINE DM_MEDIATYPE    :=  0x02000000L
DEFINE DM_DITHERTYPE	   :=  0x04000000L
DEFINE DM_PANNINGWIDTH        :=  0x08000000L
DEFINE DM_PANNINGHEIGHT       :=  0x10000000L
DEFINE DM_DISPLAYFIXEDOUTPUT  :=  0x20000000L
DEFINE DMORIENT_PORTRAIT   := 1
DEFINE DMORIENT_LANDSCAPE  := 2
/* paper selections */
DEFINE DMPAPER_FIRST							  := DMPAPER_LETTER
DEFINE DMPAPER_LETTER						  := 1
DEFINE DMPAPER_LETTERSMALL				  := 2
DEFINE DMPAPER_TABLOID						  := 3
DEFINE DMPAPER_LEDGER						  := 4
DEFINE DMPAPER_LEGAL							  := 5
DEFINE DMPAPER_STATEMENT					  := 6
DEFINE DMPAPER_EXECUTIVE					  := 7
DEFINE DMPAPER_A3								  := 8
DEFINE DMPAPER_A4								  := 9
DEFINE DMPAPER_A4SMALL						  := 10
DEFINE DMPAPER_A5								  := 11
DEFINE DMPAPER_B4							   := 12
DEFINE DMPAPER_B5							   := 13
DEFINE DMPAPER_FOLIO						   := 14
DEFINE DMPAPER_QUARTO					   := 15
DEFINE DMPAPER_10X14						   := 16
DEFINE DMPAPER_11X17						   := 17
DEFINE DMPAPER_NOTE 						   := 18
DEFINE DMPAPER_ENV_9						   := 19
DEFINE DMPAPER_ENV_10					   := 20
DEFINE DMPAPER_ENV_11					   := 21
DEFINE DMPAPER_ENV_12					   := 22
DEFINE DMPAPER_ENV_14					   := 23
DEFINE DMPAPER_CSHEET					   := 24
DEFINE DMPAPER_DSHEET					   := 25
DEFINE DMPAPER_ESHEET					   := 26
DEFINE DMPAPER_ENV_DL					   := 27
DEFINE DMPAPER_ENV_C5					   := 28
DEFINE DMPAPER_ENV_C3					   := 29
DEFINE DMPAPER_ENV_C4					   := 30
DEFINE DMPAPER_ENV_C6					   := 31
DEFINE DMPAPER_ENV_C65					   := 32
DEFINE DMPAPER_ENV_B4					   := 33
DEFINE DMPAPER_ENV_B5						   := 34
DEFINE DMPAPER_ENV_B6					   := 35
DEFINE DMPAPER_ENV_ITALY				   := 36
DEFINE DMPAPER_ENV_MONARCH			   := 37
DEFINE DMPAPER_ENV_PERSONAL 		   := 38
DEFINE DMPAPER_FANFOLD_US			   := 39
DEFINE DMPAPER_FANFOLD_STD_GERMAN  := 40
DEFINE DMPAPER_FANFOLD_LGL_GERMAN  := 41
DEFINE DMPAPER_ISO_B4					   := 42
DEFINE DMPAPER_JAPANESE_POSTCARD   := 43
DEFINE DMPAPER_9X11 						   := 44
DEFINE DMPAPER_10X11						   := 45
DEFINE DMPAPER_15X11						   := 46
DEFINE DMPAPER_ENV_INVITE			   := 47
DEFINE DMPAPER_RESERVED_48			   := 48
DEFINE DMPAPER_RESERVED_49			   := 49
DEFINE DMPAPER_LETTER_EXTRA    :=  50
DEFINE DMPAPER_LEGAL_EXTRA	   :=  51
DEFINE DMPAPER_TABLOID_EXTRA	   :=  52
DEFINE DMPAPER_A4_EXTRA 			   :=  53
DEFINE DMPAPER_LETTER_TRANSVERSE   := 54
DEFINE DMPAPER_A4_TRANSVERSE		   := 55
DEFINE DMPAPER_LETTER_EXTRA_TRANSVERSE := 56
DEFINE DMPAPER_A_PLUS					   := 57
DEFINE DMPAPER_B_PLUS					   := 58
DEFINE DMPAPER_LETTER_PLUS			   := 59
DEFINE DMPAPER_A4_PLUS					   := 60
DEFINE DMPAPER_A5_TRANSVERSE		   := 61
DEFINE DMPAPER_B5_TRANSVERSE		   := 62
DEFINE DMPAPER_A3_EXTRA 				   := 63
DEFINE DMPAPER_A5_EXTRA 				   := 64
DEFINE DMPAPER_B5_EXTRA 				   := 65
DEFINE DMPAPER_A2							   := 66
DEFINE DMPAPER_A3_TRANSVERSE		   := 67
DEFINE DMPAPER_A3_EXTRA_TRANSVERSE := 68
DEFINE DMPAPER_DBL_JAPANESE_POSTCARD := 69 /* Japanese Double Postcard 200 x 148 mm */
DEFINE DMPAPER_A6                 := 70  /* A6 105 x 148 mm                 */
DEFINE DMPAPER_JENV_KAKU2         := 71  /* Japanese Envelope Kaku #2       */
DEFINE DMPAPER_JENV_KAKU3         := 72  /* Japanese Envelope Kaku #3       */
DEFINE DMPAPER_JENV_CHOU3         := 73  /* Japanese Envelope Chou #3       */
DEFINE DMPAPER_JENV_CHOU4         := 74  /* Japanese Envelope Chou #4       */
DEFINE DMPAPER_LETTER_ROTATED     := 75  /* Letter Rotated 11 x 8 1/2 11 in */
DEFINE DMPAPER_A3_ROTATED         := 76  /* A3 Rotated 420 x 297 mm         */
DEFINE DMPAPER_A4_ROTATED         := 77  /* A4 Rotated 297 x 210 mm         */
DEFINE DMPAPER_A5_ROTATED         := 78  /* A5 Rotated 210 x 148 mm         */
DEFINE DMPAPER_B4_JIS_ROTATED     := 79  /* B4 (JIS) Rotated 364 x 257 mm   */
DEFINE DMPAPER_B5_JIS_ROTATED     := 80  /* B5 (JIS) Rotated 257 x 182 mm   */
DEFINE DMPAPER_JAPANESE_POSTCARD_ROTATED := 81 /* Japanese Postcard Rotated 148 x 100 mm */
DEFINE DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED := 82 /* Double Japanese Postcard Rotated 148 x 200 mm */
DEFINE DMPAPER_A6_ROTATED         := 83  /* A6 Rotated 148 x 105 mm         */
DEFINE DMPAPER_JENV_KAKU2_ROTATED := 84  /* Japanese Envelope Kaku #2 Rotated */
DEFINE DMPAPER_JENV_KAKU3_ROTATED := 85  /* Japanese Envelope Kaku #3 Rotated */
DEFINE DMPAPER_JENV_CHOU3_ROTATED := 86  /* Japanese Envelope Chou #3 Rotated */
DEFINE DMPAPER_JENV_CHOU4_ROTATED := 87  /* Japanese Envelope Chou #4 Rotated */
DEFINE DMPAPER_B6_JIS             := 88  /* B6 (JIS) 128 x 182 mm           */
DEFINE DMPAPER_B6_JIS_ROTATED     := 89  /* B6 (JIS) Rotated 182 x 128 mm   */
DEFINE DMPAPER_12X11              := 90  /* 12 x 11 in                      */
DEFINE DMPAPER_JENV_YOU4          := 91  /* Japanese Envelope You #4        */
DEFINE DMPAPER_JENV_YOU4_ROTATED  := 92  /* Japanese Envelope You #4 Rotated*/
DEFINE DMPAPER_P16K               := 93  /* PRC 16K 146 x 215 mm            */
DEFINE DMPAPER_P32K               := 94  /* PRC 32K 97 x 151 mm             */
DEFINE DMPAPER_P32KBIG            := 95  /* PRC 32K(Big) 97 x 151 mm        */
DEFINE DMPAPER_PENV_1             := 96  /* PRC Envelope #1 102 x 165 mm    */
DEFINE DMPAPER_PENV_2             := 97  /* PRC Envelope #2 102 x 176 mm    */
DEFINE DMPAPER_PENV_3             := 98  /* PRC Envelope #3 125 x 176 mm    */
DEFINE DMPAPER_PENV_4             := 99  /* PRC Envelope #4 110 x 208 mm    */
DEFINE DMPAPER_PENV_5             := 100 /* PRC Envelope #5 110 x 220 mm    */
DEFINE DMPAPER_PENV_6             := 101 /* PRC Envelope #6 120 x 230 mm    */
DEFINE DMPAPER_PENV_7             := 102 /* PRC Envelope #7 160 x 230 mm    */
DEFINE DMPAPER_PENV_8             := 103 /* PRC Envelope #8 120 x 309 mm    */
DEFINE DMPAPER_PENV_9             := 104 /* PRC Envelope #9 229 x 324 mm    */
DEFINE DMPAPER_PENV_10            := 105 /* PRC Envelope #10 324 x 458 mm   */
DEFINE DMPAPER_P16K_ROTATED       := 106 /* PRC 16K Rotated                 */
DEFINE DMPAPER_P32K_ROTATED       := 107 /* PRC 32K Rotated                 */
DEFINE DMPAPER_P32KBIG_ROTATED    := 108 /* PRC 32K(Big) Rotated            */
DEFINE DMPAPER_PENV_1_ROTATED     := 109 /* PRC Envelope #1 Rotated 165 x 102 mm */
DEFINE DMPAPER_PENV_2_ROTATED     := 110 /* PRC Envelope #2 Rotated 176 x 102 mm */
DEFINE DMPAPER_PENV_3_ROTATED     := 111 /* PRC Envelope #3 Rotated 176 x 125 mm */
DEFINE DMPAPER_PENV_4_ROTATED     := 112 /* PRC Envelope #4 Rotated 208 x 110 mm */
DEFINE DMPAPER_PENV_5_ROTATED     := 113 /* PRC Envelope #5 Rotated 220 x 110 mm */
DEFINE DMPAPER_PENV_6_ROTATED     := 114 /* PRC Envelope #6 Rotated 230 x 120 mm */
DEFINE DMPAPER_PENV_7_ROTATED     := 115 /* PRC Envelope #7 Rotated 230 x 160 mm */
DEFINE DMPAPER_PENV_8_ROTATED     := 116 /* PRC Envelope #8 Rotated 309 x 120 mm */
DEFINE DMPAPER_PENV_9_ROTATED     := 117 /* PRC Envelope #9 Rotated 324 x 229 mm */
DEFINE DMPAPER_PENV_10_ROTATED    := 118 /* PRC Envelope #10 Rotated 458 x 324 mm */
DEFINE DMPAPER_LAST 						   := DMPAPER_PENV_10_ROTATED
DEFINE DMPAPER_USER 						   := 256
/* bin selections */
DEFINE DMBIN_FIRST			   := DMBIN_UPPER
DEFINE DMBIN_UPPER			   := 1
DEFINE DMBIN_ONLYONE		   := 1
DEFINE DMBIN_LOWER			   := 2
DEFINE DMBIN_MIDDLE 		   := 3
DEFINE DMBIN_MANUAL 		   := 4
DEFINE DMBIN_ENVELOPE	   := 5
DEFINE DMBIN_ENVMANUAL	   := 6
DEFINE DMBIN_AUTO			   := 7
DEFINE DMBIN_TRACTOR		   := 8
DEFINE DMBIN_SMALLFMT	   := 9
DEFINE DMBIN_LARGEFMT	   := 10
DEFINE DMBIN_LARGECAPACITY := 11
DEFINE DMBIN_CASSETTE	   := 14
DEFINE DMBIN_FORMSOURCE    := 15
DEFINE DMBIN_LAST			   := DMBIN_FORMSOURCE
DEFINE DMBIN_USER			   := 256
/* print qualities */
DEFINE DMRES_DRAFT			   := (-1)
DEFINE DMRES_LOW				   := (-2)
DEFINE DMRES_MEDIUM 		   := (-3)
DEFINE DMRES_HIGH			   := (-4)
/* color enable/disable for color printers */
DEFINE DMCOLOR_MONOCHROME  := 1
DEFINE DMCOLOR_COLOR		   := 2
/* duplex enable */
DEFINE DMDUP_SIMPLEX	  := 1
DEFINE DMDUP_VERTICAL  := 2
DEFINE DMDUP_HORIZONTAL := 3
/* TrueType options */
DEFINE DMTT_BITMAP	   := 1
DEFINE DMTT_DOWNLOAD   := 2
DEFINE DMTT_SUBDEV	   := 3
DEFINE DMTT_DOWNLOAD_OUTLINE := 4
/* Collation selections */
DEFINE DMCOLLATE_FALSE := 0
DEFINE DMCOLLATE_TRUE  := 1
/* DEVMODE dmDisplayOrientation specifiations */
DEFINE DMDO_DEFAULT   :=  0
DEFINE DMDO_90        :=  1
DEFINE DMDO_180       :=  2
DEFINE DMDO_270       :=  3
/* DEVMODE dmDisplayFixedOutput specifiations */
DEFINE DMDFO_DEFAULT   := 0
DEFINE DMDFO_STRETCH   := 1
DEFINE DMDFO_CENTER    := 2
/* DEVMODE dmDisplayFlags flags */
DEFINE DM_GRAYSCALE  := 0x00000001
DEFINE DM_INTERLACED := 0x00000002
/* ICM methods */
DEFINE DMICMMETHOD_NONE    := 1
DEFINE DMICMMETHOD_SYSTEM  := 2
DEFINE DMICMMETHOD_DRIVER  := 3
DEFINE DMICMMETHOD_DEVICE  := 4
DEFINE DMICMMETHOD_USER    := 256
/* ICM Intents */
DEFINE DMICM_SATURATE	   := 1
DEFINE DMICM_CONTRAST	   := 2
DEFINE DMICM_COLORMETRIC   := 3
DEFINE DMICM_ABS_COLORIMETRIC  :=  4   /* Use specific color metric */
DEFINE DMICM_USER			   := 256
/* Media types */
DEFINE DMMEDIA_STANDARD 	   := 1
DEFINE DMMEDIA_TRANSPARENCY  := 2
DEFINE DMMEDIA_GLOSSY		   := 3
DEFINE DMMEDIA_USER 			   := 256
/* Dither types */
DEFINE DMDITHER_NONE	  := 1
DEFINE DMDITHER_COARSE				  := 2
DEFINE DMDITHER_FINE	  := 3
DEFINE DMDITHER_LINEART 			  := 4
DEFINE DMDITHER_ERRORDIFFUSION := 5
DEFINE DMDITHER_RESERVED6 := 6
DEFINE DMDITHER_RESERVED7 := 7
DEFINE DMDITHER_RESERVED8 := 8
DEFINE DMDITHER_RESERVED9 := 9
DEFINE DMDITHER_GRAYSCALE		  := 10
DEFINE DMDITHER_USER					  := 256
DEFINE DISPLAY_DEVICE_ATTACHED_TO_DESKTOP := 0x00000001
DEFINE DISPLAY_DEVICE_MULTI_DRIVER        := 0x00000002
DEFINE DISPLAY_DEVICE_PRIMARY_DEVICE      := 0x00000004
DEFINE DISPLAY_DEVICE_MIRRORING_DRIVER    := 0x00000008
DEFINE DISPLAY_DEVICE_VGA_COMPATIBLE      := 0x00000010
DEFINE DISPLAY_DEVICE_REMOVABLE           := 0x00000020
DEFINE DISPLAY_DEVICE_MODESPRUNED         := 0x08000000
DEFINE DISPLAY_DEVICE_REMOTE              := 0x04000000  
DEFINE DISPLAY_DEVICE_DISCONNECT          := 0x02000000  
/* Child device state */
DEFINE DISPLAY_DEVICE_ACTIVE             :=  0x00000001
DEFINE DISPLAY_DEVICE_ATTACHED           :=  0x00000002
DEFINE RDH_RECTANGLES  := 1
DEFINE GGO_METRICS			  := 0
DEFINE GGO_BITMAP			  := 1
DEFINE GGO_NATIVE			  := 2
DEFINE GGO_BEZIER        :=  3
DEFINE GGO_GRAY2_BITMAP    := 4
DEFINE GGO_GRAY4_BITMAP    := 5
DEFINE GGO_GRAY8_BITMAP    := 6
DEFINE GGO_GLYPH_INDEX	   := 0x0080
DEFINE  GGO_UNHINTED      :=  0x0100
DEFINE TT_POLYGON_TYPE	  := 24
DEFINE TT_PRIM_LINE 		  := 1
DEFINE TT_PRIM_QSPLINE	  := 2
DEFINE TT_PRIM_CSPLINE   :=  3
DEFINE GCP_DBCS 				  :=  0x0001
DEFINE GCP_REORDER			  :=  0x0002
DEFINE GCP_USEKERNING	  :=  0x0008
DEFINE GCP_GLYPHSHAPE	  :=  0x0010
DEFINE GCP_LIGATE			  :=  0x0020
DEFINE GCP_DIACRITIC		  :=  0x0100
DEFINE GCP_KASHIDA	  :=  0x0400
DEFINE GCP_ERROR				  :=  0x8000
DEFINE FLI_MASK 				  :=  0x103B
DEFINE GCP_JUSTIFY			  :=  0x00010000L
DEFINE FLI_GLYPHS  :=  0x00040000L
DEFINE GCP_CLASSIN			  :=  0x00080000L
DEFINE GCP_MAXEXTENT		  :=  0x00100000L
DEFINE GCP_JUSTIFYIN	  :=  0x00200000L
DEFINE GCP_DISPLAYZWG	  :=  0x00400000L
DEFINE GCP_SYMSWAPOFF	  :=  0x00800000L
DEFINE GCP_NUMERICOVERRIDE :=  0x01000000L
DEFINE GCP_NEUTRALOVERRIDE :=  0x02000000L
DEFINE GCP_NUMERICSLATIN   :=  0x04000000L
DEFINE GCP_NUMERICSLOCAL   :=  0x08000000L
DEFINE GCPCLASS_LATIN							   :=  1
DEFINE GCPCLASS_HEBREW							   :=  2
DEFINE GCPCLASS_ARABIC							   :=  2
DEFINE GCPCLASS_NEUTRAL 						   :=  3
DEFINE GCPCLASS_LOCALNUMBER 				   :=  4
DEFINE GCPCLASS_LATINNUMBER 				   :=  5
DEFINE GCPCLASS_LATINNUMERICTERMINATOR :=  6
DEFINE GCPCLASS_LATINNUMERICSEPARATOR  :=  7
DEFINE GCPCLASS_NUMERICSEPARATOR		   :=  8
DEFINE GCPCLASS_PREBOUNDLTR 			  :=  0x80
DEFINE GCPCLASS_PREBOUNDRTL 			  :=  0x40
DEFINE GCPCLASS_POSTBOUNDLTR			  :=  0x20
DEFINE GCPCLASS_POSTBOUNDRTL			  :=  0x10
DEFINE GCPGLYPH_LINKBEFORE				  :=  0x8000
DEFINE GCPGLYPH_LINKAFTER				  :=  0x4000
DEFINE TT_AVAILABLE    := 0x0001
DEFINE TT_ENABLED	   := 0x0002
DEFINE PFD_TYPE_RGBA			  := 0
DEFINE PFD_TYPE_COLORINDEX := 1
/* layer types */
DEFINE PFD_MAIN_PLANE		  := 0
DEFINE PFD_OVERLAY_PLANE	  := 1
DEFINE PFD_UNDERLAY_PLANE  := (-1)
/* PIXELFORMATDESCRIPTOR flags */
DEFINE PFD_DOUBLEBUFFER 				   := 0x00000001
DEFINE PFD_STEREO							   := 0x00000002
DEFINE PFD_DRAW_TO_WINDOW			   := 0x00000004
DEFINE PFD_DRAW_TO_BITMAP			   := 0x00000008
DEFINE PFD_SUPPORT_GDI					   := 0x00000010
DEFINE PFD_SUPPORT_OPENGL			   := 0x00000020
DEFINE PFD_GENERIC_FORMAT			   := 0x00000040
DEFINE PFD_NEED_PALETTE 				   := 0x00000080
DEFINE PFD_NEED_SYSTEM_PALETTE	   := 0x00000100
DEFINE PFD_SWAP_EXCHANGE				   := 0x00000200
DEFINE PFD_SWAP_COPY						   := 0x00000400
DEFINE PFD_SWAP_LAYER_BUFFERS     :=  0x00000800
DEFINE PFD_GENERIC_ACCELERATED    :=  0x00001000
DEFINE PFD_SUPPORT_DIRECTDRAW     :=  0x00002000
/* PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only */
DEFINE PFD_DEPTH_DONTCARE          := 0x20000000
DEFINE PFD_DOUBLEBUFFER_DONTCARE   := 0x40000000
DEFINE PFD_STEREO_DONTCARE			   := 0x80000000
DEFINE DM_UPDATE				   := 1
DEFINE DM_COPY					   := 2
DEFINE DM_PROMPT				   := 4
DEFINE DM_MODIFY				   := 8
DEFINE DM_IN_BUFFER 		   := DM_MODIFY
DEFINE DM_IN_PROMPT 		   := DM_PROMPT
DEFINE DM_OUT_BUFFER		   := DM_COPY
DEFINE DM_OUT_DEFAULT	   := DM_UPDATE
/* device capabilities indices */
DEFINE DC_FIELDS				   := 1
DEFINE DC_PAPERS				   := 2
DEFINE DC_PAPERSIZE 		   := 3
DEFINE DC_MINEXTENT 		   := 4
DEFINE DC_MAXEXTENT 		   := 5
DEFINE DC_BINS					   := 6
DEFINE DC_DUPLEX				   := 7
DEFINE DC_SIZE					   := 8
DEFINE DC_EXTRA 				   := 9
DEFINE DC_VERSION			   := 10
DEFINE DC_DRIVER				   := 11
DEFINE DC_BINNAMES			   := 12
DEFINE DC_ENUMRESOLUTIONS  := 13
DEFINE DC_FILEDEPENDENCIES := 14
DEFINE DC_TRUETYPE			   := 15
DEFINE DC_PAPERNAMES		   := 16
DEFINE DC_ORIENTATION	   := 17
DEFINE DC_COPIES				   := 18
DEFINE DC_BINADJUST 				   := 19
DEFINE DC_EMF_COMPLIANT 		   := 20
DEFINE DC_DATATYPE_PRODUCED    := 21
DEFINE DC_COLLATE              := 22
DEFINE DC_MANUFACTURER := 23
DEFINE DC_MODEL    := 24
DEFINE DC_PERSONALITY         :=  25
DEFINE DC_PRINTRATE           :=  26
DEFINE DC_PRINTRATEUNIT       :=  27
DEFINE   PRINTRATEUNIT_PPM    :=  1
DEFINE   PRINTRATEUNIT_CPS    :=  2
DEFINE   PRINTRATEUNIT_LPM    :=  3
DEFINE   PRINTRATEUNIT_IPM    :=  4
DEFINE DC_PRINTERMEM          :=  28
DEFINE DC_MEDIAREADY          :=  29
DEFINE DC_STAPLE              :=  30
DEFINE DC_PRINTRATEPPM        :=  31
DEFINE DC_COLORDEVICE         :=  32
DEFINE DC_NUP                 :=  33
DEFINE DC_MEDIATYPENAMES      :=  34
DEFINE DC_MEDIATYPES          :=  35
DEFINE DCTT_BITMAP					   := 0x0000001L
DEFINE DCTT_DOWNLOAD				   := 0x0000002L
DEFINE DCTT_SUBDEV					   := 0x0000004L
DEFINE DCTT_DOWNLOAD_OUTLINE   := 0x0000008L
DEFINE DCBA_FACEUPNONE		   := 0x0000
DEFINE DCBA_FACEUPCENTER	   := 0x0001
DEFINE DCBA_FACEUPLEFT		   := 0x0002
DEFINE DCBA_FACEUPRIGHT 	   := 0x0003
DEFINE DCBA_FACEDOWNNONE	   := 0x0100
DEFINE DCBA_FACEDOWNCENTER := 0x0101
DEFINE DCBA_FACEDOWNLEFT	   := 0x0102
DEFINE DCBA_FACEDOWNRIGHT  := 0x0103
DEFINE CA_NEGATIVE							   := 0x0001
DEFINE CA_LOG_FILTER					   :=  0x0002
/* IlluminantIndex values */
DEFINE ILLUMINANT_DEVICE_DEFAULT   := 0
DEFINE ILLUMINANT_A 						   := 1
DEFINE ILLUMINANT_B 						   := 2
DEFINE ILLUMINANT_C 						   := 3
DEFINE ILLUMINANT_D50					   := 4
DEFINE ILLUMINANT_D55					   := 5
DEFINE ILLUMINANT_D65					   := 6
DEFINE ILLUMINANT_D75					   := 7
DEFINE ILLUMINANT_F2						   := 8
DEFINE ILLUMINANT_MAX_INDEX 		   := ILLUMINANT_F2
DEFINE ILLUMINANT_TUNGSTEN			   := ILLUMINANT_A
DEFINE ILLUMINANT_DAYLIGHT			   := ILLUMINANT_C
DEFINE ILLUMINANT_FLUORESCENT	   := ILLUMINANT_F2
DEFINE ILLUMINANT_NTSC					   := ILLUMINANT_C
/* Min and max for RedGamma, GreenGamma, BlueGamma */
DEFINE RGB_GAMMA_MIN						   := WORD(_CAST,02500)
DEFINE RGB_GAMMA_MAX						   := WORD(_CAST, 65000)
/* Min and max for ReferenceBlack and ReferenceWhite */
DEFINE REFERENCE_WHITE_MIN			   := WORD(_CAST,6000)
DEFINE REFERENCE_WHITE_MAX			   := WORD(_CAST,10000)
DEFINE REFERENCE_BLACK_MIN			   := WORD(_CAST,0)
DEFINE REFERENCE_BLACK_MAX			   := WORD(_CAST,4000)
/* Min and max for Contrast, Brightness, Colorfulness, RedGreenTint */
DEFINE COLOR_ADJ_MIN						   := -100
DEFINE COLOR_ADJ_MAX						   := SHORT(_CAST, 100)
DEFINE DI_APPBANDING   := 0x0001
DEFINE FONTMAPPER_MAX := 10
DEFINE ICM_OFF := 1
DEFINE ICM_ON  := 2
DEFINE ICM_QUERY := 3
DEFINE ICM_DONE_OUTSIDEDC    := 4
DEFINE ENHMETA_SIGNATURE		   := 0x464D4520
// Stock object flag used in the object handle index in the enhanced
// metafile records.
// E.g. The object handle index (META_STOCK_OBJECT | BLACK_BRUSH)
// represents the stock object BLACK_BRUSH.
DEFINE ENHMETA_STOCK_OBJECT    := 0x80000000
// Enhanced metafile record types.
DEFINE EMR_HEADER									   := 1
DEFINE EMR_POLYBEZIER							   := 2
DEFINE EMR_POLYGON									   := 3
DEFINE EMR_POLYLINE 								   := 4
DEFINE EMR_POLYBEZIERTO 						   := 5
DEFINE EMR_POLYLINETO							   := 6
DEFINE EMR_POLYPOLYLINE 						   := 7
DEFINE EMR_POLYPOLYGON							   := 8
DEFINE EMR_SETWINDOWEXTEX					   := 9
DEFINE EMR_SETWINDOWORGEX					   := 10
DEFINE EMR_SETVIEWPORTEXTEX 				   := 11
DEFINE EMR_SETVIEWPORTORGEX 				   := 12
DEFINE EMR_SETBRUSHORGEX						   := 13
DEFINE EMR_EOF											   := 14
DEFINE EMR_SETPIXELV								   := 15
DEFINE EMR_SETMAPPERFLAGS					   := 16
DEFINE EMR_SETMAPMODE							   := 17
DEFINE EMR_SETBKMODE								   := 18
DEFINE EMR_SETPOLYFILLMODE					   := 19
DEFINE EMR_SETROP2									   := 20
DEFINE EMR_SETSTRETCHBLTMODE				   := 21
DEFINE EMR_SETTEXTALIGN 						   := 22
DEFINE EMR_SETCOLORADJUSTMENT			   := 23
DEFINE EMR_SETTEXTCOLOR 						   := 24
DEFINE EMR_SETBKCOLOR							   := 25
DEFINE EMR_OFFSETCLIPRGN						   := 26
DEFINE EMR_MOVETOEX 								   := 27
DEFINE EMR_SETMETARGN							   := 28
DEFINE EMR_EXCLUDECLIPRECT					   := 29
DEFINE EMR_INTERSECTCLIPRECT				   := 30
DEFINE EMR_SCALEVIEWPORTEXTEX			   := 31
DEFINE EMR_SCALEWINDOWEXTEX 				   := 32
DEFINE EMR_SAVEDC									   := 33
DEFINE EMR_RESTOREDC								   := 34
DEFINE EMR_SETWORLDTRANSFORM				   := 35
DEFINE EMR_MODIFYWORLDTRANSFORM 		   := 36
DEFINE EMR_SELECTOBJECT 						   := 37
DEFINE EMR_CREATEPEN								   := 38
DEFINE EMR_CREATEBRUSHINDIRECT			   := 39
DEFINE EMR_DELETEOBJECT 						   := 40
DEFINE EMR_ANGLEARC 								   := 41
DEFINE EMR_ELLIPSE									   := 42
DEFINE EMR_RECTANGLE								   := 43
DEFINE EMR_ROUNDRECT								   := 44
DEFINE EMR_ARC											   := 45
DEFINE EMR_CHORD										   := 46
DEFINE EMR_PIE											   := 47
DEFINE EMR_SELECTPALETTE						   := 48
DEFINE EMR_CREATEPALETTE						   := 49
DEFINE EMR_SETPALETTEENTRIES				   := 50
DEFINE EMR_RESIZEPALETTE						   := 51
DEFINE EMR_REALIZEPALETTE					   := 52
DEFINE EMR_EXTFLOODFILL 						   := 53
DEFINE EMR_LINETO									   := 54
DEFINE EMR_ARCTO										   := 55
DEFINE EMR_POLYDRAW 								   := 56
DEFINE EMR_SETARCDIRECTION					   := 57
DEFINE EMR_SETMITERLIMIT						   := 58
DEFINE EMR_BEGINPATH								   := 59
DEFINE EMR_ENDPATH									   := 60
DEFINE EMR_CLOSEFIGURE							   := 61
DEFINE EMR_FILLPATH 								   := 62
DEFINE EMR_STROKEANDFILLPATH				   := 63
DEFINE EMR_STROKEPATH							   := 64
DEFINE EMR_FLATTENPATH							   := 65
DEFINE EMR_WIDENPATH								   := 66
DEFINE EMR_SELECTCLIPPATH					   := 67
DEFINE EMR_ABORTPATH								   := 68
DEFINE EMR_GDICOMMENT							   := 70
DEFINE EMR_FILLRGN									   := 71
DEFINE EMR_FRAMERGN 								   := 72
DEFINE EMR_INVERTRGN								   := 73
DEFINE EMR_PAINTRGN 								   := 74
DEFINE EMR_EXTSELECTCLIPRGN 				   := 75
DEFINE EMR_BITBLT									   := 76
DEFINE EMR_STRETCHBLT							   := 77
DEFINE EMR_MASKBLT									   := 78
DEFINE EMR_PLGBLT									   := 79
DEFINE EMR_SETDIBITSTODEVICE				   := 80
DEFINE EMR_STRETCHDIBITS						   := 81
DEFINE EMR_EXTCREATEFONTINDIRECTW	   := 82
DEFINE EMR_EXTTEXTOUTA							   := 83
DEFINE EMR_EXTTEXTOUTW							   := 84
DEFINE EMR_POLYBEZIER16 						   := 85
DEFINE EMR_POLYGON16								   := 86
DEFINE EMR_POLYLINE16							   := 87
DEFINE EMR_POLYBEZIERTO16					   := 88
DEFINE EMR_POLYLINETO16 						   := 89
DEFINE EMR_POLYPOLYLINE16					   := 90
DEFINE EMR_POLYPOLYGON16						   := 91
DEFINE EMR_POLYDRAW16							   := 92
DEFINE EMR_CREATEMONOBRUSH					   := 93
DEFINE EMR_CREATEDIBPATTERNBRUSHPT	   := 94
DEFINE EMR_EXTCREATEPEN 						   := 95
DEFINE EMR_POLYTEXTOUTA 						   := 96
DEFINE EMR_POLYTEXTOUTW 						   := 97
DEFINE EMR_SETICMMODE			  :=  98
DEFINE EMR_CREATECOLORSPACE 				  :=  99
DEFINE EMR_SETCOLORSPACE						  := 100
DEFINE EMR_DELETECOLORSPACE 				  := 101
DEFINE EMR_GLSRECORD                :=  102
DEFINE EMR_GLSBOUNDEDRECORD         :=  103
DEFINE EMR_PIXELFORMAT              :=  104
DEFINE EMR_RESERVED_105             :=  105
DEFINE EMR_RESERVED_106             :=  106
DEFINE EMR_RESERVED_107             :=  107
DEFINE EMR_RESERVED_108             :=  108
DEFINE EMR_RESERVED_109             :=  109
DEFINE EMR_RESERVED_110             :=  110
DEFINE EMR_COLORCORRECTPALETTE      :=  111
DEFINE EMR_SETICMPROFILEA           :=  112
DEFINE EMR_SETICMPROFILEW           :=  113
DEFINE EMR_ALPHABLEND               :=  114
DEFINE EMR_SETLAYOUT                :=  115
DEFINE EMR_TRANSPARENTBLT           :=  116
DEFINE EMR_RESERVED_117             :=  117
DEFINE EMR_GRADIENTFILL             :=  118
DEFINE EMR_RESERVED_119             :=  119
DEFINE EMR_RESERVED_120             :=  120
DEFINE EMR_COLORMATCHTOTARGETW      :=  121
DEFINE EMR_CREATECOLORSPACEW        :=  122
DEFINE EMR_MIN											  :=  1
DEFINE EMR_MAX											  := 122
DEFINE GDICOMMENT_IDENTIFIER				   := 0x43494447
DEFINE GDICOMMENT_WINDOWS_METAFILE	   := 0x80000001
DEFINE GDICOMMENT_BEGINGROUP				   := 0x00000002
DEFINE GDICOMMENT_ENDGROUP					   := 0x00000003
DEFINE GDICOMMENT_MULTIFORMATS			   := 0x40000004
DEFINE EPS_SIGNATURE								   := 0x46535045
DEFINE GDICOMMENT_UNICODE_STRING       := 0x00000040
DEFINE GDICOMMENT_UNICODE_END          := 0x00000080
DEFINE WGL_FONT_LINES	   := 0
DEFINE WGL_FONT_POLYGONS   := 1
#endregion
