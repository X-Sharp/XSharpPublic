//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#region functions

/// <summary>
/// </summary>
/// <param name="hWndOwner"></param>
/// <param name="cFiles"></param>
/// <param name="fAsynchWork"></param>
/// <returns>
/// </returns>
UNSAFE	 FUNCTION DoSendMail(hWndOwner AS IntPtr,cFiles AS STRING,fAsynchWork AS LOGIC) AS VOID
    /// THROW NotImplementedException{}
    RETURN 
    
    
    
    
    
/// <summary>
/// Display a bitmap in a window or control.
/// </summary>
/// <param name="hWnd"></param>
/// <param name="cFileName"></param>
/// <param name="cTitle"></param>
/// <returns>
/// </returns>
FUNCTION ShowBitmap(hWnd AS IntPtr,cFileName AS STRING,cTitle AS STRING) AS LOGIC
    RETURN VOBitmaps.Show(hWnd, cFileName, cTitle)
    
/// <summary>
/// Display a bitmap stretched or shrunk to fit a window or control.
/// </summary>
/// <param name="hWnd"></param>
/// <param name="cFileName"></param>
/// <param name="cTitle"></param>
/// <returns>
/// </returns>
FUNCTION StretchBitmap(hWnd AS IntPtr,cFileName AS STRING,cTitle AS STRING) AS LOGIC
    RETURN VOBitmaps.Stretch(hWnd, cFileName, cTitle)
    
INTERNAL _DLL FUNCTION GlobalAlloc( uFlags AS DWORD, dwBytes AS DWORD ) AS IntPtr PASCAL:KERNEL32.GlobalAlloc
INTERNAL _DLL FUNCTION GlobalFree( hMem AS IntPtr ) AS IntPtr PASCAL:KERNEL32.GlobalFree
INTERNAL _DLL FUNCTION GlobalReAlloc( hMem AS IntPtr, dwBytes AS DWORD, uFlags AS DWORD )	AS IntPtr PASCAL:KERNEL32.GlobalReAlloc
INTERNAL _DLL FUNCTION GetFocus() AS IntPtr PASCAL:USER32.GetFocus
INTERNAL _DLL FUNCTION InvalidateRect( hwnd AS IntPtr, lpRect AS RECT, bErase AS LOGIC ) AS LOGIC PASCAL:USER32.InvalidateRect
INTERNAL _DLL FUNCTION GlobalLock( hMem AS IntPtr ) AS IntPtr PASCAL:KERNEL32.GlobalLock
INTERNAL _DLL FUNCTION GlobalUnlock( hMem AS IntPtr ) AS LOGIC PASCAL:KERNEL32.GlobalUnlock
INTERNAL _DLL FUNCTION SetWindowText( hwnd AS IntPtr, lpString AS STRING ) AS LOGIC PASCAL:USER32.SetWindowTextW Unicode
INTERNAL _DLL FUNCTION GetClientRect( hwnd AS IntPtr, lpRect AS RECT ) AS LOGIC PASCAL:USER32.GetClientRect
INTERNAL _DLL FUNCTION GetWindowRect( hwnd AS IntPtr, lpRect AS RECT ) AS LOGIC PASCAL:USER32.GetWindowRect
INTERNAL _DLL FUNCTION SetWindowPos( hwnd AS IntPtr, hwndInsertAfter AS IntPtr, x AS INT, y AS INT, cx AS INT, cy AS INT, uFlags AS DWORD ) AS LOGIC PASCAL:USER32.SetWindowPos
INTERNAL _DLL FUNCTION BeginPaint( hwnd AS IntPtr, lpPaint AS PAINTSTRUCT ) AS IntPtr PASCAL:USER32.BeginPaint
INTERNAL _DLL FUNCTION EndPaint( hwnd AS IntPtr, lpPaint AS PAINTSTRUCT ) AS LOGIC PASCAL:USER32.EndPaint
INTERNAL _DLL FUNCTION SelectPalette( hdc AS IntPtr, hpal AS IntPtr, bForceBackground AS LOGIC ) AS IntPtr PASCAL:GDI32.SelectPalette
INTERNAL _DLL FUNCTION RealizePalette( hdc AS IntPtr ) AS DWORD PASCAL:GDI32.RealizePalette
INTERNAL _DLL FUNCTION CreateCompatibleDC( hDc AS IntPtr ) AS IntPtr PASCAL:GDI32.CreateCompatibleDC
INTERNAL _DLL FUNCTION DeleteDC( hdc AS IntPtr ) AS LOGIC PASCAL:GDI32.DeleteDC
INTERNAL _DLL FUNCTION DeleteObject( hObject AS IntPtr ) AS LOGIC PASCAL:GDI32.DeleteObject
INTERNAL _DLL FUNCTION CreateCompatibleBitmap( hdc AS IntPtr, nWidth AS INT, nHeight AS INT ) AS IntPtr PASCAL:GDI32.CreateCompatibleBitmap
INTERNAL _DLL FUNCTION SelectObject( hdc AS IntPtr, hgdiobj AS IntPtr ) AS IntPtr PASCAL:GDI32.SelectObject
INTERNAL _DLL FUNCTION SetDIBitsToDevice( hdc AS IntPtr, xDest AS INT, YDest AS INT, dwWidth AS DWORD, deHeight AS DWORD, XSrc AS INT, YSrc AS INT, uStartScan AS DWORD, cScanLines AS DWORD,lpvBits AS IntPtr, lpbmi AS BITMAPINFO, fuColorUse AS DWORD )	AS INT PASCAL:GDI32.SetDIBitsToDevice
INTERNAL _DLL FUNCTION StretchDIBits( hdc AS IntPtr, XDest AS INT, yDest AS INT, nDestWidth AS INT, nDestHeight AS INT, XSrc AS INT, YSrc AS INT, nSrcWidth AS INT, nSrcHeight AS INT, lpBits AS IntPtr, lpBitsInfo AS BITMAPINFO,	iUsage AS DWORD, Rop AS DWORD ) AS INT PASCAL:GDI32.StretchDIBits
INTERNAL _DLL FUNCTION CreatePalette( lplgpl AS LOGPALETTE ) AS IntPtr PASCAL:GDI32.CreatePalette
INTERNAL _DLL FUNCTION GetDeviceCaps( hdc AS IntPtr, nIndex AS INT ) AS INT PASCAL:GDI32.GetDeviceCaps
INTERNAL _DLL FUNCTION CreateHalftonePalette( hdc AS IntPtr) AS IntPtr PASCAL:GDI32.CreateHalftonePalette
INTERNAL _DLL FUNCTION GetStockObject( fnObject AS INT ) AS IntPtr PASCAL:GDI32.GetStockObject

#define SETDIB        100
#define STRETCHDIB    200
#define GMEM_MOVEABLE 0x0002
#define SWP_NOMOVE    0x0002
#define SWP_NOZORDER  0x0004
#define DIB_PAL_COLORS 1 
#define DIB_RGB_COLORS 0 
#define SRCCOPY (DWORD) 0x00CC0020 
#define BITSPIXEL 12 
#define DEFAULT_PALETTE 15
#define RC_PALETTE 0x0100
#define RASTERCAPS 38



INTERNAL VOSTRUCT RGBQUAD
    MEMBER rgbBlue     AS BYTE
    MEMBER rgbGreen    AS BYTE
    MEMBER rgbRed      AS BYTE
    MEMBER rgbReserved AS BYTE
        
INTERNAL VOSTRUCT BITMAPINFOHEADER
    MEMBER biSize          AS DWORD
    MEMBER biWidth         AS LONGINT
    MEMBER biHeight        AS LONGINT
    MEMBER biPlanes        AS WORD
    MEMBER biBitCount      AS WORD
    MEMBER biCompression   AS DWORD
    MEMBER biSizeImage     AS DWORD
    MEMBER biXPelsPerMeter AS LONGINT
    MEMBER biYPelsPerMeter AS LONGINT
    MEMBER biClrUsed       AS DWORD
    MEMBER biClrImportant  AS DWORD
        
INTERNAL VOSTRUCT RECT
    MEMBER left   AS LONGINT
    MEMBER top    AS LONGINT
    MEMBER right  AS LONGINT
    MEMBER bottom AS LONGINT
        
INTERNAL VOSTRUCT BITMAPFILEHEADER ALIGN 2
    MEMBER  bfType 		AS WORD
    MEMBER  bfSize 		AS DWORD
    MEMBER  bfReserved1 	AS WORD
    MEMBER  bfReserved2 	AS WORD
    MEMBER  bfOffBits 	AS DWORD
        
INTERNAL VOSTRUCT PAINTSTRUCT
    MEMBER hdc AS IntPtr
    MEMBER fErase AS INT // LOGIC
    MEMBER rcPaint IS RECT
    MEMBER fRestore AS INT // LOGIC
    MEMBER fIncUpdate AS INT // LOGIC
    MEMBER DIM rgbReserved[32] AS BYTE
        
INTERNAL VOSTRUCT BITMAPINFO
    MEMBER bmiHeader IS BITMAPINFOHEADER
    MEMBER DIM bmiColors[1] IS RGBQUAD
        
INTERNAL VOSTRUCT LOGPALETTE
    MEMBER palVersion    AS WORD
    MEMBER palNumEntries AS WORD
    MEMBER DIM palPalEntry[1] IS PALETTEENTRY
        
INTERNAL VOSTRUCT PALETTEENTRY
    MEMBER peRed   AS BYTE
    MEMBER peGreen AS BYTE
    MEMBER peBlue  AS BYTE
    MEMBER peFlags AS BYTE
        
CLASS VOBitmaps
    PROTECTED hDIBInfo AS IntPtr
    PROTECTED hDDBitmap AS IntPtr
    PROTECTED offBits AS WORD
    PROTECTED hOldBitmap AS IntPtr
    PROTECTED hMemDC AS IntPtr
        
    STATIC METHOD Show(hWnd AS IntPtr,cFileName AS STRING,cTitle AS STRING) AS LOGIC
        RETURN Process(hWnd, cFileName, cTitle, FALSE)
            
    STATIC METHOD Stretch(hWnd AS IntPtr,cFileName AS STRING,cTitle AS STRING) AS LOGIC
        RETURN Process(hWnd, cFileName, cTitle, FALSE)
        
    STATIC METHOD Process(hWnd AS IntPtr, cFileName AS STRING, cTitle AS STRING, stretch AS LOGIC) AS LOGIC
        LOCAL bRet := FALSE AS LOGIC
        LOCAL oBitMap := VOBitmaps{} AS VOBitMaps
        oBitMap:hDIBInfo := GlobalAlloc(  GMEM_MOVEABLE, (DWORD)(sizeof(BITMAPINFOHEADER) + 256 * sizeof(RGBQUAD) ) )
            
        IF hWnd == IntPtr.Zero
            hWnd := GetFocus()
        ENDIF
            
        IF oBitMap:InitDIB( hWnd,  IIF( stretch, STRETCHDIB, SETDIB ), cTitle, cFileName )
            oBitMap:PaintDIB( hWnd, IIF( stretch, STRETCHDIB, SETDIB ), cFileName )
            bRet := TRUE
        ENDIF
            
        GlobalFree( oBitMap:hDIBInfo )
        RETURN bRet
            
    PRIVATE METHOD  InitDIB( hWnd AS IntPtr, wOperation AS SHORT, cWinTitle AS STRING, cFileName AS STRING ) AS LOGIC
        LOCAL lpbi       AS BITMAPINFOHEADER
        LOCAL rectNew    IS RECT
        LOCAL rectClient IS RECT
        LOCAL rectWindow IS RECT
        LOCAL uiDeltaX   AS DWORD
        LOCAL uiDeltaY   AS DWORD
            
        IF ! ReadDIB( cFileName )
            RETURN FALSE
        ENDIF
            
        lpbi := (BITMAPINFOHEADER PTR) GlobalLock( hDIBInfo )
            
        SetWindowText( hWnd, cWinTitle )
            
        IF wOperation == SETDIB
            rectNew:left	  := 0
            rectNew:top 	  := 0
            rectNew:right	  := (WORD)lpbi:biWidth
            rectNew:bottom	  := (WORD)lpbi:biHeight
                
            GetClientRect( hWnd, @rectClient )
            GetWindowRect( hWnd, @rectWindow )
                
            uiDeltaX := (DWORD) ( rectWindow:right -  rectWindow:left - rectClient:right )
            uiDeltaY := (DWORD) ( rectWindow:bottom - rectWindow:top - rectClient:bottom )
                
            rectNew:bottom += (INT) uiDeltaY
            rectNew:right  += (INT) uiDeltaX
                
            SetWindowPos( hWnd, NULL, 0, 0, rectNew:right - rectNew:left, rectNew:bottom - rectNew:top + 1,	SWP_NOMOVE | SWP_NOZORDER )
        ENDIF
            
        InvalidateRect( hWnd, NULL, TRUE )
            
        GlobalUnlock( hDIBInfo )
            
        RETURN TRUE
            
    PRIVATE METHOD ReadDIB( cDibFile AS STRING ) AS LOGIC
        LOCAL hf AS IntPtr
        LOCAL lpbi AS BITMAPINFOHEADER
        LOCAL bf IS BITMAPFILEHEADER
        LOCAL nNumColors AS WORD
        LOCAL result := FALSE AS LOGIC
            
        hf := FOpen2( cDibFile, FO_READ + FO_DENYNONE )
            
        IF hf == F_ERROR
            THROW Error.VOError( EG_OPEN, "ReadDIB", "cDibFile", 2, { cDibFile } )   
        ENDIF
            
        lpbi := (BITMAPINFOHEADER PTR) GlobalLock( hDIBInfo )
            
        IF sizeof( BITMAPFILEHEADER ) != FRead3( hf, @bf, sizeof( BITMAPFILEHEADER ) )
            FClose( hf )
            THROW Error.VOError( EG_READ, "ReadDIB", "cDibFile", 2, { cDibFile } )   
        ENDIF
            
        IF bf:bfType != 0x4d42   /* 'BM' */
            FClose( hf )
            THROW Error.VOError( EG_UNSUPPORTED, "ReadDIB", "cDibFile", 2, { cDibFile } )   
        ENDIF
            
        IF sizeof( BITMAPINFOHEADER ) != FRead3( hf, lpbi, sizeof( BITMAPINFOHEADER ) )
            FClose( hf )
            THROW Error.VOError( EG_READ, "ReadDIB", "cDibFile", 2, { cDibFile } )   
        ENDIF
            
        nNumColors := (WORD) lpbi:biClrUsed
            
        IF nNumColors == 0
            IF lpbi:biBitCount != 24
                nNumColors := (WORD) (1 << lpbi:biBitCount) /* standard size table */
            ENDIF
        ENDIF
            
        IF lpbi:biClrUsed == 0
            lpbi:biClrUsed := (DWORD) nNumColors
        ENDIF
            
        IF lpbi:biSizeImage == 0
            lpbi:biSizeImage := (DWORD) ( ((((lpbi:biWidth * lpbi:biBitCount) + 31) & ~31) >> 3) * lpbi:biHeight)
        ENDIF
            
        GlobalUnlock( hDIBInfo )
            
        hDIBInfo := GlobalReAlloc( hDIBInfo, lpbi:biSize + nNumColors * sizeof(RGBQUAD) + lpbi:biSizeImage, 0 )
            
        IF hDIBInfo == 0
            FClose( hf )
            THROW Error.VOError( EG_MEM, "ReadDIB", "cDibFile", 2, { cDibFile } )   
        ENDIF
            
        lpbi := (BITMAPINFOHEADER PTR) GlobalLock( hDIBInfo )
            
        FRead3( hf, ((BYTE PTR) lpbi) + lpbi:biSize, nNumColors * sizeof( RGBQUAD ) )
            
        offBits := (WORD) (lpbi:biSize + nNumColors * sizeof( RGBQUAD ) )
            
        IF bf:bfOffBits != 0
            FSeek3( hf, (INT) bf:bfOffBits, FS_SET )
        ENDIF
            
        IF lpbi:biSizeImage == FRead3( hf, ((BYTE PTR) lpbi) + offBits, lpbi:biSizeImage )
            result := TRUE
        ENDIF
            
        GlobalUnlock( hDIBInfo )
            
        FClose( hf )
            
        RETURN result
    PRIVATE METHOD PaintDIB( hWnd AS IntPtr, wOperation AS WORD, cDibFile AS STRING ) AS VOID
        LOCAL ps         IS PAINTSTRUCT
        LOCAL hDC        := IntPtr.Zero AS IntPtr
        LOCAL hOldPal   := IntPtr.Zero AS IntPtr
        LOCAL hPalette  := IntPtr.Zero AS IntPtr
        LOCAL wDIBUse   := 0 AS WORD
        LOCAL Rectangle  IS RECT
        LOCAL fNewHeader := FALSE  AS LOGIC
        LOCAL lpHeader  := NULL  AS BITMAPINFOHEADER
        LOCAL lpBmpInfo := NULL  AS BITMAPINFOHEADER
        LOCAL fError     := FALSE AS LOGIC
            
        hDC 	:= BeginPaint( hWnd, @ps )
            
        lpBmpInfo := (BITMAPINFOHEADER PTR) GlobalLock( hDIBInfo )
            
        // Test for RC_PALETTE added my Meinhard, in order to fix bug with StretchBitmap()
        // Not sure why in VO the same code works correctly, without it..
        IF lpBmpInfo:biClrUsed != 0 .AND. ((GetDeviceCaps( hDC, RASTERCAPS ) ~ RC_PALETTE) == RC_PALETTE)
            hPalette := MakeDIBPalette( lpBmpInfo, hDC )
                
            IF hPalette == IntPtr.Zero
                fError := TRUE
            ENDIF
                
            IF ! fError
                hOldPal := SelectPalette( hDC, hPalette, FALSE )
                    
                RealizePalette( hDC )
                    
                IF lpBmpInfo:biBitCount < 24
                    lpHeader := MakeIndexHeader( lpBmpInfo )
                        
                    IF lpHeader != IntPtr.Zero
                        fNewHeader := TRUE
                    ELSE
                        fError := TRUE
                    ENDIF
                        
                    wDIBUse := DIB_PAL_COLORS
                ELSE
                    lpHeader := lpBmpInfo
                    wDIBUse  := DIB_RGB_COLORS
                ENDIF
            ENDIF   
        ELSE
            lpHeader := lpBmpInfo
            wDIBUse  := DIB_RGB_COLORS
        ENDIF
            
        IF ! fError
            hMemDC	  := CreateCompatibleDC( hDC )
            hDDBitmap  := CreateCompatibleBitmap( hDC, (WORD) lpBmpInfo:biWidth, (WORD) lpBmpInfo:biHeight )
            hOldBitmap := SelectObject( hMemDC, hDDBitmap )
                
            IF wOperation == SETDIB
                SetDIBitsToDevice( hDC,; 					// hDC
                    0,;								// DestX
                    0,;								// DestY
                    (WORD) lpBmpInfo:biWidth,;		   // nDestWidth
                    (WORD) lpBmpInfo:biHeight,;		   // nDestHeight
                    0,;								// SrcX
                    0,;								// SrcY
                    0,;								// nStartScan
                    (WORD) lpBmpInfo:biHeight,;		   // nNumScans
                    ((BYTE PTR)lpBmpInfo) + offBits,;		   // lpBits
                    (BITMAPINFO PTR) lpHeader,;			// lpBitsInfo
                    wDIBUse )						// wUsage
                    
            ELSEIF wOperation == STRETCHDIB
                GetClientRect( hWnd, @Rectangle )
                StretchDIBits( hDC, 0, 0, Rectangle:right, Rectangle:bottom,;
                    0, 0, (WORD) lpBmpInfo:biWidth, (WORD) lpBmpInfo:biHeight,;
                    ((BYTE PTR)lpBmpInfo) + offBits,;
                    (BITMAPINFO PTR) lpHeader, wDIBUse, SRCCOPY )
            ENDIF
                
            SelectPalette( hDC, hOldPal, FALSE )
        ENDIF   
            
        SelectObject( hMemDC, hOldBitmap )
        DeleteDC( hMemDC )
        DeleteObject( hDDBitmap )
            
        hDDBitmap := IntPtr.Zero
        DeleteObject( hPalette )
        hPalette := IntPtr.Zero
            
        EndPaint( hWnd, @ps )
            
        IF fNewHeader
            MemFree( lpHeader )
        ENDIF
            
        GlobalUnlock( hDIBInfo )
            
        IF fError
            THROW Error.VOError( EG_CORRUPTION, "PaintDIB", "cDibFile", 3, { cDibFile } )   
        ENDIF
    PRIVATE METHOD  MakeDIBPalette( lpBmpInfo AS BITMAPINFOHEADER PTR, hDC AS IntPtr ) AS IntPtr
        LOCAL lpPal   := NULL AS LOGPALETTE
        LOCAL lpRGB   AS RGBQUAD
        LOCAL hLogPal AS IntPtr
        LOCAL i       AS WORD
        TRY
            IF lpBmpInfo:biClrUsed != 0 && lpBmpInfo:biBitCount < 24
                lpPal := (LOGPALETTE PTR) MemAlloc( sizeof(LOGPALETTE) + (WORD)lpBmpInfo:biClrUsed * sizeof(PALETTEENTRY) )
                    
                IF lpPal == NULL
                    RETURN NULL
                ENDIF
                    
                lpPal:palVersion	  := 0x300
                lpPal:palNumEntries := (WORD) lpBmpInfo:biClrUsed
                    
                lpRGB := (RGBQUAD PTR)(lpBmpInfo + lpBmpInfo:biSize)
                    
                i := 0
                    
                DO WHILE i < (WORD) lpBmpInfo:biClrUsed
                    lpPal:palPalEntry[i + 1]:peRed 	:= lpRGB:rgbRed
                    lpPal:palPalEntry[i + 1]:peGreen	:= lpRGB:rgbGreen
                    lpPal:palPalEntry[i + 1]:peBlue	:= lpRGB:rgbBlue
                    lpPal:palPalEntry[i + 1]:peFlags	:= 0
                    i++
                    lpRGB++
                ENDDO
                    
                hLogPal := CreatePalette( (LOGPALETTE PTR) lpPal )
                    
            ELSE
                IF GetDeviceCaps( hDC, BITSPIXEL ) < 24
                    hLogPal := CreateHalftonePalette( hDC )
                ELSE
                    hLogPal := GetStockObject( DEFAULT_PALETTE )
                ENDIF
            ENDIF
        FINALLY
            IF (lpPal != NULL_PTR)
                MemFree(lpPal)
            ENDIF
        END TRY
        RETURN hLogPal
            
    PRIVATE METHOD  MakeIndexHeader( lpBmpInfo AS BITMAPINFOHEADER PTR ) AS BITMAPINFOHEADER PTR
        LOCAL lpPalInfo := NULL AS BITMAPINFOHEADER PTR
        LOCAL lpTable AS WORD PTR
        LOCAL i AS WORD
            
        IF lpBmpInfo:biClrUsed != 0
            lpPalInfo := (BITMAPINFOHEADER PTR) MemAlloc( (WORD)lpBmpInfo:biSize + (WORD)lpBmpInfo:biClrUsed * sizeof(WORD) )
                
            IF lpPalInfo != NULL
                MemCopy( lpPalInfo, lpBmpInfo, sizeof( BITMAPINFOHEADER ) ) // *lpPalInfo := *lpBmpInfo;
                    
                    lpTable := (WORD PTR)(lpPalInfo + lpPalInfo:biSize)
                        
                i := 0
                DO WHILE i < (WORD) lpBmpInfo:biClrUsed
                    lpTable[1] := i  // *lpTable++ := i
                    lpTable++		   
                    i++
                ENDDO
            ENDIF
        ENDIF
            
        RETURN lpPalInfo
            
END CLASS
    
    
    
    
#endregion
