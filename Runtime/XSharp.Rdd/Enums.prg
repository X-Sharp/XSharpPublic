//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBF Header Codepage numbers.</summary>  
    ENUM DbfHeaderCodepage INHERIT BYTE
    
        MEMBER CP_DBF_DOS_OLD           := 0x00  // MS-DOS previous versions
        MEMBER CP_DBF_DOS_US            := 0x01  // U.S. MS-DOS
        MEMBER CP_DBF_DOS_INTL          := 0x02  // International MS-DOS
        MEMBER CP_DBF_WIN_ANSI          := 0x03  // Windows ANSI
        MEMBER CP_DBF_MAC_STANDARD      := 0x04  // Standard Macintosh
        
        MEMBER CP_DBF_DOS_EEUROPEAN     := 0x64  // Eastern European MS-DOS
        MEMBER CP_DBF_DOS_RUSSIAN       := 0x65  // Russian MS-DOS
        MEMBER CP_DBF_DOS_NORDIC        := 0x66  // Nordic MS-DOS
        MEMBER CP_DBF_DOS_ICELANDIC     := 0x67  // Icelandic MS-DOS
        MEMBER CP_DBF_DOS_KAMENICKY     := 0x68  // Kamenicky (Czech) MS-DOS (1)
        MEMBER CP_DBF_DOS_MAZOVIA       := 0x69  // Mazovia (Polish) MS-DOS  (1)
        MEMBER CP_DBF_DOS_GREEK         := 0x6A  // Greek MS-DOS (437G)      (1)
        MEMBER CP_DBF_DOS_TURKISH       := 0x6B  // Turkish MS-DOS
        MEMBER CP_DBF_DOS_CANADIAN      := 0x6C  // Canadian MS-DOS
        
        MEMBER CP_DBF_WIN_CHINESE_1	 := 0x78	// Chinese (Hong Kong SAR, Taiwan) Windows CP 950
        MEMBER CP_DBF_WIN_KOREAN        := 0x79	// Korean Windows				CP 949
        
        MEMBER CP_DBF_WIN_CHINESE_2	 := 0x7A	 // Chinese (PRC, Singapore) Windows	CP 936
        MEMBER CP_DBF_WIN_JAPANESE		 := 0x7B	 // Japanese Windows				CP 932
        MEMBER CP_DBF_WIN_THAI			 := 0x7C	 // Thai Windows					CP 874
        MEMBER CP_DBF_WIN_HEBREW		 := 0x7D	 // Hebrew Windows				CP 1255
        MEMBER CP_DBF_WIN_ARABIC		 := 0x7E	 // Arabic Windows				CP 1256
        
        MEMBER CP_DBF_WIN_EEUROPEAN     := 0xC8   // Eastern European Windows
        MEMBER CP_DBF_WIN_RUSSIAN       := 0xC9   // Russian Windows
        MEMBER CP_DBF_WIN_GREEK         := 0xCB   // Greek Windows
        MEMBER CP_DBF_WIN_TURKISH       := 0xCA   // Turkish Windows
        
        MEMBER CP_DBF_MAC_RUSSIAN       := 0x96   // Russian Macintosh (1)
        MEMBER CP_DBF_MAC_EEUROPEAN     := 0x97   // Macintosh EE
        MEMBER CP_DBF_MAC_GREEK         := 0x98   // Greek Macintosh
    END ENUM
    //  
    /// <summary>OS Codepages as used in DBF files.</summary>  
    ENUM OsCodepage
    
        MEMBER CP_INI_DOS_US            := 437   // U.S. MS-DOS
        MEMBER CP_INI_DOS_MAZOVIA       := 620   // Mazovia (Polish) MS-DOS (1)
        MEMBER CP_INI_DOS_GREEK         := 737   // Greek MS-DOS (1)
        MEMBER CP_INI_DOS_INTL          := 850   // International MS-DOS
        MEMBER CP_INI_DOS_EEUROPEAN     := 852   // Eastern European MS-DOS
        MEMBER CP_INI_DOS_TURKISH       := 857   // Turkish MS-DOS
        MEMBER CP_INI_DOS_ICELANDIC     := 861   // Icelandic MS-DOS
        MEMBER CP_INI_DOS_CANADIAN      := 863   // Canadian MS-DOS
        MEMBER CP_INI_DOS_NORDIC        := 865   // Nordic MS-DOS
        MEMBER CP_INI_DOS_RUSSIAN       := 866   // Russian MS-DOS
        MEMBER CP_INI_DOS_KAMENICKY     := 895   // Kamenicky (Czech) MS-DOS (1)
        
        MEMBER CP_INI_WIN_THAI          := 874   // Thai Windows
        MEMBER CP_INI_WIN_JAPANESE      := 932   // Japanese Windows
        MEMBER CP_INI_WIN_CHINESE1      := 936   // Chinese (PRC, Singapore) Windows
        MEMBER CP_INI_WIN_KOREAN        := 949   // Korean Windows
        MEMBER CP_INI_WIN_CHINESE2      := 950   // Chinese (Hong Kong SAR, Taiwan) Windows
        
        MEMBER CP_INI_WIN_EEUROPEAN     := 1250  // Eastern European Windows
        MEMBER CP_INI_WIN_RUSSIAN       := 1251  // Russian Windows
        MEMBER CP_INI_WIN_ANSI          := 1252  // Windows ANSI
        MEMBER CP_INI_WIN_GREEK         := 1253  // Greek Windows
        MEMBER CP_INI_WIN_TURKISH       := 1254  // Turkish Windows
        MEMBER CP_INI_WIN_HEBREW        := 1255  // Hebrew Windows
        MEMBER CP_INI_WIN_ARABIC        := 1256  // Arabic Windows
        
        MEMBER CP_INI_MAC_STANDARD      := 10000 // Standard Macintosh
        MEMBER CP_INI_MAC_GREEK         := 10006 // Greek Macintosh
        MEMBER CP_INI_MAC_RUSSIAN       := 10007 // Russian Macintosh (1)
        MEMBER CP_INI_MAC_EEUROPEAN     := 10029 // Macintosh EE
    END ENUM
    

    
    /// <summary>DBF Locking model.</summary>                            
    ENUM DbfLockingModel
        MEMBER Clipper52    // Clipper 5.2 locking scheme
        MEMBER Clipper53    // Clipper 5.3 locking scheme
        MEMBER FoxPro       // Visual FoxPro locking scheme
        MEMBER FoxProExt    // Visual FoxPro locking scheme
        MEMBER Clipper53Ext // Clipper 5.3 with Files up to 4GB
        MEMBER Harbour64    // Locking scheme for files > 4GB
        MEMBER VoAnsi       // VO Locking scheme for Ansi DBF files 
    END ENUM
    
    /// <summary>DBF Table flags.</summary>                            
    [Flags];
    ENUM DBFTableFlags AS BYTE
        MEMBER None             := 0
        MEMBER HasStructuralCDX := 1
        MEMBER HasMemoField     := 2
        MEMBER IsDBC            := 4
        MEMBER IsOLE            := 128
    END ENUM

    /// <summary>DBF File Versions.</summary>                            
    ENUM DBFVersion AS BYTE
        MEMBER FoxBase:=2
        MEMBER FoxBaseDBase3NoMemo:=3
        MEMBER dBase4 :=4
        MEMBER dBase5 :=5
        MEMBER VO :=7
        MEMBER Flagship := 0x13
        MEMBER Flagship248 := 0x23
        MEMBER VisualFoxPro:=0x30
        MEMBER VisualFoxProAutoIncrement:=0x31
        MEMBER VisualFoxProVarChar :=0x32
        MEMBER Flagship248WithDBV := 0x33
        MEMBER dBase4SQLTableNoMemo:=0x43
        MEMBER dBase4SQLSystemNoMemo:=0x63
        MEMBER dBase4WithMemo_:=0x7b
        MEMBER FoxBaseDBase3WithMemo:=0x83
        MEMBER VOWithMemo := 0x87
        MEMBER dBase4WithMemo:=0x8b
        MEMBER dBase4SQLTableWithMemo:=0xcb
        MEMBER ClipperSixWithSMT:=0xe5
        MEMBER FoxPro2WithMemo:=0xf5
        MEMBER FoxBASE_:=0xfb
        
        MEMBER Unknown:=0
    END ENUM
    
 
    STATIC CLASS CodePageExtensions
        STATIC METHOD ToCodePage(SELF headerCodePage AS DBFHeaderCodePage) AS OsCodePage
            SWITCH headerCodePage
            CASE DbfHeaderCodepage.CP_DBF_DOS_US        ; RETURN OsCodepage.CP_INI_DOS_US        
            CASE DbfHeaderCodepage.CP_DBF_DOS_MAZOVIA   ; RETURN OsCodepage.CP_INI_DOS_MAZOVIA   
            CASE DbfHeaderCodepage.CP_DBF_DOS_GREEK     ; RETURN OsCodepage.CP_INI_DOS_GREEK     
            CASE DbfHeaderCodepage.CP_DBF_DOS_INTL      ; RETURN OsCodepage.CP_INI_DOS_INTL      
            CASE DbfHeaderCodepage.CP_DBF_DOS_EEUROPEAN ; RETURN OsCodepage.CP_INI_DOS_EEUROPEAN 
            CASE DbfHeaderCodepage.CP_DBF_DOS_ICELANDIC ; RETURN OsCodepage.CP_INI_DOS_ICELANDIC 
            CASE DbfHeaderCodepage.CP_DBF_DOS_NORDIC    ; RETURN OsCodepage.CP_INI_DOS_NORDIC    
            CASE DbfHeaderCodepage.CP_DBF_DOS_RUSSIAN   ; RETURN OsCodepage.CP_INI_DOS_RUSSIAN   
            CASE DbfHeaderCodepage.CP_DBF_DOS_KAMENICKY ; RETURN OsCodepage.CP_INI_DOS_KAMENICKY 
            CASE DbfHeaderCodepage.CP_DBF_DOS_TURKISH   ; RETURN OsCodepage.CP_INI_DOS_TURKISH   
            CASE DbfHeaderCodepage.CP_DBF_DOS_CANADIAN  ; RETURN OsCodepage.CP_INI_DOS_CANADIAN  
            CASE DbfHeaderCodepage.CP_DBF_WIN_THAI      ; RETURN OsCodepage.CP_INI_WIN_THAI	    
            CASE DbfHeaderCodepage.CP_DBF_WIN_JAPANESE  ; RETURN OsCodepage.CP_INI_WIN_JAPANESE  
            CASE DbfHeaderCodepage.CP_DBF_WIN_CHINESE_1 ; RETURN OsCodepage.CP_INI_WIN_CHINESE1  
            CASE DbfHeaderCodepage.CP_DBF_WIN_KOREAN    ; RETURN OsCodepage.CP_INI_WIN_KOREAN	
            CASE DbfHeaderCodepage.CP_DBF_WIN_CHINESE_2 ; RETURN OsCodepage.CP_INI_WIN_CHINESE2  
                
            CASE DbfHeaderCodepage.CP_DBF_WIN_EEUROPEAN ; RETURN OsCodepage.CP_INI_WIN_EEUROPEAN  
            CASE DbfHeaderCodepage.CP_DBF_WIN_RUSSIAN   ; RETURN OsCodepage.CP_INI_WIN_RUSSIAN    
            CASE DbfHeaderCodepage.CP_DBF_WIN_ANSI      ; RETURN OsCodepage.CP_INI_WIN_ANSI       
            CASE DbfHeaderCodepage.CP_DBF_WIN_GREEK     ; RETURN OsCodepage.CP_INI_WIN_GREEK      
            CASE DbfHeaderCodepage.CP_DBF_WIN_TURKISH   ; RETURN OsCodepage.CP_INI_WIN_TURKISH    
            CASE DbfHeaderCodepage.CP_DBF_WIN_HEBREW    ; RETURN OsCodepage.CP_INI_WIN_HEBREW     
            CASE DbfHeaderCodepage.CP_DBF_WIN_ARABIC    ; RETURN OsCodepage.CP_INI_WIN_ARABIC     
                
            CASE DbfHeaderCodepage.CP_DBF_MAC_STANDARD  ; RETURN OsCodepage.CP_INI_MAC_STANDARD   
            CASE DbfHeaderCodepage.CP_DBF_MAC_GREEK     ; RETURN OsCodepage.CP_INI_MAC_GREEK      
            CASE DbfHeaderCodepage.CP_DBF_MAC_RUSSIAN   ; RETURN OsCodepage.CP_INI_MAC_RUSSIAN    
            CASE DbfHeaderCodepage.CP_DBF_MAC_EEUROPEAN ; RETURN OsCodepage.CP_INI_MAC_EEUROPEAN
            OTHERWISE
                RETURN 0
                    
            END SWITCH
                
        STATIC METHOD ToHeaderCodePage(SELF codePage AS OsCodePage) AS DbfHeaderCodePage
            SWITCH codePage
            CASE OsCodepage.CP_INI_DOS_US       ; RETURN DbfHeaderCodepage.CP_DBF_DOS_US         
            CASE OsCodepage.CP_INI_DOS_MAZOVIA  ; RETURN DbfHeaderCodepage.CP_DBF_DOS_MAZOVIA    
            CASE OsCodepage.CP_INI_DOS_GREEK    ; RETURN DbfHeaderCodepage.CP_DBF_DOS_GREEK      
            CASE OsCodepage.CP_INI_DOS_INTL     ; RETURN DbfHeaderCodepage.CP_DBF_DOS_INTL       
            CASE OsCodepage.CP_INI_DOS_EEUROPEAN; RETURN DbfHeaderCodepage.CP_DBF_DOS_EEUROPEAN  
            CASE OsCodepage.CP_INI_DOS_ICELANDIC; RETURN DbfHeaderCodepage.CP_DBF_DOS_ICELANDIC  
            CASE OsCodepage.CP_INI_DOS_NORDIC   ; RETURN DbfHeaderCodepage.CP_DBF_DOS_NORDIC     
            CASE OsCodepage.CP_INI_DOS_RUSSIAN  ; RETURN DbfHeaderCodepage.CP_DBF_DOS_RUSSIAN    
            CASE OsCodepage.CP_INI_DOS_KAMENICKY; RETURN DbfHeaderCodepage.CP_DBF_DOS_KAMENICKY  
            CASE OsCodepage.CP_INI_DOS_TURKISH  ; RETURN DbfHeaderCodepage.CP_DBF_DOS_TURKISH    
            CASE OsCodepage.CP_INI_DOS_CANADIAN ; RETURN DbfHeaderCodepage.CP_DBF_DOS_CANADIAN   
                
            CASE OsCodepage.CP_INI_WIN_EEUROPEAN; RETURN DbfHeaderCodepage.CP_DBF_WIN_EEUROPEAN  
            CASE OsCodepage.CP_INI_WIN_RUSSIAN  ; RETURN DbfHeaderCodepage.CP_DBF_WIN_RUSSIAN    
            CASE OsCodepage.CP_INI_WIN_ANSI     ; RETURN DbfHeaderCodepage.CP_DBF_WIN_ANSI       
            CASE OsCodepage.CP_INI_WIN_GREEK    ; RETURN DbfHeaderCodepage.CP_DBF_WIN_GREEK      
            CASE OsCodepage.CP_INI_WIN_TURKISH  ; RETURN DbfHeaderCodepage.CP_DBF_WIN_TURKISH    
                
            CASE OsCodepage.CP_INI_MAC_STANDARD ; RETURN DbfHeaderCodepage.CP_DBF_MAC_STANDARD   
            CASE OsCodepage.CP_INI_MAC_GREEK    ; RETURN DbfHeaderCodepage.CP_DBF_MAC_GREEK      
            CASE OsCodepage.CP_INI_MAC_RUSSIAN  ; RETURN DbfHeaderCodepage.CP_DBF_MAC_RUSSIAN    
            CASE OsCodepage.CP_INI_MAC_EEUROPEAN; RETURN DbfHeaderCodepage.CP_DBF_MAC_EEUROPEAN  
                
            OTHERWISE
                RETURN DbfHeaderCodepage.CP_DBF_DOS_US 
        END SWITCH

        STATIC METHOD IsAnsi (SELF codePage AS DBFHeaderCodePage) AS LOGIC
            SWITCH codePage
            CASE DbfHeaderCodepage.CP_DBF_WIN_EEUROPEAN
            CASE DbfHeaderCodepage.CP_DBF_WIN_RUSSIAN
            CASE DbfHeaderCodepage.CP_DBF_WIN_ANSI
            CASE DbfHeaderCodepage.CP_DBF_WIN_GREEK
            CASE DbfHeaderCodepage.CP_DBF_WIN_TURKISH
                RETURN TRUE
            END SWITCH
            RETURN FALSE
 
    END CLASS    
    
    
END NAMESPACE
