//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.IO

// VFP standard flags
DEFINE S2F_FLAG_OVERWRITE           = 0x0000
DEFINE S2F_FLAG_APPEND              = 0x0001
DEFINE S2F_FLAG_UNICODE_LE          = 0x0002
DEFINE S2F_FLAG_UTF8                = 0x0004
// X# extension flags
DEFINE S2F_FLAG_UNICODE_BE          = 0x0008
DEFINE S2F_FLAG_UNICODE_FORMATS     = S2F_FLAG_UNICODE_LE | S2F_FLAG_UTF8 | S2F_FLAG_UNICODE_BE
DEFINE S2F_FLAG_UNICODE_TEXT        = 0x0100

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mkdir/*" />
FUNCTION MkDir(cPath AS STRING) AS INT
    LOCAL nRet AS INT
    IF System.IO.Directory.Exists(cPath)
        nRet := 6
    ELSE
        TRY
            System.IO.Directory.CreateDirectory(cPath)
            nRet := 0
        CATCH
            nRet := 1
        END TRY
    END IF
RETURN nRet


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strtofile/*" />
FUNCTION StrToFile (cExpression AS STRING, cFileName AS STRING) AS INT
    RETURN StrToFile(cExpression, cFileName, S2F_FLAG_OVERWRITE)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strtofile/*" />
FUNCTION StrToFile (cExpression AS STRING, cFileName AS STRING, lAdditive AS Boolean) AS INT
    RETURN StrToFile(cExpression, cFileName, IIF(lAdditive, S2F_FLAG_APPEND, S2F_FLAG_OVERWRITE))

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strtofile/*" />
FUNCTION StrToFile (cExpression AS STRING, cFileName AS STRING, nFlags AS INT) AS INT
    LOCAL Additive := .F. AS LOGIC
    LOCAL BOM := "" AS STRING
    LOCAL Result := 0 AS DWORD
    LOCAL FHandle AS IntPtr
    LOCAL VFPBehavior := .T. AS LOGIC      // it means the string must hold an already prepared buffer, or it is binary
    LOCAL UnicodeEncoding AS System.Text.Encoding
    LOCAL IOError AS Exception
    UnicodeEncoding := System.Text.Encoding.Default
    SWITCH nFlags
    CASE S2F_FLAG_OVERWRITE
        // Just overwrite the file.        
        Additive := .F.
    CASE S2F_FLAG_APPEND
        // VFP behavior, append to a file with no Unicode encoding
        Additive := .T.
            
    CASE S2F_FLAG_UNICODE_LE
        // VFP behavior, create a Unicode file (source buffer already prepared)
        BOM := e"\xFF\xFE"
            
    CASE S2F_FLAG_UTF8
        // VFP behavior, create a UTF-8 file (source buffer already prepared)
        BOM := e"\xEF\xBB\xBF"
            
    CASE S2F_FLAG_UNICODE_BE
        // extension to VFP behavior, create a Unicode Big-Endian file (source buffer already prepared)
        BOM := e"\xFE\xFF"
            
    OTHERWISE
        // if not simply overwrite, the file will be treated as a Unicode text file
                
        IF _AND(nFlags , S2F_FLAG_UNICODE_TEXT) != 0
                        
            VFPBehavior := .F.
            Additive    := _AND(nFlags , S2F_FLAG_APPEND) != 0
                        
            // set the Unicode encoding
            SWITCH _AND(nFlags,S2F_FLAG_UNICODE_FORMATS)
            CASE S2F_FLAG_UNICODE_LE
                UnicodeEncoding := System.Text.Encoding.Unicode
                                
            CASE S2F_FLAG_UTF8
                UnicodeEncoding := System.Text.Encoding.UTF8
                                
            CASE S2F_FLAG_UNICODE_BE
                UnicodeEncoding := System.Text.Encoding.BigEndianUnicode
                                
            OTHERWISE
                // error if Unicode encoding not properly set
                THROW ArgumentException{"Incorrect Encoding Parameter",nameof(nFlags)}
                                
            END SWITCH
        ELSE
            THROW ArgumentException{"Incorrect Parameter",nameof(nFlags)}
        ENDIF
    END SWITCH
    
    * append mode?
    IF Additive
            
        IF VFPBehavior
                    
            * open an existing file, or create if it does not exists
            FHandle := FOpen(cFileName, FO_READWRITE + FO_SHARED)
            IF FHandle != F_ERROR
                * try to move to the end of the file
                FSeek3(FHandle, 0, FS_END)
                * and write the contents of the buffer
                IF FError() == 0
                    Result := FWrite(FHandle, cExpression, (DWORD) cExpression:Length)
                ENDIF
                * if everything went ok, close the file handle
                IF FError() == 0
                        FClose(FHandle)
                        IF FError() != 0
                            THROW FException()
                        ENDIF
                    * if not, before throwing the exception...
                ELSE
                    IOError := FException()
                    FClose(FHandle)         // ... try to close the handle, anyway
                    THROW IOError
                ENDIF
            ELSE
                THROW FException()
            ENDIF
        ELSE
            * in non-VFP behavior, just append the Unicode string to an existing file
            TRY
                File.AppendAllText(cFileName, cExpression, UnicodeEncoding)
            CATCH
                THROW
            END TRY
                
            Result := (DWORD) cExpression:Length
                
        ENDIF
        
    ELSE
        
        * TO-DO: check on SET("Safety") 
        
        * create a new file
        IF VFPBehavior
                
            * get an handle for a new file
            FHandle := FCreate(cFileName)
            IF FHandle != F_ERROR
                * start with the BOM of the file
                IF !String.IsNullOrEmpty(BOM)
                    Result = FWrite(FHandle, BOM, (DWORD) BOM:Length)
                ENDIF
                * try to write the contents from the buffer
                IF FError() == 0
                    Result += FWrite(FHandle, cExpression, (DWORD) cExpression:Length)
                ENDIF
                * and close the file
                IF FError() == 0
                    FClose(FHandle)
                    IF FError() != 0
                        THROW FException()
                    ENDIF
                    * if an error occurred...
                ELSE
                    IOError = FException()
                    FClose(FHandle)     // ... try to not leave the handle open, in any case
                    THROW IOError
                ENDIF
            ELSE
                THROW FException()
            ENDIF
            
        ELSE
            
            * write the Unicode string to a text file
            TRY
                File.WriteAllText(cFileName, cExpression, UnicodeEncoding)
            CATCH
                THROW
            END TRY
            Result := (DWORD) cExpression:Length
        ENDIF
        
    ENDIF
    
    * return the length of bytes / characters
    RETURN (INT) Result
    
    END FUNCTION

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/filetostr/*" />
FUNCTION FileToStr (cFileName AS STRING) AS STRING
    RETURN FileToStr(cFileName, 0)
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/filetostr/*" />
FUNCTION FileToStr (cFileName AS STRING, Flags AS INT) AS STRING 
    LOCAL FHandle AS IntPtr
    LOCAL StrLen AS DWORD
    LOCAL Result := "" AS STRING
    LOCAL IOError AS Exception
    
    IF _AND(Flags, S2F_FLAG_UNICODE_TEXT) == 0      // VFP behavior, read file as binary, even if it is a Unicode text
            
        IF File(cFileName)
            cFileName := FPathName()
        ENDIF
        FHandle := FOpen(cFileName, FO_READ + FO_SHARED)
        IF FHandle != F_ERROR
                * success opening the file, get its size...
                StrLen := (DWORD) FSize(FHandle)
                IF StrLen > 0
                    * ...and allocate a buffer for it
                    Result := Space(StrLen)
                    IF FRead(FHandle, @Result, StrLen) != StrLen
                        Result := ""
                    ENDIF
                ELSE
                    Result := ""
                ENDIF
                * any IO error will throw an exception
                IF FError() != 0
                        IOError := FException()
                        FClose(FHandle)
                    THROW IOError
                ELSE
                    FClose(FHandle)
            ENDIF
        ELSE
            THROW FException()
        ENDIF
        
    ELSE
        
        * for a Unicode text file, just read it into a string 
        TRY
            Result := File.ReadAllText(cFileName)     // read a text file
        CATCH
            THROW
        END TRY
        
    ENDIF
    
    * return the contents of the file
    RETURN Result
    
    END FUNCTION
