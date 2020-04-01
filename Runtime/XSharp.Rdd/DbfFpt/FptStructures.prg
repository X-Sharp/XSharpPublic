

//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING System.Runtime.InteropServices
USING System.IO
BEGIN NAMESPACE XSharp.RDD
    INTERNAL STRUCTURE FtpMemoToken
        PRIVATE Buffer AS BYTE[]
        
        INTERNAL CONSTRUCTOR(bData AS BYTE[])
            Buffer := bData
            
        INTERNAL PROPERTY DataType AS FlexFieldType
            GET
                RETURN (FlexFieldType) FoxToLong(Buffer, 0)
            END GET
            SET
                LongToFox((LONG) value, Buffer, 0)
            END SET
        END PROPERTY
        
        /// This includes the length of the token
        INTERNAL PROPERTY Length AS DWORD       
            GET
                RETURN FoxToDword(Buffer, 4)
            END GET
            SET
                DWordToFox(value, Buffer, 4)
            END SET
        END PROPERTY
        
        INTERNAL METHOD Clear AS VOID
            SELF:DataType := FlexFieldType.Illegal
            SELF:Length   := 0
            RETURN
            
        INTERNAL METHOD Write(hFile AS IntPtr) AS LOGIC
            TRY
                RETURN FWrite3(hFile, Buffer, 8) == 8
            CATCH AS IOException
                RETURN FALSE    
            END TRY
            
            
        INTERNAL METHOD Read(hFile AS IntPtr) AS LOGIC
            LOCAL lOk AS LOGIC
            TRY
                lOk := FRead3(hFile, Buffer, 8) == 8
                IF lOk
                    // Check for 'expected' Field Types
                    SWITCH SELF:DataType
                        CASE FlexFieldType.String
                        CASE FlexFieldType.Picture
                        CASE FlexFieldType.OleObject
                    CASE FlexFieldType.Delete
                        lOk := TRUE
                    OTHERWISE
                            IF SELF:DataType >= FlexFieldType.FirstExtended .AND. ;
                                SELF:DataType <= FlexFieldType.LastExtended
                                lOk := TRUE
                            ELSEIF SELF:DataType >= FlexFieldType.FirstExtended2 .AND. ;
                                SELF:DataType <= FlexFieldType.LastExtended2
                                lOk := TRUE
                            ELSE
                                lOk := FALSE
                        ENDIF
                    END SWITCH
                ENDIF
            CATCH AS IOException
                lOk := FALSE
            END TRY
            IF ! lOk
                SELF:DataType := FlexFieldType.Illegal
                SELF:Length   := UInt32.MaxValue
            ENDIF
            RETURN lOk
            
            
    END STRUCTURE
END NAMESPACE
