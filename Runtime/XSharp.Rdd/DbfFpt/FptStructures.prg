

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
USING STATIC XSharp.Conversions
BEGIN NAMESPACE XSharp.RDD
    INTERNAL STRUCTURE FlexMemoToken
        PRIVATE _Buffer AS BYTE[]
        PRIVATE _Stream as FileStream
        INTERNAL CONST TokenLength := 8 AS LONG

        INTERNAL CONSTRUCTOR(buffer as BYTE[], oStream as FileStream)
            _Stream := oStream
            if (buffer == NULL)
                buffer := BYTE[]{TokenLength}
            endif
            _Buffer   := buffer

        INTERNAL PROPERTY DataType AS FlexFieldType
            GET
                RETURN (FlexFieldType) BuffToLongFox(_Buffer, 0)
            END GET
            SET
                LongToBuffFox((LONG) value, _Buffer, 0)
            END SET
        END PROPERTY

        /// This includes the length of the token
        INTERNAL PROPERTY Length AS LONG
            GET
                RETURN BuffToLongFox(_Buffer, 4)
            END GET
            SET
                LongToBuffFox(VALUE, _Buffer, 4)
            END SET
        END PROPERTY

        INTERNAL METHOD Clear AS VOID
            SELF:DataType := FlexFieldType.Illegal
            SELF:Length   := 0
            RETURN
        INTERNAL METHOD Write() AS LOGIC
            RETURN _Stream:SafeWrite(_Buffer, TokenLength)

        INTERNAL METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            TRY
                lOk := _Stream:SafeRead(_Buffer, TokenLength)
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
                SELF:Length   := Int32.MaxValue
            ENDIF
            RETURN lOk


    END STRUCTURE
END NAMESPACE
