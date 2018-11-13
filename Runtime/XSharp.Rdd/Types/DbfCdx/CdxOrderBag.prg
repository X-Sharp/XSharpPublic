//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Globalization
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Text

BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// Orderbag = CDX file. Contains one or more orders = Tags
    /// </summary>
    INTERNAL CLASS CdxOrderBag INHERIT BaseIndex 
        INTERNAL _hFile     AS IntPtr
        INTERNAL _Encoding  AS Encoding
        INTERNAL _Shared    AS LOGIC
        INTERNAL _ReadOnly  AS LOGIC
        INTERNAL _Hot       AS LOGIC
        INTERNAL _oRDD      as DBFCDX
        INTERNAL _root      as CdxFileHeader
        INTERNAL _tagList   as CdxTagList

        INTERNAL PROPERTY FileName as STRING AUTO
        
        INTERNAL CONSTRUCTOR(oRDD AS DBFCDX )
            SUPER( oRdd )
            SELF:_oRdd := oRDD
            
        #region RDD Overloads
            /// <inheritdoc />		
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderInfo(nOrdinal AS DWORD) AS OBJECT
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD OrderListRebuild( ) AS LOGIC
            THROW NotImplementedException{}
            /// <inheritdoc />
        METHOD Seek(info AS DbSeekInfo) AS LOGIC		
            THROW NotImplementedException{}
            /// <inheritdoc />
        VIRTUAL PROPERTY Found AS LOGIC	
            GET
                THROW NotImplementedException{}
            END GET
        END PROPERTY
        #endregion
        #region Open and Close etc
        INTERNAL METHOD Close() as LOGIC
            RETURN TRUE
        INTERNAL METHOD Open(info AS XSharp.RDD.Support.DbOpenInfo) as LOGIC
            IF !File(info:FileName)
                return FALSE
            ENDIF
            SELF:_hFile    := FOpen(info:FileName, info:FileMode)
            if SELF:_hFile == F_ERROR
                return FALSE
            endif
            SELF:FileName := info:FileName
            _root := CdxFileHeader{SELF:_hFile}
            IF _root:Read()
                local nTagList as DWORD
                nTagList := SELF:_root:TagList
                _tagList := CdxTagList{_hFile, (int) nTagList, _root:KeyLength}
                _tagList:Read()
                // Compile expressions
                FOREACH var tag in _tagList:Tags
                    LOCAL nIndex as INT
                    nIndex := _oRdd:FieldIndex(tag:KeyExpression)
                    IF  nIndex > 0
                        tag:SingleField := TRUE
                        tag:fieldIndex := nIndex 
                    ELSE
//                        VAR oBlock := _oRDD:Compile(tag:KeyExpression)
//                        tag:KeyBlock := oBlock
//                        var oValue := _oRdd:EvalBlock(oBlock)
//                        tag:KeyType := Type.GetTypeCode(oValue:GetType())
//                        if !String.IsNullOrEmpty(tag:ForExpression)
//                            tag:KeyBlock := _oRDD:Compile(tag:ForExpression)
//                            oValue := _oRdd:EvalBlock(tag:KeyBlock)
//                            // should be logical
//                            if Type.GetTypeCode(oValue:GetType()) != typeCode.Boolean
//                                // raise an error
//                            endif
//                        ENDIF
                    ENDIF
                NEXT
            ENDIF
            RETURN TRUE
            

        #endregion
    END CLASS
END NAMESPACE
