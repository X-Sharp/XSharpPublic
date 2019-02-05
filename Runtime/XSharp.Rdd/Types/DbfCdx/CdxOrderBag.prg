//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Globalization
USING System.Collections.Generic
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Text
USING System.IO
BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// Orderbag = CDX file. Contains one or more orders = Tags
    /// </summary>
    INTERNAL CLASS CdxOrderBag INHERIT BaseIndex 
        INTERNAL _hFile     AS IntPtr
        INTERNAL _OpenInfo	AS DbOpenInfo
        INTERNAL _Encoding  AS Encoding
        INTERNAL PROPERTY Shared    AS LOGIC GET _OpenInfo:Shared
        INTERNAL PROPERTY ReadOnly  AS LOGIC GET _OpenInfo:ReadOnly
        INTERNAL _Hot       AS LOGIC
        INTERNAL _oRDD      AS DBFCDX
        INTERNAL _root      AS CdxFileHeader
        INTERNAL _tagList   AS CdxTagList
        INTERNAL _tags      AS IList<CdxTag>
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD

        INTERNAL PROPERTY FileName AS STRING AUTO
        INTERNAL PROPERTY Handle AS IntPtr GET _hFile
        INTERNAL PROPERTY Tags AS IList<CdxTag> GET _tags
        INTERNAL PROPERTY Name AS STRING AUTO
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
        METHOD Close() AS LOGIC
            FOREACH oTag AS CdxTag IN _tags
                oTag:GoCold()
            NEXT
            FClose(SELF:_hFile)
            RETURN TRUE

        INTERNAL METHOD Open(info AS XSharp.RDD.Support.DbOrderInfo) AS LOGIC
            IF !File(info:BagName)
                RETURN FALSE
            ENDIF
            SELF:_openInfo := _oRdd:_OpenInfo
            SELF:_hFile    := FOpen(info:BagName, _openInfo:FileMode)
            SELF:_Encoding := _oRdd:_Encoding
            IF SELF:_hFile == F_ERROR
                RETURN FALSE
            ENDIF
            SELF:FileName := info:BagName
            SELF:Name     := System.IO.Path.GetFileNameWithoutExtension(SELF:FileName)
            LOCAL buffer AS BYTE[]
            buffer := BYTE[]{CDXPAGE_SIZE}
            _root := CdxFileHeader{SELF, buffer}
            IF _root:Read()
                LOCAL nTagList AS Int32
                nTagList := SELF:_root:TagList
                buffer := BYTE[]{CDXPAGE_SIZE}
                _tagList := CdxTagList{SELF,  nTagList, buffer, _root:KeyLength}
                _tagList:Read()
                _tags := _tagList:Tags
                // Compile expressions
                FOREACH VAR tag IN _tags
                    LOCAL nIndex AS INT
                    nIndex := _oRdd:FieldIndex(tag:KeyExpression)
                    LOCAL oVal AS OBJECT
                    IF  nIndex > 0
                        tag:SingleField := TRUE
                        tag:fieldIndex := nIndex
                        oVal := _oRDD:GetValue(nIndex)
                    ELSE
                        VAR oBlock := _oRDD:Compile(tag:KeyExpression)
                        tag:KeyBlock := oBlock
                        oVal := _oRdd:EvalBlock(oBlock)
                    ENDIF
                    IF oVal IS System.String
                        tag:KeyType := __UsualType.STRING
                    ELSEIF oVal IS DbDate 
                        tag:KeyType := __UsualType.DATE   
                    ELSEIF oVal IS DbFloat 
                        tag:KeyType := __UsualType.FLOAT   
                    ELSEIF oVal IS LOGIC
                        tag:KeyType := __UsualType.LOGIC   
                    ELSE
                        tag:KeyType := 0
                        // Throw exception unknown key type ?
                    ENDIF
                    IF !String.IsNullOrEmpty(tag:ForExpression)
                        tag:ForBlock := _oRDD:Compile(tag:ForExpression)
                        oVal := _oRdd:EvalBlock(tag:KeyBlock)
                        // should be logical
                        IF ! oVal IS LOGIC
                            // raise an error
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
            RETURN TRUE
        #endregion

         METHOD GoCold() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
             FOREACH oTag AS CdxTag IN _tags
                IF ! oTag:GoCold()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        METHOD GetPage(nPage AS Int32, nKeyLen := 0 AS Int32) AS CdxPage
         	LOCAL isOk AS LOGIC
            LOCAL buffer AS BYTE[]
            buffer := BYTE[]{CDXPAGE_SIZE}
			// Move to top
			FSeek3( SELF:_hFile, nPage, SeekOrigin.Begin )
			// Read Buffer
			isOk := FRead3(SELF:_hFile, buffer, CDXPAGE_SIZE) == CDXPAGE_SIZE 
			//
            // Inspect first 2 byte and determine the page
            LOCAL nType AS SHORT
            nType := BitConverter.ToInt16(buffer, 0)
            SWITCH nType
            CASE 1  // Root
                RETURN CdxGeneralPage{SELF, nPage, buffer} 
            CASE 2  // Leaf
                RETURN CdxLeafPage{SELF, nPage, buffer, nKeyLen}
            CASE 3  // List of Tags
                RETURN CdxLeafPage{SELF, nPage, buffer, nKeyLen}
            CASE 0
                RETURN CdxBranchePage{SELF, nPage, buffer,nKeyLen}
            CASE -1 // Unused
            OTHERWISE // Could be tag header
               RETURN CdxGeneralPage{SELF, nPage, buffer} 
            END SWITCH
            

        METHOD Read(oPage AS CdxPage) AS LOGIC
            LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, oPage:PageNo, SeekOrigin.Begin )
			// Write Buffer
			isOk :=  FRead3(SELF:_hFile, oPage:Buffer, CDXPAGE_SIZE) == CDXPAGE_SIZE 
            RETURN IsOk

        METHOD Write(oPage AS CdxPage) AS LOGIC
            LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, oPage:PageNo, SeekOrigin.Begin )
			// Write Buffer
			isOk :=  FWrite3(SELF:_hFile, oPage:Buffer, CDXPAGE_SIZE) == CDXPAGE_SIZE 
            RETURN IsOk

        METHOD GoHot() AS LOGIC
            // Todo
            _Hot := TRUE
            RETURN TRUE

  

        #region properties

        PROPERTY Count AS LONG GET _tags:Count



        PROPERTY IsHot AS LOGIC
        GET
            FOREACH oTag AS CdxTag IN _tags
                IF oTag:IsHot
                    RETURN TRUE
                ENDIF
            NEXT
            RETURN FALSE
        END GET
        END PROPERTY
        #endregion



    END CLASS
END NAMESPACE
