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
#region constants
    PRIVATE CONST ComixXLockOfs		:= 0xfffeffffL AS LONG
    PRIVATE CONST ComixSLockOfs		:= 0xffff0000L AS LONG
    PRIVATE CONST ComixXLockLen		:= 0x00010000L AS LONG
    PRIVATE CONST FoxXLockOfs		:= 0x7ffffffeL AS LONG
    PRIVATE CONST FoxXLockLen		:= 1           AS LONG
    PRIVATE CONST FoxSLockOfs		:= 0x7ffffffeL AS LONG
    PRIVATE CONST FoxSLockLen		:= 1 AS LONG
    INTERNAL CONST CDX_EXTENSION := ".CDX" AS STRING

#endregion
        INTERNAL _hFile     AS IntPtr
        INTERNAL _OpenInfo	AS DbOpenInfo
        INTERNAL _Encoding  AS Encoding
        INTERNAL _PageList  AS CdxPageList
        INTERNAL PROPERTY Shared    AS LOGIC GET _OpenInfo:Shared
        INTERNAL PROPERTY ReadOnly  AS LOGIC GET _OpenInfo:ReadOnly
        INTERNAL _Hot       AS LOGIC
        INTERNAL _oRDD      AS DBFCDX
        INTERNAL _root      AS CdxFileHeader
        INTERNAL _tagList   AS CdxTagList
        INTERNAL _tags      AS IList<CdxTag>
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD

        INTERNAL PROPERTY FileName AS STRING AUTO
        INTERNAL PROPERTY Name AS STRING GET Path.GetFileNameWithoutExtension(FileName)
        INTERNAL PROPERTY FullPath AS STRING AUTO
        INTERNAL PROPERTY Handle AS IntPtr GET _hFile
        INTERNAL PROPERTY Tags AS IList<CdxTag> GET _tags
        INTERNAL CONSTRUCTOR(oRDD AS DBFCDX )
            SUPER( oRdd )
            SELF:_oRdd     := oRDD
            SELF:_PageList := CdxPageList{SELF}
            
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

        INTERNAL METHOD Open(info AS DbOrderInfo) AS LOGIC
            // Filename may have path or not
            // When the filename does not have a path then we look in the path of the DBF
            LOCAL cPath     AS STRING
            LOCAL cFileName AS STRING
            LOCAL cFullName AS STRING
            IF info == NULL
                RETURN FALSE
            ENDIF
            cFullName := cFileName := info:BagName
            IF String.IsNullOrEmpty(Path.GetExtension(cFullName))
                cFullName := Path.ChangeExtension(cFullname, CDX_EXTENSION)
            ENDIF
            cPath := Path.GetDirectoryName(cFullName)
            IF String.IsNullOrEmpty(cPath)
                cPath := (STRING) SELF:_oRDD:Info(DBI_FULLPATH,NULL)
                cPath := Path.GetDirectoryName(cPath)
                cFullName := Path.Combine(cPath, cFileName)
            ENDIF
            IF !File(cFullName)
                RETURN FALSE
            ENDIF
            SELF:FullPath := cFullName
            SELF:FileName := cFileName
            SELF:_openInfo := _oRdd:_OpenInfo
            SELF:_hFile    := FOpen(cFullName, _openInfo:FileMode)
            SELF:_Encoding := _oRdd:_Encoding
            IF SELF:_hFile == F_ERROR
                RETURN FALSE
            ENDIF
            SELF:FileName := info:BagName
            VAR page := SELF:GetPage(0, 0, NULL)
            _root := CdxFileHeader{SELF, page}
            SELF:SetPage(_root)
            LOCAL nTagList AS Int32
            nTagList := SELF:_root:TagList
            page     := SELF:GetPage(nTagList, _root:KeyLength, NULL)
            _tagList := CdxTagList{SELF,  page}
            SELF:SetPage(_tagList)
            _tags := _tagList:ReadTags()
            // Compile expressions
            FOREACH VAR tag IN _tags
                tag:EvaluateExpressions()
            NEXT
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

        METHOD AllocBuffer AS BYTE[]
            RETURN BYTE[]{CDXPAGE_SIZE}

        METHOD Read(nPage AS LONG, buffer AS BYTE[]) AS LOGIC
            LOCAL isOk AS LOGIC
			// Move to top
			FSeek3( SELF:_hFile, nPage, SeekOrigin.Begin )
			// Write Buffer
			isOk :=  FRead3(SELF:_hFile, buffer, CDXPAGE_SIZE) == CDXPAGE_SIZE 
            RETURN IsOk
 
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

        METHOD GetPage(nPage AS Int32, nKeyLen AS WORD,tag AS CdxTag) AS CdxPage
           RETURN SELF:_PageList:GetPage(nPage, nKeyLen,tag)
  
         METHOD SetPage(page AS CdxPage) AS VOID
            SELF:_PageList:SetPage(Page:PageNo, page)

        #region properties

        PROPERTY Count AS LONG GET _tags:Count

        PROPERTY BagHasChanged AS LOGIC 
            GET
                LOCAL nVersion AS DWORD
                LOCAL lChanged AS LOGIC
                nVersion := _root:Version
                SELF:Read(SELF:_root)
                lChanged := (nVersion != _root:Version)
                IF lChanged
                    // we 
                ENDIF
                RETURN lChanged


            END GET
        END PROPERTY

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

#region Locking
        PRIVATE _sharedLocks    := 0 AS LONG
        PRIVATE _exclusiveLocks := 0 AS LONG
        PRIVATE _useFoxLock     := FALSE AS LOGIC
        //PRIVATE _sLockGate      := 0 AS LONG
        //PRIVATE _sLockOffSet    := 0 AS DWORD
        PRIVATE _xLockedInOne   := FALSE AS LOGIC
        INTERNAL _LockOffSet    := 0 AS LONG

        INTERNAL METHOD SLock() AS LOGIC
            IF !SELF:Shared
                RETURN TRUE
            ENDIF
            IF _useFoxLock
                NOP
            ELSE
                NOP
            ENDIF
            RETURN TRUE
        INTERNAL METHOD XLock() AS LOGIC
            IF !SELF:Shared
                RETURN TRUE
            ENDIF
            IF _useFoxLock
                NOP
            ELSE
                NOP
            ENDIF
            RETURN TRUE

        INTERNAL METHOD UnLock() AS VOID
            IF !SELF:Shared
                RETURN
            ENDIF
            IF _useFoxLock
                IF _sharedLocks > 0
                    _sharedLocks -= 1
                   IF _sharedLocks == 0
                        // Unlock at the fox offset for shared
                        NOP
                   ENDIF
                ELSEIF _exclusiveLocks > 0
                    _exclusiveLocks -= 1
                    IF _exclusiveLocks == 0
                       // update Bag header version
                       // write Bag Header
                       NOP
                    ENDIF
                ENDIF
            ELSE
                IF _sharedLocks > 0
                    _sharedLocks -= 1
                   IF _sharedLocks == 0
                        // Unlock at the comix offset for shared
                   ENDIF
                ELSEIF _exclusiveLocks > 0
                    _exclusiveLocks -= 1
                    IF _exclusiveLocks == 0
                        // update Bag header version
                        // write Bag Header
                       IF _xLockedInOne
                        // special unlocking for comix
                            NOP
                       ELSE
                        NOP
                       ENDIF
                    ENDIF
                ELSE
                ENDIF
            ENDIF
            RETURN
#endregion


    END CLASS
END NAMESPACE
