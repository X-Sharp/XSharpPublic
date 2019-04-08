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
USING System.Linq

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
        INTERNAL _oRDD      AS DBFCDX
        INTERNAL _root      AS CdxFileHeader
        INTERNAL _tagList   AS CdxTagList
        INTERNAL CONST CDXPAGE_SIZE        := 512 AS WORD

        //INTERNAL PROPERTY FileName AS STRING AUTO
        INTERNAL PROPERTY FullPath AS STRING AUTO
        INTERNAL PROPERTY Name AS STRING GET Path.GetFileNameWithoutExtension(FullPath)
        INTERNAL PROPERTY Handle AS IntPtr GET _hFile
        INTERNAL PROPERTY Tags AS IList<CdxTag> GET _tagList:Tags
        INTERNAL PROPERTY Structural AS LOGIC AUTO
        INTERNAL PROPERTY Root      AS CdxFileHeader GET _root
        INTERNAL PROPERTY Encoding AS System.Text.Encoding GET _oRDD:_Encoding
        INTERNAL CONSTRUCTOR(oRDD AS DBFCDX )
            SUPER( oRdd )
            SELF:_oRdd     := oRDD
            SELF:_PageList := CdxPageList{SELF}
            SELF:_OpenInfo := oRDD:_OpenInfo
            
        #region RDD Overloads
            /// <inheritdoc />		
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            THROW NotImplementedException{}

        internal static method GetIndexExtFromDbfExt(cDbfName as STRING) AS STRING
            switch System.IO.Path.GetExtension(cDbfName:ToLower())
            case ".vcx"         // Control Library
            case ".scx"         // Screen
            case ".pjx"         // Project
            case ".mnx"         // Menu
            case ".frx"         // Report
                return ""
            case ".dbc"         // database container
                return ".dcx"
            end switch
            return CDX_EXTENSION
            /// <inheritdoc />
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
            LOCAL cTag AS STRING
            IF info:Order IS STRING
                cTag := (STRING) info:Order
            ELSE
                cTag := Path.GetFileNameWithoutExtension(info:BagName)
                info:Order := cTag
            ENDIF
            IF String.IsNullOrEmpty(cTag)
                // Exception ?
                RETURN FALSE 
            ENDIF
            VAR oTag := SELF:_FindTagByName(cTag)
            IF oTag != NULL_OBJECT
                SELF:_tagList:Remove(oTag)
            ENDIF
            oTag := CdxTag{SELF}
            VAR lOk := oTag:Create(info)
            IF lOk
                // Read the tag from disk to get all the normal stuff
                oTag  := CdxTag{SELF,oTag:Header:PageNo, oTag:OrderName}
                SELF:AddTag(oTag)
                
            ENDIF
            RETURN lOk

        METHOD AddTag(oTag AS CdxTag) AS LOGIC
            _tagList:Add(oTag)
            RETURN TRUE

        METHOD Destroy(oParam AS CdxTag) AS LOGIC
            LOCAL found := FALSE AS LOGIC
            VAR aTags   := SELF:Tags:ToArray()
            FOREACH tag AS CdxTag IN aTags
                IF String.Compare(tag:OrderName, oParam:OrderName, StringComparison.OrdinalIgnoreCase) == 0
                    oParam := tag
                    found := TRUE
                    EXIT
                ENDIF
            NEXT
            IF found
                // rewrite the taglist
                SELF:_tagList:Remove(oParam)
                // We do not Delete the pages from the tag. Comix also does not do that
            ENDIF
            RETURN found

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
            LOCAL aTags AS CdxTag[]
            LOCAL cBagName AS STRING
            LOCAL lOk AS LOGIC
            aTags := SELF:_tagList:Tags:ToArray()
            cBagName := SELF:FullPath
            lOk := TRUE
            lOk := SELF:Close()
            IF lOk
                FErase(cBagName)
                lOk := SELF:CreateBag(cBagName)
            ENDIF
            IF lOk
                FOREACH oTag AS CdxTag IN aTags
                    lOk := oTag:Rebuild()
                    IF ! lOk
                        EXIT
                    ENDIF
                    SELF:AddTag(oTag)
                NEXT
            ENDIF
            SELF:GoCold()
            RETURN lOk


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
        METHOD CreateBag(cBagName AS STRING) AS LOGIC
            LOCAL cPath     AS STRING
            LOCAL cFullName AS STRING
            LOCAL cDbf      AS STRING
            cFullName := cBagName
            cDbf      := SELF:_oRDD:_FileName
            var cExt  := GetIndexExtFromDbfExt(cDBF)
            IF String.IsNullOrEmpty(cFullName)
                cFullName := Path.ChangeExtension(cDBF, cExt)
            ELSEIF String.IsNullOrEmpty(Path.GetExtension(cFullName))
                cFullName := Path.ChangeExtension(cFullname, cExt)
            ELSEIF String.Compare(cDbf, cFullName, StringComparison.OrdinalIgnoreCase) == 0
                 cFullName := Path.ChangeExtension(cFullname, cExt)
            ENDIF
            cPath := Path.GetDirectoryName(cFullName)
            IF String.IsNullOrEmpty(cPath)
                cPath := Path.GetDirectoryName(cDbf)
                cFullName := Path.Combine(cPath, cFullName)
            ENDIF
            IF File(cFullName)
                RETURN FALSE
            ENDIF
            SELF:FullPath := cFullName
            SELF:_hFile    := FCreate(cFullName)
            // Allocate Root Page
            _root   := CdxFileHeader{SELF}
            _root:Initialize()
            _pageList:Add(_root)
            SELF:Write(_root)
            // taglist page
            VAR buffer := SELF:AllocBuffer()
            _tagList := CdxTagList{SELF,  _root:RootPage, buffer, _root:KeySize}
            _taglist:InitBlank(NULL)
            SELF:Write(_tagList)
            // we now have a 
            RETURN TRUE

        METHOD _FindTagByName(cName AS STRING) AS CdxTag
            FOREACH oTag AS CdxTag IN SELF:Tags
                IF String.Compare(oTag:OrderName, cName,StringComparison.OrdinalIgnoreCase) == 0
                    RETURN oTag
                ENDIF
            NEXT
            RETURN NULL


        METHOD Close() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all tags even if one fails
            lOk := TRUE
            FOREACH tag AS CdxTag IN Tags
                IF ! tag:Close()
                    lOk := FALSE
                ENDIF
            NEXT
            IF ! SELF:_PageList:Flush(FALSE)
                lOk := FALSE
            ENDIF
            IF ! FClose(SELF:_hFile)
                lOk := FALSE
            ENDIF
            RETURN lOk


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
                var cExt  := GetIndexExtFromDbfExt(cFullName)
                cFullName := Path.ChangeExtension(cFullname, cExt)
            ENDIF
            cPath := Path.GetDirectoryName(cFullName)
            IF String.IsNullOrEmpty(cPath)
                cPath := SELF:_oRDD:_FileName
                cPath := Path.GetDirectoryName(cPath)
                cFullName := Path.Combine(cPath, cFileName)
            ENDIF
            IF !File(cFullName)
                RETURN FALSE
            ENDIF
            SELF:FullPath := cFullName
            SELF:_openInfo := _oRdd:_OpenInfo
            SELF:_hFile    := FOpen(cFullName, _openInfo:FileMode)
            SELF:_Encoding := _oRdd:_Encoding
            IF SELF:_hFile == F_ERROR
                RETURN FALSE
            ENDIF
            _root := CdxFileHeader{SELF}
            _root:Read()
            SELF:SetPage(_root)
            VAR nTagList := SELF:_root:RootPage
            VAR page     := SELF:GetPage(nTagList, _root:KeySize, NULL)
            _tagList := CdxTagList{SELF,  page, _root:KeySize}
            SELF:SetPage(_tagList)
            _tagList:ReadTags()
            // Compile expressions
            FOREACH VAR tag IN Tags
                tag:EvaluateExpressions()
            NEXT
            RETURN TRUE
        #endregion


         METHOD MatchesFileName(cFileName AS STRING) AS LOGIC
            VAR cExt := Path.GetExtension(cFileName)
            IF String.IsNullOrEmpty(cExt)
                cFileName := Path.ChangeExtension(cFileName, CDX_EXTENSION)
            ENDIF
            IF string.Compare(SELF:FullPath, cFileName, StringComparison.OrdinalIgnoreCase) == 0
                RETURN TRUE
            ENDIF
            VAR cPath := Path.GetDirectoryName(cFileName)
            IF string.IsNullOrEmpty(cPath)
                IF String.Compare(Path.GetFileName(FullPath), cFileName, StringComparison.OrdinalIgnoreCase) == 0
                    RETURN TRUE
                ENDIF
            ENDIF
            RETURN FALSE


         METHOD Flush() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all tags even if one fails
            lOk := TRUE
            FOREACH tag AS CdxTag IN SELF:Tags
                IF ! tag:Flush()
                    lOk := FALSE
                ENDIF
            NEXT
            SELF:FlushPages()
            RETURN lOk

         INTERNAL METHOD FlushPages() AS LOGIC
            RETURN SELF:_PageList:Flush(FALSE)

         METHOD GoCold() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all tags even if one fails
            lOk := TRUE
            FOREACH tag AS CdxTag IN SELF:Tags
                IF ! tag:GoCold()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        METHOD GoHot() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all tags even if one fails
            lOk := TRUE
            FOREACH VAR tag IN SELF:Tags
                IF ! tag:GoHot()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        METHOD FindFreePage() AS LONG
            LOCAL nPage AS LONG
            LOCAL nNext AS LONG
            IF SELF:_root:FreeList != 0
                nPage := SELF:_root:FreeList
                VAR oPage := SELF:_PageList:GetPage(nPage, 0, NULL)
                IF oPage IS CdxTreePage
                    VAR tpage := (CdxTreePage) oPage
                    nNext := tpage:NextFree
                    IF nNext == -1
                        nNext := 0
                    ENDIF
                    SELF:_root:FreeList := nNext
                    SELF:_root:Write()
                ENDIF
                SELF:_PageList:Delete(nPage)
            ELSE
                nPage :=  FSeek3( SELF:_hFile, 0, SeekOrigin.End )
            ENDIF
            RETURN nPage

         METHOD FreePage(oPage AS CdxTreePage) AS LOGIC
            oPage:Clear()
            oPage:NextFree     := SELF:_root:FreeList
            SELF:_root:FreeList := oPage:PageNo
            SELF:_root:Write()
            SELF:_PageList:Delete(oPage:PageNo)
            oPage:Write()
            RETURN TRUE

        METHOD AllocBuffer(nSize := 1 AS LONG)  AS BYTE[]
            RETURN BYTE[]{CDXPAGE_SIZE *nSize}

        METHOD Read(nPage AS LONG, buffer AS BYTE[]) AS LOGIC
            LOCAL isOk AS LOGIC
			FSeek3( SELF:_hFile, nPage, SeekOrigin.Begin )
			isOk :=  FRead3(SELF:_hFile, buffer, (DWORD) Buffer:Length) == (DWORD) Buffer:Length 
            RETURN IsOk
 
        METHOD Read(oPage AS CdxPage) AS LOGIC
            LOCAL isOk AS LOGIC
			FSeek3( SELF:_hFile, oPage:PageNo, SeekOrigin.Begin )
			isOk :=  FRead3(SELF:_hFile, oPage:Buffer, (DWORD) oPage:Buffer:Length) == (DWORD) oPage:Buffer:Length 
            RETURN IsOk

        METHOD Write(oPage AS CdxPage) AS LOGIC
            LOCAL isOk AS LOGIC
            IF oPage:PageNo == -1
                oPage:PageNo := SELF:FindFreePage()
            ENDIF
            FSeek3( SELF:_hFile, oPage:PageNo, SeekOrigin.Begin )
			isOk :=  FWrite3(SELF:_hFile, oPage:Buffer, (DWORD) oPage:Buffer:Length) == (DWORD) oPage:Buffer:Length
            RETURN IsOk



        METHOD GetPage(nPage AS Int32, nKeyLen AS WORD,tag AS CdxTag) AS CdxPage
           VAR page := SELF:_PageList:GetPage(nPage, nKeyLen,tag)
           IF page != NULL
                SELF:_PageList:Add(page)
           ENDIF
           RETURN page
  
         METHOD SetPage(page AS CdxPage) AS VOID
            SELF:_PageList:SetPage(Page:PageNo, page)

        #region properties

        PROPERTY Count AS LONG GET SELF:Tags:Count

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
            // return true as soon as one tag is hot
            FOREACH oTag AS CdxTag IN SELF:Tags
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
