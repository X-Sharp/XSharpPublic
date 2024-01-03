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
USING System.Diagnostics
//#define CHECKVERSIONS

BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// Orderbag = CDX file. Contains one or more orders = Tags
    /// </summary>
    [DebuggerDisplay("{FullPath,nq} {Tags.Count} tags")];
    INTERNAL SEALED CLASS CdxOrderBag INHERIT BaseIndex
#region constants
    INTERNAL CONST CDX_EXTENSION := ".CDX" AS STRING
    INTERNAL CONST MAX_TAGNAME_LEN := 10 AS LONG

#endregion
        PRIVATE  _newPageAllocated := FALSE AS LOGIC
        INTERNAL _hFile     AS IntPtr
        INTERNAL _stream    AS FileStream
        INTERNAL _OpenInfo	AS DbOpenInfo
        INTERNAL _Encoding  AS Encoding
        INTERNAL _PageList  AS CdxPageList
        INTERNAL PROPERTY Shared    AS LOGIC GET _OpenInfo:Shared
        INTERNAL PROPERTY ReadOnly  AS LOGIC GET _OpenInfo:ReadOnly
        INTERNAL _oRdd              AS DBFCDX
        INTERNAL _root              AS CdxFileHeader
        INTERNAL _tagList           AS CdxTagList
        INTERNAL CONST CDXPAGE_SIZE := 512 AS WORD

        //INTERNAL PROPERTY FileName AS STRING AUTO
        INTERNAL PROPERTY FullPath AS STRING AUTO
        INTERNAL PROPERTY Name AS STRING GET Path.GetFileNameWithoutExtension(FullPath)
        INTERNAL PROPERTY Handle AS IntPtr GET _hFile
        INTERNAL PROPERTY Stream AS FileStream GET _stream
        INTERNAL PROPERTY Tags AS IList<CdxTag> GET IIF(_tagList == NULL, NULL, _tagList:Tags)
        INTERNAL PROPERTY TagList AS CdxTagList GET _tagList
        INTERNAL PROPERTY Structural AS LOGIC AUTO
        INTERNAL PROPERTY Root      AS CdxFileHeader GET _root
        INTERNAL PROPERTY Encoding AS System.Text.Encoding GET _oRdd:_Encoding
        INTERNAL CONSTRUCTOR(oRDD AS DBFCDX )
            SUPER( oRDD )
            SELF:_oRdd     := oRDD
            SELF:_PageList := CdxPageList{SELF}
            SELF:_OpenInfo := oRDD:_OpenInfo
            SELF:_useFoxLock := XSharp.RuntimeState.GetValue<LOGIC>(Set.FoxLock)

        #region RDD Overloads
            /// <inheritdoc />
        OVERRIDE METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderCondition(info)

        INTERNAL STATIC METHOD GetIndexExtFromDbfExt(cDbfName AS STRING) AS STRING
            SWITCH System.IO.Path.GetExtension(cDbfName:ToLower())
            CASE ".vcx"         // Control Library
            CASE ".scx"         // Screen
            CASE ".pjx"         // Project
            CASE ".mnx"         // Menu
            CASE ".frx"         // Report
                RETURN ""
            CASE ".dbc"         // database container
                RETURN ".dcx"
            END SWITCH
            RETURN CDX_EXTENSION
            /// <inheritdoc />
        OVERRIDE METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
            LOCAL cTag AS STRING
            IF info:Order IS STRING VAR strOrder .and. ! String.IsNullOrWhiteSpace(strOrder)
                cTag := strOrder
                IF cTag:Length > MAX_TAGNAME_LEN
                    SELF:_oRdd:_dbfError(Subcodes.ERDD_CREATE_ORDER,Gencode.EG_LIMIT,;
                        "DBFCDX.OrderCreate",i"Tag name '{cTag}' is too long. The maximum tag name length is {MAX_TAGNAME_LEN} characters",TRUE)
                ENDIF
            ELSE
                cTag := Path.GetFileNameWithoutExtension(info:BagName)
                if cTag:Length > MAX_TAGNAME_LEN
                    cTag := cTag:Substring(0, MAX_TAGNAME_LEN)
                endif
                info:Order := cTag
            ENDIF
            IF String.IsNullOrEmpty(cTag)
                // Exception ?
                RETURN FALSE
            ENDIF
            VAR oTag := SELF:_FindTagByName(cTag)
            VAR lOk := TRUE
            IF oTag != NULL_OBJECT
                VAR last := SELF:_tagList:Tags:Count == 1
                SELF:Destroy(oTag)
                IF last
                    SELF:CreateBag(SELF:FullPath)
                    SELF:_oRdd:_indexList:_AddBag(SELF)
                ENDIF
            ENDIF
            oTag := CdxTag{SELF}
            lOk := oTag:Create(info)
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
                IF SELF:_tagList:Tags:Count == 0
                    LOCAL cFileName := SELF:FullPath AS STRING
                    SELF:_oRdd:_indexList:_CloseBag(SELF)
                    FErase(cFileName)
                ENDIF
            ENDIF
            RETURN found

            /// <inheritdoc />
        OVERRIDE METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderDestroy(info)
            /// <inheritdoc />
        OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderInfo(nOrdinal, info)
            /// <inheritdoc />
        OVERRIDE METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderListAdd(info)
            /// <inheritdoc />
        OVERRIDE METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderListDelete(info)
            /// <inheritdoc />
        OVERRIDE METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            // Handled at the RDD Level
            RETURN SELF:_oRdd:OrderListFocus(info)
        /// <inheritdoc />

        OVERRIDE METHOD OrderListRebuild( ) AS LOGIC
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
        OVERRIDE METHOD Seek(info AS DbSeekInfo) AS LOGIC
            RETURN SELF:_oRdd:Seek(info)
            /// <inheritdoc />
        OVERRIDE PROPERTY Found AS LOGIC GET SELF:_oRdd:Found
        #endregion

        #region Open and Close etc
        METHOD CreateBag(cBagName AS STRING) AS LOGIC
            LOCAL cPath     AS STRING
            LOCAL cFullName AS STRING
            LOCAL cDbf      AS STRING
            cFullName := cBagName
            cDbf      := SELF:_oRdd:FileName
            VAR cExt  := GetIndexExtFromDbfExt(cDbf)
            IF String.IsNullOrEmpty(cFullName)
                cFullName := Path.ChangeExtension(cDbf, cExt)
            ELSEIF String.IsNullOrEmpty(Path.GetExtension(cFullName))
                cFullName := Path.ChangeExtension(cFullName, cExt)
            ELSEIF String.Compare(cDbf, cFullName, StringComparison.OrdinalIgnoreCase) == 0
                 cFullName := Path.ChangeExtension(cFullName, cExt)
            ENDIF
            cPath := Path.GetDirectoryName(cFullName)
            IF String.IsNullOrEmpty(cPath)
                cPath := Path.GetDirectoryName(cDbf)
                cFullName := Path.Combine(cPath, cFullName)
            ENDIF
            IF File(cFullName)
                LOCAL openInfo AS DbOrderInfo
                openInfo := DbOrderInfo{}
                openInfo:BagName := FPathName()
                SELF:Open(openInfo)
                RETURN TRUE
            ENDIF
            SELF:_hFile    := FCreate(cFullName)
            SELF:_stream   := FGetStream(SELF:_hFile)
            // Make sure the full name is returned and not the DOS Short Name
            IF SELF:_stream != NULL_OBJECT
                SELF:FullPath := SELF:_stream:Name
            ENDIF
            // Allocate Root Page
            _root   := CdxFileHeader{SELF}
            _root:Initialize()
            _PageList:Add(_root)
            SELF:Write(_root)
            // taglist page
            VAR buffer := SELF:AllocBuffer()
            _tagList := CdxTagList{SELF,  _root:RootPage, buffer, _root:KeySize}
            _tagList:InitBlank(NULL)
            SELF:Write(_tagList)
            RETURN TRUE

        METHOD _FindTagByName(cName AS STRING) AS CdxTag
            IF SELF:Tags != NULL
                FOREACH oTag AS CdxTag IN SELF:Tags
                    IF String.Compare(oTag:OrderName, cName,StringComparison.OrdinalIgnoreCase) == 0
                        RETURN oTag
                    ENDIF
                NEXT
            ENDIF
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
            SELF:_stream := NULL
            SELF:_hFile  := F_ERROR
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
                VAR cExt  := GetIndexExtFromDbfExt(cFullName)
                cFullName := Path.ChangeExtension(cFullName, cExt)
            ENDIF
            cPath := Path.GetDirectoryName(cFullName)
            IF String.IsNullOrEmpty(cPath)
                cPath := SELF:_oRdd:FileName
                cPath := Path.GetDirectoryName(cPath)
                cFullName := Path.Combine(cPath, cFileName)
            ENDIF
            IF !File(cFullName)
                RETURN FALSE
            ENDIF
            // Adjust Filename to handle 8 char DOS names
            SELF:_OpenInfo := _oRdd:_OpenInfo
            SELF:_hFile    := FOpen(cFullName, _OpenInfo:FileMode)
            IF SELF:_hFile == F_ERROR
                RETURN FALSE
            ENDIF
            SELF:_stream   := FGetStream(SELF:_hFile)
            IF SELF:_stream != NULL_OBJECT
                SELF:FullPath := SELF:_stream:Name
            ENDIF
            SELF:_Encoding := _oRdd:_Encoding
            _root := CdxFileHeader{SELF}
            _root:Read()
            SELF:SetPage(_root)
            VAR nTagList := SELF:_root:RootPage
            VAR page     := SELF:GetPage(nTagList, _root:KeySize, NULL)
            _tagList := CdxTagList{SELF,  page, _root:KeySize}
            SELF:SetPage(_tagList)
            _tagList:ReadTags()
            // No need to Compile expressions again. Is done in ReadTags already
            // If we do want to do it again, then remember to save the LastRddError !
            // FOREACH VAR tag IN Tags
            //    tag:EvaluateExpressions()
            // NEXT
            RETURN TRUE
        #endregion


         METHOD MatchesFileName(cFileName AS STRING) AS LOGIC
            VAR cExt := Path.GetExtension(cFileName)
            IF String.IsNullOrEmpty(cExt)
                cFileName := Path.ChangeExtension(cFileName, CDX_EXTENSION)
            ENDIF
            IF String.Compare(SELF:FullPath, cFileName, StringComparison.OrdinalIgnoreCase) == 0
                RETURN TRUE
            ENDIF
            VAR cPath := Path.GetDirectoryName(cFileName)
            IF String.IsNullOrEmpty(cPath)
                IF String.Compare(Path.GetFileName(FullPath), cFileName, StringComparison.OrdinalIgnoreCase) == 0
                    RETURN TRUE
                ENDIF
            ENDIF
            RETURN FALSE


         OVERRIDE METHOD Flush() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all tags even if one fails
            lOk := TRUE
            FOREACH tag AS CdxTag IN SELF:Tags
                IF ! tag:Flush()
                    lOk := FALSE
                ENDIF
            NEXT
            lOk := SELF:FlushPages() .and. lOk
            IF XSharp.RuntimeState.GetValue<LOGIC>(Set.HardCommit)
                _stream:Flush(TRUE)
            ELSE
                _stream:Flush(FALSE)
            ENDIF
            RETURN lOk

         INTERNAL METHOD FlushPages() AS LOGIC
            RETURN SELF:_PageList:Flush(TRUE)

         INTERNAL METHOD SavePages() AS LOGIC
            RETURN SELF:_PageList:Flush(TRUE)

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
            IF SELF:_root:FreeList > 0
                nPage := SELF:_root:FreeList
                VAR oPage := SELF:_PageList:GetPage(nPage, 0, NULL)
                IF oPage IS CdxTreePage VAR tPage
                    nNext := tPage:LeftPtr
                    IF nNext < 0
                        nNext := 0
                    ENDIF
                    SELF:_root:FreeList := nNext
                    SELF:_root:Write()
                ENDIF
                SELF:_PageList:Delete(nPage)
            ELSE
                IF _stream:Length >= Int32.MaxValue
                    var cMessage := "Maximum file size of 2 Gb for CDX file exceeded"
                    DebOut32(cMessage)
                    throw IOException{cMessage}
                ENDIF
                nPage   := (LONG) _stream:Length
                _newPageAllocated := TRUE
            ENDIF
            RETURN nPage

         METHOD AddFreePage(oPage AS CdxTreePage) AS LOGIC
            VAR nPage := oPage:PageNo
            oPage:Clear()
            oPage:LeftPtr   := SELF:_root:FreeList
            oPage:RightPtr  := 0
            oPage:Write()
            SELF:_root:FreeList := nPage
            SELF:_root:Write()
            SELF:_PageList:Delete(nPage)
            RETURN TRUE

        METHOD AllocBuffer(nSize := 1 AS LONG)  AS BYTE[]
            RETURN BYTE[]{CDXPAGE_SIZE *nSize}

        METHOD Read(nPage AS LONG, buffer AS BYTE[]) AS LOGIC
            SELF:ThrowIfNeedsLock()
            RETURN SELF:_stream:SafeReadAt(nPage, buffer)

        METHOD Read(oPage AS CdxPage) AS LOGIC
            SELF:ThrowIfNeedsLock()
            IF SELF:_root != NULL
                oPage:Generation := SELF:_root:RootVersion
            ENDIF
            RETURN SELF:_stream:SafeReadAt(oPage:PageNo, oPage:Buffer, oPage:Buffer:Length)

        METHOD Write(oPage AS CdxPage) AS LOGIC
            LOCAL isOk AS LOGIC
#ifdef CHECKVERSIONS
            SELF:_PageList:CheckVersion(SELF:Root:RootVersion)
#endif
            IF oPage:PageNo == -1
                oPage:PageNo := SELF:FindFreePage()
                oPage:IsHot  := TRUE
                SELF:_PageList:SetPage(oPage:PageNo, oPage)
            ENDIF
            if oPage:PageNo < 0
                var cMessage := i"Trying to write to negative pageno {oPage:PageNo} in CDX"
                DebOut32(cMessage)
                THROW IOException{cMessage}
            ENDIF
            IF oPage:IsHot
                isOk := SELF:_stream:SafeWriteAt(oPage:PageNo, oPage:Buffer)
            ELSE
                isOk := TRUE
            ENDIF
            RETURN isOk



        METHOD GetPage(nPage AS Int32, nKeyLen AS WORD,tag AS CdxTag) AS CdxPage
           IF nPage == -1
                RETURN NULL
           ENDIF
           VAR page := SELF:_PageList:GetPage(nPage, nKeyLen,tag)
           IF page != NULL
                SELF:SetPage( page)
           ENDIF
           RETURN page

         METHOD SetPage(page AS CdxPage) AS VOID
            SELF:_PageList:SetPage(page:PageNo, page)
#ifdef CHECKVERSIONS
            page:Generation := SELF:Root:RootVersion
#endif

        #region properties

        PROPERTY Count AS LONG GET SELF:Tags:Count
        PROPERTY BagHasChanged AS LOGIC
            GET
                LOCAL nVersion AS DWORD
                nVersion := _root:RootVersion
                SELF:Read(SELF:_root)
                RETURN nVersion != _root:RootVersion

            END GET
        END PROPERTY

        INTERNAL METHOD CheckForChangedBag() AS LOGIC
            LOCAL lChanged := FALSE AS LOGIC
            IF SELF:BagHasChanged
                lChanged := TRUE
                SELF:_PageList:Clear()
                // Add new versions of Root and Taglist to the cache
                SELF:Root:Read()
                SELF:_tagList:Read()
                SELF:_PageList:Add(SELF:Root)
                SELF:_PageList:Add(SELF:_tagList)
            ENDIF
            RETURN lChanged

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

#region Common Locking code
        PRIVATE _useFoxLock     := FALSE AS LOGIC
        PRIVATE _sharedLocks    := 0 AS LONG
        PRIVATE _exclusiveLocks := 0 AS LONG
        PRIVATE _needsLock := FALSE AS LOGIC
        PRIVATE _lastLockTick   := 0 AS INT
        STATIC PRIVATE rand := Random{100} AS System.Random

        INTERNAL PROPERTY LockNeedsRefresh AS LOGIC
            GET
                VAR refresh := XSharp.RuntimeState.GetValue<REAL8>(Set.Refresh)
                IF refresh <= 0
                    RETURN TRUE
                ENDIF
                VAR tick := System.Environment.TickCount
                IF refresh > 0 .AND. (DWORD)(tick - _lastLockTick) >= 1000*refresh
                    RETURN TRUE
                ENDIF
                RETURN FALSE
            END GET
        END PROPERTY

        PRIVATE METHOD _LockRetry(nOffSet AS INT64, nLen AS INT64,sPrefix AS STRING) AS VOID
            LOCAL result := FALSE AS LOGIC
            // note that there is no wait here. Research showed that the VO/Vulcan drivers
            // also did not wait
            REPEAT
                result := SELF:_stream:SafeLock(nOffSet, nLen)
            UNTIL result
            //DebOut32( "Locked " +nOffSet:ToString()+" "+nLen:ToString())

        PRIVATE METHOD _Unlock(nOffSet AS INT64, nLen AS INT64) AS LOGIC
            VAR res := SELF:_stream:SafeUnlock(nOffSet, nLen)
            //IF res
            //    NOP // DebOut32( "UnLocked " +nOffSet:ToString()+" "+nLen:ToString())
            //ELSE
            //    //DebOut32( "UnLock FAILED " +nOffSet:ToString()+" "+nLen:ToString())
            //ENDIF
            RETURN res

        INTERNAL PROPERTY IsLocked AS LOGIC
            GET
                BEGIN LOCK SELF
                    RETURN SELF:_exclusiveLocks > 0 .OR. SELF:_sharedLocks > 0
                END LOCK
            END GET
        END PROPERTY

        INTERNAL METHOD NeedsLock() AS VOID
            IF SELF:Shared && !IsLocked
                SELF:_needsLock := TRUE
            ENDIF
            RETURN

        INTERNAL METHOD NeedsNoLock() AS VOID
            IF SELF:Shared
                SELF:_needsLock := FALSE
            ENDIF
            RETURN

        INTERNAL METHOD ThrowIfNeedsLock() AS VOID
            IF SELF:_needsLock && SELF:Shared
                THROW CdxLockException{}
            ENDIF
            RETURN

        INTERNAL METHOD SLock() AS LOGIC
            SELF:_needsLock := FALSE
            IF !SELF:Shared
                RETURN TRUE
            ENDIF
            //DebOut32( System.Threading.Thread.CurrentThread:Name+__ENTITY__ )
            BEGIN LOCK SELF
                SELF:_sharedLocks += 1
                IF SELF:_exclusiveLocks == 0  .AND. SELF:_sharedLocks == 1
                    IF SELF:_useFoxLock
                        SELF:_SLockFox()
                    ELSE
                        SELF:_SLockComix()
                    ENDIF
                    SELF:CheckForChangedBag()
                ENDIF
            END LOCK
            RETURN TRUE


        INTERNAL METHOD XLock() AS LOGIC
            SELF:_needsLock := FALSE
            IF !SELF:Shared
                RETURN TRUE
            ENDIF
            //DebOut32( System.Threading.Thread.CurrentThread:Name+__ENTITY__ )
            BEGIN LOCK SELF
                SELF:_exclusiveLocks += 1
                IF SELF:_exclusiveLocks == 1
                    IF _useFoxLock
                        SELF:_xLockFox()
                    ELSE
                        SELF:_xLockComix()
                    ENDIF
                    IF SELF:CheckForChangedBag()
                        FOREACH VAR oTag IN SELF:Tags
                            oTag:Stack:Clear()
                        NEXT
                    ENDIF
                ENDIF
            END LOCK
            RETURN TRUE


        INTERNAL METHOD UnLock() AS VOID
            SELF:_needsLock := FALSE
            IF !SELF:Shared
                RETURN
            ENDIF
            //DebOut32( System.Threading.Thread.CurrentThread:Name+__ENTITY__ )
            BEGIN LOCK SELF
                IF _useFoxLock
                    SELF:_UnLockFox()
                ELSE
                    SELF:_UnLockComix()
                ENDIF
            END LOCK
            _lastLockTick := System.Environment.TickCount
            RETURN
#endregion
    #region FoxPro compatible locking
        PRIVATE CONST FoxXLockOfs		:= 0x7ffffffe AS INT64
        PRIVATE CONST FoxXLockLen		:= 1          AS INT64
        PRIVATE CONST FoxSLockOfs		:= 0x7ffffffe AS INT64
        PRIVATE CONST FoxSLockLen		:= 1 AS DWORD

        PRIVATE METHOD _SLockFox() AS VOID
            SELF:_LockRetry(FoxSLockOfs, FoxSLockLen,"S")
            RETURN


        PRIVATE METHOD _xLockFox() AS VOID
            SELF:_LockRetry(FoxXLockOfs, FoxXLockLen,"X")
            RETURN


        PRIVATE METHOD _UnLockFox() AS VOID
            IF SELF:_sharedLocks > 0
                SELF:_sharedLocks -= 1
                IF SELF:_sharedLocks == 0
                    SELF:_Unlock(FoxSLockOfs, FoxSLockLen)
                ENDIF
            ELSEIF SELF:_exclusiveLocks > 0
                SELF:_exclusiveLocks -= 1
                IF SELF:_exclusiveLocks == 0
                    SELF:_UpdateRootVersion()
                    SELF:_Unlock(FoxXLockOfs, FoxXLockLen)
                ENDIF
            ENDIF
            RETURN

    #endregion
    #region Comix Compatible locking
        PRIVATE _sLockGate      := 0 AS LONG        // For CmxLock
        PRIVATE _sLockOffSet    := 0 AS DWORD       // For CmxLock
        PRIVATE _xLockedInOne   := FALSE AS LOGIC
        INTERNAL _LockOffSet    := 0 AS LONG
        PRIVATE CONST SLockGateCount    := 10 AS LONG
        PRIVATE CONST ComixXLockOfs		:= 0xfffeffff AS INT64
        PRIVATE CONST ComixSLockOfs		:= 0xffff0000 AS INT64
        PRIVATE CONST ComixXLockLen		:= 0x00010000 AS INT64
        STATIC PRIVATE randVal := 0 AS DWORD
        STATIC PRIVATE randInc, randDec AS LONG

        PRIVATE METHOD randNum () AS DWORD
            randInc -= randDec
            IF randInc <= 0
                randInc := DateTime.Now:Millisecond
                randDec := (randInc >> 8) + (randInc & 0xFF) + 1
            ENDIF
            randVal += (DWORD) randInc
            RETURN randVal


        PRIVATE METHOD _SLockComix() AS VOID
            //Debout32("_SlockComix() ")
            IF ++_sLockGate >= SLockGateCount
                _sLockGate := 0
                SELF:_LockRetry(ComixXLockOfs, 1,"S")
                SELF:_Unlock(ComixXLockOfs, 1)
            ENDIF
            SELF:_sLockOffSet := SELF:randNum()
            SELF:_LockRetry( _OR(ComixSLockOfs, _sLockOffSet),1,"S")

        PRIVATE METHOD _xLockComix() AS VOID
            //Debout32("_xLockComix() ")
            SELF:_sLockGate := 0
            Debug.Assert(_sharedLocks == 0)
            IF ! SELF:_stream:SafeLock(ComixXLockOfs, ComixXLockLen+1)
                SELF:_xLockedInOne := FALSE
                SELF:_LockRetry(ComixXLockOfs, 1,"X")
                SELF:_LockRetry(ComixSLockOfs, ComixXLockLen,"X")
            ELSE
                SELF:_xLockedInOne := TRUE
            ENDIF

        PRIVATE METHOD _UnLockComix() AS VOID

            IF SELF:_sharedLocks > 0
                SELF:_sharedLocks -= 1
                IF SELF:_sharedLocks == 0
                    SELF:_Unlock(ComixSLockOfs| SELF:_sLockOffSet, 1)
                ENDIF
            ELSEIF SELF:_exclusiveLocks > 0
                SELF:_exclusiveLocks -= 1
                IF SELF:_exclusiveLocks == 0
                    SELF:_UpdateRootVersion()
                    IF SELF:_xLockedInOne
                        SELF:_Unlock(ComixXLockOfs, ComixXLockLen+1)
                    ELSE
                        SELF:_Unlock(ComixXLockOfs, 1)
                        SELF:_Unlock(ComixSLockOfs, ComixXLockLen)
                    ENDIF
                ENDIF
            ENDIF
            RETURN

        PRIVATE METHOD _UpdateRootVersion() AS VOID
            var version := SELF:_root:RootVersion +1
            SELF:_root:RootVersion := version
            SELF:_tagList:Generation := SELF:_root:RootVersion
            FOREACH VAR tag IN SELF:_tagList:Tags
                tag:Header:Generation := SELF:_root:RootVersion
            NEXT
            // Update the pagelist first. otherwise writing the _root will fail.
#ifdef CHECKVERSIONS
            SELF:_PageList:SetVersion(version)
#endif
            SELF:_root:Write()
            IF _newPageAllocated
                _stream:SafeSetLength(_stream:Length)
                _newPageAllocated := FALSE
            ENDIF

    #endregion







    END CLASS
END NAMESPACE








