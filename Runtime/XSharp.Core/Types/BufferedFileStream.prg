//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING System.Runtime
USING System.Diagnostics
USING System.Runtime.InteropServices
USING System.Collections.Generic
USING System.Runtime.CompilerServices
USING System.Linq

BEGIN NAMESPACE XSharp.IO
    // This class was inspired by a post on StackOverflow
    INTERNAL CLASS LRUCache<K,V> WHERE V IS CLASS
        PRIVATE _capacity as LONG
        PRIVATE _cacheMap AS Dictionary<K, LinkedListNode<LRUCacheItem<K, V>>>
        PRIVATE _lruList  AS LinkedList<LRUCacheItem<K, V>>

        INTERNAL CONSTRUCTOR (capacity as int)
            SELF:_capacity := capacity
            SELF:_cacheMap := Dictionary<K, LinkedListNode<LRUCacheItem<K, V>>>{}
            SELF:_lruList  := LinkedList<LRUCacheItem<K, V>>{}

        INTERNAL PROPERTY Values AS IList<V>
            [MethodImpl(MethodImplOptions.Synchronized)] ;
            GET
                VAR result := List<V>{}
                FOREACH VAR item in _cacheMap
                    VAR llnode := item:Value
                    VAR cacheItem := llnode:Value
                    result:Add(cacheItem:Value)
                NEXT
                RETURN result
            END GET
        END PROPERTY

        [MethodImpl(MethodImplOptions.Synchronized)];
        INTERNAL METHOD Get (key as K) AS V
            LOCAL node as LinkedListNode<LRUCacheItem<K, V>>
            if _cacheMap:TryGetValue(key, out node)
                LOCAL result := node:Value:Value AS V
                _lruList:Remove(node)
                _lruList:AddLast(node)
                return result
            ENDIF
        return default (V)

        [MethodImpl(MethodImplOptions.Synchronized)] ;
        INTERNAL METHOD Add(key as K, val as V) AS V
            LOCAL old as V
            if _cacheMap:Count >= _capacity
                old := SELF:RemoveFirst()
            ELSE
                old := NULL
            endif
            var cacheItem := LRUCacheItem<K, V>{key, val}
            var node      := LinkedListNode<LRUCacheItem<K, V>>{cacheItem}
            _lruList:AddLast(node)
            _cacheMap.Add(key, node)
        RETURN old

        PRIVATE METHOD RemoveFirst() AS V
            // Remove from LRUPriority
            var node := _lruList:First
            _lruList:RemoveFirst()
            // Remove from cache
            _cacheMap:Remove(node:Value:Key)
        RETURN node:Value:Value

        [MethodImpl(MethodImplOptions.Synchronized)];
        INTERNAL METHOD Remove(key as K) AS LOGIC
            IF _cacheMap:ContainsKey(key)
                VAR v := _cacheMap[key]
                _lruList:Remove(v)
                _cacheMap:Remove(key)
                return TRUE
            ENDIF
        RETURN FALSE

        [DebuggerDisplay("{Key}")];
        INTERNAL CLASS LRUCacheItem<K1,V1>
            INTERNAL @@Key as K1
            INTERNAL @@Value as V1
            INTERNAL CONSTRUCTOR (k as K1, v as V1)
                Key := k
                @@Value := v
        END CLASS
    END CLASS

    /// <summary>This class was inspired by the DiskIO module in Visual Objects</summary>
    INTERNAL CLASS PageBuffers
        [DebuggerDisplay("{Key}")];
        INTERNAL CLASS FilePage
            INTERNAL Stream     AS XsBufferedFileStream
            INTERNAL Page       AS INT64
            INTERNAL Buffer     AS Byte[]
            INTERNAL Size       AS LONG
            INTERNAL Used       AS LONG
            INTERNAL Hot        AS LOGIC
            INTERNAL PROPERTY Key  AS STRING GET GetHash(Stream, Page)

            INTERNAL STATIC METHOD GetHash(oStream as XsBufferedFileStream, nPage as INT64) AS STRING
                RETURN  oStream:FileName:ToLower()+":"+nPage:ToString()

            INTERNAL CONSTRUCTOR (oStream AS XsBufferedFileStream, nPage AS INT64, nSize AS LONG)
                SELF:Stream := oStream
                SELF:Page   := nPage
                SELF:Size   := nSize
                SELF:Used   := 0
                SELF:Hot    := FALSE
                SELF:Buffer := BYTE[]{nSize}

        END CLASS

        STATIC PROTECTED cache AS LRUCache<STRING, FilePage>

        STATIC CONSTRUCTOR
            cache := LRUCache<STRING, FilePage> {512}


        INTERNAL STATIC METHOD GetPage(oStream AS XsBufferedFileStream, nPage AS INT64, nSize AS LONG) AS FilePage
            VAR key  := FilePage.GetHash(oStream, nPage)
            VAR page := cache:Get(key)
            IF page == NULL
                page := __AddPage(oStream, nPage, nSize)
                VAR bytesRead := oStream:XRead(nPage, page:Buffer, nSize)
                page:Used     := bytesRead
                // if bytesRead < nSize then we have reached EOF
            ENDIF
            return page


        INTERNAL STATIC METHOD __AddPage(oStream AS XsBufferedFileStream, nPage AS INT64, nSize AS LONG) AS FilePage
            VAR page := FilePage{oStream, nPage, nSize}
            VAR old  := cache:Add(page:Key, page)
            IF old != NULL         // Cache is full, oldest page is returned and needs to written here
                __WritePage(old)
            ENDIF
        RETURN page

        INTERNAL STATIC METHOD __WritePage(page as FilePage) AS VOID
            IF !page:Hot
                RETURN
            ENDIF
            VAR nSize := Math.Min(page:Size, page:Used)
            IF ! page:Stream:XWrite( page:Page, page:Buffer, nSize)
                THROW Exception{"Error writing to disk"}
            ENDIF
            page:Hot := FALSE
        RETURN

        STATIC METHOD WritePages(oStream AS XsBufferedFileStream) AS VOID
            FOREACH VAR page IN cache:Values
                IF page:Stream == oStream
                    __WritePage(page)
                ENDIF
            NEXT
        RETURN


        STATIC METHOD FlushPages(oStream AS XsBufferedFileStream) AS VOID
            WritePages(oStream)
            FOREACH VAR page IN cache:Values:ToArray()
                IF page:Stream == oStream
                    // We can delete in the foreach loop because ToArray() creates a copy of the collection
                    cache:Remove(page:Key)
                ENDIF
            NEXT
        RETURN

    END CLASS

    /// <summary>This class performs buffered IO to files opened exclusively</summary>
    CLASS XsBufferedFileStream INHERIT XsFileStream

        PUBLIC CONST BUFF_SIZE  := 0x400 AS LONG
        PUBLIC CONST BUFF_MASK  := 0x3FF AS LONG
        PROTECTED _length   AS INT64
        PROTECTED _position AS INT64
        PROTECTED _closed   AS LOGIC
        PROTECTED _callOriginalMethods AS LOGIC

        CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            SUPER(path, mode, faccess, share, bufferSize, options)
            SELF:_length := SUPER:Length
            SELF:Position := 0
            SELF:_callOriginalMethods := FALSE

        /// <summary>Bypass the buffered Read mechanism and call the original Read() method in the base class</summary>
        PUBLIC METHOD XRead(page AS INT64,bytes AS BYTE[] , count AS INT) AS INT
            SELF:_callOriginalMethods := TRUE
            SELF:Position := page
            VAR result := SUPER:Read(bytes, 0, count)
            SELF:_callOriginalMethods := FALSE
            RETURN result

        /// <summary>Bypass the buffered Write mechanism and call the original Write() method in the base class</summary>
        PUBLIC METHOD XWrite(page AS INT64, bytes AS BYTE[] , count AS INT) AS LOGIC
            SELF:_callOriginalMethods := TRUE
            SELF:Position := page
            SUPER:Write(bytes, 0, count)
            SELF:_callOriginalMethods := FALSE
            RETURN TRUE

        /// <inheritdoc />
        PUBLIC OVERRIDE PROPERTY Position AS INT64
            GET
                IF SELF:_callOriginalMethods
                    RETURN SUPER:Position
                ENDIF
                RETURN _position
            END GET
            SET
                IF SELF:_callOriginalMethods
                    SUPER:Position := VALUE
                ELSE
                    _position := VALUE
                ENDIF
            END SET
        END PROPERTY
        /// <inheritdoc />
        PUBLIC OVERRIDE PROPERTY Length   AS INT64
            GET
                IF SELF:_callOriginalMethods
                    RETURN SUPER:Length
                ENDIF
                RETURN _length
            END GET

        END PROPERTY

        /// <inheritdoc />

        PUBLIC OVERRIDE METHOD SetLength (newLength AS INT64) AS VOID
            IF SELF:_callOriginalMethods
                SUPER:SetLength(newLength)
            ENDIF
            _length := newLength

        /// <inheritdoc />
        /// <remarks>This method overrides the normal behavior of the FileStream class and reads the data from an inmemory cache, when possible </remarks>
        PUBLIC OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            // pages in the cache are 1 K
            IF SELF:_callOriginalMethods
                RETURN SUPER:Read(bytes, offset, count)
            ENDIF
            var pos         := SELF:Position
            VAR pageNo      := (INT64) _AND(pos , ~BUFF_MASK)
            var pageoffset  := (LONG) _AND(pos , BUFF_MASK)
            VAR read        := 0
            VAR endOfPage   := FALSE
            DO WHILE read < count .AND. ! endOfPage
                VAR page         := PageBuffers.GetPage(SELF, pageNo, BUFF_SIZE)
                VAR buffer       := page:Buffer
                // we either read the rest of the page, or just the bytes needed
                VAR onthispage   := count - read
                if onthispage > (BUFF_SIZE - pageoffset)
                    onthispage := BUFF_SIZE - pageoffset
                else
                    nop
                ENDIF
                IF pageoffset + onthispage > page:Used
                    onthispage := Math.Max(page:Used - pageoffset, 0)
                    endOfPage := TRUE
                ENDIF
                System.Array.Copy(buffer, pageoffset, bytes, offset+read, onthispage)
                pageNo     += BUFF_SIZE
                read       += onthispage
                pageoffset := 0                 // we start the next page on byte 0
            ENDDO
            SELF:Position += read
        RETURN read

        /// <inheritdoc />
        /// <remarks>This method overrides the normal behavior of the FileStream class and writed the data to an inmemory cache, when possible </remarks>
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT, count AS INT) AS VOID
            IF SELF:_callOriginalMethods
                SUPER:Write(bytes, offset, count)
                RETURN
            ENDIF
            VAR pos         := SELF:Position
            VAR pageNo      := (INT64) _AND(pos , ~BUFF_MASK)
            var pageoffset  := (LONG) _AND(pos , BUFF_MASK)
            var written     := 0
            DO WHILE written < count
                VAR page         := PageBuffers.GetPage(SELF, pageNo, BUFF_SIZE)
                VAR buffer       := page:Buffer
                // we either write the rest of the page, or just the bytes needed
                VAR onthispage := count - written
                if onthispage > ( BUFF_SIZE - pageoffset)
                    onthispage :=  (BUFF_SIZE - pageoffset)
                else
                    nop
                endif
                System.Array.Copy(bytes, offset+written, buffer, pageoffset, onthispage)
                IF pageoffset + onthispage > page:Used
                   page:Used  := pageoffset + onthispage
                    Debug.Assert(page:Used <= page:Size)
                ENDIF
                page:Hot   := TRUE
                pageNo     += BUFF_SIZE
                written    += onthispage
                pageoffset := 0                 // we start the next page on byte 0
            ENDDO
            VAR newPos := pos + count
            SELF:_length  := Math.Max(SELF:_length, newPos)
            SELF:Position := newPos

        RETURN

        PRIVATE _temp := BYTE[]{1} AS BYTE[]
        /// <inheritdoc />
        /// <remarks>This method overrides the normal behavior of the FileStream class and writed the data to an inmemory cache, when possible </remarks>
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            IF SELF:_callOriginalMethods
                SUPER:WriteByte(b)
                RETURN
            ENDIF
            _temp[0] := b
            SELF:Write(_temp, 0, 1)
        RETURN

        /// <inheritdoc />
        /// <remarks>This method overrides the normal behavior of the FileStream class and flushes the cached data to disk before calling the parent Flush() method.</remarks>
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            PageBuffers.WritePages(SELF)
            SUPER:Flush(lCommit)
            IF SUPER:Length != _length
                SUPER:SetLength(_length)
            ENDIF
            SELF:Position := _length
        RETURN

        /// <inheritdoc />
        /// <remarks>This method overrides the normal behavior of the FileStream class and flushes the cached data to disk before calling the parent Close() method.</remarks>
        PUBLIC OVERRIDE METHOD Close( ) AS VOID
            IF ! SELF:_closed
                PageBuffers.FlushPages(SELF)
                IF SUPER:Length != _length
                    SUPER:SetLength(_length)
                ENDIF
                SELF:_closed := TRUE
            ENDIF
            SUPER:Close()
            RETURN

        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            IF SELF:_callOriginalMethods
                RETURN SUPER:Seek(offset, origin)
            ENDIF
            var pos := self:Position
            SWITCH origin
            CASE SeekOrigin.Begin
                 pos := offset
            CASE SeekOrigin.Current
                 pos += offset
            CASE SeekOrigin.End
                 pos := offset + SELF:_length
            END SWITCH
            SELF:Position := pos
            return pos

        END CLASS

END NAMESPACE
