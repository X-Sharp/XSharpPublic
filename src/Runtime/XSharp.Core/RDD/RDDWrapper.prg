//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
USING XSharp.RDD.Enums
USING System.Reflection
USING System.Collections.Generic
USING System.Diagnostics


BEGIN NAMESPACE XSharp.RDD

    /// <summary>This class can be used to wrap an existing RDD and add some functionality to it, such as
    /// logging, encryption etc. <br/>
    /// To use it, create a subclass of this class and implement the methods that you want to override.<br/>
    /// The constructor of this class takes an existing RDD object and automatically replaces it in the
    /// Workarea table.
    /// </summary>
    /// <example>
    /// <code language="X#">
    /// CLASS MyRDD Inherit XSharp.RDD.WrapperRDD
    ///
    /// CONSTRUCTOR(oRdd as XSharp.RDD.IRDD)
    ///    SUPER(oRdd)
    ///
    /// VIRTUAL METHOD GetValue(nFldPos AS LONG)		AS OBJECT
    ///    Log("Reading field "+nFldPos:ToString())
    ///    RETURN SUPER:GetValue(nFldPos)
    ///
    /// VIRTUAL METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
    ///    Log("Writing field "+nFldPos:ToString()+" value "+oValue:ToString())
    ///    RETURN SUPER:PutValue(nFldPos, oValue)
    /// END CLASS
    ///
    /// FUNCTION Start() AS VOID
    ///    // Open customer table
    ///    USE Customer NEW
    ///
    ///    // Get current RDD for the selected Workarea
    ///    VAR oRdd := XSharp.RuntimeState.Workareas:GetRDD(Select())
    ///
    ///    // Create Wrapper RDD. This automatically replaces the RDD in the Workarea table
    ///    oRdd := MyRDD{oRdd}
    ///
    ///    ? FieldGet(1)           // This should also call Log() to show reading the value
    ///    ? FieldPut(1,"Jones")   // This should also call Log() to show writing the value
    ///    DbCloseArea()
    ///    RETURN
    ///
    /// Function Log(cMessage as STRING) AS VOID
    ///    ? cMessage
    ///    RETURN
    /// </code>
    /// </example>
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS WrapperRDD IMPLEMENTS IRdd
        // Install this as follows:
        // The class itself will take care of replacing the RDD in the Workarea table
        //
        // VAR oRdd := XSharp.RuntimeState.Workareas:GetRDD(<area Number>)
        // oRdd := WrapperRDD{oRdd}
        // If you are only interested in certain methods, then subclass this class and only write the methods that you are interested in
        //

        PROTECT oRdd AS IRdd

        VIRTUAL METHOD DebuggerDisplay() AS STRING
            IF oRdd != NULL
                RETURN oRdd:Driver+" ("+oRdd:Alias+") - Wrapped"
            ENDIF
            RETURN "(uninitialized)"

        CONSTRUCTOR(loRdd AS IRdd)
            oRdd    := loRdd
            RuntimeState.Workareas:SetArea(oRdd:Area, SELF)
            RETURN


        //-------------------------------------------------------
        // Record Movement
        //-------------------------------------------------------
        #region Record Movement
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.DbEval/*" />
        VIRTUAL METHOD DbEval(info AS DbEvalInfo)		AS LOGIC
            RETURN oRdd:DbEval(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoTop/*" />
        VIRTUAL METHOD GoTop()							AS LOGIC
            RETURN oRdd:GoTop()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoBottom/*" />
        VIRTUAL METHOD GoBottom()						AS LOGIC
            RETURN oRdd:GoBottom()
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoTo/*" />
        VIRTUAL METHOD GoTo(nRec AS DWORD)				AS LOGIC
            RETURN oRdd:GoTo(nRec)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoToId/*" />
        VIRTUAL METHOD GoToId(oRec AS OBJECT)			AS LOGIC
            RETURN oRdd:GoToId(oRec)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Skip/*" />
        VIRTUAL METHOD Skip(nToSkip AS INT)				AS LOGIC
            RETURN oRdd:Skip(nToSkip)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SkipFilter/*" />
        VIRTUAL METHOD SkipFilter(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipFilter(nToSkip)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SkipRaw/*" />
        VIRTUAL METHOD SkipRaw(nToSkip AS INT)			AS LOGIC
            RETURN oRdd:SkipRaw(nToSkip)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SkipScope/*" />
        VIRTUAL METHOD SkipScope(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipScope(nToSkip)
        #endregion

        //-------------------------------------------------------
        // Read / Write
        //-------------------------------------------------------
        #region Read / Write
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Append/*" />
        VIRTUAL METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC
            RETURN oRdd:Append(lReleaseLock)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Delete/*" />
        VIRTUAL METHOD Delete()							AS LOGIC
            RETURN oRdd:Delete()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetRec/*" />
        VIRTUAL METHOD GetRec()							AS BYTE[]
            RETURN oRdd:GetRec()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Pack/*" />
        VIRTUAL METHOD Pack()							AS LOGIC
            RETURN oRdd:Pack()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.PutRec/*" />
        VIRTUAL METHOD PutRec(aRec AS BYTE[])			AS LOGIC
            RETURN oRdd:PutRec(aRec)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Recall/*" />
        VIRTUAL METHOD Recall()							AS LOGIC
            RETURN oRdd:Recall()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Zap/*" />
        VIRTUAL METHOD Zap()							AS LOGIC
            RETURN oRdd:Zap()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Close/*" />
        VIRTUAL METHOD Close() 							AS LOGIC
            RETURN oRdd:Close()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Create/*" />
        VIRTUAL METHOD Create(info AS DbOpenInfo)		AS LOGIC
            RETURN oRdd:Create(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Open/*" />
        VIRTUAL METHOD Open(info AS DbOpenInfo)			AS LOGIC
            RETURN oRdd:Open(info)

        #endregion
        //-------------------------------------------------------
        // Record Selection - Scoping
        //-------------------------------------------------------
        #region Record Selection - Scoping
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ClearFilter/*" />
        VIRTUAL METHOD ClearFilter() 					AS LOGIC
            RETURN oRdd:ClearFilter()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ClearScope/*" />
        VIRTUAL METHOD ClearScope() 					AS LOGIC
            RETURN oRdd:ClearScope()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Continue/*" />
        VIRTUAL METHOD Continue()						AS LOGIC
            RETURN oRdd:Continue()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetScope/*" />
        VIRTUAL METHOD GetScope()						AS DbScopeInfo
            RETURN oRdd:GetScope()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SetFilter/*" />
        VIRTUAL METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC
            RETURN oRdd:SetFilter(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SetScope/*" />
        VIRTUAL METHOD SetScope(info AS DbScopeInfo)	AS LOGIC
            RETURN oRdd:SetScope(info)
        #endregion
        //-------------------------------------------------------
        // Creation
        //-------------------------------------------------------

        #region Creation
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SetFieldExtent/*" />
        VIRTUAL METHOD SetFieldExtent(fieldCount AS LONG)	AS LOGIC
            RETURN oRdd:SetFieldExtent(fieldCount)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.AddField/*" />
        VIRTUAL METHOD AddField(info AS RddFieldInfo)	AS LOGIC
            RETURN oRdd:AddField(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.CreateFields/*" />
        VIRTUAL METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            RETURN oRdd:CreateFields(aFields)
        #endregion

        //-------------------------------------------------------
        // Field Related
        //-------------------------------------------------------

        #region Field Related
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.FieldIndex/*" />
        VIRTUAL METHOD FieldIndex(fieldName AS STRING)	AS LONG
            RETURN oRdd:FieldIndex(fieldName)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.FieldInfo/*" />
        VIRTUAL METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:FieldInfo(nFldPos,nOrdinal,oNewValue)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.FieldName/*" />
        VIRTUAL METHOD FieldName(nFldPos AS LONG)		AS STRING
            RETURN oRdd:FieldName(nFldPos)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetField/*" />
        VIRTUAL METHOD GetField(nFldPos AS LONG) AS RddFieldInfo
            RETURN oRdd:GetField(nFldPos)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetValue/*" />
        VIRTUAL METHOD GetValue(nFldPos AS LONG)		AS OBJECT
            RETURN oRdd:GetValue(nFldPos)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetValueFile/*" />
        VIRTUAL METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:GetValueFile(nFldPos, fileName)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GetValueLength/*" />
        VIRTUAL METHOD GetValueLength(nFldPos AS LONG)	AS LONG
            RETURN oRdd:GetValueLength(nFldPos)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.PutValue/*" />
        VIRTUAL METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            RETURN oRdd:PutValue(nFldPos, oValue)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.PutValueFile/*" />
        VIRTUAL METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:PutValueFile(nFldPos, fileName)

        #endregion
        //-------------------------------------------------------
        // Hot and Cold
        //-------------------------------------------------------
        #region Hot and Cold

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Flush/*" />
        VIRTUAL METHOD Flush() 							AS LOGIC
            RETURN oRdd:Flush()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoCold/*" />
        VIRTUAL METHOD GoCold()							AS LOGIC
            RETURN oRdd:GoCold()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.GoHot/*" />
        VIRTUAL METHOD GoHot()							AS LOGIC
            RETURN oRdd:GoHot()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Refresh/*" />
        VIRTUAL METHOD Refresh() 							AS LOGIC
            RETURN oRdd:Refresh()

        #endregion

        //-------------------------------------------------------
        // Locking
        //-------------------------------------------------------
        #region Locking
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.AppendLock/*" />
        VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC
            RETURN oRdd:AppendLock(uiMode)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.HeaderLock/*" />
        VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC
            RETURN oRdd:HeaderLock(uiMode)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Lock/*" />
        VIRTUAL METHOD Lock(uiMode REF DbLockInfo)		AS LOGIC
            RETURN oRdd:Lock(REF uiMode)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.UnLock/*" />
        VIRTUAL METHOD UnLock(oRecId AS OBJECT)			AS LOGIC
            RETURN oRdd:UnLock(oRecId)

        #endregion
        //-------------------------------------------------------
        // MemoFiles
        //-------------------------------------------------------

        #region MemoFiles

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.CloseMemFile/*" />
        VIRTUAL METHOD CloseMemFile() 					AS LOGIC
            RETURN oRdd:CloseMemFile()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.CreateMemFile/*" />
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            RETURN oRdd:CreateMemFile(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OpenMemFile/*" />
        VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC
            RETURN oRdd:OpenMemFile(info )

        #endregion
        //-------------------------------------------------------
        // Indexes and orders
        //-------------------------------------------------------
        #region Indexes and orders
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderCondition/*" />
        VIRTUAL METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN oRdd:OrderCondition(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderCreate/*" />
        VIRTUAL METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
            RETURN oRdd:OrderCreate(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderDestroy/*" />
        VIRTUAL METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderDestroy(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderInfo/*" />
        VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
            RETURN oRdd:OrderInfo(nOrdinal, info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderListAdd/*" />
        VIRTUAL METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListAdd(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderListDelete/*" />
        VIRTUAL METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListDelete(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderListFocus/*" />
        VIRTUAL METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListFocus(info )

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.OrderListRebuild/*" />
        VIRTUAL METHOD OrderListRebuild() AS LOGIC
            RETURN oRdd:OrderListRebuild()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Seek/*" />
        VIRTUAL METHOD Seek(info AS DbSeekInfo) AS LOGIC
            RETURN oRdd:Seek(info)

        #endregion
        //-------------------------------------------------------
        // Relations
        //-------------------------------------------------------
        #region Relations
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ChildEnd/*" />
        VIRTUAL METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:ChildEnd(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ChildStart/*" />
        VIRTUAL METHOD ChildStart(info AS DbRelInfo)			AS LOGIC
            RETURN oRdd:ChildStart(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ChildSync/*" />
        VIRTUAL METHOD ChildSync(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:ChildSync(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ClearRel/*" />
        VIRTUAL METHOD ClearRel()								AS LOGIC
            RETURN oRdd:ClearRel()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.ForceRel/*" />
        VIRTUAL METHOD ForceRel()								AS LOGIC
            RETURN oRdd:ForceRel()

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RelArea/*" />
        VIRTUAL METHOD RelArea(nRelNum AS DWORD)					AS DWORD
            RETURN oRdd:RelArea(nRelNum)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RelEval/*" />
        VIRTUAL METHOD RelEval(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:RelEval(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RelText/*" />
        VIRTUAL METHOD RelText(nRelNum AS DWORD)					AS STRING
            RETURN oRdd:RelText(nRelNum)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SetRel/*" />
        VIRTUAL METHOD SetRel(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:SetRel(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.SyncChildren/*" />
        VIRTUAL METHOD SyncChildren()							AS LOGIC
            RETURN oRdd:SyncChildren()

        #endregion

        //-------------------------------------------------------
        // Bulk Sort and Copy
        //-------------------------------------------------------
        #region Bulk Sort and Copy
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Sort/*" />
        VIRTUAL METHOD Sort(info AS DbSortInfo)					AS LOGIC
            RETURN oRdd:Sort(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Trans/*" />
        VIRTUAL METHOD Trans(info AS DbTransInfo) 				AS LOGIC
            RETURN oRdd:Trans(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.TransRec/*" />
        VIRTUAL METHOD TransRec(info AS DbTransInfo) 			AS LOGIC
            RETURN oRdd:TransRec(info)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.BlobInfo/*" />
        VIRTUAL METHOD BlobInfo(uiPos AS DWORD, nOrdinal AS DWORD) AS OBJECT
            RETURN oRdd:BlobInfo(uiPos, nOrdinal)
        #endregion
        //-------------------------------------------------------
        // Macro compiler
        //-------------------------------------------------------
        #region Macro compiler
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Compile/*" />
        VIRTUAL METHOD Compile(sBlock AS STRING)				AS ICodeblock
            RETURN oRdd:Compile(sBlock)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.EvalBlock/*" />
        VIRTUAL METHOD EvalBlock(oBlock AS ICodeblock)			AS OBJECT
            RETURN oRdd:EvalBlock(oBlock)
        #endregion
        //-------------------------------------------------------
        // Info
        //-------------------------------------------------------
        #region Info
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Info/*" />
        VIRTUAL METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:Info(nOrdinal, oNewValue)

        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RecInfo/*" />
        VIRTUAL METHOD RecInfo( nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:RecInfo(nOrdinal, oRecID, oNewValue)
        #endregion
        //-------------------------------------------------------
        // Properties
        //-------------------------------------------------------
        #region Properties
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Alias/*" />
        VIRTUAL PROPERTY Alias 		AS STRING	GET oRdd:Alias SET oRdd:Alias := value
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Area/*" />
        VIRTUAL PROPERTY Area		AS DWORD	GET oRdd:Area  SET oRdd:Area := value
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.BoF/*" />
        VIRTUAL PROPERTY BoF 		AS LOGIC	GET oRdd:BoF
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Deleted/*" />
        VIRTUAL PROPERTY Deleted 	AS LOGIC	GET oRdd:Deleted
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Driver/*" />
        VIRTUAL PROPERTY Driver     AS STRING	GET oRdd:Driver
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.EoF/*" />
        VIRTUAL PROPERTY EoF 		AS LOGIC	GET oRdd:EoF
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Exclusive/*" />
        VIRTUAL PROPERTY Exclusive	AS LOGIC	GET oRdd:Exclusive
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.FieldCount/*" />
        VIRTUAL PROPERTY FieldCount AS LONG		GET oRdd:FieldCount
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.FilterText/*" />
        VIRTUAL PROPERTY FilterText	AS STRING	GET oRdd:FilterText
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Found/*" />
        VIRTUAL PROPERTY Found		AS LOGIC	GET oRdd:Found SET oRdd:Found := value
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RecCount/*" />
        VIRTUAL PROPERTY RecCount	AS DWORD	GET oRdd:RecCount
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RecId/*" />
        VIRTUAL PROPERTY RecId		AS OBJECT	GET	oRdd:RecId
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.RecNo/*" />
        VIRTUAL PROPERTY RecNo		AS DWORD	GET oRdd:RecNo
        /// <include file="XSharp.CoreDocs.xml" path="doc/WrapperRDD.Shared/*" />
        VIRTUAL PROPERTY Shared		AS LOGIC	GET oRdd:Shared

        #endregion

    END CLASS
END NAMESPACE
