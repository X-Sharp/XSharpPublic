USING XSharp.RDD.Support
USING XSharp.RDD.Enums
USING System.Reflection
USING System.Diagnostics


BEGIN NAMESPACE XSharp.RDD

    /// <summary>This class can be used to wrap an existing RDD and add some functionality to it, such as
    /// logging, encryption etc. <br/>
    /// To use it, create a subclass of this class and implement the methods that you want to override.<br/>
    /// The constructor of this class takes an existing RDD object and automatically replaces it in the
    /// workarea table.
    /// </summary>
    /// <example>
    /// <code>
    /// CLASS MyRDD Inherit XSharp.RDD.WrapperRDD
    /// CONSTRUCTOR(oRDD as XSharp.RDD.IRDD)
    ///    SUPER(oRDD)
    /// METHOD GetValue(nFldPos AS LONG)		AS OBJECT
    ///    Log("Reading field "+nFldPos:ToString())
    ///    RETURN SUPER:GetValue(nFldPos)
    /// METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
    ///    Log("Writing field "+nFldPos:ToString()+" value "+oValue:ToString())
    ///    RETURN SUPER:PutValue(nFldPos, oValue)
    /// END CLASS
    /// 
    /// FUNCTION Start() AS VOID
    ///    // Open customer table
    ///    USE Customer NEW
    /// 
    ///    // Get current RDD for the selected workarea
    ///    VAR oRDD := XSharp.RuntimeState.Workareas:GetRDD(Select())
    /// 
    ///    // Create Wrapper RDD. This automatically replaces the RDD in the workarea table
    ///    oRDD := MyRDD{oRDD}
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
        // The class itself will take care of replacing the RDD in the workarea table
        //
        // VAR oRDD := XSharp.RuntimeState.Workareas:GetRDD(<area Number>)      
        // oRDD := WrapperRDD{oRDD}
        // If you are only interested in certain methods, then subclass this class and only write the methods that you are interested in
        // 

        PROTECT oRDD AS IRdd

        PRIVATE METHOD DebuggerDisplay() AS STRING
            IF oRDD != NULL
                RETURN oRDD:Driver+" ("+oRDD:Alias+") - Wrapped"
            ENDIF
            RETURN "(uninitialized)"

        CONSTRUCTOR(loRDD AS IRdd)
            LOCAL oType AS System.Type
            LOCAL oFI   AS System.Reflection.FieldInfo
            LOCAL aRDDs AS IRDD[]
            oRDD    := loRDD
            oType   := TypeOf(XSharp.RDD.WorkAreas)
            oFI     := oType:GetField("RDDs",BindingFlags.NonPublic | BindingFlags.IgnoreCase|BindingFlags.Instance)
            aRDDs   := (IRDD[]) oFI:GetValue(XSHarp.RuntimeState.Workareas)
            aRDDs[oRDD:Area] := SELF
            RETURN
            
            
        //-------------------------------------------------------
        // Record Movement
        //-------------------------------------------------------
        #region Record Movement
        /// <inheritdoc />			
        METHOD DbEval(info AS DbEvalInfo)		AS LOGIC
            RETURN oRDD:DbEval(info)        
            
        /// <inheritdoc />			
        METHOD GoTop()							AS LOGIC
            RETURN oRdd:GoTop()
            
        /// <inheritdoc />			
        METHOD GoBottom()						AS LOGIC   
            RETURN oRdd:GoBottom()
        /// <inheritdoc />			
        METHOD GoTo(nRec AS LONG)				AS LOGIC
            RETURN oRdd:Goto(nRec)
            
        /// <inheritdoc />			
        METHOD GoToId(oRec AS OBJECT)			AS LOGIC
            RETURN oRdd:GotoId(oRec)
            
        /// <inheritdoc />			
        METHOD Skip(nToSkip AS INT)				AS LOGIC
            RETURN oRdd:Skip(nToSkip)
            
        /// <inheritdoc />			
        METHOD SkipFilter(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipFilter(nToSkip)
            
        /// <inheritdoc />			
        METHOD SkipRaw(nToSkip AS INT)			AS LOGIC 
            RETURN oRdd:SkipRaw(nToSkip)
            
        /// <inheritdoc />			
        METHOD SkipScope(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipScope(nToSkip)
        #endregion
        
        //-------------------------------------------------------
        // Read / Write
        //-------------------------------------------------------
        #region Read / Write
        /// <inheritdoc />			
        METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC
            RETURN oRdd:Append(lReleaseLock)
            
        /// <inheritdoc />			
        METHOD Delete()							AS LOGIC   
            RETURN oRdd:Delete()
            
        /// <inheritdoc />			
        METHOD GetRec()							AS BYTE[]  
            RETURN oRdd:GetRec()
            
        /// <inheritdoc />			
        METHOD Pack()							AS LOGIC
            RETURN oRdd:Pack()
            
        /// <inheritdoc />			
        METHOD PutRec(aRec AS BYTE[])			AS LOGIC 
            RETURN oRdd:PutRec(aRec)
            
        /// <inheritdoc />			
        METHOD Recall()							AS LOGIC
            RETURN oRdd:Recall()
            
        /// <inheritdoc />			
        METHOD Zap()							AS LOGIC   
            RETURN oRdd:Zap()
            
        /// <inheritdoc />			
        METHOD Close() 							AS LOGIC
            RETURN oRdd:Close()
            
        /// <inheritdoc />			
        METHOD Create(info AS DbOpenInfo)		AS LOGIC  
            RETURN oRdd:Create(info)
            
        /// <inheritdoc />			
        METHOD Open(info AS DbOpenInfo)			AS LOGIC
            RETURN oRdd:Zap()

        #endregion
        //-------------------------------------------------------
        // Record Selection - Scoping
        //-------------------------------------------------------
        #region Record Selection - Scoping
        /// <inheritdoc />			
        METHOD ClearFilter() 					AS LOGIC
            RETURN oRdd:ClearFilter()
            
        /// <inheritdoc />			
        METHOD ClearScope() 					AS LOGIC 
            RETURN oRdd:ClearScope()
            
        /// <inheritdoc />			
        METHOD Continue()						AS LOGIC     
            RETURN oRdd:Continue()
            
        /// <inheritdoc />			
        METHOD GetScope()						AS DbScopeInfo 
            RETURN oRdd:GetScope()
            
        /// <inheritdoc />			
        METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC 
            RETURN oRdd:SetFilter(info)
            
        /// <inheritdoc />			
        METHOD SetScope(info AS DbScopeInfo)	AS LOGIC
            RETURN oRdd:SetScope(info)
        #endregion
        //-------------------------------------------------------
        // Creation
        //-------------------------------------------------------

        #region Creation
        /// <inheritdoc />			
        METHOD AddField(info AS RddFieldInfo)	AS LOGIC
            RETURN oRdd:AddField(info)
            
        /// <inheritdoc />			
        METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            RETURN oRdd:CreateFields(aFields)
        #endregion
        
        //-------------------------------------------------------
        // Field Related
        //-------------------------------------------------------

        #region Field Related
        /// <inheritdoc />			
        METHOD FieldIndex(fieldName AS STRING)	AS LONG 
            RETURN oRdd:FieldIndex(fieldName)
            
        /// <inheritdoc />			
        METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:FieldInfo(nFldPos,nOrdinal,oNewValue)
            
        /// <inheritdoc />			
        METHOD FieldName(nFldPos AS LONG)		AS STRING
            RETURN oRdd:FieldName(nFldPos)
            
        /// <inheritdoc />			
        METHOD GetField(nFldPos AS LONG) AS RDDFieldInfo
            RETURN oRdd:GetField(nFldPos)
            
        /// <inheritdoc />			
        METHOD GetValue(nFldPos AS LONG)		AS OBJECT
            RETURN oRdd:GetValue(nFldPos)
            
        /// <inheritdoc />			
        METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:GetValueFile(nFldPos, fileName)
            
        /// <inheritdoc />			
        METHOD GetValueLength(nFldPos AS LONG)	AS LONG
            RETURN oRdd:GetValueLength(nFldPos)

        /// <inheritdoc />			
        METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            RETURN oRdd:PutValue(nFldPos, oValue)
            
        /// <inheritdoc />			
        METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:PutValueFile(nFldPos, fileName)

        #endregion
        //-------------------------------------------------------
        // Hot and Cold
        //-------------------------------------------------------
        #region Hot and Cold

        /// <inheritdoc />			
        METHOD Flush() 							AS LOGIC
            RETURN oRdd:Flush()
            
        /// <inheritdoc />			
        METHOD GoCold()							AS LOGIC
            RETURN oRdd:GoCold()
            
        /// <inheritdoc />			
        METHOD GoHot()							AS LOGIC   
            RETURN oRdd:GoHot()
    
        /// <inheritdoc />			
        METHOD Refresh() 							AS LOGIC
            RETURN oRdd:Refresh()

        #endregion

        //-------------------------------------------------------
        // Locking
        //-------------------------------------------------------
        #region Locking
        /// <inheritdoc />			
        METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
            RETURN oRdd:AppendLock(uiMode)
            
        /// <inheritdoc />			
        METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
            RETURN oRdd:HeaderLock(uiMode)
            
        /// <inheritdoc />			
        METHOD Lock(uiMode REF DBLOCKINFO)		AS LOGIC 
            RETURN oRdd:Lock(REF uiMode)
            
        /// <inheritdoc />			
        METHOD UnLock(oRecId AS OBJECT)			AS LOGIC
            RETURN oRdd:UnLock(oRecID)

        #endregion
        //-------------------------------------------------------
        // MemoFiles
        //-------------------------------------------------------

        #region MemoFiles
        
        /// <inheritdoc />			
        METHOD CloseMemFile() 					AS LOGIC    
            RETURN oRdd:CloseMemFile()
            
        /// <inheritdoc />			
        METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            RETURN oRdd:CreateMemFile(info)
            
        /// <inheritdoc />			
        METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC   
            RETURN oRdd:OpenMemFile(info ) 		

        #endregion
        //-------------------------------------------------------
        // Indexes and orders
        //-------------------------------------------------------
        #region Indexes and orders
        /// <inheritdoc />			
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN oRdd:OrderCondition(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
            RETURN oRdd:OrderCreate(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
            RETURN oRdd:OrderDestroy(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
            RETURN oRdd:OrderInfo(nOrdinal, info ) 		
            
        /// <inheritdoc />			
        METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListAdd(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListDelete(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListFocus(info ) 		
            
        /// <inheritdoc />			
        METHOD OrderListRebuild() AS LOGIC 
            RETURN oRdd:OrderListRebuild() 		
            
        /// <inheritdoc />			
        METHOD Seek(info AS DbSeekInfo) AS LOGIC
            RETURN oRdd:Seek(info) 		

        #endregion
        //-------------------------------------------------------
        // Relations
        //-------------------------------------------------------
        #region Relations
        /// <inheritdoc />			
        METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC 
            RETURN oRdd:ChildEnd(info) 		
            
        /// <inheritdoc />			
        METHOD ChildStart(info AS DbRelInfo)			AS LOGIC
            RETURN oRdd:ChildStart(info) 		
            
        /// <inheritdoc />			
        METHOD ChildSync(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:ChildSync(info) 		
            
        /// <inheritdoc />			
        METHOD ClearRel()								AS LOGIC
            RETURN oRdd:ClearRel() 		
            
        /// <inheritdoc />			
        METHOD ForceRel()								AS LOGIC  
            RETURN oRdd:ForceRel() 		
            
        /// <inheritdoc />			
        METHOD RelArea(nRelNum AS DWORD)					AS DWORD 
            RETURN oRdd:RelArea(nRelNum) 		
            
        /// <inheritdoc />			
        METHOD RelEval(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:RelEval(info) 		
            
        /// <inheritdoc />			
        METHOD RelText(nRelNum AS DWORD)					AS STRING
            RETURN oRdd:RelText(nRelNum) 		
            
        /// <inheritdoc />			
        METHOD SetRel(info AS DbRelInfo)				AS LOGIC  
            RETURN oRdd:SetRel(info) 		
            
        /// <inheritdoc />			
        METHOD SyncChildren()							AS LOGIC
            RETURN oRdd:SyncChildren() 		

        #endregion

        //-------------------------------------------------------
        // Bulk Sort and Copy
        //-------------------------------------------------------
        #region Bulk Sort and Copy
        /// <inheritdoc />			
        METHOD Sort(info AS DbSortInfo)					AS LOGIC
            RETURN oRdd:Sort(info) 		
            
        /// <inheritdoc />			
        METHOD Trans(info AS DbTransInfo) 				AS LOGIC
            RETURN oRdd:Trans(info) 		
            
        /// <inheritdoc />			
        METHOD TransRec(info AS DbTransInfo) 			AS LOGIC
            RETURN oRdd:TransRec(info) 		
            
        /// <inheritdoc />			
        METHOD BlobInfo(uiPos AS DWORD, nOrdinal AS DWORD) AS OBJECT
            RETURN oRdd:BlobInfo(uiPos, nOrdinal) 		
        #endregion
        //-------------------------------------------------------
        // Macro compiler
        //-------------------------------------------------------
        #region Macro compiler
        /// <inheritdoc />			
        METHOD Compile(sBlock AS STRING)				AS ICodeblock
            RETURN oRdd:Compile(sBlock) 		

        /// <inheritdoc />			
        METHOD EvalBlock(oBlock AS ICodeblock)			AS OBJECT	
            RETURN oRdd:EvalBlock(oBlock) 		
        #endregion
        //-------------------------------------------------------
        // Info
        //-------------------------------------------------------
        #region Info
        /// <inheritdoc />			
        METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:Info(nOrdinal, oNewValue) 		

        /// <inheritdoc />			
        METHOD RecInfo( nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT  
            RETURN oRdd:RecInfo(nOrdinal, oRecID, oNewValue) 		
        #endregion
        //-------------------------------------------------------
        // Properties
        //-------------------------------------------------------
        #region Properties
        /// <inheritdoc />			
        PROPERTY Alias 		AS STRING	GET oRDD:Alias SET oRDD:Alias := VALUE
        /// <inheritdoc />			
        PROPERTY Area		AS DWORD	GET oRDD:Area  SET oRDD:Area := VALUE
        /// <inheritdoc />			
        PROPERTY BoF 		AS LOGIC	GET oRDD:BoF
        /// <inheritdoc />			
        PROPERTY Deleted 	AS LOGIC	GET oRDD:Deleted
        /// <inheritdoc />			
        PROPERTY Driver     AS STRING	GET oRDD:Driver
        /// <inheritdoc />			
        PROPERTY EoF 		AS LOGIC	GET oRDD:EoF
        /// <inheritdoc />			
        PROPERTY Exclusive	AS LOGIC	GET oRDD:Exclusive
        /// <inheritdoc />			
        PROPERTY FieldCount AS LONG		GET oRDD:FieldCount
        /// <inheritdoc />			
        PROPERTY FilterText	AS STRING	GET oRDD:FilterText
        /// <inheritdoc />			
        PROPERTY Found		AS LOGIC	GET oRDD:Found SET oRDD:Found := VALUE
        /// <inheritdoc />			
        PROPERTY RecCount	AS LONG		GET oRDD:RecCount
        /// <inheritdoc />			
        PROPERTY RecId		AS OBJECT	GET	oRDD:RecId
        /// <inheritdoc />			
        PROPERTY RecNo		AS LONG		GET oRDD:RecNo
        /// <inheritdoc />			
        PROPERTY Shared		AS LOGIC	GET oRDD:Shared
        
        #endregion
        
    END CLASS
END NAMESPACE 
