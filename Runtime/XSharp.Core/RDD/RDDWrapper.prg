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
        /// <inheritdoc />			
        VIRTUAL METHOD DbEval(info AS DbEvalInfo)		AS LOGIC
            RETURN oRdd:DbEval(info)        
            
        /// <inheritdoc />			
        VIRTUAL METHOD GoTop()							AS LOGIC
            RETURN oRdd:GoTop()
            
        /// <inheritdoc />			
        VIRTUAL METHOD GoBottom()						AS LOGIC   
            RETURN oRdd:GoBottom()
        /// <inheritdoc />			
        VIRTUAL METHOD GoTo(nRec AS LONG)				AS LOGIC
            RETURN oRdd:GoTo(nRec)
            
        /// <inheritdoc />			
        VIRTUAL METHOD GoToId(oRec AS OBJECT)			AS LOGIC
            RETURN oRdd:GoToId(oRec)
            
        /// <inheritdoc />			
        VIRTUAL METHOD Skip(nToSkip AS INT)				AS LOGIC
            RETURN oRdd:Skip(nToSkip)
            
        /// <inheritdoc />			
        VIRTUAL METHOD SkipFilter(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipFilter(nToSkip)
            
        /// <inheritdoc />			
        VIRTUAL METHOD SkipRaw(nToSkip AS INT)			AS LOGIC 
            RETURN oRdd:SkipRaw(nToSkip)
            
        /// <inheritdoc />			
        VIRTUAL METHOD SkipScope(nToSkip AS INT)		AS LOGIC
            RETURN oRdd:SkipScope(nToSkip)
        #endregion
        
        //-------------------------------------------------------
        // Read / Write
        //-------------------------------------------------------
        #region Read / Write
        /// <inheritdoc />			
        VIRTUAL METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC
            RETURN oRdd:Append(lReleaseLock)
            
        /// <inheritdoc />			
        VIRTUAL METHOD Delete()							AS LOGIC   
            RETURN oRdd:Delete()
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetRec()							AS BYTE[]  
            RETURN oRdd:GetRec()
            
        /// <inheritdoc />			
        VIRTUAL METHOD Pack()							AS LOGIC
            RETURN oRdd:Pack()
            
        /// <inheritdoc />			
        VIRTUAL METHOD PutRec(aRec AS BYTE[])			AS LOGIC 
            RETURN oRdd:PutRec(aRec)
            
        /// <inheritdoc />			
        VIRTUAL METHOD Recall()							AS LOGIC
            RETURN oRdd:Recall()
            
        /// <inheritdoc />			
        VIRTUAL METHOD Zap()							AS LOGIC   
            RETURN oRdd:Zap()
            
        /// <inheritdoc />			
        VIRTUAL METHOD Close() 							AS LOGIC
            RETURN oRdd:Close()
            
        /// <inheritdoc />			
        VIRTUAL METHOD Create(info AS DbOpenInfo)		AS LOGIC  
            RETURN oRdd:Create(info)
            
        /// <inheritdoc />			
        VIRTUAL METHOD Open(info AS DbOpenInfo)			AS LOGIC
            RETURN oRdd:Open(info)

        #endregion
        //-------------------------------------------------------
        // Record Selection - Scoping
        //-------------------------------------------------------
        #region Record Selection - Scoping
        /// <inheritdoc />			
        VIRTUAL METHOD ClearFilter() 					AS LOGIC
            RETURN oRdd:ClearFilter()
            
        /// <inheritdoc />			
        VIRTUAL METHOD ClearScope() 					AS LOGIC 
            RETURN oRdd:ClearScope()
            
        /// <inheritdoc />			
        VIRTUAL METHOD Continue()						AS LOGIC     
            RETURN oRdd:Continue()
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetScope()						AS DbScopeInfo 
            RETURN oRdd:GetScope()
            
        /// <inheritdoc />			
        VIRTUAL METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC 
            RETURN oRdd:SetFilter(info)
            
        /// <inheritdoc />			
        VIRTUAL METHOD SetScope(info AS DbScopeInfo)	AS LOGIC
            RETURN oRdd:SetScope(info)
        #endregion
        //-------------------------------------------------------
        // Creation
        //-------------------------------------------------------

        #region Creation
        /// <inheritdoc />			
        VIRTUAL METHOD SetFieldExtent(fieldCount AS LONG)	AS LOGIC
            RETURN oRdd:SetFieldExtent(fieldCount)
            
        /// <inheritdoc />			
        VIRTUAL METHOD AddField(info AS RddFieldInfo)	AS LOGIC
            RETURN oRdd:AddField(info)
            
        /// <inheritdoc />			
        VIRTUAL METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            RETURN oRdd:CreateFields(aFields)
        #endregion
        
        //-------------------------------------------------------
        // Field Related
        //-------------------------------------------------------

        #region Field Related
        /// <inheritdoc />			
        VIRTUAL METHOD FieldIndex(fieldName AS STRING)	AS LONG 
            RETURN oRdd:FieldIndex(fieldName)
            
        /// <inheritdoc />			
        VIRTUAL METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:FieldInfo(nFldPos,nOrdinal,oNewValue)
            
        /// <inheritdoc />			
        VIRTUAL METHOD FieldName(nFldPos AS LONG)		AS STRING
            RETURN oRdd:FieldName(nFldPos)
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetField(nFldPos AS LONG) AS RddFieldInfo
            RETURN oRdd:GetField(nFldPos)
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetValue(nFldPos AS LONG)		AS OBJECT
            RETURN oRdd:GetValue(nFldPos)
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:GetValueFile(nFldPos, fileName)
            
        /// <inheritdoc />			
        VIRTUAL METHOD GetValueLength(nFldPos AS LONG)	AS LONG
            RETURN oRdd:GetValueLength(nFldPos)

        /// <inheritdoc />			
        VIRTUAL METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            RETURN oRdd:PutValue(nFldPos, oValue)
            
        /// <inheritdoc />			
        VIRTUAL METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            RETURN oRdd:PutValueFile(nFldPos, fileName)

        #endregion
        //-------------------------------------------------------
        // Hot and Cold
        //-------------------------------------------------------
        #region Hot and Cold

        /// <inheritdoc />			
        VIRTUAL METHOD Flush() 							AS LOGIC
            RETURN oRdd:Flush()
            
        /// <inheritdoc />			
        VIRTUAL METHOD GoCold()							AS LOGIC
            RETURN oRdd:GoCold()
            
        /// <inheritdoc />			
        VIRTUAL METHOD GoHot()							AS LOGIC   
            RETURN oRdd:GoHot()
    
        /// <inheritdoc />			
        VIRTUAL METHOD Refresh() 							AS LOGIC
            RETURN oRdd:Refresh()

        #endregion

        //-------------------------------------------------------
        // Locking
        //-------------------------------------------------------
        #region Locking
        /// <inheritdoc />			
        VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
            RETURN oRdd:AppendLock(uiMode)
            
        /// <inheritdoc />			
        VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
            RETURN oRdd:HeaderLock(uiMode)
            
        /// <inheritdoc />			
        VIRTUAL METHOD Lock(uiMode REF DbLockInfo)		AS LOGIC 
            RETURN oRdd:Lock(REF uiMode)
            
        /// <inheritdoc />			
        VIRTUAL METHOD UnLock(oRecId AS OBJECT)			AS LOGIC
            RETURN oRdd:UnLock(oRecId)

        #endregion
        //-------------------------------------------------------
        // MemoFiles
        //-------------------------------------------------------

        #region MemoFiles
        
        /// <inheritdoc />			
        VIRTUAL METHOD CloseMemFile() 					AS LOGIC    
            RETURN oRdd:CloseMemFile()
            
        /// <inheritdoc />			
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            RETURN oRdd:CreateMemFile(info)
            
        /// <inheritdoc />			
        VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC   
            RETURN oRdd:OpenMemFile(info ) 		

        #endregion
        //-------------------------------------------------------
        // Indexes and orders
        //-------------------------------------------------------
        #region Indexes and orders
        /// <inheritdoc />			
        VIRTUAL METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN oRdd:OrderCondition(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
            RETURN oRdd:OrderCreate(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
            RETURN oRdd:OrderDestroy(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
            RETURN oRdd:OrderInfo(nOrdinal, info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListAdd(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListDelete(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            RETURN oRdd:OrderListFocus(info ) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD OrderListRebuild() AS LOGIC 
            RETURN oRdd:OrderListRebuild() 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD Seek(info AS DbSeekInfo) AS LOGIC
            RETURN oRdd:Seek(info) 		

        #endregion
        //-------------------------------------------------------
        // Relations
        //-------------------------------------------------------
        #region Relations
        /// <inheritdoc />			
        VIRTUAL METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC 
            RETURN oRdd:ChildEnd(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD ChildStart(info AS DbRelInfo)			AS LOGIC
            RETURN oRdd:ChildStart(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD ChildSync(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:ChildSync(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD ClearRel()								AS LOGIC
            RETURN oRdd:ClearRel() 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD ForceRel()								AS LOGIC  
            RETURN oRdd:ForceRel() 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD RelArea(nRelNum AS DWORD)					AS DWORD 
            RETURN oRdd:RelArea(nRelNum) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD RelEval(info AS DbRelInfo)				AS LOGIC
            RETURN oRdd:RelEval(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD RelText(nRelNum AS DWORD)					AS STRING
            RETURN oRdd:RelText(nRelNum) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD SetRel(info AS DbRelInfo)				AS LOGIC  
            RETURN oRdd:SetRel(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD SyncChildren()							AS LOGIC
            RETURN oRdd:SyncChildren() 		

        #endregion

        //-------------------------------------------------------
        // Bulk Sort and Copy
        //-------------------------------------------------------
        #region Bulk Sort and Copy
        /// <inheritdoc />			
        VIRTUAL METHOD Sort(info AS DbSortInfo)					AS LOGIC
            RETURN oRdd:Sort(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD Trans(info AS DbTransInfo) 				AS LOGIC
            RETURN oRdd:Trans(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD TransRec(info AS DbTransInfo) 			AS LOGIC
            RETURN oRdd:TransRec(info) 		
            
        /// <inheritdoc />			
        VIRTUAL METHOD BlobInfo(uiPos AS DWORD, nOrdinal AS DWORD) AS OBJECT
            RETURN oRdd:BlobInfo(uiPos, nOrdinal) 		
        #endregion
        //-------------------------------------------------------
        // Macro compiler
        //-------------------------------------------------------
        #region Macro compiler
        /// <inheritdoc />			
        VIRTUAL METHOD Compile(sBlock AS STRING)				AS ICodeblock
            RETURN oRdd:Compile(sBlock) 		

        /// <inheritdoc />			
        VIRTUAL METHOD EvalBlock(oBlock AS ICodeblock)			AS OBJECT	
            RETURN oRdd:EvalBlock(oBlock) 		
        #endregion
        //-------------------------------------------------------
        // Info
        //-------------------------------------------------------
        #region Info
        /// <inheritdoc />			
        VIRTUAL METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            RETURN oRdd:Info(nOrdinal, oNewValue) 		

        /// <inheritdoc />			
        VIRTUAL METHOD RecInfo( nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT  
            RETURN oRdd:RecInfo(nOrdinal, oRecID, oNewValue) 		
        #endregion
        //-------------------------------------------------------
        // Properties
        //-------------------------------------------------------
        #region Properties
        /// <inheritdoc />			
        VIRTUAL PROPERTY Alias 		AS STRING	GET oRdd:Alias SET oRdd:Alias := value
        /// <inheritdoc />			
        VIRTUAL PROPERTY Area		AS DWORD	GET oRdd:Area  SET oRdd:Area := value
        /// <inheritdoc />			
        VIRTUAL PROPERTY BoF 		AS LOGIC	GET oRdd:BoF
        /// <inheritdoc />			
        VIRTUAL PROPERTY Deleted 	AS LOGIC	GET oRdd:Deleted
        /// <inheritdoc />			
        VIRTUAL PROPERTY Driver     AS STRING	GET oRdd:Driver
        /// <inheritdoc />			
        VIRTUAL PROPERTY EoF 		AS LOGIC	GET oRdd:EoF
        /// <inheritdoc />			
        VIRTUAL PROPERTY Exclusive	AS LOGIC	GET oRdd:Exclusive
        /// <inheritdoc />			
        VIRTUAL PROPERTY FieldCount AS LONG		GET oRdd:FieldCount
        /// <inheritdoc />			
        VIRTUAL PROPERTY FilterText	AS STRING	GET oRdd:FilterText
        /// <inheritdoc />			
        VIRTUAL PROPERTY Found		AS LOGIC	GET oRdd:Found SET oRdd:Found := value
        /// <inheritdoc />			
        VIRTUAL PROPERTY RecCount	AS LONG		GET oRdd:RecCount
        /// <inheritdoc />			
        VIRTUAL PROPERTY RecId		AS OBJECT	GET	oRdd:RecId
        /// <inheritdoc />			
        VIRTUAL PROPERTY RecNo		AS LONG		GET oRdd:RecNo
        /// <inheritdoc />			
        VIRTUAL PROPERTY Shared		AS LOGIC	GET oRdd:Shared
        
        #endregion
        
    END CLASS
END NAMESPACE 
