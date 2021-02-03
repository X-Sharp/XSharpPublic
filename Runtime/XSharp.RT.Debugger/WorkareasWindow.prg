USING System.Windows.Forms
USING System.Collections.Generic
USING System.Reflection

BEGIN NAMESPACE XSharp.Debugger

ENUM WorkareaDetail
    MEMBER FileName
    MEMBER Rdd
    MEMBER Area
    MEMBER Ansi
    MEMBER Shared
    MEMBER ReadOnly
    MEMBER FieldCount
    MEMBER RecCount
    MEMBER RecSize
    MEMBER RecNo
    MEMBER Eof
    MEMBER Bof
    MEMBER Deleted
    MEMBER Found
    MEMBER Filter
END ENUM

ENUM MovementType
    MEMBER GoTo
    MEMBER GoTop
    MEMBER GoBottom
    MEMBER SkipNext
    MEMBER SkipPrevious
END ENUM

CLASS ListViewItem_workarea INHERIT ListViewItem
PROPERTY Rdd AS XSharp.RDD.IRdd AUTO
PROPERTY Workarea AS DWORD AUTO
CONSTRUCTOR(cText AS STRING)
    SUPER(cText)
RETURN
END CLASS

CLASS WorkareasWindow INHERIT System.Windows.Forms.Form
    
    PROTECT oIndexTreeView AS System.Windows.Forms.TreeView
    PROTECT oCloseButton AS System.Windows.Forms.Button
    PROTECT oNavigator AS System.Windows.Forms.ToolStrip
    PROTECT oFieldsListView AS System.Windows.Forms.ListView
    PROTECT oDetailsListView AS System.Windows.Forms.ListView
    PROTECT oWorkareasListView AS System.Windows.Forms.ListView
    // User code starts here (DO NOT remove this line)  ##USER##
    
METHOD InitializeForm() AS VOID
    
        // IDE generated code (please DO NOT modify)
    
    SELF:oIndexTreeView := System.Windows.Forms.TreeView{}
    SELF:oCloseButton := System.Windows.Forms.Button{}
    SELF:oNavigator := System.Windows.Forms.ToolStrip{}
    SELF:oFieldsListView := System.Windows.Forms.ListView{}
    SELF:oDetailsListView := System.Windows.Forms.ListView{}
    SELF:oWorkareasListView := System.Windows.Forms.ListView{}
    
    SELF:SuspendLayout()
    
    SELF:AutoScaleDimensions := System.Drawing.SizeF{ 6 , 13 }
    SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
    SELF:ClientSize := System.Drawing.Size{896 , 576}
    SELF:Location := System.Drawing.Point{100 , 100}
    SELF:MaximizeBox := FALSE
    SELF:MinimizeBox := FALSE
    SELF:Name := "WorkareasWindow"
    SELF:ShowIcon := FALSE
    SELF:ShowInTaskbar := FALSE
    SELF:SizeGripStyle := System.Windows.Forms.SizeGripStyle.Hide
    SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterParent
    SELF:Text := "Active database workareas"
    
    SELF:CancelButton := SELF:oCloseButton
    SELF:oIndexTreeView:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right
    SELF:oIndexTreeView:Location := System.Drawing.Point{600 , 8}
    SELF:oIndexTreeView:Name := "IndexTreeView"
    SELF:oIndexTreeView:Size := System.Drawing.Size{280 , 280}
    SELF:oIndexTreeView:TabIndex := 2
    SELF:Controls:Add(SELF:oIndexTreeView)
    
    SELF:oCloseButton:Anchor := System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
    SELF:oCloseButton:Location := System.Drawing.Point{757 , 544}
    SELF:oCloseButton:Name := "CloseButton"
    SELF:oCloseButton:Size := System.Drawing.Size{123 , 23}
    SELF:oCloseButton:TabIndex := 5
    SELF:oCloseButton:Text := "Close"
    SELF:Controls:Add(SELF:oCloseButton)
    
    SELF:oNavigator:SuspendLayout()
    SELF:oNavigator:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right
    SELF:oNavigator:Dock := System.Windows.Forms.DockStyle.None
    SELF:oNavigator:Location := System.Drawing.Point{8 , 296}
    SELF:oNavigator:Name := "Navigator"
    SELF:oNavigator:Size := System.Drawing.Size{872 , 30}
    SELF:oNavigator:TabIndex := 4
    SELF:Controls:Add(SELF:oNavigator)
    
    SELF:oFieldsListView:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
    SELF:oFieldsListView:FullRowSelect := TRUE
    SELF:oFieldsListView:HideSelection := FALSE
    SELF:oFieldsListView:Location := System.Drawing.Point{8 , 328}
    SELF:oFieldsListView:MultiSelect := FALSE
    SELF:oFieldsListView:Name := "FieldsListView"
    SELF:oFieldsListView:Size := System.Drawing.Size{872 , 208}
    SELF:oFieldsListView:TabIndex := 3
    SELF:oFieldsListView:View := System.Windows.Forms.View.Details
    SELF:Controls:Add(SELF:oFieldsListView)
    
    SELF:oDetailsListView:FullRowSelect := TRUE
    SELF:oDetailsListView:HideSelection := FALSE
    SELF:oDetailsListView:Location := System.Drawing.Point{288 , 8}
    SELF:oDetailsListView:MultiSelect := FALSE
    SELF:oDetailsListView:Name := "DetailsListView"
    SELF:oDetailsListView:Size := System.Drawing.Size{304 , 280}
    SELF:oDetailsListView:TabIndex := 1
    SELF:oDetailsListView:View := System.Windows.Forms.View.Details
    SELF:Controls:Add(SELF:oDetailsListView)
    
    SELF:oWorkareasListView:FullRowSelect := TRUE
    SELF:oWorkareasListView:HideSelection := FALSE
    SELF:oWorkareasListView:Location := System.Drawing.Point{8 , 8}
    SELF:oWorkareasListView:MultiSelect := FALSE
    SELF:oWorkareasListView:Name := "WorkareasListView"
    SELF:oWorkareasListView:SelectedIndexChanged += SELF:WorkareasListView_SelectedIndexChanged
    SELF:oWorkareasListView:Size := System.Drawing.Size{272 , 280}
    SELF:oWorkareasListView:TabIndex := 0
    SELF:oWorkareasListView:View := System.Windows.Forms.View.Details
    SELF:Controls:Add(SELF:oWorkareasListView)
    
    SELF:oNavigator:ResumeLayout()
    SELF:ResumeLayout()
    
RETURN

CONSTRUCTOR()
    
    SUPER()
    
    SELF:InitializeForm()
    
    SELF:oWorkareasListView:Columns:Add("No" , 50)
    SELF:oWorkareasListView:Columns:Add("Alias" , 200)
    
    SELF:oDetailsListView:Columns:Add("Property" , 90)
    SELF:oDetailsListView:Columns:Add("Value" , 200)
    
    SELF:CreateDetail("Filename")
    SELF:CreateDetail("RDD name")
    SELF:CreateDetail("Area number")
    SELF:CreateDetail("Ansi")
    SELF:CreateDetail("Shared")
    SELF:CreateDetail("ReadOnly")
    SELF:CreateDetail("Field count")
    SELF:CreateDetail("Record count")
    SELF:CreateDetail("Record size")
    SELF:CreateDetail("Record number")
    SELF:CreateDetail("Eof")
    SELF:CreateDetail("Bof")
    SELF:CreateDetail("Deleted")
    SELF:CreateDetail("Found")
    SELF:CreateDetail("Filter text")
    
    SELF:oFieldsListView:Columns:Add("Field name" , 80)
    SELF:oFieldsListView:Columns:Add("Type" , 80)
    SELF:oFieldsListView:Columns:Add("Length" , 80)
    SELF:oFieldsListView:Columns:Add("Decimals" , 80)
    SELF:oFieldsListView:Columns:Add("Value" , 400)
    
    SELF:oNavigator:AutoSize := FALSE
    
    LOCAL oSource AS BindingNavigator
    oSource := BindingNavigator{TRUE}
    
    SELF:oNavigator:Items:Add(ToolStripButton{NULL, oSource:MoveFirstItem:Image , {o,e => SELF:DoMove(MovementType.GoTop)} })
    SELF:oNavigator:Items:Add(ToolStripButton{NULL, oSource:MovePreviousItem:Image , {o,e => SELF:DoMove(MovementType.SkipPrevious)} })
    SELF:oNavigator:Items:Add(ToolStripButton{NULL, oSource:MoveNextItem:Image , {o,e => SELF:DoMove(MovementType.SkipNext)} })
    SELF:oNavigator:Items:Add(ToolStripButton{NULL, oSource:MoveLastItem:Image , {o,e => SELF:DoMove(MovementType.GoBottom)} })
    
    SELF:FillWorkareas()
    IF SELF:oWorkareasListView:Items:Count != 0
        SELF:oWorkareasListView:SelectedIndices:Add(0)
    END IF
    
RETURN

METHOD DoMove(eMove AS MovementType) AS VOID
    LOCAL oRdd AS XSharp.RDD.IRdd
    IF SELF:oWorkareasListView:SelectedIndices:Count == 0
        RETURN
    END IF
    oRdd := ((ListViewItem_workarea)SELF:oWorkareasListView:SelectedItems[0]):Rdd
    SWITCH eMove
    CASE MovementType.GoTop
        oRdd:GoTop()
    CASE MovementType.GoBottom
        oRdd:GoBottom()
    CASE MovementType.SkipNext
        oRdd:Skip(+1)
    CASE MovementType.SkipPrevious
        oRdd:Skip(-1)
    END SWITCH
    SELF:FillAll()
RETURN

PROTECTED METHOD CreateDetail(cName AS STRING) AS VOID
    LOCAL oItem AS ListViewItem_workarea
    oItem := ListViewItem_workarea{cName}
    oItem:SubItems:Add("")
    SELF:oDetailsListView:Items:Add(oItem)
RETURN
PROTECTED METHOD SetDetail(nIndex AS INT , cValue AS STRING) AS VOID
    LOCAL oItem AS ListViewItem_workarea
    oItem := (ListViewItem_workarea)SELF:oDetailsListView:Items[nIndex]
    oItem:SubItems[1]:Text := cValue
RETURN
PROTECTED METHOD ClearDetails() AS VOID
    LOCAL oItem AS ListViewItem_workarea
    LOCAL n AS INT
    FOR n := 0 UPTO SELF:oDetailsListView:Items:Count - 1
        oItem := (ListViewItem_workarea)SELF:oDetailsListView:Items[n]
        oItem:SubItems[1]:Text := ""
    NEXT
RETURN

METHOD WorkareasListView_SelectedIndexChanged(sender AS System.Object , e AS System.EventArgs) AS VOID
    SELF:FillAll()
RETURN

METHOD FillAll() AS VOID
    LOCAL oRdd AS XSharp.RDD.IRdd
    LOCAL lError := FALSE AS LOGIC
    LOCAL nWorkarea AS DWORD
    
    IF SELF:oWorkareasListView:SelectedIndices:Count == 0
        SELF:oIndexTreeView:Nodes:Clear()
        SELF:oFieldsListView:Items:Clear()
        SELF:oNavigator:Enabled := FALSE
        SELF:ClearDetails()
        RETURN
    END IF
    
    oRdd := ((ListViewItem_workarea)SELF:oWorkareasListView:SelectedItems[0]):Rdd
    nWorkarea := ((ListViewItem_workarea)SELF:oWorkareasListView:SelectedItems[0]):Workarea
    Cursor.Current := Cursors.WaitCursor
    
    SELF:oDetailsListView:BeginUpdate()
    SELF:oIndexTreeView:BeginUpdate()
    SELF:oFieldsListView:BeginUpdate()
    TRY
        FillDetails(oRdd, nWorkarea)
    CATCH
        SELF:ClearDetails()
        lError := TRUE
    END TRY
    
    IF lError
        SELF:oIndexTreeView:Nodes:Clear()
        SELF:oFieldsListView:Items:Clear()
    ELSE
        TRY
            FillIndexes(oRdd)
        CATCH
            SELF:oIndexTreeView:Nodes:Clear()
        END TRY
        TRY
            TRY
                FillFields(oRdd)
            CATCH /*e*/ AS System.Exception
                //					MessageBox.Show(e:ToString(), "Some error happened:")
            END TRY
            SELF:oNavigator:Enabled := TRUE
        CATCH
            SELF:oFieldsListView:Items:Clear()
            SELF:oNavigator:Enabled := FALSE
        END TRY
    END IF
    SELF:oDetailsListView:EndUpdate()
    SELF:oIndexTreeView:EndUpdate()
    SELF:oFieldsListView:EndUpdate()
    
    Cursor.Current := Cursors.Default
RETURN


METHOD FillWorkareas() AS VOID
    LOCAL oItem AS ListViewItem_workarea
    LOCAL nWorkarea AS DWORD
    
    LOCAL aItems AS SortedList<DWORD,ListViewItem_workarea>
    aItems := SortedList<DWORD,ListViewItem_workarea>{}
    FOR LOCAL i := 1 AS DWORD UPTO XSharp.RDD.Workareas.MaxWorkareas
        LOCAL oRdd AS XSharp.RDD.IRdd
        oRdd:=  XSharp.RuntimeState.Workareas:GetRDD(i)
        IF oRdd != NULL
            //				? oRdd:Alias
            nWorkarea := i
            oItem := ListViewItem_workarea{nWorkarea:ToString()}
            oItem:SubItems:Add(oRdd:Alias)
            oItem:Rdd := oRdd
            oItem:Workarea := nWorkarea
            aItems:Add(nWorkarea , oItem)
        END IF
    NEXT
    
    FOREACH oSortedItem AS KeyValuePair<DWORD,ListViewItem_workarea> IN aItems
        SELF:oWorkareasListView:Items:Add(oSortedItem:Value)
    NEXT
    
RETURN

METHOD FillDetails(oRdd AS XSharp.RDD.IRdd, nWorkarea AS DWORD) AS VOID
        /*		FOR LOCAL n := 1 AS INT UPTO 50
        ? n , oRdd:Info(n,NULL)
        NEXT
        LOCAL oDbf AS XSharp.RDD.Workarea
        oDbf := (XSharp.RDD.Workarea)oRdd*/
    
        //		SELF:SetDetail(WorkareaDetail.FileName , oRdd:Info(24,NULL):ToString())
    SELF:FillFieldDetail(WorkareaDetail.FileName , oRdd, "_FileName")
    SELF:SetDetail(WorkareaDetail.Rdd , oRdd:Driver)
    SELF:SetDetail(WorkareaDetail.Area , nWorkarea:ToString())
    SELF:FillFieldDetail(WorkareaDetail.Ansi , oRdd, "_Ansi")
    SELF:SetDetail(WorkareaDetail.FieldCount , oRdd:FieldCount:ToString())
    SELF:SetDetail(WorkareaDetail.RecCount , oRdd:RecCount:ToString())
    SELF:FillFieldDetail(WorkareaDetail.RecSize , oRdd , "_RecordLength")
    SELF:FillFieldDetail(WorkareaDetail.Shared , oRdd , "Shared")
    SELF:FillFieldDetail(WorkareaDetail.ReadOnly , oRdd , "_ReadOnly")
    SELF:FillFieldDetail(WorkareaDetail.Deleted , oRdd , "Deleted")
    SELF:SetDetail(WorkareaDetail.Eof , oRdd:EoF:ToString())
    SELF:SetDetail(WorkareaDetail.Bof , oRdd:BoF:ToString())
    SELF:FillFieldDetail(WorkareaDetail.Found , oRdd , "Found")
    SELF:SetDetail(WorkareaDetail.Filter , oRdd:FilterText)
    SELF:FillFieldDetail(WorkareaDetail.RecNo , oRdd , "RecNo")
RETURN
VIRTUAL METHOD FillFieldDetail(eDetail AS WorkareaDetail, oRdd AS XSharp.RDD.IRdd, cField AS STRING) AS VOID
    LOCAL oType AS Type
    LOCAL oFieldInfo AS System.Reflection.FieldInfo
    TRY
        oType := oRdd:GetType()
        oFieldInfo := oType:GetField(cField, System.Reflection.BindingFlags.Public + System.Reflection.BindingFlags.NonPublic + System.Reflection.BindingFlags.Instance)
        IF oFieldInfo != NULL
            SELF:SetDetail((INT)eDetail , oFieldInfo:GetValue(oRdd):ToString())
        ELSE
            LOCAL oPropertyInfo := NULL AS System.Reflection.PropertyInfo
            DO WHILE oPropertyInfo == NULL .AND. oType != NULL
                oPropertyInfo := oType:GetProperty(cField, System.Reflection.BindingFlags.DeclaredOnly + System.Reflection.BindingFlags.Public + System.Reflection.BindingFlags.Instance)
                IF oPropertyInfo == NULL
                    oType := oType:BaseType
                END IF
            END DO
            IF oPropertyInfo == NULL
                //					MessageBox.Show("NotFound field " + cField)
                RETURN
            END IF
            SELF:SetDetail((INT)eDetail , oPropertyInfo:GetValue(oRdd):ToString())
        END IF
    CATCH /*e*/ AS System.Exception
            //			MessageBox.Show(e:ToString(), "Some error happened:")
    END TRY
    
METHOD FillFields(oRdd AS XSharp.RDD.IRdd) AS VOID
    LOCAL nFieldCount AS INT
    
    SELF:oFieldsListView:Items:Clear()
    
    nFieldCount := oRdd:FieldCount
    FOR LOCAL nField := 1 AS INT UPTO nFieldCount
        LOCAL oItem AS ListViewItem
        oItem := ListViewItem{}
        
        LOCAL oInfo AS XSharp.RDD.Support.RddFieldInfo
        oInfo := oRdd:GetField(nField)
        oItem:Text := oInfo:Name
        oItem:SubItems:Add(oInfo:FieldType:ToString())
        oItem:SubItems:Add(oInfo:Length:ToString())
        oItem:SubItems:Add(oInfo:Decimals:ToString())
        oItem:SubItems:Add(oRdd:GetValue(nField):ToString())
        SELF:oFieldsListView:Items:Add(oItem)
    NEXT
    
RETURN

METHOD FillIndexes(oRdd AS XSharp.RDD.IRdd) AS VOID
    
        //		LOCAL oOrderInfoMethod AS MethodInfo
        //		LOCAL oItmResultField AS FieldInfo
    LOCAL oIndexNode AS TreeNode
    LOCAL oOrderNode AS TreeNode
        //		LOCAL oDBOrderInfo AS Value
        //		LOCAL oStringValue AS Value
    LOCAL nCurrentOrder := 0 AS INT
    LOCAL lMultiOrder AS LOGIC
        //		LOCAL oType AS DebugType
        //		LOCAL aParams AS Value[]
    LOCAL cOldIndex AS STRING
        //		LOCAL oValue AS Value
    LOCAL cValue AS STRING
    LOCAL nOrders AS INT
    LOCAL n,m AS INT
    
        //		aParams := Value[]{2}
        //		oType := DebugType.Create(SELF:oDebugProcess , Nullable<DWORD>{} , "Vulcan.RDD.DBORDERINFO")
    
    SELF:oIndexTreeView:Nodes:Clear()
    LOCAL oInfo AS XSharp.RDD.Support.DbOrderInfo
    oInfo := XSharp.RDD.Support.DbOrderInfo{}
        //		oInfo:Order := 2
        /*		? 3,oRdd:OrderInfo(3,oInfo)
        ? 4,oRdd:OrderInfo(4,oInfo)
        ? 5,oRdd:OrderInfo(5,oInfo)
        ? 6,oRdd:OrderInfo(6,oInfo)
        ? 7,oRdd:OrderInfo(7,oInfo)
        ? 8,oRdd:OrderInfo(8,oInfo)
        ? 44,oRdd:OrderInfo(44,oInfo)
        ? "aaa",oInfo:BagName*/
    
    lMultiOrder := FALSE
    
    //		lMultiOrder := oExpression:Evaluate(SELF:oDebugProcess):AsString:Contains("CDX")
    
    //		oOrderInfoMethod := SELF:oRDDBaseType:GetMethod("OrderInfo")
    //		oItmResultField := oType:GetField("itmResult")
    
    //		oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
    //		aParams[1] := PrimitiveExpression{DBOI_NAME}:Evaluate(SELF:oDebugProcess)
    //		aParams[2] := oDBOrderInfo
    //		oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    //		cValue := oDBOrderInfo:GetFieldValue(oItmResultField):AsString
    
    //		aParams[1] := PrimitiveExpression{DBOI_NUMBER}:Evaluate(SELF:oDebugProcess)
    //		oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    //		cValue += " (" + oDBOrderInfo:GetFieldValue(oItmResultField):PrimitiveValue:ToString() + ")"
    
    cValue := "AAA"
    
    //		SELF:oIndexTreeView:Nodes:Add("Current Order = " + cValue)
    
    /*		aParams[1] := PrimitiveExpression{DBOI_EXPRESSION}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    cValue := oDBOrderInfo:GetFieldValue(oItmResultField):AsString*/
    cValue := "BBB"
    //		SELF:oIndexTreeView:Nodes:Add("Current Expression = " + cValue)
    
    IF lMultiOrder
        
        cOldIndex := ""
        n := 1
        DO WHILE TRUE .AND. n < 10
            
    /*				oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
    oDBOrderInfo:GetFieldValue(oType:GetField("itmOrder")):SetValue(PrimitiveExpression{n}:Evaluate(SELF:oDebugProcess))
    //		oDBOrderInfo:GetFiebldValue(oType:GetField("itmCobExpr")):SetValue(PrimitiveExpression{n}:Evaluate(SELF:oDebugProcess))
    aParams[1] := PrimitiveExpression{DBOI_INDEXNAME}:Evaluate(SELF:oDebugProcess)
    aParams[2] := oDBOrderInfo
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    oValue := oDBOrderInfo:GetFieldValue(oItmResultField)
    IF oValue:IsNull
    EXIT
    END IF
    cValue := oValue:AsString*/
            cValue := oRdd:OrderInfo(5,oInfo):ToString()
            
            IF cValue == cOldIndex
                EXIT
            END IF
            cOldIndex := cValue
            
    /*				aParams[1] := PrimitiveExpression{DBOI_FULLPATH}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    cValue += "  (" + oDBOrderInfo:GetFieldValue(oItmResultField):AsString + ")"*/
            
            cValue += "  (" + oRdd:OrderInfo(7,oInfo):ToString() + ")"
            
            oIndexNode := TreeNode{cValue}
            
    /*				oStringValue := Eval.NewString(SELF:oDebugProcess , cOldIndex)
    oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
    oDBOrderInfo:GetFieldValue(oType:GetField("sBagName")):SetValue(oStringValue)
    aParams[1] := PrimitiveExpression{DBOI_ORDERCOUNT}:Evaluate(SELF:oDebugProcess)
    aParams[2] := oDBOrderInfo
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    nOrders := (INT)oDBOrderInfo:GetFieldValue(oItmResultField):PrimitiveValue*/
            nOrders := (INT)oRdd:OrderInfo(DBOI_ORDERCOUNT,oInfo)
            
            FOR m := 1 UPTO nOrders
                nCurrentOrder ++
        /*					oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
        oDBOrderInfo:GetFieldValue(oType:GetField("itmOrder")):SetValue(PrimitiveExpression{nCurrentOrder}:Evaluate(SELF:oDebugProcess))
        aParams[1] := PrimitiveExpression{DBOI_NAME}:Evaluate(SELF:oDebugProcess)
        aParams[2] := oDBOrderInfo
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        cValue := oDBOrderInfo:GetFieldValue(oItmResultField):AsString
        
        aParams[1] := PrimitiveExpression{DBOI_EXPRESSION}:Evaluate(SELF:oDebugProcess)
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        cValue := cValue + "  (" + oDBOrderInfo:GetFieldValue(oItmResultField):AsString + ")"*/
                
                oInfo:Order := m
                cValue := oRdd:OrderInfo(DBOI_NAME,oInfo):ToString()
                
                oOrderNode := TreeNode{cValue}
                
        /*					aParams[1] := PrimitiveExpression{DBOI_ISCOND}:Evaluate(SELF:oDebugProcess)
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        oOrderNode:Nodes:Add(SELF:CreateNode("Descending" , oDBOrderInfo , oItmResultField))
        
        aParams[1] := PrimitiveExpression{DBOI_ISDESC}:Evaluate(SELF:oDebugProcess)
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        oOrderNode:Nodes:Add(SELF:CreateNode("Conditional" , oDBOrderInfo , oItmResultField))
        
        aParams[1] := PrimitiveExpression{DBOI_CONDITION}:Evaluate(SELF:oDebugProcess)
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        oOrderNode:Nodes:Add(SELF:CreateNode("Condition" , oDBOrderInfo , oItmResultField))
        
        aParams[1] := PrimitiveExpression{DBOI_RECNO}:Evaluate(SELF:oDebugProcess)
        oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
        oOrderNode:Nodes:Add(SELF:CreateNode("RecNo" , oDBOrderInfo , oItmResultField))*/
                
                oIndexNode:Nodes:Add(oOrderNode)
                oIndexNode:Expand()
                
            NEXT
            
            SELF:oIndexTreeView:Nodes:Add(oIndexNode)
            
            n ++
        END DO
        
    ELSE
        
        nOrders := (INT)oRdd:OrderInfo(DBOI_ORDERCOUNT,oInfo)
    /*			oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
    aParams[1] := PrimitiveExpression{DBOI_ORDERCOUNT}:Evaluate(SELF:oDebugProcess)
    aParams[2] := oDBOrderInfo
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    nOrders := (INT)oDBOrderInfo:GetFieldValue(oItmResultField):PrimitiveValue*/
        
        FOR n := 1 UPTO nOrders
    /*				oDBOrderInfo := Eval.NewObjectNoConstructor(oType)
    oDBOrderInfo:GetFieldValue(oType:GetField("itmOrder")):SetValue(PrimitiveExpression{n}:Evaluate(SELF:oDebugProcess))
    aParams[1] := PrimitiveExpression{DBOI_INDEXNAME}:Evaluate(SELF:oDebugProcess)
    aParams[2] := oDBOrderInfo
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    cValue := oDBOrderInfo:GetFieldValue(oItmResultField):AsString
    
    aParams[1] := PrimitiveExpression{DBOI_FULLPATH}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    cValue += " (" + oDBOrderInfo:GetFieldValue(oItmResultField):AsString + ")"
    
    oOrderNode := TreeNode{cValue}
    
    aParams[1] := PrimitiveExpression{DBOI_EXPRESSION}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    oOrderNode:Nodes:Add(TreeNode{"Expression = " + oDBOrderInfo:GetFieldValue(oItmResultField):AsString})
    
    aParams[1] := PrimitiveExpression{DBOI_ISCOND}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    oOrderNode:Nodes:Add(SELF:CreateNode("Descending" , oDBOrderInfo , oItmResultField))
    
    aParams[1] := PrimitiveExpression{DBOI_ISDESC}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    oOrderNode:Nodes:Add(SELF:CreateNode("Conditional" , oDBOrderInfo , oItmResultField))
    
    aParams[1] := PrimitiveExpression{DBOI_CONDITION}:Evaluate(SELF:oDebugProcess)
    oExpression:Evaluate(SELF:oDebugProcess):InvokeMethod(oOrderInfoMethod , aParams)
    oOrderNode:Nodes:Add(SELF:CreateNode("Condition" , oDBOrderInfo , oItmResultField))*/
            
            oInfo:Order := n
            cValue := oRdd:OrderInfo(5,oInfo):ToString()
            cValue += "  (" + oRdd:OrderInfo(7,oInfo):ToString() + ")"
            
            oOrderNode := TreeNode{cValue}
            
            SELF:oIndexTreeView:Nodes:Add(oOrderNode)
            oOrderNode:Expand()
        NEXT
        
    END IF
    
RETURN

END CLASS
END NAMESPACE
