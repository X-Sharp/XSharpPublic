USING System.Drawing
USING System.Windows.Forms
USING System.Reflection

PARTIAL CLASS UnitForm INHERIT System.Windows.Forms.Form

	CONSTRUCTOR()
		
		SUPER()
		
		SELF:InitializeForm()
		
		SELF:oTestsList:Columns:Add("Test name" , 300)
		SELF:oTestsList:Columns:Add("Status" , 80)
		SELF:oTestsList:Columns:Add("Failed / Tests" , 100 , HorizontalAlignment.Right)
		
	RETURN
	
	METHOD AddTest(oTest AS TestInfo) AS VOID
		LOCAL oItem AS ListViewItem
		oItem := ListViewItem{}
		oItem:Text := oTest:Type:Name + " : " + oTest:Test:Name
		oItem:SubItems:Add("Not run")
		oItem:SubItems:Add("?")
		oItem:Tag := oTest
		SELF:oTestsList:Items:Add(oItem)
	RETURN
	
	
	METHOD RunAllButtonClick(sender AS System.Object , e AS System.EventArgs) AS VOID
		FOREACH oItem AS ListViewItem IN SELF:oTestsList:Items
			LOCAL oTest AS TestInfo
			oTest := oItem:Tag ASTYPE TestInfo
			SELF:RunTest(oTest)
		NEXT
		SELF:ShowResults()
	RETURN
	
	METHOD RunSelectedButtonClick(sender AS System.Object , e AS System.EventArgs) AS VOID
		FOREACH oItem AS ListViewItem IN SELF:oTestsList:SelectedItems
			LOCAL oTest AS TestInfo
			oTest := oItem:Tag ASTYPE TestInfo
			SELF:RunTest(oTest)
		NEXT
		SELF:ShowResults()
	RETURN

	METHOD RunTest(oTest AS TestInfo) AS VOID
		LOCAL oInstance AS OBJECT
		
		XideUnitTest.CurrentTest := oTest
		oTest:Reset()
		
		oInstance := oTest:Type:GetConstructors()[1]:Invoke(NULL)

		LOCAL oExpected := NULL AS OBJECT
		oExpected := oTest:Test:GetCustomAttribute(TypeOf(ExpectedExceptionAttribute))
		
		IF oTest:Type:GetMethod("Init") != NULL
			oTest:Type:GetMethod("Init"):Invoke(oInstance , NULL)
		END IF
		
		LOCAL oException := NULL AS Exception
		TRY
			oTest:Test:Invoke(oInstance , NULL)
		CATCH e AS Exception
			oException := e
			IF oException:InnerException != NULL
				oException := oException:InnerException
			END IF
		END TRY
		
		IF oExpected == NULL
			IF oException != NULL
				oTest:SubTestRun(FALSE, "Intercepted unexpected exception: " + oException:ToString() , "" , 0)
			END IF
		ELSE
			IF oException == NULL
				oTest:SubTestRun(FALSE, "Was expecting exception: " + oExpected:ToString() , "" , 0)
			END IF
		END IF
		
		LOCAL oItem AS ListViewItem
		oItem := SELF:GetTestItem(oTest)
		IF oItem != NULL
			IF oTest:Success
				oItem:ForeColor := Color.DarkGreen
				oItem:SubItems[1]:Text := "Success"
				oItem:SubItems[2]:Text := oTest:SubTests:Count:ToString()
			ELSE
				oItem:ForeColor := Color.Red
				oItem:SubItems[1]:Text := "FAILED!"
				oItem:SubItems[2]:Text := oTest:CountFailedSubTests() + "/" + oTest:SubTests:Count:ToString()
			END IF
		END IF
		
/*		IF oException != NULL
			oItem:SubItems[1]:Text := oException:GetType():ToString()
		END IF*/
		
	RETURN
	
	METHOD ShowResults() AS VOID
		LOCAL nFailed := 0, nTotal := 0 AS INT
		FOREACH oItem AS ListViewItem IN SELF:oTestsList:Items
			LOCAL oTest AS TestInfo
			oTest := oItem:Tag ASTYPE TestInfo
			nFailed += oTest:CountFailedSubTests()
			nTotal += oTest:SubTests:Count
		NEXT
		SELF:Text := System.String.Format("Failed {0} out of {1} tests" , nFailed , nTotal)
	RETURN
	
	METHOD GetTestItem(oTest AS TestInfo) AS ListViewItem
		FOREACH oItem AS ListViewItem IN SELF:oTestsList:Items
			IF oItem:Tag == oTest
				RETURN oItem
			END IF
		NEXT
	RETURN NULL

	METHOD TestsList_SelectedIndexChanged(sender AS System.Object , e AS System.EventArgs) AS VOID
		SELF:SetText()
	RETURN
	
	METHOD SetText() AS VOID
		STATIC LOCAL oText := System.Text.StringBuilder{} AS System.Text.StringBuilder
		oText:Clear()
		FOREACH oItem AS ListViewItem IN SELF:oTestsList:SelectedItems
			LOCAL oTest AS TestInfo
			oTest := oItem:Tag ASTYPE TestInfo
			IF oTest != NULL
				IF .not. oTest:Success
					FOREACH oSubTest AS SubTestInfo IN oTest:SubTests
						IF .not. oSubTest:Success
							oText:Append("Test failed in ")
							oText:Append(Xide.Unit.Assert.GetFileName(oSubTest:FileName))
							oText:Append(", line ")
							oText:Append(oSubTest:Line)
							TRY
								LOCAL aLines := NULL AS STRING[]
								LOCAL cCurrent := NULL AS STRING
								IF oSubTest:FileName != cCurrent
									aLines := System.IO.File.ReadAllLines(oSubTest:FileName)
									cCurrent := oSubTest:FileName
									oText:AppendLine()
									oText:AppendLine()
									oText:Append(aLines[oSubTest:Line]:Trim())
								END IF
								
							END TRY
							oText:AppendLine()
							oText:AppendLine()
							oText:Append(oSubTest:Message)
							oText:AppendLine()
							oText:AppendLine("--------------------------------------------------------------------------")
						END IF
					NEXT
				END IF
			END IF
		NEXT
		
		IF oText:Length == 0
			oText:Append("No subtests failed")
		END IF
		
		SELF:oInfoText:Text := oText:ToString()
		
	RETURN
	
END CLASS

