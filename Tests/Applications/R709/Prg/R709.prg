Using System
Using System.Collections.Generic
Using System.Text
// test case for https://github.com/X-Sharp/XSharpPublic/issues/376




FUNCTION Start() As Void Strict
	try	
	Var oTest = TestClass{}
	
		oTest.DemoCsharpDllRequiredError(123)
	catch E AS EXCEPTION		
		MessageBox(e:ToString())
	END TRY
	Return
	
END FUNCTION
	

Define Class TestClass As Custom
	
	Public cTable = "Customers"
	Public cPkField = "ID"
	
	*---------------------------------------------------------------------------------
	Function DemoCsharpDllRequiredError(uValue)
  
		Local lcSql

		Text To lcSql Textmerge Noshow
			Select * From <<This.cTable>> Where <<This.cPkField>> = <<uValue>>
		Endtext
  
		? lcSql
	
		Wait
		
		Return
  
	End Function
  
End Define
