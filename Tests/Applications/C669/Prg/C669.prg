// 669. error XS1503: Argument 1: cannot convert from 'int' to 'string'

FUNCTION Start() AS VOID
	// Following ccode in c# compiles without errors
	// In x#, it reports error XS1503: Argument 1: cannot convert from 'int' to 'string'
	LOCAL oChartAreaCollection AS System.Windows.Forms.DataVisualization.Charting.ChartAreaCollection
	IF FALSE
		? oChartAreaCollection[0]
	ENDIF

	// Basically the class hierarchy of oChartAreaCollection is similar to the one below
	// But this one does compile with no issues. Apparently there's something else getting in the way in the ChartAreaCollection class
	LOCAL o AS TestClass
	o := TestClass{}
	? o[10]
RETURN



CLASS BaseClass           
	PRIVATE testbase AS INT
	PROPERTY SELF[n AS INT] AS INT GET n * 2  SET testbase := VALUE
//	PROPERTY SELF[n AS LOGIC] AS STRING GET n:ToString()
END CLASS
CLASS Foo INHERIT BaseClass
END CLASS
CLASS MiddleClass INHERIT Foo                                    
	PRIVATE test AS STRING
	PROPERTY SELF[n AS STRING] AS STRING GET n SET test := VALUE
END CLASS
CLASS TestClass INHERIT MiddleClass
END CLASS
