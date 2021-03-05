// C769. Compiler crash with implementing interface indexer

USING System.ComponentModel

FUNCTION Start( ) AS VOID
	// the following 2 did not compiler in the previous compiler either, they report
	// error XS0021: Cannot apply indexing with [] to an expression of type 'Test1'
	// Am I wrong thinking they should be working?
	LOCAL o1 := Test1{} AS IDataErrorInfo
	? o1["abc"]
	? o1:Item["abc"]

	VAR o2 := (IDataErrorInfo) Test2{}
	? o2["abc"]
	? o2:Item["abc"]
	VAR o3 := Test3{}
	? o3["abc"]
	? o3:Item["abc"]
RETURN

CLASS Test1 IMPLEMENTS System.ComponentModel.IDataErrorInfo
	PROPERTY System.ComponentModel.IDataErrorInfo.Item[cColumnName AS STRING] AS STRING
		GET
			RETURN cColumnName + "123"
		END GET
	END PROPERTY
	PROPERTY System.ComponentModel.IDataErrorInfo.Error AS STRING GET NULL
END CLASS

CLASS Test2 IMPLEMENTS IDataErrorInfo
	PROPERTY IDataErrorInfo.Item[cColumnName AS STRING] AS STRING
		GET
			RETURN cColumnName + "123"
		END GET
	END PROPERTY
	PROPERTY IDataErrorInfo.Error AS STRING GET NULL
END CLASS



// This was not working with the previous compiler either, reports
// error XS0535: 'Test3' does not implement interface member 'System.ComponentModel.IDataErrorInfo.this[string].get'
// Shouldn't it work though?
// Changing to using SELF instead of Item does work ok as expected
CLASS Test3 IMPLEMENTS IDataErrorInfo
	PROPERTY SELF[cColumnName AS STRING] AS STRING
//	PROPERTY Item[cColumnName AS STRING] AS STRING
		GET
			RETURN cColumnName + "123"
		END GET
	END PROPERTY
	PROPERTY Error AS STRING GET NULL
END CLASS

