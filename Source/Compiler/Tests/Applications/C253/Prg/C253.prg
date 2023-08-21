// 253. error XS0200: Property or indexer '__Usual.Value' cannot be assigned to -- it is read only
// lb+
CLASS TestClass
EXPORT Value AS INT
END CLASS
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := TestClass{}
u:Value := 123
? u:Value

