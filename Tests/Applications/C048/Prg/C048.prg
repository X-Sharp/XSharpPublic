// 48. error XS0200: Property or indexer 'TestClass.Test' cannot be assigned to -- it is read only
CLASS TestClass
METHOD TestMethod(n AS INT) AS VOID
SELF:Test := n
PROPERTY Test AS INT AUTO
END CLASS

