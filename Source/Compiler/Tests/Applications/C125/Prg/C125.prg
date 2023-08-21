// 125. error XS0621: 'TestClass.Test': virtual or abstract members cannot be private
CLASS TestClass
PRIVATE PROPERTY Test AS INT GET 0
PRIVATE ACCESS Test2 AS INT
RETURN 0
END CLASS

