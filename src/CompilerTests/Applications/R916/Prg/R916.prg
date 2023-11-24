// See https://github.com/X-Sharp/XSharpPublic/issues/1385

FUNCTION Start as VOID
    var oClass := MyClass{}
    ? oClass
    RETURN
CLASS MyClass
  PROTECTED MyVariable AS STRING
  PROTECTED MyVariable AS STRING
  PROTECTED MyVariable AS STRING
END CLASS

