// 190. error XS1503: Argument 1: cannot convert from 'string' to 'int'
FUNCTION Start() AS VOID
LOCAL o AS System.Collections.Generic.Dictionary<STRING,STRING>
o := System.Collections.Generic.Dictionary<STRING,STRING>{}
o:Add("test" , "test")
? o:Item["test"]

