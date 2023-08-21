// 149. assertion failed and compiler crash with different casing in namespace names
BEGIN NAMESPACE abc.def
CLASS Test1
END CLASS
END NAMESPACE
   
BEGIN NAMESPACE ABC.def
CLASS Test2 INHERIT System.Collections.ArrayList
END CLASS
END NAMESPACE

