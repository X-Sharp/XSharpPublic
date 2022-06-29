// 113. error XS0060: Inconsistent accessibility: base class 'Parent' is less accessible than class 'Child'
#pragma warnings(60, off) // inconsistend accessibility
INTERNAL CLASS Parent
END CLASS
CLASS Child INHERIT Parent
END CLASS

FUNCTION Start() AS VOID

RETURN
