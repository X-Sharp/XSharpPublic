// 798. Compiler crash overriding CLIPPER method with STRICT (when return type is strongly typed)
// this should report an error:
CLASS Parent
	VIRTUAL METHOD test() AS LOGIC CLIPPER
	RETURN FALSE
END CLASS

CLASS Child INHERIT Parent
	VIRTUAL METHOD test() AS LOGIC PASCAL
	RETURN FALSE
END CLASS
