// 57. error XS0171: Field 'TestStruc.c' must be fully assigned before control is returned to the caller
// error XS0171: Field 'TestStruc.i' must be fully assigned before control is returned to the caller

STRUCTURE TestStruc
	EXPORT c AS STRING
	EXPORT i AS INT
	CONSTRUCTOR(n AS INT)
END STRUCTURE

