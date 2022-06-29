// 80. error XS0666: 'MyStruct.c': new protected member declared in struct
// this does compile in vulcan!!!
#pragma warnings(666, off) // new protected member in struct
STRUCTURE MyStruct
	PROTECT c AS STRING
END STRUCTURE

