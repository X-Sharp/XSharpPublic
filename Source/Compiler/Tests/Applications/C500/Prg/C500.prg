// 500. error XS9002: Parser: unexpected input '._gdImageCreate@'
// to be honest I have no idea what that syntax means at the end of the _DLL function
// vulcan does compile it, though.

_DLL FUNCTION gdImageCreate( sx AS INT, sy AS INT ) AS strGDImage STRICT:bgd._gdImageCreate@8
//BGD_DECLARE(gdImagePtr) gdImageCreate (int sx, int sy);

_DLL FUNCTION gdImageCreateTrueColor( sx AS INT, sy AS INT ) AS strGDImage STRICT:bgd._gdImageCreateTrueColor@8  
//BGD_DECLARE(gdImagePtr) gdImageCreateTrueColor (int sx, int sy);

_DLL FUNCTION gdImageCreateFromPng( fd AS PTR ) AS strGDImage STRICT:bgd._gdImageCreateFromPng@4 	
//BGD_DECLARE(gdImagePtr) gdImageCreateFromPng (FILE * fd);
//BGD_DECLARE(gdImagePtr) gdImageCreateFromPngCtx (gdIOCtxPtr in);

_DLL FUNCTION gdImageColorClosestHWB( gdImagePtr AS strGDImage, nRed AS INT, nGreen AS INT, nBlue AS INT ) AS INT STRICT:bgd._gdImageColorClosestHWB@16 
//BGD_DECLARE(int) gdImageColorClosestHWB (gdImagePtr im, int r, int g, int b);


FUNCTION Start() AS VOID

RETURN



VOSTRUCT strGDImage
	MEMBER pImg AS PTR
