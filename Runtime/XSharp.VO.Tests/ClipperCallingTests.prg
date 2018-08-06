//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit


// WinBool test
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS ClipperCallingTests

		[Fact, Trait("Category", "Clipper")];
		METHOD Clippertest AS VOID
			Assert.Equal(0, Mtest() )
			Assert.Equal(1, Mtest(1) )
			Assert.Equal(2, Mtest(1,2) )
			Assert.Equal(3, Mtest(1,2,3) )
			Assert.Equal(4, Mtest(1,2,3,4) )
			Assert.Equal(0, SMtest() )
			Assert.Equal(1, SMtest(1) )
			Assert.Equal(2, SMtest(1,2) )
			Assert.Equal(3, SMtest(1,2,3) )
			Assert.Equal(4, SMtest(1,2,3,4) )
		    Assert.Equal( 1, Ftest( 1 ) )   
		    Assert.Equal( 1, Ftest( 2 ) )   
		    Assert.Equal( 2, Ftest( 1, 2 ) )   
		    Assert.Equal( true, TestFunc( 1, "2" ) )   
		    Assert.Equal( true, TestFunc( 1, "2", 3 ) )   
			Assert.Equal( false, TestFunc() )   

      Assert.Equal( true, TestFunc4( 1 ) == 1 )
      Assert.Equal( true, TestFunc4( "2" ) == "2" )
      Assert.Equal( true, TestFunc4( 123.456 ) == 123.456 )
      Assert.Equal( true, TestFunc4( 1957.12.20 ) == 1957.12.20 )
      Assert.Equal( true, TestFunc4( TRUE ) == .T. )
      Assert.Equal( true, TestFunc4( FALSE ) == .F. )
      Assert.Equal( true, TestFunc5( , 1 ) == 1 )
      Assert.Equal( true, TestFunc5( , "2" ) == "2" )
      Assert.Equal( true, TestFunc5( , 123.456 ) == 123.456 )
      Assert.Equal( true, TestFunc5( , 1957.12.20 ) == 1957.12.20 )
      Assert.Equal( true, TestFunc5( , TRUE ) == .T. )
      Assert.Equal( true, TestFunc5( , FALSE ) == .F. )
      Assert.Equal( true, TestFunc5() == NIL )
      Assert.Equal( true, TestFunc5( 1 ) == NIL )

		METHOD MTest( x, y, z ) AS INT CLIPPER
			RETURN pcount()
      
		STATIC METHOD SMtest( x, y, z ) AS INT CLIPPER
			RETURN pcount()

		
	END CLASS
END NAMESPACE


     
STATIC FUNCTION FTest( x, y, z ) AS INT CLIPPER
   RETURN pcount()
   
STATIC FUNCTION TestFunc( x, y ) AS LOGIC CLIPPER
   RETURN x == 1 && y == "2"

  
STATIC FUNCTION TestFunc4( x ) AS USUAL CLIPPER
   RETURN  _GetFParam( 1 ) 
   
STATIC FUNCTION TestFunc5( x, y ) AS USUAL CLIPPER
   RETURN _GetFParam( 2 )          