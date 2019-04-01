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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS MiscTests

		[Fact, Trait("Category", "Misc")];
		METHOD IntLiteralTest() AS VOID
			LOCAL dec AS DWORD
			LOCAL thou AS DWORD
	
			dec := SetDecimalSep(Asc(","))
			thou := SetThousandSep(Asc("."))
			Assert.Equal(" 12.345,00" , Transform(12345.0, "999,999.99"))
			// this fails:
			Assert.Equal(" 12.345,00" , Transform(12345, "999,999.99"))
	        
			SetDecimalSep(Asc("."))
			SetThousandSep(Asc(","))
			Assert.Equal(" 12,345.00" , Transform(12345.0, "999,999.99"))
			Assert.Equal(" 12,345.00" , Transform(12345, "999,999.99"))
	
			SetDecimalSep(dec)
			SetThousandSep(thou)
		RETURN

		[Fact, Trait("Category", "SplitPath")];
		METHOD SplitPathTests() AS VOID
			LOCAL cDrive := "",cDir := "",cFile := "",cExt := "" AS STRING
			SplitPath("C:\folder\file.ext" ,  REF cDrive , REF cDir , REF cFile , REF cExt)
			
			Assert.Equal("C:" , cDrive)
			Assert.Equal("\folder\" , cDir)
	
			Assert.Equal("file" , cFile)
			Assert.Equal(".ext" , cExt)
		RETURN
	
		[Fact, Trait("Category", "SplitPathVO")];
		METHOD SplitPathVO() AS VOID
			LOCAL cPath ,cDrive ,cDir ,cFile ,cExt AS STRING
			LOCAL pszDrive, pszDir AS PSZ
			LOCAL pszFile, pszExt AS PSZ
		
			cPath := "C:\testfolder\testfile.prg"
		
			pszDrive := MemAlloc(2+1)
			pszDir   := MemAlloc(255+1)
			pszFile  := MemAlloc(255+1)
			pszExt   := MemAlloc(7+1) 
		
			SplitPath( String2Psz(cPath), pszDrive, pszDir, pszFile, pszExt)
		
			cDrive := Psz2String(pszDrive)
			cDir   := Psz2String(pszDir)
			cFile  := Psz2String(pszFile)
			cExt   := Psz2String(pszExt)
		
			MemFree(pszDrive)
			MemFree(pszDir)
			MemFree(pszFile)
			MemFree(pszExt)

            Assert.Equal("C:" , cDrive)
			Assert.Equal("\testfolder\" , cDir)
			Assert.Equal("\testfolder\" , cDir)
			Assert.Equal("testfile" , cFile)
			Assert.Equal(".prg" , cExt)
		RETURN
	
		[Fact, Trait("Category", "Directory")];
		METHOD DirectoryTests() AS VOID
			Assert.Equal(0u , ALen(Directory("K:\drive does not exist")))
			Assert.Equal(0u , ALen(Directory("K:\drive does not exist","V")))
			Assert.Equal(0u , ALen(Directory("C:\invalid:path")))
			Assert.Equal(1u , ALen(Directory("C:\invalid:path","V")))
			Assert.Equal(0u , ALen(Directory("C:invalid:path")))
			Assert.Equal(1u , ALen(Directory("C:invalid:path","V")))
			Assert.Equal(0u , ALen(Directory("N:")))
			Assert.Equal(0u , ALen(Directory("N:","V")))
	
		[Fact, Trait("Category", "Empty")];
		METHOD EmptyFuncTests() AS VOID
			Assert.True(Empty(0))
			Assert.True(Empty(FALSE))
			Assert.True(Empty(""))
			Assert.True(Empty(Chr(10) + Chr(9) + Chr(13)))
			Assert.True(Empty(NULL_SYMBOL))
			Assert.True(Empty(0000.00.00))
			Assert.True(Empty({}))
	
			Assert.True(Empty(NULL_PTR))
			LOCAL p AS PTR
			p := NULL_PTR
			Assert.True(Empty(p))
	
			Assert.True(Empty(NULL_PSZ))
			LOCAL z AS PSZ
			z := NULL_PSZ
			Assert.True(Empty(z))
			
			
	
			Assert.False(Empty(1))
			Assert.False(Empty(TRUE))
			Assert.False(Empty("a"))
			Assert.False(Empty(Chr(1)))
			Assert.False(Empty(#abc))
			Assert.False(Empty(2000.01.01))
			Assert.False(Empty({{}}))
			
			p := @p
			Assert.False(Empty(p))
			z := String2Psz("test")
			Assert.False(Empty(z))


		[Fact, Trait("Category", "Empty")];
		METHOD W2String_tests() AS VOID
			LOCAL cText := "The brown fox does the funny stuff that we all know" AS STRING
			Assert.Equal(cText , W2String(String2W(cText)))

	END CLASS

END NAMESPACE
