//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Text
USING XUnit


BEGIN NAMESPACE XSharp.VO.Tests

	CLASS MemoryTests
		[Fact, Trait("Category", "Memory")]; 
		UNSAFE METHOD MemoryTest() AS VOID
			LOCAL p,q AS IntPtr
			p := MemAlloc(10)
			q := MemAlloc(20)
			Assert.NotEqual(NULL, p)
			Assert.NotEqual(p,q)
			Assert.Equal(MemFree(p),0)
			Assert.Equal(MemFree(q),0)
			Assert.True(MemFree(q)!=0) 
			Assert.False(MemFree(q)==0)
			LOCAL pMem AS BYTE PTR
			pMem := MemAlloc(10)
			MemSet(pMem, 123, 5)
			Assert.Equal(pMem[1], 123)
			Assert.Equal(pMem[2], 123)
			Assert.Equal(pMem[3], 123)
			Assert.Equal(pMem[4], 123)
			Assert.Equal(pMem[5], 123)
			Assert.Equal(pMem[6], 0)
			Assert.Equal(pMem[7], 0)
			Assert.Equal(pMem[8], 0)
			Assert.Equal(pMem[9], 0)
			Assert.Equal(pMem[10], 0)
			LOCAL pZero AS BYTE PTR
			pZero := MemByte(pMem, 0, 10)
			Assert.Equal(pZero[1], 0)
			Assert.Equal( ((DWORD) pMem + 5), (DWORD) pZero)
			LOCAL pMem2 AS BYTE PTR
			pMem2 := MemAlloc(10)
			MemCopy(pMem2, pMem, 10)
			//Assert.Equal( PtrLen(pMem) > 10 , TRUE)
			//Assert.Equal( PtrLenWrite(pMem)> 10, TRUE)
			MemFree(pMem)
			MemFree(pMem2)
		RETURN
		
		[Fact, Trait("Category", "Memory")]; 
		UNSAFE METHOD MemoryTest2() AS VOID
			LOCAL pMem AS IntPtr
			pMem := MemAlloc(100)
			MemSet(pMem, 0xFF, 100)
			Assert.True( MemByte(pMem, 0xFF, 100)== pMem)
			Assert.True( MemWord(pMem, 0xFFFF, 50)== pMem)
			Assert.True( MemDWord(pMem, 0xFFFFFFFFU, 25)== pMem)
			Assert.True( MemInt(pMem, -1, 25)== pMem)
			Assert.True( MemShort(pMem, -1, 50) == pMem)

	END CLASS
END NAMESPACE
