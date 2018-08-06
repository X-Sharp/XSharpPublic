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
USING System.Diagnostics


BEGIN NAMESPACE XSharp.Core.Tests
	//[TestClass];
	//CLASS DynamicTests
//
		//[TestMethod];
		//METHOD Usual2DynTest() as void
			//local u as ___Usual
			//u:=___Usual{}
			//u:=___Usual{5}
		//RETURN
//
		//[TestMethod];
		//METHOD PerfTest() as void
			//local  i as int 
			//local  u as __Usual
			//local  u2 as ___Usual
			//var stopWatch := Stopwatch{}
			//stopWatch:Start()
			//for i:=1 upto 1000000
				//u := __Usual{i}
				//u:= "Text"
				//u:=0
				//if (u)
				   //u := true
				//endif
			//next
			//stopWatch:Stop()
			//var  ts := stopWatch:Elapsed
			//var  elapsedTime := String.Format("{0:00}:{1:00}:{2:00}.{3:00}",ts:Hours, ts:Minutes, ts:Seconds,ts:Milliseconds / 10)
			//Console.WriteLine("RunTime " + elapsedTime)
			//stopWatch:Start()
			//for i:=1 upto 1000000
				//u2 := ___Usual{i}
				//u2:Value := "Text"
				//u2:Value:=0
				//if ((int)u2:Value==0)
				   //u2:Value := true
				//endif
			//next
			//stopWatch:Stop()
			//ts := stopWatch:Elapsed
			//elapsedTime := String.Format("{0:00}:{1:00}:{2:00}.{3:00}",ts:Hours, ts:Minutes, ts:Seconds,ts:Milliseconds / 10)
			//Console.WriteLine("RunTime " + elapsedTime)
		//return
//
	//END CLASS

END NAMESPACE // XSharp.Runtime.Tests