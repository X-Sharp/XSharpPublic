USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime
using System.Diagnostics

BEGIN NAMESPACE XSharp.Runtime.Tests
	[TestClass];
	CLASS DynamicTests

		[TestMethod];
		METHOD Usual2DynTest() as void
			local u as ___Usual
			u:=___Usual{}
			u:=___Usual{5}
		RETURN

		[TestMethod];
		METHOD PerfTest() as void
			local  i as int 
			local  u as __Usual
			local  u2 as ___Usual
			var stopWatch := Stopwatch{}
			stopWatch:Start()
			for i:=1 upto 1000000
				u := __Usual{i}
				u:= "Text"
				u:=0
				if (u)
				   u := true
				endif
			next
			stopWatch:Stop()
			var  ts := stopWatch:Elapsed
			var  elapsedTime := String.Format("{0:00}:{1:00}:{2:00}.{3:00}",ts:Hours, ts:Minutes, ts:Seconds,ts:Milliseconds / 10)
			Console.WriteLine("RunTime " + elapsedTime)
			stopWatch:Start()
			for i:=1 upto 1000000
				u2 := ___Usual{i}
				u2:Value := "Text"
				u2:Value:=0
				if ((int)u2:Value==0)
				   u2:Value := true
				endif
			next
			stopWatch:Stop()
			ts := stopWatch:Elapsed
			elapsedTime := String.Format("{0:00}:{1:00}:{2:00}.{3:00}",ts:Hours, ts:Minutes, ts:Seconds,ts:Milliseconds / 10)
			Console.WriteLine("RunTime " + elapsedTime)
		return

	END CLASS

	public class ___Usual implements IComparable
		private dyn    as dynamic
		private voType as VODataType

		#region constructors
		constructor()
			voType := VODataType.VONIL
		return 
		constructor(i as int)
			dyn := i
			voType := VODataType.VOINT
		return 
		public  constructor(u as ___Usual)
			dyn := u
			voType := u:VOType
		return
		public  constructor(l as Logic)
			dyn := l
			voType := VODataType:VOLOGIC
		return
		public  constructor(a as __Array)
			dyn := a
			voType := VODataType:VOARRAY
		return
		public  constructor(dt as System.DateTime)
			dyn := dt
			voType := VODataType:VOOBJECT
		return
		public constructor(i as Int64)
			dyn := i
			voType := VODataType:VOINT64
		return
		public constructor(p as System.IntPtr)
			dyn := p
			voType := VODataType:VOPTR
		return
	    public constructor(o as Object)
			dyn := o
			voType := VODataType:VOOBJECT
		return
		public constructor(s as string)
			dyn := s
			voType := VODataType:VOSTRING
		return
		public constructor(d as __VODate)
			dyn := d
			voType := VODataType:VODATE
		return
		public constructor(p as PSZ)
			dyn := p
			voType := VODataType:VOPSZ
		return
		public constructor(s as __Symbol)
			dyn := s
			voType := VODataType:VOSYMBOL
		return
		#endregion
		#region properties
		public property VODataType as VODataType
			get
				return voType
			end get
		end property
		public property Value as object
			get
				return dyn
			end get
			set 
			    dyn := value
			end set
		end property
		#endregion
		#region implementation IComparable
		public method CompareTo(o as Object) as Long

		if ( o:GetType() == typeof(___Usual) )
		   local u := (___Usual)o as ___Usual
		   if ( u:Value:GetType() == self:Value:GetType() ) 
				if (self:Value is IComparable)
					local compareTo := ( IComparable) self:Value as IComparable
					return compareTo:CompareTo(u:Value)
				else
					/// type does not supprt IComparable
				endif
		   else
		      /// Type are diffent question is if there is a meaning full comparision
		   endif
		else
		   /// We compare against something which is not a ___Usual
		   if ( self:Value:GetType() == o:GetType() )
		      if (self:Value is IComparable)
		         local compareTo := ( IComparable) self:Value as IComparable
			     return compareTo:CompareTo(o)
			  else
			     /// type does not supprt IComparable
			  endif
		   else
		      /// Type are diffent question is if there is a meaning full comparision
		   endif
		endif

		return 0
		#endregion
	end class

	public enum VODataType as Long
		member VOARRAY:=5
		member VOCODEBLOCK:=9
		member VODATE:=2
		member VOFLOAT:=3
		member VOINT:=1
		member VOINT64:=0x16
		member VOLOGIC:=8
		member VONIL:=0
		member VOOBJECT:=6
		member VOPSZ:=0x11
		member VOPTR:=0x12
		member VOSTRING:=7
		member VOSYMBOL:=10
	end enum

END NAMESPACE // XSharp.Runtime.Tests