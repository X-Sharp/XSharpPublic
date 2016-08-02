// Usual.prg
// Created by    : Meinhard
// Creation Date : 16.05.2016 11:51:51
// Created for   : 
// WorkStation   : MSMLENOVO

/*
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE Vulcan

	public class ___Usual implements IComparable
		private dyn    as dynamic
		private voType as VODataType

		#region constructors
		constructor()
			voType := VODataType.VO__Usual._NIL
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
		public constructor(s as string)
			dyn := s
			voType := VODataType:VOSTRING
		return
		public constructor(c as char)
			dyn := c:ToString()
			voType := VODataType:VOSTRING
		return
		public constructor(d as __VODate)
			dyn := d
			voType := VODataType:VODATE
		return
		public constructor(p as __Psz)
			dyn := p
			voType := VODataType:VOPSZ
		return
		public constructor(s as __Symbol)
			dyn := s
			voType := VODataType:VOSYMBOL
		return
		public constructor(d as double)
			dyn := d
			voType := VODataType:VOFLOAT
		return
	    public constructor(o as Object)
			if ( o != null )
				var varType := o:GetType()
				if ( varType == typeof(___Usual) )
				   /// unbox usual
				   dyn := ((___Usual)o):Value
				   votype := ((___Usual)o):VODataType
				else
					var typeCode := System.Type.GetTypeCode(varType)
					switch typeCode
					case  System.TypeCode.DBNull
						dyn := null
						voType := VODataType.VO__Usual._NIL
					case System.TypeCode.Boolean
						self( (Logic)o )
					case System.TypeCode.SByte
						self( (SByte)o )
					case System.TypeCode.Byte
						self( (Byte)o )
					case System.TypeCode.Int16
						self( (Short)o )
					case System.TypeCode.UInt16
						self( (Word)o )
					case System.TypeCode.Int32
						self( (int)o )
					case System.TypeCode.UInt32
						if ((DWord)o  <= 0x7fffffff)
							self( (int)o )
						else
							self( (double)o )
						endif
					case System.TypeCode.Int64
						self( (int64)o )
					case System.TypeCode.UInt64
						self( (int64)o )
					case System.TypeCode.Single
						self( (double)o )
					case System.TypeCode.Double
						self( (double)o )
					case System.TypeCode.DateTime
						self( (DateTime)o )
					case System.TypeCode.String
						self( (string)o )	
					case System.TypeCode.Char
						self( (char)o )												
					otherwise
						dyn := o
						voType := VODataType:VOUnknown
						if ( typeCode == System.TypeCode.Object ) && ( varType == typeof(__Array))
						   self((__Array)o)
						else
							if (typeCode == System.TypeCode.Object ) && ( varType == typeof(__VODate))
							   self((__VODate)o)
							endif
						endif
					end switch
			    endif
			endif
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
		member VOUnknown := -1
	end enum
END NAMESPACE // XSharp.Runtime.Tests
*/