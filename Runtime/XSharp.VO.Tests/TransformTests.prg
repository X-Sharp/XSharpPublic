USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using System.Globalization


BEGIN NAMESPACE XSharp.VO.Tests

	class TransFormTests

		[Fact, Trait("Category", "TransForm")];
		method LogicalTest() as void 
			// with @R other chars are appended, but only a single YN or TF flag is written. Other positions become a space
			Assert.Equal("Y", TransForm(true, "Y"))
			Assert.Equal("N", TransForm(false, "Y"))
			Assert.Equal("T", TransForm(true, "L"))
			Assert.Equal("F", TransForm(false, "L"))
			Assert.Equal("T x", TransForm(true, "@R L x"))
			Assert.Equal("F x", TransForm(false, "@R L x"))
			Assert.Equal("Y x", TransForm(true, "@R YYx"))
			Assert.Equal("N x", TransForm(false, "@R YYx"))

		[Fact, Trait("Category", "TransForm")];
		method DateTest() as void 
			// with @R other chars are appended, but only a single YN or TF flag is written. Other positions become a space
			Assert.Equal("10/12/2010", TransForm(2010.12.10,"@E"))
			Assert.Equal("20/11/2010", TransForm(2010.11.20,"@E"))
			SetDateFormat("dd-mm-yyyy")
			Assert.Equal("20-11-2010", TransForm(2010.11.20,"@D"))
			SetDateFormat("dd-mm-yy")
			Assert.Equal("20-11-10", TransForm(2010.11.20,"@D"))

		[Fact, Trait("Category", "TransForm")];
		method StringTest() as void 
			Assert.Equal("ABC", Transform("abc", "@!"))
			Assert.Equal("AbC", Transform("abc", "!a!"))
			Assert.Equal("1234 AB", Transform("1234ab", "@R! 9999 AA"))
			Assert.Equal("1234 AB", Transform("1234ab", "@R 9999 !!"))

		[Fact, Trait("Category", "TransForm")];
		method LongTest() as void 
			Assert.Equal("  1", Transform(1, "999"))
			Assert.Equal("123", Transform(123, "###"))
			Assert.Equal("23", Transform(123, "##"))			// todo: should return **
			Assert.Equal("23", Transform(123, "99"))			// todo: should return **
			Assert.Equal("(1)", Transform(-1, "@( 99"))
			Assert.Equal("(-1)", Transform(-1, "@( 999"))
			Assert.Equal("( -1)", Transform(-1, "@( 9999"))
			Assert.Equal(" (-1)", Transform(-1, "@) 9999"))
			Assert.Equal("  (-1)", Transform(-1, "@) 99999"))
			Assert.Equal("   ", Transform(0, "@Z 999"))
			Assert.Equal(" -1 DB", Transform(-1, "@X 999"))
			Assert.Equal(" -1 DB", Transform(-1, "@XC 999"))
			Assert.Equal("  1 CR", Transform(1, "@XC 999"))
			Assert.Equal("  1 CR", Transform(1, "@C 999"))
			Assert.Equal("  0", Transform(0, "@C 999"))
			Assert.Equal("1,234", Transform(1234, "9,999"))

		[Fact, Trait("Category", "TransForm")];
		method FLoatTest() as void 
			SetDecimalSep (  46)	// .
			SetThousandSep (  44)	// ,
			Assert.Equal("1.23", Transform(1.23, "9.99"))
			Assert.Equal("1,23", Transform(1.23, "@E 9.99"))


	end class
end NAMESPACE