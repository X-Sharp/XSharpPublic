USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
BEGIN NAMESPACE XSharp.VFP.Tests

    #pragma warnings(1998, off)

    CLASS FinancialTests
    [Fact, Trait("Category", "Financial")];
    METHOD PaymentTests() AS VOID
        SetFloatDelta(0.01)
        SetDecimal ( 2 )


        ? "--- Payment() ---"
        ? Payment ( 100000 , .105/12 , (20*12)  )
        Assert.True( Payment ( 100000 , .105/12 , (20*12)  ) == 998.38)
        ? Payment ( -100000 , .105/12 , (20*12)  )
        Assert.True( Payment ( -100000 , .105/12 , (20*12)  ) == -998.38)
        ? Payment ( -100000 , .105/12 , 250.5  )
        Assert.True( Payment ( -100000 , .105/12 , 250.5  ) == -985.68)
        ? Payment ( 100235 , .105/12 , 250.5 )
        Assert.True( Payment ( 100235 , .105/12 , 250.5 )  == 988.00)
        ? Payment ( 100235.56 , .105/12 , -250.2 )
        Assert.True( Payment ( 100235.56 , .105/12 , -250.2 ) == 989.10)
        ? Payment ( -100235.56 , -.105/12 , 250.2 )
        Assert.True( Payment ( -100235.56 , -.105/12 , 250.2 ) == -989.10)
        ? Payment ( 100000 , -.08/12 , -180.2 )
        Assert.True( Payment ( 100000 , -.08/12 , -180.2 ) == 955.65)
        ? Payment ( 100000 , .08/12 , 180 )
        Assert.True( Payment ( 100000 , .08/12 , 180 ) == 955.65)
        ? Payment (  100000 , -.08/12   , (30*12) )
        Assert.True( Payment (  100000 , -.08/12   , (30*12) ) == 733.76)
        /*
        We are borrowing $200,000 for 30 years at 4%.
        Since we want the monthly payment, we know we need to express
        the function arguments in monthly periods. So, we would set up
        the function as follows:
        */
        ? Payment (  200000 , .04/12 , 30*12 )
        Assert.True( Payment (  200000 , .04/12 , 30*12 ) == 954.83)


        * $10,000 at 12% for 1 year, monthly payments
        ? Payment(10000, .12/12, 12)
        Assert.True(Payment(10000, .12/12, 12) == 888.49)
        * Make it weekly payments
        ? Payment(10000, .12/52, 52)
        Assert.True(Payment(10000, .12/52, 52) == 204.30)
        * Make one annual payment
        ? Payment(10000, .12, 1)
        Assert.True(Payment(10000, .12, 1) == 11200)

    [Fact, Trait("Category", "Financial")];
    METHOD PVTests() AS VOID
        SetFloatDelta(0.01)
        SetDecimal ( 2 )
        ? "--- PV() ---"
        ? PV (  500 ,.075/12 , 48 )
        Assert.True(PV (  500 ,.075/12 , 48 ) == 20679.19)
        ? PV ( -500 , - .075/12 , 48 )
        Assert.True(PV ( -500 ,.075/12 , 48 ) == -20679.19)
        ? PV (  500 , .075/12 , 48.5 )
        Assert.True(PV (  500 , .075/12 , 48.5 ) == 21047.64)
        ? PV (  -500 ,.075/12 , -48.5 )
        Assert.True(PV (  -500 ,.075/12 , -48.5 ) == -21047.64)
        ?

    [Fact, Trait("Category", "Financial")];
    METHOD FVTests() AS VOID
        SetFloatDelta(0.01)
        SetDecimal ( 2 )
        ? "--- FV() ---"
        ? FV ( 500 , .075/12 , 48 )
        Assert.True(FV ( 500 , .075/12 , 48 )  == 27887.93)
        ? FV ( 10000 , .02 , 12 )
        Assert.True(FV ( 10000 , .02 , 12 )  == 134120.90)
        ? FV ( -10000 , .02 , 12 )
        Assert.True(FV ( -10000 , .02 , 12 )   == -134120.90)
        ? FV ( -10000 , .02 , 12.4999 )
        Assert.True(FV ( -10000 , .02 , 12.4999 ) == -134120.90)
        ? FV ( 10000.45 , 0.02 , 12.4 )
        Assert.True(FV ( 10000.45 , 0.02 , 12.4 ) ==  134126.93)
        ? FV ( 10000.45 , -0.02 , 12.4 )
        Assert.True(FV ( 10000.45 , -0.02 , 12.4 ) ==  134126.93)
        ? FV ( 10000.45 , 0.02 , -12.4 )
        Assert.True(FV ( 10000.45 , 0.02 , -12.4 ) ==  134126.93)
        ? FV ( 10000.45 , 0.02 , 12.5 )
        Assert.True(FV ( 10000.45 , 0.02 , 12.5 ) ==  146809.92)
        ? FV ( 10000.45 , 0.02 , 13 )
        Assert.True(FV ( 10000.45 , 0.02 , 13 ) == 146809.92)
        ? FV ( -10000.45 , 0.02 , 13.4 )
        Assert.True(FV ( -10000.45 , 0.02 , 13.4 ) == -146809.92)
    END CLASS
END NAMESPACE
