
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
#pragma options ("lb", on)

BEGIN NAMESPACE XSharp.RT.Tests

CLASS LateBindingTests
    [Fact, Trait("Category", "LateBinding")];
    METHOD TestNullableProperty() AS VOID
        local u as usual
        u := Foo{}
        u:nullableint :=42
        u:nullabledate := 2023.01.01
        Assert.Equal( (int) u:nullableint, 42L)
        Assert.Equal((int) u:GetNullValue(), 42L)
        Assert.Equal( (date) u:nullabledate, 2023.01.01)
        u:nullabledate += 1
        Assert.Equal( (date) u:nullabledate, 2023.01.02)
        u:nullabledate := null
        Assert.Equal( u:nullabledate, null)
        u:nullabledate := null_date
        Assert.Equal( (date) u:nullabledate, null_date)
        u:nullableint := null
        Assert.Equal( u:nullableint, null)

        local o as usual
        o := Foo{}
        o:nullableint :=42
        o:nullabledate := 2023.01.01
        Assert.Equal( (int) o:nullableint, 42L)
        Assert.Equal((int) o:GetNullValue(), 42L)

        Assert.Equal( (date) o:nullabledate, 2023.01.01)
        o:nullabledate += 1
        Assert.Equal( (date) o:nullabledate, 2023.01.02)
        o:nullabledate := null
        Assert.Equal( o:nullabledate, null)
        o:nullabledate := null_date
        Assert.Equal( (date) o:nullabledate, null_date)
        o:nullableint := null
        Assert.Equal( o:nullableint, null)


     [Fact, Trait("Category", "LateBinding")];
    METHOD TestProperty() AS VOID
        local u as usual
        u := Foo{}
        u:intproperty := 42
        u:dateproperty := 2023.01.01
        Assert.Equal( (int) u:intproperty, 42)
        Assert.Equal((int) u:GetValue(), 42L)
        Assert.Equal( (date) u:dateproperty, 2023.01.01)
        u:dateproperty += 1
        Assert.Equal( (date) u:dateproperty, 2023.01.02)
        u:dateproperty := null_date
        Assert.Equal( (date) u:dateproperty, null_date)
        local o as usual
        o := Foo{}
        o:intproperty := 42
        o:dateproperty := 2023.01.01
        Assert.Equal( (int) o:intproperty, 42)
        Assert.Equal((int) o:GetValue(), 42L)

        Assert.Equal( (date) o:dateproperty, 2023.01.01)
        o:dateproperty += 1
        Assert.Equal( (date) o:dateproperty, 2023.01.02)
        o:dateproperty := null_date
        Assert.Equal( (date) o:dateproperty, null_date)


    class Foo
        property nullableint as Nullable<int> auto get set
        property nullabledate as Nullable<date> auto get set
        property intproperty as int auto get set
        property dateproperty as date auto get set
        method GetValue() as int
            return intproperty
        method GetNullValue() as int
            return nullableint:Value

    end class
END CLASS
END NAMESPACE
