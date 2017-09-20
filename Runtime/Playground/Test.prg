using XUnit
USING XSharp


PUBLIC CLASS DateTests

[Fact];
PUBLIC METHOD XsTestDate2() AS VOID
VAR d  := __VoDate{2017,1,1}
Assert.Equal(d:Year, 2017)
end class