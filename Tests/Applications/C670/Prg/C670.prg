// 670. error XS7038: Failed to emit module 'C670'.
// All the following lines (if all others are omitted) result to a "Failed to emit module"
// Of course instead of that, a compiler error message should had been reported for each line
FUNCTION Start() AS VOID
	LOCAL c AS STRING
	LOCAL l AS LOGIC

	c := "adf"
	l := c < 1
	l := c > TRUE
	l := c > 2010.10.10
	l := l > 2010.10.10
	l := l > "A"
	l := c < 'A'
	l := c >= 1
RETURN
