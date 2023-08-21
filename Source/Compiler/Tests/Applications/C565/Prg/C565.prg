// 565. Missing overflow checks for numeric conversion when /ovf is enabled
#pragma warnings(168, off) // declared but not used
#pragma warnings(219, off) // declared but not used

FUNCTION Start() AS VOID
	LOCAL i AS INT
	LOCAL d AS DWORD
	LOCAL b AS BYTE
	LOCAL ui64 AS UINT64

	FOR LOCAL n := 1 AS INT UPTO 5
		LOCAL lOverflow := FALSE AS LOGIC
		TRY
			// all following expressions should lead to an overflow eception
//			SWITCH n // nned to run the test also with vulcan
			DO CASE
			CASE n == 1
				i := -10
				d := (DWORD)i
			CASE n == 2
				d := UInt32.MaxValue
				i := (INT)d
			CASE n == 3
				i := -1
				b := (BYTE)i
			CASE n == 4
				i := -1
				b := (BYTE)i
			CASE n == 5
				i := -1
				ui64 := (UINT64) i
			END

		CATCH e AS System.OverflowException
			lOverflow := TRUE
			? "Overflow correctly detected for test", n
		END TRY

		IF .not. lOverflow
			THROW Exception{"Overflow check missing for test " + n:ToString()}
		END IF

	NEXT

	// Conversions with the _CAST syntax should not cause an exception:
	i := -10
	d := DWORD(_CAST,i)

	d := UInt32.MaxValue
	i := INT(_CAST,d)

	i := -1
	b := BYTE(_CAST,i)

	i := -1
	b := BYTE(_CAST,i)

	ui64 := UINT64(_CAST,UInt64.MaxValue)

RETURN
