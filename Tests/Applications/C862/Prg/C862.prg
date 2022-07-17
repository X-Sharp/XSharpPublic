// 862. Unexpected XS0460 error when adding PUBLIC keyword
// https://github.com/X-Sharp/XSharpPublic/issues/1072
// error XS0460: Constraints for override and explicit interface implementation methods are inherited from the base method, so they cannot be specified directly, except for either a 'class', or a 'struct' constraint.

// /vo3+
USING System.Collections

PUBLIC CLASS Test

    METHOD Dummy1<T>(val AS STRING) AS T where T IS IEnumerable
        RETURN Default(T)

    PUBLIC METHOD Dummy2<T>(val AS STRING) AS T where T IS IEnumerable // error XS0460
        RETURN Default(T)

END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS Test
	o := Test{}
	? o:Dummy1<ArrayList>("asd") == NULL
	? o:Dummy2<ArrayList>("asd") == NULL
RETURN
