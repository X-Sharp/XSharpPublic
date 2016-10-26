// 264. error XS1978: Cannot use an expression of type 'void*' as an argument to a dynamically dispatched operation.
FUNCTION Start() AS VOID
LOCAL p AS PTR
LOCAL u AS USUAL
u := NIL
p := NULL_PTR
u:Something(p)

