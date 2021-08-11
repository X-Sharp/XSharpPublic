// 796. Compiler crash with incorrect function params in codeblock
FUNCTION Start() AS VOID
LOCAL cb AS CODEBLOCK
cb := {|| EvalFunc( "test" )}
cb := {|| EvalFunc( 1U )}
cb := {|| StringProc( 1 )}

FUNCTION EvalFunc(n AS INT) AS VOID
? n
PROCEDURE StringProc(s AS STRING)
? s
