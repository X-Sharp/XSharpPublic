// 377. compiler crash with comparison NULL with string
// with /vo2 NOT enabled
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := NULL

? c == NULL
? c = NULL
?
? null_string == c // TRUE
? null_string != c // FALSE
? NULL = c  // TRUE
? NULL == c // TRUE
? NULL != c // FALSE

