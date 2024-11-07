// 930. Problem with generic types without generic type parameters
// https://github.com/X-Sharp/XSharpPublic/issues/1623
FUNCTION Start( ) AS VOID
LOCAL t AS System.Type
t := TypeOf(System.Collections.Generic.Dictionary<INT,STRING>) // OK
t := TypeOf(System.Collections.Generic.Dictionary<,>) // error XS9002: Parser: unexpected input ','
t := TypeOf(System.Collections.Generic.List< >) // error XS9002: Parser: unexpected input '>'
t := TypeOf(System.Collections.Generic.List<>) 
t := TypeOf(System.Collections.Generic.List <>) 
? t:ToString()

