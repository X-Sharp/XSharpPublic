// 968. Problems with XBase++ code [#1960, #1960]
// https://github.com/X-Sharp/XSharpPublic/issues/1960
// https://github.com/X-Sharp/XSharpPublic/issues/1961

// XBase++ dialect
PROCEDURE Main
	LOCAL bErr,oErr

   bErr := ErrorBlock( {|o| BREAK(o) } )
 
   BEGIN SEQUENCE  

	   ErrorBlock( bErr ) 

   RECOVER USING oErr 
          ErrorBlock( bErr ) 
   ENDSEQUENCE // compiler endless recursive state

// compiler endless recursive state
#xcommand WAIT [<*any*>]  => WAIT
WAIT
