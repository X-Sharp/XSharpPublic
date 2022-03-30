/*
TEXT [TO VarName [ADDITIVE] [TEXTMERGE] [NOSHOW] [FLAGS nValue] [PRETEXT eExpression]]

      TextLines

ENDTEXT
*/          
#xcommand ENDTEXT => #endtext

#xcommand  TEXT TO <varname> [<tm:TEXTMERGE>] [<noshow:NOSHOW>] [FLAGS <flags>] [PRETEXT <expression> ]      ;
      =>  __TextInit( iif(<.tm.>,TRUE, SetTextMerge()), <.noshow.>, iif(<.flags.>, <!flags!>, 0), <!expression!>  ) ;;
        #text := <varname>,  ,__TextLine , __TextEnd 
        
#xcommand  TEXT TO <varname> ADDITIVE [<tm:TEXTMERGE>] [<noshow:NOSHOW>] [FLAGS <flags>] [PRETEXT <expression> ]   ;
      =>  __TextInit(iif(<.tm.>,TRUE, SetTextMerge()), <.noshow.>, iif(<.flags.>, <!flags!>, 0), <!expression!>  ) ;;
        #text += <varname>, ,__TextLine  , __TextEnd

#command SET TEXTMERGE <x:ON,OFF,&>      =>  Set( Set.TextMerge, <(x)> )
#command SET TEXTMERGE DELIMITERS [TO <cLeft> [, <cRight>]] => ;
   SetTextMergeDelimiters(<cLeft>, iif ( <.cRight.>,<!cRight!>,<cLeft>))
        

FUNCTION Start( ) AS VOID
    local cTest        
    LOCAL dDate = Date()                                       
    SET TEXTMERGE ON
	TEXT TO cTest NOSHOW PRETEXT 3
	    aa  << DateTime() >>
	    bb  << dDate >>
	    cc
    ENDTEXT  
? cTest 
? dDate
    \\ abc << ToDay() >> 
    \\ def 
RETURN

