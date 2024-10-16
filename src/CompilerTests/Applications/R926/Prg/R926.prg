
FUNCTION Start( ) AS VOID
    local result as string
	result := TextMerge("a","b")
	? result
	result := TextMerge2("a","b")
	? result
	result := TextMerge3("a","b")
	? result
RETURN


FUNCTION TextMerge(tcFileName1 AS STRING, tcFileName2 AS STRING) AS STRING
   LOCAL lcOutput := "" AS STRING
   TEXT TO lcOutput NOSHOW TEXTMERGE
       COPY FILE "<<tcFileName1>>" TO "<<tcFileName2>>"
   ENDTEXT
   return lcOutPut

FUNCTION TextMerge2() AS STRING
   PARAMETERS tcFileName1 , tcFileName2
   LOCAL lcOutput := "" AS STRING
   TEXT TO lcOutput NOSHOW TEXTMERGE
       COPY FILE "<<tcFileName1>>" TO "<<tcFileName2>>"
   ENDTEXT
   return lcOutPut

FUNCTION TextMerge3() AS STRING
   LPARAMETERS tcFileName1 , tcFileName2
   LOCAL lcOutput := "" AS STRING
   TEXT TO lcOutput NOSHOW TEXTMERGE
       COPY FILE "<<tcFileName1>>" TO "<<tcFileName2>>"
   ENDTEXT
   return lcOutPut

