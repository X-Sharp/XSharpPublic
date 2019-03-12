// warning XS0114: 'C666.Functions.ToString()' hides inherited member 'object.ToString()'. To make the current member override that implementation, add the override keyword. Otherwise add the new keyword.
/*
Obviously not important problem, but might be showing other hidden problems as well.
When defining the function like this:

FUNCTION ToString(u AS USUAL) AS INT

then interestingly the warning does not show.

Also if the function is defined in an executable isntead of a library, again the warning does not get reported
*/
FUNCTION ToString() AS INT
RETURN 0
	
