// Assembly name is set to "Image" 
// (original sample included a library named Image, thus also the assembly name was Image)
// note that this is working correctly in Core, error is reported only in vulcan dialect
                               
// error XS0118: 'Image' is a namespace but is used like a type
// output Image, dialect Vulcan
USING System.Drawing

FUNCTION Start() AS VOID
LOCAL oImage AS Image
oImage := NULL
? oImage
