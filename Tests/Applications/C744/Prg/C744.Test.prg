// 744. Problem using PCallNative() in source files with dots in the filename
/*
error XS9038: Error locating the generated DELEGATE '$PCallNative$Start$C744 . Test_54_3_2' FOR the pseudo FUNCTION 'PCallNative<DWORD>'.
error XS0246: The type or namespace name '$PCallNative$Start$C744' could not be Found (are you missing a USING directive or an assembly reference?)
*/
FUNCTION Start() AS VOID
LOCAL h := NULL_PTR AS PTR
PCallNative<DWORD>( h , 1) 
