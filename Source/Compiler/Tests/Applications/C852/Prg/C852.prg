#region Something
// https://github.com/X-Sharp/XSharpPublic/issues/1046
FUNCTION Start() AS VOID
? "test"

#endregion

// 852. Problem with #region..#endregion and external file specified with /stddefs
// error XS9003: Pre-processor: #endregion directive without matching #region found
// Problem happens only when #region is the first line in the .prg and the file specified
// with /stddefs contains an #include in its final line
