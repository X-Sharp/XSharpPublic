// 487. error XS1025: Single-line comment or end-of-line expected

// allowed here:
#region Start function and other stuuf

FUNCTION Start() AS VOID

RETURN

// error here:
#endregion Start function and other stuuf

