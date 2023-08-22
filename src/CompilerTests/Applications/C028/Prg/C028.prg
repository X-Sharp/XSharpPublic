// 28. error XS0019: Operator '+' cannot be applied to operands of type 'AnchorStyles' and 'AnchorStyles'
#using System.Windows.Forms
FUNCTION Start() AS VOID
LOCAL e AS AnchorStyles
e := AnchorStyles.Left + AnchorStyles.Right
? e

