// 124. warning XS0238: 'Parent.TEst()' cannot be sealed because it is not an override
// vo3
CLASS Parent
    SEALED METHOD TEst() AS VOID
    ? "parent"
END CLASS

CLASS Child INHERIT Parent
    NEW METHOD TEst() AS VOID
    ? "Child"
END CLASS

FUNCTION Start() AS VOID
LOCAL o AS Parent
o := Child{}
o:TEst()
RETURN
