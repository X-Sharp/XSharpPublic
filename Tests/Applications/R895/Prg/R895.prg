// https://github.com/X-Sharp/XSharpPublic/issues/1284
// with iClass() {A, B, C} => {iClass():A, iClass():B, iClass():C}
#xtranslate with <expr> {<v1>[, <vn>]} => {<expr>:<v1> [, <expr>:<vn>]}

procedure Main()
    local arr := with Example() {A, B, C,/*aaa*/ D,E}

    AEval(arr, {|c| Qout(c) })
    return


class Example
exported:
    inline access class method A(); return "A"
    inline access class method B(); return "B"
    inline access class method C(); return "C"
    inline access class method D(); return "D"
    inline access class method E(); return "E"
endclass

