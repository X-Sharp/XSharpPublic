// https://github.com/X-Sharp/XSharpPublic/issues/963
#xtranslate new <Exp> ( [<args,...>] ) => <Exp>():new([<args>])
class Example  
exported:    
    Inline Method Init (arg)
    return     
endclass

procedure main()
    local o := new Example(2)
return
