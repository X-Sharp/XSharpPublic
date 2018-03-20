//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics
using XSharp
begin namespace XSharp	
    abstract class @@CodeBlock implements ICodeBlock
        private initonly _pcount as int
        property PCount as int get _pcount
        
        public constructor (pCount as int)
            _pcount := pCount
        
        public abstract method Eval(args params XSharp.__Usual[] ) as usual
        
        public method EvalBlock(args params object[] ) as object
            var num := args:Length
            var uArgs := <usual>{num}
            for var i := 1 to num
                uArgs[i] := (usual) args[i]
            next
            return self:Eval(uArgs)
        
        public override method ToString() as string
        return "{|" + self:_pcount:ToString() + "| ... }"
        
    end class
    
    
    public class @@_CodeBlock inherit @@CodeBlock
        protect _innerBlock as ICodeBlock 
        protect _cMacro		as string
        public constructor(innerBlock as ICodeBlock, cMacro as string)
            super(innerBlock:Pcount)
            _innerBlock := innerBlock
            _cMacro		:= cMacro
        
        public override method Eval(args params usual[]) as usual
            var num := args:Length
            var oArgs := <object>{num}
            for var i := 1 to num
                oArgs[i] := (object) args[i]
            next
        return (usual) self:_innerBlock:EvalBlock(oArgs)
        
        public override method ToString() as string
        return _cMacro
    end class
    
end namespace