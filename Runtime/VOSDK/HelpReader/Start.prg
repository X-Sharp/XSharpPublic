#using System.Reflection
#using System.IO
#using System.Xml
#using System.Text.RegularExpressions
#using System.Text
//
// Start.prg
//

FUNCTION Start() AS INT
    //TestReg()
    GenXML("c:\Program Files (x86)\Vulcan.NET 2.0\Redist\Assemblies\v2.0\VulcanRTFuncs.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOGUIClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOInternetClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOConsoleClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVORDDClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOReportClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOSQLClasses.dll")
    //GenXML("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOSystemClasses.dll")

    RETURN 0


FUNCTION GenXML(cAssembly AS STRING) AS VOID
    //TestVol()
        
    //TestReg()

    LOCAL a AS Assembly
    LOCAL writer AS TextWriter
    LOCAL types as Type[]
    LOCAL i AS INT
    LOCAL reader AS TextReader
    local lDone as logic
    LOCAL t AS Type
    LOCAL j AS INT
    LOCAL methods as MethodInfo[]
    LOCAL m AS MethodInfo
    LOCAL props as PropertyInfo[]
    LOCAL p AS PropertyInfo
    LOCAL fields as FieldInfo[]
    LOCAL f as FieldInfo
    
    
    LOCAL nPos AS DWORD      
    //a := Assembly.LoadFrom("c:\Program Files (x86)\Vulcan.NET\Assembly\VulcanVOGUIClasses.dll")
    a := Assembly.LoadFrom(cAssembly)

    writer := File.CreateText("d:\Vulcan.NET\Dev\Output\Release\v2.0\"+a:GetName():Name+".xml")
    writer:WriteLine("<?xml version="+chr(34)+"1.0"+chr(34)+" encoding="+chr(34)+"utf-8"+chr(34)+"?>")
    writer:WriteLine("<doc>")
    writer:WriteLine("  <assembly>")
    writer:WriteLine("    <name>"+a:GetName():Name+"</name>")
    writer:WriteLine("  </assembly>")
    writer:WriteLine("  <members>")
    
    ? "Assembly: "+a:GetName():Name
    
  
    types := a:GetTypes()
    
    ? types:Length
    

    classList := {}
    methodList := {}
    propList := {}
    fieldList := {}
    
    FOR i := 1 UPTO types:Length
        t := types[i]
        if ! t:Name == "Functions"
            loop
        endif
        
        IF (nPos := AScan(classList,{|a| a == t:FullName})) == 0
            AAdd(classList,t:FullName)
            AAdd(methodList,t:FullName+".#ctor")
        ENDIF

        methods := t:GetMethods(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        FOR j := 1 UPTO methods:Length
            m := methods[j]
            IF (nPos := AScan(methodList,{|a| a == t:FullName+"."+m:Name})) == 0
                AAdd(methodList,t:FullName+"."+m:Name)
            ENDIF
        NEXT        

        props := t:GetProperties(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        FOR j := 1 UPTO props:Length
            p := props[j]
            IF (nPos := AScan(propList,{|a| a == t:FullName+"."+p:Name})) == 0
                AAdd(propList,t:FullName+"."+p:Name)
            ENDIF
        NEXT        

        fields := t:GetFields(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        FOR j := 1 UPTO fields:Length
            f := fields[j]
            IF (nPos := AScan(fieldList,{|a| a == t:FullName+"."+f:Name})) == 0
                AAdd(fieldList,t:FullName+"."+f:Name)
            ENDIF
        NEXT        
    NEXT    
    
    
    FOR i := 1 UPTO types:Length
        t := types[i]
        if ! t:Name == "Functions"
            loop
        endif
        //? i, t:Name, t:Namespace, t:Module
        ? "Member: "+t:FullName,"class_"+t:Name+".htm"
        
        //IF ! t:Name == "Accelerator"
        //    LOOP
        //ENDIF

        
        //----------------------------------------
        // Class
        //----------------------------------------


        if File.Exists("d:\Visual Objects\Cavo\HTML\"+"class_"+t:Name+".htm")

            writer:WriteLine("    <member name="+chr(34)+"T:"+t:FullName+chr(34)+">")
            ? "exists"
            reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"class_"+t:Name+".htm")

            aSeeAlso := {}

            WriteMemberDetail(reader,writer,detailtype.classdetail)
            
            WriteSeeAlso(writer)

            writer:WriteLine("    </member>")
            
            reader:Close()
            
            lDone := true
        endif
        
        //----------------------------------------
        // Constructor
        //----------------------------------------
        if File.Exists("d:\Visual Objects\Cavo\HTML\"+"method_"+t:Name+"_init.htm")
            writer:WriteLine("    <member name="+chr(34)+"M:"+t:FullName+".#ctor"+chr(34)+">")
            reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"method_"+t:Name+"_init.htm")
            aSeeAlso := {}
            WriteMemberDetail(reader,writer,detailtype.methoddetail)
            WriteSeeAlso(writer)
            writer:WriteLine("    </member>")
        endif

            
        //----------------------------------------
        // Method
        //----------------------------------------
        
        methods := t:GetMethods(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        
        FOR j := 1 UPTO methods:Length
            m := methods[j]
            if m:Name == "InList"
                ?
            endif
            if File.Exists("d:\Visual Objects\Cavo\HTML\"+"method_"+t:Name+"_"+m:Name+".htm")
                writer:WriteLine("    <member name="+chr(34)+"M:"+t:FullName+"."+m:Name+chr(34)+">")

                reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"method_"+t:Name+"_"+m:Name+".htm")
                
                aSeeAlso := {}
                
                WriteMemberDetail(reader,writer,detailtype.methoddetail)
                
                WriteSeeAlso(writer)

                writer:WriteLine("    </member>")

                reader:Close()
            elseif File.Exists("d:\Visual Objects\Cavo\HTML\"+"function_"+m:Name+".htm")
                writer:WriteLine("    <member name="+chr(34)+"M:"+t:FullName+"."+m:Name+chr(34)+">")

                reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"function_"+m:Name+".htm")
                
                aSeeAlso := {}
                
                WriteMemberDetail(reader,writer,detailtype.methoddetail)
                
                WriteSeeAlso(writer)

                writer:WriteLine("    </member>")

                reader:Close()
            endif

            
        NEXT
        
        //----------------------------------------
        // Property
        //----------------------------------------
        
        props := t:GetProperties(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        
        FOR j := 1 UPTO props:Length
            p := props[j]
            if File.Exists("d:\Visual Objects\Cavo\HTML\"+"a_"+t:Name+"_"+p:Name+".htm")
                writer:WriteLine("    <member name="+chr(34)+"P:"+t:FullName+"."+p:Name+chr(34)+">")

                reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"a_"+t:Name+"_"+p:Name+".htm")
                
                aSeeAlso := {}
                
                WriteMemberDetail(reader,writer,detailtype.propertydetail)
                
                WriteSeeAlso(writer)

                writer:WriteLine("    </member>")

                reader:Close()
            endif

            
        NEXT
        
        //----------------------------------------
        // Field
        //----------------------------------------
        
        fields := t:GetFields(BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static)
        
        FOR j := 1 UPTO fields:Length
            f := fields[j]
            if File.Exists("d:\Visual Objects\Cavo\HTML\"+"export_"+t:Name+"_"+f:Name+".htm")
                writer:WriteLine("    <member name="+chr(34)+"F:"+t:FullName+"."+f:Name+chr(34)+">")

                reader := File.OpenText("d:\Visual Objects\Cavo\HTML\"+"export_"+t:Name+"_"+f:Name+".htm")
                
                aSeeAlso := {}
                
                WriteMemberDetail(reader,writer,detailtype.fielddetail)
                
                WriteSeeAlso(writer)

                writer:WriteLine("    </member>")

                reader:Close()
            endif

            
        NEXT
        
        //if lDone
        //    exit
        //endif

    NEXT

    writer:WriteLine("  </members>")
    writer:WriteLine("</doc>")
    
    writer:Close()


    RETURN
    
FUNCTION WriteSeeAlso(writer AS TextWriter) AS VOID
    if ! aSeeAlso == {}
        LOCAL i AS INT
        LOCAL c AS STRING
        LOCAL extra AS STRING
        FOR i := 1 UPTO aSeeAlso:Length
            c := FileNameToRef(aSeeAlso[i,1])
            IF ! empty(c)
                writer:WriteLine("      <seealso cref="+chr(34)+c+chr(34)+"/>")
            ENDIF
        NEXT
    endif
    
    return

FUNCTION FileNameToRef(cFile AS STRING) AS STRING
    LOCAL c AS STRING
    LOCAL cName AS STRING

    DO CASE 
    CASE left(cFile,5) == "class"
        c := "T:Vulcan.VO."+strtran(substr(cFile,7),".htm","")
        cName := SubStr(c,3)
        
    CASE left(cFile,6) == "method"
        c := "M:Vulcan.VO."+strtran(substr(cFile,8),".htm","")
        c := strtran(c,"_",".")
        if right(c,5) == ".init"
            c := substr(c,1,slen(c)-5)+".#ctor"
        endif
        cName := SubStr(c,3)
    ENDCASE    
    
    IF ! cName == NULL
        IF ! empty(classList)
            LOCAL nPos AS DWORD
            IF (nPos := Ascan(classList,{|x| Upper(x) == Upper(cName)})) > 0
                c := Left(c,2)+classList[nPos]
            ENDIF
        ENDIF
        IF ! empty(methodList)
            IF (nPos := Ascan(methodList,{|x| Upper(x) == Upper(cName)})) > 0
                c := Left(c,2)+methodList[nPos]
            ENDIF
        ENDIF
    ELSE
        ? cFile
    ENDIF
    
        
    return c
    

    
FUNCTION WriteMemberDetail(reader as TextReader, writer AS TextWriter,detailType AS detailtype) AS VOID
    LOCAL s AS STRING
    LOCAL collect AS LOGIC
    LOCAL chunk AS STRING
    LOCAL mode AS helpsection

    chunk := ""
    collect := false
    do while ! (s := reader:Readline()) == NULL
        if s:StartsWith("<p><span class="+chr(34)+"f_Heading1"+chr(34)+">") .and. ! s:Contains("GUI Notes") .and. ! chunk == ""
            collect := false
            //? chunk
            WriteSection(writer, mode, chunk)
            
            chunk := ""
        endif
        if collect
            chunk += s:Replace("&nbsp;"," ")
            //chunk += s:Replace("<p>","||||para>"):Replace("</p>","||||/para>"+CRLF):Replace("&nbsp;"," ")
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">Description</span></p>"
            collect := true
            if detailType == detailtype.propertydetail .or. detailType == detailtype.fielddetail
                mode := helpsection.summary
            else
                mode := helpsection.remarks
            endif
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">Arguments</span></p>"
            collect := true
            mode := helpsection.arguments
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">Purpose</span></p>"
            collect := true
            mode := helpsection.summary
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">Returns</span></p>"
            collect := true
            mode := helpsection.returns
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">Examples</span></p>"
            collect := true
            mode := helpsection.examples
        endif
        if s == "<p><span class="+chr(34)+"f_Heading1"+chr(34)+">See Also</span></p>"
            collect := true
            mode := helpsection.seealso
        endif
    enddo
    
    if ! chunk == ""
        WriteSection(writer, mode, chunk)
    endif
    
    RETURN    

FUNCTION GetArgs(chunk AS STRING,a AS ARRAY, start as INT) AS VOID
    LOCAL match AS Match
    LOCAL arg as STRING
    LOCAL desc AS STRING
    LOCAL rest AS STRING
    LOCAL exp AS STRING
    LOCAL reg AS Regex
    LOCAL index as INT
    LOCAL length AS INT
    
    exp := "(.*?)<span class="+chr(34)+"f_List"+chr(34)+">&lt;</span><span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span><span class="+chr(34)+"f_List"+chr(34)+">&gt;[ ]*</span>(.*)"
    reg := Regex{exp}
    match := reg:Match(chunk,start)
    if match:Success
        //? match:Length
        
        arg := match:Groups[2]:Value
        rest := match:Groups[3]:Value
        index := match:Groups[2]:Index
        length := match:Groups[2]:Length
        aadd(a,{arg,index,length})
        GetArgs(chunk,a,index+length)
    else
        //&lt;<span class="f_Italic">kBorderStyle</span>&gt;
        exp := "(.*?)&lt;<span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span>&gt;(.*)"
        reg := Regex{exp}
        match := reg:Match(chunk,start)
        if match:Success
            arg := match:Groups[2]:Value
            rest := match:Groups[3]:Value
            index := match:Groups[2]:Index
            length := match:Groups[2]:Length
            aadd(a,{arg,index,length})
            GetArgs(chunk,a,index+length)
        else
//                exp := "(.*?)<span class="+chr(34)+"f_List"+chr(34)+">&lt;(.*?)&gt;</span>(.*)"
            exp := "(.*?)&lt;(.*?)&gt;(.*)"
            reg := Regex{exp}
            match := reg:Match(chunk,start)
            if match:Success
                arg := match:Groups[2]:Value
                rest := match:Groups[3]:Value
                index := match:Groups[2]:Index
                length := match:Groups[2]:Length
                aadd(a,{arg,index,length})
                GetArgs(chunk,a,index+length)
            else
                exp := "(.*?)<span class="+chr(34)+"f_Italic"+chr(34)+">&lt;(.*?)&gt;</span>(.*)"
                reg := Regex{exp}
                match := reg:Match(chunk,start)
                if match:Success
                    arg := match:Groups[2]:Value
                    rest := match:Groups[3]:Value
                    index := match:Groups[2]:Index
                    length := match:Groups[2]:Length
                    aadd(a,{arg,index,length})
                    GetArgs(chunk,a,index+length)
                //else
                    //? chunk
                endif
            endif
        endif
    endif
    
    RETURN

FUNCTION GetSeeAlso(chunk AS STRING,a AS ARRAY, start as INT) AS STRING
    LOCAL match AS Match
    LOCAL href as STRING
    LOCAL reftext as STRING
    LOCAL desc AS STRING
    LOCAL rest AS STRING
    LOCAL exp AS STRING
    LOCAL reg AS Regex
    LOCAL index as INT
    LOCAL length AS INT
    
    exp := "(.*?)<span class="+chr(34)+"f_link"+chr(34)+">(.*?)<a href="+chr(34)+"(.*?)"+chr(34)+">(.*?)</a>(.*?)</span>(.*)" // "(.*?)&lt;(.*?)&gt;(.*)"
    reg := Regex{exp}
    match := reg:Match(chunk,start)
    if match:Success
        //? match:Length
        
        href := match:Groups[3]:Value
        reftext := match:Groups[4]:Value
        rest := match:Groups[6]:Value
        aadd(a,{href,reftext})
        chunk := match:Groups[1]:Value + " `see cref="+chr(34)+ FileNameToRef(href) +chr(34)+ ">" + reftext + "`/see>" + rest
        chunk := GetSeeAlso(chunk,a,0)
    else
        exp := "(.*?)<a href="+chr(34)+"(.*?)"+chr(34)+">(.*?)</a>(.*)" // "(.*?)&lt;(.*?)&gt;(.*)"
        reg := Regex{exp}
        match := reg:Match(chunk,start)
        if match:Success
            href := match:Groups[2]:Value
            reftext := match:Groups[3]:Value
            rest := match:Groups[4]:Value
            aadd(a,{href,reftext})
            chunk := match:Groups[1]:Value + " `see cref="+chr(34)+ FileNameToRef(href) +chr(34)+ ">" + reftext + "`/see>" + rest
            chunk := GetSeeAlso(chunk,a,0)
        endif   
    endif
    
    RETURN chunk
        
FUNCTION WriteSection(writer as TextWriter, mode as helpsection, chunk as string) AS VOID
    
//    if mode == helpsection.arguments
    
//        LOCAL a := {} AS ARRAY
        
//        GetArgs(chunk,a,0)
        
//        LOCAL i as int
//        LOCAL c := "" AS STRING
//        FOR i := 1 UPTO ALen(a)
//            c += "      `param name="+chr(34)+a[i,1]+chr(34)+">"+chunk:Substring(a[i,2],a[i,3])+"`/param>"+CRLF
//        NEXT
        
//        chunk := c
        
//        /*
//        LOCAL match AS Match
//        LOCAL arg as STRING
//        LOCAL rest AS STRING
//        LOCAL exp AS STRING
        
//        exp := "(.*?)<span class="+chr(34)+"f_List"+chr(34)+">&lt;</span><span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span><span class="+chr(34)+"f_List"+chr(34)+">&gt;[ ]*</span>(.*)"
//        match := Regex.Match(chunk,exp)
//        if match:Success
//            ? match:Length
//            arg := match:Groups[2]:Value
//            rest := match:Groups[3]:Value
//        else
//            //&lt;<span class="f_Italic">kBorderStyle</span>&gt;
//            exp := "(.*?)&lt;<span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span>&gt;(.*)"
//            match := Regex.Match(chunk,exp)
//            if match:Success
//                arg := match:Groups[2]:Value
//                rest := match:Groups[3]:Value
//            else
////                exp := "(.*?)<span class="+chr(34)+"f_List"+chr(34)+">&lt;(.*?)&gt;</span>(.*)"
//                exp := "(.*?)&lt;(.*?)&gt;(.*)"
//                match := Regex.Match(chunk,exp)
//                if match:Success
//                    arg := match:Groups[2]:Value
//                    rest := match:Groups[3]:Value
//                else
//                    exp := "(.*?)<span class="+chr(34)+"f_Italic"+chr(34)+">&lt;(.*?)&gt;</span>(.*)"
//                    match := Regex.Match(chunk,exp)
//                    if match:Success
//                        arg := match:Groups[2]:Value
//                        rest := match:Groups[3]:Value
//                    else
//                        ? chunk
//                    endif
//                endif
//            endif
//        endif
        
//        */
//    endif

    
    chunk := Regex.Replace(chunk,"<p class="+chr(34)+"p_CodeExample"+chr(34)+"(.*?)>(.*?)</p>","`para>$2`/para>")
    chunk := Regex.Replace(chunk,"<p class="+chr(34)+"p_Heading1"+chr(34)+"(.*?)>(.*?)</p>","`para>$2`/para>")
    chunk := Regex.Replace(chunk,"<p>(.*?)</p>","`para>$1`/para>")
    chunk := Regex.Replace(chunk,"<p class="+chr(34)+"p_Indented"+chr(34)+"(.*?)>(.*?)</p>","&#160;&#160;&#160;&#160;$2")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_IndentedList"+chr(34)+"(.*?)>(.*?)</span>","&#160;&#160;&#160;&#160;$2")
    chunk := Regex.Replace(chunk,"<span style="+chr(34)+"width:(.*?)px"+chr(34)+"(.*?)>(.*?)</span>","$3&#160;&#160;&#160;&#160;")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_CodeExample"+chr(34)+"(.*?)>(.*?)</span>","$2")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_BoldItalic"+chr(34)+"(.*?)>(.*?)</span>","`b>`i>$2`/i>`/b>")

LOCAL ccc AS STRING
if mode == helpsection.arguments
    ccc := chunk
endif

    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_List"+chr(34)+">&lt;</span><span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span><span class="+chr(34)+"f_List"+chr(34)+">&gt;(.*?)</span>","`i>&lt;$1&gt;$2`/i>")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_Italic"+chr(34)+"(.*?)>(.*?)</span>","`i>$2`/i>")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_Bold"+chr(34)+"(.*?)>(.*?)</span>","`b>$2`/b>")
//        chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_Heading1"+chr(34)+"(.*?)>(.*?)</span>","<font size="+chr(34)+"+1"+chr(34)+">$2</font>")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_Heading1"+chr(34)+"(.*?)>(.*?)</span>","`b>$2`/b>")
    //chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_Italic"+chr(34)+"(.*)>(.*)</span>","<i>$2</i>")
//    chunk := Regex.Replace(chunk,"<span style="+chr(34)+"color: (.*?)"+chr(34)+">(.*?)</span>","$2")

    chunk := Regex.Replace(chunk,"<span style="+chr(34)+"font(.*?)>(.*?)</span>","$2")
    chunk := chunk:Replace("<span class="+chr(34)+"f_List"+chr(34)+"> </span>","")
    //chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_List"+chr(34)+">(.*?)</span>(.*?)<span class="+chr(34)+"f_List"+chr(34)+">(.*?)</span>","`list type="+chr(34)+"table"+chr(34)+">`item>`term>$1`/term>`description>$3`/description>`/item>`/list>")
    //chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_List"+chr(34)+">(.*?)</span>","`list type="+chr(34)+"bullet"+chr(34)+">`item>`term> `/term>`description>$1`/description>`/item>`/list>")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_List"+chr(34)+" style="+chr(34)+"font-weight: bold;"+chr(34)+">(.*?)</span>","`b>$2`/b>")
    chunk := Regex.Replace(chunk,"<span class="+chr(34)+"f_List"+chr(34)+">(.*?)</span>","`para>$1`/para>")
  
    chunk := GetSeeAlso(chunk,aSeeAlso,0)
    
    //&lt;/td>&lt;/tr>&lt;/table>&lt;/body>&lt;/html>
    chunk := Regex.Replace(chunk,"<div (.*?)>","")
    //chunk := Regex.Replace(chunk,"<table (.*?)>","`table>")
    //chunk := Regex.Replace(chunk,"<tr (.*?)>","`tr>")
    //chunk := Regex.Replace(chunk,"<td (.*?)>","`td>")
    chunk := Regex.Replace(chunk,"<table (.*?)>","")
    chunk := Regex.Replace(chunk,"<tr (.*?)>","`para>`/para>")
    chunk := Regex.Replace(chunk,"<td (.*?)>","&#160;")
    chunk := Regex.Replace(chunk,"<p (.*?)>","")
    //chunk := chunk:Replace("<td>","`td>")
    //chunk := chunk:Replace("<tr>","`tr>")
    //chunk := chunk:Replace("<table>","`table>")
    chunk := chunk:Replace("<td>","&#160;")
    chunk := chunk:Replace("<tr>","`para>`/para>")
    chunk := chunk:Replace("<table>","")
    //chunk := chunk:Replace("</td>","`/td>")
    //chunk := chunk:Replace("</tr>","`/tr>")
    //chunk := chunk:Replace("</table>","`/table>")
    chunk := chunk:Replace("</td>","")
    chunk := chunk:Replace("</tr>","")
    chunk := chunk:Replace("</table>","")
    chunk := chunk:Replace("</body>","")
    chunk := chunk:Replace("</div>","")
    chunk := chunk:Replace("</p>","")
    chunk := chunk:Replace("</html>","")
//        chunk := chunk:Replace("<","&lt;"):Replace("||||","<"):Replace("<para>",""):Replace("</para>","<para></para>")
    chunk := chunk:Replace("<","&lt;")
    chunk := chunk:Replace("`","<")
    
    
    do case
    case mode == helpsection.summary
        writer:WriteLine("      <summary>"+chunk+"</summary>")
    case mode == helpsection.examples
        writer:WriteLine("      <example>"+chunk+"</example>")
    case mode == helpsection.returns
        writer:WriteLine("      <returns>"+chunk+"</returns>")
    case mode == helpsection.arguments
        writer:WriteLine("      <param name="+chr(34)+"$args"+chr(34)+">"+chunk+"</param>")
    case mode == helpsection.remarks
       writer:WriteLine("      <remarks>"+chunk+"</remarks>")
    end case

    RETURN        
    
ENUM detailtype
    classdetail
    methoddetail
    propertydetail
    fielddetail
END ENUM

ENUM helpsection
    summary
    remarks
    examples
    returns
    arguments
    seealso
END ENUM
    
FUNCTION TestReg() AS VOID
    LOCAL c AS STRING
    c := "abcdefg <span class="+chr(34)+"f_Heading1"+chr(34)+">GUI Notes</span> xyz"
    
    c := MemoRead("d:\temp\chunk.txt")

    LOCAL ccc AS STRING
    ccc := c

    c := Regex.Replace(c,"<span class="+chr(34)+"f_List"+chr(34)+"&lt;</span><span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span><span class="+chr(34)+"f_List"+chr(34)+">&gt;</span>","`i>&lt;$1&gt;`/i>")

    c := Regex.Replace(c,"<span class="+chr(34)+"f_List"+chr(34)+">&lt;</span><span class="+chr(34)+"f_Italic"+chr(34)+">(.*?)</span><span class="+chr(34)+"f_List"+chr(34)+">&gt;</span>","`i>&lt;$1&gt;`/i>")
    
    
    RETURN
    
FUNCTION TestIndexProperty() AS VOID
    LOCAL dictionary := MyDictionary{} as MyDictionary
    LOCAL c AS STRING
    
    dictionary:list["test1"] := "result1"
    dictionary:list["test2"] := "result2"
    dictionary:list["test3"] := "result3"
    
    c := dictionary:list["test2"]  // result2
    
    RETURN
    

    
CLASS MyDictionary
    PROTECT masterList := {} AS ARRAY
    
ASSIGN list(value AS STRING, index AS STRING) AS VOID
    LOCAL nPos AS DWORD
    
    // Check if index is already in the list
    IF (nPos := AScan(SELF:masterList,{|a| a[1] == index})) > 0
        // re-assign this index entry
        SELF:masterList[nPos] := {index,value}
    ELSE
        // add a new entry
        AAdd(masterList,{index,value})
    ENDIF
    RETURN
    
ACCESS list(index AS STRING) AS STRING
    LOCAL result AS STRING
    LOCAL nPos AS DWORD
    
    // find the index in the list
    IF (nPos := AScan(SELF:masterList,{|a| a[1] == index})) > 0
        result := SELF:masterList[nPos,2]
    ENDIF
    
    RETURN result
    
END CLASS

_DLL FUNCTION GetVolumeInformation(Volume AS STRING, VolumeName AS StringBuilder, ;
	VolumeNameSize AS DWORD, SerialNumber AS DWORD PTR, maxComponentLength AS DWORD PTR, ;
	flags AS DWORD PTR, fs AS StringBuilder, fs_size AS DWORD) AS LOGIC PASCAL:kernel32.GetVolumeInformationW
	
/*_DLL FUNC GetVolumeInformationZZ(lpRootPathName AS PSZ, lpVolumeNameBuffer AS PSZ,;
	nVolumeNameSize AS DWORD, lpVolumeSerialNumber AS DWORD PTR,;
	lpMaximumComponentLength AS DWORD PTR, lpFileSystemFlags AS DWORD PTR,;
	lpFileSystemNameBuffer AS PSZ, nFileSystemNameSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.GetVolumeInformationA	*/

FUNCTION TestVol() AS VOID
    LOCAL serialNum, maxComponentLength, flags AS DWORD
    LOCAL volumename := StringBuilder{256} AS StringBuilder
    LOCAL fstype := StringBuilder{256} AS StringBuilder

    serialNum := 0
    maxComponentLength := 0
    flags := 0
    
    LOCAL ok AS LOGIC
    ok := GetVolumeInformation("D:\", volumename, ;
	(DWORD)volumename:Capacity - 1, @serialNum, @maxComponentLength, ;
	@flags, fstype, (DWORD)fstype:Capacity - 1)
	
	? serialNum
    serialNum := 0
    maxComponentLength := 0
    flags := 0
	
    ok := GetVolumeInformation("C:\", volumename, ;
	(DWORD)volumename:Capacity - 1, @serialNum, @maxComponentLength, ;
	@flags, fstype, (DWORD)fstype:Capacity - 1)
	
	? serialNum
    serialNum := 0
    maxComponentLength := 0
    flags := 0

    ok := GetVolumeInformation("J:\", volumename, ;
	(DWORD)volumename:Capacity - 1, @serialNum, @maxComponentLength, ;
	@flags, fstype, (DWORD)fstype:Capacity - 1)
	
	? serialNum, String.Format("{0:X}",serialNum), String.Format("{0:X}",255)
    serialNum := 0
    maxComponentLength := 0
    flags := 0
	
    ok := GetVolumeInformation("M:\", volumename, ;
	(DWORD)volumename:Capacity - 1, @serialNum, @maxComponentLength, ;
	@flags, fstype, (DWORD)fstype:Capacity - 1)
	
	? serialNum

	RETURN
	

GLOBAL aSeeAlso AS ARRAY
GLOBAL classList := {} AS ARRAY
GLOBAL methodList := {} AS ARRAY
GLOBAL propList := {} AS ARRAY
GLOBAL fieldList := {} AS ARRAY
