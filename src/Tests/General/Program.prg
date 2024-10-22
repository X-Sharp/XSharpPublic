using System.Threading.Tasks
ASYNC FUNCTION Start() AS Task
    //RddSetDefault("AXDBFCDX")
    AWAIT Test()


    wait
GLOBAL gsNames as STRING

ASYNC FUNCTION Test() AS Task
    local oServer as DbServer
    oServer := DbServer{"Customer"}

    AWAIT DoSomeTaskAsync(oServer)
    ? oServer:Used
    ? gsNames
    oServer:Close()
    RETURN

ASYNC FUNCTION DoSomeTaskAsync(oServer as DbServer) AS Task<LOGIC>
    LOCAL sNames as STRING
    oServer:GoTop()
    sNames := ""
    DO WHILE ! oServer:EOF
        sNames += oServer:FieldGet(#LastName) + ","
        oServer:Skip()
    END DO
    VAR _ :=  AWAIT ShowNames(oServer, sNames)
    RETURN TRUE

    ASYNC FUNCTION ShowNames(oServer as DbServer,sNames as STRING) AS Task<LOGIC>
        AWAIT Task.Delay(1000)
        gsNames := sNames
    Return True







