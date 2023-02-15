//
// This example shows that you can call an async task and wait for it to finish
// The result of the async task (in this case the size of the file that has been downloaded)
// will be come available when the task has finished
// The calling code (The Start()) function will not have to wait until the async task has
// finished. That is why the line "2....." will be printed before the results from TestClass.DoTest()
// The sample also shows an event and displays the thread id's. You can see that the DownloadFileTaskAsync() method
// starts multiple threads to download the web document in multiple pieces.

USING System
USING System.Threading.Tasks

FUNCTION Start() AS VOID
    ? "1. calling long process"
    TestClass.DoTest()
    ? "2. this should be printed while processing"
    Console.ReadKey()


CLASS TestClass
	STATIC PROTECT oLock AS OBJECT		// To make sure we synchronize the writing to the screen
	STATIC CONSTRUCTOR
		oLock := OBJECT{}

    ASYNC STATIC METHOD DoTest() AS VOID
        LOCAL Size AS INT64
        Size := AWAIT LoooongProcess()
        ? "3. returned from long process"
        ? Size, " Bytes downloaded"

    ASYNC STATIC METHOD LoooongProcess() AS Task<INT64>
        VAR WebClient := System.Net.WebClient{}
        VAR FileName := System.IO.Path.GetTempPath()+"temp.txt"
		webClient:DownloadProgressChanged += OnDownloadProgress
        webClient:Credentials := System.Net.CredentialCache.DefaultNetworkCredentials

        AWAIT webClient:DownloadFileTaskAsync("http://google.com", FileName)
        VAR dirInfo      := System.IO.DirectoryInfo{System.IO.Path.GetTempPath()}
        VAR Files        := dirInfo:GetFiles("temp.txt")
        IF Files:Length > 0
            ? "Opening file ", FileName
            System.Diagnostics.Process.Start(FileName)
            RETURN Files[1]:Length
        ENDIF
        RETURN 0

	STATIC METHOD OnDownloadProgress (sender AS OBJECT, e AS System.Net.DownloadProgressChangedEventArgs) AS VOID
		BEGIN LOCK oLock
            IF e:TotalBytesToReceive > e:BytesReceived
			    ? String.Format("{0,3} % Size: {1,8:N0} Thread {2}", 100*e:BytesReceived / e:TotalBytesToReceive , e:BytesReceived, System.Threading.Thread.CurrentThread:ManagedThreadId)
            ELSE
			    ? String.Format("Size: {0,8:N0} Thread {1}", e:BytesReceived, System.Threading.Thread.CurrentThread:ManagedThreadId)
            ENDIF
		END LOCK
		RETURN

END CLASS
