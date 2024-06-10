#region DEFINES
DEFINE LVSERVER	:=	2
DEFINE ORDERNAME			:=	2
DEFINE RELFIELD				:=	3
DEFINE SERVERNAME			:=	1
DEFINE TVSERVER	:=	1
#endregion
global cDataFile := "gstutor.db" as string
global fact as System.Data.Common.DbProviderFactory
[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XApp
   TRY
      /// SQLRDD Additions Start
       IF ! File(cDataFile)
           ErrorBox{,"File not found: " + cDataFile}
           quit
        endif
        // This makes sure that the provider is linked in
       fact := System.Data.SQLite.SQLiteFactory.Instance
        //SqlDbSetProvider("SQLITE")
        //var hnd:= SqlDbOpenConnection(i"Data Source={cDataFile};Pooling=False;LegacyFieldTypes=True;") //
        SqlDbSetProvider("SQLSERVER")
        var hnd:= SqlDbOpenConnection("Server=(local);Initial catalog=gsTutor;Trusted_Connection=True;") // LegacyFieldTypes=True;

      var oConn := SqlDbGetConnection(hnd)
      oConn:MetadataProvider := SqlMetadataProviderDatabase{oConn}
        oConn:CallBack += MyEventHandler
		/// SQLRDD Additions End
		oXApp := XApp{}
		oXApp:Start()
	CATCH oException AS Exception
		ErrorDialog(oException)
	END TRY
RETURN 0

// SQL Additions start
FUNCTION MyEventHandler(o as Object, e as SqlRddEventArgs) as object
   //System.Diagnostics.Debug.WriteLine(i"{e:Name}, {e:Reason}, {e:Value}")
   RETURN e:Value
// SQL Additions end

CLASS XApp INHERIT App
method Initialize()

#ifdef __APPWIZ__MODSPLASHSCREEN
  SplashScreen{self}:Show()
#endif

  // add your application initialization code here

	RETURN SELF


METHOD Start()
	LOCAL oMainWindow AS DataExplorer
	LOCAL aLevelValues, aServers AS ARRAY
	LOCAL oDim AS dimension

	/*
	create ARRAY's WITH the information needed TO build the Treeview and Listview
	Format:

	aServer := Symbol of mainserver, Symbol of Detailserver, Symbol of the relation field

	aLevelStatus := Label to Show in the first Datalevel of the treeview
	*/

	aServers := {#CUSTOMER,#ORDERS,#CUSTNUM}
	aLevelValues := {#LASTNAME,#FIRSTNAME}

	// Create DataExplorer Mainwindow
	oMainWindow := DataExplorer{SELF, aServers, aLevelValues, FALSE}

	// define the size of the Datawindow
	oMainWindow:Size := Dimension{640, 480}

	// Set the relative size for the panes
	oDim := oMainWindow:GetPaneSize(1)
	oDim:Width := LONG(Float(oDim:Width)/ 1.5)
	oMainWindow:SetPaneSize(oDim, 1)

	// set the caption of the DataExplorer
	oMainWindow:Caption := "Simple DataExplorer"

	// set the system icon of the DataExplorer
	oMainWindow:IconSm := iconDB{}

	// define the Menu of the DataExplorer
	oMainWindow:Menu := DataExplorerMenu{}

	// Show the DataExplorer Mainwindow centered
	oMainWindow:Show(SHOWCENTERED)

	SELF:Exec()

	RETURN SELF

END CLASS

