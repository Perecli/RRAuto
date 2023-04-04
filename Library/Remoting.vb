Imports System.Windows.Threading
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC

'SignalR
Imports Owin
Imports Microsoft.Owin
Imports Microsoft.Owin.Hosting
Imports Microsoft.Owin.StaticFiles
Imports Microsoft.Owin.FileSystems
Imports Microsoft.AspNet.SignalR
Imports Microsoft.AspNet.SignalR.Hubs

Namespace Remoting

    ''' <summary>Class for providing object remoting services.</summary>
    Public Class RemService

        'service hosts
        Private Shared _objServHost As IDisposable

        ''' <summary>The listening port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 8080.</value>
        Public Shared Property ServPort() As UShort = 8080

        ''' <summary>Gets or sets the dispatcher that will be responsible for thread synchronization.</summary>         
        ''' <remarks>
        ''' RemService events will be raised on the thread this dispatcher is associated with. In most cases this will be the UI thread. 
        ''' If a sync context is not given, the events will not be raised.
        ''' </remarks>
        Public Shared Property SyncContext() As Dispatcher

        ''' <summary>Adds a URL reservation entry on local machine for the HTTP remoting host.</summary>      
        ''' <remarks>The HTTP service must be registered with HTTP.SYS for non Administrator accounts.</remarks>
        Public Shared Function AddHttpAcl() As String

            'some command lines for debugging:
            'netsh http add urlacl url=http://+:8080/ user=Everyone
            'netsh http delete urlacl url=http://+:8080/
            'netsh http show urlacl

            Dim strOutput As String
            Dim strTempFile = IO.Path.GetTempFileName()  'temp file to store the output results of netsh command
            Using objProcess = New Process()
                With objProcess
                    .StartInfo.FileName = "cmd"
                    .StartInfo.Arguments = $"/c netsh http add urlacl url=http://+:{RemService.ServPort}/ user={Environment.UserDomainName}\{Environment.UserName} > {strTempFile}"
                    .StartInfo.Verb = "runas"   'starts process as admin and prompts for UAC (only works if .UseShellExecute = True)
                    .StartInfo.CreateNoWindow = True
                    .StartInfo.WindowStyle = ProcessWindowStyle.Hidden
                    .StartInfo.UseShellExecute = True
                    .Start()
                    .WaitForExit()
                    strOutput = IO.File.ReadAllText(strTempFile)  'get output results back from temp file
                    IO.File.Delete(strTempFile)
                End With
            End Using

            Return strOutput

        End Function

        ''' <summary>Initializes the remoting service to begin servicing clients.</summary>
        Public Shared Sub Start()

            Try
                _objServHost = WebApp.Start(Of OwinConfig)("http://+:" & RemService.ServPort)
            Catch ex As Exception
                RemService.Stop()  'closes any service hosts that have opened before the error was thrown so failure won't occur on next .Start()
                Throw New ApplicationException("There was a problem starting the remoting service.", ex.GetBaseException)
            End Try

        End Sub

        ''' <summary>Gets a value indicating whether the remoting service is currently runnig.</summary>
        Public Shared ReadOnly Property IsStarted() As Boolean
            Get
                Return _objServHost IsNot Nothing
            End Get
        End Property

        ''' <summary>Terminates the remoting servive.</summary>
        Public Shared Sub [Stop]()

            If _objServHost IsNot Nothing Then
                _objConnections.Clear()
                RaiseConnectionActionEvent(Connection.Action.AllClear, Nothing)

                _objServHost.Dispose()
                _objServHost = Nothing
            End If

        End Sub

#Region "SignalR Connection Management"

        Private Shared _objConnections As New Dictionary(Of String, Connection)
        Private Shared _objConnLock As New ReaderWriterLockSlim  'better than SyncLock when you do more reads than writes beacuse it allows simultaneous readers

        ''' <summary>Represents a client remoting connection.</summary>
        Public Class Connection

            Private _strID As String
            Private _strClientIP As String
            Private _strClientName As String
            Private _datCreated As Date
            Private _datReconnected As Date

            ''' <summary>Descibes the <see cref="ConnectionAction"/> event.</summary>
            Public Enum Action As Byte
                ''' <summary>New connection has been created.</summary>
                Created
                ''' <summary>Connection data has been updated.</summary>
                Updated
                ''' <summary>An existing connection has been deleted.</summary>
                Deleted
                ''' <summary>All existing connections have been deleted.</summary>
                AllClear
            End Enum

            Friend Sub New(objContext As HubCallerContext)
                _strID = objContext.ConnectionId
                _strClientIP = objContext.Request.Environment("server.RemoteIpAddress").ToString
                _strClientName = objContext.QueryString("cname")
                _datCreated = Now
                _datReconnected = _datCreated
            End Sub

            ''' <summary>Gets the unique connection identifier.</summary>
            Public ReadOnly Property ID As String
                Get
                    Return _strID
                End Get
            End Property

            ''' <summary>Gets the IP address of the client's machine that initiated the connection.</summary>
            Public ReadOnly Property ClientIP As String
                Get
                    Return _strClientIP
                End Get
            End Property

            ''' <summary>Gets the name given to the client that initiated the connection.</summary>
            Public ReadOnly Property ClientName As String
                Get
                    Return _strClientName
                End Get
            End Property

            ''' <summary>Gets the time stamp the connection was created.</summary>
            Public ReadOnly Property Created As Date
                Get
                    Return _datCreated
                End Get
            End Property

            ''' <summary>Gets the time stamp the connection was last used.</summary>
            Public Property Reconnected As Date
                Get
                    Return _datReconnected
                End Get
                Friend Set(value As Date)
                    _datReconnected = value
                End Set
            End Property

        End Class

        ''' <summary>Gets a list of remote client connections that are currently being serviced.</summary>
        Public Shared ReadOnly Property Connections() As Connection()
            Get

                _objConnLock.EnterReadLock()
                Try
                    'didn't allow acces to _objConnections directly so user can't alter the list
                    Return _objConnections.Values.ToArray()
                Finally
                    _objConnLock.ExitReadLock()
                End Try

            End Get
        End Property

        ''' <summary>Registers a remote client connection with the server's connection list.</summary>
        Friend Shared Sub RegisterConnection(objContext As HubCallerContext)

            'this can be called when a new connection is created or when a connection already exists on reconnect
            'if RRA is closed and re-opened the clients will re-register so connections should be re-added to the list
            'if RRA's remoting service is stopped and re-started the clients will re-register again but this time the connections are already in the list so just update them

            Dim objCurrConn As Connection, objNewConn As Connection

            _objConnLock.EnterWriteLock()
            Try
                _objConnections.TryGetValue(objContext.ConnectionId, objCurrConn)
                If objCurrConn Is Nothing Then
                    objNewConn = New Connection(objContext)
                    _objConnections.Add(objContext.ConnectionId, objNewConn)
                Else
                    objCurrConn.Reconnected = Now
                End If
            Finally
                _objConnLock.ExitWriteLock()
            End Try

            If objCurrConn Is Nothing Then
                RaiseConnectionActionEvent(Connection.Action.Created, objNewConn)
            Else
                RaiseConnectionActionEvent(Connection.Action.Updated, objCurrConn)
            End If

        End Sub

        ''' <summary>Unregisters an existing remote client connection from the server's connection list.</summary>
        Friend Shared Sub UnregisterConnection(objContext As HubCallerContext)

            Dim objCurrConn As Connection

            _objConnLock.EnterWriteLock()
            Try
                _objConnections.TryGetValue(objContext.ConnectionId, objCurrConn)
                If objCurrConn IsNot Nothing Then _objConnections.Remove(objCurrConn.ID)
            Finally
                _objConnLock.ExitWriteLock()
            End Try

            If objCurrConn IsNot Nothing Then RaiseConnectionActionEvent(Connection.Action.Deleted, objCurrConn)

        End Sub


        ''' <summary>Occurs when a remoting connection event occurs.</summary>
        ''' <param name="objConnection">The connection object the action occured on.</param>
        ''' <param name="enuAction">Descibes the event type.</param>
        ''' <remarks>This event will not be raised if a <see cref="SyncContext" /> is not given.</remarks>
        Public Shared Event ConnectionAction(enuAction As Connection.Action, objConnection As Connection)

        Private Shared Sub RaiseConnectionActionEvent(enuAction As Connection.Action, objConnection As Connection)
            'this always gets called on a worker thread

            Select Case True
                Case RemService.SyncContext Is Nothing
                    Exit Sub
                Case RemService.SyncContext.CheckAccess
                    RaiseEvent ConnectionAction(enuAction, objConnection)
                Case Else
                    RemService.SyncContext.InvokeAsync(Sub() RaiseEvent ConnectionAction(enuAction, objConnection))
            End Select
        End Sub

#End Region

    End Class

#Region "OWIN and SignalR Hubs"

    Friend Class OwinConfig

        Public Sub Configuration(objApp As IAppBuilder)

            objApp.UseErrorPage()

            Dim objFsOptions = New FileServerOptions() With {
                .EnableDefaultFiles = True,
                .RequestPath = New PathString(),
                .FileSystem = New EmbeddedResourceFileSystem("RRAutoLib")  'root namespace for embedded resources
            }

            'map additional MIME types that are not mapped by default
            With DirectCast(objFsOptions.StaticFileOptions.ContentTypeProvider, ContentTypes.FileExtensionContentTypeProvider).Mappings
                .Add(".json", "application/json")
            End With

            objApp.UseFileServer(objFsOptions)

            objApp.MapSignalR() 'maps all hub endpoint classes derived from Hub

        End Sub

    End Class

    'note: all hubs share the same connection thus the hub connection handlers could get raised on multiple hubs;
    'the presence of at least one registered client method on the hub's proxy before the client calls start
    'is what tells SignalR to trigger the OnConnected event on the server side

    ''' <summary>SignalR server endpoint for throttle control.</summary>
    ''' <remarks>
    ''' Don't instantiate this class directly. This class is used by SignalR to auto generate a client side proxy for remoting.
    ''' The described methods are client side javascript stubs that will use SignalR to call into the remoting service.
    ''' </remarks>
    <HubName("EngineHub")>  'name available on client side
    Public Class EngineHub
        Inherits Hub

        ''' <exclude />
        Public Overrides Function OnConnected() As Task

            RemService.RegisterConnection(Context)

            Return MyBase.OnConnected()
        End Function

        ''' <exclude />
        Public Overrides Function OnDisconnected(stopCalled As Boolean) As Task

            RemService.UnregisterConnection(Context)

            Return MyBase.OnDisconnected(stopCalled)
        End Function

        ''' <exclude />
        Public Overrides Function OnReconnected() As Task

            RemService.RegisterConnection(Context)

            Return MyBase.OnReconnected()
        End Function

        '--------- Engine event subscription management ---------

        Private Shared _objSubscriptions As New Dictionary(Of String, List(Of String))  'one to many engine to connections relationships; used to know which connections should be notified when engine events occur
        Private Shared _objSubLock As New ReaderWriterLockSlim  'better than SyncLock when you do more reads than writes beacuse it allows simultaneous readers

        ''' <summary>Registers an Engine with a connections so its events can be raised on that connection.</summary>
        Private Shared Sub SubscribeEngine(strEngineID As String, strConnID As String)

            Dim objConnIDs As List(Of String), blnBindEvents As Boolean = False

            _objSubLock.EnterWriteLock()
            Try
                _objSubscriptions.TryGetValue(strEngineID, objConnIDs)
                If objConnIDs Is Nothing Then
                    'engine events are not bound so bind the engine and subscribe this connection to it

                    _objSubscriptions.Add(strEngineID, New List(Of String) From {strConnID})
                    blnBindEvents = True  'bind engine outside this lock block so we minimize the lock
                Else
                    'engine events are bound (probably to another connection) so subscribe this connection to the engine

                    If Not objConnIDs.Contains(strConnID) Then
                        objConnIDs.Add(strConnID)
                    End If
                End If
            Finally
                _objSubLock.ExitWriteLock()
            End Try

            If blnBindEvents Then

            End If

        End Sub

        ''' <summary>Unregisters an Engine so its events are no longer raised for any connections.</summary>
        Private Shared Sub UnsubscribeEngine(strEngineID As String, strConnID As String)

            Dim objConnIDs As List(Of String), blnUnbindEvents As Boolean = False

            _objSubLock.EnterWriteLock()
            Try
                _objSubscriptions.TryGetValue(strEngineID, objConnIDs)
                If objConnIDs IsNot Nothing AndAlso objConnIDs.Contains(strConnID) Then
                    objConnIDs.Remove(strConnID)
                    If objConnIDs.Count = 0 Then
                        _objSubscriptions.Remove(strEngineID)
                        blnUnbindEvents = True  'unbind engine outside this lock block so we minimize the lock
                    End If
                End If
            Finally
                _objSubLock.ExitWriteLock()
            End Try

            If blnUnbindEvents Then

            End If

        End Sub

        '--------- public client hub methods ---------

        ''' <summary>Gets a list of available <see cref="Engine" /> objects to be controlled.</summary>
        ''' <returns></returns>
        <HubMethodName("GetList")>
        Public Function GetList() As IList

            Dim objList As New List(Of Object)
            For Each objEngine As Engine In CtcService.Engines
                objList.Add(New With {
                    .ID = objEngine.ID,
                    .Address = objEngine.Address,
                    .Name = objEngine.Name
                })
            Next
            Return objList

        End Function

        ''' <summary>Takes control of a given <see cref="Engine" /> object.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param>
        ''' <returns></returns>
        <HubMethodName("Control")>
        Public Async Function Control(strEngineID As String) As Task(Of Object)

            SubscribeEngine(strEngineID, Context.ConnectionId)

            Dim objEngine = EnsureEngineFromID(strEngineID)

            If Not objEngine.IsBound Then
                Dim enuResult = Await objEngine.BindSlot(True)  'todo: make BindSlot() and .IsBound members thread safe
                Select Case enuResult
                    Case Engine.BindSlotResult.Success, Engine.BindSlotResult.SuccessSlotStolen
                    Case Else
                        Throw New HubException($"Failed to bind Engine object to slot.{vbCrLf}Result: {enuResult}")
                End Select
            End If
            Return New With {
                .Name = objEngine.Name,
                .Address = objEngine.Address,
                .Speed = objEngine.Speed,
                .MaxSpeed = objEngine.SpeedStepMax,
                .Direction = If(objEngine.Direction = LocoDirection.Forward, "F", "R"),
                .Functions = objEngine.Functions
            }

        End Function

        ''' <summary>Releases control of a given <see cref="Engine" /> object.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param>
        <HubMethodName("Release")>
        Public Sub Release(strEngineID As String)

            UnsubscribeEngine(strEngineID, Context.ConnectionId)  'connection ID used for unique client ID since it is unique per client

        End Sub

        ''' <summary>Calls the Engine.<see cref="Engine.SetSpeed(Byte, Guid)" /> method on the server.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param>        
        ''' <param name="bytSpeed">Valid values are 0 to Engine.<see cref="Engine.SpeedStepMax"/>.</param>
        <HubMethodName("SetSpeed")>
        Public Sub SetSpeed(strEngineID As String, bytSpeed As Byte)

            EnsureEngineFromID(strEngineID).SetSpeed(bytSpeed, Guid.Parse(Context.ConnectionId))

        End Sub

        ''' <summary>Calls the Engine.<see cref="Engine.EmergencyStop" /> method on the server.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param> 
        <HubMethodName("EStop")>
        Public Sub EStop(strEngineID As String)

            EnsureEngineFromID(strEngineID).EmergencyStop()

        End Sub

        ''' <summary>Sets the Engine.<see cref="Engine.Direction" /> property on the server.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param> 
        ''' <param name="strDirection">Valid values are: "F" for forward; "R" for reverse.</param>
        <HubMethodName("SetDir")>
        Public Sub SetDir(strEngineID As String, strDirection As String)

            EnsureEngineFromID(strEngineID).Direction = If(strDirection = "F", LocoDirection.Forward, LocoDirection.Reverse)

        End Sub

        ''' <summary>Sets the Engine.<see cref="Engine.Functions(Byte)" /> property on the server.</summary>
        ''' <param name="strEngineID">Value corresponding to the Engine.<see cref="Engine.ID" />.</param> 
        ''' <param name="bytIdx">Valid values are 0-28.</param>
        ''' <param name="enuState">Valid values are: 1 for on; 0 for off.</param>
        <HubMethodName("SetFunc")>
        Public Sub SetFunc(strEngineID As String, bytIdx As Byte, enuState As OnOff)

            EnsureEngineFromID(strEngineID).Functions(bytIdx) = enuState

        End Sub

        Private Function EnsureEngineFromID(strEngineID As String) As Engine

            Dim objEngine = CtcService.Engines(Guid.Parse(strEngineID))
            If objEngine Is Nothing Then
                Throw New HubException("Engine object with given ID not found.")
            Else
                Return objEngine
            End If

        End Function

    End Class

    ''' <summary>SignalR server endpoint for monitoring Loconet traffic.</summary>
    ''' <remarks>
    ''' Don't instantiate this class directly. This class is used by SignalR to auto generate a client side proxy for remoting.
    ''' The described methods are client side javascript stubs that will use SignalR to call into the remoting service.
    ''' </remarks>
    ''' <exclude />
    <HubName("LoconetHub")>
    Public Class LoconetHub
        Inherits Hub

        ''' <exclude />
        Public Overrides Function OnConnected() As Task

            RemService.RegisterConnection(Context)

            ' Add your own code here.
            ' For example: in a chat application, record the association between
            ' the current connection ID And user name, And mark the user as online.
            ' After the code in this method completes, the client Is informed that
            ' the connection Is established; for example, in a JavaScript client,
            ' the start().done callback Is executed.

            Return MyBase.OnConnected()
        End Function

        ''' <exclude />
        Public Overrides Function OnDisconnected(stopCalled As Boolean) As Task

            RemService.UnregisterConnection(Context)

            ' Add your own code here.
            ' For example: in a chat application, mark the user as offline, 
            ' delete the association between the current connection id And user name.

            Return MyBase.OnDisconnected(stopCalled)
        End Function

        ''' <exclude />
        Public Overrides Function OnReconnected() As Task

            RemService.RegisterConnection(Context)

            ' Add your own code here.
            ' For example: in a chat application, you might have marked the
            ' user as offline after a period of inactivity; in that case 
            ' mark the user as online again.

            Return MyBase.OnReconnected()
        End Function

        <HubMethodName("Test")>
        Public Function Test() As String
            Return $"LoconetHub: {Context.ConnectionId}"
        End Function

    End Class

#End Region

End Namespace
