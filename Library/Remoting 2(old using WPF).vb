Imports System.Windows.Threading
Imports System.ServiceModel
Imports System.ServiceModel.Channels
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC

'SignalR
Imports Microsoft.Owin.Hosting
Imports Microsoft.AspNet.SignalR
Imports System.Web.Script.Serialization

Namespace Remoting

    ''' <summary>Class for providing object remoting services.</summary>
    Public Class RemService

        'service hosts
        Private Shared _objHostWcfEngine As ServiceHost
        Private Shared _objHostWcfLoconet As ServiceHost
        Private Shared _objHostSignalR As IDisposable

        ''' <summary>The WCF listening port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 32168.</value>
        Public Shared Property WcfPort() As UShort = 32168

        ''' <summary>The SignalR listening port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 8080.</value>
        Public Shared Property SignalrPort() As UShort = 8080

        ''' <summary>Allows remote clients using <see cref="EngineProxy" /> to connect to the WCF service host.</summary>
        Public Shared Property AllowHostWcfEngine() As Boolean = True

        ''' <summary>Allows remote clients using <see cref="LoconetServiceProxy" /> to connect to the WCF service host.</summary>
        Public Shared Property AllowHostWcfLoconet() As Boolean = True

        ''' <summary>Allows remote clients to use the SignalR service host.</summary>
        ''' <remarks>The SignalR host is a limited version of the service that can be more easily consumed by cross platform clients.</remarks>
        Public Shared Property AllowHostSignalr() As Boolean = False

        ''' <summary>Gets or sets the dispatcher that will be responsible for thread synchronization.</summary>         
        ''' <remarks>
        ''' RemService events will be raised on the thread this dispatcher is associated with. In most cases this will be the UI thread. 
        ''' If a sync context is not given, the events will not be raised.
        ''' </remarks>
        Public Shared Property SyncContext() As Dispatcher

        ''' <summary>Adds a URL reservation entry for the SignalR HTTP host.</summary>
        ''' <remarks>Unlike WCF's net.tcp service, SignalR's HTTP service must be registered with HTTP.SYS for non Administrator accounts.</remarks>
        Public Shared Sub AddHttpAcl()

            'some command lines for debugging:
            'netsh http add urlacl url=http://+:8080/ user=Everyone
            'netsh http delete urlacl url=http://+:8080/
            'netsh http show urlacl

            Dim objProcStartInfo As New ProcessStartInfo("netsh",
                String.Format("http add urlacl url=http://+:{0}/ user={1}\{2}",
                RemService.SignalrPort, Environment.UserDomainName, Environment.UserName))

            With objProcStartInfo
                .Verb = "runas"
                .CreateNoWindow = True
                .WindowStyle = ProcessWindowStyle.Hidden
                .UseShellExecute = True
            End With
            Process.Start(objProcStartInfo).WaitForExit()

        End Sub

        ''' <summary>Initializes the remoting service to begin servicing clients.</summary>
        Public Shared Sub Start()

            Dim objNetTcpBinding As New NetTcpBinding()
            objNetTcpBinding.ReceiveTimeout = TimeSpan.MaxValue
            objNetTcpBinding.Security.Mode = SecurityMode.None

            Try
                If RemService.AllowHostWcfEngine Then
                    _objHostWcfEngine = New ServiceHost(GetType(RemEngine), New Uri("net.tcp://localhost:" & RemService.WcfPort))
                    _objHostWcfEngine.AddServiceEndpoint(GetType(IRemEngine), objNetTcpBinding, "Engine")
                    _objHostWcfEngine.Open()
                End If

                If RemService.AllowHostWcfLoconet Then
                    _objHostWcfLoconet = New ServiceHost(New RemLoconetService, New Uri("net.tcp://localhost:" & RemService.WcfPort))
                    _objHostWcfLoconet.AddServiceEndpoint(GetType(IRemLoconetService), objNetTcpBinding, "LoconetService")
                    _objHostWcfLoconet.Open()
                End If
            Catch ex As Exception
                RemService.Stop()  'closes any service hosts that have opened before the error was thrown so failure won't occur on next .Start()
                Throw New ApplicationException("There was a problem starting the WCF remoting service.", ex)
            End Try

            If RemService.AllowHostSignalr Then
                Try
                    _objHostSignalR = WebApp.Start(Of OwinConfig)("http://+:" & RemService.SignalrPort)
                Catch ex As Exception
                    RemService.Stop()  'closes any service hosts that have opened before the error was thrown so failure won't occur on next .Start()
                    Throw New ApplicationException("There was a problem starting the SignalR remoting service.", ex)
                End Try
            End If

        End Sub

        ''' <summary>Gets a value indicating whether the remoting service is currently runnig.</summary>
        Public Shared ReadOnly Property IsStarted() As Boolean
            Get
                Return _
                    _objHostWcfEngine IsNot Nothing OrElse
                    _objHostWcfLoconet IsNot Nothing OrElse
                    _objHostSignalR IsNot Nothing
            End Get
        End Property

        ''' <summary>Terminates the remoting servive.</summary>
        Public Shared Sub [Stop]()

            KillAllConns()
            CloseWcfHost(_objHostWcfEngine)
            CloseWcfHost(_objHostWcfLoconet)

            If _objHostSignalR IsNot Nothing Then
                _objHostSignalR.Dispose()
                _objHostSignalR = Nothing
            End If

        End Sub

        Private Shared Sub CloseWcfHost(ByRef objHost As ServiceHost)

            If objHost IsNot Nothing Then
                If objHost.State = CommunicationState.Opened Then objHost.Close() 'closing a faulted service host throws an error
                objHost = Nothing  'this is why ByRef is used
            End If

        End Sub

#Region "Connection Management WPF"

        Private Shared _intLastConnID As Integer = 0                                 'last ID assigned to a new connection 
        Private Shared _objConnections As New Dictionary(Of IChannel, Connection)  'all the connections actively serviced by all remoting hosts

        ''' <summary>Represents a client remoting connection.</summary>
        Public Class Connection

            'A connection object is an abstraction to the channel object that the user can handle.

            Private _intID As Integer
            Private _strIP As String
            Private _strServiceHost As String
            Private _objRemInstance As Object   'holds, for example, the actual Engine object being remoted
            Private _objChannel As IChannel

            Friend Sub New(strIP As String, strServiceHost As String, objRemInstance As Object, objChannel As IChannel)
                _intLastConnID += 1
                _intID = _intLastConnID
                _strIP = strIP
                _strServiceHost = strServiceHost
                _objRemInstance = objRemInstance
                _objChannel = objChannel
            End Sub

            ''' <summary>Gets the unique connection identifier.</summary>
            Public ReadOnly Property ID As Integer
                Get
                    Return _intID
                End Get
            End Property

            ''' <summary>Gets the IP address of the client's machine that initiated the connection.</summary>
            Public ReadOnly Property IP As String
                Get
                    Return _strIP
                End Get
            End Property

            ''' <summary>Gets the name of the service host being remotely controlled by this connection.</summary>
            Public ReadOnly Property ServiceHost As String
                Get
                    Return _strServiceHost
                End Get
            End Property

            ''' <summary>Gets the server instance of the remoted object being controlled by the client.</summary>
            ''' <remarks>Returns value only if the remoting instance mode is of a "per session" type (e.g. an <see cref="Engine" /> instance).</remarks>
            Public Property RemInstance As Object
                Get
                    Return _objRemInstance
                End Get
                Friend Set(value As Object)
                    _objRemInstance = value
                End Set
            End Property

            ''' <summary>The channel servicing this conection.</summary>
            ''' <remarks>The user has no access to this but silently supplies it when killing connections.</remarks>
            Friend Property Channel As IChannel
                Get
                    Return _objChannel
                End Get
                Set(value As IChannel)
                    _objChannel = value
                End Set
            End Property

            ''' <summary>Terminates this remote connection.</summary>
            Public Sub Kill()

                SyncLock DirectCast(_objConnections, ICollection).SyncRoot
                    'syncklock here to make sure the channel is not nothing for the duration of the block 
                    'note that in RemService.UnregisterConnection() I set "objConnection.Channel = Nothing" in a critical section synclocked on the same object

                    'if conection no longer has a channel, it means the channel was previously unregistered and this connection is obsolete
                    If _objChannel IsNot Nothing Then
                        'closes the channel and raises the iChannel.Closed event on the server and iChannel.Faulted event on the client
                        'iChannel.Closed event is raised on this thread
                        _objChannel.Abort()

                        'didn't use this because it does not raise any event on the client
                        'objConnection.Channel.BeginClose(Nothing, Nothing)
                    End If
                End SyncLock

            End Sub

        End Class

        ''' <summary>Registers the current operation context channel with server's master list of connections to allow management.</summary>
        Friend Shared Sub RegisterConnection(strName As String, objRemInstance As Object)

            Dim objChannel As IChannel = OperationContext.Current.Channel
            Dim objMessageProp As RemoteEndpointMessageProperty = OperationContext.Current.IncomingMessageProperties(RemoteEndpointMessageProperty.Name)

            'retrieving the objMessageProp.Address is supported in .NET 3.0 SP1 or higher only
            Dim objConn As New Connection(objMessageProp.Address, strName, objRemInstance, objChannel)

            SyncLock DirectCast(_objConnections, ICollection).SyncRoot
                _objConnections.Add(objChannel, objConn)
            End SyncLock

            'RaiseConnectionCreatedEvent(objConn)

        End Sub

        ''' <summary>Unregisters the given channel from server's master list of connections.</summary>
        Friend Shared Sub UnregisterConnection(objChannel As IChannel)

            Dim objConn As Connection

            SyncLock DirectCast(_objConnections, ICollection).SyncRoot
                objConn = _objConnections(objChannel)

                'remove object references in case the user still holds a reference to this Connection object
                objConn.Channel = Nothing
                objConn.RemInstance = Nothing   'unlike .Channel this property has a Public Get, so the user could still be holding a reference to the RemInstance; that's a risk we must take 

                _objConnections.Remove(objChannel)
            End SyncLock

            'RaiseConnectionKilledEvent(objConn)

        End Sub

        ''' <summary>Gets a list of remote client connections that are currently being serviced.</summary>
        Public Shared ReadOnly Property Connections() As Connection()
            Get

                SyncLock DirectCast(_objConnections, ICollection).SyncRoot
                    'didn't allow acces to _objConnections directly so user can't alter the list
                    Return _objConnections.Values.ToArray()
                End SyncLock

            End Get
        End Property

        ''' <summary>Terminates all remote connections.</summary>
        Public Shared Sub KillAllConns()

            SyncLock DirectCast(_objConnections, ICollection).SyncRoot
                'reasons for this unusual way of iterating though _objConnections:
                '- the Connection.Kill() call stack eventually executes a _objConnections.Remove() which is not allowed in a For..Each
                '- _objConnections is a Dictionary type which can not access items by index value (the other option for iterating without a For..Each)

                While _objConnections.Count > 0
                    _objConnections.First.Value.Kill()
                End While
            End SyncLock

        End Sub

#End Region

    End Class

    ''' <summary>Class for connecting a client to a remoting service.</summary>
    Public Class RemClient

        ''' <summary>The name of the remoting server the client will connect to.</summary>
        ''' <value>A server name or IP address.</value>
        Public Shared Property ServerName() As String

        ''' <summary>The listening TCP port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 32168.</value>
        Public Shared Property Port() As UShort = 32168

        ''' <summary>Gets or sets the dispatcher that will be responsible for thread synchronization.</summary> 
        ''' <remarks>
        ''' RemClient events will be raised on the thread this dispatcher is associated with. In most cases this will be the UI thread.
        ''' If a sync context is not given, remoting events will be raised on pooled worker threads.
        ''' </remarks>
        Public Shared Property SyncContext() As Dispatcher

    End Class

#Region "WCF Engine"

    <ServiceContract(SessionMode:=SessionMode.Required, CallbackContract:=GetType(IEngineCallback))>
    Friend Interface IRemEngine

        <OperationContract()>
        Sub Ping()

        <OperationContract()>
        Function GetName() As String
        <OperationContract()>
        Sub SetName(value As String)

        <OperationContract()>
        Function GetAddress() As UShort
        <OperationContract()>
        Sub SetAddress(value As UShort)

        <OperationContract()>
        Function GetSpeedSteps() As SpeedSteps
        <OperationContract()>
        Sub SetSpeedSteps(value As SpeedSteps)

        <OperationContract()>
        Function BindSlot(blnSteal As Boolean) As Task(Of Engine.BindSlotResult)

        <OperationContract()>
        Function UnbindSlot(blnNetMustCfm As Boolean) As Task(Of Engine.UnbindSlotResult)

        <OperationContract()>
        Function GetIsBound() As Boolean

        <OperationContract()>
        Function GetSlot() As Byte

        <OperationContract()>
        Function GetSpeedStepMax() As Byte

        <OperationContract()>
        Function GetSpeed() As Byte
        <OperationContract()>
        Sub SetSpeed(value As Byte)

        <OperationContract()>
        Sub SetSpeedWithToken(bytSpeed As Byte, sctClientToken As Guid)

        <OperationContract()>
        Sub EmergencyStop()

        <OperationContract()>
        Function StartSpeedRamp(bytSpeedTarget As Byte, srtInterval As UShort) As Task(Of Boolean)

        <OperationContract()>
        Sub StopSpeedRamp()

        <OperationContract()>
        Function GetDirection() As LocoDirection
        <OperationContract()>
        Sub SetDirection(value As LocoDirection)

        <OperationContract()>
        Function GetFunctions(bytIdx As Byte) As OnOff
        <OperationContract()>
        Sub SetFunctions(bytIdx As Byte, value As OnOff)

    End Interface

    <ServiceBehavior(InstanceContextMode:=InstanceContextMode.PerSession, UseSynchronizationContext:=False, ConcurrencyMode:=ConcurrencyMode.Single, IncludeExceptionDetailInFaults:=True)>
    Friend Class RemEngine
        Implements IRemEngine

        Private WithEvents _objEngine As Engine
        Private _objChannel As IChannel

        Public Sub New()
            _objChannel = OperationContext.Current.Channel

            AddHandler _objChannel.Faulted, AddressOf FaultedClosedHandler   'occurs if there is a fault with the client, probably due to TCP disconnection
            AddHandler _objChannel.Closed, AddressOf FaultedClosedHandler   'occurs if the communication channel is being disposed of

            _objEngine = New Engine
            '_objEngine.Remoted = True
            _objEngine.DefaultName = "Remoted"

            RemService.RegisterConnection("Trottle", _objEngine)
        End Sub

        Public Sub Ping() Implements IRemEngine.Ping
            'used for returning exceptions to the user if the initial connection fails 
        End Sub

        Private Sub FaultedClosedHandler(sender As Object, e As EventArgs)
            RemService.UnregisterConnection(_objChannel)

            RemoveHandler _objChannel.Faulted, AddressOf FaultedClosedHandler
            RemoveHandler _objChannel.Closed, AddressOf FaultedClosedHandler

            _objChannel = Nothing
            _objEngine = Nothing
        End Sub

        '--------

        Public Function GetName() As String Implements IRemEngine.GetName
            Return _objEngine.Name
        End Function
        Public Sub SetName(value As String) Implements IRemEngine.SetName
            _objEngine.Name = value
        End Sub

        Public Function GetAddress() As UShort Implements IRemEngine.GetAddress
            Return _objEngine.Address
        End Function
        Public Sub SetAddress(value As UShort) Implements IRemEngine.SetAddress
            _objEngine.Address = value
        End Sub

        Public Function GetSpeedSteps() As SpeedSteps Implements IRemEngine.GetSpeedSteps
            Return _objEngine.SpeedSteps
        End Function
        Public Sub SetSpeedSteps(value As SpeedSteps) Implements IRemEngine.SetSpeedSteps
            _objEngine.SpeedSteps = value
        End Sub

        Public Function BindSlot(blnSteal As Boolean) As Task(Of Engine.BindSlotResult) Implements IRemEngine.BindSlot
            Return _objEngine.BindSlot(blnSteal)
        End Function

        Public Function UnbindSlot(blnNetMustCfm As Boolean) As Task(Of Engine.UnbindSlotResult) Implements IRemEngine.UnbindSlot
            Return _objEngine.UnbindSlot(blnNetMustCfm)
        End Function

        Public Function GetIsBound() As Boolean Implements IRemEngine.GetIsBound
            Return _objEngine.IsBound
        End Function

        Public Function GetSlot() As Byte Implements IRemEngine.GetSlot
            Return _objEngine.Slot
        End Function

        Public Function GetSpeedStepMax() As Byte Implements IRemEngine.GetSpeedStepMax
            Return _objEngine.SpeedStepMax
        End Function

        Public Function GetSpeed() As Byte Implements IRemEngine.GetSpeed
            Return _objEngine.Speed
        End Function
        Public Sub SetSpeed(value As Byte) Implements IRemEngine.SetSpeed
            _objEngine.Speed = value
        End Sub

        Public Sub SetSpeedWithToken(bytSpeed As Byte, sctClientToken As Guid) Implements IRemEngine.SetSpeedWithToken
            _objEngine.SetSpeed(bytSpeed, sctClientToken)
        End Sub

        Public Sub EmergencyStop() Implements IRemEngine.EmergencyStop
            _objEngine.EmergencyStop()
        End Sub

        Public Function StartSpeedRamp(bytSpeedTarget As Byte, srtInterval As UShort) As Task(Of Boolean) Implements IRemEngine.StartSpeedRamp
            Return _objEngine.StartSpeedRamp(bytSpeedTarget, srtInterval)
        End Function

        Public Sub StopSpeedRamp() Implements IRemEngine.StopSpeedRamp
            _objEngine.StopSpeedRamp()
        End Sub

        Public Function GetDirection() As Loconet.LocoDirection Implements IRemEngine.GetDirection
            Return _objEngine.Direction
        End Function
        Public Sub SetDirection(value As Loconet.LocoDirection) Implements IRemEngine.SetDirection
            _objEngine.Direction = value
        End Sub

        Public Function GetFunctions(bytIdx As Byte) As Loconet.OnOff Implements IRemEngine.GetFunctions
            Return _objEngine.Functions(bytIdx)
        End Function
        Public Sub SetFunctions(bytIdx As Byte, value As Loconet.OnOff) Implements IRemEngine.SetFunctions
            _objEngine.Functions(bytIdx) = value
        End Sub

        Private Sub BindSlotResponseHandler(enuResult As Engine.BindSlotResult) Handles _objEngine.BindSlotResponse
            DirectCast(_objChannel, IEngineCallback).BindSlotResponse(enuResult)
        End Sub

        Private Sub UnbindSlotResponseHandler(enuResult As Engine.UnbindSlotResult) Handles _objEngine.UnbindSlotResponse
            DirectCast(_objChannel, IEngineCallback).UnbindSlotResponse(enuResult)
        End Sub

        Private Sub SpeedChangedHandler(sctClientToken As Guid) Handles _objEngine.SpeedChanged
            DirectCast(_objChannel, IEngineCallback).SpeedChanged(sctClientToken)
        End Sub

        Private Sub DirectionChangedHandler() Handles _objEngine.DirectionChanged
            DirectCast(_objChannel, IEngineCallback).DirectionChanged()
        End Sub

        Private Sub FunctionChangedHandler() Handles _objEngine.FunctionChanged
            DirectCast(_objChannel, IEngineCallback).FunctionChanged()
        End Sub

    End Class


    Friend Interface IEngineCallback

        <OperationContract(IsOneWay:=True)>
        Sub BindSlotResponse(enuResult As Engine.BindSlotResult)

        <OperationContract(IsOneWay:=True)>
        Sub UnbindSlotResponse(enuResult As Engine.UnbindSlotResult)

        <OperationContract(IsOneWay:=True)>
        Sub SpeedChanged(sctClientToken As Guid)

        <OperationContract(IsOneWay:=True)>
        Sub DirectionChanged()

        <OperationContract(IsOneWay:=True)>
        Sub FunctionChanged()

    End Interface

    ''' <summary>Remoting proxy for <see cref="Engine" /> objects created on the server.</summary>
    <CallbackBehavior(UseSynchronizationContext:=False, ConcurrencyMode:=ConcurrencyMode.Multiple)>
    Public Class EngineProxy
        Implements IEngineCallback

        Private _objEngine As IRemEngine

        ''' <summary>Creates an <see cref="Engine" /> object, on the remote server, and connects this proxy to it.</summary>
        ''' <remarks>
        ''' The created object is exclusively owned by this proxy and lives on the server for the duration of the proxy's connection to it.
        ''' If the connection to the server is faulted for any reason, the remote server will dispose the object automatically and raise the <see cref="Disconnected" /> event.
        ''' If this proxy needs to regain remote control, the <see cref="Connect" /> method must be called again. If already connected calling this method will be ignored.
        ''' </remarks>
        Public Sub Connect()
            If _objEngine IsNot Nothing Then Exit Sub

            Dim objEndpointAddress As New EndpointAddress(String.Format("net.tcp://{0}:{1}/Engine", RemClient.ServerName, RemClient.Port))
            Dim objBinding As New NetTcpBinding()
            objBinding.SendTimeout = TimeSpan.MaxValue
            objBinding.Security.Mode = SecurityMode.None
            Dim objFactory As New DuplexChannelFactory(Of IRemEngine)(Me, objBinding, objEndpointAddress)
            _objEngine = objFactory.CreateChannel()
            Try
                _objEngine.Ping()  'we don't know if the connection is good until we do a first call
            Catch ex As Exception
                _objEngine = Nothing
                Throw ex
            End Try
            Dim objChannel As IChannel = _objEngine
            AddHandler objChannel.Faulted, AddressOf FaultedHandler 'occurs if there is a fault with the client, probably due to TCP disconnection
            AddHandler objChannel.Closed, AddressOf ClosedHandler
        End Sub

        ''' <summary>Indicates if this proxy is connected to the remote object on the server.</summary>
        Public ReadOnly Property IsConnected As Boolean
            Get
                Return _objEngine IsNot Nothing
            End Get
        End Property

        ''' <summary>Disconnects from the <see cref="Engine" /> object, on the remote server, and informs the server to dispose it.</summary>
        ''' <remarks>If already disconnected calling this method will be ignored.</remarks>
        Public Sub Disconnect()
            If _objEngine Is Nothing Then Exit Sub

            Dim objChannel As IChannel = _objEngine
            objChannel.BeginClose(Nothing, Nothing)
        End Sub


        Private Sub DisposeRemoteObj()
            Dim objChannel As IChannel = _objEngine
            RemoveHandler objChannel.Faulted, AddressOf FaultedHandler
            RemoveHandler objChannel.Closed, AddressOf ClosedHandler
            _objEngine = Nothing
        End Sub

        Private Sub FaultedHandler(sender As Object, e As EventArgs)
            DisposeRemoteObj()
            RaiseDisconnectedEvent(True)
        End Sub

        Private Sub ClosedHandler(sender As Object, e As EventArgs)
            DisposeRemoteObj()
            RaiseDisconnectedEvent(False)
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.Name" /> member.</summary>
        Public Property Name() As String
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetName
            End Get
            Set(value As String)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetName(value)
            End Set
        End Property

        ''' <summary>Proxies the <see cref="Engine.Address" /> member.</summary>
        Public Property Address() As UShort
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetAddress
            End Get
            Set(value As UShort)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetAddress(value)
            End Set
        End Property

        ''' <summary>Proxies the <see cref="Engine.SpeedSteps" /> member.</summary>
        Public Property SpeedSteps() As SpeedSteps
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetSpeedSteps
            End Get
            Set(value As SpeedSteps)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetSpeedSteps(value)
            End Set
        End Property

        ''' <summary>Proxies the <see cref="Engine.BindSlot" /> member.</summary>
        Public Function BindSlot(blnSteal As Boolean) As Task(Of Engine.BindSlotResult)
            If _objEngine Is Nothing Then Exit Function
            Return _objEngine.BindSlot(blnSteal)
        End Function

        ''' <summary>Proxies the <see cref="Engine.UnbindSlot" /> member.</summary>
        Public Function UnbindSlot(blnNetMustCfm As Boolean) As Task(Of Engine.UnbindSlotResult)
            If _objEngine Is Nothing Then Exit Function
            Return _objEngine.UnbindSlot(blnNetMustCfm)
        End Function

        ''' <summary>Proxies the <see cref="Engine.IsBound" /> member.</summary>
        Public ReadOnly Property IsBound() As Boolean
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetIsBound
            End Get
        End Property

        ''' <summary>Proxies the <see cref="Engine.Slot" /> member.</summary>
        Public ReadOnly Property Slot() As Byte
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetSlot
            End Get
        End Property

        ''' <summary>Proxies the <see cref="Engine.SpeedStepMax" /> member.</summary>
        Public ReadOnly Property SpeedStepMax() As Byte
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetSpeedStepMax
            End Get
        End Property

        ''' <summary>Proxies the <see cref="Engine.Speed" /> member.</summary>
        Public Property Speed() As Byte
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetSpeed
            End Get
            Set(value As Byte)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetSpeed(value)
            End Set
        End Property

        ''' <summary>Proxies the <see cref="Engine.SetSpeed" /> member.</summary>
        Public Sub SetSpeed(bytSpeed As Byte, sctClientToken As Guid)
            If _objEngine Is Nothing Then Exit Sub
            _objEngine.SetSpeedWithToken(bytSpeed, sctClientToken)
        End Sub

        ''' <summary>Proxies the <see cref="Engine.EmergencyStop" /> member.</summary>
        Public Sub EmergencyStop()
            If _objEngine Is Nothing Then Exit Sub
            _objEngine.EmergencyStop()
        End Sub

        ''' <summary>Proxies the <see cref="Engine.StartSpeedRamp" /> member.</summary>
        Public Function StartSpeedRamp(bytSpeedTarget As Byte, srtInterval As UShort) As Task(Of Boolean)
            If _objEngine Is Nothing Then Exit Function
            Return _objEngine.StartSpeedRamp(bytSpeedTarget, srtInterval)
        End Function

        ''' <summary>Proxies the <see cref="Engine.StopSpeedRamp" /> member.</summary>
        Public Sub StopSpeedRamp()
            If _objEngine Is Nothing Then Exit Sub
            _objEngine.StopSpeedRamp()
        End Sub

        ''' <summary>Proxies the <see cref="Engine.Direction" /> member.</summary>
        Public Property Direction() As LocoDirection
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetDirection
            End Get
            Set(value As LocoDirection)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetDirection(value)
            End Set
        End Property

        ''' <summary>Proxies the <see cref="Engine.Functions" /> member.</summary>
        Public Property Functions(bytIdx As Byte) As OnOff
            Get
                If _objEngine Is Nothing Then Exit Property
                Return _objEngine.GetFunctions(bytIdx)
            End Get
            Set(value As OnOff)
                If _objEngine Is Nothing Then Exit Property
                _objEngine.SetFunctions(bytIdx, value)
            End Set
        End Property

        '---------

        ''' <summary>Occurs after this proxy has been disconnected from the remote server object.</summary>
        ''' <param name="blnFaulted"><i>True</i> if disconnection occurred due to a connection fault, <i>False</i> if initiated by user.</param>
        ''' <remarks>All proxy calls will be ignored post this event. <see cref="Connect" /> must be called to regain remote control.</remarks>
        Public Event Disconnected(blnFaulted As Boolean)

        Private Sub RaiseDisconnectedEvent(blnFaulted As Boolean)
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent Disconnected(blnFaulted)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent Disconnected(blnFaulted)
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent Disconnected(blnFaulted))
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.BindSlotResponse" /> member.</summary>
        Public Event BindSlotResponse(enuResult As Engine.BindSlotResult)

        ''' <exclude />
        Public Sub BindSlotResponseHandler(enuResult As Engine.BindSlotResult) Implements IEngineCallback.BindSlotResponse
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent BindSlotResponse(enuResult)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent BindSlotResponse(enuResult)  'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent BindSlotResponse(enuResult))
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.UnbindSlotResponse" /> member.</summary>
        Public Event UnbindSlotResponse(enuResult As Engine.UnbindSlotResult)

        ''' <exclude />
        Public Sub UnbindSlotResponseHandler(enuResult As Engine.UnbindSlotResult) Implements IEngineCallback.UnbindSlotResponse
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent UnbindSlotResponse(enuResult)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent UnbindSlotResponse(enuResult)  'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent UnbindSlotResponse(enuResult))
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.SpeedChanged" /> member.</summary>
        Public Event SpeedChanged(sctClientToken As Guid)

        ''' <exclude />
        Public Sub SpeedChangedHandler(sctClientToken As Guid) Implements IEngineCallback.SpeedChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent SpeedChanged(sctClientToken)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent SpeedChanged(sctClientToken)  'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent SpeedChanged(sctClientToken))
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.DirectionChanged" /> member.</summary>
        Public Event DirectionChanged()

        ''' <exclude />
        Public Sub DirectionChangedHandler() Implements IEngineCallback.DirectionChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent DirectionChanged()
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent DirectionChanged()  'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent DirectionChanged())
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="Engine.FunctionChanged" /> member.</summary>
        Public Event FunctionChanged()

        ''' <exclude />
        Public Sub FunctionChangedHandler() Implements IEngineCallback.FunctionChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent FunctionChanged()
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent FunctionChanged()  'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent FunctionChanged())
            End Select
        End Sub

    End Class

#End Region

#Region "WCF Loconet"

    <ServiceContract(SessionMode:=SessionMode.Required, CallbackContract:=GetType(ILoconetServiceCallback))>
    Friend Interface IRemLoconetService

        <OperationContract()>
        Sub Subscribe()

        <OperationContract()>
        Function TxPacket(objPacket As Packet) As Task(Of Boolean)

        <OperationContract()>
        Function TxPriorityPacket(objPacket As Packet) As Task(Of Boolean)

        <OperationContract()>
        Sub TxPrioritySpeed(bytSlot As Byte, bytSpeed As Byte, Optional objTag As Object = Nothing)

    End Interface

    <ServiceBehavior(InstanceContextMode:=InstanceContextMode.Single, UseSynchronizationContext:=False, ConcurrencyMode:=ConcurrencyMode.Multiple, IncludeExceptionDetailInFaults:=True)>
    Friend Class RemLoconetService
        Implements IRemLoconetService

        Private WithEvents _objLoconetService As LoconetService = CtcService.LoconetService
        Private _objChannels As New List(Of IChannel)

        Public Sub Subscribe() Implements IRemLoconetService.Subscribe
            Dim objChannel As IChannel = OperationContext.Current.Channel

            SyncLock DirectCast(_objChannels, ICollection).SyncRoot
                _objChannels.Add(objChannel)
            End SyncLock

            AddHandler objChannel.Faulted, AddressOf FaultedClosedHandler   'faults happen on exceptions or on connection drops
            AddHandler objChannel.Closed, AddressOf FaultedClosedHandler

            RemService.RegisterConnection("LoconetSrv", Nothing)
        End Sub

        Private Sub FaultedClosedHandler(sender As Object, e As EventArgs)
            Dim objChannel As IChannel = sender

            RemService.UnregisterConnection(objChannel)

            RemoveHandler objChannel.Faulted, AddressOf FaultedClosedHandler
            RemoveHandler objChannel.Closed, AddressOf FaultedClosedHandler

            SyncLock DirectCast(_objChannels, ICollection).SyncRoot
                _objChannels.Remove(objChannel)
            End SyncLock
        End Sub

        '--------

        Public Function TxPacket(objPacket As Packet) As Task(Of Boolean) Implements IRemLoconetService.TxPacket
            Return _objLoconetService.TxPacket(objPacket)
        End Function

        Public Function TxPriorityPacket(objPacket As Packet) As Task(Of Boolean) Implements IRemLoconetService.TxPriorityPacket
            Return _objLoconetService.TxPriorityPacket(objPacket)
        End Function

        Public Sub TxPrioritySpeed(bytSlot As Byte, bytSpeed As Byte, Optional objTag As Object = Nothing) Implements IRemLoconetService.TxPrioritySpeed
            _objLoconetService.TxPrioritySpeed(bytSlot, bytSpeed, objTag)
        End Sub

        Private Sub RxPacketHandler(objPacket As Packet) Handles _objLoconetService.RxPacketOnWorkerThread
            For Each objCallback As ILoconetServiceCallback In _objChannels
                objCallback.RxPacket(objPacket)
            Next
        End Sub

    End Class


    Friend Interface ILoconetServiceCallback

        <OperationContract(IsOneWay:=True)>
        Sub RxPacket(objPacket As Packet)

    End Interface

    ''' <summary>Remoting proxy for the <see cref="LoconetService" /> instance on the server.</summary>
    <CallbackBehavior(UseSynchronizationContext:=False, ConcurrencyMode:=ConcurrencyMode.Multiple)>
    Public Class LoconetServiceProxy
        Implements ILoconetServiceCallback

        Private _objLoconetService As IRemLoconetService

        ''' <summary>Connects this proxy to the <see cref="LoconetService" /> instance on the server.</summary>
        ''' <remarks>
        ''' If the connection to the server is faulted for any reason, the remote server will raise the <see cref="Disconnected" /> event.
        ''' If this proxy needs to regain remote control, the <see cref="Connect" /> method must be called again. If already connected calling this method will be ignored.
        ''' </remarks>
        Public Sub Connect()
            If _objLoconetService IsNot Nothing Then Exit Sub

            Dim objEndpointAddress As New EndpointAddress(String.Format("net.tcp://{0}:{1}/LoconetService", RemClient.ServerName, RemClient.Port))
            Dim objBinding As New NetTcpBinding()
            objBinding.SendTimeout = TimeSpan.MaxValue
            objBinding.Security.Mode = SecurityMode.None
            Dim objFactory As New DuplexChannelFactory(Of IRemLoconetService)(Me, objBinding, objEndpointAddress)
            _objLoconetService = objFactory.CreateChannel()
            Try
                _objLoconetService.Subscribe()  'confirms the connection is good and registers proxy for callbacks
            Catch ex As Exception
                _objLoconetService = Nothing
                Throw ex
            End Try
            Dim objChannel As IChannel = _objLoconetService
            AddHandler objChannel.Faulted, AddressOf FaultedHandler 'occurs if there is a fault with the client, probably due to TCP disconnection
            AddHandler objChannel.Closed, AddressOf ClosedHandler
        End Sub

        ''' <summary>Indicates if this proxy is connected to the remote object on the server.</summary>
        Public ReadOnly Property IsConnected As Boolean
            Get
                Return _objLoconetService IsNot Nothing
            End Get
        End Property

        ''' <summary>Disconnects from the <see cref="LoconetService" /> object, on the remote server.</summary>
        ''' <remarks>If already disconnected calling this method will be ignored.</remarks>
        Public Sub Disconnect()
            If _objLoconetService Is Nothing Then Exit Sub

            Dim objChannel As IChannel = _objLoconetService
            objChannel.BeginClose(Nothing, Nothing)
        End Sub


        Private Sub DisposeRemoteObj()
            Dim objChannel As IChannel = _objLoconetService
            RemoveHandler objChannel.Faulted, AddressOf FaultedHandler
            RemoveHandler objChannel.Closed, AddressOf ClosedHandler
            _objLoconetService = Nothing
        End Sub

        Private Sub FaultedHandler(sender As Object, e As EventArgs)
            DisposeRemoteObj()
            RaiseDisconnectedEvent(True)
        End Sub

        Private Sub ClosedHandler(sender As Object, e As EventArgs)
            DisposeRemoteObj()
            RaiseDisconnectedEvent(False)
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="TxPacket" /> member.</summary>
        Public Function TxPacket(objPacket As Packet) As Task(Of Boolean)
            If _objLoconetService Is Nothing Then Exit Function
            Return _objLoconetService.TxPacket(objPacket)
        End Function

        ''' <summary>Proxies the <see cref="TxPriorityPacket" /> member.</summary>
        Public Function TxPriorityPacket(objPacket As Packet) As Task(Of Boolean)
            If _objLoconetService Is Nothing Then Exit Function
            Return _objLoconetService.TxPriorityPacket(objPacket)
        End Function

        ''' <summary>Proxies the <see cref="TxPrioritySpeed" /> member.</summary>
        Public Sub TxPrioritySpeed(bytSlot As Byte, bytSpeed As Byte)
            If _objLoconetService Is Nothing Then Exit Sub

            'note that we don't support the TxPrioritySpeed's optional objTag parameter for remoting because:
            '- objTag is not marked as serializable in the Packet class so it would not get marshalled any way
            '- even if objTag was marked as serializable, types have to be known by both client/server
            _objLoconetService.TxPrioritySpeed(bytSlot, bytSpeed)
        End Sub

        '---------

        ''' <summary>Occurs after this proxy has been disconnected from the remote server object.</summary>
        ''' <param name="blnFaulted"><i>True</i> if disconnection occurred due to a connection fault, <i>False</i> if initiated by user.</param>
        ''' <remarks>All proxy calls will be ignored post this event. <see cref="Connect" /> must be called to regain remote control.</remarks>
        Public Event Disconnected(blnFaulted As Boolean)

        Private Sub RaiseDisconnectedEvent(blnFaulted As Boolean)
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent Disconnected(blnFaulted)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent Disconnected(blnFaulted) 'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent Disconnected(blnFaulted))
            End Select
        End Sub

        '---------

        ''' <summary>Proxies the <see cref="LoconetService.RxPacketOnSyncThread" /> member.</summary>
        Public Event RxPacket(objPacket As Packet)

        ''' <exclude />
        Public Sub RxPacketHandler(objPacket As Packet) Implements ILoconetServiceCallback.RxPacket
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent RxPacket(objPacket)
                Case RemClient.SyncContext.CheckAccess
                    RaiseEvent RxPacket(objPacket)       'this is unlikely to occur since this is called on a worker thread
                Case Else
                    RemClient.SyncContext.InvokeAsync(Sub() RaiseEvent RxPacket(objPacket))
            End Select
        End Sub

    End Class

#End Region

    Friend Class EngineEndPoint
        Inherits PersistentConnection

        Private Shared _objConnection As IConnection = GlobalHost.ConnectionManager.GetConnectionContext(Of EngineEndPoint).Connection
        Private Shared _objJsonSerializer As New JavaScriptSerializer()
        Private Shared _objEngines As New Dictionary(Of String, Engine)

        Protected Overrides Function OnConnected(objRequest As IRequest, strConnID As String) As Task

            Dim objEngine As New Engine
            'objEngine.Remoted = True
            objEngine.DefaultName = "Remoted"
            AddHandler objEngine.BindSlotResponse,
                Sub(enuResult)
                    _objConnection.Send(strConnID, New With {
                        .Event = "BindSlotResponse",
                        .Result = enuResult.ToString,
                        .SpdMax = objEngine.SpeedStepMax,
                        .Speed = objEngine.Speed,
                        .Dir = objEngine.Direction.ToString,
                        .Func = New With {
                            .F0 = objEngine.Functions(0).ToString,
                            .F1 = objEngine.Functions(1).ToString,
                            .F2 = objEngine.Functions(2).ToString,
                            .F3 = objEngine.Functions(3).ToString,
                            .F4 = objEngine.Functions(4).ToString,
                            .F5 = objEngine.Functions(5).ToString,
                            .F6 = objEngine.Functions(6).ToString,
                            .F7 = objEngine.Functions(7).ToString,
                            .F8 = objEngine.Functions(8).ToString
                        }
                    })
                End Sub

            AddHandler objEngine.UnbindSlotResponse,
                Sub(enuResult)
                    _objConnection.Send(strConnID, New With {
                        .Event = "UnbindSlotResponse",
                        .Result = enuResult.ToString
                    })
                End Sub

            AddHandler objEngine.SpeedChanged,
                Sub(sctClientToken)
                    _objConnection.Send(strConnID, New With {
                        .Event = "SpeedChanged",
                        .Speed = objEngine.Speed,
                        .CliID = sctClientToken
                    })
                End Sub

            AddHandler objEngine.DirectionChanged,
                Sub()
                    _objConnection.Send(strConnID, New With {
                        .Event = "DirectionChanged",
                        .Dir = objEngine.Direction.ToString
                    })
                End Sub

            AddHandler objEngine.FunctionChanged,
                Sub()
                    _objConnection.Send(strConnID, New With {
                        .Event = "FunctionChanged",
                        .Func = New With {
                            .F0 = objEngine.Functions(0).ToString,
                            .F1 = objEngine.Functions(1).ToString,
                            .F2 = objEngine.Functions(2).ToString,
                            .F3 = objEngine.Functions(3).ToString,
                            .F4 = objEngine.Functions(4).ToString,
                            .F5 = objEngine.Functions(5).ToString,
                            .F6 = objEngine.Functions(6).ToString,
                            .F7 = objEngine.Functions(7).ToString,
                            .F8 = objEngine.Functions(8).ToString
                        }
                    })
                End Sub

            _objEngines.Add(strConnID, objEngine)

        End Function

        Protected Overrides Function OnReceived(objRequest As IRequest, strConnID As String, strData As String) As Task

            Dim objEngine As Engine = _objEngines(strConnID)
            Dim objData As Object = _objJsonSerializer.DeserializeObject(strData)

            Select Case objData("Meth")

                Case "BindSlot"
                    objEngine.Name = objData("Name")
                    objEngine.SpeedSteps = [Enum].Parse(GetType(SpeedSteps), objData("SpdStp"))
                    objEngine.Address = objData("Addr")
                    Return objEngine.BindSlot(objData("Steal"))

                Case "UnbindSlot"
                    Return objEngine.UnbindSlot(objData("NetCfm"))

                Case "SetSpeed"
                    objEngine.SetSpeed(objData("Speed"), Guid.Parse(objData("CliID")))
                    Return Task.FromResult(0)

                Case "EStop"
                    objEngine.EmergencyStop()
                    Return Task.FromResult(0)

                Case "SetDir"
                    objEngine.Direction = [Enum].Parse(GetType(Loconet.LocoDirection), objData("Dir"))
                    Return Task.FromResult(0)

                Case "SetFunc"
                    objEngine.Functions(objData("Num")) = [Enum].Parse(GetType(Loconet.OnOff), objData("State"))
                    Return Task.FromResult(0)

            End Select

        End Function

        Protected Overrides Function OnDisconnected(objRequest As IRequest, strConnID As String, blnStopCalled As Boolean) As Task

            _objEngines.Remove(strConnID)

            'todo: unbind the Engine events 

        End Function


        Private Shared Function ClientIP(objRequest As IRequest) As String
            Return objRequest.Environment("server.RemoteIpAddress").ToString
        End Function

    End Class

End Namespace

