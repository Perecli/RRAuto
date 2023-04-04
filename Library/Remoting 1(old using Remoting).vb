Imports System.Runtime.Remoting.Channels.Tcp
Imports System.Runtime.Remoting.Channels
Imports System.Runtime.Remoting.Activation
Imports System.Runtime.Remoting
Imports System.Runtime.Serialization.Formatters
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports RRAutoLib.Common

Namespace Remoting

    ''' <summary>Class for providing object remoting services.</summary>
    Public NotInheritable Class RemService

        Private Shared _srtListenPort As UShort = 32168
        Private Shared _objTcpChannel As TcpChannel

        ''' <summary>The listening TCP port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 32168.</value>
        Public Shared Property ListenPort() As UShort
            Get
                Return _srtListenPort
            End Get
            Set(ByVal value As UShort)
                _srtListenPort = value
            End Set
        End Property

        ''' <summary>Initializes the remoting service to begin servicing clients.</summary>
        Public Shared Sub Start()
            If _objTcpChannel IsNot Nothing Then Exit Sub

            'configure TCP channel with low automatic deserialization
            '_objTcpChannel = New TcpChannel(_srtListenPort)

            'configure TCP channel with full automatic deserialization
            Dim objProvider As New BinaryServerFormatterSinkProvider()
            objProvider.TypeFilterLevel = TypeFilterLevel.Full
            Dim objProperties As IDictionary = New Hashtable() From {{"port", _srtListenPort}}
            _objTcpChannel = New TcpChannel(objProperties, Nothing, objProvider)

            ChannelServices.RegisterChannel(_objTcpChannel, False)
            If RemotingConfiguration.GetRegisteredActivatedServiceTypes.Length = 0 Then     'these can't be unregistered so only register once in case Start() is called a second time
                RemotingConfiguration.RegisterActivatedServiceType(GetType(RemEngine))      'registers a CAO (client activated object)
            End If
        End Sub

        ''' <summary>Terminates the remoting servive.</summary>
        Public Shared Sub [Stop]()
            ChannelServices.UnregisterChannel(_objTcpChannel)
            _objTcpChannel = Nothing
        End Sub

    End Class

    ''' <summary>Class for connecting a client to a remoting service.</summary>
    Public NotInheritable Class RemClient

        Private Shared _srtServerName As String
        Private Shared _srtLocalPort As UShort = 0
        Private Shared _srtRemotePort As UShort = 32168
        Private Shared _objTcpChannel As TcpChannel
        Private Shared _objSyncContext As SyncContext

        ''' <summary>The name of the remoting server the client will connect to.</summary>
        ''' <value>A server name or IP address.</value>
        Public Shared Property ServerName() As String
            Get
                Return _srtServerName
            End Get
            Set(ByVal value As String)
                _srtServerName = value
            End Set
        End Property

        ''' <summary>The outbound TCP port of the client.</summary>
        ''' <value>Valid port numbers are 1-65535. A port value of 0 causes dynamic assignment. The default is 0.</value>
        Public Shared Property LocalPort() As UShort
            Get
                Return _srtLocalPort
            End Get
            Set(ByVal value As UShort)
                _srtLocalPort = value
            End Set
        End Property

        ''' <summary>The listening TCP port of the remoting service.</summary>
        ''' <value>Valid port numbers are 1-65535. The dafault is 32168.</value>
        Public Shared Property RemotePort() As UShort
            Get
                Return _srtRemotePort
            End Get
            Set(ByVal value As UShort)
                _srtRemotePort = value
            End Set
        End Property

        ''' <summary>Gets or sets the thread synchronization context on which remoting events should be raised.</summary> 
        ''' <value>A <see cref="SyncContext" /> instance created on the thread to be synchronized on. If a sync context is not given, remoting events will be raised on pooled worker threads.</value>
        Public Shared Property SyncContext() As SyncContext
            Get
                Return _objSyncContext
            End Get
            Set(ByVal Value As SyncContext)
                _objSyncContext = Value
            End Set
        End Property

        ''' <summary>Initializes a client connection to a remoting servive.</summary>
        Public Shared Sub Start()
            If _objTcpChannel IsNot Nothing Then Exit Sub

            'configure TCP channel with low automatic deserialization
            '_objTcpChannel = New TcpChannel(_srtLocalPort)

            'configure TCP channel with full automatic deserialization
            Dim objProvider As New BinaryServerFormatterSinkProvider()
            objProvider.TypeFilterLevel = TypeFilterLevel.Full
            Dim objProperties As IDictionary = New Hashtable() From {{"port", _srtLocalPort}}
            _objTcpChannel = New TcpChannel(objProperties, Nothing, objProvider)

            ChannelServices.RegisterChannel(_objTcpChannel, False)
        End Sub

        ''' <summary>Terminates the client's connection to the remoting service.</summary>
        Public Shared Sub [Stop]()
            ChannelServices.UnregisterChannel(_objTcpChannel)
            _objTcpChannel = Nothing
        End Sub

        ''' <summary>Gets a reference to a client created object instantiated on the remoting server.</summary>
        Friend Shared Function ActivateRemObj(ByVal objRemType As Type) As Object
            Dim objaAttribs As Object() = {New UrlAttribute(String.Format("tcp://{0}:{1}", _srtServerName, _srtRemotePort))}
            Try
                Return Activator.CreateInstance(objRemType, New Object() {}, objaAttribs)
            Catch ex As Exception
                If TypeOf ex Is Reflection.TargetInvocationException Then ex = ex.InnerException
                Throw New ApplicationException(String.Format("Failed to create '{0}' object on remoting server: {1}", objRemType, ex.Message), ex)
            End Try
        End Function

    End Class

    ''' <summary>An <see cref="Engine" /> wrapper class that provides remoting support.</summary>
    ''' <remarks>An instance of this class is returned through <see cref="RemEngineProxy.Stub" /> after creating a <see cref="RemEngineProxy" /> on the client side.</remarks>
    Public NotInheritable Class RemEngine
        Inherits MarshalByRefObject

        Private WithEvents _objEngine As Engine

        Public Sub New()
            _objEngine = New Engine
            _objEngine.Remoted = True
            _objEngine.DefaultName = "Remoted"
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.Name" />.</summary>
        Public Property Name() As String
            Get
                Return _objEngine.Name
            End Get
            Set(ByVal value As String)
                _objEngine.Name = value
            End Set
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.Address" />.</summary>
        Public Property Address() As UShort
            Get
                Return _objEngine.Address
            End Get
            Set(ByVal value As UShort)
                _objEngine.Address = value
            End Set
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.SpeedSteps" />.</summary>
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return _objEngine.SpeedSteps
            End Get
            Set(ByVal value As SpeedSteps)
                _objEngine.SpeedSteps = value
            End Set
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.BindSlot" />.</summary>
        Public Sub BindSlot(Optional ByVal blnSteal As Boolean = False)
            _objEngine.BindSlot(blnSteal)
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.UnbindSlot" />.</summary>
        Public Sub UnbindSlot(Optional ByVal blnExecSync As Boolean = False)
            _objEngine.UnbindSlot(blnExecSync)
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.Slot" />.</summary>
        Public ReadOnly Property Slot() As Byte
            Get
                Return _objEngine.Slot
            End Get
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.SpeedStepMax" />.</summary>
        Public ReadOnly Property SpeedStepMax() As Byte
            Get
                Return _objEngine.SpeedStepMax
            End Get
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.Speed" />.</summary>
        Public Property Speed() As Byte
            Get
                Return _objEngine.Speed
            End Get
            Set(ByVal value As Byte)
                _objEngine.Speed = value
            End Set
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.SetSpeed" />.</summary>
        Public Sub SetSpeed(ByVal bytSpeed As Byte, ByVal sctClientToken As Guid)
            _objEngine.SetSpeed(bytSpeed, sctClientToken)
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.EmergencyStop" />.</summary>
        Public Sub EmergencyStop()
            _objEngine.EmergencyStop()
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.StartSpeedRamp" />.</summary>
        Public Sub StartSpeedRamp(ByVal bytSpeedTarget As Byte, ByVal srtInterval As UShort)
            _objEngine.StartSpeedRamp(bytSpeedTarget, srtInterval)
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.StopSpeedRamp" />.</summary>
        Public Sub StopSpeedRamp()
            _objEngine.StopSpeedRamp()
        End Sub

        ''' <summary>Remoting wrapper for <see cref="Engine.Direction" />.</summary>
        Public Property Direction() As LocoDirection
            Get
                Return _objEngine.Direction
            End Get
            Set(ByVal value As LocoDirection)
                _objEngine.Direction = value
            End Set
        End Property

        ''' <summary>Remoting wrapper for <see cref="Engine.Functions" />.</summary>
        Public Property Functions(ByVal bytIdx As Byte) As OnOff
            Get
                Return _objEngine.Functions(bytIdx)
            End Get
            Set(ByVal value As OnOff)
                _objEngine.Functions(bytIdx) = value
            End Set
        End Property

        ''' <summary>Used internally by the <see cref="RemEngineProxy" />.</summary>
        ''' <remarks>Do not use this event directly. Use the <see cref="RemEngineProxy.BindSlotResponse" /> event instead.</remarks>
        Public Event BindSlotResponse(ByVal bytResult As Engine.BindSlotResult)
        Private Sub BindSlotResponseHandler(ByVal bytResult As Engine.BindSlotResult) Handles _objEngine.BindSlotResponse
            RaiseEvent BindSlotResponse(bytResult)
        End Sub

        ''' <summary>Used internally by the <see cref="RemEngineProxy" />.</summary>
        ''' <remarks>Do not use this event directly. Use the <see cref="RemEngineProxy.UnbindSlotResponse" /> event instead.</remarks>
        Public Event UnbindSlotResponse(ByVal bytResult As Engine.UnbindSlotResult)
        Private Sub UnbindSlotResponseHandler(ByVal bytResult As Engine.UnbindSlotResult) Handles _objEngine.UnbindSlotResponse
            RaiseEvent UnbindSlotResponse(bytResult)
        End Sub

        ''' <summary>Used internally by the <see cref="RemEngineProxy" />.</summary>
        ''' <remarks>Do not use this event directly. Use the <see cref="RemEngineProxy.SpeedChanged" /> event instead.</remarks>
        Public Event SpeedChanged(ByVal sctClientToken As Guid)
        Private Sub SpeedChangedHandler(ByVal sctClientToken As Guid) Handles _objEngine.SpeedChanged
            RaiseEvent SpeedChanged(sctClientToken)
        End Sub

        ''' <summary>Used internally by the <see cref="RemEngineProxy" />.</summary>
        ''' <remarks>Do not use this event directly. Use the <see cref="RemEngineProxy.DirectionChanged" /> event instead.</remarks>
        Public Event DirectionChanged()
        Private Sub DirectionChangedHandler() Handles _objEngine.DirectionChanged
            RaiseEvent DirectionChanged()
        End Sub

        ''' <summary>Used internally by the <see cref="RemEngineProxy" />.</summary>
        ''' <remarks>Do not use this event directly. Use the <see cref="RemEngineProxy.FunctionChanged" /> event instead.</remarks>
        Public Event FunctionChanged()
        Private Sub FunctionChangedHandler() Handles _objEngine.FunctionChanged
            RaiseEvent FunctionChanged()
        End Sub

        Public Overrides Function InitializeLifetimeService() As Object
            'normally this returns on ILease object; but when we return Nothing it is treated as an infinite lease
            Return Nothing
        End Function

    End Class

    ''' <summary>Proxy used to reference a remoted engine and marshall its events from the remoting server back to the client.</summary>
    ''' <remarks>The <see cref="RemClient" /> must be started before creating this engine proxy instance.</remarks>
    Public NotInheritable Class RemEngineProxy
        Inherits MarshalByRefObject

        Private WithEvents _objEngine As RemEngine

        Public Sub New()
            _objEngine = RemClient.ActivateRemObj(GetType(RemEngine))
        End Sub

        ''' <summary>Gets the engine reference created on the remote server.</summary>
        Public ReadOnly Property Stub() As RemEngine
            Get
                Return _objEngine
            End Get
        End Property

        '------

        ''' <summary>Remoting wrapper for <see cref="Engine.BindSlotResponse" />.</summary>
        Public Event BindSlotResponse(ByVal bytResult As Engine.BindSlotResult)

        ''' <summary>Used internally to synchronize the <see cref="BindSlotResponse" /> event raised by the remoting service.</summary>
        ''' <remarks>Do not use this method directly.</remarks>
        Public Sub BindSlotResponseHandler(ByVal bytResult As Engine.BindSlotResult) Handles _objEngine.BindSlotResponse
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent BindSlotResponse(bytResult)
                Case RemClient.SyncContext.OnSyncThread
                    RaiseEvent BindSlotResponse(bytResult)  'this will probably never occur since remoting events get called on worker threads
                Case Else
                    RemClient.SyncContext.BeginInvoke(New Action(Sub()
                                                                     RaiseEvent BindSlotResponse(bytResult)
                                                                 End Sub))
            End Select
        End Sub

        '------

        ''' <summary>Remoting wrapper for <see cref="Engine.UnbindSlotResponse" />.</summary>
        Public Event UnbindSlotResponse(ByVal bytResult As Engine.UnbindSlotResult)

        ''' <summary>Used internally to synchronize the <see cref="UnbindSlotResponse" /> event raised by the remoting service.</summary>
        ''' <remarks>Do not use this method directly.</remarks>
        Public Sub UnbindSlotResponseHandler(ByVal bytResult As Engine.UnbindSlotResult) Handles _objEngine.UnbindSlotResponse
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent UnbindSlotResponse(bytResult)
                Case RemClient.SyncContext.OnSyncThread
                    RaiseEvent UnbindSlotResponse(bytResult)  'this will probably never occur since remoting events get called on worker threads
                Case Else
                    RemClient.SyncContext.BeginInvoke(New Action(Sub()
                                                                     RaiseEvent UnbindSlotResponse(bytResult)
                                                                 End Sub))
            End Select
        End Sub

        '------

        ''' <summary>Remoting wrapper for <see cref="Engine.SpeedChanged" />.</summary>
        Public Event SpeedChanged(ByVal sctClientToken As Guid)

        ''' <summary>Used internally to synchronize the <see cref="SpeedChanged" /> event raised by the remoting service.</summary>
        ''' <remarks>Do not use this method directly.</remarks>
        Public Sub SpeedChangedHandler(ByVal sctClientToken As Guid) Handles _objEngine.SpeedChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent SpeedChanged(sctClientToken)
                Case RemClient.SyncContext.OnSyncThread
                    RaiseEvent SpeedChanged(sctClientToken)  'this will probably never occur since remoting events get called on worker threads
                Case Else
                    RemClient.SyncContext.BeginInvoke(New Action(Sub()
                                                                     RaiseEvent SpeedChanged(sctClientToken)
                                                                 End Sub))
            End Select
        End Sub

        '------

        ''' <summary>Remoting wrapper for <see cref="Engine.DirectionChanged" />.</summary>
        Public Event DirectionChanged()

        ''' <summary>Used internally to synchronize the <see cref="DirectionChanged" /> event raised by the remoting service.</summary>
        ''' <remarks>Do not use this method directly.</remarks>
        Public Sub DirectionChangedHandler() Handles _objEngine.DirectionChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent DirectionChanged()
                Case RemClient.SyncContext.OnSyncThread
                    RaiseEvent DirectionChanged()  'this will probably never occur since remoting events get called on worker threads
                Case Else
                    RemClient.SyncContext.BeginInvoke(New Action(Sub()
                                                                     RaiseEvent DirectionChanged()
                                                                 End Sub))
            End Select
        End Sub

        '------

        ''' <summary>Remoting wrapper for <see cref="Engine.FunctionChanged" />.</summary>
        Public Event FunctionChanged()

        ''' <summary>Used internally to synchronize the <see cref="FunctionChanged" /> event raised by the remoting service.</summary>
        ''' <remarks>Do not use this method directly.</remarks>
        Public Sub FunctionChangedHandler() Handles _objEngine.FunctionChanged
            Select Case True
                Case RemClient.SyncContext Is Nothing
                    RaiseEvent FunctionChanged()
                Case RemClient.SyncContext.OnSyncThread
                    RaiseEvent FunctionChanged()  'this will probably never occur since remoting events get called on worker threads
                Case Else
                    RemClient.SyncContext.BeginInvoke(New Action(Sub()
                                                                     RaiseEvent FunctionChanged()
                                                                 End Sub))
            End Select
        End Sub

        '------

        Public Overrides Function InitializeLifetimeService() As Object
            'normally this returns on ILease object; but when we return Nothing it is treated as an infinite lease
            Return Nothing
        End Function

    End Class

End Namespace
