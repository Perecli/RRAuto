Imports System.IO.Ports
Imports System.Windows.Threading
Imports System.Collections.Specialized
Imports System.Runtime.Serialization
Imports System.Security.Permissions
Imports RRAutoLib.CTC

Namespace Loconet

    ''' <summary>The main class for providing Loconet services and communication.</summary>
    ''' <remarks>
    ''' <para>
    ''' This service creates a connection to a given serial port and maintains a transmission thread, separate from the calling thread.
    ''' Events are executed on either a pool of worker threads or can be synchronized to a given thread. In most cases this will be the UI thread.
    ''' (see: <see cref="LoconetService.SyncContext"/> for event synchronization).
    ''' </para>
    ''' <para>
    ''' There are two transmission queues: a normal and a priority queue. Packets placed in the priority queue preempt the ones in the normal queue.
    ''' All packet transmissions are asynchronous and they can either be awaited on, or used in a fire and forget fashion. 
    ''' </para>
    ''' <para>
    ''' All methods implemented by this class are thread safe.
    ''' </para>
    ''' </remarks>
    ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
    Public NotInheritable Class LoconetService

        Private Const PACKET_ECHO_TIMEOUT As UShort = 200
        Private Const PACKET_RESPONSE_TIMEOUT As UShort = 500

        Private _objSerialPort As SerialPort
        Private _bytComPort As Byte = 1
        Private _enuBaudRate As BaudRate = BaudRate._57600
        Private _blnFlowControl As Boolean = True                       'RTS/CTS flow control assumed; if others are needed we can change this to an enumeration
        Private _objSyncContext As Dispatcher                           'outgoing events will be marshaled on this dispather's thread

        Private _objTxToken As New Object                               'token for critical section synchronization used by Tx methods
        Private _blnIsStarted As Boolean                                'used to indicate if the LoconetService is started 

        Private _objStopWatch As Stopwatch                              'used for time stamping packet traffic
        Private _objTxThread As Thread                                  'thread used for packet transmission
        Private _objTxQueues As TxQueues                                'holds queues to be synchronized between producer and consumer
        Private _objTxRxSync As TxRxSync                                'holds data to be synchronized between Tx and Rx thread 

        Private _bytaRxBuf As Byte()                                    'receive buffer; reused for each read
        Private _objRxBytes As New List(Of Byte)                        'current packet being assembled
        Private _srtRxPakLen As Short                                   'length that current packed should be

#Region "Classes"

        Private NotInheritable Class TxQueues
            Friend PriorityQueue As New QueueX(Of Packet)
            Friend NormalQueue As New QueueX(Of Packet)
        End Class

        Private NotInheritable Class TxRxSync

            Friend Enum MatchType As Byte
                None = 0
                Echo
                Response
            End Enum

            'outstanding packet waiting for echoe/response
            Friend AssertedPacket As Packet

            'indicates how matching attempts should be performed with the asserted packet
            Friend MatchNeeded As MatchType

            'maintains if asserted packet has received an echoe
            Friend EchoeReceived As Boolean

        End Class

#End Region

#Region "Events"

        'packet events -----------------------------------------

        ''' <summary>Occurs when a Loconet packet is received.</summary>
        ''' <param name="objPacket">The <see cref="Packet"/> received from Loconet.</param>
        ''' <remarks>
        ''' Methods registered with this event will execute on an arbitrary worker thread. 
        ''' Except for special cases, the <see cref="RxPacketOnSyncThread"/> event is prefered over this one.
        ''' </remarks>
        ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
        Public Event RxPacketOnWorkerThread(objPacket As Packet)

        ''' <summary>Occurs when a Loconet packet is received.</summary>
        ''' <param name="objPacket">The <see cref="Packet"/> received from Loconet.</param>
        ''' <remarks>
        ''' Methods registered with this event will execute on the thread associated with the dispather specified by the <see cref="LoconetService.SyncContext"/> property. 
        ''' This is the prefered event over the alternative <see cref="RxPacketOnWorkerThread"/> because you can synchronize this event with your UI objects for thread safety.
        ''' </remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
        ''' <seealso cref="LoconetService.SyncContext"/>
        ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
        Public Event RxPacketOnSyncThread(objPacket As Packet)

        Private Sub RaiseRxPacketEvent(objPacket As Packet)
            'synchronous execution on caller's thread (gets called from one of many serial port's Rx pooled threads)
            RaiseEvent RxPacketOnWorkerThread(objPacket)

            'asynchronous call on the SyncObj's thread
            If _objSyncContext IsNot Nothing Then
                _objSyncContext.InvokeAsync(Sub() RaiseEvent RxPacketOnSyncThread(objPacket))
            End If
        End Sub

#End Region

#Region "Properties"

        ''' <summary>Gets or sets the communication port number of the Loconet interface.</summary>
        ''' <value>Default value is 1.</value>
        ''' <remarks>If the LoconetService is started, setting this property will not take effect until the service is stopped and restarted.</remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>        
        Public Property ComPort() As Byte
            Get
                Return _bytComPort
            End Get
            Set(Value As Byte)
                If Value = 0 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid COM port. Value can not be 0.")

                _bytComPort = Value
            End Set
        End Property

        ''' <summary>Gets or sets the baud rate speed of the Loconet interface.</summary>
        ''' <value>Default value is <see cref="BaudRate._57600"/>.</value>
        ''' <remarks>If the LoconetService is started, setting this property will not take effect until the service is stopped and restarted.</remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>        
        Public Property BaudRate() As BaudRate
            Get
                Return _enuBaudRate
            End Get
            Set(Value As BaudRate)
                _enuBaudRate = Value
            End Set
        End Property

        ''' <summary>Gets or sets whether flow control should be used with the Loconet interface.</summary>
        ''' <value><i>True</i> if flow control is enabled (default); <i>False</i> if flow control is disabled.</value>
        ''' <remarks>The flow control used is RTS/CTS. If the LoconetService is started, setting this property will not take effect until the service is stopped and restarted.</remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>   
        Public Property FlowControl() As Boolean
            Get
                Return _blnFlowControl
            End Get
            Set(value As Boolean)
                _blnFlowControl = value
            End Set
        End Property

        ''' <summary>Indicates whether the Loconet service has been started.</summary>
        ''' <value><i>True</i> if service is started; <i>False</i> if service is stoped.</value>
        ''' <remarks>When the Loconet service is started it possesses a connection to the given COM port and is ready to process packet transfers.</remarks>
        ''' <seealso cref="LoconetService.Start"/>
        ''' <seealso cref="LoconetService.Stop"/>
        Public ReadOnly Property IsStarted() As Boolean
            Get
                'reading or writing a boolean is an atomic operation that does not require explicit synchronization
                Return _blnIsStarted
            End Get
        End Property

        ''' <summary>Gets or sets the dispatcher that will be responsible for thread synchronization.</summary> 
        ''' <remarks>
        ''' LoconetService events will be raised on the thread this dispatcher is associated with. In most cases this will be the UI thread. 
        ''' If left null (Default), the <see cref="LoconetService.RxPacketOnSyncThread"/> event will not be raised but the <see cref="LoconetService.RxPacketOnWorkerThread"/> event will still be raised.
        ''' </remarks> 
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example> 
        ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
        Public Property SyncContext() As Dispatcher
            Get
                Return _objSyncContext
            End Get
            Set(Value As Dispatcher)
                _objSyncContext = Value
            End Set
        End Property

        ''' <summary>Gets the elapsed time since the Loconet service was started.</summary>
        ''' <returns>If the service is stopped, the returned value is not valid.</returns>
        ''' <remarks>
        ''' The elapsed time is retrieved from the internal stopwatch that is used to time stamp incoming packets.
        ''' This value can be used to time stamp custom code, which can later be compared against the packet stamps, for troubleshooting timing issues.
        ''' </remarks>
        Public ReadOnly Property ElapsedTime() As TimeSpan
            Get
                If _objStopWatch IsNot Nothing Then Return _objStopWatch.Elapsed
            End Get
        End Property

#End Region

#Region "Methods"

        ''' <summary>Starts the Loconet service and connects to the serial port.</summary>
        ''' <remarks>The Loconet service must be started before it can process packet transfers.</remarks>
        ''' <example>The following example performs the following:
        ''' <list type="bullet">
        ''' <item><description>Starts the Loconet service.</description></item>
        ''' <item><description>Sends a packet to turn on track power.</description></item>
        ''' <item><description>Listens for and displays incoming Loconet packets.</description></item>
        ''' </list>
        ''' <para><b>Note:</b> In this example the <i>RxPacket</i> method is executed on the UI thread, because we passed its dispatcher to the <see cref="LoconetService.SyncContext"/> property.</para>
        '''	<code lang="VB">        
        '''     Imports RRAutoLib.Loconet
        ''' 
        '''     Public Class Sample
        ''' 
        '''         Private WithEvents locServ As New LoconetService
        '''
        '''         Public Sub Initialize()
        '''             'set up serial communication parameters
        '''             locServ.BaudRate = BaudRate._57600
        '''             locServ.ComPort = 1
        '''             locServ.FlowControl = True
        '''
        '''             'pass the UI dispatcher to the LoconetService
        '''             locServ.SyncContext = Dispatcher.CurrentDispatcher
        '''
        '''             'start the Loconet service
        '''             Try
        '''                 locServ.Start()
        '''             Catch ex As Exception
        '''                 Console.WriteLine(ex.Message)
        '''                 Exit Sub
        '''             End Try
        '''
        '''             'send a power-on packet
        '''             locServ.TxPacket(New PkSetPowerOn)
        '''         End Sub
        '''
        '''         Private Sub RxPacket(pk As Packet) Handles locServ.RxPacketOnSyncThread
        '''             'display Loconet events
        '''             Console.WriteLine(pk.Description)
        '''         End Sub
        '''
        '''         Public Sub Dispose()
        '''             locServ.Stop()
        '''         End Sub
        ''' 
        '''     End Class
        '''	</code>
        '''	</example>
        ''' <seealso cref="LoconetService.Stop"/>
        ''' <seealso cref="LoconetService.IsStarted"/>        
        Public Sub Start()

            'owning a lock the token is required to guarantee that no other method call conflicts with starting the service
            'the service has to be atomicaly started/stopped to be in a consistent state
            SyncLock _objTxToken
                If Not _blnIsStarted Then

                    Try
                        'open serial connection
                        _objSerialPort = New SerialPort()
                        With _objSerialPort
                            .PortName = "COM" & _bytComPort
                            .BaudRate = _enuBaudRate
                            '.RtsEnable = True    this is redundant when specifying Handshake.RequestToSend
                            .Handshake = If(_blnFlowControl, Handshake.RequestToSend, Handshake.None)
                            .DataBits = 8
                            .Parity = Parity.None
                            .StopBits = StopBits.One
                            '.WriteBufferSize = 64   'documentation states that the WriteBufferSize property ignores any value smaller than 2048
                            '.ReadBufferSize = 512   'documentation states that the ReadBufferSize property ignores any value smaller than 4096
                            .ReadTimeout = 5
                            .WriteTimeout = 5
                            .ReceivedBytesThreshold = 1
                            .Open()
                            .DiscardInBuffer()
                            .DiscardOutBuffer()
                            ReDim _bytaRxBuf(.ReadBufferSize - 1)
                        End With
                        AddHandler _objSerialPort.DataReceived, AddressOf RxSerialPortBytes
                    Catch ex As Exception
                        Me.Stop()
                        Throw New ApplicationException("Error occurred connecting to serial port: " & ex.Message, ex)
                    End Try

                    _objStopWatch = Stopwatch.StartNew()
                    _objTxQueues = New TxQueues
                    _objTxRxSync = New TxRxSync
                    _objRxBytes = New List(Of Byte)
                    _srtRxPakLen = 0

                    Try
                        'start worker thread that handles packet transmission
                        _objTxThread = New Thread(AddressOf TxThread)
                        With _objTxThread
                            .IsBackground = True
                            .Name = "LoconetServiceTx"
                            .Priority = ThreadPriority.Normal
                            .Start()
                        End With
                    Catch ex As Exception
                        Me.Stop()
                        Throw New ApplicationException("Error occurred starting Loconet service thread: " & ex.Message, ex)
                    End Try

                    _blnIsStarted = True

                End If
            End SyncLock

        End Sub

        ''' <summary>Stops the Loconet service and disconnects from the serial port.</summary>
        ''' <remarks>If chnaging any connection properties, the Loconet service must be stoped and restarted before new setting will be applied.</remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
        ''' <seealso cref="LoconetService.Start"/>
        ''' <seealso cref="LoconetService.IsStarted"/>
        Public Sub [Stop]()

            'owning a lock the token is required to guarantee that no other method call conflicts with starting the service
            'the service has to be atomicaly started/stopped to be in a consistent state
            SyncLock _objTxToken
                'note that we don't check if the thread is started here because this function can be called either way
                'for example, the Start() method can call this function if the start failed

                'dispose of the Tx thread
                If _objTxThread IsNot Nothing Then
                    _objTxThread.Abort()
                    If _objTxThread.IsAlive Then _objTxThread.Join()
                    _objTxThread = Nothing
                End If

                'dispose of the SerialPort instance
                If _objSerialPort IsNot Nothing Then
                    RemoveHandler _objSerialPort.DataReceived, AddressOf RxSerialPortBytes
                    If _objSerialPort.IsOpen() Then _objSerialPort.Close()
                    _objSerialPort.Dispose()
                    _objSerialPort = Nothing
                End If

                'dispose of remaining objects
                _objStopWatch = Nothing
                _objTxQueues = Nothing
                _objTxRxSync = Nothing
                _bytaRxBuf = Nothing
                _objRxBytes = Nothing

                _blnIsStarted = False

            End SyncLock

        End Sub

        '------------ Tx Methods ------------

        ''' <summary>Transmits a packet asynchronously with normal priority.</summary>
        ''' <param name="objPacket">The <see cref="Packet"/> to be transmited.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>
        ''' The awaitable <see cref="Tasks.Task" /> returning <i>True</i> if the packet echoed back from the Loconet interface, indicating a successfull send.
        ''' <i>False</i> could be returned if the service is stopped or the interface did not respond in a timely fashion. 
        ''' </returns>
        ''' <remarks>
        ''' The Loconet service has two transmition queues; a normal queue accessed by <i>TxPacket</i> and a priority queue accessed by <see cref="TxPriorityPacket"/>. 
        ''' Packets placed in the priority queue will jump in line to be transmited ahead of packets placed in the normal queue.
        ''' For example, <see cref="TxPriorityPacket"/> is better suited for locomotive control where responsiveness is more important.
        ''' Response packets are returned through the sending packet's <see cref="Packet.RxPacket"/> property for those that expect them.
        ''' </remarks>
        ''' <example>The following example request data from command station slot 3. Data can then be read from the returned <i>objResponsePacket</i>.
        '''	<code lang="VB">      
        '''    'assumes LoconetService (locServ) has been previously initialized and started
        '''    Dim packet As New PkReqSlotData(3)
        '''    Dim success As Boolean = Await locServ.TxPacket(packet)
        '''    Dim responsePk As Packet = packet.RxPacket
        '''	</code>
        '''	</example>
        ''' <seealso cref="Packet.Clone"/>
        ''' <seealso cref="TxPriorityPacket"/>
        ''' <seealso cref="TxPrioritySpeed"/>
        Public Function TxPacket(objPacket As Packet, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
            Return TxPacket(objPacket, False, sctCancelToken)
        End Function

        ''' <summary>Transmits a packet asynchronously with high priority.</summary>
        ''' <param name="objPacket">The <see cref="Packet"/> to be transmited.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>
        ''' The awaitable <see cref="Tasks.Task" /> returning <i>True</i> if the packet echoed back from the Loconet interface, indicating a successfull send.
        ''' <i>False</i> could be returned if the service is stopped or the interface did not respond in a timely fashion. 
        ''' </returns>
        ''' <remarks>
        ''' The Loconet service has two transmition queues; a normal queue accessed by <see cref="TxPacket(Packet, CancellationToken)"/> and a priority queue accessed by <i>TxPriorityPacket</i>.
        ''' Packets placed in the priority queue will jump in line to be transmited ahead of packets placed in the normal queue.
        ''' For example, <i>TxPriorityPacket</i> is better suited for locomotive control where responsiveness is more important.
        ''' Response packets are returned through the sending packet's <see cref="Packet.RxPacket"/> property for those that expect them.
        ''' </remarks>
        ''' <seealso cref="Packet.Clone"/>
        ''' <seealso cref="TxPacket(Packet, CancellationToken)"/>
        ''' <seealso cref="TxPrioritySpeed"/>
        Public Function TxPriorityPacket(objPacket As Packet, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
            Return TxPacket(objPacket, True, sctCancelToken)
        End Function

        Private Function TxPacket(objPacket As Packet, blnPriority As Boolean, sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objTcs As New TaskCompletionSource(Of Boolean)
            Dim sctCtr As CancellationTokenRegistration

            'register cancellation delegate
            sctCtr = sctCancelToken.Register(
                Sub()
                    '- if sctCancelToken is Nothing when the sctCancelToken.Register() is called this registration is ignored
                    '- otherwise if sctCancelToken has already been canceled when the sctCancelToken.Register() is called,
                    '  this lambda will execute synchronously before the sctCancelToken.Register() returns

                    'synclock guarantees that the service is started and ramains so during this execution
                    SyncLock _objTxToken
                        If _blnIsStarted Then
                            SyncLock _objTxQueues
                                If blnPriority Then
                                    _objTxQueues.PriorityQueue.Remove(objPacket)
                                Else
                                    _objTxQueues.NormalQueue.Remove(objPacket)
                                End If
                            End SyncLock
                        End If
                    End SyncLock

                    objTcs.TrySetCanceled()
                    sctCtr.Dispose()
                End Sub)

            If Not sctCancelToken.IsCancellationRequested Then  'when sctCancelToken is Nothing, IsCancellationRequested is always False

                'register task completion delegate
                objPacket.OnTxComplete =
                    Sub()
                        'gets executed on the LoconetService TxThread
                        objTcs.TrySetResult(_objTxRxSync.EchoeReceived)
                        sctCtr.Dispose()  'safe to call even when sctCtr was never initialized
                    End Sub

                'synclock guarantees that the service is started and ramains so during this execution
                SyncLock _objTxToken
                    If _blnIsStarted Then
                        'enqueue packet and signal worker thread that it has work to do
                        SyncLock _objTxQueues
                            If blnPriority Then
                                _objTxQueues.PriorityQueue.Enqueue(objPacket)
                            Else
                                _objTxQueues.NormalQueue.Enqueue(objPacket)
                            End If
                            Monitor.Pulse(_objTxQueues)
                        End SyncLock
                    Else
                        Return Task.FromResult(False)
                    End If
                End SyncLock

            End If

            Return objTcs.Task

        End Function


        ''' <summary>Transmits a locomotive speed control packet asynchronously with high priority.</summary>
        ''' <param name="bytSlot">Valid values are 1-120. Some command stations have less than 120 slots.</param>
        ''' <param name="bytSpeed">The speed value that should be assigned to the locomotive located at the <paramref  name="bytSlot"/> number.</param>
        ''' <param name="objTag">An optional arbitrary object to be associated with the packet.</param>
        ''' <remarks>
        ''' On average, the Loconet network can handle packet transmissions no faster than one every 9-15ms. If you have a user interface, such as a slider,
        ''' that generates speed changes faster than the described interval, packets can get backlogged in the transmission queues, causing sluggish performance. 
        ''' Unlike <see cref="TxPriorityPacket"/>, this method checks the priority queue for unprocessed speed control packets before inserting a new packet.
        ''' If it finds one with the same slot number, the existing packet will be updated with the new speed value. If no match is found, this method behaves
        ''' similarly to <see cref="TxPriorityPacket"/> except it is not awaitable or cancelable.
        ''' </remarks>
        ''' <example>
        ''' The following two lines are equivalent except the latter overrides obsolete packets backlogged in the queue.
        ''' <code lang="VB">        
        '''     'assumes LoconetService (locServ) has been previously initialized and started
        '''     locServ.TxPriorityPacket(New PkSetSlotSpeed(3, 50))
        '''     locServ.TxPrioritySpeed(3, 50)
        ''' </code>
        ''' </example>
        ''' <seealso cref="TxPacket(Packet, CancellationToken)"/>
        ''' <seealso cref="TxPriorityPacket"/>
        Public Sub TxPrioritySpeed(bytSlot As Byte, bytSpeed As Byte, Optional objTag As Object = Nothing)

            'synclock guarantees that the service is started and ramains so during this execution
            SyncLock _objTxToken
                If _blnIsStarted Then

                    SyncLock _objTxQueues
                        'check if unsent packet exists in tx packet queue
                        Dim objSpeedPacket As PkSetSlotSpeed
                        For Each objPacket As Packet In _objTxQueues.PriorityQueue
                            If TypeOf objPacket Is PkSetSlotSpeed AndAlso
                               DirectCast(objPacket, PkSetSlotSpeed).Slot = bytSlot Then
                                objSpeedPacket = objPacket
                                Exit For
                            End If
                        Next
                        'insert or update packet
                        If objSpeedPacket Is Nothing Then
                            _objTxQueues.PriorityQueue.Enqueue(New PkSetSlotSpeed(bytSlot, bytSpeed, objTag))
                            Monitor.Pulse(_objTxQueues)
                        Else
                            objSpeedPacket.Speed = bytSpeed
                            objSpeedPacket.Tag = objTag
                        End If
                    End SyncLock

                End If
            End SyncLock

        End Sub

        '----------------------------------------

        Private Sub TxThread()
            Dim objCurrTxPacket As Packet

            Do
                objCurrTxPacket = Nothing

                'get new packet from queues
                SyncLock _objTxQueues
                    While objCurrTxPacket Is Nothing
                        If _objTxQueues.PriorityQueue.Count > 0 Then
                            objCurrTxPacket = _objTxQueues.PriorityQueue.Dequeue()
                        End If
                        If objCurrTxPacket Is Nothing Then
                            If _objTxQueues.NormalQueue.Count > 0 Then
                                objCurrTxPacket = _objTxQueues.NormalQueue.Dequeue()
                            End If
                        End If
                        If objCurrTxPacket Is Nothing Then
                            Monitor.Wait(_objTxQueues)
                        End If
                    End While
                End SyncLock

                'send packet
                objCurrTxPacket.SetParityByte()
                objCurrTxPacket.IsEchoe = True
                SyncLock _objTxRxSync
                    Dim blnWritten As Boolean = False
                    Try
                        'SerialPort.Write() bug found here
                        'blocks thread indefinitely after several successfull writes, if command station is disconnected or off while the Locobuffer II USB is connected 
                        'location: SerialStream.cs; EndWrite() method; line: wh.WaitOne(); 
                        _objSerialPort.Write(objCurrTxPacket.Bytes, 0, objCurrTxPacket.Bytes.Length)
                        blnWritten = True
                    Catch ex As Exception
                        'in case the serial port does't exist, the interface is not connected or off, or something went wrong with the interface
                        'the blnWritten should remain False
                    End Try
                    If blnWritten Then
                        'init _objTxRxSync
                        _objTxRxSync.AssertedPacket = objCurrTxPacket
                        _objTxRxSync.EchoeReceived = False

                        'wait for Rx thread to echoe asserted packet
                        _objTxRxSync.MatchNeeded = TxRxSync.MatchType.Echo
                        If Monitor.Wait(_objTxRxSync, PACKET_ECHO_TIMEOUT) Then
                            If objCurrTxPacket.NeedsPacketResponse Then
                                'wait for Rx thread to match a response to asserted packet
                                _objTxRxSync.MatchNeeded = TxRxSync.MatchType.Response
                                Monitor.Wait(_objTxRxSync, PACKET_RESPONSE_TIMEOUT)
                            End If
                        End If

                        'notify task completion only for awaitable TX
                        If objCurrTxPacket.OnTxComplete IsNot Nothing Then objCurrTxPacket.OnTxComplete.Invoke()

                        'clean up
                        _objTxRxSync.AssertedPacket = Nothing
                        _objTxRxSync.MatchNeeded = TxRxSync.MatchType.None
                    Else
                        'in case packet failed to be written

                        _objTxRxSync.EchoeReceived = False
                        If objCurrTxPacket.OnTxComplete IsNot Nothing Then objCurrTxPacket.OnTxComplete.Invoke()
                    End If
                End SyncLock
            Loop

        End Sub


        Private Sub RxSerialPortBytes(sender As Object, e As System.IO.Ports.SerialDataReceivedEventArgs)
            'this Sub is called on a separate thread created by the SerialPort class
            'it is not always guaranteed to be called on the same thread ID but the calls are serialized 

            If Thread.CurrentThread.Name = "" Then Thread.CurrentThread.Name = "LoconetServiceRx" 'can not rename if already named; throws exceptions
            Try
                Dim intBytesRead As Integer = _objSerialPort.Read(_bytaRxBuf, 0, _objSerialPort.BytesToRead)
                ProccessBytes(intBytesRead)
            Catch : End Try
        End Sub

        Private Sub ProccessBytes(intBytesRead As Integer)

            For intParsePos As Integer = 0 To intBytesRead - 1

                '_srtRxPakLen flags:
                '   -1 = current packet length defined by next byte read
                '    0 = current packet length is undefined

                Dim bytCurrByte As Byte = _bytaRxBuf(intParsePos)

                'if current byte is an opcode byte
                If CType(bytCurrByte And 128, Boolean) Then

                    'if start of new packet is premature
                    'in normal conditions count should be 0 here
                    If _objRxBytes.Count > 0 Then
                        CreatePacket(False)
                        _objRxBytes.Clear()
                    End If

                    _objRxBytes.Add(bytCurrByte)
                    Select Case (bytCurrByte And 96) >> 5
                        Case 0
                            _srtRxPakLen = 2
                        Case 1
                            _srtRxPakLen = 4
                        Case 2
                            _srtRxPakLen = 6
                        Case 3
                            _srtRxPakLen = -1  'variable length packet to be determined by next byte
                    End Select
                Else
                    _objRxBytes.Add(bytCurrByte)

                    'if data byte expected is a packet length byte
                    If _srtRxPakLen = -1 Then
                        _srtRxPakLen = bytCurrByte
                    End If

                    'if reached the end of the packet
                    If _srtRxPakLen = _objRxBytes.Count Then
                        CreatePacket(True)
                        _objRxBytes.Clear()
                        _srtRxPakLen = 0
                    End If
                End If

            Next
        End Sub

        Private Sub CreatePacket(blnIsComplete As Boolean)
            Dim objPacket As Packet

            'fixes the problem where FIFO is not quaranteed by the thread locking ready queue
            Thread.Sleep(1)

            'manage packet echo 
            SyncLock _objTxRxSync
                If _objTxRxSync.MatchNeeded = TxRxSync.MatchType.Echo AndAlso
                   _objTxRxSync.AssertedPacket.Equals(_objRxBytes) Then

                    _objTxRxSync.EchoeReceived = True

                    objPacket = _objTxRxSync.AssertedPacket
                    objPacket.TimeStamp = _objStopWatch.Elapsed.TotalSeconds

                    Monitor.Pulse(_objTxRxSync)  'signal Tx thread to continue
                End If
            End SyncLock

            'if received packet was an echo
            If objPacket IsNot Nothing Then
                RaiseRxPacketEvent(objPacket)
                Exit Sub
            End If

            'create packet
            If blnIsComplete Then
                Select Case _objRxBytes(0)
                    Case &H81
                        objPacket = New PkBusy
                    Case &H82
                        objPacket = New PkSetPowerOff
                    Case &H83
                        objPacket = New PkSetPowerOn
                    Case &H85
                        objPacket = New PkSetEmergStop
                    Case &HA0
                        objPacket = New PkSetSlotSpeed
                    Case &HA1
                        objPacket = New PkSetSlotDirFunc
                    Case &HA2
                        objPacket = New PkSetSlotFunc5to8
                    Case &HA3
                        objPacket = New PkSetSlotFunc9to12
                    Case &HB0
                        objPacket = New PkSetSwitch
                    Case &HB1
                        objPacket = New PkSwitchInput
                    Case &HB2
                        objPacket = New PkInput
                    Case &HB4
                        objPacket = New PkLongAck
                    Case &HB5
                        objPacket = New PkSlotStatus
                    Case &HB8
                        objPacket = New PkSlotUnlink
                    Case &HB9
                        objPacket = New PkSlotLink
                    Case &HBA
                        objPacket = New PkSlotMove
                    Case &HBB
                        objPacket = New PkReqSlotData
                    Case &HBC
                        objPacket = New PkGetSwitchState
                    Case &HBE
                        objPacket = New PkSetLocoAdrExp
                    Case &HBF
                        objPacket = New PkSetLocoAdr
                    Case &HD0
                        objPacket = New PkMultiSense
                    Case &HD4
                        objPacket = New PkSlotCommand
                    Case &HD5
                        objPacket = New PkLocoCommand
                    Case &HE4
                        Select Case _objRxBytes(1)
                            Case &HC
                                'OPC_SE_LONG not currently supported
                            Case &H9
                                objPacket = New PkSecurityElem
                            Case &H8
                                objPacket = New PkLissy
                        End Select
                    Case &HE5
                        Select Case _objRxBytes(1)
                            Case &H10
                                Select Case _objRxBytes(4)
                                    Case 1
                                        objPacket = New PkLocoIO
                                    Case Else
                                        objPacket = New PkPeerXfer
                                End Select
                            Case &H9
                                objPacket = New PkFind
                        End Select
                    Case &HE6, &HEE 'read/write slot extended
                        objPacket = New PkRdWrSlotDataExp
                    Case &HE7, &HEF 'read/write slot normal
                        Select Case _objRxBytes(2) 'slot byte
                            Case 0  'special slot
                                objPacket = New PkRdWrSlotData  'use generic for now
                            Case 1 To 120  'loco values
                                objPacket = New PkLocoSlot
                            Case 123  'fast clock
                                objPacket = New PkFastClock
                            Case 124  'DCC decoder programming
                                objPacket = New PkDccProgram
                            Case 127  'command station OPS
                                objPacket = New PkComStatOps
                            Case Else  'generic
                                objPacket = New PkRdWrSlotData
                        End Select
                    Case &HED
                        Select Case _objRxBytes(1)
                            Case &HB
                                objPacket = New PkImmediate
                            Case &HF, &H1F
                                objPacket = New PkSpecialOem
                        End Select
                End Select
                If objPacket Is Nothing Then objPacket = New PkUnknown(_objRxBytes.Count, "Unknown packet type")
            Else
                objPacket = New PkUnknown(_objRxBytes.Count, "Corrupt packet")
            End If
            _objRxBytes.CopyTo(objPacket.Bytes)
            objPacket.TimeStamp = _objStopWatch.Elapsed.TotalSeconds

            'manage packet responses
            SyncLock _objTxRxSync
                If _objTxRxSync.MatchNeeded = TxRxSync.MatchType.Response AndAlso
                   _objTxRxSync.AssertedPacket.ValidPacketResponse(objPacket) Then

                    _objTxRxSync.AssertedPacket.RxPacket = objPacket

                    objPacket.Tag = _objTxRxSync.AssertedPacket.Tag

                    Monitor.Pulse(_objTxRxSync)  'signal Tx thread to continue
                End If
            End SyncLock

            RaiseRxPacketEvent(objPacket)

        End Sub

#End Region

    End Class

#Region "Enumerations"

    ''' <summary>Specifies the baud rate speed for the communication port.</summary>
    ''' <remarks>This enumeration is used by the <see cref="LoconetService.BaudRate"/> property.</remarks>
    ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
    ''' <seealso cref="LoconetService.Start"/>
    Public Enum BaudRate
        ''' <summary>Used by the MS100 interface. Note: The MS100 interface is not recommended since it can not ensure the required timings in hardware.</summary>
        _16457 = 16457
        ''' <summary>Used by Locobuffer and Uhlenbrock.</summary>
        _19200 = 19200
        ''' <summary>Used by Uhlenbrock.</summary>
        _38400 = 38400
        ''' <summary>Used by Locobuffer and Uhlenbrock. This is the recommended rate for Locobuffer.</summary>
        _57600 = 57600
        ''' <summary>Used by Uhlenbrock.</summary>
        _115200 = 115200
    End Enum

    ''' <summary>Specifies the OpCode as defined by Loconet.</summary>
    ''' <remarks>This enumeration is used by <see cref="Packet"/> classes. It represents a code identifier which defines a packet type and its command function as it relates to Loconet.</remarks>
    ''' <seealso cref="Packet"/>
    ''' <seealso cref="Packet.OpCode"/>
    Public Enum OpCodes As Byte
        ''' <summary>Used by <see cref="PkUnknown"/> packet class.</summary>
        None

        '2 byte opcodes
        ''' <summary>Used by <see cref="PkSetEmergStop"/> packet class.</summary>
        IDLE
        ''' <summary>Used by <see cref="PkSetPowerOn"/> packet class.</summary>
        GPON
        ''' <summary>Used by <see cref="PkSetPowerOff"/> packet class.</summary>
        GPOFF
        ''' <summary>Used by <see cref="PkBusy"/> packet class.</summary>
        BUSY

        '4 byte opcodes
        ''' <summary>Used by <see cref="PkSetLocoAdr"/> packet class.</summary>
        LOCO_ADR                'response: SL_RD_DATA or LONG_ACK
        ''' <summary>Used by <see cref="PkSetLocoAdrExp"/> packet class.</summary>
        LOCO_XADR               'response: SL_RD_DATA_EXP or LONG_ACK
        ''' <summary>Not implemented.</summary>
        SW_ACK                  'response: LONG_ACK
        ''' <summary>Used by <see cref="PkGetSwitchState"/> packet class.</summary>
        SW_STATE                'response: LONG_ACK
        ''' <summary>Used by <see cref="PkReqSlotData"/> packet class.</summary>
        RQ_SL_DATA              'response: SL_RD_DATA or SL_RD_DATA_EXP or LONG_ACK
        ''' <summary>Used by <see cref="PkSlotMove"/> packet class.</summary>
        MOVE_SLOTS              'response: SL_RD_DATA or LONG_ACK
        ''' <summary>Used by <see cref="PkSlotLink"/> packet class.</summary>
        LINK_SLOTS              'response: SL_RD_DATA or LONG_ACK
        ''' <summary>Used by <see cref="PkSlotUnlink"/> packet class.</summary>
        UNLINK_SLOTS            'response: SL_RD_DATA
        ''' <summary>Not implemented.</summary>
        CONSIST_FUNC
        ''' <summary>Used by <see cref="PkSlotStatus"/> packet class.</summary>
        SLOT_STAT1
        ''' <summary>Used by <see cref="PkLongAck"/> packet class.</summary>
        LONG_ACK
        ''' <summary>Used by <see cref="PkInput"/> packet class.</summary>
        INPUT_REP
        ''' <summary>Used by <see cref="PkSwitchInput"/> packet class.</summary>
        SW_REP
        ''' <summary>Used by <see cref="PkSetSwitch"/> packet class.</summary>
        SW_REQ                  'response: LONG_ACK for failure only
        ''' <summary>Used by <see cref="PkSetSlotFunc9to12"/> packet class.</summary>
        LOCO_F912
        ''' <summary>Used by <see cref="PkSetSlotFunc5to8"/> packet class.</summary>
        LOCO_SND
        ''' <summary>Used by <see cref="PkSetSlotDirFunc"/> packet class.</summary>
        LOCO_DIRF
        ''' <summary>Used by <see cref="PkSetSlotSpeed"/> packet class.</summary>
        LOCO_SPD

        '6 byte opcode
        ''' <summary>Used by <see cref="PkMultiSense"/> packet class.</summary>
        MULTI_SENSE             'response: LONG_ACK for some types 
        ''' <summary>Used by <see cref="PkSlotCommand"/> packet class.</summary>
        EXP_CMD
        ''' <summary>Used by <see cref="PkLocoCommand"/> packet class.</summary>
        SAFE_COMMANDS

        'variable size opcodes
        ''' <summary>Used by <see cref="PkRdWrSlotData"/> packet class.</summary>
        SL_RD_DATA
        ''' <summary>Used by <see cref="PkRdWrSlotData"/> packet class.</summary>
        WR_SL_DATA              'response: LONG_ACK
        ''' <summary>Used by <see cref="PkRdWrSlotDataExp"/> packet class.</summary>
        SL_RD_DATA_EXP
        ''' <summary>Used by <see cref="PkRdWrSlotDataExp"/> packet class.</summary>
        WR_SL_DATA_EXP          'response: LONG_ACK
        ''' <summary>Used by <see cref="PkPeerXfer"/> packet class.</summary>
        PEER_XFER
        ''' <summary>Not implemented.</summary>
        THROT_STAT
        ''' <summary>Used by <see cref="PkFind"/> packet class.</summary>
        FIND
        ''' <summary>Used by <see cref="PkSecurityElem"/> packet class.</summary>
        SE
        ''' <summary>Used by <see cref="PkLissy"/> packet class.</summary>
        LISSY
        ''' <summary>Used by <see cref="PkImmediate"/> packet class.</summary>
        IMM_PACKET
        ''' <summary>Used by <see cref="PkSpecialOem"/> packet class.</summary>
        SPCL_OEM
    End Enum

    ''' <summary>Specifies the position of a turnout.</summary>
    ''' <remarks>This enumeration is used by the following classes:
    ''' <list type="bullet">
    ''' <item><description><see cref="PkSetSwitch"/></description></item>
    ''' <item><description><see cref="PkRdWrSlotData"/></description></item>
    ''' <item><description><see cref="PkMultiSense"/></description></item>
    ''' <item><description><see cref="PkLongAck"/></description></item>
    ''' <item><description><see cref="PkLocoIO"/></description></item>
    ''' </list>
    ''' </remarks>
    Public Enum SwitchState As Byte
        ''' <summary>Turnout is in a straight or normal position.</summary>
        Closed = 1
        ''' <summary>Turnout is in a diverging position.</summary>
        Thrown = 0
    End Enum

    ''' <summary>Specifies whether a resource is <i>On</i> or <i>Off</i>.</summary>
    Public Enum OnOff As Byte
        ''' <summary>Resource is on or set.</summary>
        [On] = 1
        ''' <summary>Resource is off or cleared.</summary>
        Off = 0
    End Enum

    ''' <summary>Affirmative or negative indicator.</summary>
    Public Enum YesNo As Byte
        ''' <summary>Affirmative.</summary>
        Yes = 1
        ''' <summary>Negative.</summary>
        No = 0
    End Enum

    ''' <summary>Specifies a locomotive's direction of travel.</summary>
    Public Enum LocoDirection As Byte
        'documentations states Forward = 1; Reverse = 0 but this seems wrong
        'when compared to what the DT300 does
        ''' <summary>A forward direction of travel.</summary>
        Forward = 0
        ''' <summary>A reverse direction of travel.</summary>
        Reverse = 1
    End Enum

    ''' <summary>Specifies a locomotive's orientation in relation to the track.</summary>
    Public Enum LocoOrientation As Byte
        ''' <summary>The left side of the locomotive is on rail A when looking forward.</summary>
        RailA = 0
        ''' <summary>The left side of the locomotive is on rail B when looking forward.</summary>
        RailB = 1
    End Enum

    ''' <summary>Specifies the input type of a DS54 input pair.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkInput"/> class.</remarks>
    ''' <seealso cref="PkInput.DS54InputPair"/>
    Public Enum DS54InputType As Byte
        ''' <summary>Auxiliary type input such as a push button.</summary>
        Aux = 0
        ''' <summary>Switch feedback type input.</summary>
        Switch = 1
    End Enum

    ''' <summary>Specifies the activity status of a command station slot.</summary>
    ''' <remarks>This enumeration is used by the following classes:
    ''' <list type="bullet">
    ''' <item><description><see cref="PkSlotStatus"/></description></item>
    ''' <item><description><see cref="PkLocoSlot"/></description></item>
    ''' <item><description><see cref="PkSlotCommand"/></description></item>
    ''' <item><description><see cref="PkRdWrSlotDataExp"/></description></item>
    ''' </list>
    ''' </remarks>
    Public Enum SlotActivity As Byte
        ''' <summary>The slot's status when no locomotive address has ever been assigned to it, since the last command station reset. Data in slot is not valid.</summary>
        Free = 0
        ''' <summary>The slot's status when it is not in use by a throttle but refresh rail packets are still being sent by the command station.
        ''' <para>
        ''' This status indicates that the slot has not been purged for several possible reasons:
        ''' <list type="bullet">
        ''' <item><description>The purge timeout has not expired.</description></item>
        ''' <item><description>Slot is part of a consist.</description></item>
        ''' <item><description>Speed has not been set to 0.</description></item>
        ''' </list>
        ''' </para>
        ''' </summary>
        Common = 1
        ''' <summary>The slot's status when it is not being used by a throttle and no refresh rail packets are being sent by the command station. Slot is purged.</summary>
        Idle = 2
        ''' <summary>The slot's status when it is being used by a throttle.</summary>
        InUse = 3
    End Enum

    ''' <summary>Specifies a slot's speed step configuration.</summary>
    ''' <remarks>This enumeration is used by the following classes:
    ''' <list type="bullet">
    ''' <item><description><see cref="PkSlotStatus"/></description></item>
    ''' <item><description><see cref="PkLocoSlot"/></description></item>
    ''' <item><description><see cref="PkSlotCommand"/></description></item>
    ''' <item><description><see cref="PkRdWrSlotDataExp"/></description></item>
    ''' </list>
    ''' </remarks>
    Public Enum SpeedSteps As Byte
        ''' <summary>28 speed steps.</summary>
        DCC_28_SS = 0
        ''' <summary>14 speed steps (Motorola trinary format).</summary>
        Trinary_Old_14 = 1
        ''' <summary>14 speed steps.</summary>
        DCC_14_SS = 2
        ''' <summary>128 speed steps.</summary>
        DCC_128_SS = 3
        ''' <summary>28 speed steps with advanced functions.</summary>
        DCC_28_SS_Adv = 4
        ''' <summary>Reserved for future implementations.</summary>
        Reserved1 = 5
        ''' <summary>Reserved for future implementations.</summary>
        Reserved2 = 6
        ''' <summary>128 speed steps with advanced functions.</summary>
        DCC_128_SS_Adv = 7
    End Enum

    ''' <summary>Specifies a slot's consist configuration.</summary>
    ''' <remarks>This configuration shows how slots are linked to each other. This enumeration is used by the following classes:
    ''' <list type="bullet">
    ''' <item><description><see cref="PkSlotStatus"/></description></item>
    ''' <item><description><see cref="PkLocoSlot"/></description></item>
    ''' <item><description><see cref="PkSlotCommand"/></description></item>
    ''' <item><description><see cref="PkRdWrSlotDataExp"/></description></item>
    ''' </list>
    ''' </remarks>
    Public Enum ConsistType As Byte
        ''' <summary>Free locomotive with no consist linking.</summary>
        NoConsist = 0
        ''' <summary>Logical consist sub-member only linked upwards.</summary>
        SubMember = 1
        ''' <summary>Logical consist top only linked downwards.</summary>
        ConsistTop = 2
        ''' <summary>Logical mid consist linked up and down.</summary>
        MidConsist = 3
    End Enum

    ''' <summary>Specifies the type of a complex sensor message.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkMultiSense"/> class.</remarks>
    Public Enum MultiSenseType As Byte
        ''' <summary>A message indicating that a transponder has exited a block.</summary>
        TransponderRelease
        ''' <summary>A message indicating that a transponder has entered a block.</summary>
        TransponderDetect
        ''' <summary>A message indicating the transponder's direction of travel.</summary>
        TransponderInputBits
        ''' <summary>A status message generated by a power management device such as a PM4.</summary>
        PowerManagerStatus
        ''' <summary>A message used to program power management devices.</summary>
        PowerManagerOps
        ''' <summary>A message used to program block detection devices.</summary>
        BlockDetectionOps
        ''' <summary>A message used to program security element devices.</summary>
        SecurityElementOps
        ''' <summary>A message used to program stationary decoder devices.</summary>
        StationaryDecoderOps
        ''' <summary>A message not currently supported by this API.</summary>
        OtherDetection
    End Enum

    ''' <summary>Specifies the track programming mode.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkDccProgram"/> class.</remarks>
    Public Enum DccProgMode As Byte
        ''' <summary>Paged mode, byte program, on the service track.</summary>
        ServTrkPagedByte = 32
        ''' <summary>Direct mode, byte program, on the service track.</summary>
        ServTrkDirectByte = 40
        ''' <summary>Direct mode, bit program, on the service track.</summary>
        ServTrkDirectBit = 8
        ''' <summary>Physical register mode, byte program, on the service track.</summary>
        ServTrkPhysRegByte = 16
        ''' <summary>Operational mode, byte program, with feedback.</summary>
        OperByteFeedback = 44
        ''' <summary>Operational mode, byte program, with no feedback.</summary>
        OperByteNoFeedback = 36
        ''' <summary>Operational mode, bit program, with feedback.</summary>
        OperBitFeedback = 12
        ''' <summary>Operational mode, bit program, with no feedback.</summary>
        OperBitNoFeedback = 4
    End Enum

    ''' <summary>Specifies the error types returned by a programming reply.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkDccProgram"/> class.</remarks>
    <Flags()> Public Enum DccProgErrFlags As Byte
        ''' <summary>Service mode programming track is empty (no decoder detected).</summary>        
        NoDecoder = 1
        ''' <summary>No write acknowledge response from decoder.</summary>  
        NoWriteAck = 2
        ''' <summary>Failed to detect read compare acknowledge response from decoder.</summary>  
        NoReadAck = 4
        ''' <summary>The command was aborted.</summary>  
        Aborted = 8
    End Enum

    ''' <summary>Specifies the type of a switch input message.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkSwitchInput"/> class.</remarks> 
    Public Enum SwitchInputType As Byte
        ''' <summary>A message indicating input levels for turnout feedback.</summary>
        InputLevels
        ''' <summary>A message indicating current output levels.</summary>
        OutputLevels
    End Enum

    ''' <summary>Specifies the type of a long acknowledgement message.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkLongAck"/> class.</remarks> 
    Public Enum LongAckType As Byte
        ''' <summary>An acknowledgement type that has not been defined in this library.</summary>
        Unknown
        ''' <summary>An error response from a <see cref="PkSetLocoAdr"/> packet.</summary>
        NoFreeSlot
        ''' <summary>A response from a <see cref="PkGetSwitchState"/> packet with the switch state.</summary>
        SwitchStateResponse
        ''' <summary>An error response from a <see cref="PkSlotMove"/> packet.</summary>
        IllegalMove
        ''' <summary>An error response from a <see cref="PkSlotLink"/> packet.</summary>
        InvalidLink
        ''' <summary>An error response from a <see cref="PkSetSwitch"/> packet.</summary>
        SetSwitchFailed
        ''' <summary>A response from a <see cref="PkMultiSense"/> packet confirming an OPS write to a power management device.</summary>
        PmOpsWriteOK
        ''' <summary>A response from a <see cref="PkMultiSense"/> packet reading an OPS from a power management device.</summary>
        PmOpsRead
        ''' <summary>A response from a <see cref="PkMultiSense"/> packet confirming an OPS write to a block detection device.</summary>
        BdOpsWriteOK
        ''' <summary>A response from a <see cref="PkMultiSense"/> packet reading an OPS from a block detection device.</summary>
        BdOpsRead
        ''' <summary>A response from a <see cref="PkRdWrSlotData"/> packet doing a slot write.</summary>
        ProgNoReply
        ''' <summary>A response from a <see cref="PkDccProgram"/> packet. Programmer is busy and task was aborted.</summary>
        ProgBusy
        ''' <summary>A response from a <see cref="PkDccProgram"/> packet. Task was accepted and reply will follow at completion.</summary>
        ProgAccepted
        ''' <summary>A response from a <see cref="PkDccProgram"/> packet. Task was accepted with no reply to be expected at completion.</summary>
        ProgAcceptedBlind
        ''' <summary>A response from a <see cref="PkImmediate"/> packet.</summary>
        ImmediateRes
        ''' <summary>A response from a <see cref="PkReqSlotData"/> packet.</summary>
        UnsupportedSlot
    End Enum

    ''' <summary>Specifies the direction of travel.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkLissy"/> class.</remarks> 
    Public Enum DirectionNS As Byte
        ''' <summary>Traveling in northerly direction.</summary>
        North
        ''' <summary>Traveling in southerly direction.</summary>
        South
    End Enum

    ''' <summary>Specifies the command type to control a locomotive.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkLocoCommand"/> class.</remarks> 
    Public Enum LocoCommand
        ForSpeed
        RevSpeed
        Func_0_6
        Func_7_13
        Func_14_20
        Func_21_28
    End Enum

    ''' <summary>Specifies the command type to sent to a slot.</summary>
    ''' <remarks>This enumeration is used by the <see cref="PkSlotCommand"/> class.</remarks> 
    Public Enum SlotCommand
        Move
        Link
        Unlink
        Status
        Unknown  'reserved
    End Enum

#End Region

    ''' <summary>Generic bitwise functions that aide in encoding and decoding of packet bytes.</summary>
    Public Module BitWise

        Friend Function GetAddress(bytHighByte As Byte, bytLowByte As Byte) As UShort
            Return (CType(bytHighByte, UShort) << 7) Or bytLowByte
        End Function

        Friend Sub SetAddress(ByRef bytHighByte As Byte, ByRef bytLowByte As Byte,  Value As UShort)
            If Value > 16383 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid address. Valid values are 0-16383.")

            bytHighByte = (Value >> 7) And 127
            bytLowByte = Value And 127
        End Sub

        Friend Function GetDirection(bytDataByte As Byte) As LocoDirection
            Return (bytDataByte And 32) >> 5
        End Function

        Friend Sub SetDirection(ByRef bytDataByte As Byte, Value As LocoDirection)
            If Value = LocoDirection.Reverse Then
                bytDataByte = bytDataByte Or 32
            Else
                bytDataByte = bytDataByte And 223
            End If
        End Sub

        Friend Function GetFunctions0to4(bytDataByte As Byte) As OnOff()
            bytDataByte = (bytDataByte << 1) Or ((bytDataByte >> 4) And 1)
            Return BitWise.BitFlagsToArray(bytDataByte, 5)
        End Function

        Friend Sub SetFunctions0to4(ByRef bytDataByte As Byte,  enuaFuncs As OnOff())
            If enuaFuncs.GetUpperBound(0) <> 4 Then Throw New IndexOutOfRangeException("The upper bound of given array must be 4.")

            Dim bytFuncBits As Byte = BitWise.ArrayToBitFlags(enuaFuncs)        'gets [0,0,0,F4,F3,F2,F1,F0]
            bytFuncBits = (bytFuncBits >> 1) Or ((bytFuncBits And 1) << 4)      'gets [0,0,0,F0,F4,F3,F2,F1]
            bytDataByte = bytFuncBits Or (bytDataByte And 224)                  'gets [P,P,P,F0,F4,F3,F2,F1]    P = previous bits
        End Sub

        Friend Sub SetFunctions5to8or9to12(ByRef bytDataByte As Byte,  enuaFuncs As OnOff())
            If enuaFuncs.GetUpperBound(0) <> 3 Then Throw New IndexOutOfRangeException("The upper bound of given array must be 3.")

            Dim bytFuncBits As Byte = BitWise.ArrayToBitFlags(enuaFuncs)        'gets [0,0,0,0,F8,F7,F6,F5] or [0,0,0,0,F12,F11,F10,F9]
            bytDataByte = bytFuncBits Or (bytDataByte And 240)                  'gets [P,P,P,P,F8,F7,F6,F5] or [P,P,P,P,F12,F11,F10,F9]    P = previous bits
        End Sub

        Friend Function GetSwitch(bytHighByte As Byte, bytLowByte As Byte) As UShort
            Return (((bytHighByte And 15) << 7) Or (bytLowByte And 127)) + 1
        End Function

        Friend Sub SetSwitch(ByRef bytHighByte As Byte, ByRef bytLowByte As Byte, Value As UShort)
            If Value < 1 Or Value > 2048 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid switch number. Valid values are 1-2048.")

            Value -= 1
            bytLowByte = Value And 127
            bytHighByte = ((Value >> 7) And 15) Or (bytHighByte And 240)
        End Sub

        Friend Function GetOnOffState(bytDataByte As Byte) As OnOff
            Return (bytDataByte And 16) >> 4
        End Function

        Friend Sub SetOnOffState(ByRef bytDataByte As Byte, Value As OnOff)
            If Value = OnOff.On Then
                bytDataByte = bytDataByte Or 16
            Else
                bytDataByte = bytDataByte And 239
            End If
        End Sub

        Friend Function GetActivity(bytDataByte As Byte) As SlotActivity
            Return (bytDataByte And 48) >> 4
        End Function

        Friend Sub SetActivity(ByRef bytDataByte As Byte, Value As SlotActivity)
            bytDataByte = bytDataByte And 207
            Select Case Value
                Case SlotActivity.Free
                    'do nothing; above statement removed the bits
                Case SlotActivity.Common
                    bytDataByte = bytDataByte Or 16
                Case SlotActivity.Idle
                    bytDataByte = bytDataByte Or 32
                Case SlotActivity.InUse
                    bytDataByte = bytDataByte Or 48
            End Select
        End Sub

        Friend Function GetConsistType(bytDataByte As Byte) As ConsistType
            Return ((bytDataByte And 8) >> 2) Or ((bytDataByte And 64) >> 6)
        End Function

        Friend Sub SetConsistType(ByRef bytDataByte As Byte,  Value As ConsistType)
            bytDataByte = (bytDataByte And 183) Or ((Value And 1) << 6) Or ((Value And 2) << 2)
        End Sub

        Friend Function GetSpeedSteps(bytDataByte As Byte) As SpeedSteps
            Return bytDataByte And 7
        End Function

        Friend Sub SetSpeedSteps(ByRef bytDataByte As Byte,  Value As SpeedSteps)
            bytDataByte = (bytDataByte And 248) Or Value
        End Sub

        Friend Function GetDS54Address(bytDataByte1 As Byte, bytDataByte2 As Byte) As UShort
            Return (((bytDataByte1 And 124) >> 2) Or ((bytDataByte2 And 15) << 5)) + 1
        End Function

        Friend Sub SetDS54Address(ByRef bytDataByte1 As Byte, ByRef bytDataByte2 As Byte, Value As UShort)
            If Value < 1 Or Value > 512 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DS54 address. Valid values are 1-512.")

            Value -= 1
            bytDataByte1 = ((Value And 31) << 2) Or (bytDataByte1 And 131)
            bytDataByte2 = ((Value And 480) >> 5) Or (bytDataByte2 And 240)
        End Sub

        Friend Function GetDS54InputPair(bytDataByte As Byte) As Byte
            Return (bytDataByte And 3) + 1
        End Function

        Friend Sub SetDS54InputPair(ByRef bytDataByte As Byte, Value As Byte)
            If Value < 1 Or Value > 4 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DS54 input pair. Valid values are 1-4.")

            Value -= 1
            bytDataByte = (Value And 3) Or (bytDataByte And 252)
        End Sub

        Friend Function GetDS54InputType(bytDataByte As Byte) As DS54InputType
            Return (bytDataByte And 32) >> 5
        End Function

        Friend Sub SetDS54InputType(ByRef bytDataByte As Byte, Value As DS54InputType)
            bytDataByte = (Value << 5) Or (bytDataByte And 223)
        End Sub


        ''' <summary>Copies a subset block of bits from one unsigned integer to another.</summary>
        ''' <param name="intSource">Bit source to copy from.</param>
        ''' <param name="bytSrcLoc">Start location of the source bit block. Location is zero based where zero is the least significant bit and the location is the least significant bit of the block.</param>
        ''' <param name="bytSrcLen">Number of bits in the block to copy from the source bytes. Bits are counted from right to left starting at <i>bytSrcLoc</i>.</param>
        ''' <param name="intTarget">Bit target to copy over. Results are returned through this parameter as well as the function's return value.</param>
        ''' <param name="bytTrgLoc">Start location of the target bit block. Location is zero based where zero is the least significant bit and the location is the least significant bit of the block.</param>
        ''' <example>
        ''' Given a byte <i>S</i> of [S7,<b>S6</b>,<b>S5</b>,<b>S4</b>,S3,S2,S1,S0] and a byte <i>T</i> of [T7,T6,T5,T4,T3,T2,T1,T0]
        ''' the following example will copy the bits in bold from <i>S</i> overwritting the bits in <i>T</i> resulting in [T7,T6,T5,T4,<b>S6</b>,<b>S5</b>,<b>S4</b>,T0]. 
        ''' Original bits, other than the three overwritten, are left as they were.
        '''	<code lang="VB">
        ''' CopyBits(S, 4, 3, T, 1)         
        '''	</code>
        '''	</example>
        Public Function CopyBits(intSource As UInteger, bytSrcLoc As Byte, bytSrcLen As Byte, ByRef intTarget As UInteger, bytTrgLoc As Byte) As UInteger

            'configure masks
            Dim intMask As UInteger = Not ((Not 0) << bytSrcLen)
            Dim intSourceMask As UInteger = intMask << bytSrcLoc
            Dim intTargetMask As UInteger = intMask << bytTrgLoc

            'remove bits from target that will be overwritten so the OR with the source bits will work
            intTarget = (intTarget And Not intTargetMask)

            'add bits from source to target
            Select Case True
                Case bytTrgLoc > bytSrcLoc
                    intTarget = intTarget Or ((intSource And intSourceMask) << bytTrgLoc - bytSrcLoc)
                Case bytTrgLoc < bytSrcLoc
                    intTarget = intTarget Or ((intSource And intSourceMask) >> bytSrcLoc - bytTrgLoc)
                Case Else
                    intTarget = intTarget Or (intSource And intSourceMask)
            End Select

            Return intTarget

        End Function

        ''' <summary>Converts bit flag fragments of an unsigned integer to a bit array.</summary>
        ''' <param name="intSource">Source bits to extract the flags from.</param>
        ''' <param name="bytLength">Number of bits that represent the flags, read from right to left.</param>
        ''' <remarks>Assumes the bit flags start with the least significant bit. If they don't, use <see cref="CopyBits" /> to correct this.</remarks>
        Public Function BitFlagsToArray(intSource As UInteger, bytLength As Byte) As OnOff()
            Dim sctBitVec As New BitVector32(intSource)
            Dim enuaData(bytLength - 1) As OnOff
            For bytIdx As Byte = 0 To bytLength - 1
                enuaData(bytIdx) = If(sctBitVec.Item(1 << bytIdx), OnOff.On, OnOff.Off)
            Next
            Return enuaData
        End Function

        ''' <summary>Converts a bit array to bit flags within an unsigned integer.</summary>
        ''' <param name="enuArray">Bit array used as the source for the conversion.</param>
        ''' <remarks>Flags are placed in the unsigned integer from right to left with the first array element placed in the least significant bit.</remarks>
        Public Function ArrayToBitFlags(enuArray As OnOff()) As UInteger
            Dim sctBitVec As New BitVector32(0)
            For bytIdx As Byte = 0 To enuArray.GetUpperBound(0)
                sctBitVec.Item(1 << bytIdx) = enuArray(bytIdx)
            Next
            Return sctBitVec.Data
        End Function

    End Module

#Region "Packets"

    ''' <summary>Provides an abstract base class for all Loconet packet types.</summary>
    ''' <remarks>This class contains the base properties and methods that are common to all packet (classes with the <i>Pk</i> prefix) types.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public MustInherit Class Packet
        Implements ISerializable, ICloneable

        'the KnownType attributes above are strictly for WCF serialization
        'apparently when a specific packet type is marshaled through its base type, WCF needs to know which inherited types are associated with the base type

        ''' <summary>Exposed through <see cref="Bytes"/> property.</summary>
        Protected _bytaBytes As Byte()
        ''' <summary>Exposed through <see cref="ID"/> property.</summary>
        Protected _sctID As Guid = Guid.NewGuid
        ''' <summary>Exposed through <see cref="RxPacket"/> property.</summary>
        Protected _objRxPacket As Packet
        ''' <summary>Exposed through <see cref="Tag"/> property.</summary>
        Protected _objTag As Object
        ''' <summary>Exposed through <see cref="IsEchoe"/> property.</summary>
        Protected _blnIsEchoe As Boolean
        ''' <summary>Exposed through <see cref="TimeStamp"/> property.</summary>
        Protected _dblTimeStamp As Double

        Protected Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            Select Case context.State
                Case StreamingContextStates.File
                    'for layout save
                    Try
                        _bytaBytes = info.GetValue("Bytes", GetType(Object))
                    Catch
                        _bytaBytes = info.GetValue("_bytaPacket", GetType(Object))

                        'compatibility added 11/25/11
                        CtcService.CompatibilityMode = True
                    End Try

                Case StreamingContextStates.All
                    'for WCF
                    _bytaBytes = info.GetValue("Bytes", GetType(Object))
                    _sctID = info.GetValue("ID", GetType(Object))
                    _objRxPacket = info.GetValue("RxPacket", GetType(Object))
                    _blnIsEchoe = info.GetBoolean("IsEchoe")
                    _dblTimeStamp = info.GetDouble("TimeStamp")

            End Select

        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)>
        Protected Overridable Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            Select Case context.State
                Case StreamingContextStates.File
                    'for layout save
                    info.AddValue("Bytes", _bytaBytes)

                Case StreamingContextStates.All
                    'for WCF
                    info.AddValue("Bytes", _bytaBytes)
                    info.AddValue("ID", _sctID)
                    info.AddValue("RxPacket", _objRxPacket)
                    info.AddValue("IsEchoe", _blnIsEchoe)
                    info.AddValue("TimeStamp", _dblTimeStamp)
            End Select

        End Sub

#End Region

        ''' <summary>Gets the unique identifier of the packet.</summary>
        ''' <value>A global unique identifier.</value>
        ''' <remarks>The <i>ID</i> value is automaticaly generated for every <see cref="Packet"/> instance. Clone copies of a <see cref="Packet"/> will each have their own unique IDs.</remarks>
        ''' <seealso cref="Clone"/>
        Public ReadOnly Property ID() As Guid
            Get
                Return _sctID
            End Get
        End Property

        ''' <summary>Gets the received response packet post transmit.</summary>
        ''' <remarks>Some packet types do not expect responses. For those <i>Nothing</i> will be returnd.</remarks>
        ''' <seealso cref="NeedsPacketResponse"/>
        ''' <seealso cref="ValidPacketResponse"/>
        ''' <seealso cref="Clone"/>
        Public Property RxPacket() As Packet
            Get
                Return _objRxPacket
            End Get
            Friend Set(value As Packet)
                _objRxPacket = value
            End Set
        End Property

        ''' <summary>Gets or sets an arbitrary object that is associated with the packet.</summary>
        ''' <value>An <see cref="System.Object"/> that contains data to be associated with the packet. The default is a null reference.</value>
        ''' <remarks>This property's value will be automatically transfered from the request packet to the response packet, if one exists. <i>Tag</i> can be used to attach data to a packet that might be needed when the packet is echoed back or when a response packet is returned.</remarks>
        Public Property Tag() As Object
            Get
                Return _objTag
            End Get
            Set(Value As Object)
                _objTag = Value
            End Set
        End Property

        ''' <summary>Gets value indicating if the packet is an echoe.</summary>
        ''' <value><i>True</i> if the packet is an echoed packet, otherwise <i>False</i>.</value>
        ''' <remarks>This property can be used for monitoring incomming packets to distinguish ones that were generated by the PC versus ones generated by external devices.</remarks>
        ''' <seealso cref="Clone"/>
        Public Property IsEchoe() As Boolean
            Get
                Return _blnIsEchoe
            End Get
            Friend Set(value As Boolean)
                _blnIsEchoe = value
            End Set
        End Property

        ''' <summary>Gets an event time stamp of the packet's echo or arrival.</summary>
        ''' <value>The number of seconds elapsed since the LoconetService was started.</value>
        ''' <remarks>When the LoconetService is started, the internal running timer used to mark packet events is reset to 0. For outgoing packets, their time stamp is marked, when the Loconet interface echoes their successful transmission. For incoming packets the mark occurs on packet arrival.</remarks>
        Public Property TimeStamp() As Double
            Get
                Return _dblTimeStamp
            End Get
            Friend Set(value As Double)
                _dblTimeStamp = value
            End Set
        End Property

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <remarks>This code is automatically assigned to the packet internally. It represents the packet identification code at the Loconet level.</remarks>
        Public MustOverride ReadOnly Property OpCode() As OpCodes

        ''' <summary>Gets the bytes that makes up a Loconet packet message.</summary>
        ''' <value>An array of bytes representing a Loconet message.</value>
        ''' <remarks>These bytes are the raw encoded data (as defined by the Loconet specification) that are transferred between the PC and Loconet devices. These bytes should not be edited directly. Instead use the provided properties of the various <see cref="Packet"/> classes.</remarks>
        ''' <seealso cref="BytesToString"/>
        Public ReadOnly Property Bytes() As Byte()
            Get
                Return _bytaBytes
            End Get
        End Property

        ''' <summary>Converts the <see cref="Packet.Bytes"/> into a readable string.</summary>
        ''' <returns>A string of serialized bytes in hex format.</returns>
        ''' <seealso cref="Bytes"/>
        Public Function BytesToString() As String
            Dim strMsg As New Text.StringBuilder
            For Each bytValue As Byte In _bytaBytes
                strMsg.Append(String.Format("{0}{1:X2}", If(strMsg.Length > 0, " ", ""), bytValue))
            Next
            Return strMsg.ToString
        End Function

        Friend ReadOnly Property ParityIsOK() As Boolean
            Get
                Dim bytCheckSum As Byte = 0
                For Each bytValue As Byte In _bytaBytes
                    bytCheckSum = bytCheckSum Xor bytValue
                Next
                Return bytCheckSum = &HFF
            End Get
        End Property

        Friend Sub SetParityByte()
            Dim bytCheckSum As Byte = 0
            For bytIdx As Byte = 0 To _bytaBytes.Length - 2
                bytCheckSum = bytCheckSum Xor _bytaBytes(bytIdx)
            Next
            _bytaBytes(_bytaBytes.Length - 1) = Not bytCheckSum
        End Sub

        Friend Overloads Function Equals(bytes As IList) As Boolean
            If _bytaBytes.Length <> bytes.Count Then Return False
            For bytIdx As Byte = 0 To _bytaBytes.GetUpperBound(0)
                If _bytaBytes(bytIdx) <> bytes(bytIdx) Then Return False
            Next
            Return True
        End Function

        ''' <summary>Callback delegate registration for async TX task completion.</summary>
        Friend Property OnTxComplete As Action

        ''' <summary>Gets value indicating if this packet expects a response packet.</summary>
        ''' <value><i>True</i> if the packet expects a response, otherwise <i>False</i>.</value>
        ''' <remarks>
        ''' This function is called internally during the Loconet read phase to match response packets to transmitted packets waiting for a response. 
        ''' The base implementation gets this value from one of the bits in the first packet byte (as per spec). 
        ''' This is appropriate for most packet types, however some packets may deviate from this default behavior. 
        ''' For those, this property must be overridden.<p/>If this property returns <i>True</i>, <see cref="Packet.ValidPacketResponse"/> should be overridden in derived classes.
        ''' </remarks>
        ''' <seealso cref="ValidPacketResponse"/>
        Public Overridable ReadOnly Property NeedsPacketResponse() As Boolean
            Get
                Return _bytaBytes(0) And 8
            End Get
        End Property

        ''' <summary>Gets value indicating if given packet is a valid response for this packet.</summary>
        ''' <param name="objResponsePacket">Response packet to be checked for validity.</param>
        ''' <returns><i>True</i> if <i>objResponsePacket</i> is a valid response for this packet; otherwise <i>False</i>.</returns>
        ''' <remarks>
        ''' This function is called internally during the Loconet read phase to match response packets to transmitted packets waiting for a response. 
        ''' The base implementation always returns <i>True</i>. Derived classes should override the base implementation when <see cref="Packet.NeedsPacketResponse"/> returns <i>True</i>.
        ''' </remarks>
        ''' <seealso cref="NeedsPacketResponse"/>
        Public Overridable Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return True
        End Function

        ''' <summary>User friendly description of the packet.</summary>                
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
        ''' <seealso cref="ParmsDesc"/>
        Public MustOverride ReadOnly Property Description() As String

        ''' <summary>User friendly description of the most significant packet parameters.</summary>        
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
        ''' <seealso cref="Description"/>
        Public MustOverride ReadOnly Property ParmsDesc() As String

        ''' <summary>Creates a copy of the packet.</summary>
        ''' <returns>A <see cref="Packet"/> object that has the same data content as the cloned packet.</returns>
        ''' <remarks>
        ''' Only the following methods are cloned. The rest are initialized instead:
        ''' <list type="bullet">
        ''' <item><description><see cref="Bytes"/></description></item>
        ''' <item><description><see cref="Tag"/></description></item>
        ''' </list>
        ''' A packet should not be altered in any way after its transmission since the Loconet service
        ''' will proccess it at an indeterminant time on another thread. If you must send the same
        ''' packet repeatedly, send a clone of it instead to insure packet uniqueness and immutability.
        ''' </remarks>
        ''' <example>
        ''' The following example shows the correct way of transmitting a packet repeatedly:
        ''' <code lang="VB">        
        '''     'assumes LoconetService (locServ) has been previously initialized and started
        '''     Dim packet As New PkReqSlotData(3)
        '''     locServ.TxPacket(packet.Clone)
        '''     packet.Slot = 4
        '''     locServ.TxPacket(packet.Clone)
        '''     packet.Slot = 5
        '''     locServ.TxPacket(packet)
        ''' </code>
        ''' </example>
        ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
        ''' <seealso cref="LoconetService.TxPriorityPacket"/>
        Public Overridable Function Clone() As Object Implements ICloneable.Clone
            'don't know why cloning with MemberwiseClone() does not work properly when transmitting cloned packets with changing values
            'Dim objPacket As Packet = Me.MemberwiseClone()
            'objPacket._sctID = Guid.NewGuid
            'objPacket._blnIsEchoe = False
            'Return objPacket

            Dim objPacket As Packet = Activator.CreateInstance(Me.GetType)
            _bytaBytes.CopyTo(objPacket._bytaBytes, 0)  '"objPacket._bytaBytes = _bytaBytes" strangely causes same problem as MemberwiseClone()
            objPacket._objTag = _objTag
            Return objPacket
        End Function

    End Class

    '⟱⟱⟱ general command station  ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that broadcasts an emergency stop.</summary>
    ''' <remarks>A packet response is not expected. <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSetEmergStop
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(1) {&H85, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.IDLE"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.IDLE
            End Get
        End Property

        ''' <summary>User friendly description of the packet.</summary>       
        ''' <seealso cref="PkSetEmergStop.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Emergency stop broadcast"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>        
        ''' <seealso cref="PkSetEmergStop.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "None"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that turns track power on.</summary>
    ''' <remarks>A packet response is not expected.</remarks>
    ''' <example>See the <see cref="LoconetService.Start"/> method for a code example.</example>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSetPowerOn
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(1) {&H83, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.GPON"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.GPON
            End Get
        End Property

        ''' <summary>User friendly description of the packet.</summary> 
        ''' <seealso cref="PkSetPowerOn.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Track power on"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkSetPowerOn.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "None"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that turns track power off.</summary>
    ''' <remarks>A packet response is not expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>     
    <Serializable()> Public Class PkSetPowerOff
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(1) {&H82, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.GPOFF"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.GPOFF
            End Get
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetPowerOff.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Track power off"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetPowerOff.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "None"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that indicates the master is busy.</summary>
    ''' <remarks>This packet is used as a response, from the command station, to inform other devices that it is still busy proccessing the last command received.</remarks>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>     
    <Serializable()> Public Class PkBusy
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(1) {&H81, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.BUSY"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.BUSY
            End Get
        End Property

        Public Overrides ReadOnly Property NeedsPacketResponse() As Boolean
            Get
                'overridden because spec states this packet does not need a response and should not be transmitted from a PC
                'but Locobuffer responds to this packet with a PeerXfer for repoting
                Return True
            End Get
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            If objResponsePacket.OpCode = OpCodes.PEER_XFER Then
                Dim objPeerPacket As PkPeerXfer = objResponsePacket
                Return objPeerPacket.SourceID = 80 And objPeerPacket.TargetHighAdr = 1 And objPeerPacket.TargetLowAdr = 80
            Else
                Return False
            End If
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkBusy.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Master is busy"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkBusy.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "None"
            End Get
        End Property

    End Class

    '⟱⟱⟱ switching ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that sets the state of a switch.</summary>
    ''' <remarks>A <see cref="PkLongAck"/> response is expected if the command failed, otherwise no response.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkSetSwitch
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB0, 0, 0, 0}
            Me.Key = OnOff.On
        End Sub

        Public Sub New(srtSwitch As UShort, enuState As SwitchState, enuKey As OnOff)
            Me.New()
            Me.Switch = srtSwitch
            Me.State = enuState
            Me.Key = enuKey
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SW_REQ"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SW_REQ
            End Get
        End Property

        ''' <summary>Gets or sets the switch number to be set.</summary>
        ''' <value>Valid values are 1-2048.</value>
        Public Property Switch() As UShort
            Get
                Return BitWise.GetSwitch(_bytaBytes(2), _bytaBytes(1))
            End Get
            Set(Value As UShort)
                BitWise.SetSwitch(_bytaBytes(2), _bytaBytes(1), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the switch.</summary>
        ''' <value>The state of the switch.</value>
        ''' <remarks>The switch state usually indicates the point position of a turnout. However, these packets could alternatively be used for non-turnout functions in which case this state may represent any toggle type state.</remarks>
        Public Property State() As SwitchState
            Get
                Return (_bytaBytes(2) And 32) >> 5
            End Get
            Set(Value As SwitchState)
                If Value = SwitchState.Closed Then
                    _bytaBytes(2) = _bytaBytes(2) Or 32
                Else
                    _bytaBytes(2) = _bytaBytes(2) And 223
                End If
            End Set
        End Property

        ''' <summary>Gets or sets the key press state of the set switch command.</summary>
        ''' <value><see cref="OnOff.On"/> for key down; <see cref="OnOff.Off"/> for key up.</value>
        ''' <remarks>
        ''' Loconet throttles usually generate pairs of these <i>PkSetSwitch</i> packets for each state change. 
        ''' One when the <i>Set Switch</i> button is depressed and a second when released with <i>Key</i> values of <i>On</i> and <i>Off</i> respectively. 
        ''' Similarly, when generating these packets from the PC, this behavior could be simulated through this property.
        ''' </remarks>
        Public Property Key() As OnOff
            Get
                Return BitWise.GetOnOffState(_bytaBytes(2))
            End Get
            Set(Value As OnOff)
                BitWise.SetOnOffState(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary> 
        ''' <seealso cref="PkSetSwitch.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Switch control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkSetSwitch.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Switch={Me.Switch} Dir={Me.State} Key={Me.Key}"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that requests the state of a switch.</summary>
    ''' <remarks>A <see cref="PkLongAck"/> response is expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkGetSwitchState
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HBC, 0, 0, 0}
        End Sub

        Public Sub New(srtSwitch As UShort)
            Me.New()
            Me.Switch = srtSwitch
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SW_STATE"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SW_STATE
            End Get
        End Property

        ''' <summary>Gets or sets the switch number to be queried.</summary>
        ''' <value>The number of the switch.</value>
        ''' <remarks>Valid switch numbers range between 1 and 2048 inclusive.</remarks>
        Public Property Switch() As UShort
            Get
                Return BitWise.GetSwitch(_bytaBytes(2), _bytaBytes(1))
            End Get
            Set(Value As UShort)
                BitWise.SetSwitch(_bytaBytes(2), _bytaBytes(1), Value)
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkGetSwitchState.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Switch state request"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkGetSwitchState.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "Num=" & Me.Switch
            End Get
        End Property

    End Class

    '⟱⟱⟱ loco control ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that assigns a locomotive address to a slot.</summary>
    ''' <remarks>One of two packet responses is expected:
    ''' <list type="bullet">
    ''' <item><description>A <see cref="PkLocoSlot"/>, if address assignment was successful.</description></item>
    ''' <item><description>A <see cref="PkLongAck"/>, if no free slot was found.</description></item>
    ''' </list>
    ''' </remarks>
    ''' <seealso cref="PkSetLocoAdrExp"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkSetLocoAdr
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HBF, 0, 0, 0}
        End Sub

        Public Sub New(srtAddress As UShort)
            Me.New()
            Me.Address = srtAddress
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_ADR"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_ADR
            End Get
        End Property

        ''' <summary>Gets or sets the DCC locomotive address to be assigned to a free slot.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        Public Property Address() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(1), _bytaBytes(2))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(1), _bytaBytes(2), Value)
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA Or
                   objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetLocoAdr.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Locomotive slot assignment"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetLocoAdr.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "Adr=" & Me.Address
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that sets the speed of a locomotive assigned to a slot.</summary>
    ''' <remarks>
    ''' A packet response is not expected.
    ''' <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.
    ''' </remarks>
    ''' <seealso cref="PkLocoCommand"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.TxPrioritySpeed"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkSetSlotSpeed
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HA0, 0, 0, 0}
        End Sub

        Public Sub New(bytSlot As Byte, bytSpeed As Byte, Optional objTag As Object = Nothing)
            Me.New()
            Me.Slot = bytSlot
            Me.Speed = bytSpeed
            Me.Tag = objTag
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_SPD"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_SPD
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property Slot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's speed setting.</summary>
        ''' <value>Valid speed values are 0-127</value>
        Public Property Speed() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid speed value. Valid values are 0-127.")

                _bytaBytes(2) = Value
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetSlotSpeed.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Speed control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetSlotSpeed.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slot={0} Spd={1}", Me.Slot, Me.Speed)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that sets the direction of travel and functions 0 through 4 for a locomotive assigned to a slot.</summary>
    ''' <remarks>
    ''' A packet response is not expected.
    ''' <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.
    ''' </remarks>
    ''' <seealso cref="PkLocoCommand"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSetSlotDirFunc
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HA1, 0, 0, 0}
        End Sub

        Public Sub New(bytSlot As Byte, enuDirection As LocoDirection, enuaFuncs As OnOff())
            Me.New()
            Me.Slot = bytSlot
            Me.Direction = enuDirection
            Me.Functions = enuaFuncs
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_DIRF"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_DIRF
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property Slot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's direction of travel.</summary>
        Public Property Direction() As LocoDirection
            Get
                Return BitWise.GetDirection(_bytaBytes(2))
            End Get
            Set(Value As LocoDirection)
                BitWise.SetDirection(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 0 through 4.</summary>
        ''' <value>A zero based array of five members of the <see cref="OnOff"/> enumeration type.</value>
        Public Property Functions() As OnOff()
            Get
                Return BitWise.GetFunctions0to4(_bytaBytes(2))
            End Get
            Set(Value As OnOff())
                BitWise.SetFunctions0to4(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetSlotDirFunc.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Direction/function control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetSlotDirFunc.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Dim f() As OnOff = Me.Functions
                Return $"Slot={Me.Slot} Dir={Me.Direction} Func(0-4)={{{f(0)},{f(1)},{f(2)},{f(3)},{f(4)}}}"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that sets the functions 5 through 8 for a locomotive assigned to a slot.</summary>
    ''' <remarks>
    ''' A packet response is not expected.
    ''' <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.
    ''' </remarks>
    ''' <seealso cref="PkLocoCommand"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSetSlotFunc5to8
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HA2, 0, 0, 0}
        End Sub

        Public Sub New(bytSlot As Byte, enuaFuncs As OnOff())
            Me.New()
            Me.Slot = bytSlot
            Me.Functions = enuaFuncs
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_SND"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_SND
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property Slot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 5 through 8.</summary>
        ''' <value>A zero based array of four members of the <see cref="OnOff"/> enumeration type.</value>
        Public Property Functions() As OnOff()
            Get
                Return BitWise.BitFlagsToArray(_bytaBytes(2), 4)
            End Get
            Set(Value As OnOff())
                BitWise.SetFunctions5to8or9to12(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetSlotFunc5to8.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Function control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetSlotFunc5to8.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Dim f() As OnOff = Me.Functions
                Return $"Slot={Me.Slot} Func(5-8)={{{f(0)},{f(1)},{f(2)},{f(3)}}}"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that sets the functions 9 through 12 for a locomotive assigned to a slot.</summary>
    ''' <remarks>
    ''' A packet response is not expected.
    ''' <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.
    ''' </remarks>
    ''' <seealso cref="PkLocoCommand"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSetSlotFunc9to12
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HA3, 0, 0, 0}
        End Sub

        Public Sub New(bytSlot As Byte, enuaFuncs As OnOff())
            Me.New()
            Me.Slot = bytSlot
            Me.Functions = enuaFuncs
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_F912"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_F912
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property Slot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 9 through 12.</summary>
        ''' <value>A zero based array of four members of the <see cref="OnOff"/> enumeration type.</value>
        Public Property Functions() As OnOff()
            Get
                'currently uses same decoding as PkSetSlotFunc5to8 but this is not documented in the Loconet spec
                Return BitWise.BitFlagsToArray(_bytaBytes(2), 4)
            End Get
            Set(Value As OnOff())
                BitWise.SetFunctions5to8or9to12(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetSlotFunc9to12.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Function control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetSlotFunc9to12.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Dim f() As OnOff = Me.Functions
                Return $"Slot={Me.Slot} Func(9-12)={{{f(0)},{f(1)},{f(2)},{f(3)}}}"
            End Get
        End Property

    End Class

    '⟱⟱⟱ slot manipulation ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that moves a slot.</summary>
    ''' <remarks>One of two packet responses is expected:
    ''' <list type="bullet">
    ''' <item><description>A <see cref="PkRdWrSlotData"/>, if the slot move was successful.</description></item>
    ''' <item><description>A <see cref="PkLongAck"/>, if the slot move was illegal.</description></item>
    ''' </list>
    ''' </remarks>
    ''' <example>This example demonstrates how <i>PkSlotMove</i> is used to purge a slot:
    '''	<code lang="VB">    
    '''     'assumes LoconetService (locServ) has been previously initialized and started
    '''
    '''     'slot being released
    '''     Dim slot As Byte = 5
    '''      
    '''     'set slot speed to 0
    '''     locServ.TxPacket(New PkSetSlotSpeed(slot, 0))  
    '''     
    '''     'set slot status
    '''     locServ.TxPacket(New PkSlotStatus(slot, SlotActivity.Idle, ConsistType.NoConsist, SpeedSteps.DCC_128_SS))
    '''     
    '''     'do a slot move to special slot 0
    '''     locServ.TxPacket(New PkSlotMove(slot, 0)) 
    '''	</code>
    '''	</example>
    '''	<seealso cref="PkSlotCommand"/> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>     
    <Serializable()> Public Class PkSlotMove
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HBA, 0, 0, 0}
        End Sub

        Public Sub New(bytSourceSlot As Byte, bytDestSlot As Byte)
            Me.New()
            Me.SourceSlot = bytSourceSlot
            Me.DestSlot = bytDestSlot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.MOVE_SLOTS"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.MOVE_SLOTS
            End Get
        End Property

        ''' <summary>Gets or sets the source slot to move from.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property SourceSlot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid source slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the destination slot to move to.</summary>
        ''' <value>Valid slot numbers are 0-120. Some command stations have less than 120 slots.</value>
        ''' <remarks>A destination slot of zero releases (purges) the source slot.</remarks>
        Public Property DestSlot() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid destination slot number. Valid values are 0-120.")

                _bytaBytes(2) = Value
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA Or
                   objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSlotMove.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot move"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkSlotMove.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Src={0} Dest={1}", Me.SourceSlot, Me.DestSlot)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that links a slot to another.</summary>
    ''' <remarks>This packet is used for creating consists. One of two packet responses is expected:
    ''' <list type="bullet">
    ''' <item><description>A <see cref="PkRdWrSlotData"/>, if the slot link was successful.</description></item>
    ''' <item><description>A <see cref="PkLongAck"/>, if the slot link was invalid.</description></item>
    ''' </list>
    ''' </remarks>
    '''	<seealso cref="PkSlotCommand"/> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSlotLink
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB9, 0, 0, 0}
        End Sub

        Public Sub New(bytSlaveSlot As Byte, bytMasterSlot As Byte)
            Me.New()
            Me.SlaveSlot = bytSlaveSlot
            Me.MasterSlot = bytMasterSlot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LINK_SLOTS"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LINK_SLOTS
            End Get
        End Property

        ''' <summary>Gets or sets the slave slot to be linked to the master slot.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property SlaveSlot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slave slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the master slot which the slave slot will link to.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property MasterSlot() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid master slot number. Valid values are 1-120.")

                _bytaBytes(2) = Value
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA Or
                   objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSlotLink.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot link"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSlotLink.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slave={0} Master={1}", Me.SlaveSlot, Me.MasterSlot)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that unlinks a slot from another.</summary>
    ''' <remarks>This packet is used to break up consists. A <see cref="PkRdWrSlotData"/> response is expected.</remarks>
    '''	<seealso cref="PkSlotCommand"/> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSlotUnlink
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB8, 0, 0, 0}
        End Sub

        Public Sub New(bytUnlinkSlot As Byte, bytFromSlot As Byte)
            Me.New()
            Me.UnlinkSlot = bytUnlinkSlot
            Me.FromSlot = bytFromSlot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.UNLINK_SLOTS"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.UNLINK_SLOTS
            End Get
        End Property

        ''' <summary>Gets or sets the slot to be unlinked.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property UnlinkSlot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid unlink slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the slot to unlink from.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property FromSlot() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid from slot number. Valid values are 1-120.")

                _bytaBytes(2) = Value
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSlotUnlink.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot unlink"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSlotUnlink.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Unlink={0} From={1}", Me.UnlinkSlot, Me.FromSlot)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that sets a slot's status.</summary>
    ''' <remarks>A packet response is not expected.</remarks>
    ''' <example>See the <see cref="PkSlotMove"/> class for a code example.</example>
    '''	<seealso cref="PkSlotCommand"/> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>     
    <Serializable()> Public Class PkSlotStatus
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB5, 0, 0, 0}
        End Sub

        Public Sub New(bytSlot As Byte, enuActivity As SlotActivity, enuConsistType As ConsistType, enuSpeedSteps As SpeedSteps)
            Me.New()
            Me.Slot = bytSlot
            Me.Activity = enuActivity
            Me.ConsistType = enuConsistType
            Me.SpeedSteps = enuSpeedSteps
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SLOT_STAT1"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SLOT_STAT1
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Property Slot() As Byte
            Get
                Return _bytaBytes(1)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                _bytaBytes(1) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the slot's activity status.</summary>
        Public Property Activity() As SlotActivity
            Get
                Return BitWise.GetActivity(_bytaBytes(2))
            End Get
            Set(Value As SlotActivity)
                BitWise.SetActivity(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's consist configuration.</summary>
        Public Property ConsistType() As ConsistType
            Get
                Return BitWise.GetConsistType(_bytaBytes(2))
            End Get
            Set(Value As ConsistType)
                BitWise.SetConsistType(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's speed step configuration.</summary>
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return BitWise.GetSpeedSteps(_bytaBytes(2))
            End Get
            Set(Value As SpeedSteps)
                BitWise.SetSpeedSteps(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSlotStatus.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot status"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSlotStatus.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slot={0} Activity={1}", Me.Slot, Me.Activity.ToString)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that requests a slot's data.</summary>
    ''' <remarks>
    ''' The packet responses depend on the following:
    ''' <li>Normal slot 1-120 requests will receive a <see cref="PkLocoSlot"/> packet.</li>
    ''' <li>Normal slot 123 request will receive a <see cref="PkFastClock"/> packet.</li>
    ''' <li>Normal slot 124 request will receive a <see cref="PkDccProgram"/> packet.</li>
    ''' <li>Normal slot 127 request will receive a <see cref="PkComStatOps"/> packet.</li>
    ''' <li>Other normal slots requests will receive a generic <see cref="PkRdWrSlotData"/> packet.</li>
    ''' <li>Expanded slot requests, will receive a <see cref="PkRdWrSlotDataExp"/> packet.</li>
    ''' <li>Uhlenbrock's Intellibox uniquely responds with a <see cref="PkLongAck"/> packet for slots it doesn't support.</li>
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkReqSlotData
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HBB, 0, 0, 0}
        End Sub

        Public Sub New(slot As UShort, Optional expanded As Boolean = False)
            Me.New()
            Me.Expanded = expanded  'must be set first
            Me.Slot = slot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.RQ_SL_DATA"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.RQ_SL_DATA
            End Get
        End Property

        ''' <summary>Indicates that the newer expanded slot format is expected as response.</summary>
        ''' <value>Defaults to <i>False</i> if not set.</value>
        ''' <remarks>Expanded format only supported by newer command stations such as Digitrax DCS52, DCS210, and DCS240.</remarks>
        Public Property Expanded() As Boolean
            Get
                Return BitWise.CopyBits(_bytaBytes(2), 6, 1, 0, 0)
            End Get
            Set(value As Boolean)
                BitWise.CopyBits(value, 0, 1, _bytaBytes(2), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid values are 0-127 for standard slot requests and 0-511 for expanded slot requests.</value>
        ''' <remarks>For proper encoding, make sure to set the <see cref="PkReqSlotData.Expanded"/> property prior to setting this one.</remarks>
        Public Property Slot() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(1), 0, 7, 0, 0)      'least significant 7 bits for normal slot request
                If Me.Expanded Then
                    Return BitWise.CopyBits(_bytaBytes(2), 0, 2, bits, 7)   'extra 2 bits for expanded slot request
                Else
                    Return bits
                End If
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(1), 0)     'least significant 7 bits for normal slot request
                If Me.Expanded Then
                    BitWise.CopyBits(value, 7, 2, _bytaBytes(2), 0) 'extra 2 bits for expanded slot request
                End If
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            If Not Me.Expanded Then
                Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA OrElse
                       objResponsePacket.OpCode = OpCodes.LONG_ACK                  'Intellibox returns LONG_ACK for unsupported slots
            Else
                Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA_EXP OrElse     'if command station supports expanded requests
                       objResponsePacket.OpCode = OpCodes.SL_RD_DATA OrElse         'if command station doesn't support expanded requests
                       objResponsePacket.OpCode = OpCodes.LONG_ACK                  'don't know if Intellibox even supports expanded slots but just in case
            End If
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkReqSlotData.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot data request"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkReqSlotData.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Expanded={Me.Expanded} Slot={Me.Slot}"
            End Get
        End Property

    End Class

    ''' <summary>Represents a base Loconet packet that reads/writes data from/to a command station's slot.</summary>
    ''' <remarks>For slot writes, a <see cref="PkLongAck"/> response is expected.</remarks>
    ''' <seealso cref="PkRdWrSlotDataExp"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkRdWrSlotData
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(13) {&HEF, &HE, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.WR_SL_DATA"/> or <see cref="OpCodes.SL_RD_DATA"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Select Case _bytaBytes(0)
                    Case &HEF
                        Return OpCodes.WR_SL_DATA
                    Case &HE7
                        Return OpCodes.SL_RD_DATA
                End Select
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid values are 1-127.</value>
        Public Overridable Property Slot() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-127.")

                _bytaBytes(2) = Value
            End Set
        End Property

        Public ReadOnly Property ServTrackIsBusy() As Boolean
            Get
                Return _bytaBytes(7) And 8
            End Get
        End Property

        Public ReadOnly Property IsLoconet11() As Boolean
            Get
                Return _bytaBytes(7) And 4
            End Get
        End Property

        Public ReadOnly Property TrackIsPaused() As Boolean
            Get
                Return Not CType(_bytaBytes(7) And 2, Boolean)
            End Get
        End Property

        Public ReadOnly Property TrackPowerIsOn() As Boolean
            Get
                Return _bytaBytes(7) And 1
            End Get
        End Property

        ''' <remarks>Only returns valid value from slot 0.</remarks>
        Public ReadOnly Property IsIntellibox() As Boolean
            Get
                Return Chr(_bytaBytes(11)) = "I" And Chr(_bytaBytes(12)) = "B"
            End Get
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkRdWrSlotData.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA
                        Return "Slot data write"
                    Case OpCodes.SL_RD_DATA
                        Return "Slot data read"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkRdWrSlotData.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Slot={Me.Slot}"
            End Get
        End Property

        ''' <summary>Same as <see cref="Packet.Clone"/> but also converts to a slot write if cloaning a slot read.</summary>
        Public Overrides Function Clone() As Object
            Dim objPacket As PkRdWrSlotData = MyBase.Clone()
            objPacket.Bytes(0) = &HEF   'this converts the cloned packet to a write slot packet in case we are cloning a read slot packet
            Return objPacket
        End Function

    End Class

    ''' <summary>Represents a Loconet packet that reads/writes locomotive data from/to the command station.</summary>
    ''' <remarks>
    ''' This packet type reads and writes from command station slots 1-120.
    ''' For slot writes, a <see cref="PkLongAck"/> response is expected.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkLocoSlot
        Inherits PkRdWrSlotData

        Public Sub New()
            MyBase.New()
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid slot numbers are 1-120. Some command stations have less than 120 slots.</value>
        Public Overrides Property Slot() As Byte
            Get
                Return MyBase.Slot
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 120 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid slot number. Valid values are 1-120.")

                MyBase.Slot = Value
            End Set
        End Property

        ''' <summary>Gets or sets the slot's activity status.</summary>
        Public Property Activity() As SlotActivity
            Get
                Return BitWise.GetActivity(_bytaBytes(3))
            End Get
            Set(Value As SlotActivity)
                BitWise.SetActivity(_bytaBytes(3), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's consist configuration.</summary>
        Public Property ConsistType() As ConsistType
            Get
                Return BitWise.GetConsistType(_bytaBytes(3))
            End Get
            Set(Value As ConsistType)
                BitWise.SetConsistType(_bytaBytes(3), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's speed step configuration.</summary>
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return BitWise.GetSpeedSteps(_bytaBytes(3))
            End Get
            Set(Value As SpeedSteps)
                BitWise.SetSpeedSteps(_bytaBytes(3), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC locomotive address assigned to the slot.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        Public Property Address() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(9), _bytaBytes(4))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(9), _bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's speed setting.</summary>
        ''' <value>Valid speed values are 0-127</value>
        Public Property Speed() As Byte
            Get
                Return _bytaBytes(5)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid speed value. Valid values are 0-127.")

                _bytaBytes(5) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's direction of travel.</summary>
        Public Property Direction() As LocoDirection
            Get
                Return BitWise.GetDirection(_bytaBytes(6))
            End Get
            Set(Value As LocoDirection)
                BitWise.SetDirection(_bytaBytes(6), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 0 through 4.</summary>
        ''' <value>A zero based array of five members of the <see cref="OnOff"/> enumeration type.</value>
        Public Property Functions0to4() As OnOff()
            Get
                Return BitWise.GetFunctions0to4(_bytaBytes(6))
            End Get
            Set(Value As OnOff())
                BitWise.SetFunctions0to4(_bytaBytes(6), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 5 through 8.</summary>
        ''' <value>A zero based array of four members of the <see cref="OnOff"/> enumeration type.</value>
        Public Property Functions5to8() As OnOff()
            Get
                Return BitWise.BitFlagsToArray(_bytaBytes(10), 4)
            End Get
            Set(Value As OnOff())
                BitWise.SetFunctions5to8or9to12(_bytaBytes(10), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the throttle ID associated with a slot.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        ''' <remarks>This value is usually set by a physical throttle after acquiring a slot, but a PC application can also set this with an arbitrary value for identification purposes.</remarks>
        Public Property ThrottleID() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(12), _bytaBytes(11))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(12), _bytaBytes(11), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkLocoSlot.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA
                        Return "Loco slot write"
                    Case OpCodes.SL_RD_DATA
                        Return "Loco slot read"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkLocoSlot.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slot={0} Adr={1} Spd={2}", Me.Slot, Me.Address, Me.Speed)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that reads/writes fast clock from/to the command station.</summary>
    ''' <remarks>
    ''' This packet type reads and writes from command station slot 123.
    ''' For slot writes, a <see cref="PkLongAck"/> response is expected.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkFastClock
        Inherits PkRdWrSlotData

        Public Sub New()
            MyBase.New()
            MyBase.Slot = 123
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the command stations's slot number.</summary>
        ''' <returns>This value is set by the constructor to 123 and can not be changed.</returns>        
        Public Overrides Property Slot() As Byte
            Get
                Return MyBase.Slot
            End Get
            Set(Value As Byte)
                'slot is set in the constructor and should not be changed
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkFastClock.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA
                        Return "Fast clock write"
                    Case OpCodes.SL_RD_DATA
                        Return "Fast clock read"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkFastClock.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slot={0}", Me.Slot)
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet used to program DCC decoders on the track.</summary>
    ''' <remarks>
    ''' A <see cref="PkLongAck"/> response is expected indicating if programming with given parameters is allowed.
    ''' If a valid programming task is started, a <see cref="PkDccProgram"/> response will also follow echoing most of the request values plus the <see cref="PkDccProgram.ProgErrFlags"/> indicating if the task returned an error.
    ''' This packet type reads and writes from command station slot 124.
    ''' </remarks>
    ''' <example>The following are four examples communicating with decoders.
    '''	<code lang="VB">    
    '''     'assumes LoconetService (locServ) has been previously initialized and started
    '''
    '''     'sets CV57 (speed compensation) and is performed on the service track
    '''     locServ.TxPacket(New PkDccProgram(True, DccProgMode.ServTrkPagedByte, 57, 5))
    ''' 
    '''     'also sets CV57 but in opperation mode; note: decoder address 2045 is also specified
    '''     locServ.TxPacket(New PkDccProgram(True, DccProgMode.OperByteNoFeedback, 2045, 57, 5))
    ''' 
    '''     'gets value of CV57 on the service track; note the CvValue can be any value like 0
    '''     locServ.TxPacket(New PkDccProgram(False, DccProgMode.ServTrkPagedByte, 57, 0))
    ''' 
    '''     'simple way to set CV17 and CV18 to the 4 digit address of 2045
    '''     'this is a Shared helper function that internally sends two PkDccProgram packets
    '''     PkDccProgram.SetDcc4DigitAddress(locServ, DccProgMode.ServTrkPagedByte, 2045)
    ''' </code>
    '''	</example> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkDccProgram
        Inherits PkRdWrSlotData

        Public Sub New()
            MyBase.New()
            MyBase.Slot = 124
        End Sub

        ''' <param name="blnProgWrite">This value will be assigned to the <see cref="ProgWrite"/> property.</param>
        ''' <param name="enuProgMode">This value will be assigned to the <see cref="ProgMode"/> property.</param>
        ''' <param name="srtCvNumber">This value will be assigned to the <see cref="CvNumber"/> property.</param>
        ''' <param name="bytCvValue">This value will be assigned to the <see cref="CvValue"/> property.</param>
        ''' <remarks>This constructor is best used for service track programming.</remarks>
        Public Sub New(blnProgWrite As Boolean, enuProgMode As DccProgMode, srtCvNumber As UShort, bytCvValue As Byte)
            Me.New()
            Me.ProgWrite = blnProgWrite
            Me.ProgMode = enuProgMode
            Me.CvNumber = srtCvNumber
            Me.CvValue = bytCvValue
        End Sub

        ''' <param name="blnProgWrite">This value will be assigned to the <see cref="ProgWrite"/> property.</param>
        ''' <param name="enuProgMode">This value will be assigned to the <see cref="ProgMode"/> property.</param>
        ''' <param name="srtOpAddress">This value will be assigned to the <see cref="OpAddress"/> property.</param>
        ''' <param name="srtCvNumber">This value will be assigned to the <see cref="CvNumber"/> property.</param>
        ''' <param name="bytCvValue">This value will be assigned to the <see cref="CvValue"/> property.</param>
        ''' <remarks>This constructor is best used for operational mode programming.</remarks>
        Public Sub New(blnProgWrite As Boolean, enuProgMode As DccProgMode, srtOpAddress As UShort, srtCvNumber As UShort, bytCvValue As Byte)
            Me.New()
            Me.ProgWrite = blnProgWrite
            Me.ProgMode = enuProgMode
            Me.OpAddress = srtOpAddress
            Me.CvNumber = srtCvNumber
            Me.CvValue = bytCvValue
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the command stations's slot number.</summary>
        ''' <returns>This value is set by the constructor to 124 and can not be changed.</returns>  
        Public Overrides Property Slot() As Byte
            Get
                Return MyBase.Slot
            End Get
            Set(Value As Byte)
                'slot is set in the constructor and should not be changed
            End Set
        End Property

        ''' <summary>Gets or sets if packet is used for reading or writing data.</summary>
        ''' <value><i>True</i> if packet writes data; <i>False</i> if packet reads data.</value>
        Public Property ProgWrite() As Boolean
            Get
                Return _bytaBytes(3) And 64
            End Get
            Set(Value As Boolean)
                If Value Then
                    _bytaBytes(3) = _bytaBytes(3) Or 64
                Else
                    _bytaBytes(3) = _bytaBytes(3) And 191
                End If
            End Set
        End Property

        ''' <summary>Gets or sets the track programming mode.</summary>
        ''' <value>One of the <see cref="DccProgMode"/> values.</value>
        Public Property ProgMode() As DccProgMode
            Get
                Return _bytaBytes(3) And 60
            End Get
            Set(value As DccProgMode)
                _bytaBytes(3) = (_bytaBytes(3) And 195) Or value
            End Set
        End Property

        ''' <summary>Error flags returned from an attempted programming task.</summary>
        ''' <value>One or more of the <see cref="DccProgErrFlags"/>.</value>
        Public ReadOnly Property ProgErrFlags() As Byte
            Get
                Return _bytaBytes(4)
            End Get
        End Property

        ''' <summary>Gets or sets the DCC decoder's address for operation mode programming.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        ''' <remarks>This value should be set only if the <see cref="ProgMode"/> is set to one of the operation modes, otherwise it should be 0.</remarks>
        Public Property OpAddress() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(5), _bytaBytes(6))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(5), _bytaBytes(6), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC decoder configuration number.</summary>
        ''' <value>Valid values can not be outside 1-1024 but check the decoder's documentation actual valid values.</value>
        ''' <seealso cref="CvValue"/>
        Public Property CvNumber() As UShort
            Get
                Return (_bytaBytes(9) Or ((_bytaBytes(8) And 1) << 7) Or (CType(_bytaBytes(8) And 48, UShort) << 4)) + 1
            End Get
            Set(value As UShort)
                If value < 1 Or value > 1024 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid CV number. Valid values are 1-1024.")

                value -= 1
                _bytaBytes(9) = value And 127
                _bytaBytes(8) = (_bytaBytes(8) And 254) Or ((value And 128) >> 7)
                _bytaBytes(8) = (_bytaBytes(8) And 207) Or ((value And 768) >> 4)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC decoder configuration value.</summary>
        ''' <value>Valid values are 0-255.</value>
        ''' <remarks>Check the decoder's documentation for valid values.</remarks>
        ''' <seealso cref="CvNumber"/>
        Public Property CvValue() As Byte
            Get
                Return _bytaBytes(10) Or ((_bytaBytes(8) And 2) << 6)
            End Get
            Set(value As Byte)
                'no range check required here because all values in a byte are allowed

                _bytaBytes(10) = value And 127
                _bytaBytes(8) = (_bytaBytes(8) And 253) Or ((value And 128) >> 6)
            End Set
        End Property

        ''' <summary>Sets the four digit address of a DCC decoder.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param>
        ''' <param name="enuProgMode">One of the <see cref="DccProgMode"/> values that specifies the programming mode.</param>
        ''' <param name="srtAddress">The four digit DCC Address</param>
        ''' <remarks>Simplifies the proceess of programming CV17 and CV18 by encoding the four digit address into two <see cref="PkDccProgram"/> packets and transmitting them. Note: This method is <i>Shared</i> so no instance is required.</remarks>
        Public Shared Async Function SetDcc4DigitAddress(objLoconetService As LoconetService, enuProgMode As DccProgMode, srtAddress As UShort) As Task
            Await objLoconetService.TxPacket(New PkDccProgram(True, enuProgMode, 17, (srtAddress >> 8) + 192))
            Await objLoconetService.TxPacket(New PkDccProgram(True, enuProgMode, 18, srtAddress And 255))
            Await objLoconetService.TxPacket(New PkDccProgram(True, enuProgMode, 29, 38)) 'configure for 4 digit address
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkDccProgram.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA
                        If Me.ProgWrite Then
                            Return "DCC program write"
                        Else
                            Return "DCC program request read"
                        End If
                    Case OpCodes.SL_RD_DATA
                        Return "DCC program read"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkDccProgram.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Mode={0} OpAdr={1} CvNum={2} CvValue={3}{4}", _
                Me.ProgMode.ToString, _
                Me.OpAddress, _
                Me.CvNumber, _
                Me.CvValue, _
                If(Me.OpCode = OpCodes.SL_RD_DATA, " Reply=" & Me.ProgErrFlags, ""))
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that reads/writes operation switches from/to the command station.</summary>
    ''' <remarks>    
    ''' This packet type reads and writes from command station slot 127.
    ''' For slot writes, a <see cref="PkLongAck"/> response is expected.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkComStatOps
        Inherits PkRdWrSlotData

        Public Sub New()
            MyBase.New()
            MyBase.Slot = 127
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the command stations's slot number.</summary>
        ''' <returns>This value is set by the constructor to 127 and can not be changed.</returns>   
        Public Overrides Property Slot() As Byte
            Get
                Return MyBase.Slot
            End Get
            Set(Value As Byte)
                'slot is set in the constructor and should not be changed
            End Set
        End Property

        ''' <summary>Gets or sets the OPS configuration values of a command station.</summary>
        ''' <value>An array of OPS values with an upper bound of 64.</value>
        ''' <remarks>To access an OPS value in the array use the index value corresponding to the OPS number. Array value at index 0 is ignored.</remarks>
        Public Property CommandStationOps() As SwitchState()
            Get
                Dim enuaOps(64) As SwitchState
                Dim objBitArray As New BitArray(New Byte() {_bytaBytes(3), _bytaBytes(4), _bytaBytes(5), _bytaBytes(6), _bytaBytes(8), _bytaBytes(9), _bytaBytes(10), _bytaBytes(11)})
                For bytIdx As Byte = 0 To 63
                    If objBitArray(bytIdx) Then
                        enuaOps(bytIdx + 1) = SwitchState.Closed
                    Else
                        enuaOps(bytIdx + 1) = SwitchState.Thrown
                    End If
                Next
                Return enuaOps
            End Get
            Set(Value As SwitchState())
                If Value.GetUpperBound(0) <> 64 Then Throw New IndexOutOfRangeException("The upper bound of the given array must be 64.")

                Dim objBitArray As New BitArray(64)
                For bytIdx As Byte = 1 To 64
                    If bytIdx Mod 8 = 0 Then
                        objBitArray(bytIdx - 1) = 0  'make sure the 8th bit, in each non-OpCode byte, is 0 as per Loconet spec
                    Else
                        objBitArray(bytIdx - 1) = Value(bytIdx)
                    End If
                Next

                Dim bytaOpsPacketBytes(7) As Byte
                objBitArray.CopyTo(bytaOpsPacketBytes, 0)

                Dim bytSrcIdx As Byte = 0
                For Each bytDestIdx As Byte In New Byte() {3, 4, 5, 6, 8, 9, 10, 11}
                    _bytaBytes(bytDestIdx) = bytaOpsPacketBytes(bytSrcIdx)
                    bytSrcIdx += 1
                Next
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkComStatOps.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA
                        Return "Command station OPS write"
                    Case OpCodes.SL_RD_DATA
                        Return "Command station OPS read"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkComStatOps.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return String.Format("Slot={0}", Me.Slot)
            End Get
        End Property

    End Class

    '⟱⟱⟱ expanded messages for newer command stations ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that assigns a locomotive address to a slot.</summary>
    ''' <remarks>
    ''' This uses the expanded version format only supported by newer command stations such as Digitrax DCS52, DCS210, and DCS240.
    ''' One of two packet responses is expected:
    ''' <list type="bullet">
    ''' <item><description>A <see cref="PkRdWrSlotDataExp"/>, if address assignment was successful.</description></item>
    ''' <item><description>A <see cref="PkLongAck"/>, if no free slot was found.</description></item>
    ''' </list>
    ''' </remarks>
    ''' <seealso cref="PkSetLocoAdr"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkSetLocoAdrExp
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HBE, 0, 0, 0}
        End Sub

        Public Sub New(address As UShort)
            Me.New()
            Me.Address = address
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LOCO_XADR"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LOCO_XADR
            End Get
        End Property

        ''' <summary>Gets or sets the DCC locomotive address to be assigned to a free slot.</summary>
        ''' <value>Valid values are 0-16383.</value>
        Public Property Address() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(1), _bytaBytes(2))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(1), _bytaBytes(2), Value)
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA_EXP Or
                   objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSetLocoAdrExp.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Locomotive slot assignment (Exp)"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSetLocoAdrExp.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "Adr=" & Me.Address
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that controls speed, direction, and functions of a locomotive assigned to a slot.</summary>
    ''' <remarks>
    ''' This uses the expanded version format only supported by newer command stations such as Digitrax DCS52, DCS210, and DCS240.
    ''' A packet response is not expected.
    ''' <b>Note:</b> This packet type is a good candidate to be used with the <see cref="LoconetService.TxPriorityPacket"/> method rather than the convetional <see cref="LoconetService.TxPacket(Packet, CancellationToken)"/> method.
    ''' </remarks>
    ''' <seealso cref="PkSetSlotSpeed"/>
    ''' <seealso cref="PkSetSlotDirFunc"/>
    ''' <seealso cref="PkSetSlotFunc5to8"/>
    ''' <seealso cref="PkSetSlotFunc9to12"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.TxPriorityPacket"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>  
    <Serializable()> Public Class PkLocoCommand
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(5) {&HD5, 0, 0, 0, 0, 0}
        End Sub

        Public Sub New(slot As Byte, command As LocoCommand)
            Me.New()
            Me.Slot = slot
            Me.Command = command
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SAFE_COMMANDS"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SAFE_COMMANDS
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid values are 0-511.</value>
        Public Property Slot() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(2), 0, 7, 0, 0)  'least significant 7 bits
                Return BitWise.CopyBits(_bytaBytes(1), 0, 2, bits, 7)   'extra 2 most significant bits
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(2), 0)     'least significant 7 bits
                BitWise.CopyBits(value, 7, 2, _bytaBytes(1), 0)     'extra 2 most significant bits
            End Set
        End Property

        ''' <summary>Actual command value being abstracted from the user becaues Digitrax is encoding function 28 in an unintuitive manner.</summary>
        Private Property _Command() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(1), 3, 4, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 4, _bytaBytes(1), 3)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive command being submitted.</summary>
        Public Property Command() As LocoCommand
            Get
                Return If(Me._Command = 6, 5, Me._Command)
            End Get
            Set(value As LocoCommand)
                Me._Command = value
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's speed setting.</summary>
        ''' <remarks>Only use if <see cref="PkLocoCommand.Command"/> is either <see cref="LocoCommand.ForSpeed"/> or <see cref="LocoCommand.RevSpeed"/>.</remarks>
        ''' <value>Valid values are 0-127</value>
        Public Property Speed() As Byte
            Get
                Return _bytaBytes(4)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(4), 0)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions.</summary>
        ''' <remarks>
        ''' Only use if <see cref="PkLocoCommand.Command"/> is either:
        ''' <li><see cref="LocoCommand.Func_0_6"/></li>
        ''' <li><see cref="LocoCommand.Func_7_13"/></li>
        ''' <li><see cref="LocoCommand.Func_14_20"/></li>
        ''' <li><see cref="LocoCommand.Func_21_28"/></li>
        ''' For proper encoding, make sure to set the <see cref="PkLocoCommand.Command"/> property prior to setting this one.
        ''' </remarks>
        ''' <value>An array of functions states in the range specified by <see cref="PkLocoCommand.Command"/>.</value>
        Public Property Functions() As OnOff()
            Get
                Dim funcs = BitWise.BitFlagsToArray(_bytaBytes(4), 7).ToList
                Select Case Me._Command  'strangely Func 28 is stored in the command bits
                    Case 5
                        funcs.Add(OnOff.Off)
                    Case 6
                        funcs.Add(OnOff.On)
                End Select
                Return funcs.ToArray
            End Get
            Set(value As OnOff())
                BitWise.CopyBits(BitWise.ArrayToBitFlags(value), 0, 7, _bytaBytes(4), 0)
                If Me.Command = LocoCommand.Func_21_28 Then
                    Select Case value(7)
                        Case OnOff.Off
                            Me._Command = 5
                        Case OnOff.On
                            Me._Command = 6
                    End Select
                End If
            End Set
        End Property

        ''' <summary>Gets or sets the throttle ID that is the owner of the slot.</summary>
        ''' <remarks>Slot data is only updated if this ID matches with equivalent value currently in slot.</remarks>
        ''' <value>Valid values are 0-127</value>
        Public Property ThrottleID() As Byte
            Get
                Return _bytaBytes(3)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(3), 0)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkLocoCommand.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Locomotive command (Exp)"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkLocoCommand.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Select Case Me.Command
                    Case LocoCommand.ForSpeed
                        Return $"Slot={Me.Slot} Dir=Forward Spd={Me.Speed}"
                    Case LocoCommand.RevSpeed
                        Return $"Slot={Me.Slot} Dir=Reverse Spd={Me.Speed}"
                End Select

                Dim f() As OnOff = Me.Functions
                Select Case Me.Command
                    Case LocoCommand.Func_0_6
                        Return $"Slot={Me.Slot} Func(0-6)={{{f(0)},{f(1)},{f(2)},{f(3)},{f(4)},{f(5)},{f(6)}}}"
                    Case LocoCommand.Func_7_13
                        Return $"Slot={Me.Slot} Func(7-13)={{{f(0)},{f(1)},{f(2)},{f(3)},{f(4)},{f(5)},{f(6)}}}"
                    Case LocoCommand.Func_14_20
                        Return $"Slot={Me.Slot} Func(14-20)={{{f(0)},{f(1)},{f(2)},{f(3)},{f(4)},{f(5)},{f(6)}}}"
                    Case LocoCommand.Func_21_28
                        Return $"Slot={Me.Slot} Func(21-28)={{{f(0)},{f(1)},{f(2)},{f(3)},{f(4)},{f(5)},{f(6)},{f(7)}}}"
                End Select
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that performs slot move, link, unlink, and status.</summary>
    ''' <remarks>
    ''' This uses the expanded version format only supported by newer command stations such as Digitrax DCS52, DCS210, and DCS240.
    ''' A packet response is not expected.
    ''' </remarks>
    ''' <seealso cref="PkSlotMove"/>
    ''' <seealso cref="PkSlotLink"/>
    ''' <seealso cref="PkSlotUnlink"/>
    ''' <seealso cref="PkSlotStatus"/> 
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkSlotCommand
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(5) {&HD4, 0, 0, 0, 0, 0}
        End Sub

        Public Sub New(slot As Byte)
            Me.New()
            Me.Slot = slot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.EXP_CMD"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.EXP_CMD
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid values are 0-511.</value>
        Public Property Slot() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(2), 0, 7, 0, 0)  'least significant 7 bits
                Return BitWise.CopyBits(_bytaBytes(1), 0, 2, bits, 7)   'extra 2 most significant bits
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(2), 0)     'least significant 7 bits
                BitWise.CopyBits(value, 7, 2, _bytaBytes(1), 0)     'extra 2 most significant bits
            End Set
        End Property

        ''' <summary>Gets or sets the slot command being submitted.</summary>
        Public Property Command() As SlotCommand
            Get
                If BitWise.CopyBits(_bytaBytes(1), 3, 4, 0, 0) = 7 Then
                    Select Case BitWise.CopyBits(_bytaBytes(3), 4, 3, 0, 0)
                        Case 0
                            Return SlotCommand.Move
                        Case 4
                            Return SlotCommand.Link
                        Case 5
                            Return SlotCommand.Unlink
                        Case 6
                            Return SlotCommand.Status
                        Case Else
                            Return SlotCommand.Unknown
                    End Select
                Else
                    Return SlotCommand.Unknown
                End If
            End Get
            Set(value As SlotCommand)
                If value = SlotCommand.Unknown Then Return
                BitWise.CopyBits(7, 0, 4, _bytaBytes(1), 3)
                Select Case value
                    Case SlotCommand.Move
                        BitWise.CopyBits(0, 0, 3, _bytaBytes(3), 4)
                    Case SlotCommand.Link
                        BitWise.CopyBits(4, 0, 3, _bytaBytes(3), 4)
                    Case SlotCommand.Unlink
                        BitWise.CopyBits(5, 0, 3, _bytaBytes(3), 4)
                    Case SlotCommand.Status
                        BitWise.CopyBits(6, 0, 3, _bytaBytes(3), 4)
                End Select
            End Set
        End Property

        ''' <summary>Gets or sets the command stations's destination slot number.</summary>
        ''' <value>Valid values are 0-511.</value>
        ''' <remarks>Not valid if value of <see cref="PkSlotCommand.Command"/> is <see cref="SlotCommand.Status"/>./></remarks>
        Public Property DestSlot() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(4), 0, 7, 0, 0)  'least significant 7 bits
                Return BitWise.CopyBits(_bytaBytes(3), 0, 2, bits, 7)   'extra 2 most significant bits
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(4), 0)     'least significant 7 bits
                BitWise.CopyBits(value, 7, 2, _bytaBytes(3), 0)     'extra 2 most significant bits
            End Set
        End Property

        ''' <summary>Gets or sets the slot's activity status.</summary>
        ''' <remarks>Only valid if value of <see cref="PkSlotCommand.Command"/> is <see cref="SlotCommand.Status"/>./></remarks>
        Public Property Activity() As SlotActivity
            Get
                Return BitWise.GetActivity(_bytaBytes(4))
            End Get
            Set(Value As SlotActivity)
                BitWise.SetActivity(_bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's consist configuration.</summary>
        ''' <remarks>Only valid if value of <see cref="PkSlotCommand.Command"/> is <see cref="SlotCommand.Status"/>./></remarks>
        Public Property ConsistType() As ConsistType
            Get
                Return BitWise.GetConsistType(_bytaBytes(4))
            End Get
            Set(Value As ConsistType)
                BitWise.SetConsistType(_bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's speed step configuration.</summary>
        ''' <remarks>Only valid if value of <see cref="PkSlotCommand.Command"/> is <see cref="SlotCommand.Status"/>./></remarks>
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return BitWise.GetSpeedSteps(_bytaBytes(4))
            End Get
            Set(Value As SpeedSteps)
                BitWise.SetSpeedSteps(_bytaBytes(4), Value)
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.SL_RD_DATA_EXP
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSlotCommand.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Slot command (Exp)"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSlotCommand.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Slot={Me.Slot} Cmd={Me.Command}"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that reads/writes data from/to a command station's slot.</summary>    
    ''' <remarks>
    ''' This uses the expanded version format only supported by newer command stations such as Digitrax DCS52, DCS210, and DCS240.
    ''' For slot writes, a <see cref="PkLongAck"/> response is expected.
    ''' </remarks>
    ''' <seealso cref="PkRdWrSlotData"/>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkRdWrSlotDataExp
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(20) {&HEE, &H15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        End Sub

        Public Sub New(slot As Byte)
            Me.New()
            Me.Slot = slot
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.WR_SL_DATA_EXP"/> or <see cref="OpCodes.SL_RD_DATA_EXP"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Select Case _bytaBytes(0)
                    Case &HEE
                        Return OpCodes.WR_SL_DATA_EXP
                    Case &HE6
                        Return OpCodes.SL_RD_DATA_EXP
                End Select
            End Get
        End Property

        ''' <summary>Gets or sets the command stations's slot number.</summary>
        ''' <value>Valid values are 0-511.</value>
        Public Property Slot() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(3), 0, 7, 0, 0)  'least significant 7 bits
                Return BitWise.CopyBits(_bytaBytes(2), 0, 2, bits, 7)   'extra 2 most significant bits
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(3), 0)     'least significant 7 bits
                BitWise.CopyBits(value, 7, 2, _bytaBytes(2), 0)     'extra 2 most significant bits
            End Set
        End Property

        ''' <summary>Gets or sets the slot's activity status.</summary>
        Public Property Activity() As SlotActivity
            Get
                Return BitWise.GetActivity(_bytaBytes(4))
            End Get
            Set(Value As SlotActivity)
                BitWise.SetActivity(_bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's consist configuration.</summary>
        Public Property ConsistType() As ConsistType
            Get
                Return BitWise.GetConsistType(_bytaBytes(4))
            End Get
            Set(Value As ConsistType)
                BitWise.SetConsistType(_bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the slot's speed step configuration.</summary>
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return BitWise.GetSpeedSteps(_bytaBytes(4))
            End Get
            Set(Value As SpeedSteps)
                BitWise.SetSpeedSteps(_bytaBytes(4), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC locomotive address assigned to the slot.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        Public Property Address() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(6), _bytaBytes(5))
            End Get
            Set(Value As UShort)
                BitWise.SetAddress(_bytaBytes(6), _bytaBytes(5), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's speed setting.</summary>
        ''' <value>Valid speed values are 0-127</value>
        Public Property Speed() As Byte
            Get
                Return _bytaBytes(8)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid speed value. Valid values are 0-127.")

                _bytaBytes(8) = Value
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's direction of travel.</summary>
        Public Property Direction() As LocoDirection
            Get
                Return BitWise.GetDirection(_bytaBytes(10))
            End Get
            Set(Value As LocoDirection)
                BitWise.SetDirection(_bytaBytes(10), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the locomotive's functions 0 through 28.</summary>
        ''' <value>A 29 member zero based array.</value>
        Public Property Functions() As OnOff()
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(10), 4, 1, 0, 0)   'f0
                bits = BitWise.CopyBits(_bytaBytes(10), 0, 4, bits, 1)    'f1-f4
                bits = BitWise.CopyBits(_bytaBytes(11), 0, 7, bits, 5)    'f5-f11
                bits = BitWise.CopyBits(_bytaBytes(9), 4, 1, bits, 12)    'f12
                bits = BitWise.CopyBits(_bytaBytes(12), 0, 7, bits, 13)   'f13-f19
                bits = BitWise.CopyBits(_bytaBytes(9), 5, 1, bits, 20)    'f20
                bits = BitWise.CopyBits(_bytaBytes(13), 0, 7, bits, 21)   'f21-f27
                bits = BitWise.CopyBits(_bytaBytes(9), 6, 1, bits, 28)    'f28
                Return BitWise.BitFlagsToArray(bits, 29)
            End Get
            Set(value As OnOff())
                Dim bits = BitWise.ArrayToBitFlags(value)
                BitWise.CopyBits(bits, 0, 1, _bytaBytes(10), 4)    'f0
                BitWise.CopyBits(bits, 1, 4, _bytaBytes(10), 0)    'f1-f4
                BitWise.CopyBits(bits, 5, 7, _bytaBytes(11), 0)    'f5-f11
                BitWise.CopyBits(bits, 12, 1, _bytaBytes(9), 4)    'f12
                BitWise.CopyBits(bits, 13, 7, _bytaBytes(12), 0)   'f13-f19
                BitWise.CopyBits(bits, 20, 1, _bytaBytes(9), 5)    'f20
                BitWise.CopyBits(bits, 21, 7, _bytaBytes(13), 0)   'f21-f27
                BitWise.CopyBits(bits, 28, 1, _bytaBytes(9), 6)    'f28
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkRdWrSlotDataExp.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.OpCode
                    Case OpCodes.WR_SL_DATA_EXP
                        Return "Slot data write (Exp)"
                    Case OpCodes.SL_RD_DATA_EXP
                        Return "Slot data read (Exp)"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkRdWrSlotDataExp.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Slot={Me.Slot} Adr={Me.Address} Spd={Me.Speed}"
            End Get
        End Property

        ''' <summary>Same as <see cref="Packet.Clone"/> but also converts to a slot write if cloaning a slot read.</summary>
        Public Overrides Function Clone() As Object
            Dim objPacket As PkRdWrSlotDataExp = MyBase.Clone()
            objPacket.Bytes(0) = &HEE   'this converts the cloned packet to a write slot packet in case we are cloning a read slot packet
            Return objPacket
        End Function

    End Class

    '⟱⟱⟱ input ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that is generated by a general sensor device.</summary>
    ''' <remarks><i>PkInput</i> packets are usually received from input devices such as the BDL16 or DS54, however other devices such as the LocoIO can use this packet type in both directions input/output depending on the device's configuration.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkInput
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB2, 0, 0, 0}
        End Sub

        Public Sub New(srtAddress As UShort, enuState As OnOff)
            Me.New()
            Me.Address = srtAddress
            Me.State = enuState
        End Sub

        Public Sub New(srtBDL16Address As UShort, bytBDL16Port As Byte, enuState As OnOff)
            Me.New()
            Me.BDL16Address = srtBDL16Address
            Me.BDL16Port = bytBDL16Port
            Me.State = enuState
        End Sub

        Public Sub New(srtDS54Address As UShort, bytDS54InputPair As Byte, enuDS54InputType As DS54InputType, enuState As OnOff)
            Me.New()
            Me.DS54Address = srtDS54Address
            Me.DS54InputPair = bytDS54InputPair
            Me.DS54InputType = enuDS54InputType
            Me.State = enuState
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.INPUT_REP"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.INPUT_REP
            End Get
        End Property

        ''' <summary>Gets or sets the device independent address.</summary>
        ''' <value>Valid address values are 0-4095.</value>
        Public Property Address() As UShort
            Get
                'converts from: Byte(1)[0,A7,A6,A5,A4,A3,A2,A1], Byte(2)[0,x,A0,x,A11,A10,A9,A8] to [A11,A10,A9,A8,A7,A6,A5,A4,A3,A2,A1,A0]
                Dim srtResult As UShort = 0
                CopyBits(_bytaBytes(2), 5, 1, srtResult, 0)     'A0
                CopyBits(_bytaBytes(1), 0, 7, srtResult, 1)     'A1-A7  
                CopyBits(_bytaBytes(2), 0, 4, srtResult, 8)     'A8-A11
                Return srtResult
            End Get
            Set(Value As UShort)
                If Value > 4095 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid address. Valid values are 0-4095.")

                CopyBits(Value, 1, 7, _bytaBytes(1), 0)     'A1-A7 
                CopyBits(Value, 0, 1, _bytaBytes(2), 5)     'A0
                CopyBits(Value, 8, 4, _bytaBytes(2), 0)     'A8-A11
            End Set
        End Property

        ''' <summary>Gets or sets the DBL16x device address.</summary>
        ''' <value>Valid address values are 1-256.</value>
        Public Property BDL16Address() As UShort
            Get
                'gets A1-A8 from Byte(1)[0,A4,A3,A2,A1,P4,P3,P2]; Byte(2)[0,x,P1,x,A8,A7,A6,A5]
                Return (((_bytaBytes(1) And 120) >> 3) Or ((_bytaBytes(2) And 15) << 4)) + 1
            End Get
            Set(Value As UShort)
                If Value < 1 Or Value > 256 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid BDL16 address. Valid values are 1-256.")

                Value -= 1
                _bytaBytes(1) = ((Value And 15) << 3) Or (_bytaBytes(1) And 135)
                _bytaBytes(2) = ((Value And 240) >> 4) Or (_bytaBytes(2) And 240)
            End Set
        End Property

        ''' <summary>Gets or sets the DBL16x port number.</summary>
        ''' <value>Valid port values are 1-16.</value>
        Public Property BDL16Port() As Byte
            Get
                'gets P1-P4 from Byte(1)[0,A4,A3,A2,A1,P4,P3,P2]; Byte(2)[0,x,P1,x,A8,A7,A6,A5]
                Return (((_bytaBytes(1) And 7) << 1) Or ((_bytaBytes(2) And 32) >> 5)) + 1
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 16 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid BDL16 port. Valid values are 1-16.")

                Value -= 1
                _bytaBytes(1) = ((Value >> 1) And 7) Or (_bytaBytes(1) And 248)
                _bytaBytes(2) = ((Value And 1) << 5) Or (_bytaBytes(2) And 223)
            End Set
        End Property

        ''' <summary>Gets or sets the DS54 device address.</summary>
        ''' <value>Valid address values are 1-512.</value>
        Public Property DS54Address() As UShort
            Get
                Return BitWise.GetDS54Address(_bytaBytes(1), _bytaBytes(2))
            End Get
            Set(Value As UShort)
                BitWise.SetDS54Address(_bytaBytes(1), _bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DS54 input pair.</summary>
        ''' <value>Valid pair values are 1-4.</value>
        Public Property DS54InputPair() As Byte
            Get
                Return BitWise.GetDS54InputPair(_bytaBytes(1))
            End Get
            Set(Value As Byte)
                BitWise.SetDS54InputPair(_bytaBytes(1), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DS54 input type.</summary>
        Public Property DS54InputType() As DS54InputType
            Get
                Return BitWise.GetDS54InputType(_bytaBytes(2))
            End Get
            Set(Value As DS54InputType)
                BitWise.SetDS54InputType(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the reported state of the input address.</summary>
        Public Property State() As OnOff
            Get
                Return BitWise.GetOnOffState(_bytaBytes(2))
            End Get
            Set(Value As OnOff)
                BitWise.SetOnOffState(_bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkInput.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Sensor event"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkInput.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Adr={Me.Address} State={Me.State} BDL16(Adr={Me.BDL16Address} Port={Me.BDL16Port}) DS54(Adr={Me.DS54Address} Input={Me.DS54InputPair} Type={Me.DS54InputType})"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that is generated by a turnout sensor device.</summary>
    ''' <remarks><i>PkSwitchInput</i> packets are usually received from devices such as the DS54 which sense turnout positions.</remarks>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>    
    <Serializable()> Public Class PkSwitchInput
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(3) {&HB1, 0, 0, 0}
        End Sub

        Public Sub New(srtAddress As UShort, enuState As OnOff)
            Me.New()
            Me.Address = srtAddress
            Me.State = enuState
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SW_REP"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SW_REP
            End Get
        End Property

        ''' <summary>Gets or sets the device independent input address.</summary>
        ''' <value>Valid values are 0-2047.</value>
        Public Property Address() As UShort
            Get
                'converts from: Byte(1)[0,A6,A5,A4,A3,A2,A1,A0], Byte(2)[0,x,x,x,A10,A9,A8,A7] to [A10,A9,A8,A7,A6,A5,A4,A3,A2,A1,A0]
                Dim srtResult As UShort = _bytaBytes(1)         'A0-A6 
                CopyBits(_bytaBytes(2), 0, 4, srtResult, 7)     'A7-A10
                Return srtResult
            End Get
            Set(Value As UShort)
                If Value > 2047 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid address. Valid values are 0-2047.")

                CopyBits(Value, 0, 7, _bytaBytes(1), 0)     'A0-A6 
                CopyBits(Value, 7, 4, _bytaBytes(2), 0)     'A7-A10
            End Set
        End Property

        ''' <summary>Gets or sets the reported switch number.</summary>
        ''' <value>Valid values are 1-2048.</value>
        Public Property Switch() As UShort
            Get
                Return BitWise.GetSwitch(_bytaBytes(2), _bytaBytes(1))
            End Get
            Set(Value As UShort)
                BitWise.SetSwitch(_bytaBytes(2), _bytaBytes(1), Value)
            End Set
        End Property

        Public ReadOnly Property Type() As SwitchInputType
            Get
                Select Case _bytaBytes(2) >> 6
                    Case 0
                        Return SwitchInputType.OutputLevels
                    Case 1
                        Return SwitchInputType.InputLevels
                End Select
            End Get
        End Property

        ''' <summary>Gets or sets the DS54 device address.</summary>
        ''' <value>Valid address values are 1-512.</value>
        Public Property DS54Address() As UShort
            Get
                Return BitWise.GetDS54Address(_bytaBytes(1), _bytaBytes(2))
            End Get
            Set(Value As UShort)
                BitWise.SetDS54Address(_bytaBytes(1), _bytaBytes(2), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DS54 input pair.</summary>
        ''' <value>Valid address values are 1-4.</value>
        Public Property DS54InputPair() As Byte
            Get
                Return BitWise.GetDS54InputPair(_bytaBytes(1))
            End Get
            Set(Value As Byte)
                BitWise.SetDS54InputPair(_bytaBytes(1), Value)
            End Set
        End Property

        ''' <summary>Gets or sets the DS54 input type.</summary>
        Public Property DS54InputType() As DS54InputType
            Get
                Return BitWise.GetDS54InputType(_bytaBytes(2))
            End Get
            Set(value As DS54InputType)
                BitWise.SetDS54InputType(_bytaBytes(2), value)
            End Set
        End Property

        ''' <summary>Gets or sets the reported state of the input address.</summary>
        Public Property State() As OnOff
            Get
                Return BitWise.GetOnOffState(_bytaBytes(2))
            End Get
            Set(value As OnOff)
                BitWise.SetOnOffState(_bytaBytes(2), value)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSwitchInput.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Switch sensor event"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSwitchInput.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"Adr={Me.Address} Switch={Me.Switch} State={Me.State} DS54(Adr={Me.DS54Address} Input={Me.DS54InputPair} Type={Me.DS54InputType})"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet for complex sensor data exchange.</summary>
    ''' <remarks><i>PkMultiSense</i> packets can have several uses depending on the value assigned to the <see cref="PkMultiSense.Type"/> property:
    ''' <list type="bullet">
    ''' <item><description>Report transponding events.</description></item>
    ''' <item><description>Report power management events.</description></item>
    ''' <item><description>Program power management devices.</description></item>
    ''' <item><description>Program block detection devices.</description></item>
    ''' <item><description>Program security element devices.</description></item>
    ''' <item><description>Program stationary decoder devices.</description></item>
    ''' </list>
    ''' For programming types, a <see cref="PkLongAck"/> response is expected to confirm the operation.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkMultiSense
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(5) {&HD0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.MULTI_SENSE"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.MULTI_SENSE
            End Get
        End Property

        ''' <summary>Gets or sets the type of message encoded in this packet.</summary>
        Public Property Type() As MultiSenseType
            Get
                Select Case _bytaBytes(1) >> 5
                    Case 0  '[000xxxxx]
                        Return MultiSenseType.TransponderRelease
                    Case 1  '[001xxxxx]
                        Return MultiSenseType.TransponderDetect
                    Case 2  '[010xxxxx]
                        Return MultiSenseType.TransponderInputBits
                    Case 3  '[011xxxxx]
                        Select Case True
                            Case _bytaBytes(3) >> 4 = 3     '[0011xxxx]
                                Return MultiSenseType.PowerManagerStatus
                            Case _bytaBytes(3) = &H70       '[01110000]
                                Return MultiSenseType.PowerManagerOps
                            Case _bytaBytes(3) = &H71       '[01110001]
                                Return MultiSenseType.BlockDetectionOps
                            Case _bytaBytes(3) = &H72       '[01110010]
                                Return MultiSenseType.SecurityElementOps
                            Case _bytaBytes(3) = &H73       '[01110011]
                                Return MultiSenseType.StationaryDecoderOps
                            Case Else
                                Return MultiSenseType.OtherDetection
                        End Select
                End Select
            End Get
            Set(Value As MultiSenseType)
                Select Case Value
                    Case MultiSenseType.TransponderRelease, MultiSenseType.TransponderDetect, MultiSenseType.TransponderInputBits
                        CopyBits(Value, 0, 3, _bytaBytes(1), 5) '[XXXxxxxx]
                    Case MultiSenseType.PowerManagerStatus, MultiSenseType.PowerManagerOps, MultiSenseType.BlockDetectionOps, MultiSenseType.SecurityElementOps, MultiSenseType.StationaryDecoderOps
                        CopyBits(3, 0, 3, _bytaBytes(1), 5)     '[011xxxxx]
                        CopyBits(1, 0, 3, _bytaBytes(1), 1)     '[xxxx001x]
                        Select Case Value
                            Case MultiSenseType.PowerManagerStatus
                                CopyBits(3, 0, 4, _bytaBytes(3), 4) '[0011xxxx]
                                CopyBits(1, 0, 4, _bytaBytes(4), 4) '[0001xxxx]
                            Case MultiSenseType.PowerManagerOps
                                _bytaBytes(3) = &H70                '[01110000]
                            Case MultiSenseType.BlockDetectionOps
                                _bytaBytes(3) = &H71                '[01110001]
                            Case MultiSenseType.SecurityElementOps
                                _bytaBytes(3) = &H72                '[01110010]
                            Case MultiSenseType.StationaryDecoderOps
                                _bytaBytes(3) = &H73                '[01110011]
                        End Select
                End Select
            End Set
        End Property

        ''' <summary>Gets or sets the device independent address of the transponding receiver reporting the event.</summary>
        ''' <value>Valid address values are 0-4095.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderRelease"/> or <see cref="MultiSenseType.TransponderDetect"/>.</remarks>
        Public Property DeviceAddress() As UShort
            Get
                'gets A1-A12 from Byte(1)[0,T,T,A12,A11,A10,A9,A8]; Byte(2)[0,A7,A6,A5,A4,A3,A2,A1]
                Dim srtResult As UShort = 0
                CopyBits(_bytaBytes(1), 0, 5, srtResult, 7)
                CopyBits(_bytaBytes(2), 0, 7, srtResult, 0)
                Return srtResult
            End Get
            Set(value As UShort)
                If value > 4095 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid receiver address. Valid values are 0-4095.")

                CopyBits(value, 7, 5, _bytaBytes(1), 0)
                CopyBits(value, 0, 7, _bytaBytes(2), 0)
            End Set
        End Property

        ''' <summary>Gets or sets the BDL16x address of the transponding receiver reporting the event.</summary>
        ''' <value>Valid address values are 1-256.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderRelease"/> or <see cref="MultiSenseType.TransponderDetect"/>.</remarks>
        Public Property BDL16Address() As UShort
            Get
                'gets A1-A8 from DeviceAddress[ A8,A7,A6,A5 | A4,A3,A2,A1 | Z3,Z2,Z1,X ]
                Return (Me.DeviceAddress >> 4) + 1
            End Get
            Set(value As UShort)
                If value < 1 Or value > 256 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid BDL16 receiver address. Valid values are 1-256.")

                value -= 1
                Me.DeviceAddress = (Me.DeviceAddress And 15) Or ((value And 255) << 4)
            End Set
        End Property

        ''' <summary>Gets or sets the BDL16x zone of the transponding receiver reporting the event.</summary>
        ''' <value>Valid zone values are 1-8.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderRelease"/> or <see cref="MultiSenseType.TransponderDetect"/>.</remarks>
        Public Property BDL16Zone() As Byte
            Get
                'gets Z1-Z3 from DeviceAddress[ A8,A7,A6,A5 | A4,A3,A2,A1 | Z3,Z2,Z1,X ]
                Return ((Me.DeviceAddress >> 1) And 7) + 1
            End Get
            Set(value As Byte)
                If value < 1 Or value > 8 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid BDL16 receiver zone. Valid values are 1-8.")

                value -= 1
                Me.DeviceAddress = (Me.DeviceAddress And 4081) Or ((value And 7) << 1)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's orientation reported by the transponder.</summary>        
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderInputBits"/>.</remarks>
        Public Property LocoOrientation As LocoOrientation
            Get
                'Byte(2)[0,Orient,Inv,Dir,I4,I3,I2,I1]
                Return CopyBits(_bytaBytes(2), 6, 1, 0, 0)
            End Get
            Set(value As LocoOrientation)
                CopyBits(value, 0, 1, _bytaBytes(2), 6)
            End Set
        End Property

        ''' <summary>Gets or sets if current <see cref="LocoOrientation"/> is inverted, probably due to auto-reverse, reported by the transponder.</summary>        
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderInputBits"/>.</remarks>
        Public Property LocoOrientInverted As Boolean
            Get
                'Byte(2)[0,Orient,Inv,Dir,I4,I3,I2,I1]
                Return CopyBits(_bytaBytes(2), 5, 1, 0, 0)
            End Get
            Set(value As Boolean)
                CopyBits(If(value, 1, 0), 0, 1, _bytaBytes(2), 5)
            End Set
        End Property

        ''' <summary>Gets actual locomotive's orientation reported by the transponder, taking <see cref="LocoOrientInverted"/> into account.</summary>          
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderInputBits"/>.</remarks>
        Public ReadOnly Property ActualLocoOrient As LocoOrientation
            Get
                Return If(Me.LocoOrientInverted, If(Me.LocoOrientation = LocoOrientation.RailA, LocoOrientation.RailB, LocoOrientation.RailA), Me.LocoOrientation)
            End Get
        End Property

        ''' <summary>Gets or sets the locomotive's direction reported by the transponder.</summary>        
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderInputBits"/>.</remarks>
        Public Property LocoDirection As LocoDirection
            Get
                'Byte(2)[0,Orient,Inv,Dir,I4,I3,I2,I1]
                Return CopyBits(_bytaBytes(2), 4, 1, 0, 0)
            End Get
            Set(value As LocoDirection)
                CopyBits(value, 0, 1, _bytaBytes(2), 4)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's address reported by the transponder.</summary>
        ''' <value>Valid address values are 0-14847.</value>
        ''' <remarks>
        ''' Valid only if <see cref="Type"/> is <see cref="MultiSenseType.TransponderRelease"/>, <see cref="MultiSenseType.TransponderDetect"/>, or <see cref="MultiSenseType.TransponderInputBits"/>.
        ''' Reading supports 7 or 14 bit addressing. Writting always encodes as 14 bit.
        ''' </remarks>
        Public Property DccAddress() As UShort
            Get
                Select Case _bytaBytes(3)
                    Case 116 To 127  'special reserved values
                        If _bytaBytes(3) = 125 Then  'indicates _bytaBytes(4) is a 7 bit DCC address
                            Return _bytaBytes(4)
                        End If
                        Return 0  'other special values not defined
                    Case Else
                        Return (CType(_bytaBytes(3), UShort) << 7) Or _bytaBytes(4)
                End Select
            End Get
            Set(value As UShort)
                'note: we are not encoding 7 bit addresses and we are staying out of the reserved values zone
                If value > 14847 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DCC address. Valid values are 0-14847.")

                CopyBits(value, 7, 7, _bytaBytes(3), 0)
                CopyBits(value, 0, 7, _bytaBytes(4), 0)
            End Set
        End Property

        ''' <summary>Gets the power manager's auto reverse configuration of its four relays.</summary>
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.PowerManagerStatus"/>.</remarks>
        Public ReadOnly Property PmAsAutoRev() As YesNo()
            Get
                Return BitFlagsToArray(_bytaBytes(3), 4)
            End Get
        End Property

        ''' <summary>Gets the power manager's on/off status of its four relays.</summary>
        ''' <remarks>Valid only if <see cref="Type"/> is <see cref="MultiSenseType.PowerManagerStatus"/>.</remarks>
        Public ReadOnly Property PmRelayStatus() As OnOff()
            Get
                Return BitFlagsToArray(_bytaBytes(4), 4)
            End Get
        End Property

        ''' <summary>Gets or sets if packet is used for reading or writing data.</summary>
        ''' <value>Returns <i>True</i> if packet writes data; <i>False</i> if packet reads data.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is a non transponder type.</remarks>
        Public Property ProgWrite() As Boolean
            Get
                Return _bytaBytes(1) And 16
            End Get
            Set(Value As Boolean)
                If Value Then
                    _bytaBytes(1) = _bytaBytes(1) Or 16
                Else
                    _bytaBytes(1) = _bytaBytes(1) And 239
                End If
            End Set
        End Property

        ''' <summary>Gets or sets the address of the device being programmed.</summary>
        ''' <value>Valid values are 1-256.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is used for programming.</remarks>
        Public Property ProgAddress() As UShort
            Get
                Return (((_bytaBytes(1) And 1) << 7) Or _bytaBytes(2)) + 1
            End Get
            Set(Value As UShort)
                If Value < 1 Or Value > 256 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid programming address. Valid values are 1-256.")

                Value -= 1
                _bytaBytes(1) = (_bytaBytes(1) And 126) Or ((Value >> 7) And 1)
                _bytaBytes(2) = Value And 127
            End Set
        End Property

        ''' <summary>Gets or sets the option switch number to be affected.</summary>
        ''' <value>Valid values are 1-64, but they should be numbers supported by the device being read/written.</value>
        ''' <remarks>Valid only if <see cref="Type"/> is used for programming.</remarks>
        Public Property ProgOpSwitch() As Byte
            Get
                Return (_bytaBytes(4) >> 1) + 1
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 64 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid switch number. Valid values are 1-64.")

                _bytaBytes(4) = (_bytaBytes(4) And 1) Or (((Value - 1) And 63) << 1)
            End Set
        End Property

        ''' <summary>Gets or sets the state of the option switch.</summary>        
        ''' <remarks>Valid only if <see cref="Type"/> is used for programming.</remarks>
        Public Property ProgOpsState() As SwitchState
            Get
                Return _bytaBytes(4) And 1
            End Get
            Set(Value As SwitchState)
                _bytaBytes(4) = (_bytaBytes(4) And 126) Or Value
            End Set
        End Property

        Public Overrides ReadOnly Property NeedsPacketResponse() As Boolean
            Get
                Select Case Me.Type
                    Case MultiSenseType.PowerManagerOps, MultiSenseType.BlockDetectionOps, MultiSenseType.SecurityElementOps, MultiSenseType.StationaryDecoderOps
                        Return True
                    Case Else
                        Return False
                End Select
            End Get
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkMultiSense.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.Type
                    Case MultiSenseType.TransponderRelease
                        Return "Transponder release event"
                    Case MultiSenseType.TransponderDetect
                        Return "Transponder detect event"
                    Case MultiSenseType.TransponderInputBits
                        Return "Transponder input event"
                    Case MultiSenseType.PowerManagerStatus
                        Return "Power manager event"
                    Case MultiSenseType.PowerManagerOps
                        Return "Power manager OPS programming"
                    Case MultiSenseType.BlockDetectionOps
                        Return "Block detection OPS programming"
                    Case MultiSenseType.SecurityElementOps
                        Return "Security element OPS programming"
                    Case MultiSenseType.StationaryDecoderOps
                        Return "Stationary decoder OPS programming"
                    Case MultiSenseType.OtherDetection
                        Return "Undefined multi-sense message"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkMultiSense.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Select Case Me.Type
                    Case MultiSenseType.TransponderRelease, MultiSenseType.TransponderDetect
                        Return $"DevAdr={Me.DeviceAddress} BDL16Adr={Me.BDL16Address} BDL16Zone={Me.BDL16Zone} DccAdr={Me.DccAddress}"

                    Case MultiSenseType.TransponderInputBits
                        Return $"LocoOrient={Me.ActualLocoOrient} LocoDir={Me.LocoDirection} DccAdr={Me.DccAddress}"

                    Case MultiSenseType.PowerManagerStatus
                        Dim enuaAutoRev() As YesNo = Me.PmAsAutoRev
                        Dim enuaRelayStatus() As OnOff = Me.PmRelayStatus
                        Return $"Action={If(Me.ProgWrite, "Write", "Read")} Adr={Me.ProgAddress} " &
                               $"AutoRev(1-4)={{{enuaAutoRev(0)},{enuaAutoRev(1)},{enuaAutoRev(2)},{enuaAutoRev(3)}}} " &
                               $"RelayStat(1-4)={{{enuaRelayStatus(0)},{enuaRelayStatus(1)},{enuaRelayStatus(2)},{enuaRelayStatus(3)}}}"

                    Case MultiSenseType.PowerManagerOps, MultiSenseType.BlockDetectionOps, MultiSenseType.SecurityElementOps, MultiSenseType.StationaryDecoderOps
                        Return $"Action={If(Me.ProgWrite, "Write", "Read")} Adr={Me.ProgAddress} OPS={Me.ProgOpSwitch} State={Me.ProgOpsState}"

                End Select
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet used for security element data exchange.</summary>
    ''' <remarks>A packet response is not expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkSecurityElem
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(8) {&HE4, &H9, 0, 0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SE"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SE
            End Get
        End Property

        ''' <summary>Gets or sets the device address.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        Public Property Address() As UShort
            Get
                Return BitWise.GetAddress(_bytaBytes(2), _bytaBytes(3)) + 1
            End Get
            Set(value As UShort)
                BitWise.SetAddress(_bytaBytes(2), _bytaBytes(3), value - 1)
            End Set
        End Property

        ''' <summary>Gets or sets the command grouping.</summary>
        ''' <value>A 7 bit value.</value>
        Public Property Command() As Byte
            Get
                Return _bytaBytes(4)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(4), 0)
            End Set
        End Property

        ''' <summary>Gets or sets the direction codes.</summary>
        ''' <value>A 2 bit value.</value>
        Public Property Direction() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(5), 4, 2, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 2, _bytaBytes(5), 4)
            End Set
        End Property

        ''' <summary>Gets or sets the detection codes.</summary>
        ''' <value>A 2 bit value.</value>
        Public Property Detection() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(5), 2, 2, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 2, _bytaBytes(5), 2)
            End Set
        End Property

        ''' <summary>Gets or sets the switch states.</summary>
        ''' <value>A 2 bit value.</value>
        Public Property SwitchState() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(5), 0, 2, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 2, _bytaBytes(5), 0)
            End Set
        End Property

        ''' <summary>Gets or sets how <see cref="AxSpeed" /> is encoded.</summary>
        ''' <value>If <i>True</i>, velocity is in 8m/s increments, 16mph. (512mph max), otherwise if <i>False</i>, velocity is in 2m/s increments, 4mph. (128mph max).</value>
        Public Property Ax8mIncr() As Boolean
            Get
                Return BitWise.CopyBits(_bytaBytes(6), 6, 1, 0, 0)
            End Get
            Set(value As Boolean)
                BitWise.CopyBits(value, 0, 1, _bytaBytes(6), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the speed from security element leg A, to switch selected leg.</summary>
        ''' <value>See <see cref="Ax8mIncr" />.</value>
        Public Property AxSpeed() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(6), 0, 6, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 6, _bytaBytes(6), 0)
            End Set
        End Property

        ''' <summary>Gets or sets how <see cref="XaSpeed" /> is encoded.</summary>
        ''' <value>If <i>True</i>, velocity is in 8m/s increments, 16mph. (512mph max), otherwise if <i>False</i>, velocity is in 2m/s increments, 4mph. (128mph max).</value>
        Public Property Xa8mIncr() As Boolean
            Get
                Return BitWise.CopyBits(_bytaBytes(7), 6, 1, 0, 0)
            End Get
            Set(value As Boolean)
                BitWise.CopyBits(value, 0, 1, _bytaBytes(7), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the speed from security element switch selected leg, to leg A.</summary>
        ''' <value>See <see cref="Xa8mIncr" />.</value>
        Public Property XaSpeed() As Byte
            Get
                Return BitWise.CopyBits(_bytaBytes(7), 0, 6, 0, 0)
            End Get
            Set(value As Byte)
                BitWise.CopyBits(value, 0, 6, _bytaBytes(7), 0)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSecurityElem.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Security element data exchange"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkSecurityElem.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "Adr=" & Me.Address
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that is generated by a Lissy infrared sensor device.</summary>
    ''' <remarks>
    ''' Lissy is an automatic train detection system made by Uhlenbrock.
    ''' A packet response is not expected.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkLissy
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(7) {&HE4, &H8, 0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LISSY"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LISSY
            End Get
        End Property

        ''' <summary>Gets or sets the sensing device address.</summary>
        ''' <value>Valid address values are 0-4095.</value>
        Public Property DevAddr() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(4), 0, 7, 0, 0)
                Return BitWise.CopyBits(_bytaBytes(3), 0, 5, bits, 7)
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(4), 0)
                BitWise.CopyBits(value, 7, 5, _bytaBytes(3), 0)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC locomotive address.</summary>
        ''' <value>Valid address values are 0-16383.</value>
        Public Property DccAddr() As UShort
            Get
                Dim bits = BitWise.CopyBits(_bytaBytes(6), 0, 7, 0, 0)
                Return BitWise.CopyBits(_bytaBytes(5), 0, 7, bits, 7)
            End Get
            Set(value As UShort)
                BitWise.CopyBits(value, 0, 7, _bytaBytes(6), 0)
                BitWise.CopyBits(value, 7, 7, _bytaBytes(5), 0)
            End Set
        End Property

        ''' <summary>Gets or sets the locomotive's direction of travel.</summary>
        Public Property Direction() As DirectionNS
            Get
                Return BitWise.CopyBits(_bytaBytes(3), 5, 1, 0, 0)
            End Get
            Set(value As DirectionNS)
                BitWise.CopyBits(value, 0, 1, _bytaBytes(3), 5)
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary> 
        ''' <seealso cref="PkLissy.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Lissy sensor event"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkLissy.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return $"DevAdr={Me.DevAddr} DccAdr={Me.DccAddr} Dir={Me.Direction}"
            End Get
        End Property

    End Class

    '⟱⟱⟱ byte transfers ⟱⟱⟱

    ''' <summary>Represents a Loconet packet that moves 8 bytes between devices.</summary>
    ''' <remarks>A packet response is not expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkPeerXfer
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(15) {&HE5, &H10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.PEER_XFER"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.PEER_XFER
            End Get
        End Property

        ''' <summary>Gets or sets the source identifier of the byte move.</summary>
        ''' <value>Valid values are 0 to 127. 0 is the master.</value>
        Public Property SourceID() As Byte
            Get
                Return _bytaBytes(2)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid source identifier. Valid values are 0-127.")

                _bytaBytes(2) = Value And 127
            End Set
        End Property

        ''' <summary>Gets or sets the target's low address of the byte move.</summary>
        ''' <value>The 7 least significant bits of the 14 bit target address.</value>
        ''' <seealso cref="PkPeerXfer.TargetHighAdr"/> 
        Public Property TargetLowAdr() As Byte
            Get
                Return _bytaBytes(3)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid target low address. Valid values are 0-127.")

                _bytaBytes(3) = Value And 127
            End Set
        End Property

        ''' <summary>Gets or sets the target's high address of the byte move.</summary>
        ''' <value>The 7 most significant bits of the 14 bit target address.</value>
        ''' <seealso cref="PkPeerXfer.TargetLowAdr"/> 
        Public Property TargetHighAdr() As Byte
            Get
                Return _bytaBytes(4)
            End Get
            Set(Value As Byte)
                If Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid target high address. Valid values are 0-127.")

                _bytaBytes(4) = Value And 127
            End Set
        End Property

        ''' <summary>Gets or sets the 8 bytes to be moved.</summary>
        ''' <param name="bytIdx">Valid values are 1-8, indicating the byte index number.</param>
        ''' <value>The byte specified by <paramref name="bytIdx"/>.</value>
        Public Property Data(bytIdx As Byte) As Byte
            Get
                If bytIdx < 1 Or bytIdx > 8 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid data byte index. Valid values are 1-8.")

                Dim bytHigh1Bits As Byte
                Dim bytLow7Bits As Byte
                Dim bytBitMask As Byte
                Select Case bytIdx
                    Case 1, 2, 3, 4
                        bytHigh1Bits = _bytaBytes(5)
                        bytLow7Bits = _bytaBytes(5 + bytIdx)
                        bytBitMask = 1 << (bytIdx - 1)
                    Case 5, 6, 7, 8
                        bytHigh1Bits = _bytaBytes(10)
                        bytLow7Bits = _bytaBytes(6 + bytIdx)
                        bytBitMask = 1 << (bytIdx - 5)
                End Select
                Return bytLow7Bits Or If(bytHigh1Bits And bytBitMask, 128, 0)
            End Get
            Set(Value As Byte)
                If bytIdx < 1 Or bytIdx > 8 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid data byte index. Valid values are 1-8.")

                Dim bytHigh1BitsIdx As Byte
                Dim bytLow7BitsIdx As Byte
                Dim bytBitMask As Byte
                Select Case bytIdx
                    Case 1, 2, 3, 4
                        bytHigh1BitsIdx = 5
                        bytLow7BitsIdx = 5 + bytIdx
                        bytBitMask = 1 << (bytIdx - 1)
                    Case 5, 6, 7, 8
                        bytHigh1BitsIdx = 10
                        bytLow7BitsIdx = 6 + bytIdx
                        bytBitMask = 1 << (bytIdx - 5)
                End Select
                If Value >= 128 Then
                    _bytaBytes(bytLow7BitsIdx) = Value And 127
                    _bytaBytes(bytHigh1BitsIdx) = _bytaBytes(bytHigh1BitsIdx) Or bytBitMask
                Else
                    _bytaBytes(bytLow7BitsIdx) = Value
                    _bytaBytes(bytHigh1BitsIdx) = _bytaBytes(bytHigh1BitsIdx) And Not bytBitMask
                End If
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkPeerXfer.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Peer to peer byte transfer"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks> 
        ''' <seealso cref="PkPeerXfer.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Dim objSb As New Text.StringBuilder
                objSb.Append(String.Format("Src={0} DstH={1} DstL={2} Data(1-8)=Hex{{", Me.SourceID, Me.TargetHighAdr, Me.TargetLowAdr))
                For bytIdx As Byte = 1 To 8
                    If bytIdx > 1 Then objSb.Append(",")
                    Dim strValue As String = Hex(Me.Data(bytIdx))
                    If strValue.Length = 1 Then objSb.Append("0")
                    objSb.Append(strValue)
                Next
                objSb.Append("}")
                Return objSb.ToString
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that comunicates with a LocoIO device.</summary>
    ''' <remarks>
    '''     A <see cref="PkLocoIO"/> response is expected. 
    '''     The static (<i>Shared</i> in Visual Basic) members in this class are helpers which simplify LocoIO operations.
    ''' </remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkLocoIO
        Inherits PkPeerXfer

        ''' <summary>The actions a LocoIO packet can perform.</summary>
        Public Enum CommandType As Byte
            CvWrite = 1
            CvRead = 2
            MultiPortWrite = 3
            MultiPortRead = 4
        End Enum

        Public Sub New()
            MyBase.New()
            Me.SourceID = 80
            Me.TargetLowAdr = 81 'dafault LocoIO device address
            Me.TargetHighAdr = 1 'Digitrax has assigned 1 to this value for LocoIO devices            
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Target address of the LocoIO device this packet is being sent to.</summary>
        ''' <value>Valid values are 0-79 and 81-127 with a default of 81. 0 is used for broadcasts. 80 is reserved for the PC.</value>
        ''' <remarks>
        ''' Must be used in conjunction with <see cref="SubAddress"/> to identify a unique LocoIO device except when performing broadcasts.
        ''' This property is required for all <see cref="Command"/> types.
        ''' </remarks>
        Public Property Address() As Byte
            Get
                Return Me.TargetLowAdr
            End Get
            Set(Value As Byte)
                If Value = 80 Or Value > 127 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid address. Valid values are 0-79 and 81-127. 0 is used for broadcasts. 80 is reserved for the PC.")

                Me.TargetLowAdr = Value
            End Set
        End Property

        ''' <summary>Target sub-address of the LocoIO device this packet is being sent to.</summary>
        ''' <value>Valid values are 1-126 with a default of 1.</value>
        ''' <remarks>
        ''' Must be used in conjunction with <see cref="Address"/> to identify a unique LocoIO device.
        ''' This property is required only for commands <see cref="CommandType.CvRead"/> and <see cref="CommandType.CvWrite"/>.
        ''' </remarks>
        Public Property SubAddress() As Byte
            Get
                Return Me.Data(5)
            End Get
            Set(Value As Byte)
                If Value < 1 Or Value > 126 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid sub-address. Valid values are 1-126.")

                Me.Data(5) = Value
            End Set
        End Property

        ''' <summary>The action this LocoIO packet will perform.</summary>
        Public Property Command() As CommandType
            Get
                Return Me.Data(1)
            End Get
            Set(Value As CommandType)
                Me.Data(1) = Value
            End Set
        End Property

        ''' <summary>The memory register to be read or written to in a LocoIO device.</summary>
        ''' <value>Valid values are 0-98 and 101-124.</value>
        ''' <remarks>
        ''' The LocoIO documentation calls these SV locations.
        ''' This property is required only for commands <see cref="CommandType.CvRead"/> and <see cref="CommandType.CvWrite"/>.
        ''' </remarks>
        Public Property Register() As Byte
            Get
                Return Me.Data(2)
            End Get
            Set(Value As Byte)
                If (Value > 98 And Value < 101) Or Value > 124 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid register number. Valid values are 0-98 and 101-124.")

                Me.Data(2) = Value
            End Set
        End Property

        ''' <summary>Value to be written to a LocoIO's memory register.</summary>
        ''' <value>Any valid value specified in the LocoIO documentation.</value>    
        ''' <remarks>
        ''' This value will be written to memory register, specified by the <see cref="Register"/> property, during a write operation.
        ''' This property is required only for command <see cref="CommandType.CvWrite"/>.
        ''' </remarks>    
        Public Property WriteData() As Byte
            Get
                Return Me.Data(4)
            End Get
            Set(Value As Byte)
                Me.Data(4) = Value
            End Set
        End Property

        ''' <summary>Values returned from the LocoIO's memory registers.</summary>
        ''' <value>Any valid value specified in the LocoIO documentation.</value> 
        ''' <remarks>
        ''' Three values are returned from adjacent memory registers, after a read request operation.<br/>
        ''' ReadData(1) = value found at the <see cref="Register"/> location.<br/>
        ''' ReadData(2) = value found at the <see cref="Register"/> + 1 location.<br/>
        ''' ReadData(3) = value found at the <see cref="Register"/> + 2 location.<br/>
        ''' This property should only be read after sending a <see cref="PkLocoIO"/> packet with a <see cref="CommandType.CvRead"/> command.
        ''' </remarks>
        Public Property ReadData(bytPosition As Byte) As Byte
            Get
                If bytPosition < 1 Or bytPosition > 3 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid position index. Valid values are 1-3.")

                Return Me.Data(5 + bytPosition)
            End Get
            Set(Value As Byte)
                If bytPosition < 1 Or bytPosition > 3 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid position index. Valid values are 1-3.")

                Me.Data(5 + bytPosition) = Value
            End Set
        End Property

        ''' <summary>Ports to be affected by a multiport write.</summary>
        ''' <value>A zero based boolean array of 16 values.</value>
        ''' <remarks>
        ''' Each array member represents a LocoIO port.<br/>
        ''' <i>True</i> = port will be affected.<br/>
        ''' <i>False</i> = port will be ignored.<br/>
        ''' This property is required only for command <see cref="CommandType.MultiPortWrite"/>.
        ''' </remarks>
        Public Property MultiMask() As Boolean()
            Get
                Dim blna(15) As Boolean

                Dim sctBitVec As New BitVector32(Me.Data(5))
                For bytIdx As Byte = 1 To 8
                    blna(bytIdx - 1) = sctBitVec.Item(1 << (8 - bytIdx))
                Next

                sctBitVec = New BitVector32(Me.Data(7))
                For bytIdx As Byte = 9 To 16
                    blna(bytIdx - 1) = sctBitVec.Item(1 << (16 - bytIdx))
                Next

                Return blna
            End Get
            Set(Value As Boolean())
                Dim sctBitVec As New BitVector32(Me.Data(5))
                For bytIdx As Byte = 1 To 8
                    sctBitVec.Item(1 << (8 - bytIdx)) = Value(bytIdx - 1)
                Next
                Me.Data(5) = CType(sctBitVec.Data, Byte)

                sctBitVec = New BitVector32(Me.Data(7))
                For bytIdx As Byte = 9 To 16
                    sctBitVec.Item(1 << (16 - bytIdx)) = Value(bytIdx - 1)
                Next
                Me.Data(7) = CType(sctBitVec.Data, Byte)
            End Set
        End Property

        ''' <summary>Port state values for a multiport write.</summary>
        ''' <value>A zero based boolean array of 16 values.</value>
        ''' <remarks>
        ''' Each array member represents a LocoIO port.<br/>
        ''' <i>True</i> = port will be activated.<br/>
        ''' <i>False</i> = port will be deactivated.<br/>
        ''' Unless the corresponding port in <see cref="MultiMask"/> is <i>True</i> this value will no be applied.
        ''' This property is required only for command <see cref="CommandType.MultiPortWrite"/>.
        ''' </remarks>
        Public Property MultiState() As Boolean()
            Get
                Dim blna(15) As Boolean

                Dim sctBitVec As New BitVector32(Me.Data(6))
                For bytIdx As Byte = 1 To 8
                    blna(bytIdx - 1) = sctBitVec.Item(1 << (8 - bytIdx))
                Next

                sctBitVec = New BitVector32(Me.Data(8))
                For bytIdx As Byte = 9 To 16
                    blna(bytIdx - 1) = sctBitVec.Item(1 << (16 - bytIdx))
                Next

                Return blna
            End Get
            Set(Value As Boolean())
                Dim sctBitVec As New BitVector32(Me.Data(6))
                For bytIdx As Byte = 1 To 8
                    sctBitVec.Item(1 << (8 - bytIdx)) = Value(bytIdx - 1)
                Next
                Me.Data(6) = CType(sctBitVec.Data, Byte)

                sctBitVec = New BitVector32(Me.Data(8))
                For bytIdx As Byte = 9 To 16
                    sctBitVec.Item(1 << (16 - bytIdx)) = Value(bytIdx - 1)
                Next
                Me.Data(8) = CType(sctBitVec.Data, Byte)
            End Set
        End Property

        Public Overrides ReadOnly Property NeedsPacketResponse() As Boolean
            Get
                'overridden because normally OPS_PEER_XFER does not expect a response but LocoIO does respond
                Return True
            End Get
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            If TypeOf objResponsePacket Is PkLocoIO Then
                Dim objPkLocoIO As PkLocoIO = CType(objResponsePacket, PkLocoIO)
                Return objPkLocoIO.Address = 80
            Else
                Return False
            End If
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkLocoIO.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "LocoIO control"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkLocoIO.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Dim objSb As New Text.StringBuilder
                objSb.Append(String.Format("Src={0} Dst={1} Data(1-8)=Hex{{", Me.SourceID, Me.Address))
                For bytIdx As Byte = 1 To 8
                    If bytIdx > 1 Then objSb.Append(",")
                    Dim strValue As String = Hex(Me.Data(bytIdx))
                    If strValue.Length = 1 Then objSb.Append("0")
                    objSb.Append(strValue)
                Next
                objSb.Append("}")
                Return objSb.ToString
            End Get
        End Property

        '-- helpers --------------------------------

        ''' <summary>Specifies how a LocoIO port is configured.</summary>
        ''' <remarks>These types are supported by the LocoIO as of v1.47 firmware.</remarks>
        Public Enum ConfigType As UShort
            ''' <summary>An empty and invalid configuration. All ports start out with this configuration after the LocoIO firmware is written.</summary>
            NotDefined = 0

            'Configuration byte << 4 combined with most significant 4 bits of Value2 (the second value of the packet containeed in the 8 byte payload)

            'inputs ---------------

            ''' <summary>Generates <see cref="PkInput"/> packets. Input is active low.</summary>
            InInpRepActLow = (31 << 4) Or 1         '0001
            ''' <summary>Generates <see cref="PkInput"/> packets. Input is active high.</summary>
            InInpRepActHigh = (95 << 4)             '0000 
            ''' <summary>Generates <see cref="PkSetSwitch"/> packets. Toggle switch input.</summary>
            InSwReqToggle = (15 << 4) Or 1          '0001   'this existed in 1.35
            ''' <summary>Generates <see cref="PkSwitchInput"/> packets. Toggle switch input.</summary>
            InSwRepToggle = (7 << 4) Or 1           '0001
            ''' <summary>Generates <see cref="PkSetSwitch"/> packets. Push button input is active low.</summary>
            InSwReqPushBtnActLow = (47 << 4) Or 1   '0001   'this existed in 1.35
            ''' <summary>Generates <see cref="PkSetSwitch"/> packets. Push button input is active high.</summary>
            InSwReqPushBtnActHigh = (111 << 4)      '0000   'this existed in 1.35
            ''' <summary>Generates <see cref="PkSwitchInput"/> packets. Push button input is active low.</summary>
            InSwRepPushBtnActLow = (39 << 4) Or 1   '0001
            ''' <summary>Generates <see cref="PkSwitchInput"/> packets. Push button input is active high.</summary>
            InSwRepPushBtnActHigh = (103 << 4)      '0000
            ''' <summary>Generates <see cref="PkSwitchInput"/> packets. Feedback with one sensor.</summary>
            InSwRepFeedbackOne = (23 << 4) Or 7     '0111
            ''' <summary>Generates <see cref="PkSwitchInput"/> packets. Feedback with two sensors.</summary>
            InSwRepFeedbackTwo = (55 << 4) Or 6     '0110(Off) 0111(On) 

            'outputs --------------

            ''' <summary>Acts on <see cref="PkInput"/> packets.</summary>
            OutInpRepNorm = (192 << 4)              '0000               'this existed in 1.35
            ''' <summary>Acts on <see cref="PkInput"/> packets. Output will blink at the rate specified by <see cref="Setup.BlinkingRate"/>.</summary>
            OutInpRepBlink = (208 << 4)             '0000
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Port is off at power up.</summary>
            OutSwReqOffNorm = (128 << 4) Or 1       '0001(T) 0011(C)    'this existed in 1.35
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Port is off at power up. Output will blink at the rate specified by <see cref="Setup.BlinkingRate"/>.</summary>
            OutSwReqOffBlink = (144 << 4) Or 1      '0001(T) 0011(C)
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Port is on at power up.</summary>
            OutSwReqOnNorm = (129 << 4) Or 1        '0001(T) 0011(C)
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Port is on at power up. Output will blink at the rate specified by <see cref="Setup.BlinkingRate"/>.</summary>
            OutSwReqOnBlink = (145 << 4) Or 1       '0001(T) 0011(C)
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Pulse with a manual reset.</summary>
            OutSwReqPulseManual = (136 << 4)        '0000(T) 0010(C)
            ''' <summary>Acts on <see cref="PkSetSwitch"/> packets. Pulse with an auto reset if manual reset not received.</summary>
            OutSwReqPulseAuto = (140 << 4)          '0000(T) 0010(C)

            'unnecessary configurations
            'these are for applications(i.e. RR&Co.) that need sequencial ports for 4 state accessories
            'OutSwReqOnNorm4Way = 161       0001(T) 0011(C)
            'OutSwReqOffNorm4Way = 160      0001(T) 0011(C)
            'OutSwReqOffBlink4Way = 176     0001(T) 0011(C)
            'OutSwReqOnBlink4Way = 177      0001(T) 0011(C)

        End Enum

        ''' <summary>Represents the setup parameters of a LocoIO device.</summary>
        ''' <remarks>This class encapsulates the data requred to read or write the setup of a LocoIO device.</remarks>
        ''' <seealso cref="WriteSetup"/>
        ''' <seealso cref="Read"/>
        Public NotInheritable Class Setup
            Private _bytVersion As Byte
            Private _blnPortRefresh As Boolean
            Private _blnAlternatePushButtons As Boolean
            Private _blnServoMotorOutputs As Boolean
            Private _bytBlinkingRate As Byte

            Public Property Version() As Byte
                Get
                    Return _bytVersion
                End Get
                Friend Set(value As Byte)
                    _bytVersion = value
                End Set
            End Property

            Public Property PortRefresh() As Boolean
                Get
                    Return _blnPortRefresh
                End Get
                Set(Value As Boolean)
                    _blnPortRefresh = Value
                End Set
            End Property

            Public Property AlternatePushButtons() As Boolean
                Get
                    Return _blnAlternatePushButtons
                End Get
                Set(Value As Boolean)
                    _blnAlternatePushButtons = Value
                End Set
            End Property

            Public Property ServoMotorOutputs() As Boolean
                Get
                    Return _blnServoMotorOutputs
                End Get
                Set(Value As Boolean)
                    _blnServoMotorOutputs = Value
                End Set
            End Property

            Public Property BlinkingRate() As Byte
                Get
                    Return _bytBlinkingRate
                End Get
                Set(value As Byte)
                    _bytBlinkingRate = value And 15
                End Set
            End Property
        End Class

        ''' <summary>Represents 1 of 16 LocoIO port configurations.</summary>
        ''' <remarks>This class encapsulates the data required to read or write the configuration of a LocoIO port.</remarks>
        ''' <seealso cref="WriteConfig"/>
        ''' <seealso cref="Read"/>
        Public NotInheritable Class PortConfig
            Private _bytPort As Byte
            Private _enuConfig As ConfigType
            Private _objPacket As Packet

            Public Sub New()
                Me.Port = 1
                Me.Config = ConfigType.NotDefined
            End Sub

            ''' <summary>LocoIO port number to be configured.</summary>
            ''' <value>Valid values are 1-16.</value>
            Public Property Port() As Byte
                Get
                    Return _bytPort
                End Get
                Set(Value As Byte)
                    _bytPort = Value
                End Set
            End Property

            ''' <summary>The configuration that determines how the LocoIO port should behave.</summary>
            ''' <remarks>
            ''' Setting this value populates the <see cref="Packet"/> property with an appropriate packet type for the given configuration. 
            ''' The following packet types are created depending on a string contained in the <see cref="ConfigType"/>.
            ''' <li>Containing "InpRep" will generate a <see cref="PkInput"/>.</li>
            ''' <li>Containing "SwRep" will generate a <see cref="PkSwitchInput"/>.</li>
            ''' <li>Containing "SwReq" will generate a <see cref="PkSetSwitch"/>.</li>
            ''' </remarks>
            Public Property Config() As ConfigType
                Get
                    Return _enuConfig
                End Get
                Set(Value As ConfigType)
                    _enuConfig = Value
                    Select Case True
                        Case _enuConfig.ToString.Contains("InpRep")
                            _objPacket = New PkInput()
                        Case _enuConfig.ToString.Contains("SwRep")
                            _objPacket = New PkSwitchInput()
                        Case _enuConfig.ToString.Contains("SwReq")
                            _objPacket = New PkSetSwitch()
                        Case Else
                            _objPacket = Nothing
                    End Select
                    If _objPacket IsNot Nothing Then _objPacket.Bytes(2) = (_enuConfig And 15) << 4
                End Set
            End Property

            ''' <summary>Packet to be acted on or generated by the LocoIO.</summary>
            ''' <remarks>
            ''' Assigning a <see cref="ConfigType"/> to the <see cref="Config"/> property, will create a packet instance which can be accessed and altered through this member.
            ''' See the <see cref="Config"/> member about the possible packet types that can be returned by this property.
            ''' </remarks>
            Public ReadOnly Property Packet() As Packet
                Get
                    Return _objPacket
                End Get
            End Property
        End Class


        ''' <summary>Sends a broadcast messages to all LocoIO devices seen on the network.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param>
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="bytLocoIOSubAdr">LocoIO sub-address to be assigned. Valid values are 1-126 with a default of 1.</param>
        ''' <remarks>
        '''     This is used to initially assign an address and sub-address to a LocoIO device. For all other communications, it's this address and sub-address combination that will uniquely identify the LocoIO device.
        '''     Make sure only one LocoIO device is connected to the network when performing this action.
        '''     The LocoIO device being written will respond with a <see cref="PkPeerXfer"/> packet as a write confirmation.
        '''</remarks>
        Public Shared Sub BroadcastAddress(objLoconetService As LoconetService, bytLocoIOAdr As Byte, bytLocoIOSubAdr As Byte)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = 0
            objPkLocoIO.Command = PkLocoIO.CommandType.CvWrite

            objPkLocoIO.Register = 1             'write to SV1
            objPkLocoIO.WriteData = bytLocoIOAdr
            objLoconetService.TxPacket(objPkLocoIO.Clone())

            objPkLocoIO.Register = 2             'write to SV2
            objPkLocoIO.WriteData = bytLocoIOSubAdr
            objLoconetService.TxPacket(objPkLocoIO)
        End Sub


        ''' <summary>Sends a message to a LocoIO device requesting setup data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param>
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="bytLocoIOSubAdr">LocoIO sub-address to be assigned. Valid values are 1-126 with a default of 1.</param>
        ''' <returns>The response packet.</returns>
        ''' <remarks>
        '''     The interogated LocoIO device will respond with a <see cref="PkPeerXfer"/> packet which can be cast into a
        '''     <see cref="PkLocoIO"/> from which the <see cref="PkLocoIO.Read"/> method can be used to retieve a <see cref="PkLocoIO.Setup"/> instance.
        ''' </remarks>
        ''' <seealso cref="Setup"/>
        ''' <seealso cref="Read"/>
        Public Shared Async Function RequestSetup(objLoconetService As LoconetService, bytLocoIOAdr As Byte, bytLocoIOSubAdr As Byte) As Task(Of PkLocoIO)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.SubAddress = bytLocoIOSubAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.CvRead
            objPkLocoIO.Register = 0    'read SV0
            Await objLoconetService.TxPacket(objPkLocoIO)
            Return objPkLocoIO.RxPacket
        End Function

        ''' <summary>Sends a message to a LocoIO device writting setup data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param> 
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="bytLocoIOSubAdr">LocoIO sub-address to be assigned. Valid values are 1-126 with a default of 1.</param>
        ''' <param name="objSetup">A <see cref="PkLocoIO.Setup"/> instance containing setup data.</param>
        ''' <remarks>The LocoIO device being written will respond with a <see cref="PkPeerXfer"/> packet as a write confirmation.</remarks>
        ''' <seealso cref="Setup"/>
        Public Shared Sub WriteSetup(objLoconetService As LoconetService, bytLocoIOAdr As Byte, bytLocoIOSubAdr As Byte, objSetup As Setup)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.SubAddress = bytLocoIOSubAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.CvWrite
            objPkLocoIO.Register = 0     'read SV0

            'value that gets stuffed in SV0
            Dim sctBitVec As New BitVector32(objSetup.BlinkingRate << 4)
            sctBitVec.Item(1) = objSetup.PortRefresh
            sctBitVec.Item(2) = objSetup.AlternatePushButtons
            sctBitVec.Item(8) = objSetup.ServoMotorOutputs
            objPkLocoIO.WriteData = CType(sctBitVec.Data, Byte)

            objLoconetService.TxPacket(objPkLocoIO)
        End Sub


        ''' <summary>Sends a message to a LocoIO device requesting configuration data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param> 
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="bytLocoIOSubAdr">LocoIO sub-address to be assigned. Valid values are 1-126 with a default of 1.</param>
        ''' <param name="bytPort">LocoIO port number (1-16) being interogated.</param>
        ''' <returns>The response packet.</returns>
        ''' <remarks>
        '''     The interogated LocoIO device will respond with a <see cref="PkPeerXfer"/> packet which can be cast into a
        '''     <see cref="PkLocoIO"/> from which the <see cref="PkLocoIO.Read"/> method can be used to retieve a <see cref="PkLocoIO.PortConfig"/> instance.
        ''' </remarks>
        ''' <seealso cref="PortConfig"/>
        ''' <seealso cref="Read"/>
        Public Shared Async Function RequestConfig(objLoconetService As LoconetService, bytLocoIOAdr As Byte, bytLocoIOSubAdr As Byte, bytPort As Byte) As Task(Of PkLocoIO)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.SubAddress = bytLocoIOSubAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.CvRead
            objPkLocoIO.Register = bytPort * 3   'set LocoIO port to read
            Await objLoconetService.TxPacket(objPkLocoIO)
            Return objPkLocoIO.RxPacket
        End Function

        ''' <summary>Sends three messages to a LocoIO device writting configuration data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param> 
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="bytLocoIOSubAdr">LocoIO sub-address to be assigned. Valid values are 1-126 with a default of 1.</param>
        ''' <param name="objPortConfig">A <see cref="PkLocoIO.PortConfig"/> instance containing configuration data.</param>
        ''' <remarks>The LocoIO device being written will respond with a <see cref="PkPeerXfer"/> packet as a write confirmation.</remarks>
        ''' <seealso cref="PortConfig"/>
        Public Shared Sub WriteConfig(objLoconetService As LoconetService, bytLocoIOAdr As Byte, bytLocoIOSubAdr As Byte, objPortConfig As PortConfig)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.SubAddress = bytLocoIOSubAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.CvWrite
            objPkLocoIO.Register = objPortConfig.Port * 3
            objPkLocoIO.WriteData = objPortConfig.Config >> 4
            objLoconetService.TxPacket(objPkLocoIO.Clone())

            'write value1
            objPkLocoIO.Register = objPortConfig.Port * 3 + 1
            If objPortConfig.Packet Is Nothing Then
                objPkLocoIO.WriteData = 0
            Else
                objPkLocoIO.WriteData = objPortConfig.Packet.Bytes(1)
            End If
            objLoconetService.TxPacket(objPkLocoIO.Clone())

            'write value2
            objPkLocoIO.Register = objPortConfig.Port * 3 + 2
            If objPortConfig.Packet Is Nothing Then
                objPkLocoIO.WriteData = 0
            Else
                objPkLocoIO.WriteData = objPortConfig.Packet.Bytes(2)
            End If
            objLoconetService.TxPacket(objPkLocoIO)
        End Sub


        ''' <summary>Sends a message to a LocoIO device requesting states data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param> 
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>      
        ''' <returns>The response packet.</returns>
        ''' <remarks>
        '''     The interogated LocoIO device will respond with a <see cref="PkPeerXfer"/> packet which can be cast into a
        '''     <see cref="PkLocoIO"/> from which the <see cref="PkLocoIO.Read"/> method can be used to retieve an <see cref="Boolean"/> array of states.
        ''' </remarks>
        ''' <seealso cref="Read"/>
        Public Shared Async Function RequestStates(objLoconetService As LoconetService, bytLocoIOAdr As Byte) As Task(Of PkLocoIO)
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.MultiPortRead
            Await objLoconetService.TxPacket(objPkLocoIO)
            Return objPkLocoIO.RxPacket
        End Function

        ''' <summary>Sends a messages to a LocoIO device writting state data.</summary>
        ''' <param name="objLoconetService">A <see cref="LoconetService"/> instance that will service the command.</param> 
        ''' <param name="bytLocoIOAdr">LocoIO address to be assigned. Valid values are 1-79 and 81-127 with a default of 81. 80 is reserved for the PC.</param>
        ''' <param name="blnaMultiMask">A <see cref="Boolean"/> 16 element array containing a mask indicating which of the 16 states should be written.</param>
        ''' <param name="blnaMultiState">A <see cref="Boolean"/> 16 element array containing the state values to be written. Values masked out will be ignored.</param>
        ''' <remarks>The LocoIO device being written will respond with a <see cref="PkPeerXfer"/> packet as a write confirmation.</remarks>
        Public Shared Sub WriteStates(objLoconetService As LoconetService, bytLocoIOAdr As Byte, blnaMultiMask As Boolean(), blnaMultiState As Boolean())
            Dim objPkLocoIO As New PkLocoIO
            objPkLocoIO.Address = bytLocoIOAdr
            objPkLocoIO.Command = PkLocoIO.CommandType.MultiPortWrite
            objPkLocoIO.MultiMask = blnaMultiMask
            objPkLocoIO.MultiState = blnaMultiState
            objLoconetService.TxPacket(objPkLocoIO)
        End Sub


        ''' <summary>Reads requested data, returned from the LocoIO.</summary>
        ''' <returns>A <see cref="Setup"/> or <see cref="PortConfig"/> class instance, or a zero based boolean array of 16 values.</returns>
        ''' <remarks>
        ''' When <see cref="RequestSetup"/> is called, a Read() on the returned packet returns a <see cref="Setup"/> instance.
        ''' When <see cref="RequestConfig"/> is called, a Read() on the returned packet returns a <see cref="PortConfig"/> instance.
        ''' When <see cref="RequestStates"/> is called, a Read() on the returned packet returns a boolean array.
        ''' </remarks>
        ''' <seealso cref="RequestSetup"/>
        ''' <seealso cref="RequestConfig"/>
        ''' <seealso cref="RequestStates"/>
        ''' <seealso cref="Setup"/>
        ''' <seealso cref="PortConfig"/>
        Public Function Read() As Object
            Select Case Me.Command
                Case PkLocoIO.CommandType.CvRead
                    If Me.Register = 0 Then   'if it's a setup packet CV0
                        Dim objSetup As New Setup
                        objSetup.Version = Me.Data(3) 'this could be retrieved on any LocoIO packet read but I put it here only because it seems to fit 
                        Dim sctBitVec As New BitVector32(Me.ReadData(1))
                        objSetup.PortRefresh = sctBitVec.Item(1)
                        objSetup.AlternatePushButtons = sctBitVec.Item(2)
                        objSetup.ServoMotorOutputs = sctBitVec.Item(8)
                        objSetup.BlinkingRate = Me.ReadData(1) >> 4
                        Return objSetup
                    Else
                        Dim objPortConfig As New PortConfig
                        objPortConfig.Port = Math.Floor(Me.Register / 3)

                        Dim srtReadData1 As UShort = Me.ReadData(1)
                        For Each srtValue As UShort In [Enum].GetValues(GetType(ConfigType))
                            If srtValue >> 4 = srtReadData1 Then
                                'has to be set before setting Packet.Bytes() so the corrent packet type is created
                                objPortConfig.Config = srtValue
                                Exit For
                            End If
                        Next

                        If objPortConfig.Packet IsNot Nothing Then
                            objPortConfig.Packet.Bytes(1) = Me.ReadData(2)
                            objPortConfig.Packet.Bytes(2) = Me.ReadData(3)
                        End If

                        Return objPortConfig
                    End If

                Case PkLocoIO.CommandType.MultiPortRead
                    Return Me.MultiState
            End Select
        End Function

    End Class

    '⟱⟱⟱ other ⟱⟱⟱

    ''' <summary>Represents an acknowledgement Loconet packet response.</summary>
    ''' <remarks>This packet is used as a response for several packet types and should not be transmitted from the PC.</remarks>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>     
    <Serializable()> Public Class PkLongAck
        Inherits Packet

        Friend Sub New()
            _bytaBytes = New Byte(3) {}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.LONG_ACK"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.LONG_ACK
            End Get
        End Property

        ''' <summary>Gets the acknowledgement type.</summary>
        ''' <remarks>The type describes the kind of acknowledgement message we received.</remarks>
        Public ReadOnly Property Type() As LongAckType
            Get
                '_bytaBytes(1) will always be the the OpCode of the packet the Long Ack is replying to with bit 7 zeroed
                'for example if _bytaBytes(1) is &H3B, it is a reply to a &HBB OpCode packet

                '_bytaBytes(2) will be the result code

                Select Case _bytaBytes(1)
                    Case &H30
                        Return LongAckType.SetSwitchFailed
                    Case &H39
                        Return LongAckType.InvalidLink
                    Case &H3A
                        Return LongAckType.IllegalMove
                    Case &H3B   'returned by the Intellibox for certain slots it doesn't support like 127
                        Return LongAckType.UnsupportedSlot
                    Case &H3C
                        Return LongAckType.SwitchStateResponse
                    Case &H3F
                        Return LongAckType.NoFreeSlot
                    Case &H50
                        Select Case _bytaBytes(2)
                            Case &H10, &H30
                                Return LongAckType.PmOpsRead
                            Case Else
                                Return LongAckType.PmOpsWriteOK
                        End Select
                    Case &H0
                        Select Case _bytaBytes(2)
                            Case &H10, &H30
                                Return LongAckType.BdOpsRead
                            Case Else
                                Return LongAckType.BdOpsWriteOK
                        End Select
                    Case &H6F   'differs from documentation but seems correct
                        Select Case _bytaBytes(2)
                            Case &H7F
                                Return LongAckType.ProgNoReply
                            Case 0
                                Return LongAckType.ProgBusy
                            Case 1
                                Return LongAckType.ProgAccepted
                            Case &H40
                                Return LongAckType.ProgAcceptedBlind
                        End Select
                    Case &H6D   'this is not documented but it's what my command station returns
                        Return LongAckType.ImmediateRes
                    Case Else
                        Return LongAckType.Unknown
                End Select
            End Get
        End Property

        ''' <summary>Gets the state returned by some acknowledgement types.</summary>
        ''' <remarks>This value is significant only for the following acknowledgement types: <see cref="LongAckType.SwitchStateResponse"/>, <see cref="LongAckType.PmOpsRead"/>, and <see cref="LongAckType.BdOpsRead"/>.</remarks>
        Public ReadOnly Property State() As SwitchState
            Get
                Return (_bytaBytes(2) And 32) >> 5
            End Get
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkLongAck.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Long acknowledge"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkLongAck.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Select Case Me.Type
                    Case LongAckType.SwitchStateResponse, LongAckType.PmOpsRead, LongAckType.BdOpsRead
                        Return "Type=" & Me.Type.ToString & " State=" & Me.State.ToString
                    Case Else
                        Return "Type=" & Me.Type.ToString
                End Select
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet that finds data on the network.</summary>
    ''' <remarks>A <i>PkFind</i> packet response is possible but not expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkFind
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(8) {&HE5, &H9, 0, 0, 0, 0, 0, 0, 0}
            Me.IsRequest = True
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.FIND"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.FIND
            End Get
        End Property

        ''' <summary>Gets or sets wheather this packet is a request.</summary>
        ''' <value><i>True</i> if packet initiates the find; <i>False</i> if packet is a response.</value>
        Public Property IsRequest() As Boolean
            Get
                Return _bytaBytes(2) And 64
            End Get
            Set(Value As Boolean)
                If Value Then
                    _bytaBytes(2) = _bytaBytes(2) Or 64
                Else
                    _bytaBytes(2) = _bytaBytes(2) And 191
                End If
            End Set
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkFind.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.IsRequest
                    Case True
                        Return "Find request"
                    Case False
                        Return "Find response"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkFind.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return ""
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet used to send a direct DCC packet.</summary>
    ''' <remarks>This Loconet packet wraps a DCC packet which it forwards directly to the track. A <see cref="PkLongAck"/> response is expected.</remarks>
    ''' <example>
    ''' In the following example, we are setting functions 21 through 28 of an auxiliary decoder configured with an address of 813.
    ''' The order of parameter assignment is very important due to the offsets created by variable bit addressing and instruction.
    ''' For proper encoding, the address must be set first, then the instruction, and lastly the instruction specific method that encodes the data.
    '''	<code lang="VB">           
    '''     'assumes LoconetService (locServ) has been previously initialized and started
    '''     locServ.TxPacket(New PkImmediate() With {
    '''         .DccAddress = 813,
    '''         .DccInstruction = PkImmediate.DccInstrType.Func21To28,
    '''         .DccFunctions = {1, 0, 0, 0, 1, 0, 0, 0}
    '''     })
    '''	</code>
    '''	</example>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkImmediate
        Inherits Packet

        ''' <summary>Instruction type encoded in the <see cref="DccBytes"/>.</summary>
        Public Enum DccInstrType As Byte
            ''' <summary>Decoder and consist control instruction.</summary>
            DecoderCont
            ''' <summary>Advanced operation instruction.</summary>
            AdvancedOpr
            ''' <summary>Speed and direction instruction for reverse operation.</summary>
            SpeedDirRev
            ''' <summary>Speed and direction instruction for forward operation.</summary>
            SpeedDirFor
            ''' <summary>Function control instruction for auxiliary functions 0 through 4.</summary>
            Func0to4
            ''' <summary>Function control instruction for auxiliary functions 5 through 8.</summary>
            Func5to8
            ''' <summary>Function control instruction for auxiliary functions 9 through 12.</summary>
            Func9to12
            ''' <summary>Binary state control instruction (long form).</summary>
            BinStateLong
            ''' <summary>Binary state control instruction (short form).</summary>
            BinStateShort
            ''' <summary>Function control instruction for auxiliary functions 13 through 20.</summary>
            Func13To20
            ''' <summary>Function control instruction for auxiliary functions 21 through 28.</summary>
            Func21To28
            ''' <summary>Configuration variable access instruction.</summary>
            CvAccess
        End Enum

        Public Sub New()
            _bytaBytes = New Byte(10) {&HED, &HB, &H7F, 0, &H20, 0, 0, 0, 0, 0, 0}
            Me.RepeatCount = 4    'it seems good practice to repeat a few times in case of bad track contact
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.IMM_PACKET"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.IMM_PACKET
            End Get
        End Property

        ''' <summary>Gets or sets the number of times to repeat the DCC packet transmission.</summary>
        ''' <remarks>For example a repeat count of 1 means 2 DCC packets will be sent. The default is 4.</remarks>
        Public Property RepeatCount() As Byte
            Get
                Return _bytaBytes(3) And 7
            End Get
            Set(Value As Byte)
                _bytaBytes(3) = (Value And 7) Or (_bytaBytes(3) And 248)
            End Set
        End Property

        ''' <summary>Gets or sets the number of significant bytes in the <see cref="DccBytes"/> not including the checksum byte.</summary>
        ''' <value>Valid values are 2-4</value>
        ''' <remarks>This property is set automatically if DCC instruction specific methods are used. You only need to set this if you are encoding the <see cref="DccBytes"/> yourself.</remarks>
        Public Property DccByteCount() As Byte
            Get
                Return (_bytaBytes(3) >> 4) And 7
            End Get
            Set(Value As Byte)
                If Value < 2 Or Value > 4 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DCC byte count. Valid values are 2-4.")

                _bytaBytes(3) = ((Value And 7) << 4) Or (_bytaBytes(3) And 143)
            End Set
        End Property

        ''' <summary>Gets or sets the bytes that make up the DCC packet payload.</summary>
        ''' <param name="bytIdx">Valid values are 1-5, indicating the byte index number.</param>
        Public Property DccBytes(bytIdx As Byte) As Byte
            Get
                If bytIdx < 1 Or bytIdx > 5 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DCC byte index. Valid values are 1-5.")

                Dim bytBitMask As Byte = 1 << (bytIdx - 1)
                Return _bytaBytes(bytIdx + 4) Or If(_bytaBytes(4) And bytBitMask, 128, 0)
            End Get
            Set(value As Byte)
                If bytIdx < 1 Or bytIdx > 5 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DCC byte index. Valid values are 1-5.")

                _bytaBytes(bytIdx + 4) = value And 127
                Dim bytBitMask As Byte = 1 << (bytIdx - 1)
                _bytaBytes(4) = If(value >= 128, _bytaBytes(4) Or bytBitMask, _bytaBytes(4) And Not bytBitMask)
            End Set
        End Property

        ''' <summary>Gets or sets the DCC address encoded in the <see cref="DccBytes"/>.</summary>
        ''' <value>Valid address values are 1-10239.</value>       
        ''' <example>See the <see cref="PkImmediate"/> class for a code example.</example> 
        Public Property DccAddress() As UShort
            Get
                If Not Me.IsDccLongAdr Then
                    Return Me.DccBytes(1)
                Else
                    Return ((Me.DccBytes(1) And 63) << 8) Or Me.DccBytes(2)
                End If
            End Get
            Set(Value As UShort)
                If Value < 1 Or Value > 10239 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid DCC address. Valid values are 1-10239.")

                If Value <= 127 Then
                    'short address format:
                    'Byte1 {0,A6,A5,A4,A3,A2,A1,A0}

                    Me.DccBytes(1) = Value
                Else
                    'long address format:
                    'Byte1 [1,1,A13,A12,A11,A10,A9,A8]
                    'Byte2 [A7,A6,A5,A4,A3,A2,A1,A0]

                    Me.DccBytes(1) = ((Value >> 8) Or 192)
                    Me.DccBytes(2) = (Value And 255)
                End If
            End Set
        End Property

        ''' <summary>Gets the length type of the DCC address encoded in the <see cref="DccBytes"/>.</summary>
        ''' <returns><i>True</i> if encoded address is long (14 bit); <i>False</i> if encoded address is short (7 bit).</returns>
        Public ReadOnly Property IsDccLongAdr() As Boolean
            Get
                'if the two most significant bits, of the first address byte, are "11" then we have a long address (second address byte to follow)
                Return (Me.DccBytes(1) >> 6) = 3
            End Get
        End Property

        ''' <summary>Gets or sets the DCC instruction encoded in the <see cref="DccBytes"/>.</summary>
        ''' <remarks>The <see cref="DccAddress"/> must be set before setting this property for proper byte offset. Setting this property will invalidate any instruction specific data that may have been previously set.</remarks>
        ''' <example>See the <see cref="PkImmediate"/> class for a code example.</example>
        Public Property DccInstruction() As DccInstrType
            Get
                Dim bytInstrByte As Byte = Me.DccBytes(If(Me.IsDccLongAdr, 3, 2))
                Select Case True
                    Case bytInstrByte >> 5 = 0
                        Return DccInstrType.DecoderCont

                    Case bytInstrByte >> 5 = 1
                        Return DccInstrType.AdvancedOpr

                    Case bytInstrByte >> 5 = 2
                        Return DccInstrType.SpeedDirRev

                    Case bytInstrByte >> 5 = 3
                        Return DccInstrType.SpeedDirFor

                    Case bytInstrByte >> 5 = 4
                        Return DccInstrType.Func0to4

                    Case bytInstrByte >> 4 = 11
                        Return DccInstrType.Func5to8

                    Case bytInstrByte >> 4 = 10
                        Return DccInstrType.Func9to12

                    Case bytInstrByte = 192
                        Return DccInstrType.BinStateLong

                    Case bytInstrByte = 221
                        Return DccInstrType.BinStateShort

                    Case bytInstrByte = 222
                        Return DccInstrType.Func13To20

                    Case bytInstrByte = 223
                        Return DccInstrType.Func21To28

                    Case bytInstrByte >> 5 = 7
                        Return DccInstrType.CvAccess

                End Select
            End Get
            Set(value As DccInstrType)
                Dim bytIdx As Byte = If(Me.IsDccLongAdr, 3, 2)
                'D = data to be encoded later by instruction specific methods
                Select Case value
                    Case DccInstrType.DecoderCont
                        Me.DccBytes(bytIdx) = 0       '[000 DDDDD]                        

                    Case DccInstrType.AdvancedOpr
                        Me.DccBytes(bytIdx) = 32      '[001 DDDDD]

                    Case DccInstrType.SpeedDirRev
                        Me.DccBytes(bytIdx) = 64      '[010 DDDDD]

                    Case DccInstrType.SpeedDirFor
                        Me.DccBytes(bytIdx) = 96      '[011 DDDDD]

                    Case DccInstrType.Func0to4
                        Me.DccBytes(bytIdx) = 128     '[100 DDDDD]

                    Case DccInstrType.Func5to8
                        Me.DccBytes(bytIdx) = 176     '[101 1DDDD]

                    Case DccInstrType.Func9to12
                        Me.DccBytes(bytIdx) = 160     '[101 0DDDD]

                    Case DccInstrType.BinStateLong
                        Me.DccBytes(bytIdx) = 192     '[110 00000]

                    Case DccInstrType.BinStateShort
                        Me.DccBytes(bytIdx) = 221     '[110 11101]

                    Case DccInstrType.Func13To20
                        Me.DccBytes(bytIdx) = 222     '[110 11110]

                    Case DccInstrType.Func21To28
                        Me.DccBytes(bytIdx) = 223     '[110 11111]

                    Case DccInstrType.CvAccess
                        Me.DccBytes(bytIdx) = 224     '[111 DDDDD]

                End Select
            End Set
        End Property

        ''' <summary>Gets or sets the data encoded in the <see cref="DccBytes"/> related to function change requests.</summary>
        ''' <value>Array of function states.</value>
        ''' <remarks>
        ''' This property is applicable for these instruction types: <see cref="DccInstrType.Func0to4"/>, <see cref="DccInstrType.Func5to8"/>, <see cref="DccInstrType.Func9to12"/>, 
        ''' <see cref="DccInstrType.Func13To20"/>, and <see cref="DccInstrType.Func21To28"/>. The <see cref="DccInstruction"/> must be set before setting this property.
        ''' </remarks>
        ''' <example>See the <see cref="PkImmediate"/> class for a code example.</example>
        Public Property DccFunctions() As OnOff()
            Get
                Dim bytDataByte As Byte
                Select Case Me.DccInstruction
                    Case DccInstrType.Func0to4
                        bytDataByte = Me.DccBytes(If(Me.IsDccLongAdr, 3, 2))
                        Return BitWise.GetFunctions0to4(bytDataByte)

                    Case DccInstrType.Func5to8, DccInstrType.Func9to12
                        bytDataByte = Me.DccBytes(If(Me.IsDccLongAdr, 3, 2))
                        Return BitWise.BitFlagsToArray(bytDataByte, 4)

                    Case DccInstrType.Func13To20, DccInstrType.Func21To28
                        bytDataByte = Me.DccBytes(If(Me.IsDccLongAdr, 4, 3))
                        Return BitWise.BitFlagsToArray(bytDataByte, 8)

                    Case Else
                        Throw New ApplicationException("Property not applicable for encoded DCC instruction type.")

                End Select
            End Get
            Set(value As OnOff())
                Dim bytByteCount As Byte = If(Me.IsDccLongAdr, 2, 1)

                Select Case Me.DccInstruction
                    Case DccInstrType.Func0to4
                        bytByteCount += 1
                        BitWise.SetFunctions0to4(Me.DccBytes(bytByteCount), value)

                    Case DccInstrType.Func5to8, DccInstrType.Func9to12
                        bytByteCount += 1
                        BitWise.SetFunctions5to8or9to12(Me.DccBytes(bytByteCount), value)

                    Case DccInstrType.Func13To20, DccInstrType.Func21To28
                        If value.GetUpperBound(0) <> 7 Then Throw New IndexOutOfRangeException("The upper bound of given array must be 7.")
                        bytByteCount += 2
                        Me.DccBytes(bytByteCount) = BitWise.ArrayToBitFlags(value)

                    Case Else
                        Throw New ApplicationException("Property not applicable for encoded DCC instruction type.")

                End Select

                Me.DccByteCount = bytByteCount
            End Set
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkImmediate.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Select Case Me.DccInstruction
                    Case DccInstrType.DecoderCont
                        Return "DCC decoder/consist control"
                    Case DccInstrType.AdvancedOpr
                        Return "DCC advanced operation"
                    Case DccInstrType.SpeedDirRev, DccInstrType.SpeedDirFor
                        Return "DCC speed/direction control"
                    Case DccInstrType.Func0to4, DccInstrType.Func5to8, DccInstrType.Func9to12, DccInstrType.Func13To20, DccInstrType.Func21To28
                        Return "DCC function control"
                    Case DccInstrType.BinStateLong, DccInstrType.BinStateShort
                        Return "DCC binary state control"
                    Case DccInstrType.CvAccess
                        Return "DCC CV access"
                    Case Else
                        Return "Unknown DCC instruction"
                End Select
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkImmediate.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Select Case Me.DccInstruction
                    Case DccInstrType.Func0to4, DccInstrType.Func5to8, DccInstrType.Func9to12, DccInstrType.Func13To20, DccInstrType.Func21To28
                        Dim strParmsDesc As String = $"Adr={Me.DccAddress}"
                        Dim enuaFuncs() As OnOff = Me.DccFunctions
                        Select Case Me.DccInstruction
                            Case DccInstrType.Func0to4
                                strParmsDesc &= String.Format(" Func(0-4)={{{0},{1},{2},{3},{4}}}", enuaFuncs(0).ToString, enuaFuncs(1).ToString, enuaFuncs(2).ToString, enuaFuncs(3).ToString, enuaFuncs(4).ToString)

                            Case DccInstrType.Func5to8
                                strParmsDesc &= String.Format(" Func(5-8)={{{0},{1},{2},{3}}}", enuaFuncs(0).ToString, enuaFuncs(1).ToString, enuaFuncs(2).ToString, enuaFuncs(3).ToString)

                            Case DccInstrType.Func9to12
                                strParmsDesc &= String.Format(" Func(9-12)={{{0},{1},{2},{3}}}", enuaFuncs(0).ToString, enuaFuncs(1).ToString, enuaFuncs(2).ToString, enuaFuncs(3).ToString)

                            Case DccInstrType.Func13To20
                                strParmsDesc &= String.Format(" Func(13-20)={{{0},{1},{2},{3},{4},{5},{6},{7}}}", enuaFuncs(0).ToString, enuaFuncs(1).ToString, enuaFuncs(2).ToString, enuaFuncs(3).ToString, enuaFuncs(4).ToString, enuaFuncs(5).ToString, enuaFuncs(6).ToString, enuaFuncs(7).ToString)

                            Case DccInstrType.Func21To28
                                strParmsDesc &= String.Format(" Func(21-28)={{{0},{1},{2},{3},{4},{5},{6},{7}}}", enuaFuncs(0).ToString, enuaFuncs(1).ToString, enuaFuncs(2).ToString, enuaFuncs(3).ToString, enuaFuncs(4).ToString, enuaFuncs(5).ToString, enuaFuncs(6).ToString, enuaFuncs(7).ToString)

                        End Select
                        Return strParmsDesc
                    Case Else
                        Return String.Format("RepCnt={0} BytCnt={1} DccBytes={{{2:X2} {3:X2} {4:X2} {5:X2} {6:X2}{7}", Me.RepeatCount, Me.DccByteCount, Me.DccBytes(1), Me.DccBytes(2), Me.DccBytes(3), Me.DccBytes(4), Me.DccBytes(5), "}")
                End Select
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet used by the Intellibox for special control operations.</summary>
    ''' <remarks>A <see cref="PkLongAck"/> response is expected.</remarks>
    ''' <seealso cref="LoconetService.TxPacket(Packet, CancellationToken)"/>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/>
    <Serializable()> Public Class PkSpecialOem
        Inherits Packet

        Public Sub New()
            _bytaBytes = New Byte(14) {&HED, &HF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.SPCL_OEM"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.SPCL_OEM
            End Get
        End Property

        Public Overrides Function ValidPacketResponse(objResponsePacket As Packet) As Boolean
            Return objResponsePacket.OpCode = OpCodes.LONG_ACK
        End Function

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkSpecialOem.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return "Special OEM data"
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>  
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkSpecialOem.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return "No parameters decoded"
            End Get
        End Property

    End Class

    ''' <summary>Represents a Loconet packet of an unknown type.</summary>
    ''' <remarks>Incoming Loconet packets that are either not natively supported by this API or are corrupt will return this type. To create you own packet classes you must inherit from this class.</remarks>
    ''' <example>The following is an example of a custom packet class:
    '''	<code lang="VB">    
    '''     Imports RRAutoLib.Loconet
    ''' 
    '''     Public Class PkCustom
    '''         Inherits PkUnknown
    ''' 
    '''         Public Sub New()
    '''             'initialize an n byte packet;
    '''             'in this example it is a 4 byte OPS_LOCO_ADR
    '''             _bytaBytes = New Byte(3) {&amp;HBF, 0, 0, 0}
    '''         End Sub
    ''' 
    '''         Public Sub New(address As UShort)
    '''             'simplify class construction
    '''             Me.New()
    '''             Me.Address = address
    '''         End Sub
    ''' 
    '''         Public Property Address() As UShort
    '''             Get
    '''                 'decode parameter from packet bytes
    '''                 Dim bits = BitWise.CopyBits(_bytaBytes(3), 0, 7, 0, 0)  'least significant 7 bits
    '''                 Return BitWise.CopyBits(_bytaBytes(2), 0, 7, bits, 7)   'most significant 7 bits
    '''             End Get
    '''             Set(value As UShort)
    '''                 'encode parameter to packet bytes
    '''                 BitWise.CopyBits(value, 0, 7, _bytaBytes(3), 0)     'least significant 7 bits
    '''                 BitWise.CopyBits(value, 7, 7, _bytaBytes(2), 0)     'most significant 7 bits
    '''             End Set
    '''         End Property
    ''' 
    '''         Public Overrides ReadOnly Property NeedsPacketResponse() As Boolean
    '''             Get
    '''                 'indicates that this packet expects a response
    '''                 Return True
    '''             End Get
    '''         End Property
    ''' 
    '''         Public Overrides Function ValidPacketResponse(responsePacket As RRAutoLib.Loconet.Packet) As Boolean
    '''             'indicates that this packet expects either a slot read or a long aknoledge as a response packet    
    '''             Return responsePacket.OpCode = OpCodes.SL_RD_DATA OrElse _
    '''                    responsePacket.OpCode = OpCodes.LONG_ACK
    '''         End Function
    ''' 
    '''         Public Overrides ReadOnly Property Description() As String
    '''             Get
    '''                 'brief description of packet
    '''                 Return "My custom packet"
    '''             End Get
    '''         End Property
    ''' 
    '''         Public Overrides ReadOnly Property ParmsDesc() As String
    '''             Get
    '''                 'description of decoded packet parameters
    '''                 Return $"Adr={Me.Address}"
    '''             End Get
    '''         End Property
    ''' 
    '''     End Class
    '''	</code>
    ''' </example>
    ''' <seealso cref="LoconetService.RxPacketOnWorkerThread"/>
    ''' <seealso cref="LoconetService.RxPacketOnSyncThread"/> 
    <Serializable()> Public Class PkUnknown
        Inherits Packet

        Private _strDescription As String

        ''' <summary>Permits instantiation of user derived classes.</summary>
        ''' <remarks>This constructor should only be used by derived classes after implementation. It is not implemented in this class.</remarks>
        Public Sub New()
        End Sub

        Friend Sub New(bytByteCount As Byte, strDescription As String)
            _bytaBytes = New Byte(bytByteCount - 1) {}
            _strDescription = strDescription
        End Sub

        ''' <summary>Used to inherit custom serialization from <see cref="Packet" /> because constructors are not inherited.</summary>
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>Gets the Loconet operation code associated with the packet.</summary>
        ''' <value>The <see cref="OpCodes.None"/> member of the <see cref="OpCodes"/> enumeration type.</value>
        ''' <remarks>The <i>OpCode</i> is automatically assigned to the packet internally.</remarks>
        Public Overrides ReadOnly Property OpCode() As OpCodes
            Get
                Return OpCodes.None
            End Get
        End Property

        ''' <summary>User friendly description of the packet.</summary>
        ''' <seealso cref="PkUnknown.ParmsDesc"/>
        Public Overrides ReadOnly Property Description() As String
            Get
                Return _strDescription
            End Get
        End Property

        ''' <summary>User friendly description of the most significant packet parameters.</summary>
        ''' <remarks>Parameters decoded from the packet's <see cref="Packet.Bytes"/>.</remarks>
        ''' <seealso cref="PkUnknown.Description"/>
        Public Overrides ReadOnly Property ParmsDesc() As String
            Get
                Return ""
            End Get
        End Property

    End Class

#End Region

End Namespace
