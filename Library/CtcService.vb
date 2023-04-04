Imports System.Runtime.Serialization
Imports System.Runtime.Serialization.Formatters
Imports System.Windows.Threading
Imports RRAutoLib.Loconet
Imports RRAutoLib.Scripting

Namespace CTC

    ''' <summary>The main class for providing CTC (Centralized Traffic Control) services.</summary>
    Public NotInheritable Class CtcService
        Private Shared _objDefinitionRootData As New RootData               'the object definitions serialized to file
        Private Shared _objStateRootData As New List(Of CtcObjectBase)      'the object states serialized to file
        Private Shared _objCtcThread As Thread                              'background CTC thread
        Private Shared _objCtcDispatcher As Dispatcher                      'the dispacher responsible for executing queued methods on the CTC thread

#Region "Events"

        ''' <summary>Occurs before the <see cref="CtcService" /> is about to be stared or stopped.</summary>
        ''' <remarks>If <see cref="CtcService.IsStarted" /> is queried in this event the current mode before the change will be returned.</remarks>
        Public Shared Event BeforeModeChange()

        ''' <summary>Occurs after the <see cref="CtcService" /> has been stared or stopped.</summary>
        ''' <remarks>If <see cref="CtcService.IsStarted" /> is queried in this event the new changed mode will be returned.</remarks>
        Public Shared Event ModeChanged()

        '---------------------

        ''' <summary>Occurs when a propery of a <see cref="CtcObjectBase" /> instance is changed.</summary>
        Public Shared Event ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)

        Friend Shared Sub RaiseObjectChangedEvent(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                RaiseEvent ObjectChanged(objSender, objChangedObject, objPreviousObject)
            Else
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent ObjectChanged(objSender, objChangedObject, objPreviousObject))
            End If

        End Sub

        '---------------------

        ''' <summary>The category of a CTC message event.</summary>
        Public Enum MessageCat As Byte
            ''' <summary>CTC operation message.</summary>
            Native
            ''' <summary>Message posted by user script.</summary>
            User
            ''' <summary>Error message.</summary>
            [Error]
        End Enum

        ''' <summary>Occurs when a CTC message has been received.</summary>
        ''' <param name="enuCategory">The message category.</param>
        ''' <param name="strMessage">The text message.</param>
        Public Shared Event RxMessage(enuCategory As MessageCat, strMessage As String)

        ''' <summary>Posts a text message to the CTC event stream.</summary>
        ''' <param name="enuCategory">The message category.</param>
        ''' <param name="strMessage">The text message.</param>
        ''' <remarks>Messages can be observed through the <see cref="RxMessage" /> event.</remarks>
        Public Shared Sub PostMessage(enuCategory As MessageCat, strMessage As String)

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                RaiseEvent RxMessage(enuCategory, strMessage)
            Else
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent RxMessage(enuCategory, strMessage))
            End If

        End Sub

#End Region

#Region "Properties"

        ''' <summary>Gets or sets the service that provides communications between the CTC and the Loconet network.</summary>
        ''' <value>A <see cref="Loconet.LoconetService"/> instance.</value>
        ''' <remarks>The given <see cref="Loconet.LoconetService"/> instance should be configured and started before using the CTC for operation.</remarks>
        Public Shared Property LoconetService() As LoconetService

        ''' <summary>Gets or sets the dispatcher that will be responsible for thread synchronization.</summary> 
        ''' <remarks>
        ''' CtcService events will be raised on the thread this dispatcher is associated with. In most cases this will be the UI thread. 
        ''' If left null (Default), events will be raised on the CTC thread but this is not recommended.
        ''' </remarks> 
        Public Shared Property SyncContext() As Dispatcher

        ''' <summary>Gets or sets whether the CTC service is running in simulation mode.</summary>
        ''' <remarks>When <i>True</i>, Loconet packet exchanges will be suppressed, allowing state change operations to occur while disconnected.</remarks>
        Public Shared Property SimulationMode() As Boolean

        ''' <summary>Gets or sets whether set state commands should be transmitted even if the target state is already held.</summary>
        Public Shared Property ForceSetState() As Boolean

        ''' <summary>Gets a value indicating whether the CTC service is currently runnig.</summary>
        ''' <value><i>True</i> if service is running; otherwise, <i>False</i>.</value>
        Public Shared ReadOnly Property IsStarted() As Boolean
            Get
                Return _objCtcThread IsNot Nothing
            End Get
        End Property


        ''' <summary>Gets the collection of <see cref="Track" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Tracks() As TracksList
            Get
                Return _objDefinitionRootData.Tracks
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Block" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Blocks() As BlocksList
            Get
                Return _objDefinitionRootData.Blocks
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Route" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Routes() As RoutesList
            Get
                Return _objDefinitionRootData.Routes
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Sensor" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Sensors() As SensorsList
            Get
                Return _objDefinitionRootData.Sensors
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Signal" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Signals() As SignalsList
            Get
                Return _objDefinitionRootData.Signals
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Accessory" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Accessories() As AccessoriesList
            Get
                Return _objDefinitionRootData.Accessories
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Label" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Labels() As LabelsList
            Get
                Return _objDefinitionRootData.Labels
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Button" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Buttons() As ButtonsList
            Get
                Return _objDefinitionRootData.Buttons
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Engine" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Engines() As EnginesList
            Get
                Return _objDefinitionRootData.Engines
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="Sequence" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property Sequences() As SequencesList
            Get
                Return _objDefinitionRootData.Sequences
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="EventScript" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property EventScripts() As EventScriptsList
            Get
                Return _objDefinitionRootData.EventScripts
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="StepScript" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property StepScripts() As StepScriptsList
            Get
                Return _objDefinitionRootData.StepScripts
            End Get
        End Property

        ''' <summary>Gets the collection of <see cref="GlobalScript" /> objects managed by the CTC.</summary>
        Public Shared ReadOnly Property GlobalScripts() As GlobalScriptsList
            Get
                Return _objDefinitionRootData.GlobalScripts
            End Get
        End Property

        ''' <summary>Gets or sets the binary representation of an arbitrary object that provides additional data to be stored with the layout data.</summary>
        ''' <remarks>Best practice is to store the result byte stream of your serialized object.</remarks>
        Public Shared Property PersistedTag() As Byte()
            Get
                Return _objDefinitionRootData.PersistedTag
            End Get
            Set(Value As Byte())
                _objDefinitionRootData.PersistedTag = Value
            End Set
        End Property


        Friend Shared ReadOnly Property IsCurrentThread() As Boolean
            Get
                Return Thread.CurrentThread Is _objCtcThread
            End Get
        End Property

        ''' <summary>Type of serialization. {definitions or states}</summary>
        Friend Shared Property SerializationContent As SerializationContent

        ''' <summary>Indicates that object graph had to be converted from previous version during deserialization.</summary>
        Friend Shared Property CompatibilityMode As Boolean

#End Region

#Region "Methods"

        ''' <summary>Clears out all the layout objects.</summary>
        ''' <remarks>The CTC service must be stopped for this operation.</remarks>
        Public Shared Sub NewLayout()
            CtcIsStartedException.Check()
            _objDefinitionRootData = New RootData
        End Sub

        ''' <summary>Saves the current railroad layout configuration and states.</summary>
        ''' <param name="strFileName">The name of the path and file to be loaded.</param>
        ''' <param name="enuSerializationContent">The content being loaded: definitions or states.</param>
        ''' <remarks>
        ''' Since layout definitions and states are stored in separate files, this method should
        ''' be called once for each content. States should be saved at the end of an operation
        ''' session even if definitions have not changed. 
        ''' </remarks>
        Public Shared Sub SaveLayout(strFileName As String, enuSerializationContent As SerializationContent)

            Dim objLayoutFile As New IO.FileInfo(strFileName)
            Dim objFileStream As IO.FileStream
            Try
                objFileStream = objLayoutFile.OpenWrite()
            Catch ex As Exception
                Throw New ApplicationException(String.Format("A problem occurred opening file for writing: {0}{1}{0}{0}{2}", vbCrLf, strFileName, ex.Message), ex)
            End Try
            Try
                Dim objFormatter As IFormatter = New Binary.BinaryFormatter
                objFormatter.Context = New StreamingContext(StreamingContextStates.File)
                CtcService.SerializationContent = enuSerializationContent
                Select Case enuSerializationContent
                    Case SerializationContent.Definition
                        objFormatter.Serialize(objFileStream, _objDefinitionRootData)

                    Case SerializationContent.State
                        _objStateRootData.Clear()
                        For Each objTrack As Track In CtcService.Tracks
                            If objTrack.IsTurnout Then
                                _objStateRootData.Add(objTrack)
                            End If
                        Next
                        For Each objBlock As Block In CtcService.Blocks
                            _objStateRootData.Add(objBlock)
                        Next
                        For Each objRoute As Route In CtcService.Routes
                            _objStateRootData.Add(objRoute)
                        Next
                        For Each objSensor As Sensor In CtcService.Sensors
                            _objStateRootData.Add(objSensor)
                        Next
                        For Each objSignal As Signal In CtcService.Signals
                            _objStateRootData.Add(objSignal)
                        Next
                        For Each objAccessory As Accessory In CtcService.Accessories
                            _objStateRootData.Add(objAccessory)
                        Next
                        For Each objLabel As Label In CtcService.Labels
                            _objStateRootData.Add(objLabel)
                        Next
                        For Each objButton As Button In CtcService.Buttons
                            _objStateRootData.Add(objButton)
                        Next
                        objFormatter.Serialize(objFileStream, _objStateRootData)

                End Select                
            Catch ex As Exception
                Throw New ApplicationException(String.Format("Failed to save {1} objects to file.{0}{0}{2}", vbCrLf, enuSerializationContent.ToString.ToLower, ex.Message), ex)
            Finally
                If objFileStream IsNot Nothing Then objFileStream.Close()
            End Try
        End Sub

        ''' <summary>Loads a given railroad layout configuration and states.</summary>
        ''' <param name="strFileName">The name of the path and file to be loaded.</param>
        ''' <param name="enuSerializationContent">The content being loaded: definitions or states.</param>
        ''' <param name="blnCompatibilityMode">Returns <i>True</i> if layout had to be loaded in compatibility mode.</param>
        ''' <remarks>
        ''' Since layout definitions and states are stored in separate files, this method should
        ''' be called once for each content. Definitions must always be loaded first. 
        ''' The CTC service must be stopped for this operation.
        ''' </remarks>
        Public Shared Sub LoadLayout(strFileName As String, enuSerializationContent As SerializationContent, Optional ByRef blnCompatibilityMode As Boolean = False)
            CtcIsStartedException.Check()

            Dim objLayoutFile As New IO.FileInfo(strFileName)
            If Not objLayoutFile.Exists Then Throw New IO.FileNotFoundException(String.Format("Could not find file: {0}{1}", vbCrLf, strFileName))

            Dim objFileStream As IO.FileStream
            Try
                objFileStream = objLayoutFile.OpenRead()
            Catch ex As Exception
                Throw New ApplicationException(String.Format("A problem occurred opening file for reading: {0}{1}{0}{0}{2}", vbCrLf, strFileName, ex.Message), ex)
            End Try
            Try
                Dim objFormatter As IFormatter = New Binary.BinaryFormatter
                objFormatter.Context = New StreamingContext(StreamingContextStates.File)
                objFormatter.Binder = New VersionDeserializationBinder
                CtcService.SerializationContent = enuSerializationContent
                CtcService.CompatibilityMode = False
                Select Case enuSerializationContent
                    Case SerializationContent.Definition
                        _objDefinitionRootData = objFormatter.Deserialize(objFileStream)

                    Case SerializationContent.State
                        objFormatter.Deserialize(objFileStream)
                End Select
                _objDefinitionRootData.PostDeserialize()
                _objDefinitionRootData.Initialize()
                blnCompatibilityMode = CtcService.CompatibilityMode
            Catch ex As Exception
                'if exception is thrown by methods invoked through reflection
                If TypeOf ex Is Reflection.TargetInvocationException Then ex = ex.InnerException

                Throw New ApplicationException(String.Format("Failed to load {1} objects from file.{0}Looks like the file is no longer compatible with this verison.{0}{0}{2}", vbCrLf, enuSerializationContent.ToString.ToLower, ex.Message), ex)
            Finally
                If objFileStream IsNot Nothing Then objFileStream.Close()
            End Try
        End Sub

        ''' <summary>Releases any resources used by the current layout.</summary>
        '''<remarks>All engines bound to command station slots will be unbound and the slots will be marked <i>Idle</i>.</remarks>
        Public Shared Async Function DisposeLayout() As Task
            For Each objEngine As Engine In CtcService.Engines
                Await objEngine.UnbindSlot(False)
            Next
            For Each objSequence As Sequence In CtcService.Sequences
                objSequence.Stop()
            Next
        End Function

        '--------------------------

        ''' <summary>Starts the CTC Service.</summary>
        Public Shared Async Sub Start()
            If CtcService.IsStarted Then Exit Sub

            RaiseEvent BeforeModeChange()

            'compile code
            Dim objErrors As List(Of ScriptError) = CompilerService.CompileScript(True)
            If objErrors.Count > 0 Then
                CtcService.PostMessage(CtcService.MessageCat.Error, "Errors encountered compiling script. Scripting will be disabled for this operation session.")
            Else
                CtcService.PostMessage(CtcService.MessageCat.Native, "Script compiled successfully.")
            End If

            Using objCtcStarted As New ManualResetEvent(False)

                'start worker thread that listens to commands
                _objCtcThread = New Thread(AddressOf InitCtcThread)
                With _objCtcThread
                    .IsBackground = True
                    .Name = "CtcService"
                    .Priority = ThreadPriority.Normal
                    .Start(objCtcStarted)
                End With

                'wait for thread to actually start
                objCtcStarted.WaitOne()

                'now we can use the _objCtcDispatcher
            End Using

            For Each objBlock As Block In Blocks
                Await objBlock.AfterCtcServiceStart()
            Next
            For Each objSensor As Sensor In Sensors
                Await objSensor.AfterCtcServiceStart()
            Next
            For Each objEngine As Engine In Engines
                Await objEngine.AfterCtcServiceStart()
            Next

            CtcService.PostMessage(CtcService.MessageCat.Native, "CTC started.")
            RaiseEvent ModeChanged()

            CtcService.Execute(Sub() CompilerService.InvokeGlobalMethod("OnCtcStart"))
        End Sub

        Private Shared Sub InitCtcThread(objCtcStarted As ManualResetEvent)

            _objCtcDispatcher = Dispatcher.CurrentDispatcher

            'notify the UI thread that _objDispatcher has been set
            DirectCast(objCtcStarted, ManualResetEvent).Set()

            'handles error propagated to the Dispatcher.Run()
            'rarely errors not handled properly down stream can still leak out here
            AddHandler _objCtcDispatcher.UnhandledException,
                Sub(sender As Object, e As DispatcherUnhandledExceptionEventArgs)
                    CtcService.PostMessage(CtcService.MessageCat.Error, "Error occurred on CTC thread dispatcher: " &
                        If(e.Exception.InnerException Is Nothing, e.Exception.Message, e.Exception.InnerException.Message))
                    e.Handled = True
                End Sub

            'initiate the dispatcher to process requests
            'this method does not return until Dispatcher.InvokeShutdown() is called
            Dispatcher.Run()

            'CtcThread will terminate when this method exists
        End Sub

        ''' <summary>Places a subroutine on the CTC thread execution queue.</summary>
        Public Shared Sub Execute(delLambda As Action)
            If _objCtcDispatcher IsNot Nothing Then
                _objCtcDispatcher.InvokeAsync(
                    Sub()
                        Try
                            delLambda.Invoke()
                        Catch ex As Exception
                            CtcService.PostMessage(CtcService.MessageCat.Error, "Error occurred invoking job on CTC thread: " &
                                If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message))
                        End Try
                    End Sub)
            End If
        End Sub

        ''' <summary>Stops the CTC Service.</summary>
        Public Shared Async Sub [Stop]()
            If Not CtcService.IsStarted Then Exit Sub
            OnCtcThreadException.Check()

            RaiseEvent BeforeModeChange()

            Dim objTcs As New TaskCompletionSource
            CtcService.Execute(Async Sub()
                                   Dim objTask As Task = CompilerService.InvokeGlobalMethod("OnCtcStop")
                                   If objTask IsNot Nothing Then Await objTask
                                   objTcs.TrySetResult()
                               End Sub)
            Await objTcs.Task

            For Each objBlock As Block In CtcService.Blocks
                objBlock.BeforeCtcServiceStop()
            Next
            For Each objSensor As Sensor In CtcService.Sensors
                objSensor.BeforeCtcServiceStop()
            Next
            For Each objEngine As Engine In CtcService.Engines
                objEngine.BeforeCtcServiceStop()
            Next

            'todo: place a cancel order on all EventScripts to cancel all task being awaited
            'right now we are just stopping all the scripts cold when the resources are disposed

            For Each objStepScript As StepScript In CtcService.StepScripts
                objStepScript.BeforeCtcServiceStop()
            Next

            _objCtcDispatcher.InvokeShutdown()
            _objCtcDispatcher = Nothing

            _objCtcThread.Join()  'wait for thread to terminate (i.e. InitCtcThread() method to end)
            _objCtcThread = Nothing

            For Each objStepScript As StepScript In CtcService.StepScripts
                objStepScript.AfterCtcServiceStop()
            Next

            CompilerService.UnbindScript()

            CtcService.PostMessage(CtcService.MessageCat.Native, "CTC stopped.")
            RaiseEvent ModeChanged()
        End Sub

        '--------------------------

        ''' <summary>Refreshes the physical railroad states to the CTC held states.</summary>
        ''' <remarks>The CTC service must be started for this operation.</remarks>
        Public Shared Sub BroadcastStates()

            If CtcService.IsCurrentThread Then
                BroadcastStatesOnCtc()
            Else
                CtcIsStoppedException.Check()

                CtcService.Execute(AddressOf BroadcastStatesOnCtc)
            End If

        End Sub

        Private Shared Async Sub BroadcastStatesOnCtc()
            For Each objTrack As Track In CtcService.Tracks.Where(Function(t As Track) t.IsTurnout)
                If objTrack.States.Disposition = PkStatesList.StateDisposition.Pending Then
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Could not broadcast the state for [{0}] because it is in a pending disposition.", objTrack.ToString))
                Else
                    For Each objPacket As Packet In objTrack.State
                        Await CtcService.LoconetService.TxPacket(objPacket.Clone())
                    Next
                End If
            Next
            For Each objSignal As Signal In CtcService.Signals
                If objSignal.Aspects.Disposition = PkStatesList.StateDisposition.Pending Then
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Could not broadcast the state for [{0}] because it is in a pending disposition.", objSignal.ToString))
                Else
                    For Each objPacket As Packet In objSignal.Aspect
                        Await CtcService.LoconetService.TxPacket(objPacket.Clone())
                    Next
                End If
            Next
            For Each objAccessory As Accessory In CtcService.Accessories.Where(Function(a As Accessory) a.States.Count > 0)
                If objAccessory.States.Disposition = PkStatesList.StateDisposition.Pending Then
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Could not broadcast the state for [{0}] because it is in a pending disposition.", objAccessory.ToString))
                Else
                    For Each objPacket As Packet In objAccessory.State
                        Await CtcService.LoconetService.TxPacket(objPacket.Clone())
                    Next
                End If
            Next

            CtcService.PostMessage(CtcService.MessageCat.Native, "States broadcast complete.")

        End Sub

        ''' <summary>Queries the physical railroad's sensors for their current states.</summary>
        ''' <remarks>The CTC service must be started for this operation.</remarks>
        Public Shared Async Sub QuerySensors()
            CtcIsStoppedException.Check()  'needed because sensors only listen to sensor packets while CTC is running

            'i did not put this on the CTC thread because I am not reading CTC objects, everything here is thread safe, and it is cheap to run

            With CtcService.LoconetService
                Await .TxPacket(New PkSetSwitch(1017, SwitchState.Closed, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1018, SwitchState.Closed, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1019, SwitchState.Closed, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1020, SwitchState.Closed, OnOff.Off))

                Await .TxPacket(New PkSetSwitch(1017, SwitchState.Thrown, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1018, SwitchState.Thrown, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1019, SwitchState.Thrown, OnOff.Off))
                Await .TxPacket(New PkSetSwitch(1020, SwitchState.Thrown, OnOff.Off))
            End With

            CtcService.PostMessage(CtcService.MessageCat.Native, "Sensors query complete.")
        End Sub

        ''' <summary>Sets all the physical railroad states to their default values and eliminates pending states.</summary>
        ''' <remarks>The CTC service must be stopped for this operation.</remarks>
        Public Shared Sub ResetStates()
            CtcIsStartedException.Check()

            'turnouts
            For Each objTrack As Track In CtcService.Tracks
                If objTrack.IsTurnout Then
                    For Each objPacket As Packet In objTrack.States(objTrack.States._bytDefaultState)
                        CtcService.LoconetService.TxPacket(objPacket.Clone)
                    Next
                    objTrack.States.Reset()
                    objTrack.NotifyObjectChanged(Nothing)
                End If
            Next

            'signals
            For Each objSignal As Signal In CtcService.Signals
                For Each objPacket As Packet In objSignal.Aspects(objSignal.Aspects._bytDefaultState)
                    CtcService.LoconetService.TxPacket(objPacket.Clone)
                Next
                objSignal.Aspects.Reset()
                objSignal.NotifyObjectChanged(Nothing)
            Next

            'accessories
            For Each objAccessory As Accessory In CtcService.Accessories
                If objAccessory.States.Count > 0 Then  'objAccessory.States.DefaultState does not point to a valid state if no states exist
                    For Each objPacket As Packet In objAccessory.States(objAccessory.States._bytDefaultState)
                        CtcService.LoconetService.TxPacket(objPacket.Clone)
                    Next
                    objAccessory.States.Reset()
                    objAccessory.NotifyObjectChanged(Nothing)
                End If
            Next

            'routes
            For Each objRoute As Route In CtcService.Routes
                objRoute.Reset()
                objRoute.NotifyObjectChanged(Nothing)
            Next

            'labels
            For Each objLabel As Label In CtcService.Labels
                objLabel.Reset()
                objLabel.NotifyObjectChanged(Nothing)
            Next

            'buttons
            For Each objButton As Button In CtcService.Buttons
                objButton.Reset()
                objButton.NotifyObjectChanged(Nothing)
            Next

        End Sub

#End Region

    End Class

#Region "Exceptions"

    ''' <summary>Exception thrown when execution is forbidden while the <see cref="CTCService" /> is stopped.</summary>
    ''' <remarks>During operation, the CTC service should be running so it can monitor actions that require managing objects states.</remarks>
    Public NotInheritable Class CtcIsStoppedException
        Inherits ApplicationException
        Private _strMessage As String

        Friend Sub New(strMessage As String)
            _strMessage = strMessage
        End Sub

        Friend Shared Function Check() As Boolean
            If Not CtcService.IsStarted Then
                Dim objStackTrace As New StackTrace
                Dim objMethod As Reflection.MethodBase = objStackTrace.GetFrame(1).GetMethod()  'GetFrame of 1 means the second to the last call on the stack since the call to this class is the last
                Dim strMessage As String = String.Format("Can not call [{0}] if CtcService is stopped.", objMethod.DeclaringType.Name & "." & objMethod.Name)
                Throw New CtcIsStoppedException(strMessage)
            End If
        End Function

        Public Overrides ReadOnly Property Message() As String
            Get
                Return _strMessage
            End Get
        End Property
    End Class

    ''' <summary>Exception thrown when execution is forbidden while the <see cref="CTCService" /> is started.</summary>
    ''' <remarks>During layout editing, the CTC service should not be running since it can not operate properly while the layout definition is changing.</remarks>
    Public NotInheritable Class CtcIsStartedException
        Inherits ApplicationException
        Private _strMessage As String

        Friend Sub New(strMessage As String)
            _strMessage = strMessage
        End Sub

        Friend Shared Function Check() As Boolean
            If CtcService.IsStarted Then
                Dim objStackTrace As New StackTrace
                Dim objMethod As Reflection.MethodBase = objStackTrace.GetFrame(1).GetMethod()  'GetFrame of 1 means the second to the last call on the stack since the call to this class is the last
                Dim strMessage As String = String.Format("Can not call [{0}] if CtcService is started.", objMethod.DeclaringType.Name & "." & objMethod.Name)
                Throw New CtcIsStartedException(strMessage)
            End If
        End Function

        Public Overrides ReadOnly Property Message() As String
            Get
                Return _strMessage
            End Get
        End Property
    End Class

    ''' <summary>Exception thrown when execution is forbidden on the <see cref="CTCService" />'s thread.</summary>
    Public NotInheritable Class OnCtcThreadException
        Inherits ApplicationException
        Private _strMessage As String

        Friend Sub New(strMessage As String)
            _strMessage = strMessage
        End Sub

        Friend Shared Function Check() As Boolean
            If CtcService.IsCurrentThread Then
                Dim objStackTrace As New StackTrace
                Dim objMethod As Reflection.MethodBase = objStackTrace.GetFrame(1).GetMethod()  'GetFrame of 1 means the second to the last call on the stack since the call to this class is the last
                Dim strMessage As String = String.Format("Can not call [{0}] on the CtcService thread.", objMethod.DeclaringType.Name & "." & objMethod.Name)
                Throw New OnCtcThreadException(strMessage)
            End If
        End Function

        Public Overrides ReadOnly Property Message() As String
            Get
                Return _strMessage
            End Get
        End Property
    End Class

#End Region

End Namespace



