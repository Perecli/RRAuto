Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet
Imports RRAutoLib.Remoting
Imports System.Windows.Threading
Imports System.Runtime.Serialization
Imports System.Security.Permissions

'operation mode
Friend Enum AppMode
    Edit
    Operation
End Enum

'switchboard track selection modes while in edit mode
Friend Enum SbTrackMode
    Tracks
    Blocks
    Routes
End Enum

'switchboard constants
Friend Class SbConst

    Friend Shared CellSize As New Size(50, 50)
    Friend Const TrackBrushCellOverlap As Integer = 1   'units the track decals overlap their neighbor cells on all sides
    Friend Shared GridSize As New Size(45, 28)          'number of switchboard column/rows the grid is rendered for
    Friend Const MinScaleFactor As Double = 0.2         'the smallest allowed switchboard zoom scale
    Friend Const MaxScaleFactor As Double = 80          'the largest allowed switchboard zoom scale
    Friend Const PanIncrement As Integer = 20           'amount switchboard is being panned for each keyboard press

End Class

Class Application

#Region "Globals"

    Private _objMutex As System.Threading.Mutex
    Private _enuAppMode As AppMode                  'todo: get rid of this flag and use CtcService.IsStarted instead for checking mode
    Private _enuSbTrackMode As SbTrackMode
    Private _objContextObj As Object

    Friend ReadOnly Property Name() As String
        Get
            Return "Railroad Automation"
        End Get
    End Property

    Friend Property AppMode() As AppMode            'todo: convert this to method when getting rid of _enuAppMode for CtcService.IsStarted
        Get
            Return _enuAppMode
        End Get
        Set(value As AppMode)
            If value <> _enuAppMode Then
                _enuAppMode = value
                Select Case _enuAppMode
                    Case AppMode.Edit
                        SpeechRecService.Stop()
                        CtcService.Stop()
                    Case AppMode.Operation
                        CtcService.Start()
                        If My.Settings.BroadcastStates Then CtcService.BroadcastStates()
                        If My.Settings.QuerySensors Then CtcService.QuerySensors()
                        SpeechRecService.Start()
                End Select
            End If
        End Set
    End Property

    Friend Property SbTrackMode() As SbTrackMode
        Get
            Return _enuSbTrackMode
        End Get
        Set(value As SbTrackMode)
            If value <> _enuSbTrackMode Then
                _enuSbTrackMode = value
                RaiseEvent SbTrackModeChanged()
            End If
        End Set
    End Property

    Friend ReadOnly Property ContextObj() As Object
        Get
            Return _objContextObj
        End Get
    End Property

    Friend ReadOnly Property SbContextObj() As CtcObjectBase
        Get
            Return Me.SbContextObj(_objContextObj)
        End Get
    End Property

    Friend ReadOnly Property SbContextObj(objContextObj As Object) As CtcObjectBase
        Get
            Select Case True
                Case TypeOf objContextObj Is CtcObjectBase
                    Return objContextObj

                Case TypeOf objContextObj Is ICtcObjectListBase
                    Return Nothing

                Case TypeOf objContextObj Is PkStatesList.State
                    Return DirectCast(objContextObj, PkStatesList.State).Parent

                Case TypeOf objContextObj Is Packet
                    Return DirectCast(DirectCast(objContextObj, Packet).Tag, PkStatesList.State).Parent
            End Select
        End Get
    End Property

    Friend Sub SetContextObj(objSender As Object, objContextObj As Object, Optional blnInitialize As Boolean = False)

        Dim objPrevContextObj As Object = If(blnInitialize, Nothing, _objContextObj)
        _objContextObj = objContextObj
        RaiseEvent ContextObjChanged(objSender, objPrevContextObj, objContextObj)

        'if a track, block, or route is selected, change the switchboard track selection mode to the matching type
        Select Case True
            Case TypeOf objContextObj Is Track
                Me.SbTrackMode = RRAuto.SbTrackMode.Tracks
            Case TypeOf objContextObj Is Block
                Me.SbTrackMode = RRAuto.SbTrackMode.Blocks
            Case TypeOf objContextObj Is Route
                Me.SbTrackMode = RRAuto.SbTrackMode.Routes
        End Select

    End Sub

    Friend Sub SetLayoutDirty()
        Me.LayoutIsDirty = True
    End Sub

    Friend Property LayoutIsDirty As Boolean
    Friend Property LayoutConfig() As LayoutConfig
    Friend Property LastSavedLayoutConfig() As LayoutConfig

    'the last track orientation that was placed; global so all switchboard viewports can share this
    Friend Property LastTrackOrientation As Byte = 1

    'Ctc object references used for the style copy/paste feature; global so a styles can be copied between layouts
    Friend Property SignalStyle As Signal
    Friend Property LabelStyle As RRAutoLib.CTC.Label
    Friend Property ButtonStyle As RRAutoLib.CTC.Button

#End Region

#Region "RRAuto General Events"

    Friend Event CtcObjectsLoaded()
    Friend Event ContextObjChanged(objSender As Object, objPrevContextObj As Object, objCurrContextObj As Object)
    Friend Event SbTrackModeChanged()
    Friend Event SbColorChanged()

    ''' <summary>Notifies listeners that all layout objects have been changed.</summary>
    Friend Sub NotifyCtcObjectsLoaded()
        RaiseEvent CtcObjectsLoaded()
    End Sub

    ''' <summary>Notifies listeners that switchboard colors have changed.</summary>
    Friend Sub NotifySbColorChanged()
        RaiseEvent SbColorChanged()
    End Sub

#End Region

#Region "DoEvents Implementation"

    ''' <summary>Processes all UI messages currently in the message queue.</summary>
    Public Shared Sub DoEvents()
        'create new nested message pump
        Dim objNestedFrame As New DispatcherFrame()

        'dispatch a callback to the current message queue, when getting called, this callback will end the nested message loop
        'note that the priority of this callback should be lower than the that of UI event messages.
        Dim objExitOperation As DispatcherOperation = Dispatcher.CurrentDispatcher.BeginInvoke(DispatcherPriority.Background, New DispatcherOperationCallback(AddressOf ExitFrame), objNestedFrame)

        'pump the nested message loop, the nested message loop will immediately process the messages left inside the message queue
        Dispatcher.PushFrame(objNestedFrame)

        'if the "ExitFrame" callback doesn't get finished, abort it
        If objExitOperation.Status <> DispatcherOperationStatus.Completed Then
            objExitOperation.Abort()
        End If
    End Sub

    Private Shared Function ExitFrame(objState As Object) As Object

        'exit the nested message loop
        TryCast(objState, DispatcherFrame).Continue = False

    End Function

#End Region

#Region "WPF Application Events"

    ''' <summary>Raised when the application starts, before the startup form is created.</summary>
    Private Sub Application_Startup(sender As Object, e As StartupEventArgs) Handles Me.Startup

        'since the framework sets My.Application.MainWindow to the first window created we do this first because My.Application.MainWindow
        'is used for MassageBox calls which can be called at any time by Application_DispatcherUnhandledException
        Dim objSplashScreen As New Windows.SplashScreen
        objSplashScreen.StatusBar.Maximum = 8

        'enforce single instance
        _objMutex = New System.Threading.Mutex(True, "RRAutoInstance")
        Dim blnOwnedByMe As Boolean = _objMutex.WaitOne(TimeSpan.Zero, False)
        If Not blnOwnedByMe Then
            MessageBox.Show(My.Application.MainWindow, "Only one instance of this application is allowed.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            My.Application.Shutdown()
            Exit Sub
        End If
        _objMutex.ReleaseMutex()

        'for thread debugging
        System.Threading.Thread.CurrentThread.Name = "RRAutoUI"

        objSplashScreen.Show()

        objSplashScreen.SetStatus("Initializing Application...")

        'theme the dock control
        Divelements.SandDock.Rendering.Themes.SetOffice2007Theme(Divelements.SandDock.Rendering.Office2007.Office2007ColorScheme.Silver)

        _enuAppMode = My.Settings.LastModeUsed
        If My.Settings.RecentDocuments Is Nothing Then My.Settings.RecentDocuments = New ArrayList

        CtcService.LoconetService = New LoconetService
        CtcService.SimulationMode = My.Settings.SimulationMode
        CtcService.ForceSetState = My.Settings.ForceSetState

        'set up event sync for the APIs            
        CtcService.LoconetService.SyncContext = Dispatcher.CurrentDispatcher
        CtcService.SyncContext = Dispatcher.CurrentDispatcher
        RemService.SyncContext = Dispatcher.CurrentDispatcher

        objSplashScreen.SetStatus("Creating Main Window...")

        Dim objMainWindow As New Windows.Main   'this calls the Initialized event on the main form
        My.Application.MainWindow = objMainWindow  'splash screen hands off main window status
        objSplashScreen.Close()
        objSplashScreen = Nothing
        objMainWindow.Show()                    'this calls the Loaded event in the main form

    End Sub

    ''' <summary>Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.</summary>
    Private Sub Application_Exit(sender As Object, e As System.Windows.ExitEventArgs) Handles Me.Exit

        My.Settings.LastModeUsed = _enuAppMode
        My.Settings.Save()

        _objMutex.Close()
    End Sub

    ''' <summary>Raised if the application encounters an unhandled exception.</summary>
    Private Sub Application_DispatcherUnhandledException(sender As Object, e As System.Windows.Threading.DispatcherUnhandledExceptionEventArgs) Handles Me.DispatcherUnhandledException
        Dim strClip As String = "The full error has been copied to the clipboard."
        Select Case True
            Case TypeOf e.Exception Is ApplicationException
                MessageBox.Show(My.Application.MainWindow, String.Format("{1}{0}{0}{2}", vbCrLf, e.Exception.Message, strClip),
                                My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            Case Else
                MessageBox.Show(My.Application.MainWindow, String.Format("An unpredictable exception occurred:{0}{1}{0}{0}{2}", vbCrLf, e.Exception.Message, strClip),
                                My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Stop)
        End Select
        Clipboard.SetDataObject(e.Exception.ToString, True)
        My.Application.Shutdown()
    End Sub

#End Region

End Class

<Serializable()> Friend NotInheritable Class LayoutConfig
    Implements ISerializable
    Implements ICloneable

    Public Contents As New Dictionary(Of Guid, IContentConfig)
    Public DockingLayout As String = My.Resources.DockingDefault

    Public Interface IContentConfig
    End Interface

    <Serializable()> Friend NotInheritable Class SwitchboardConfig
        Implements ISerializable
        Implements IContentConfig

        Public ReadOnly Deserialized As Boolean   'indicates that the data elements for this object have been populated from a previous saved session
        Public ScaleFactor As Double              'stores zooming
        Public PanPos As Point                    'stores panned position

        Public Sub New()
        End Sub

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            Me.Deserialized = True
            Me.ScaleFactor = info.GetDouble("ScaleFactor")
            Me.PanPos = info.GetValue("PanPos", GetType(Object))
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)>
        Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            info.AddValue("ScaleFactor", Me.ScaleFactor)
            info.AddValue("PanPos", Me.PanPos)
        End Sub

    End Class

    <Serializable()> Friend NotInheritable Class ScriptEditorConfig
        Implements ISerializable
        Implements IContentConfig

        Public Type As ScriptType

        Public Enum ScriptType As Byte
            [Event]
            [Step]
            [Global]
        End Enum

        Public Sub New()
        End Sub

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            Me.Type = info.GetValue("Type", GetType(Object))
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)>
        Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            info.AddValue("Type", Me.Type)
        End Sub

    End Class

    <Serializable()> Friend NotInheritable Class ThrottleConfig
        Implements ISerializable
        Implements IContentConfig

        Public KnobRampType As String = "Linear"
        Public FuncIconResKeys(7) As String       'throttle function icon configuration

        Public Sub New()
        End Sub

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            Me.KnobRampType = info.GetString("KnobRampType")
            Me.FuncIconResKeys = info.GetValue("FuncIconResKeys", GetType(Object))
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)>
        Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            info.AddValue("KnobRampType", Me.KnobRampType)
            info.AddValue("FuncIconResKeys", Me.FuncIconResKeys)
        End Sub

    End Class


    Public Sub New(Optional blnDefault As Boolean = True)
        If blnDefault Then _
            Me.Contents.Add(New Guid("{F118E3BB-3503-45a9-9BF6-274EC56B0B53}"), New SwitchboardConfig())
    End Sub

    Protected Sub New(info As SerializationInfo, context As StreamingContext)
        Me.Contents = info.GetValue("Contents", GetType(Object))
        Me.DockingLayout = info.GetString("DockingLayout")
    End Sub

    <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
    Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
        info.AddValue("Contents", Me.Contents)
        info.AddValue("DockingLayout", Me.DockingLayout)
    End Sub

    Public Function Clone() As Object Implements ICloneable.Clone
        Dim objLayoutConfig As New LayoutConfig(False)
        Dim objContents As New Dictionary(Of Guid, IContentConfig)
        For Each objContent As KeyValuePair(Of Guid, LayoutConfig.IContentConfig) In Me.Contents
            objContents.Add(objContent.Key, objContent.Value)
        Next
        objLayoutConfig.Contents = objContents
        objLayoutConfig.DockingLayout = Me.DockingLayout

        Return objLayoutConfig
    End Function

End Class