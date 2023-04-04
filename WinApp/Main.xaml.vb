Imports System.Threading.Tasks
Imports System.Deployment.Application
Imports RRAuto.CustomControls
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting
Imports RRAutoLib.Remoting
Imports Divelements.SandDock

Namespace Windows

    Class Main

        Private Sub Main_Initialized(sender As Object, e As EventArgs) Handles Me.Initialized

            Dim objSplashScreen As SplashScreen = My.Application.MainWindow

            objSplashScreen.SetStatus("Starting Remoting Service...")

            If My.Settings.EnableRemServ Then
                RemServiceStart()
            End If

            objSplashScreen.SetStatus("Configuring UI Elements...")

            'populate recent documents
            For Each strDocPath As String In My.Settings.RecentDocuments
                Me.Ribbon.ApplicationPopup.RecentDocuments.Add(strDocPath, IO.Path.GetFileName(strDocPath))
            Next

            'about popup
            Me.AppName.Text = My.Application.Name
            If ApplicationDeployment.IsNetworkDeployed Then
                Me.Version.Text = "Version: " & ApplicationDeployment.CurrentDeployment.CurrentVersion.ToString
                Me.Deployment.Text = "Deployment: ClickOnce"
            Else
                Me.Version.Text = "Version: " & My.Application.Info.Version.ToString
                Me.Deployment.Text = "Deployment: Standard Install"
            End If

            SetAppModeIndicators()
            SetConnectionOffIndicator()  'in case initial connection fails
            SetSpeechEnabledIndicator()
            SetVoiceFeedbackIndicator()
            SetRemotingEnabledIndicator()

#If Not DEBUG Then
            Me.RibGrpTesting.Visibility = Visibility.Hidden
            Me.RibGrpTesting.Width = 0   'this is needed for ribbon to collapse properly
#End If

            objSplashScreen.SetStatus("Loading Static Docking Contents...")

            'since DockingWindows are automaticaly opened when created through XAML, I opted to create them through code instead
            'they are opened when setting the layout from XML, this way some contents don't have to load if they are hidden to start

            CreateStaticContent(New Guid("{cc2e8cbd-d720-4a1c-85b4-3d60c20c02be}"), "Object Browser", "DocObjects", "IconObjects", New ContObjects, False)
            CreateStaticContent(New Guid("{beb0d8c1-e725-44ef-850a-24f49a8903c5}"), "Properties", "DocProperties", "IconProperties", New ContPropGrid, False)

            Dim objBrowser As New WebBrowser()
            objBrowser.NavigateToString(My.Resources.SpeechGrammar)
            CreateStaticContent(New Guid("{da4c3ddd-2fa0-4311-9924-db181cd764fb}"), "Speech Grammar", "DocSpeechGrammar", "IconGrammar", objBrowser, False)

            CreateStaticContent(New Guid("{84087c56-a2f8-4266-97fa-bbe41ec7e69d}"), "Loconet Packets Log", "DocLoconetLog", "IconPacketTraffic", New ContLoconet, False)
            CreateStaticContent(New Guid("{862c300b-90f5-40e8-bd55-18d1b0d6f872}"), "CTC Events", "DocCtcEvents", "IconRailroadCross", New ContCtcEvents, False)
            CreateStaticContent(New Guid("{8b07fc00-25d7-499c-92af-1ddfa6e048bc}"), "Slot Status", "DocSlotStatus", "IconComStation", New ContSlots, False)
            CreateStaticContent(New Guid("{c3f98f57-1400-43ba-ba05-39a7e8a23c3a}"), "Remote Connections", "DocRemoting", "IconNetwork", New ContRemoting, False)
            CreateStaticContent(New Guid("{3732EE0F-3535-4888-897F-C59D047A4F53}"), "Script Templates", "DocTemplates", "IconDocObjects", New ContScriptTemplates, False)

            objSplashScreen.SetStatus("Binding Events...")

            CommandManager.AddCanExecuteHandler(Me, New CanExecuteRoutedEventHandler(AddressOf CanExecuteCommand))
            CommandManager.AddExecutedHandler(Me, New ExecutedRoutedEventHandler(AddressOf ExecuteCommand))

            Me.InputBindings.Add(New InputBinding(ApplicationCommands.Help, New KeyGesture(Key.F1)))

            AddHandler My.Application.SbTrackModeChanged, AddressOf SetSbTrackModeIndicators
            AddHandler CtcService.ModeChanged, AddressOf SetAppModeIndicators
            AddHandler CtcService.LoconetService.RxPacketOnSyncThread, AddressOf RxPacket
            AddHandler SpeechRecService.SpeechActivity, AddressOf SetSpeechActivityIndicators

            objSplashScreen.SetStatus("Connecting to Loconet...")

            'open Loconet connection
            'must be called after Loconet event handlers have been registered because track power status is retrieved by this
            'must be called before loading layout so broadcast states and query sensors will work
            If Not CtcService.SimulationMode Then ToggleConnection(Nothing, Nothing)  'don't auto connect if in simulation mode

        End Sub

        Private Sub Main_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded

        End Sub

        Private Sub Main_ContentRendered(sender As Object, e As EventArgs) Handles Me.ContentRendered
            '- this method gets called after the Loaded event when the whole window has been rendered

            '- not using the splash screen to show layout loading progress because message boxes are shown behind it which is undesirable
            '- since layout loading could also occur at a time other than an application start, I should put a progress bar in the starus bar of the application

            '- this must be called after the form is loaded since sanddock makes this main window as the owner of its floating windows
            '  and the framework does not allow this practice if the owner is not loaded
            '- although we could, we don't use the Loaded event to load the layout because during the loaded event the user is shown
            '  a blank white screen which is not apropriate expecialy during long layout loads
            LoadLayout(StartupLayoutFileName())
        End Sub


#Region "Save/Load Layout"

        Private Function StartupLayoutFileName() As String
            Dim straArgs() As String
            If ApplicationDeployment.IsNetworkDeployed Then
                'for ClickOnce deployment
                straArgs = AppDomain.CurrentDomain.SetupInformation.ActivationArguments.ActivationData
                If straArgs IsNot Nothing AndAlso IO.Path.GetExtension(straArgs(0)) = ".rra" Then
                    Dim objUri As New Uri(straArgs(0))   'note that the returned activator path is in an URI format: "file:///e:/folder/file%20name.rra"
                    Return objUri.LocalPath   'return standard path format "e:\folder\file name.rra" converted from URI 
                End If
            Else
                'for standard install deployment
                straArgs = Environment.GetCommandLineArgs()
                If straArgs.Length > 1 Then
                    Return straArgs(1)  'return path passed through command line
                End If
            End If

            'return last layout used if so specified
            If My.Settings.LoadLastFile Then Return My.Settings.LastFileUsed
        End Function


        Private Sub SaveLayout()

            Me.CommitAllScriptEdits()

            If My.Settings.LastFileUsed = "" Then
                SaveAsLayout()
            Else
                SaveLayout(My.Settings.LastFileUsed)
            End If

        End Sub

        Private Sub SaveAsLayout()
            Dim saveFileDialog As New Microsoft.Win32.SaveFileDialog
            With saveFileDialog
                .FileName = "Layout"
                .DefaultExt = "rra"
                .Filter = "Railroad Automation Files (*.rra)|*.rra|All Files (*.*)|*.*"
                .InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                If .ShowDialog() Then
                    SaveLayout(.FileName)
                End If
            End With
        End Sub

        Private Sub SaveLayout(strFileName As String)
            My.Application.LayoutConfig.DockingLayout = Me.DockSite.GetLayout(True)
            My.Application.LastSavedLayoutConfig = My.Application.LayoutConfig.Clone
            CtcService.PersistedTag = Serialization.ToBytes(My.Application.LayoutConfig, "layout configuration")  'save LayoutConfig

            'save layout definitions
            Try
                CtcService.SaveLayout(strFileName, SerializationContent.Definition)
            Catch ex As Exception
                MessageBox.Show(Me, ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                Exit Sub
            End Try

            My.Application.LayoutIsDirty = False
            Me.Ribbon.ApplicationPopup.RecentDocuments.Add(strFileName, IO.Path.GetFileName(strFileName))
            If Not My.Settings.RecentDocuments.Contains(strFileName) Then My.Settings.RecentDocuments.Add(strFileName)
            My.Settings.LastFileUsed = strFileName
            Me.Title = String.Format("{0} - {1}", My.Application.Name, strFileName)
        End Sub


        Private Async Function UnLoadLayout() As Task(Of Boolean)

            Me.CommitAllScriptEdits()  'to make sure LayoutIsDirty reflects the correct state

            If My.Application.LayoutIsDirty Then
                Select Case MessageBox.Show(Me, "Save changes to this layout?",
                    My.Application.Name, MessageBoxButton.YesNoCancel, MessageBoxImage.Exclamation, MessageBoxResult.Yes)

                    Case MessageBoxResult.Yes
                        SaveLayout()

                    Case MessageBoxResult.Cancel
                        Return False
                End Select
            End If

            CloseAllContents()
            Application.DoEvents()

            If My.Application.AppMode = AppMode.Operation Then
                SpeechRecService.Stop()
                CtcService.Stop()
            End If
            Await CtcService.DisposeLayout()

            'save layout states
            If My.Settings.LastFileUsed <> Nothing Then
                Dim strFileName As String = IO.Path.ChangeExtension(My.Settings.LastFileUsed, ".rrs")
                Try
                    CtcService.SaveLayout(strFileName, SerializationContent.State)
                Catch ex As Exception
                    MessageBox.Show(Me, ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                End Try
            End If

            Return True

        End Function

        Private Sub LoadLayout(strFileName As String)
            Dim blnCompatibilityMode As Boolean = False

            Me.Cursor = Cursors.Wait
            Application.DoEvents()

            If strFileName = Nothing Then  'in this case we load an empty layout
                CtcService.NewLayout()
                My.Application.LayoutConfig = New LayoutConfig
                My.Application.LastSavedLayoutConfig = My.Application.LayoutConfig.Clone

                My.Settings.LastFileUsed = strFileName
                Me.Title = My.Application.Name

                LoadDynamicContents()
                If My.Application.AppMode = AppMode.Operation Then
                    CtcService.Start()
                    SpeechRecService.Start()
                End If
            Else
                'load layout definitions
                Try
                    CtcService.LoadLayout(strFileName, SerializationContent.Definition, blnCompatibilityMode)
                Catch ex As Exception
                    Me.Cursor = Cursors.Arrow
                    MessageBox.Show(Me, ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)

                    'load empty layout
                    LoadLayout(Nothing)

                    'remove layouts that no longer exist from the recent documents list
                    If TypeOf ex Is IO.FileNotFoundException Then
                        Me.Ribbon.ApplicationPopup.RecentDocuments.Remove(strFileName)
                        If My.Settings.RecentDocuments.Contains(strFileName) Then My.Settings.RecentDocuments.Remove(strFileName)
                    End If

                    Exit Sub
                End Try

                'load LayoutConfig
                Dim objLayoutConfig As LayoutConfig = Serialization.FromBytes(CtcService.PersistedTag, "layout configuration")
                My.Application.LayoutConfig = If(objLayoutConfig Is Nothing, New LayoutConfig, objLayoutConfig)
                My.Application.LastSavedLayoutConfig = My.Application.LayoutConfig.Clone

                'add layout to the recent documents list
                Me.Ribbon.ApplicationPopup.RecentDocuments.Add(strFileName, IO.Path.GetFileName(strFileName))
                If Not My.Settings.RecentDocuments.Contains(strFileName) Then My.Settings.RecentDocuments.Add(strFileName)

                My.Settings.LastFileUsed = strFileName
                Me.Title = String.Format("{0} - {1}", My.Application.Name, strFileName)

                'load layout states
                Dim strStatesFileName As String = IO.Path.ChangeExtension(My.Settings.LastFileUsed, ".rrs")
                Try
                    CtcService.LoadLayout(strStatesFileName, SerializationContent.State)
                Catch ex As Exception
                    Me.Cursor = Cursors.Arrow
                    MessageBox.Show(Me, ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                    Me.Cursor = Cursors.Wait
                    Application.DoEvents()
                End Try

                LoadDynamicContents()
                If My.Application.AppMode = AppMode.Operation Then
                    CtcService.Start()
                    If My.Settings.BroadcastStates Then CtcService.BroadcastStates()
                    If My.Settings.QuerySensors Then CtcService.QuerySensors()
                    SpeechRecService.Start()
                End If
            End If
            SetDockingLayout()   'needs to be called after LoadDynamicContents()

            My.Application.LayoutIsDirty = False
            My.Application.NotifyCtcObjectsLoaded()
            My.Application.SetContextObj(Me, CtcService.Tracks, True)    'initialize context object

            Me.Cursor = Cursors.Arrow

            If blnCompatibilityMode Then
                MessageBox.Show(Me, "This layout file was opened in compatibility mode." & vbCrLf & vbCrLf &
                    "Next time the layout is saved, it will be upgraded to the current version format. " &
                    "Since downgrading the layout back to a previous version is not supported, a backup should be made prior to saving.",
                    My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Information)
            End If
        End Sub

#End Region

#Region "Docking"

        'Some notes on docking
        '----------------------------
        'for static docking contents where .CloseMethod = Hide, .Close() actually means hide
        'for dynamic docking contents where .CloseMethod = Detach, .Close() means close and dispose
        '
        'Look at these to determine which docking content is currently selected:
        'DockSite.LastActiveWindowChanged event
        'Me.DockSite.LastActiveWindow
        '
        'to activate (have tab shown as selected) an already opened docking content, call DockableWindow.Open() on the opened content

        Private _intSbViewportCounter As Integer

        ''' <summary>Sets the disposition of all docking windows to a previously saved state.</summary>
        ''' <remarks>Can not be called in Window.Initialized(). Must be called after the window has loaded which sets the Window.Owner property.</remarks>
        Private Sub SetDockingLayout()

            'ensure the SplitContainer needed by SetLayout() exists for document windows to be placed into
            'for some reason unlike DockableWindow objects, the docking engine for DocumentWindow objects does not create the needed split containers automaticaly (bad design?)
            Dim objDocCont As DocumentContainer = Me.DockSite.Child
            If objDocCont.Content Is Nothing Then objDocCont.Content = New SplitContainer
            'note that I added DocumentContainer and SplitContainer in XAML but this only works on initial layout load; if the dynamic contents are removed by the user
            'the docking engine removes the SplitContainer but does not create it back when SetLayout() is called

            Try
                'temporarily place the thread in the US culture so we can deserialize the default layout
                'in the same culture it was serialized; this is needed for foreign users  
                'Thread.CurrentThread.CurrentCulture = New CultureInfo("en-US")
                Me.DockSite.SetLayout(My.Application.LayoutConfig.DockingLayout)
                'Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture()
            Catch ex As Exception
                MessageBox.Show(Me, "The docking layout could not be loaded." & vbCrLf & ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                Exit Sub
            End Try
            Me.DockSite.ActivatePrimaryDocument()  'activates the document that was active when the layout was last saved; without this no tab is active until one is selected by the user

        End Sub

        ''' <summary>This event gets called during a SetLayout when an object described in the layout XML does not exist, therefore its layout can not be set.</summary>
        Private Sub DockSite_LoadWindow(sender As Object, e As LoadWindowEventArgs) Handles DockSite.LoadWindow

#If DEBUG Then
            MessageBox.Show(Me, String.Format("Window docking content with GUID: {{{0}}} no longer exists.", e.Guid.ToString),
                            My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
#End If

        End Sub

        ''' <summary>Called when we need to create contents that are static and of single instance throughout the session.</summary>
        Private Sub CreateStaticContent(sctGuid As Guid, strTitle As String, strName As String, strIconResKey As String, objContent As UIElement, blnIsDocument As Boolean)

            Dim objWindow As DockableWindow
            If blnIsDocument Then
                objWindow = New DocumentWindow()
            Else
                objWindow = New DockableWindow()
                objWindow.ShowOptionsButton = False
            End If
            With objWindow
                .Guid = sctGuid
                .Name = strName
                .DockSite = Me.DockSite
                .Title = strTitle
                .Child = objContent
                .Image = TryFindResource(strIconResKey)
                .CloseMethod = WindowCloseMethod.Hide
            End With

        End Sub

        ''' <summary>Called when loading a layout's contents from a previously saved session.</summary>
        Private Sub LoadDynamicContents()

            _intSbViewportCounter = 0
            Dim objObsolete As New List(Of Guid)
            For Each objContent As KeyValuePair(Of Guid, LayoutConfig.IContentConfig) In My.Application.LayoutConfig.Contents
                If CreateDynamicContent(objContent.Key, objContent.Value) Is Nothing Then
                    'a docking layout that is restored to a last saved configuration can hold an obsolte reference to a CtcBaseObject.ID GUID that may have been deleted
                    objObsolete.Add(objContent.Key)
                End If
            Next
            'remove these obsolete references so they won't be serialized on next save
            'note this is not performed within loop above because removal of objects from a collection while being iterated is not allowed
            For Each sctKey As Guid In objObsolete
                My.Application.LayoutConfig.Contents.Remove(sctKey)
            Next

        End Sub

        ''' <summary>Called when we need to create a new content or when loading a layout's contents from a previous session.</summary>
        Private Function CreateDynamicContent(sctGuid As Guid, objContentConfig As LayoutConfig.IContentConfig) As DockableWindow

            Dim objWindow As DockableWindow
            Select Case True
                Case TypeOf objContentConfig Is LayoutConfig.SwitchboardConfig
                    Dim objContSwitchboard As New ContSwitchboard
                    objContSwitchboard.Switchboard.Initialize(objContentConfig)

                    _intSbViewportCounter += 1
                    objWindow = New DocumentWindow(Me.DockSite, "SB Viewport " & _intSbViewportCounter, objContSwitchboard)
                    With objWindow
                        .Name = "DocSbViewPort"    'not currently used but may be needed in the future to find this particular docking window type
                        .Image = TryFindResource("IconSwitchboard")
                        .DockingRules.AllowFloat = True
                    End With
                    AddHandler objContSwitchboard.Switchboard.CellLocChanged, AddressOf SetCellLocIndicators

                Case TypeOf objContentConfig Is LayoutConfig.ScriptEditorConfig
                    Dim objScript As IScript
                    Dim strIconResource As String
                    Select Case DirectCast(objContentConfig, LayoutConfig.ScriptEditorConfig).Type
                        Case LayoutConfig.ScriptEditorConfig.ScriptType.Event
                            strIconResource = "IconDocEvent"
                            objScript = CtcService.EventScripts(sctGuid)

                        Case LayoutConfig.ScriptEditorConfig.ScriptType.Step
                            strIconResource = "IconDocStep"
                            objScript = CtcService.StepScripts(sctGuid)

                        Case LayoutConfig.ScriptEditorConfig.ScriptType.Global
                            strIconResource = "IconDocGlobe"
                            objScript = CtcService.GlobalScripts(sctGuid)

                    End Select
                    If objScript Is Nothing Then Exit Function 'see comments in LoadDynamicContents() regarding obsolete contents
                    objWindow = New DocumentWindow()
                    With objWindow
                        .DockSite = Me.DockSite
                        .Name = "DocScriptEditor"
                        .Image = TryFindResource(strIconResource)
                        .DockingRules.AllowFloat = True
                        .Child = New ContScriptEditor(objScript)
                    End With

                Case TypeOf objContentConfig Is LayoutConfig.ThrottleConfig
                    Dim objEngine As Engine = CtcService.Engines(sctGuid)
                    If objEngine Is Nothing Then Exit Function 'see comments in LoadDynamicContents() regarding obsolete contents
                    objWindow = New DockableWindow()
                    With objWindow
                        .DockSite = Me.DockSite
                        .Name = "DocThrottle"    'not currently used but may be needed in the future to find this particular docking window type
                        .Image = TryFindResource("IconThrottle")
                        .FloatingSize = New Size(225, 300)
                        .FloatingLocation = New Point(100, 200)
                        .ShowOptionsButton = False
                        .Child = New ContThrottle(objEngine, objContentConfig)
                    End With

            End Select
            objWindow.Guid = sctGuid
            objWindow.CloseMethod = WindowCloseMethod.Detach
            AddHandler objWindow.Closed, AddressOf DisposeDynamicContent

            Return objWindow

        End Function

        ''' <summary>Called when reloading content layout configuration.</summary>
        Private Sub CloseDynamicContents()

            For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows
                If objWindow.CloseMethod = WindowCloseMethod.Detach Then objWindow.Close()
            Next

        End Sub

        ''' <summary>Called before swapping railroad automation layouts in the same session.</summary>
        Private Sub CloseAllContents()

            For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows
                objWindow.Close()
            Next

        End Sub

        ''' <summary>Called when dynamic contents are closed.</summary>
        Private Sub DisposeDynamicContent(sender As Object, e As EventArgs)

            Dim objWindow As DockableWindow = sender
            Select Case True
                Case TypeOf objWindow.Child Is ContSwitchboard
                    Dim objContent As ContSwitchboard = objWindow.Child
                    RemoveHandler objContent.Switchboard.CellLocChanged, AddressOf SetCellLocIndicators
                    objContent.Switchboard.Dispose()

                Case TypeOf objWindow.Child Is ContScriptEditor
                    Dim objContent As ContScriptEditor = objWindow.Child
                    objContent.Dispose()

                Case TypeOf objWindow.Child Is ContThrottle
                    Dim objContent As ContThrottle = objWindow.Child
                    objContent.Dispose()

            End Select
            RemoveHandler DirectCast(sender, DockableWindow).Closed, AddressOf DisposeDynamicContent
            My.Application.LayoutConfig.Contents.Remove(objWindow.Guid)

        End Sub

        ''' <summary>Implementation for missing DockSite.FindWindow() by window name instead of GUID.</summary>
        Private Function DockSiteFindWindow(strName As String) As DockableWindow

            For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows()
                If objWindow.Name = strName Then Return objWindow
            Next

        End Function

        ''' <summary>Implements DockSite.FindWindow() without calling DockSite.LoadWindow() if window was not found.</summary>
        Private Function DockSiteFindWindow(strGuid As Guid) As DockableWindow

            For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows()
                If objWindow.Guid = strGuid Then Return objWindow
            Next

        End Function

        ''' <summary>Toggle between open/close docking content.</summary>        
        Friend Sub ToggleDockableContent(strName As String)

            Dim objWindow As DockableWindow = DockSiteFindWindow(strName)
            If objWindow Is Nothing Then
                MessageBox.Show(Me, String.Format("Window docking content '{0}' was not found.", strName), My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            Else
                If objWindow.IsVisible Then
                    objWindow.Close()
                Else
                    objWindow.Open()
                End If
            End If

        End Sub

#End Region

#Region "Indicators"

        Private Sub SetCellLocIndicators(sctLocation As Location)
            Me.SblCol.Text = "Col " & sctLocation.Col
            Me.SblRow.Text = "Row " & sctLocation.Row
        End Sub

        Private Sub SetAppModeIndicators()
            If CtcService.IsStarted Then
                Me.BtnMode.Image = TryFindResource("IconEdit")
                Me.SblModeImage.Source = TryFindResource("IconGear")
                Me.Ribbon.SelectedTab = Me.Operation
                Me.SblModeText.Text = "Operation"
            Else
                Me.BtnMode.Image = TryFindResource("IconGear")
                Me.SblModeImage.Source = TryFindResource("IconEdit")
                Me.Ribbon.SelectedTab = Me.Editing
                Me.SblModeText.Text = "Edit"
            End If
            Me.BtnBroadcastStates.IsEnabled = CtcService.IsStarted
            Me.BtnQuerySensors.IsEnabled = CtcService.IsStarted
            Me.BtnResetStates.IsEnabled = Not CtcService.IsStarted
            Me.BtnSelTracks.IsEnabled = Not CtcService.IsStarted
            Me.BtnSelBlocks.IsEnabled = Not CtcService.IsStarted
            Me.BtnSelRoutes.IsEnabled = Not CtcService.IsStarted
            Me.BtnCheckScript.IsEnabled = Not CtcService.IsStarted

            SetSbTrackModeIndicators()
        End Sub

        Private Sub SetSbTrackModeIndicators()
            If CtcService.IsStarted Then
                Me.BtnSelTracks.IsChecked = False
                Me.BtnSelBlocks.IsChecked = False
                Me.BtnSelRoutes.IsChecked = False
            Else
                Me.BtnSelTracks.IsChecked = My.Application.SbTrackMode = SbTrackMode.Tracks
                Me.BtnSelBlocks.IsChecked = My.Application.SbTrackMode = SbTrackMode.Blocks
                Me.BtnSelRoutes.IsChecked = My.Application.SbTrackMode = SbTrackMode.Routes
            End If
        End Sub

        Private Sub RxPacket(objPacket As Packet)
            Select Case True
                Case TypeOf objPacket Is PkRdWrSlotData
                    Dim objRdWrSlotData As PkRdWrSlotData = objPacket
                    If objRdWrSlotData.Slot = 0 Then
                        If objRdWrSlotData.TrackPowerIsOn Then
                            SetPowerOnIndicator()
                        Else
                            SetPowerOffIndicator()
                        End If
                    End If
                Case TypeOf objPacket Is PkSetPowerOn
                    SetPowerOnIndicator()
                Case TypeOf objPacket Is PkSetPowerOff
                    SetPowerOffIndicator()
            End Select
        End Sub

        Private Sub SetConnectionOnIndicator()
            Me.SblConnectionImage.Source = TryFindResource("IconConnect")
            Me.SblConnectionText.Text = "Connected"
        End Sub

        Private Sub SetConnectionOffIndicator()
            Me.SblConnectionImage.Source = TryFindResource("IconDisconnect")
            Me.SblConnectionText.Text = "Disconnected"
        End Sub

        Private Sub SetPowerUnKnownIndicator()
            Me.SblTrackPowerImage.Source = TryFindResource("IconPowerUnknown")
            Me.SblTrackPowerText.Text = "?"
        End Sub

        Private Sub SetPowerOnIndicator()
            Me.BtnTrackPower.Tag = True
            Me.SblTrackPowerImage.Source = TryFindResource("IconPowerOn")
            Me.SblTrackPowerText.Text = "On"
        End Sub

        Private Sub SetPowerOffIndicator()
            Me.BtnTrackPower.Tag = False
            Me.SblTrackPowerImage.Source = TryFindResource("IconPowerOff")
            Me.SblTrackPowerText.Text = "Off"
        End Sub

        Private Sub SetSpeechActivityIndicators(enuType As SpeechRecService.ActivityType, strText As String)
            Select Case True
                Case enuType = SpeechRecService.ActivityType.SysMessage
                    Me.SblSpeechImage.Source = TryFindResource("IconBallBlue")
                    Me.SblSpeechText.Text = strText

                Case enuType = SpeechRecService.ActivityType.Null
                    Me.SblSpeechImage.Source = Nothing
                    Me.SblSpeechText.Text = ""

                Case enuType = SpeechRecService.ActivityType.Hypothesized
                    Me.SblSpeechImage.Source = TryFindResource("IconBallYellow")
                    Me.SblSpeechText.Text = "Hypothesized: " & strText

                Case enuType = SpeechRecService.ActivityType.Rejected
                    Me.SblSpeechImage.Source = TryFindResource("IconBallRed")
                    Me.SblSpeechText.Text = String.Format("Rejected{0} {1}", If(strText = Nothing, "", ":"), strText)

                Case enuType = SpeechRecService.ActivityType.Accepted
                    Me.SblSpeechImage.Source = TryFindResource("IconBallGreen")
                    Me.SblSpeechText.Text = "Accepted: " & strText

                Case enuType = SpeechRecService.ActivityType.Paused
                    Me.SblSpeechImage.Source = TryFindResource("IconBallGray")
                    Me.SblSpeechText.Text = "Accepted: " & strText

            End Select
        End Sub

        Private Sub SetSpeechEnabledIndicator()
            If My.Settings.SpeechService Then
                Me.SblSpeechServiceText.Text = "Enabled"
            Else
                Me.SblSpeechServiceText.Text = "Disabled"
            End If
        End Sub

        Private Sub SetVoiceFeedbackIndicator()
            If My.Settings.VoiceFeedback Then
                Me.SblVoiceFeedbackText.Text = "Enabled"
            Else
                Me.SblVoiceFeedbackText.Text = "Disabled"
            End If
        End Sub

        Private Sub SetRemotingEnabledIndicator()
            If My.Settings.EnableRemServ Then

                'with this method of getting the local IP we can't distinguish between virtual interfaces like VMWARE adds and VPNs
                'Dim objIP = Net.Dns.GetHostEntry(String.Empty).AddressList.First(Function(i) i.AddressFamily = Net.Sockets.AddressFamily.InterNetwork)
                'Dim strHostNAme = Net.Dns.GetHostName()   'to get machine name if needed in the future

                'this is a more accurate way to get the IP than Net.Dns.GetHostEntry() when there are multiple IP addresses available on local machine;
                'it makes a UDP connection to a target, the target doesn't even need to exist and it will return the preferred outbound IP address of local machine
                'this is a clever workaround to weed out virtual interfaces, VPNs and such 
                Dim strLocalIP As String
                Using objSocket = New Net.Sockets.Socket(Net.Sockets.AddressFamily.InterNetwork, Net.Sockets.SocketType.Dgram, 0)
                    objSocket.Connect("10.0.0.0", 65530)
                    strLocalIP = DirectCast(objSocket.LocalEndPoint, Net.IPEndPoint).Address.ToString
                End Using

                Me.SblRemotingServiceText.Text = $"Enabled on: http://{strLocalIP}:{RemService.ServPort}"
            Else
                Me.SblRemotingServiceText.Text = "Disabled"
            End If
        End Sub

#End Region

#Region "Commands"

        Friend Sub ShortCutKeys(sender As Object, e As KeyEventArgs)
            'note: be careful with all non WPF windows (i.e. WinForms dialong boxes, MsgBox())
            'upon opening such windows they block key capture unless the WPF main window is set back in focus

            Select Case True
                Case Keyboard.Modifiers = ModifierKeys.Control
                    Select Case e.Key
                        Case Key.N
                            NewLayout(Nothing, Nothing)
                        Case Key.O
                            OpenLayout(Nothing, Nothing)
                        Case Key.S
                            SaveLayout()
                        Case Key.B
                            If My.Application.AppMode = AppMode.Operation Then BroadcastStates(Nothing, Nothing)
                        Case Key.Q
                            If My.Application.AppMode = AppMode.Operation Then QuerySensors(Nothing, Nothing)
                        Case Key.K
                            If My.Application.AppMode = AppMode.Edit Then CheckScript(Nothing, Nothing)
                        Case Key.P
                            ToggleTrackPower(Nothing, Nothing)
                        Case Key.Delete
                            CloseDocking(Nothing, Nothing)
                        Case Key.Insert
                            DockingToSaved(Nothing, Nothing)
                    End Select

                Case Else
                    Select Case e.Key
                        Case Key.F5
                            ToggleMode(Nothing, Nothing)

                        'these are used to generate test packet shown in the blocks/sensors tutorial
                        Case Key.F11
                            'CtcService.LoconetService.TxPacket(New PkInput(10, 5, OnOffState.On))
                        Case Key.F12
                            'CtcService.LoconetService.TxPacket(New PkInput(10, 5, OnOffState.Off))
                    End Select

            End Select

        End Sub


        Private Sub CanExecuteCommand(sender As Object, e As CanExecuteRoutedEventArgs)
            e.Handled = True

            Select Case True
                Case e.Command Is ApplicationCommands.Help
                    e.CanExecute = True

                Case Else
                    e.Handled = False

            End Select

        End Sub

        Private Sub ExecuteCommand(sender As Object, e As ExecutedRoutedEventArgs)

            e.Handled = True

            Select Case True
                Case e.Command Is ApplicationCommands.Help
                    Process.Start("http://www.perecli.com/rrauto/guide")

                Case Else
                    e.Handled = False

            End Select

        End Sub


        Private Async Sub NewLayout(sender As Object, e As RoutedEventArgs)
            If Await UnLoadLayout() Then LoadLayout(Nothing)
        End Sub

        Private Async Sub OpenLayout(sender As Object, e As RoutedEventArgs)
            Dim openFileDialog As New Microsoft.Win32.OpenFileDialog
            With openFileDialog
                .DefaultExt = "rra"
                .Filter = "Railroad Automation Files (*.rra)|*.rra|All Files (*.*)|*.*"
                If My.Settings.LastFileUsed = "" Then
                    .InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                Else
                    .InitialDirectory = IO.Path.GetDirectoryName(My.Settings.LastFileUsed)
                End If
                If .ShowDialog() Then
                    If Await UnLoadLayout() Then LoadLayout(.FileName)
                End If
            End With
        End Sub

        Private Async Sub OpenRecentDocument(sender As Object, e As Divelements.SandRibbon.DocumentEventArgs)
            If Await UnLoadLayout() Then LoadLayout(e.Document.Path)
        End Sub

        Private Sub SaveLayout(sender As Object, e As RoutedEventArgs)
            SaveLayout()
        End Sub

        Private Sub SaveAsLayout(sender As Object, e As RoutedEventArgs)
            SaveAsLayout()
        End Sub

        Private Sub OpenChangeLog(sender As Object, e As RoutedEventArgs)
            Process.Start("https://www.perecli.com/rrauto/ChangeLog.aspx")

            'I kept this here for refference
            '            Try
            '#If DEBUG Then
            '                Process.Start(AppDomain.CurrentDomain.BaseDirectory & "..\release.txt")
            '#Else
            '                Process.Start(AppDomain.CurrentDomain.BaseDirectory & "release.txt")
            '#End If
            '            Catch ex As Exception
            '                MessageBox.Show(Me, ex.Message, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            '            End Try
        End Sub

        Private Sub OpenRRAutoSite(sender As Object, e As RoutedEventArgs)
            Process.Start("http://www.perecli.com/rrauto")
        End Sub

        Private Sub OpenEmail(sender As Object, e As RoutedEventArgs)
            Process.Start("mailto:perecli@live.com")
        End Sub

        Private Sub OpenPaypal(sender As Object, e As RoutedEventArgs)
            Process.Start("https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=Perecli%40live%2ecom&lc=US&item_name=Railroad%20Automation%20Development&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted")
        End Sub

        Private Sub OpenOptions(sender As Object, e As System.EventArgs)
            Dim objWindow As New Windows.Options
            objWindow.Owner = Me
            objWindow.ShowDialog()
        End Sub


        Private Sub ToggleMode(sender As Object, e As RoutedEventArgs)
            'commits edited property grid value before switching mode; 
            'prevents exceptions when switching mode with shortcuts on properties that require a particular CTC service start state
            'Me.Focus()   'see it still needed with WPF Property Grid

            Select Case My.Application.AppMode
                Case AppMode.Edit
                    My.Application.AppMode = AppMode.Operation
                Case AppMode.Operation
                    My.Application.AppMode = AppMode.Edit
            End Select
        End Sub


        Private Sub SetEditMode(sender As Object, e As RoutedEventArgs)
            'commits edited property grid value before switching mode; 
            'prevents exceptions when switching mode with shortcuts on properties that require a particular CTC service start state
            'Me.Focus()   'see it still needed with WPF Property Grid

            My.Application.AppMode = AppMode.Edit
        End Sub

        Private Sub BroadcastStates(sender As Object, e As RoutedEventArgs)
            CtcService.BroadcastStates()
        End Sub

        Private Sub QuerySensors(sender As Object, e As RoutedEventArgs)
            CtcService.QuerySensors()
        End Sub

        Private Sub RemotingService(sender As Object, e As RoutedEventArgs)
            If My.Settings.EnableRemServ Then
                RemService.Stop()
                My.Settings.EnableRemServ = False
                SetRemotingEnabledIndicator()
            Else
                RemServiceStart()
                If My.Settings.EnableRemServ Then SetRemotingEnabledIndicator()
            End If
        End Sub

        Private Sub RemoteConnections(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocRemoting")
        End Sub

        Private Sub SpeechService(sender As Object, e As RoutedEventArgs)
            If My.Settings.SpeechService Then
                My.Settings.SpeechService = False
                SetSpeechEnabledIndicator()
                If My.Application.AppMode = AppMode.Operation Then SpeechRecService.Stop()
            Else
                My.Settings.SpeechService = True
                SetSpeechEnabledIndicator()
                If My.Application.AppMode = AppMode.Operation Then SpeechRecService.Start()
            End If
        End Sub

        Private Sub VoiceFeedback(sender As Object, e As RoutedEventArgs)
            My.Settings.VoiceFeedback = Not My.Settings.VoiceFeedback
            SetVoiceFeedbackIndicator()
        End Sub

        Private Sub OpenSpeechGrammar(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocSpeechGrammar")
        End Sub

        Private Sub OpenSbViewport(sender As Object, e As RoutedEventArgs)
            Dim sctGuid As Guid = Guid.NewGuid
            Dim objSwitchboardConfig As New LayoutConfig.SwitchboardConfig()
            CreateDynamicContent(sctGuid, objSwitchboardConfig).Open()
            My.Application.LayoutConfig.Contents.Add(sctGuid, objSwitchboardConfig)
        End Sub

        Private Sub OpenObjectBrowser(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocObjects")
        End Sub

        Private Sub OpenProperties(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocProperties")
        End Sub

        Private Sub OpenCtcEvents(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocCtcEvents")
        End Sub

        Private Sub CloseDocking(sender As Object, e As RoutedEventArgs)
            For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows
                If Not TypeOf objWindow Is DocumentWindow Then objWindow.Close()
            Next
        End Sub

        Private Sub DockingToSaved(sender As Object, e As RoutedEventArgs)
            CloseDynamicContents()
            My.Application.LayoutConfig = My.Application.LastSavedLayoutConfig.Clone
            LoadDynamicContents()
            SetDockingLayout()
        End Sub

        Private Sub DockingToDefault(sender As Object, e As RoutedEventArgs)
            CloseDynamicContents()
            My.Application.LayoutConfig = New LayoutConfig
            LoadDynamicContents()
            SetDockingLayout()
        End Sub


        Private Sub Test1(sender As Object, e As RoutedEventArgs)
            'Dim objAssembly As Reflection.Assembly = Reflection.Assembly.GetAssembly(GetType(CtcService))
            'For Each objMethodInfo As Type In objAssembly.GetType("RRAutoLib.CTC.CtcService").GetNestedTypes()
            '    MessageBox.Show(objMethodInfo.Name)
            'Next

            'show docking layout
            'MessageBox.Show(Me.DockSite.GetLayout(True))

            'test IMM_PACKET 
            'CtcService.LoconetService.TxPacket(New PkImmediate() With {
            '    .DccAddress = 813,
            '    .DccInstruction = PkImmediate.DccInstrType.Func21To28,
            '    .DccFunctions = {1, 0, 0, 0, 0, 0, 0, 0}
            '})

            'test MULTI_SENSE 
            'CtcService.LoconetService.TxPacket(New PkMultiSense With {
            '    .Type = MultiSenseType.TransponderDetect,
            '    .DeviceAddress = 34,
            '    .DccAddress = 50
            '})

        End Sub

        Private Sub Test2(sender As Object, e As RoutedEventArgs)
            'show layout config
            'Dim strMessage As String
            'For Each a As DockableWindow In Me.DockSite.GetAllWindows()
            '    strMessage &= String.Format("{0} {1}{2}", a.Guid, a.Name, vbNewLine)
            'Next
            'MessageBox.Show(My.Application.LayoutConfig.DockingLayout & vbNewLine & strMessage)

            'make the current docking layout the default 
            'Dim objStreamWriter As New IO.StreamWriter("..\resources\DockingDefault.xml")
            'objStreamWriter.Write(Me.DockSite.GetLayout(True))
            'objStreamWriter.Close()
            'MessageBox.Show("Default Layout has been set.")

            'PkDccProgram.SetDcc4DigitAddress(CtcService.LoconetService, DccProgMode.ServTrkPagedByte, 1020)

            'turn on speed compensation in OP mode
            'CtcService.LoconetService.TxPacket(New PkDccProgram(True, DccProgMode.OperByteNoFeedback, 1020, 57, 5))
        End Sub

        Private Sub Test3(sender As Object, e As RoutedEventArgs)
            'set docking layout
            'Me.DockSite.SetLayout(My.Application.LayoutConfig.DockingLayout)

            'test for missing packet responses
            'For intSwitch As Byte = 1 To 44
            '    Dim objBDLPacket As New PkMultiSense
            '    objBDLPacket.ProgWrite = False
            '    objBDLPacket.Type = MultiSenseType.BlockDetectionOps
            '    objBDLPacket.ProgAddress = 4
            '    objBDLPacket.ProgOpSwitch = intSwitch
            '    CtcService.LoconetService.TxPacket(objBDLPacket)
            'Next

            'save the current docking layout to the desktop 
            Dim objStreamWriter As New IO.StreamWriter("D:\Profile\Desktop\DockingDefault.xml")
            objStreamWriter.Write(Me.DockSite.GetLayout(True))
            objStreamWriter.Close()
            MessageBox.Show("Current Layout has been saved to the desktop as 'DockingDefault.xml'.")
        End Sub


        Private Sub SetOperationMode(sender As Object, e As RoutedEventArgs)
            'commits edited property grid value before switching mode; 
            'prevents exceptions when switching mode with shortcuts on properties that require a particular CTC service start state
            'Me.Focus()   'see it still needed with WPF Property Grid

            My.Application.AppMode = AppMode.Operation
        End Sub

        Private Sub ResetStates(sender As Object, e As RoutedEventArgs)
            CtcService.ResetStates()
            MessageBox.Show(Me, "States have been reset.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Information)
        End Sub

        Private Sub SelTracks(sender As Object, e As RoutedEventArgs)
            My.Application.SbTrackMode = SbTrackMode.Tracks
        End Sub

        Private Sub SelBlocks(sender As Object, e As RoutedEventArgs)
            My.Application.SbTrackMode = SbTrackMode.Blocks
        End Sub

        Private Sub SelRoutes(sender As Object, e As RoutedEventArgs)
            My.Application.SbTrackMode = SbTrackMode.Routes
        End Sub

        Private Sub CheckScript(sender As Object, e As RoutedEventArgs)

            Me.CommitAllScriptEdits()

            Dim objErrors As List(Of CompilerService.ScriptError) = CompilerService.ValidateScript()
            If objErrors.Count > 0 Then
                Dim objWindow As New Windows.ScriptErrors(objErrors)
                objWindow.Owner = Me
                objWindow.ShowDialog()
            Else
                MessageBox.Show(Me, "Script compiled successfully.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Information)
            End If

        End Sub

        Private Sub OpenGenAssembly(sender As Object, e As RoutedEventArgs)

            Me.CommitAllScriptEdits()

            Dim objWindow As New Windows.GeneratedAssembly
            objWindow.Owner = Me
            objWindow.ShowDialog()

        End Sub

        Private Sub OpenExportScript(sender As Object, e As RoutedEventArgs)

            Me.CommitAllScriptEdits()

            Dim saveFileDialog As New Microsoft.Win32.SaveFileDialog
            With saveFileDialog
                .FileName = "Scripts"
                .DefaultExt = "txt"
                .Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
                .InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                If .ShowDialog() Then
                    Dim objStreamWriter As IO.StreamWriter = New IO.StreamWriter(.FileName, False)
                    With objStreamWriter

                        'enumerate Event Scripts
                        For Each objScript As EventScript In CtcService.EventScripts
                            'add script name
                            .WriteLine("==========================================================================================")
                            .WriteLine(String.Format("Event Script Name: ""{0}""", objScript.Name))
                            .WriteLine("------------------------------------------------------------------------------------------")

                            'add script bindings
                            If objScript.EventBindings.Count > 0 Then
                                .WriteLine("Event Bindings:")
                                For Each objEvent As EventScript.Event In objScript.EventBindings
                                    .WriteLine(String.Format("- {0}.{1}", objEvent.Owner.ToString, objEvent.Name))
                                Next
                                .WriteLine("------------------------------------------------------------------------------------------")
                            End If

                            'add script code
                            .WriteLine(objScript.Script.Replace(vbLf, vbCrLf))  'apparently the code editor control saves vbLf instead of vbCrLf and the writer does not respect vbLf
                            .WriteLine()
                            .WriteLine()
                        Next

                        'Enumerate Step Scripts
                        For Each objScript As StepScript In CtcService.StepScripts
                            'add script name
                            .WriteLine("==========================================================================================")
                            .WriteLine(String.Format("Step Script Name: ""{0}""", objScript.Name))
                            .WriteLine("------------------------------------------------------------------------------------------")

                            'add script code
                            .WriteLine(objScript.Script.Replace(vbLf, vbCrLf))  'apparently the code editor control saves vbLf instead of vbCrLf and the writer does not respect vbLf
                            .WriteLine()
                            .WriteLine()
                        Next

                        'Enumerate Global Scripts
                        For Each objScript As GlobalScript In CtcService.GlobalScripts
                            'add script name
                            .WriteLine("==========================================================================================")
                            .WriteLine(String.Format("Global Script Name: ""{0}""", objScript.Name))
                            .WriteLine("------------------------------------------------------------------------------------------")

                            'add script code
                            .WriteLine(objScript.Script.Replace(vbLf, vbCrLf))  'apparently the code editor control saves vbLf instead of vbCrLf and the writer does not respect vbLf
                            .WriteLine()
                            .WriteLine()
                        Next

                        .Close()
                    End With
                End If
            End With

        End Sub

        Private Sub OpenScriptTemplates(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocTemplates")
        End Sub

        Private Sub OpenLibraryDocs(sender As Object, e As RoutedEventArgs)
            Process.Start("http://www.perecli.com/rrauto/ApiDocs/html/T_RRAutoLib_Scripting_Scripting.htm")
        End Sub


        Friend Sub ToggleConnection(sender As Object, e As RoutedEventArgs)
            If CtcService.LoconetService.IsStarted Then
                CtcService.LoconetService.Stop()
                SetConnectionOffIndicator()
            Else
                Dim blnTryAgain As Boolean = False
                CtcService.LoconetService.ComPort = My.Settings.ComPort
                CtcService.LoconetService.BaudRate = My.Settings.BaudRate
                CtcService.LoconetService.FlowControl = My.Settings.FlowControl
                Do
                    Dim blnStarted As Boolean = False
                    Try
                        CtcService.LoconetService.Start()
                        blnStarted = True
                    Catch ex As Exception
                        blnTryAgain = MessageBox.Show(Me, ex.Message & vbCrLf & "Retry connection attempt?", My.Application.Name, MessageBoxButton.OKCancel, MessageBoxImage.Exclamation) = MessageBoxResult.OK
                    End Try
                    If blnStarted Then SetConnectionOnIndicator()
                Loop Until Not blnTryAgain
                SetPowerUnKnownIndicator()
                CtcService.LoconetService.TxPacket(New PkReqSlotData(0))  'requests a slot packet to set the track power status
            End If
        End Sub

        Friend Sub ToggleTrackPower(sender As Object, e As RoutedEventArgs)
            If Me.BtnTrackPower.Tag Then
                SetPowerUnKnownIndicator()
                CtcService.LoconetService.TxPacket(New PkSetPowerOff)
            Else
                SetPowerUnKnownIndicator()
                CtcService.LoconetService.TxPacket(New PkSetPowerOn)
            End If
        End Sub

        Private Sub OpenSlotStatus(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocSlotStatus")
        End Sub

        Private Sub OpenLoconetLog(sender As Object, e As RoutedEventArgs)
            ToggleDockableContent("DocLoconetLog")
        End Sub

        Private Async Sub OpenLocoBufferRep(sender As Object, e As RoutedEventArgs)
            Dim objPacket As New PkBusy
            Await CtcService.LoconetService.TxPacket(objPacket)
            If objPacket.RxPacket IsNot Nothing Then
                Dim objPeerPacket As PkPeerXfer = objPacket.RxPacket
                MessageBox.Show(Me, String.Format("Version: {0}.{1}.{2}.{3}", objPeerPacket.Data(1) >> 4, objPeerPacket.Data(1) And 15, objPeerPacket.Data(5) >> 4, objPeerPacket.Data(5) And 15) & Environment.NewLine &
                    "Bad Frames Count: " & ((CType(objPeerPacket.Data(2), Integer) << 16) Or (CType(objPeerPacket.Data(3), Integer) << 8) Or objPeerPacket.Data(4)) & Environment.NewLine &
                    "Collisions Count: " & ((CType(objPeerPacket.Data(6), Integer) << 16) Or (CType(objPeerPacket.Data(7), Integer) << 8) Or objPeerPacket.Data(8)), "LocoBuffer Report", MessageBoxButton.OK, MessageBoxImage.Information)
            End If
        End Sub

        Private Sub OpenLocoIOProg(sender As Object, e As RoutedEventArgs)
            Dim blnWindowOpen As Boolean
            For Each objWindow As Window In Me.OwnedWindows
                If objWindow.Name = "LocoIoWindow" Then
                    blnWindowOpen = True
                    Exit For
                End If
            Next
            If Not blnWindowOpen Then
                Dim objWindow As New Windows.LocoIoProg
                objWindow.Owner = Me
                objWindow.Show()
            End If
        End Sub

        Private Sub OpenDccProg(sender As Object, e As RoutedEventArgs)
            Dim blnWindowOpen As Boolean
            For Each objWindow As Window In Me.OwnedWindows
                If objWindow.Name = "DccWindow" Then
                    blnWindowOpen = True
                    Exit For
                End If
            Next
            If Not blnWindowOpen Then
                Dim objWindow As New Windows.DccProg
                objWindow.Owner = Me
                objWindow.Show()
            End If
        End Sub


        Friend Sub RemServiceStart()

            RemService.ServPort = My.Settings.RemServPort

            Dim blnAddHttpAcl As Boolean = False        'asert that HttpAcl should be added
            Dim blnHttpAclAdded As Boolean = False      'indicates that an HttpAcl add occured
            Do
                Try
                    RemService.Start()
                    My.Settings.EnableRemServ = True
                Catch e As Exception
                    Dim strMessage As String
                    Dim be = e.GetBaseException
                    If TypeOf be Is Net.HttpListenerException AndAlso Not blnHttpAclAdded Then
                        strMessage = "An HTTP listener must be registered with the operating system, before starting the remoting service for the first time. " &
                                     "This requires permission elevation which you'll be prompted for next."
                        blnAddHttpAcl = True
                    Else
                        strMessage = $"{e.Message}{vbCrLf}{TypeName(be)}: {be.Message}"
                        My.Settings.EnableRemServ = False
                    End If
                    MessageBox.Show(My.Application.MainWindow, strMessage, My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                End Try

                blnHttpAclAdded = False

                If blnAddHttpAcl Then
                    blnAddHttpAcl = False

                    Dim strResult = RemService.AddHttpAcl()
                    If strResult.Contains("Error:") Then
                        My.Settings.EnableRemServ = False
                        MessageBox.Show(My.Application.MainWindow, $"Failed to register HTTP listener.{strResult}",
                                        My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                    Else
                        blnHttpAclAdded = True
                        MessageBox.Show(My.Application.MainWindow, $"Your machine's firewall must also be configured to allow incoming TCP traffic on port {RemService.ServPort}.",
                                        My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Information)
                    End If
                End If

            Loop Until Not blnHttpAclAdded

        End Sub

        Friend Sub OpenThrottle(objEngine As Engine)

            Dim objDocWin As DockableWindow = DockSiteFindWindow(objEngine.ID)

            If objDocWin Is Nothing Then
                Dim objThrottleConfig As New LayoutConfig.ThrottleConfig()
                CreateDynamicContent(objEngine.ID, objThrottleConfig).Float(WindowOpenMethod.OpenSelectActivate)
                My.Application.LayoutConfig.Contents.Add(objEngine.ID, objThrottleConfig)
            Else
                objDocWin.Open()    'sets existing window active
            End If

        End Sub

        Friend Sub OpenScriptEditor(objScript As IScript)

            Dim objDocWin As DocumentWindow = DockSiteFindWindow(objScript.ID)

            If objDocWin Is Nothing Then
                Dim objScriptEditorConfig As New LayoutConfig.ScriptEditorConfig
                Select Case True
                    Case TypeOf objScript Is EventScript
                        objScriptEditorConfig.Type = LayoutConfig.ScriptEditorConfig.ScriptType.Event
                    Case TypeOf objScript Is StepScript
                        objScriptEditorConfig.Type = LayoutConfig.ScriptEditorConfig.ScriptType.Step
                    Case TypeOf objScript Is GlobalScript
                        objScriptEditorConfig.Type = LayoutConfig.ScriptEditorConfig.ScriptType.Global
                End Select
                CreateDynamicContent(objScript.ID, objScriptEditorConfig).Open()
                My.Application.LayoutConfig.Contents.Add(objScript.ID, objScriptEditorConfig)
            Else
                objDocWin.Open()    'sets existing window active
            End If

        End Sub

        Private Sub CommitAllScriptEdits()

            If Not CtcService.IsStarted Then  'if the CTC is started the commit already occurred during the mode change and not edits where allowed since
                For Each objWindow As DockableWindow In Me.DockSite.GetAllWindows().Where(Function(dw) dw.Name = "DocScriptEditor")
                    DirectCast(objWindow.Child, ContScriptEditor).CommitScriptEdits()
                Next
            End If

        End Sub

#End Region

        Private blnOkToClose As Boolean = False

        Protected Overrides Async Sub OnClosing(e As ComponentModel.CancelEventArgs)

            MyBase.OnClosing(e)

            If Not blnOkToClose Then
                e.Cancel = True

                'if an Await is used while e.Cancel is False the application will terminate before the Await gets a chance to 
                'return its value because the UI message pump could become empty promting the application to think its done;
                'this prevents all downstream async continuations from executing; for this reson we have to set e.Cancel to True
                'before doing the Await; but if the returned value from UnLoadLayout() indicates that it is OK to close,
                'e.Cancel can't be set back to False; so in this case we have to retrigger the shutdown with a flag indicating
                'that a previous shutdown call, executed the prerequisite code, and can proceede with the application closure

                If Await UnLoadLayout() Then
                    RemoveHandler My.Application.SbTrackModeChanged, AddressOf SetSbTrackModeIndicators
                    RemoveHandler CtcService.ModeChanged, AddressOf SetAppModeIndicators
                    RemoveHandler CtcService.LoconetService.RxPacketOnSyncThread, AddressOf RxPacket
                    RemoveHandler SpeechRecService.SpeechActivity, AddressOf SetSpeechActivityIndicators

                    CtcService.LoconetService.Stop()

                    blnOkToClose = True
                    My.Application.Shutdown()
                End If
            End If

        End Sub

    End Class

End Namespace
