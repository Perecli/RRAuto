Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting
Imports RRAuto.DrawHelper

Namespace CustomControls

    Friend Class ContObjects
        Inherits TreeViewX

        Public Sub New()

            Me.BeginInit()
            With Me.Items
                .Add(New TreeViewNode2(TryFindResource("IconTrack"), "Tracks"))
                .Add(New TreeViewNode2(TryFindResource("IconBlock"), "Blocks"))
                .Add(New TreeViewNode2(TryFindResource("IconRoute"), "Routes"))
                .Add(New TreeViewNode2(TryFindResource("IconSensor"), "Sensors"))
                .Add(New TreeViewNode2(TryFindResource("IconSignal"), "Signals"))
                .Add(New TreeViewNode2(TryFindResource("IconAccessory"), "Accessories"))
                .Add(New TreeViewNode2(TryFindResource("IconLabel"), "Labels"))
                .Add(New TreeViewNode2(TryFindResource("IconButton"), "Buttons"))
                .Add(New TreeViewNode2(TryFindResource("IconEngine"), "Engines"))
                .Add(New TreeViewNode2(TryFindResource("IconCronometer"), "Sequences"))
                .Add(New TreeViewNode2(TryFindResource("IconDocEvent"), "Event Scripts"))
                .Add(New TreeViewNode2(TryFindResource("IconDocStep"), "Step Scripts"))
                .Add(New TreeViewNode2(TryFindResource("IconDocGlobe"), "Global Scripts"))
            End With
            Me.EndInit()

            Me.ContextMenu = New ContextMenu()

            'add event handlers
            AddHandler Me.ContextMenuOpening, AddressOf ContextMenuOpen
            AddHandler My.Application.CtcObjectsLoaded, AddressOf CtcObjectsLoaded
            AddHandler My.Application.ContextObjChanged, AddressOf ContextObjChanged
            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged

        End Sub

        Private Enum GroupIndex As Byte
            Tracks = 0
            Blocks
            Routes
            Sensors
            Signals
            Accessories
            Labels
            Buttons
            Engines
            Sequences
            EventScripts
            StepScripts
            GlobalScripts
        End Enum

        Private Sub LoadTree()
            Dim objNode As TreeViewNode2

            Me.BeginInit()

            objNode = Me.Items(GroupIndex.Tracks)
            objNode.Tag = CtcService.Tracks
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Blocks)
            objNode.Tag = CtcService.Blocks
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Routes)
            objNode.Tag = CtcService.Routes
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Sensors)
            objNode.Tag = CtcService.Sensors
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Signals)
            objNode.Tag = CtcService.Signals
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Accessories)
            objNode.Tag = CtcService.Accessories
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Labels)
            objNode.Tag = CtcService.Labels
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Buttons)
            objNode.Tag = CtcService.Buttons
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Engines)
            objNode.Tag = CtcService.Engines
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.Sequences)
            objNode.Tag = CtcService.Sequences
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.EventScripts)
            objNode.Tag = CtcService.EventScripts
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.StepScripts)
            objNode.Tag = CtcService.StepScripts
            UpdateNodeFromTag(objNode)

            objNode = Me.Items(GroupIndex.GlobalScripts)
            objNode.Tag = CtcService.GlobalScripts
            UpdateNodeFromTag(objNode)

            Me.EndInit()
        End Sub

        Private Sub UpdateNodeFromTag(objNode As TreeViewNode2)

            With objNode
                Select Case True
                    Case TypeOf .Tag Is ICtcObjectListBase
                        .Items.Clear()
                        For Each objObject As CtcObjectBase In .Tag
                            Dim objChildNode As New TreeViewNode2
                            objChildNode.Tag = objObject
                            .Items.Add(objChildNode)
                            UpdateNodeFromTag(objChildNode)
                        Next
                        .Sort()

                    Case TypeOf .Tag Is Track
                        Dim objTrack As Track = .Tag
                        .Text = objTrack.Name
                        .ImageSource = GetTrackDrawingImage(objTrack)
                        If objTrack.IsTurnout Then
                            UpdateStateNodes(objNode, objTrack.States)
                        Else
                            .Items.Clear()
                        End If

                    Case TypeOf .Tag Is Block
                        Dim objBlock As Block = .Tag
                        .Text = objBlock.Name
                        .ImageSource = TryFindResource("IconBlock")

                    Case TypeOf .Tag Is Route
                        Dim objRoute As Route = .Tag
                        .Text = objRoute.Name
                        Select Case objRoute.State
                            Case Route.RouteState.Unlocked
                                .ImageSource = TryFindResource("IconRoute")
                            Case Route.RouteState.Locked
                                .ImageSource = TryFindResource("IconRouteLocked")
                            Case Route.RouteState.Pending
                                .ImageSource = TryFindResource("IconRoutePending")
                        End Select

                    Case TypeOf .Tag Is Sensor
                        Dim objSensor As Sensor = .Tag
                        .Text = objSensor.Name
                        If objSensor.State = OnOff.On Then
                            .ImageSource = TryFindResource("IconSensor")
                        Else
                            .ImageSource = TryFindResource("IconSensorOff")
                        End If

                    Case TypeOf .Tag Is Signal
                        Dim objSignal As Signal = .Tag
                        .Text = objSignal.Name
                        .ImageSource = GetSignalDrawingImage(objSignal)
                        UpdateStateNodes(objNode, objSignal.Aspects)

                    Case TypeOf .Tag Is Accessory
                        Dim objAccessory As Accessory = .Tag
                        .Text = objAccessory.Name
                        .ImageSource = TryFindResource("IconAccessory")
                        UpdateStateNodes(objNode, objAccessory.States)

                    Case TypeOf .Tag Is Label
                        Dim objLabel As Label = .Tag
                        .Text = objLabel.Name
                        .ImageSource = TryFindResource("IconLabel")

                    Case TypeOf .Tag Is Button
                        Dim objButton As Button = .Tag
                        .Text = objButton.Name
                        .ImageSource = GetButtonDrawingImage(objButton)

                    Case TypeOf .Tag Is Engine
                        Dim objEngine As Engine = .Tag
                        .Text = objEngine.Name
                        .ImageSource = TryFindResource("IconEngine")

                    Case TypeOf .Tag Is Sequence
                        Dim objSequence As Sequence = .Tag
                        .Text = objSequence.Name
                        Select Case objSequence.Status
                            Case Sequence.SeqStatus.Idle
                                .ImageSource = TryFindResource("IconCronometer")
                            Case Sequence.SeqStatus.Recording
                                .ImageSource = TryFindResource("IconCronRecord")
                            Case Sequence.SeqStatus.Playing
                                .ImageSource = TryFindResource("IconCronPlay")
                        End Select

                    Case TypeOf .Tag Is EventScript
                        Dim objScript As EventScript = .Tag
                        .Text = objScript.Name
                        .ImageSource = TryFindResource("IconDocEvent")

                    Case TypeOf .Tag Is StepScript
                        Dim objScript As StepScript = .Tag
                        .Text = objScript.Name
                        .ImageSource = TryFindResource("IconDocStep")

                    Case TypeOf .Tag Is GlobalScript
                        Dim objScript As GlobalScript = .Tag
                        .Text = objScript.Name
                        .ImageSource = TryFindResource("IconDocGlobe")

                End Select
            End With

        End Sub

        Private Sub UpdateStateNodes(objNode As TreeViewNode2, objStates As PkStatesList)
            'insert new state nodes
            For Each objState As PkStatesList.State In objStates
                Dim blnMatch As Boolean = False
                For Each objChildNode As TreeViewNode2 In objNode.Items
                    If objState Is objChildNode.Tag Then
                        blnMatch = True
                        Exit For
                    End If
                Next
                If Not blnMatch Then
                    'insert state node
                    Dim objChildNode As New TreeViewNode2
                    objChildNode.ImageSource = TryFindResource("State")
                    objChildNode.Tag = objState
                    objNode.Items.Add(objChildNode)
                End If
            Next

            'delete obsolete state nodes
            Dim intMaxIdx As Integer = objNode.Items.Count - 1
            Dim intIdx As Integer = 0
            Do While intIdx <= intMaxIdx
                Dim objChildNode As TreeViewNode2 = objNode.Items(intIdx)
                Dim blnMatch As Boolean = False
                For Each objState As PkStatesList.State In objStates
                    If objState Is objChildNode.Tag Then
                        blnMatch = True
                        Exit For
                    End If
                Next
                If Not blnMatch Then
                    'delete state node
                    objChildNode.Tag = Nothing
                    objNode.Items.Remove(objChildNode)
                    intMaxIdx -= 1
                Else
                    intIdx += 1
                End If
            Loop

            'update state nodes
            For Each objChildNode As TreeViewNode2 In objNode.Items
                For Each objState As PkStatesList.State In objStates
                    If objChildNode.Tag Is objState Then
                        'don't know why but setting a node's Text takes a lot of clock cycles; this may no longer be true in WPF
                        If objChildNode.Text <> objState.Name Then objChildNode.Text = objState.Name
                        UpdatePacketNodes(objChildNode, objState)
                        Exit For
                    End If
                Next
            Next
        End Sub

        Private Sub UpdatePacketNodes(objNode As TreeViewNode2, objState As PkStatesList.State)
            'insert new packet nodes
            For Each objPacket As Packet In objState
                Dim blnMatch As Boolean = False
                For Each objChildNode As TreeViewNode2 In objNode.Items
                    If objPacket Is objChildNode.Tag Then
                        blnMatch = True
                        Exit For
                    End If
                Next
                If Not blnMatch Then
                    'insert packet node
                    Dim objChildNode As New TreeViewNode2
                    objChildNode.Text = objPacket.Description
                    Select Case True
                        Case TypeOf objPacket Is PkSetSwitch
                            objChildNode.ImageSource = TryFindResource("IconSwitch")
                        Case TypeOf objPacket Is PkInput
                            objChildNode.ImageSource = TryFindResource("IconSensor")
                        Case TypeOf objPacket Is PkImmediate
                            objChildNode.ImageSource = TryFindResource("IconDecoder")
                        Case TypeOf objPacket Is PkLocoIO
                            objChildNode.ImageSource = TryFindResource("Device")
                    End Select
                    objChildNode.Tag = objPacket
                    objNode.Items.Add(objChildNode)
                End If
            Next

            'delete obsolete packet nodes
            Dim intMaxIdx As Integer = objNode.Items.Count - 1
            Dim intIdx As Integer = 0
            Do While intIdx <= intMaxIdx
                Dim objChild As TreeViewNode2 = objNode.Items(intIdx)
                Dim blnMatch As Boolean = False
                For Each objPacket As Packet In objState
                    If objChild.Tag Is objPacket Then
                        blnMatch = True
                        Exit For
                    End If
                Next
                If Not blnMatch Then
                    'delete packet node
                    objChild.Tag = Nothing
                    objNode.Items.Remove(objChild)
                    intMaxIdx -= 1
                Else
                    intIdx += 1
                End If
            Loop

        End Sub


        Private Function GetGroupIndex(objObject As Object) As GroupIndex
            Select Case True
                Case TypeOf objObject Is Track Or TypeOf objObject Is TracksList
                    Return GroupIndex.Tracks
                Case TypeOf objObject Is Block Or TypeOf objObject Is BlocksList
                    Return GroupIndex.Blocks
                Case TypeOf objObject Is Route Or TypeOf objObject Is RoutesList
                    Return GroupIndex.Routes
                Case TypeOf objObject Is Sensor Or TypeOf objObject Is SensorsList
                    Return GroupIndex.Sensors
                Case TypeOf objObject Is Signal Or TypeOf objObject Is SignalsList
                    Return GroupIndex.Signals
                Case TypeOf objObject Is Accessory Or TypeOf objObject Is AccessoriesList
                    Return GroupIndex.Accessories
                Case TypeOf objObject Is Label Or TypeOf objObject Is LabelsList
                    Return GroupIndex.Labels
                Case TypeOf objObject Is Button Or TypeOf objObject Is ButtonsList
                    Return GroupIndex.Buttons
                Case TypeOf objObject Is Engine Or TypeOf objObject Is EnginesList
                    Return GroupIndex.Engines
                Case TypeOf objObject Is Sequence Or TypeOf objObject Is SequencesList
                    Return GroupIndex.Sequences
                Case TypeOf objObject Is EventScript Or TypeOf objObject Is EventScriptsList
                    Return GroupIndex.EventScripts
                Case TypeOf objObject Is StepScript Or TypeOf objObject Is StepScriptsList
                    Return GroupIndex.StepScripts
                Case TypeOf objObject Is GlobalScript Or TypeOf objObject Is GlobalScriptsList
                    Return GroupIndex.GlobalScripts
            End Select
        End Function

        Private Function FindNode(enuIdx As GroupIndex, objCtcObject As CtcObjectBase) As TreeViewNode2
            For Each objNode As TreeViewNode2 In Me.Items(enuIdx).items
                If objNode.Tag Is objCtcObject Then
                    Return objNode
                End If
            Next
        End Function

#Region "Context Menu"

        Private _objContextMenuNode As TreeViewNode2

        Private Sub ContextMenuOpen(sender As Object, e As ContextMenuEventArgs)

            e.Handled = True                'without this, the context menu settings set in this sub are not applied properly

            Dim objNode As TreeViewNode2 = TreeViewX.ParseForNode(e)
            Select Case True
                Case objNode Is Nothing
                    'do nothing because user clicked probably clicked on the background of the control (not over a node)
                    Exit Sub

                Case TypeOf e.OriginalSource Is TreeViewNode2
                    'e.OriginalSource contains a TreeViewNode2 only when context menu is launched through the keyboard
                    'otherwise it returns a visual that the Node is composed of

                    _objContextMenuNode = Me.SelectedItem

                    'position the context menu by the selected node
                    _objContextMenuNode.BringIntoView()
                    Application.DoEvents()
                    Me.ContextMenu.PlacementTarget = _objContextMenuNode.ContextMenuPlacementTarget
                    Me.ContextMenu.Placement = Primitives.PlacementMode.Bottom

                Case Else
                    _objContextMenuNode = objNode
                    Me.ContextMenu.Placement = Primitives.PlacementMode.MousePoint

            End Select

            Dim objContextMenuObj As Object = _objContextMenuNode.Tag

            Me.ContextMenu.Items.Clear()

            'for both AppModes
            Select Case True
                Case TypeOf objContextMenuObj Is Engine
                    AddMenuItem("Open Throttle", False, False, "IconThrottle", objContextMenuObj, New RoutedEventHandler(AddressOf Throttle_Click))
                    Dim objEngine As Engine = objContextMenuObj
                    If objEngine.IsBound Then
                        AddMenuItem("Unbind from Slot", False, False, "IconUnbind", Nothing, New RoutedEventHandler(AddressOf UnbindSlot_Click))
                    Else
                        AddMenuItem("Bind to Slot", False, False, "IconBind", Nothing, New RoutedEventHandler(AddressOf BindSlot_Click))
                    End If

                Case TypeOf objContextMenuObj Is IScript
                    AddMenuItem("Open Script", False, False, "IconDocument", objContextMenuObj, New RoutedEventHandler(AddressOf OpenScriptEditor_Click))
            End Select

            Select Case My.Application.AppMode
                Case AppMode.Edit
                    Select Case True
                        Case TypeOf objContextMenuObj Is ICtcObjectListBase
                            AddMenuItem("Add Object", False, False, "IconAdd", Nothing, New RoutedEventHandler(AddressOf AddItem_Click))

                        Case (TypeOf objContextMenuObj Is Track AndAlso DirectCast(objContextMenuObj, Track).IsTurnout) OrElse
                             TypeOf objContextMenuObj Is Signal OrElse
                             TypeOf objContextMenuObj Is Accessory
                            If TypeOf objContextMenuObj Is Accessory Then
                                AddMenuItem("Add State", True, False, Nothing, Nothing, New RoutedEventHandler(AddressOf AddState_Click))
                                AddMenuItem("Delete All States", False, False, Nothing, Nothing, New RoutedEventHandler(AddressOf DeleteAllStates_Click))
                            End If
                            AddMenuItem("Delete All Packets", True, False, Nothing, Nothing, New RoutedEventHandler(AddressOf DeleteAllPackets_Click))

                        Case TypeOf objContextMenuObj Is Sequence
                            AddMenuItem("Record", True, False, "IconRecord", Nothing, New RoutedEventHandler(AddressOf SeqRecord_Click))
                            AddMenuItem("Play", False, False, "IconPlay", Nothing, New RoutedEventHandler(AddressOf SeqPlay_Click))
                            AddMenuItem("Stop", False, False, "IconStop", Nothing, New RoutedEventHandler(AddressOf SeqStop_Click))

                        Case TypeOf objContextMenuObj Is EventScript
                            AddMenuItem("Event Binding", False, False, "IconEvent", objContextMenuObj, New RoutedEventHandler(AddressOf OpenEventBinding_Click))

                        Case TypeOf objContextMenuObj Is PkStatesList.State
                            AddMenuItem("Add 'Set Switch' Packet", False, False, "IconSwitch", Nothing, New RoutedEventHandler(AddressOf AddSwitchPacket_Click))
                            AddMenuItem("Add 'Input Device' Packet", False, False, "IconSensor", Nothing, New RoutedEventHandler(AddressOf AddAdrPortPacket_Click))
                            AddMenuItem("Add 'DCC Function' Packet", False, False, "IconDecoder", Nothing, New RoutedEventHandler(AddressOf AddDccFuncPacket_Click))
                            AddMenuItem("Add 'LocoIO Multi-port' Packet", False, False, "Device", Nothing, New RoutedEventHandler(AddressOf AddMultiPortPacket_Click))
                            AddMenuItem("Test Transmit Packets", True, False, Nothing, Nothing, New RoutedEventHandler(AddressOf TxPackets_Click))
                            AddMenuItem("Copy Packets to Other States", False, False, Nothing, Nothing, New RoutedEventHandler(AddressOf CopyPackets_Click))
                            If TypeOf DirectCast(objContextMenuObj, PkStatesList.State).Parent Is Accessory Then _
                                AddMenuItem("Delete State", True, False, "IconDelete", Nothing, New RoutedEventHandler(AddressOf DeleteState_Click))

                        Case TypeOf objContextMenuObj Is Packet
                            AddMenuItem("Test Transmit Packet", True, False, Nothing, Nothing, New RoutedEventHandler(AddressOf TxPacket_Click))
                            AddMenuItem("Delete Packet", False, False, "IconDelete", Nothing, New RoutedEventHandler(AddressOf DeletePacket_Click))
                    End Select

                    Select Case True
                        Case TypeOf objContextMenuObj Is CtcObjectBase
                            AddMenuItem("Delete Object", True, False, "IconDelete", Nothing, New RoutedEventHandler(AddressOf Delete_Click))

                    End Select

                Case AppMode.Operation
                    Select Case True
                        Case TypeOf objContextMenuObj Is Track AndAlso DirectCast(objContextMenuObj, Track).IsTurnout
                            Dim objTrack As Track = objContextMenuObj
                            For Each objState As PkStatesList.State In objTrack.States
                                AddMenuItem(objState.Name, False, objTrack.State Is objState, Nothing, objState.Value, New RoutedEventHandler(AddressOf SetObjectState_Click))
                            Next

                        Case TypeOf objContextMenuObj Is Route
                            Dim objRoute As Route = objContextMenuObj
                            AddMenuItem("Set Only", False, False, Nothing, "Set", New RoutedEventHandler(AddressOf SetRoute_Click))
                            AddMenuItem("Set and Lock", False, False, Nothing, "Lock", New RoutedEventHandler(AddressOf SetRoute_Click))
                            AddMenuItem("Unlock", False, False, Nothing, "Unlock", New RoutedEventHandler(AddressOf SetRoute_Click))

                        Case TypeOf objContextMenuObj Is Sensor
                            Dim objSensor As Sensor = objContextMenuObj
                            For Each enuState As OnOff In New OnOff() {OnOff.On, OnOff.Off}
                                AddMenuItem(enuState.ToString, False, objSensor.State = enuState, Nothing, enuState, New RoutedEventHandler(AddressOf SetSensor_Click))
                            Next

                        Case TypeOf objContextMenuObj Is Signal
                            Dim objSignal As Signal = objContextMenuObj
                            For Each objState As PkStatesList.State In objSignal.Aspects
                                AddMenuItem(objState.Name, False, objSignal.Aspect Is objState, Nothing, objState.Value, New RoutedEventHandler(AddressOf SetObjectState_Click))
                            Next

                        Case TypeOf objContextMenuObj Is Accessory
                            Dim objAccessory As Accessory = objContextMenuObj
                            For Each objState As PkStatesList.State In objAccessory.States
                                AddMenuItem(objState.Name, False, objAccessory.State Is objState, Nothing, objState.Value, New RoutedEventHandler(AddressOf SetObjectState_Click))
                            Next

                        Case TypeOf objContextMenuObj Is Sequence
                            AddMenuItem("Record", False, False, "IconRecord", Nothing, New RoutedEventHandler(AddressOf SeqRecord_Click))
                            AddMenuItem("Play", False, False, "IconPlay", Nothing, New RoutedEventHandler(AddressOf SeqPlay_Click))
                            AddMenuItem("Stop", False, False, "IconStop", Nothing, New RoutedEventHandler(AddressOf SeqStop_Click))

                        Case TypeOf objContextMenuObj Is EventScript
                            AddMenuItem("Execute", False, False, "IconDocExecute", Nothing, New RoutedEventHandler(AddressOf ExecuteScript_Click))

                        Case TypeOf objContextMenuObj Is StepScript
                            Dim objStepScript As StepScript = objContextMenuObj
                            If objStepScript.Running Then
                                AddMenuItem("Stop", True, False, "IconStop", Nothing, New RoutedEventHandler(AddressOf StopStepScript_Click))
                            Else
                                Dim blnFirst As Boolean = True
                                Dim intIdx As Integer = 0
                                For Each strStepName As String In objStepScript.StepNames
                                    AddMenuItem(strStepName, blnFirst, objStepScript.StepPos = intIdx, Nothing, intIdx, New RoutedEventHandler(AddressOf RunStepScript_Click))
                                    intIdx += 1
                                    blnFirst = False
                                Next
                            End If

                    End Select

            End Select

            If Me.ContextMenu.Items.Count > 0 Then _
                Me.ContextMenu.IsOpen = True 'apparently if you fidle with positioning in this event this must be set to true in order for the context menu to appear

        End Sub

        Private Sub AddMenuItem(strText As String, blnBeginGroup As Boolean, blnChecked As Boolean, objImageResKey As String, objTag As Object, delHandler As RoutedEventHandler)
            Dim objMenuItem As New MenuItem()
            With objMenuItem
                .Header = strText
                .IsChecked = blnChecked
                .Tag = objTag
                If objImageResKey <> Nothing Then
                    Dim objImage As New Image
                    objImage.Width = 16
                    objImage.Height = 16
                    objImage.Source = TryFindResource(objImageResKey)
                    .Icon = objImage
                End If
                If delHandler IsNot Nothing Then AddHandler .Click, delHandler
            End With
            If blnBeginGroup AndAlso Me.ContextMenu.Items.Count > 0 Then Me.ContextMenu.Items.Add(New Separator())
            Me.ContextMenu.Items.Add(objMenuItem)
        End Sub

        Private Sub AddItem_Click(sender As Object, e As RoutedEventArgs)
            Dim objListObject As Object = _objContextMenuNode.Tag
            Dim objCtcObjectBase As CtcObjectBase
            If TypeOf objListObject Is SignalsList Then
                'since the signal look is controled outside of the API we can not default it there
                Dim objSignal As New Signal(GetSignalLook(SignalLookKey.x2_Gen_Mult).Config, SignalLookKey.x2_Gen_Mult.ToString)
                objListObject.Add(objSignal)
                objCtcObjectBase = objSignal
            Else
                objCtcObjectBase = objListObject.Add()
            End If
            objCtcObjectBase.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()

            Dim enuIdx As GroupIndex = GetGroupIndex(objCtcObjectBase)
            FindNode(enuIdx, objCtcObjectBase).IsSelected = True
        End Sub

        Private Sub Delete_Click(sender As Object, e As RoutedEventArgs)
            Dim objCtcObjectBase As CtcObjectBase = _objContextMenuNode.Tag
            With objCtcObjectBase
                .DeleteSelf()
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub


        Private Sub BindSlot_Click(sender As Object, e As RoutedEventArgs)
            Dim objEngine As Engine = _objContextMenuNode.Tag
            objEngine.BindSlot(True)
        End Sub

        Private Sub UnbindSlot_Click(sender As Object, e As RoutedEventArgs)
            Dim objEngine As Engine = _objContextMenuNode.Tag
            objEngine.UnbindSlot(True)
        End Sub

        Private Sub Throttle_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(My.Application.MainWindow, Windows.Main).OpenThrottle(sender.Tag)
        End Sub


        Private Sub OpenScriptEditor_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(My.Application.MainWindow, Windows.Main).OpenScriptEditor(DirectCast(sender.Tag, IScript))
        End Sub

        Private Sub OpenEventBinding_Click(sender As Object, e As RoutedEventArgs)
            Dim objWindow As New Windows.ScriptEventBinder
            objWindow.Owner = My.Application.MainWindow
            objWindow.EventScript = sender.Tag
            objWindow.ShowDialog()
        End Sub


        Private Sub SeqRecord_Click(sender As Object, e As RoutedEventArgs)
            Dim objSequence As Sequence = _objContextMenuNode.Tag
            objSequence.Stop()
            If Not objSequence.GetItems.Count > 0 OrElse MessageBox.Show(My.Application.MainWindow, "Overwrite existing recording?", My.Application.Name, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                If objSequence.Record() Then
                    My.Application.SetLayoutDirty()
                End If
            End If
        End Sub

        Private Sub SeqPlay_Click(sender As Object, e As RoutedEventArgs)
            Dim objSequence As Sequence = _objContextMenuNode.Tag
            objSequence.Stop()
            objSequence.Play()
        End Sub

        Private Sub SeqStop_Click(sender As Object, e As RoutedEventArgs)
            Dim objSequence As Sequence = _objContextMenuNode.Tag
            objSequence.Stop()
        End Sub


        Private Sub AddState_Click(sender As Object, e As RoutedEventArgs)
            Dim objAccessory As Accessory = _objContextMenuNode.Tag
            objAccessory.AddState()
            objAccessory.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub DeleteAllStates_Click(sender As Object, e As RoutedEventArgs)
            Dim objAccessory As Accessory = _objContextMenuNode.Tag
            objAccessory.ClearStates()
            objAccessory.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub DeleteAllPackets_Click(sender As Object, e As RoutedEventArgs)
            For Each objState As PkStatesList.State In _objContextMenuNode.Tag.States
                objState.ClearPackets()
            Next
            _objContextMenuNode.Tag.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub DeleteState_Click(sender As Object, e As RoutedEventArgs)
            Dim objState As PkStatesList.State = _objContextMenuNode.Tag
            DirectCast(objState.Parent, Accessory).RemoveState(objState)
            objState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddSwitchPacket_Click(sender As Object, e As RoutedEventArgs)
            Dim objState As PkStatesList.State = _objContextMenuNode.Tag
            Dim objPacket As New PkSetSwitch
            objState.AddPacket(objPacket)
            objPacket.Tag = objState  'set parent reference
            objState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddAdrPortPacket_Click(sender As Object, e As RoutedEventArgs)
            Dim objState As PkStatesList.State = _objContextMenuNode.Tag
            Dim objPacket As New PkInput
            objState.AddPacket(objPacket)
            objPacket.Tag = objState  'set parent reference
            objState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddDccFuncPacket_Click(sender As Object, e As RoutedEventArgs)
            Dim objState As PkStatesList.State = _objContextMenuNode.Tag
            Dim objPacket As New PkImmediate

            'default to something valid
            objPacket.DccAddress = 1
            objPacket.DccInstruction = PkImmediate.DccInstrType.Func0to4

            objState.AddPacket(objPacket)
            objPacket.Tag = objState  'set parent reference
            objState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddMultiPortPacket_Click(sender As Object, e As RoutedEventArgs)
            Dim objState As PkStatesList.State = _objContextMenuNode.Tag
            Dim objPacket As New PkLocoIO
            objPacket.Command = PkLocoIO.CommandType.MultiPortWrite
            objState.AddPacket(objPacket)
            objPacket.Tag = objState  'set parent reference
            objState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub TxPackets_Click(sender As Object, e As RoutedEventArgs)
            For Each objPacket As Packet In DirectCast(_objContextMenuNode.Tag, PkStatesList.State)
                CtcService.LoconetService.TxPacket(objPacket.Clone())
            Next
        End Sub

        Private Sub CopyPackets_Click(sender As Object, e As RoutedEventArgs)
            Dim objStateSource As PkStatesList.State = _objContextMenuNode.Tag
            For Each objStateTarget As PkStatesList.State In DirectCast(objStateSource.Parent, IPkStates).States
                If Not objStateSource Is objStateTarget Then
                    For Each objPacket As Packet In objStateSource
                        objStateTarget.AddPacket(objPacket.Clone())
                    Next
                End If
            Next
            objStateSource.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub DeletePacket_Click(sender As Object, e As RoutedEventArgs)
            Dim objPacket As Packet = _objContextMenuNode.Tag
            Dim objParentState As PkStatesList.State = objPacket.Tag
            objParentState.RemovePacket(objPacket)
            objParentState.Parent.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub TxPacket_Click(sender As Object, e As RoutedEventArgs)
            CtcService.LoconetService.TxPacket(DirectCast(_objContextMenuNode.Tag, Packet).Clone())
        End Sub


        Private Sub SetObjectState_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(_objContextMenuNode.Tag, IPkStates).SetState(DirectCast(sender.Tag, Byte))
        End Sub

        Private Sub SetSensor_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(_objContextMenuNode.Tag, Sensor).SetState(sender.tag)
        End Sub

        Private Sub SetRoute_Click(sender As Object, e As RoutedEventArgs)
            Select Case sender.Tag
                Case "Set"
                    DirectCast(_objContextMenuNode.Tag, Route).Set()
                Case "Lock"
                    DirectCast(_objContextMenuNode.Tag, Route).Lock()
                Case "Unlock"
                    DirectCast(_objContextMenuNode.Tag, Route).Unlock()
            End Select
        End Sub


        Private Sub ExecuteScript_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(_objContextMenuNode.Tag, EventScript).Execute(New ScriptEventArgs())
        End Sub


        Private Sub RunStepScript_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(_objContextMenuNode.Tag, StepScript).StartAt(sender.Tag)
        End Sub

        Private Sub StopStepScript_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(_objContextMenuNode.Tag, StepScript).Stop()
        End Sub

#End Region

#Region "Overrides"

        Protected Overrides Sub OnSelectedItemChanged(e As System.Windows.RoutedPropertyChangedEventArgs(Of Object))
            MyBase.OnSelectedItemChanged(e)

            If Not _blnExternalContextObjChange Then 'makes sure ContextObjChanged events are not echoed back out when originated externally
                My.Application.SetContextObj(Me, Me.SelectedItem.Tag)
            End If
        End Sub

        Protected Overrides Sub OnNodeDoubleClick(objNode As TreeViewItem)
            MyBase.OnNodeDoubleClick(objNode)

            Select Case True
                Case TypeOf objNode.Tag Is Engine
                    Throttle_Click(objNode, Nothing)

                Case TypeOf objNode.Tag Is IScript
                    OpenScriptEditor_Click(objNode, Nothing)
            End Select

        End Sub

#End Region

#Region "Custom Event Handling"

        Private _blnExternalContextObjChange As Boolean = False

        Private Sub CtcObjectsLoaded()
            _blnExternalContextObjChange = True   'makes sure ContextObjChanged events are not echoed back out when originated externally
            LoadTree()
            _blnExternalContextObjChange = False
        End Sub

        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            Dim enuGroupIdx As GroupIndex = GetGroupIndex(objChangedObject)
            Dim objNode As TreeViewNode2 = FindNode(enuGroupIdx, objChangedObject)
            Select Case True
                Case objNode Is Nothing
                    If objChangedObject.IsDeleted() Then
                        'do nothing
                        'this could occur if multiple NotifyObjectChanged() calls are performed after an object is deleted
                    Else
                        'add node
                        objNode = New TreeViewNode2
                        objNode.Tag = objChangedObject
                        Dim objParentNode As TreeViewNode2 = Me.Items(enuGroupIdx)

                        objParentNode.Items.Add(objNode)
                        UpdateNodeFromTag(objNode)
                        objParentNode.Sort()
                        objNode.BringIntoView()
                    End If

                Case objChangedObject.IsDeleted()   'delete node
                    Dim objParentNode As TreeViewNode2 = Me.Items(enuGroupIdx)
                    objParentNode.Items.Remove(objNode)

                Case Else                           'change node
                    UpdateNodeFromTag(objNode)
                    If objChangedObject.Name <> objPreviousObject.Name Then DirectCast(objNode.Parent, TreeViewNode2).Sort()

            End Select
        End Sub

        Private Sub ContextObjChanged(objSender As Object, objPrevContextObj As Object, objCurrContextObj As Object)
            If objSender Is Me Then Exit Sub 'don't listen to my own broadcasted events

            Dim enuIdx As GroupIndex = GetGroupIndex(objCurrContextObj)
            Dim objNodeToBeSelected As TreeViewNode2
            If TypeOf objCurrContextObj Is ICtcObjectListBase Then
                objNodeToBeSelected = Me.Items(enuIdx)
            Else
                objNodeToBeSelected = FindNode(enuIdx, objCurrContextObj)
            End If

            _blnExternalContextObjChange = True   'makes sure ContextObjChanged events are not echoed back out when originated externally
            objNodeToBeSelected.IsSelected = True  'note: OnSelectedItemChanged is not called if this node is already selected
            _blnExternalContextObjChange = False
            objNodeToBeSelected.BringIntoView()
        End Sub

#End Region

    End Class

End Namespace
