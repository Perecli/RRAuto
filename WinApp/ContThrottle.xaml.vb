Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports Divelements.SandDock

Namespace CustomControls

    Partial Public Class ContThrottle

        Private _objEngine As Engine
        Private _objConfig As LayoutConfig.ThrottleConfig   'holds: knob ramp type and the function icon configuration

        'unique identifier for this throttle; 
        'this allows the throttle to know if received speed packets are local (originated from itself) or foreign (originated from other throttles with posibly the same assigned engine);
        'to reduse lag, local and foreign packets are treated differently in updating the speed indicators;
        'indicators for local speed changes are updated pre packet send, while indicators for foreign speed changes are updated on packet echoes (post packet send)
        Private _sctClientToken As Guid = Guid.NewGuid

        Friend Sub New(objEngine As Engine, objConfig As LayoutConfig.ThrottleConfig)

            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            _objEngine = objEngine
            _objConfig = objConfig   'todo: the FuncIconResKeys values in this object are currently not implemented 

            'set the knob ramp type from previous saved session
            SetKnobRampType(_objConfig.KnobRampType)

            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged

            AddHandler _objEngine.BindSlotResponse, AddressOf BindSlotResponse
            AddHandler _objEngine.UnbindSlotResponse, AddressOf UnbindSlotResponse
            AddHandler _objEngine.SpeedChanged, AddressOf RefreshSpeed
            AddHandler _objEngine.DirectionChanged, AddressOf RefreshDirection
            AddHandler _objEngine.FunctionChanged, AddressOf RefreshFunction

            ConfigureTrottle()
            If _objEngine.IsBound Then
                RefreshSpeed(Nothing)
                RefreshDirection()
                RefreshFunction()
            End If
        End Sub

        Private Sub Me_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
            'the docking system calls this event not only when the control is first opened but also every time the document window
            'comes into view after being out of view behind another tabbed document

            'blnInit ensures statements are only executed when the control is first loaded
            Static blnInit As Boolean = True
            If blnInit Then
                'these statements are located in this event because the needed Me.Parent value is not populated until the control is Loaded
                SetHostEngineName()
                AddHandler DirectCast(Me.Parent, Control).PreviewKeyDown, AddressOf ShortCutKeysDown
                AddHandler DirectCast(Me.Parent, Control).PreviewKeyUp, AddressOf ShortCutKeysUp
                blnInit = False
            End If
        End Sub

        Private Sub ConfigureTrottle()
            Dim blnBound As Boolean = _objEngine.IsBound

            If blnBound Then
                Me.BtnBind.ToolTip = "Unbind from Slot"
                Me.ImgBind.Source = TryFindResource("IconUnbind")

                With Me.SpeedKnob
                    .BeginInit()
                    .Minimum = 0
                    .Maximum = _objEngine.SpeedStepMax
                    .LargeMarkerCnt = SpeedKnob.Maximum / 6
                    .SmallMarkerCnt = SpeedKnob.Maximum / 18
                    .EndInit()
                End With
            Else
                Me.BtnBind.ToolTip = "Bind to Slot"
                Me.ImgBind.Source = TryFindResource("IconBind")
            End If

            Me.BtnEStop.IsEnabled = blnBound
            Me.BtnRampType.IsEnabled = blnBound
            Me.BtnDir.IsEnabled = blnBound
            Me.GroupBoxKnob.IsEnabled = blnBound
            Me.ToolBarBottom.IsEnabled = blnBound
        End Sub

        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            If objChangedObject Is _objEngine Then
                Select Case True
                    Case objChangedObject.IsDeleted
                        'close throttle content if engine has been deleted
                        DirectCast(Me.Parent, DockableWindow).Close()

                    Case objPreviousObject.Name <> objChangedObject.Name
                        SetHostEngineName()

                End Select
            End If
        End Sub

        Private Sub SetHostEngineName()
            With DirectCast(Me.Parent, DockableWindow)
                .Title = "Throttle - " & _objEngine.Name
                .TabText = _objEngine.Name
            End With
        End Sub

#Region "UI Event Handlers"

        Private Sub BtnBind_Click(sender As Object, e As RoutedEventArgs)
            If _objEngine.IsBound Then
                _objEngine.UnbindSlot(True)
            Else
                _objEngine.BindSlot(True)
            End If
        End Sub

        Private Sub BtnRampType_PreviewMouseDown(sender As Object, e As MouseButtonEventArgs)
            If e.ChangedButton = MouseButton.Left Then
                Dim objRampTypeMenu As ContextMenu = Me.BtnRampType.FindResource("RampTypeMenu")
                objRampTypeMenu.PlacementTarget = Me.BtnRampType
                objRampTypeMenu.IsOpen = True
            End If
        End Sub

        Private Sub RampType_Click(sender As Object, e As RoutedEventArgs)
            SetKnobRampType(sender.Tag)
        End Sub

        Private Sub BtnEStop_Click(sender As Object, e As RoutedEventArgs)
            EmergencyStop()
        End Sub

        Private Sub BtnDir_Click(sender As Object, e As RoutedEventArgs)
            ChangeDirection()
        End Sub

        Private Sub SpeedKnob_MouseUp(sender As Object, e As MouseButtonEventArgs) Handles SpeedKnob.MouseUp
            If e.ChangedButton = MouseButton.Right Then ChangeDirection()
        End Sub

        Private Sub SpeedKnob_ValueChangedInternally() Handles SpeedKnob.ValueChangedInternally
            _objEngine.SetSpeed(Me.SpeedKnob.Value, _sctClientToken)
            Me.Speed.Content = Math.Round((Me.SpeedKnob.Value) / (SpeedKnob.Maximum) * 100) & "%"
        End Sub

        Private Sub BtnFunc_PreviewMouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs)
            'used the Preview version of this event because MouseLeftButtonDown event does not get raised, 
            'probably because the Click event sets the event handled internally

            _objEngine.Functions(sender.Tag) = OnOff.On
        End Sub

        Private Sub BtnFunc_Click(sender As Object, e As RoutedEventArgs)
            'this gets raised on mouse up and toggles the state of the sender.IsChecked internally,
            'so sender.IsChecked if polled here has the new toggled value

            If sender.Tag = "2" Then  'function 2 is push state
                sender.IsChecked = False  'force state of sender.IsChecked regardles of what state it was set internally
                _objEngine.Functions(sender.Tag) = OnOff.Off
            Else          'all other functions are toggle state
                _objEngine.Functions(sender.Tag) = If(sender.IsChecked, OnOff.On, OnOff.Off)
            End If
        End Sub

        Private Sub ShortCutKeysDown(sender As Object, e As KeyEventArgs)

            If Not Me.GroupBoxKnob.IsEnabled Then Exit Sub

            e.Handled = True

            Select Case e.Key
                Case Key.Escape
                    EmergencyStop()
                Case Key.F2
                    SetKnobRampType("Linear")
                Case Key.F3
                    SetKnobRampType("ExpLow")
                Case Key.F4
                    SetKnobRampType("ExpHigh")
                Case Key.Space
                    ChangeDirection()
                Case Key.Home
                    ChangeDirection(LocoDirection.Forward)
                Case Key.End
                    ChangeDirection(LocoDirection.Reverse)
                Case Key.D0, Key.NumPad0
                    _objEngine.Functions(0) = If(Me.BtnF0.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D1, Key.NumPad1
                    _objEngine.Functions(1) = If(Me.BtnF1.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D2, Key.NumPad2
                    'function 2 is push state; all others are toggle state
                    If _objEngine.Functions(2) = OnOff.Off Then  'only set when func is off to prevent hold down key repeat
                        _objEngine.Functions(2) = OnOff.On
                    End If
                Case Key.D3, Key.NumPad3
                    _objEngine.Functions(3) = If(Me.BtnF3.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D4, Key.NumPad4
                    _objEngine.Functions(4) = If(Me.BtnF4.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D5, Key.NumPad5
                    _objEngine.Functions(5) = If(Me.BtnF5.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D6, Key.NumPad6
                    _objEngine.Functions(6) = If(Me.BtnF6.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D7, Key.NumPad7
                    _objEngine.Functions(7) = If(Me.BtnF7.IsChecked, OnOff.Off, OnOff.On)
                Case Key.D8, Key.NumPad8
                    _objEngine.Functions(8) = If(Me.BtnF8.IsChecked, OnOff.Off, OnOff.On)
                Case Key.Up, Key.Right
                    Me.SpeedKnob.StepUp()
                Case Key.Down, Key.Left
                    Me.SpeedKnob.StepDown()
                Case Else
                    e.Handled = False
            End Select
        End Sub

        Private Sub ShortCutKeysUp(sender As Object, e As KeyEventArgs)

            If Not Me.GroupBoxKnob.IsEnabled Then Exit Sub

            e.Handled = True

            Select Case e.Key
                Case Key.D2, Key.NumPad2
                    _objEngine.Functions(2) = OnOff.Off
                Case Else
                    e.Handled = False
            End Select

        End Sub


        Private Sub EmergencyStop()
            _objEngine.EmergencyStop()
            Me.SpeedKnob.Value = 0
            Me.Speed.Content = "0%"
        End Sub

        Private Sub SetKnobRampType(strKnobRampType As String)
            Select Case strKnobRampType
                Case "Linear"
                    Me.BtnRampTypeImg.Source = TryFindResource("IconAccel1")
                    Me.SpeedKnob.RampType = Knob.RampTypes.Linear
                Case "ExpLow"
                    Me.BtnRampTypeImg.Source = TryFindResource("IconAccel2")
                    Me.SpeedKnob.RampType = Knob.RampTypes.ExpLow
                Case "ExpHigh"
                    Me.BtnRampTypeImg.Source = TryFindResource("IconAccel3")
                    Me.SpeedKnob.RampType = Knob.RampTypes.ExpHigh
            End Select

            'save knob ramp type with the layout
            _objConfig.KnobRampType = strKnobRampType
        End Sub

        Private Sub ChangeDirection()
            Select Case _objEngine.Direction
                Case LocoDirection.Forward
                    _objEngine.Direction = LocoDirection.Reverse
                Case LocoDirection.Reverse
                    _objEngine.Direction = LocoDirection.Forward
            End Select
        End Sub

        Private Sub ChangeDirection(enuDirection As LocoDirection)
            If enuDirection <> _objEngine.Direction Then
                _objEngine.Direction = enuDirection
            End If
        End Sub

#End Region

#Region "Engine Event Handlers"

        Private Sub BindSlotResponse(enuResult As Engine.BindSlotResult)
            Select Case enuResult
                Case Engine.BindSlotResult.Success, Engine.BindSlotResult.SuccessSlotStolen
                    ConfigureTrottle()
                    RefreshSpeed(Nothing)
                    RefreshDirection()
                    RefreshFunction()
                Case Else
                    'do nothing on failure since a message is already posted in the events window                    
            End Select
        End Sub

        Private Sub UnbindSlotResponse(enuResult As Engine.UnbindSlotResult)
            Select Case enuResult
                Case Engine.UnbindSlotResult.Success, Engine.UnbindSlotResult.SuccessNoCfm
                    ConfigureTrottle()
                Case Else
                    'do nothing on failure since a message is already posted in the events window                    
            End Select
        End Sub

        Private Sub RefreshSpeed(sctClientToken As Guid)
            If Not _sctClientToken.Equals(sctClientToken) Then
                Me.SpeedKnob.Value = _objEngine.Speed
                Me.Speed.Content = Math.Round(_objEngine.Speed / (SpeedKnob.Maximum) * 100) & "%"
            End If
        End Sub

        Private Sub RefreshDirection()
            Select Case _objEngine.Direction
                Case LocoDirection.Forward
                    Me.SpeedKnob.KnobColor = Colors.SteelBlue
                    Me.ImgDir.Source = TryFindResource("IconReverse")
                    Me.BtnDir.ToolTip = "Reverse (Space or End)"
                Case LocoDirection.Reverse
                    Me.SpeedKnob.KnobColor = Colors.Goldenrod
                    Me.ImgDir.Source = TryFindResource("IconForward")
                    Me.BtnDir.ToolTip = "Forward (Space or Home)"
            End Select
        End Sub

        Private Sub RefreshFunction()
            Me.BtnF0.IsChecked = _objEngine.Functions(0)
            Me.BtnF1.IsChecked = _objEngine.Functions(1)
            Me.BtnF2.IsChecked = _objEngine.Functions(2)
            Me.BtnF3.IsChecked = _objEngine.Functions(3)
            Me.BtnF4.IsChecked = _objEngine.Functions(4)

            Me.BtnF5.IsChecked = _objEngine.Functions(5)
            Me.BtnF6.IsChecked = _objEngine.Functions(6)
            Me.BtnF7.IsChecked = _objEngine.Functions(7)
            Me.BtnF8.IsChecked = _objEngine.Functions(8)

            Me.BtnF9.IsChecked = _objEngine.Functions(9)
            Me.BtnF10.IsChecked = _objEngine.Functions(10)
            Me.BtnF11.IsChecked = _objEngine.Functions(11)
            Me.BtnF12.IsChecked = _objEngine.Functions(12)

            Me.BtnF13.IsChecked = _objEngine.Functions(13)
            Me.BtnF14.IsChecked = _objEngine.Functions(14)
            Me.BtnF15.IsChecked = _objEngine.Functions(15)
            Me.BtnF16.IsChecked = _objEngine.Functions(16)
            Me.BtnF17.IsChecked = _objEngine.Functions(17)
            Me.BtnF18.IsChecked = _objEngine.Functions(18)
            Me.BtnF19.IsChecked = _objEngine.Functions(19)
            Me.BtnF20.IsChecked = _objEngine.Functions(20)

            Me.BtnF21.IsChecked = _objEngine.Functions(21)
            Me.BtnF22.IsChecked = _objEngine.Functions(22)
            Me.BtnF23.IsChecked = _objEngine.Functions(23)
            Me.BtnF24.IsChecked = _objEngine.Functions(24)
            Me.BtnF25.IsChecked = _objEngine.Functions(25)
            Me.BtnF26.IsChecked = _objEngine.Functions(26)
            Me.BtnF27.IsChecked = _objEngine.Functions(27)
            Me.BtnF28.IsChecked = _objEngine.Functions(28)
        End Sub

#End Region

        Public Sub Dispose()
            'remove event handlers
            RemoveHandler CtcService.ObjectChanged, AddressOf ObjectChanged
            RemoveHandler DirectCast(Me.Parent, Control).PreviewKeyDown, AddressOf ShortCutKeysDown
            RemoveHandler DirectCast(Me.Parent, Control).PreviewKeyUp, AddressOf ShortCutKeysUp

            RemoveHandler _objEngine.BindSlotResponse, AddressOf BindSlotResponse
            RemoveHandler _objEngine.UnbindSlotResponse, AddressOf UnbindSlotResponse
            RemoveHandler _objEngine.SpeedChanged, AddressOf RefreshSpeed
            RemoveHandler _objEngine.DirectionChanged, AddressOf RefreshDirection
            RemoveHandler _objEngine.FunctionChanged, AddressOf RefreshFunction
        End Sub

    End Class

End Namespace
