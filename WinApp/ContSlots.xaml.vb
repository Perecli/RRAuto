Imports System.ComponentModel
Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet

Namespace CustomControls

    Public Class ContSlots

        Private _bytSlotCount As Byte = 0   'number of slots that have been populated in the list

        Friend Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            InitContextMenus()

            AddHandler CtcService.LoconetService.RxPacketOnSyncThread, AddressOf RxPacket

        End Sub

        Private Async Sub Me_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded

            'populate the list on first load assuming the LoconetService is started
            If _bytSlotCount = 0 Then
                Dim objPkReqSlotData As New PkReqSlotData(127)
                Await CtcService.LoconetService.TxPacket(objPkReqSlotData)
                Select Case True
                    Case objPkReqSlotData.RxPacket Is Nothing
                        'no response; Loconet must be disconnected

                    Case TypeOf objPkReqSlotData.RxPacket Is PkComStatOps
                        Dim objComStatOps As PkComStatOps = objPkReqSlotData.RxPacket

                        'find out how many slots should be shown based on command station
                        Dim enuOps() As SwitchState = objComStatOps.CommandStationOps
                        Select Case True
                            Case (enuOps(59) = SwitchState.Thrown And enuOps(60) = SwitchState.Closed And enuOps(61) = SwitchState.Closed) Or
                                 (enuOps(59) = SwitchState.Closed And enuOps(60) = SwitchState.Thrown And enuOps(61) = SwitchState.Closed)
                                'first line is for DCS100
                                'second line is for DCS200
                                'they both have the same number of slots

                                Select Case enuOps(44)
                                    Case SwitchState.Thrown
                                        _bytSlotCount = 22
                                    Case SwitchState.Closed
                                        _bytSlotCount = 120
                                End Select

                            Case enuOps(60) = SwitchState.Thrown And enuOps(61) = SwitchState.Thrown
                                'DB150
                                _bytSlotCount = 22

                            Case (enuOps(59) = SwitchState.Thrown And enuOps(60) = SwitchState.Closed And enuOps(61) = SwitchState.Thrown) Or
                                 (enuOps(59) = SwitchState.Closed And enuOps(60) = SwitchState.Closed And enuOps(61) = SwitchState.Closed)
                                'DCS50
                                _bytSlotCount = 10

                            Case enuOps(59) = SwitchState.Closed And enuOps(60) = SwitchState.Closed And enuOps(61) = SwitchState.Thrown
                                'DCS51
                                _bytSlotCount = 20

                            Case Else
                                MessageBox.Show(My.Application.MainWindow, "Unknown command station.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)

                        End Select

                    Case TypeOf objPkReqSlotData.RxPacket Is PkLongAck  'returned by Intellibox because it doesn't support slot 127
                        objPkReqSlotData = New PkReqSlotData(0)
                        Await CtcService.LoconetService.TxPacket(objPkReqSlotData)
                        Select Case True
                            Case objPkReqSlotData.RxPacket Is Nothing
                                'no response; Loconet must be disconnected

                            Case TypeOf objPkReqSlotData.RxPacket Is PkRdWrSlotData
                                Dim objRdWrSlotData As PkRdWrSlotData = objPkReqSlotData.RxPacket
                                If objRdWrSlotData.IsIntellibox Then
                                    _bytSlotCount = 119
                                Else
                                    MessageBox.Show(My.Application.MainWindow, "Unknown command station.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                                End If

                        End Select

                End Select

                'set up number of slot rows
                Me.BeginInit()
                For bytSlot As Byte = 1 To _bytSlotCount
                    Me.Items.Add(New ListItem(bytSlot))
                Next
                Me.EndInit()

                PollSlots(Me.Items)
            End If

        End Sub

#Region "Context Menu"

        Private Sub InitContextMenus()
            Me.ContextMenu = New ContextMenu()

            AddMenuItem("Select All", Nothing, New RoutedEventHandler(AddressOf SelectAll_Click))
            AddMenuItem("Re-poll selected", Nothing, New RoutedEventHandler(AddressOf PollSlot_Click))
            AddMenuItem("Purge selected", Nothing, New RoutedEventHandler(AddressOf PurgeSlot_Click))
        End Sub

        Private Sub AddMenuItem(strText As String, objImageResKey As String, delHandler As RoutedEventHandler)
            Dim objMenuItem As New MenuItem()
            With objMenuItem
                .Header = strText
                If objImageResKey <> Nothing Then
                    Dim objImage As New Image
                    objImage.Width = 16
                    objImage.Height = 16
                    objImage.Source = TryFindResource(objImageResKey)
                    .Icon = objImage
                End If
                If delHandler IsNot Nothing Then AddHandler .Click, delHandler
            End With
            Me.ContextMenu.Items.Add(objMenuItem)
        End Sub

        Private Sub SelectAll_Click(sender As Object, e As RoutedEventArgs)
            Me.SelectAll()
        End Sub

        Private Sub PollSlot_Click(sender As Object, e As RoutedEventArgs)
            If Me.SelectedItems.Count = 0 Then
                MessageBox.Show(My.Application.MainWindow, "No rows have been selected.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            Else
                PollSlots(Me.SelectedItems)
            End If
        End Sub

        Private Sub PurgeSlot_Click(sender As Object, e As RoutedEventArgs)
            If Me.SelectedItems.Count = 0 Then
                MessageBox.Show(My.Application.MainWindow, "No rows have been selected.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            Else
                For Each objItem As ListItem In Me.SelectedItems
                    CtcService.LoconetService.TxPacket(New PkSetSlotSpeed(objItem.Slot, 0))  'todo: stop other threads operating this slot's speed
                    CtcService.LoconetService.TxPacket(New PkSlotStatus(objItem.Slot, SlotActivity.Idle, ConsistType.NoConsist, SpeedSteps.DCC_128_SS))
                    CtcService.LoconetService.TxPacket(New PkSlotMove(objItem.Slot, 0))
                Next
            End If
        End Sub

#End Region

        Private Sub RxPacket(objPacket As Packet)
            Select Case True
                Case TypeOf objPacket Is PkLocoSlot
                    Dim objLocoSlot As PkLocoSlot = objPacket
                    If objLocoSlot.Slot <= _bytSlotCount Then
                        Dim objItem As ListItem = Me.Items(objLocoSlot.Slot - 1)
                        objItem.Address = objLocoSlot.Address
                        objItem.Speed = objLocoSlot.Speed
                        objItem.Direction = DirectionResource(objLocoSlot.Direction)
                        Dim enuaFuncs0to4() As OnOff = objLocoSlot.Functions0to4
                        objItem.Func0 = FunctionResource(enuaFuncs0to4(0))
                        objItem.Func1 = FunctionResource(enuaFuncs0to4(1))
                        objItem.Func2 = FunctionResource(enuaFuncs0to4(2))
                        objItem.Func3 = FunctionResource(enuaFuncs0to4(3))
                        objItem.Func4 = FunctionResource(enuaFuncs0to4(4))
                        Dim enuaFuncs5to8() As OnOff = objLocoSlot.Functions5to8
                        objItem.Func5 = FunctionResource(enuaFuncs5to8(0))
                        objItem.Func6 = FunctionResource(enuaFuncs5to8(1))
                        objItem.Func7 = FunctionResource(enuaFuncs5to8(2))
                        objItem.Func8 = FunctionResource(enuaFuncs5to8(3))
                        objItem.Activity = objLocoSlot.Activity.ToString
                        objItem.SpeedSteps = objLocoSlot.SpeedSteps.ToString.Replace("_", " ")
                        objItem.Consist = objLocoSlot.ConsistType.ToString
                    End If

                Case TypeOf objPacket Is PkSetSlotSpeed
                    Dim objSlotSpeed As PkSetSlotSpeed = objPacket
                    If objSlotSpeed.Slot > 0 And objSlotSpeed.Slot <= _bytSlotCount Then
                        Dim objItem As ListItem = Me.Items(objSlotSpeed.Slot - 1)
                        objItem.Speed = objSlotSpeed.Speed
                    End If

                Case TypeOf objPacket Is PkSetSlotDirFunc
                    Dim objSlotDirFunc As PkSetSlotDirFunc = objPacket
                    If objSlotDirFunc.Slot > 0 And objSlotDirFunc.Slot <= _bytSlotCount Then
                        Dim objItem As ListItem = Me.Items(objSlotDirFunc.Slot - 1)
                        objItem.Direction = DirectionResource(objSlotDirFunc.Direction)
                        Dim enuaFuncs() As OnOff = objSlotDirFunc.Functions
                        objItem.Func0 = FunctionResource(enuaFuncs(0))
                        objItem.Func1 = FunctionResource(enuaFuncs(1))
                        objItem.Func2 = FunctionResource(enuaFuncs(2))
                        objItem.Func3 = FunctionResource(enuaFuncs(3))
                        objItem.Func4 = FunctionResource(enuaFuncs(4))
                    End If

                Case TypeOf objPacket Is PkSetSlotFunc5to8
                    Dim objSlotFunc5to8 As PkSetSlotFunc5to8 = objPacket
                    If objSlotFunc5to8.Slot > 0 And objSlotFunc5to8.Slot <= _bytSlotCount Then
                        Dim objItem As ListItem = Me.Items(objSlotFunc5to8.Slot - 1)
                        Dim enuaFuncs() As OnOff = objSlotFunc5to8.Functions
                        objItem.Func5 = FunctionResource(enuaFuncs(0))
                        objItem.Func6 = FunctionResource(enuaFuncs(1))
                        objItem.Func7 = FunctionResource(enuaFuncs(2))
                        objItem.Func8 = FunctionResource(enuaFuncs(3))
                    End If

                Case TypeOf objPacket Is PkSlotStatus
                    Dim objStatus As PkSlotStatus = objPacket
                    If objStatus.Slot > 0 And objStatus.Slot <= _bytSlotCount Then
                        Dim objItem As ListItem = Me.Items(objStatus.Slot - 1)
                        objItem.Activity = objStatus.Activity.ToString
                        objItem.SpeedSteps = objStatus.SpeedSteps.ToString.Replace("_", " ")
                        objItem.Consist = objStatus.ConsistType.ToString
                    End If

            End Select
        End Sub

        Private Function DirectionResource(enuDirection As LocoDirection) As DrawingImage
            Return If(enuDirection = LocoDirection.Forward, TryFindResource("IconForward"), TryFindResource("IconReverse"))
        End Function

        Private Function FunctionResource(enuFunction As OnOff) As DrawingImage
            Return If(enuFunction = OnOff.On, TryFindResource("IconCheckBox1"), TryFindResource("IconCheckBox0"))
        End Function

        Private Sub PollSlots(objItems As IList)
            For Each objItem As ListItem In objItems
                CtcService.LoconetService.TxPacket(New PkReqSlotData(objItem.Slot))
            Next
        End Sub

        Private Class ListItem
            Implements INotifyPropertyChanged
            Private _bytSlot As Byte
            Private _strAddress As String
            Private _strSpeed As String
            Private _objDirection As DrawingImage
            Private _objFunc0 As DrawingImage
            Private _objFunc1 As DrawingImage
            Private _objFunc2 As DrawingImage
            Private _objFunc3 As DrawingImage
            Private _objFunc4 As DrawingImage
            Private _objFunc5 As DrawingImage
            Private _objFunc6 As DrawingImage
            Private _objFunc7 As DrawingImage
            Private _objFunc8 As DrawingImage
            Private _strActivity As String
            Private _strSpeedSteps As String
            Private _strConsist As String

            Public Event PropertyChanged(sender As Object, e As PropertyChangedEventArgs) Implements INotifyPropertyChanged.PropertyChanged

            Private Sub NotifyPropertyChanged(strPropertyName As String)
                RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(strPropertyName))
            End Sub

            Public Sub New(bytSlot As Byte)
                _bytSlot = bytSlot
            End Sub

            Public Property Slot() As Byte
                Get
                    Return _bytSlot
                End Get
                Set(value As Byte)
                    If value <> _bytSlot Then
                        _bytSlot = value
                        NotifyPropertyChanged("Slot")
                    End If
                End Set
            End Property

            Public Property Address() As String
                Get
                    Return _strAddress
                End Get
                Set(value As String)
                    If value <> _strAddress Then
                        _strAddress = value
                        NotifyPropertyChanged("Address")
                    End If
                End Set
            End Property

            Public Property Speed() As String
                Get
                    Return _strSpeed
                End Get
                Set(value As String)
                    If value <> _strSpeed Then
                        _strSpeed = value
                        NotifyPropertyChanged("Speed")
                    End If
                End Set
            End Property

            Public Property Direction() As DrawingImage
                Get
                    Return _objDirection
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objDirection) Then
                        _objDirection = value
                        NotifyPropertyChanged("Direction")
                    End If
                End Set
            End Property

            Public Property Func0() As DrawingImage
                Get
                    Return _objFunc0
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc0) Then
                        _objFunc0 = value
                        NotifyPropertyChanged("Func0")
                    End If
                End Set
            End Property

            Public Property Func1() As DrawingImage
                Get
                    Return _objFunc1
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc1) Then
                        _objFunc1 = value
                        NotifyPropertyChanged("Func1")
                    End If
                End Set
            End Property

            Public Property Func2() As DrawingImage
                Get
                    Return _objFunc2
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc2) Then
                        _objFunc2 = value
                        NotifyPropertyChanged("Func2")
                    End If
                End Set
            End Property

            Public Property Func3() As DrawingImage
                Get
                    Return _objFunc3
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc3) Then
                        _objFunc3 = value
                        NotifyPropertyChanged("Func3")
                    End If
                End Set
            End Property

            Public Property Func4() As DrawingImage
                Get
                    Return _objFunc4
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc4) Then
                        _objFunc4 = value
                        NotifyPropertyChanged("Func4")
                    End If
                End Set
            End Property

            Public Property Func5() As DrawingImage
                Get
                    Return _objFunc5
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc5) Then
                        _objFunc5 = value
                        NotifyPropertyChanged("Func5")
                    End If
                End Set
            End Property

            Public Property Func6() As DrawingImage
                Get
                    Return _objFunc6
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc6) Then
                        _objFunc6 = value
                        NotifyPropertyChanged("Func6")
                    End If
                End Set
            End Property

            Public Property Func7() As DrawingImage
                Get
                    Return _objFunc7
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc7) Then
                        _objFunc7 = value
                        NotifyPropertyChanged("Func7")
                    End If
                End Set
            End Property

            Public Property Func8() As DrawingImage
                Get
                    Return _objFunc8
                End Get
                Set(value As DrawingImage)
                    If Not value.Equals(_objFunc8) Then
                        _objFunc8 = value
                        NotifyPropertyChanged("Func8")
                    End If
                End Set
            End Property

            Public Property Activity() As String
                Get
                    Return _strActivity
                End Get
                Set(value As String)
                    If value <> _strActivity Then
                        _strActivity = value
                        NotifyPropertyChanged("Activity")
                    End If
                End Set
            End Property

            Public Property SpeedSteps() As String
                Get
                    Return _strSpeedSteps
                End Get
                Set(value As String)
                    If value <> _strSpeedSteps Then
                        _strSpeedSteps = value
                        NotifyPropertyChanged("SpeedSteps")
                    End If
                End Set
            End Property

            Public Property Consist() As String
                Get
                    Return _strConsist
                End Get
                Set(value As String)
                    If value <> _strConsist Then
                        _strConsist = value
                        NotifyPropertyChanged("Consist")
                    End If
                End Set
            End Property

        End Class

    End Class

End Namespace
