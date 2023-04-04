Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC

Public Class DrawHelper

    'colors found in the Brushes ResourceDictionary that will be replaced
    Private Shared C_ActiveColor As Color = "#FFAAAAAA".ToColor
    Private Shared C_PassiveColor As Color = "#FFDDDDDD".ToColor
    Private Shared C_StateColor As Color = "#FFFFFF00".ToColor
    Private Shared C_SelfConnColor As Color = "#FFFF0000".ToColor
    Private Shared C_OtherConnColor As Color = "#FF0000FF".ToColor
    Private Shared C_SelectorColor As Color = "#FFFFFFFF".ToColor

    ''' <summary>
    ''' Used to determine which connections are possible for a certain track type and out of those which are valid for rendering 
    ''' and how they should be rendered in respect to the current layout.
    ''' </summary>
    Public Class TrackConnDefinitions
        Implements IEnumerable
        Private _objConnections As New List(Of Connection)

        Public Enum Direction As Byte
            N = 1
            NE
            E
            SE
            S
            SW
            W
            NW
        End Enum

        Public Enum ConnType As Byte
            Active
            Passive
        End Enum

        Public Class Connection
            Friend Direction As Direction

            Friend SelfConnType As ConnType
            Friend OtherConnType As ConnType

            Friend OtherTrack As Track

            Friend Sub New(enuSelfConnType As ConnType,  enuDirection As Direction)
                SelfConnType = enuSelfConnType
                Direction = enuDirection
            End Sub
        End Class

        Public Sub New(objTrack As Track,  blnValidate As Boolean)
            GenerateConnDefinitions(objTrack)

            If blnValidate Then
                ValidateConnection(objTrack, Direction.N, objTrack.Location.Offset(0, -1))
                ValidateConnection(objTrack, Direction.NE, objTrack.Location.Offset(1, -1))
                ValidateConnection(objTrack, Direction.E, objTrack.Location.Offset(1, 0))
                ValidateConnection(objTrack, Direction.SE, objTrack.Location.Offset(1, 1))
                ValidateConnection(objTrack, Direction.S, objTrack.Location.Offset(0, 1))
                ValidateConnection(objTrack, Direction.SW, objTrack.Location.Offset(-1, 1))
                ValidateConnection(objTrack, Direction.W, objTrack.Location.Offset(-1, 0))
                ValidateConnection(objTrack, Direction.NW, objTrack.Location.Offset(-1, -1))
            End If
        End Sub

        Public Sub GenerateConnDefinitions(objTrack As Track)
            If objTrack Is Nothing Then Exit Sub

            Dim bytTrackState As Byte
            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

            If My.Application.AppMode = AppMode.Edit AndAlso TypeOf objSbContextObj Is Route AndAlso DirectCast(objSbContextObj, Route).RouteElements.Contains(objTrack) Then
                'when routes are selected for edit, use the states of route definition rather than the current state of the track
                bytTrackState = CType(objSbContextObj, Route).RouteElements(objTrack).State
            Else
                If objTrack.States IsNot Nothing Then bytTrackState = objTrack.State.Value
            End If

            With _objConnections
                Select Case objTrack.Type
                    Case Track.TrackType.TrackStraight
                        .Add(New Connection(ConnType.Active, Direction.N))
                        .Add(New Connection(ConnType.Active, Direction.S))

                    Case Track.TrackType.TrackCurved1
                        .Add(New Connection(ConnType.Active, Direction.N))
                        .Add(New Connection(ConnType.Active, Direction.SE))

                    Case Track.TrackType.TrackCurved2
                        .Add(New Connection(ConnType.Active, Direction.N))
                        .Add(New Connection(ConnType.Active, Direction.E))

                    Case Track.TrackType.TrackCrossing1
                        .Add(New Connection(ConnType.Active, Direction.N))
                        .Add(New Connection(ConnType.Active, Direction.E))
                        .Add(New Connection(ConnType.Active, Direction.S))
                        .Add(New Connection(ConnType.Active, Direction.W))

                    Case Track.TrackType.TrackCrossing2
                        .Add(New Connection(ConnType.Active, Direction.N))
                        .Add(New Connection(ConnType.Active, Direction.NE))
                        .Add(New Connection(ConnType.Active, Direction.S))
                        .Add(New Connection(ConnType.Active, Direction.SW))

                    Case Track.TrackType.TrackTerminator
                        .Add(New Connection(ConnType.Active, Direction.N))

                    Case Track.TrackType.TurnoutLeft
                        Select Case bytTrackState
                            Case Track.TrackState.Straight
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NW))
                            Case Track.TrackState.Diverging
                                .Add(New Connection(ConnType.Active, Direction.NW))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                        End Select

                    Case Track.TrackType.TurnoutRight
                        Select Case bytTrackState
                            Case Track.TrackState.Straight
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                            Case Track.TrackState.Diverging
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                        End Select

                    Case Track.TrackType.TurnoutSingleSlip
                        Select Case bytTrackState
                            Case Track.TrackState.Cross1
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                                .Add(New Connection(ConnType.Passive, Direction.SW))
                            Case Track.TrackState.Cross2
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.SW))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                                .Add(New Connection(ConnType.Passive, Direction.S))
                            Case Track.TrackState.Diverging
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                                .Add(New Connection(ConnType.Passive, Direction.SW))
                        End Select


                    Case Track.TrackType.TurnoutDoubleSlip
                        Select Case bytTrackState
                            Case Track.TrackState.Cross1
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                                .Add(New Connection(ConnType.Passive, Direction.SW))
                            Case Track.TrackState.Cross2
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.SW))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                                .Add(New Connection(ConnType.Passive, Direction.S))
                            Case Track.TrackState.Diverging1
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                                .Add(New Connection(ConnType.Passive, Direction.SW))
                            Case Track.TrackState.Diverging2
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.SW))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                                .Add(New Connection(ConnType.Passive, Direction.S))
                        End Select

                    Case Track.TrackType.TurnoutThreeWay
                        Select Case bytTrackState
                            Case Track.TrackState.Straight
                                .Add(New Connection(ConnType.Active, Direction.N))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NW))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                            Case Track.TrackState.Diverging1
                                .Add(New Connection(ConnType.Active, Direction.NW))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                                .Add(New Connection(ConnType.Passive, Direction.NE))
                            Case Track.TrackState.Diverging2
                                .Add(New Connection(ConnType.Active, Direction.NE))
                                .Add(New Connection(ConnType.Active, Direction.S))
                                .Add(New Connection(ConnType.Passive, Direction.NW))
                                .Add(New Connection(ConnType.Passive, Direction.N))
                        End Select

                End Select
            End With

            'rotate connection definitions based on orientation
            For Each intItem As Connection In _objConnections
                intItem.Direction += objTrack.Orientation - 1
                If intItem.Direction > 8 Then intItem.Direction -= 8
            Next
        End Sub

        Public Sub ValidateConnection(objSelfTrack As Track,  objDirection As Direction,  sctOtherLocation As Location)
            Dim objSelfConnection As Connection = Me.Item(objDirection)
            If objSelfConnection Is Nothing Then Exit Sub 'our track type does not have a defined connection towards the given neighbor cell

            Dim objOtherTrack As Track = CtcService.Tracks(sctOtherLocation).LastOrDefault()
            If objOtherTrack Is Nothing Then
                'no neighbor track exists in the given direction
                _objConnections.Remove(objSelfConnection)
            Else
                Dim objOtherConnections As New TrackConnDefinitions(objOtherTrack, False)
                Dim objOtherConnection As Connection = objOtherConnections.Item(DirectionComplement(objDirection))

                If objOtherConnection IsNot Nothing AndAlso objSelfTrack.Block IsNot Nothing AndAlso objSelfTrack.Block Is objOtherTrack.Block Then
                    'valid connection found so get more data about neighbor
                    objSelfConnection.OtherConnType = objOtherConnection.SelfConnType
                    objSelfConnection.OtherTrack = objOtherTrack
                Else
                    'no complement neighbor connection found or the two tracks being matched are not part of the same block
                    _objConnections.Remove(objSelfConnection)
                End If
            End If

        End Sub

        Public Function DirectionComplement(sctDirection As Direction) As Direction
            sctDirection += 4
            If sctDirection > 8 Then
                Return sctDirection - 8
            Else
                Return sctDirection
            End If
        End Function

        Public ReadOnly Property Item(enuDirection As Direction) As Connection
            Get
                For Each objConnection As Connection In _objConnections
                    If objConnection.Direction = enuDirection Then Return objConnection
                Next
            End Get
        End Property

        Public Function GetEnumerator() As System.Collections.IEnumerator Implements System.Collections.IEnumerable.GetEnumerator
            'allows For...Each usage
            Return _objConnections.GetEnumerator()
        End Function

    End Class

#Region "Signal Looks"
    'to add a new signal look just add a new SignalLookKey, a new "Select Case" relationship in the GetSignalLook(), and a new signal brush

    'note: this enum gets serialized into the Signal object as a string (by name not value)
    'this was done so changes in the enum byte values caused by new item insertions between existing ones 
    'would not break compatibility as long as the existing enum names stay the same
    Public Enum SignalLookKey As Byte
        'starts at 1 so we can pass "Nothing" to methods that accept the SignalLookKey type parameter; "Nothing" translates to 0 which is outside the valid enumerations
        x2_Gen_Sing = 1
        x2_Gen_Mult
        x2_DE_Block
        x2_DE_Shunt1
        x2_DE_Shunt2
        x2_DE_Wait
        x2_UK_Block
        x2_UK_Shunt
        x3_Gen_Sing
        x3_Gen_Mult
        x3_Automotive
        x3_DE_Entry
        x3_UK_Block
        x4_Gen_Sing
        x4_Gen_Mult
        x4_DE_Depart
        x4_DE_Distant
        x4_UK_Block
        x5_Gen_Sing
    End Enum

    Public Structure SignalLook
        Public Config As Signal.SignalConfig  'tie-in with the CTC API
        Public Name As String                 'tie-in to the UI
        Public BrushKey As String             'tie-in to the Expression Sesign

        Public Sub New(enuConfig As Signal.SignalConfig,  strName As String,  strBrushKey As String)
            Me.Config = enuConfig
            Me.Name = strName
            Me.BrushKey = strBrushKey
        End Sub
    End Structure

    Public Shared Function GetSignalLook(strSignalLookKey As String) As SignalLook

        Dim enuSignalLookKey As SignalLookKey
        If [Enum].TryParse(strSignalLookKey, enuSignalLookKey) Then Return GetSignalLook(enuSignalLookKey)

    End Function

    Public Shared Function GetSignalLook(enuSignalLookKey As SignalLookKey) As SignalLook

        Select Case enuSignalLookKey
            Case SignalLookKey.x2_Gen_Sing
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 Generic Single", "xn_Gen_Sing")
            Case SignalLookKey.x2_Gen_Mult
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 Generic Multiple", "x2_Gen_Mult")
            Case SignalLookKey.x2_DE_Block
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 DE Block", "x2_DE_Block")
            Case SignalLookKey.x2_DE_Shunt1
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 DE Shunting1", "x2_DE_Shunt1")
            Case SignalLookKey.x2_DE_Shunt2
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 DE Shunting2", "x2_DE_Shunt2")
            Case SignalLookKey.x2_DE_Wait
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 DE Wait", "x2_DE_Wait")
            Case SignalLookKey.x2_UK_Block
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 UK Block", "x2_UK_Block")
            Case SignalLookKey.x2_UK_Shunt
                Return New SignalLook(Signal.SignalConfig.StopClear, "x2 UK Shunting", "x2_UK_Shunt")
            Case SignalLookKey.x3_Gen_Sing
                Return New SignalLook(Signal.SignalConfig.StopClearCaution, "x3 Generic Single", "xn_Gen_Sing")
            Case SignalLookKey.x3_Gen_Mult
                Return New SignalLook(Signal.SignalConfig.StopClearCaution, "x3 Generic Multiple", "x3_Gen_Mult")
            Case SignalLookKey.x3_Automotive
                Return New SignalLook(Signal.SignalConfig.StopClearCaution, "x3 Automotive", "x3_Automotive")
            Case SignalLookKey.x3_DE_Entry
                Return New SignalLook(Signal.SignalConfig.StopClearCaution, "x3 DE Entry", "x3_DE_Entry")
            Case SignalLookKey.x3_UK_Block
                Return New SignalLook(Signal.SignalConfig.StopClearCaution, "x3 UK Block", "x3_UK_Block")
            Case SignalLookKey.x4_Gen_Sing
                Return New SignalLook(Signal.SignalConfig.StopClearCautionShunt, "x4 Generic Single", "xn_Gen_Sing")
            Case SignalLookKey.x4_Gen_Mult
                Return New SignalLook(Signal.SignalConfig.StopClearCautionShunt, "x4 Generic Multiple", "x4_Gen_Mult")
            Case SignalLookKey.x4_DE_Depart
                Return New SignalLook(Signal.SignalConfig.StopClearCautionShunt, "x4 DE Departure", "x4_DE_Depart")
            Case SignalLookKey.x4_DE_Distant
                Return New SignalLook(Signal.SignalConfig.DarkStopClearCaution, "x4 DE Distant", "x4_DE_Distant")
            Case SignalLookKey.x4_UK_Block
                Return New SignalLook(Signal.SignalConfig.StopClearCautionCaution2, "x4 UK Block", "x4_UK_Block")
            Case SignalLookKey.x5_Gen_Sing
                Return New SignalLook(Signal.SignalConfig.DarkStopClearCautionShunt, "x5 Generic Single", "xn_Gen_Sing")
        End Select

    End Function

#End Region

#Region "Get DrawingGroup"

    Public Shared Function GetTrackDrawing(objTrack As Track, sctTrackDerivedColors As TrackDerivedColors) As DrawingGroup
        If objTrack Is Nothing Then Exit Function

        Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

        'get drawing resource
        Dim strResourceName As String = String.Format("{0}_{1}", objTrack.Type, ((objTrack.Orientation - 1) And 1) + 1)
        If objTrack.IsTurnout Then
            If My.Application.AppMode = AppMode.Edit AndAlso TypeOf objSbContextObj Is Route AndAlso DirectCast(objSbContextObj, Route).RouteElements.Contains(objTrack) Then
                'when routes are selected for edit, show state of route definition rather than the current state of the track
                strResourceName &= "_" & DirectCast(objSbContextObj, Route).RouteElements(objTrack).State.ToString
            Else
                strResourceName &= "_" & DirectCast(objTrack.State.Value, Track.TrackState).ToString
            End If
        End If
        Dim objDrawing As DrawingGroup = My.Application.TryFindResource(strResourceName)
        If objDrawing Is Nothing Then
            objDrawing = My.Application.TryFindResource("UndefinedDrawing")
            If objDrawing IsNot Nothing Then Return objDrawing.Clone
        Else
            objDrawing = objDrawing.Clone

            'rotate drawing
            Select Case objTrack.Orientation
                Case 3, 4
                    objDrawing.ApplyTransform(New RotateTransform(90))
                Case 5, 6
                    objDrawing.ApplyTransform(New RotateTransform(180))
                Case 7, 8
                    objDrawing.ApplyTransform(New RotateTransform(270))
            End Select

            'substitute color fills
            For Each objSubDrawing As GeometryDrawing In objDrawing.Children
                Select Case DirectCast(objSubDrawing.Brush, SolidColorBrush).Color
                    Case C_ActiveColor
                        objSubDrawing.Brush = New SolidColorBrush(sctTrackDerivedColors.ActiveColor)

                    Case C_PassiveColor
                        objSubDrawing.Brush = New SolidColorBrush(sctTrackDerivedColors.PassiveColor)

                    Case C_StateColor
                        objSubDrawing.Brush = New SolidColorBrush(sctTrackDerivedColors.StateColor)

                End Select
            Next

            Return objDrawing
        End If

    End Function

    Public Shared Function GetTrackConnDrawing(objConnection As TrackConnDefinitions.Connection, sctSelfTrackDerivedColors As TrackDerivedColors, sctOtherTrackDerivedColors As TrackDerivedColors) As DrawingGroup

        'get drawing resource
        Dim objDrawing As DrawingGroup
        Select Case objConnection.Direction
            Case TrackConnDefinitions.Direction.N, TrackConnDefinitions.Direction.E, TrackConnDefinitions.Direction.S, TrackConnDefinitions.Direction.W
                objDrawing = My.Application.TryFindResource("VerticalConnection")
            Case Else
                objDrawing = My.Application.TryFindResource("DiagonalConnection")
        End Select
        If objDrawing Is Nothing Then Exit Function
        objDrawing = objDrawing.Clone

        'rotate drawing
        Select Case objConnection.Direction
            Case TrackConnDefinitions.Direction.E, TrackConnDefinitions.Direction.SE
                objDrawing.ApplyTransform(New RotateTransform(90))
            Case TrackConnDefinitions.Direction.S, TrackConnDefinitions.Direction.SW
                objDrawing.ApplyTransform(New RotateTransform(180))
            Case TrackConnDefinitions.Direction.W, TrackConnDefinitions.Direction.NW
                objDrawing.ApplyTransform(New RotateTransform(270))
        End Select

        'substitute color fills
        For Each objSubDrawing As GeometryDrawing In objDrawing.Children
            If TypeOf objSubDrawing.Brush Is LinearGradientBrush Then
                For Each objGradientStop As GradientStop In DirectCast(objSubDrawing.Brush, LinearGradientBrush).GradientStops
                    Select Case objGradientStop.Color
                        Case C_SelfConnColor
                            Select Case objConnection.SelfConnType
                                Case TrackConnDefinitions.ConnType.Active
                                    objGradientStop.Color = sctSelfTrackDerivedColors.ActiveColor
                                Case TrackConnDefinitions.ConnType.Passive
                                    objGradientStop.Color = sctSelfTrackDerivedColors.PassiveColor
                            End Select

                        Case C_OtherConnColor
                            Select Case objConnection.OtherConnType
                                Case TrackConnDefinitions.ConnType.Active
                                    objGradientStop.Color = sctOtherTrackDerivedColors.ActiveColor
                                Case TrackConnDefinitions.ConnType.Passive
                                    objGradientStop.Color = sctOtherTrackDerivedColors.PassiveColor
                            End Select

                    End Select
                Next
            End If
        Next

        Return objDrawing

    End Function

    Public Shared Function GetSignalDrawing(objSignal As Signal) As DrawingGroup
        If objSignal Is Nothing Then Exit Function

        'get drawing resource
        Dim strResourceName As String = $"{GetSignalLook(objSignal.LookKey).BrushKey}_{CType(objSignal.Aspect.Value, Signal.SignalAspect).ToString}"
        Dim objDrawing As DrawingGroup = My.Application.TryFindResource(strResourceName)
        If objDrawing Is Nothing Then objDrawing = My.Application.TryFindResource("UndefinedDrawing")
        If objDrawing IsNot Nothing Then
            Return objDrawing.Clone
        End If

    End Function

    Public Shared Function GetLabelDrawing(objLabel As Label) As DrawingGroup
        If objLabel Is Nothing Then Exit Function

        Dim objVisual As New DrawingVisual

        Dim objBrushText As New SolidColorBrush(objLabel.TextColor.ToColor)
        objBrushText.Freeze()

        Dim objText As New FormattedText(If(objLabel.Text = Nothing, objLabel.Name, objLabel.Text), Globalization.CultureInfo.GetCultureInfo("en-us"), FlowDirection.LeftToRight, New Typeface(objLabel.FontFamily), 20, objBrushText)
        Dim sctTextBackRect As New Rect(New Size(objLabel.TextPadding * 2 + objText.WidthIncludingTrailingWhitespace, objLabel.TextPadding * 2 + objText.Height))

        Dim objBrushTextBack As New SolidColorBrush(objLabel.BackColor.ToColor)
        objBrushTextBack.Freeze()

        With objVisual.RenderOpen()
            .DrawRoundedRectangle(objBrushTextBack, DirectCast(Nothing, Pen), sctTextBackRect, objLabel.BackCornerRadius, objLabel.BackCornerRadius)
            .DrawText(objText, New Point(sctTextBackRect.X + objLabel.TextPadding, sctTextBackRect.Y + objLabel.TextPadding))
            .Close()
        End With

        Return objVisual.Drawing

    End Function

    Public Shared Function GetButtonDrawing(objButton As Button) As DrawingGroup
        If objButton Is Nothing Then Exit Function

        'get drawing resource
        Dim objDrawing As DrawingGroup = My.Application.TryFindResource(String.Format("Button{0}{1}", objButton.Look.ToString, objButton.State.ToString))
        If objDrawing Is Nothing Then objDrawing = My.Application.TryFindResource("UndefinedDrawing")
        If objDrawing IsNot Nothing Then
            Return objDrawing.Clone
        End If

    End Function

#End Region

#Region "Get DrawingImage"

    Public Shared Function GetTrackDrawingImage(objTrack As Track) As DrawingImage
        Dim sctTrackDerivedColors As TrackDerivedColors = GetTrackDerivedColors(objTrack, False, Colors.White)
        Dim objDrawing As DrawingGroup = GetTrackDrawing(objTrack, sctTrackDerivedColors)
        'RemoveCellFraming(objDrawing)  'did't like that the tracks don't remain proportinal
        Return New DrawingImage(objDrawing)
    End Function

    Public Shared Function GetSignalDrawingImage(objSignal As Signal) As DrawingImage
        Dim objDrawing As DrawingGroup = GetSignalDrawing(objSignal)
        'RemoveCellFraming(objDrawing)  'signals no longer have an oversized transparent background now that I went to visual hit test rather than entire cell hot spot
        Return New DrawingImage(objDrawing)
    End Function

    Public Shared Function GetButtonDrawingImage(objButton As Button) As DrawingImage
        Dim objDrawing As DrawingGroup = GetButtonDrawing(objButton)
        'RemoveCellFraming(objDrawing)  'signals no longer have an oversized transparent background now that I went to visual hit test rather than entire cell hot spot
        Return New DrawingImage(objDrawing)
    End Function

    'remove the transparent background used for positioning switchboard brushes like tracks/signals so that the brush can be rendered larger (i.e. in a context menu)
    Private Shared Sub RemoveCellFraming(objDrawing As DrawingGroup)
        If objDrawing Is Nothing Then Exit Sub

        Dim intIdx As Integer = 0
        Dim objDrawings As DrawingCollection = objDrawing.Children
        Do While intIdx < objDrawings.Count  'can't use For Each here because For Each collection can not be tampered with while iterating
            Dim objGeometryDrawing As GeometryDrawing = objDrawings(intIdx)
            If TypeOf objGeometryDrawing.Brush Is SolidColorBrush AndAlso DirectCast(objGeometryDrawing.Brush, SolidColorBrush).Color.A = 0 Then
                objDrawings.RemoveAt(intIdx)
            Else
                intIdx += 1
            End If
        Loop
    End Sub

#End Region

#Region "Color Functions"

    Public Structure TrackDerivedColors

        Friend Sub New(sctActiveColor As Color,  sctPassiveColor As Color,  sctStateColor As Color)
            ActiveColor = sctActiveColor
            PassiveColor = sctPassiveColor
            StateColor = sctStateColor
        End Sub

        Friend Property ActiveColor As Color

        Friend Property PassiveColor As Color

        Friend Property StateColor As Color

    End Structure

    Public Shared Function GetTrackDerivedColors(objTrack As Track,  blnObserveContextObj As Boolean,  sctBackColor As Color) As TrackDerivedColors
        Dim sctTrackDerivedColors As TrackDerivedColors

        Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

        'determine color fills
        If My.Application.AppMode = AppMode.Edit Then
            Select Case True
                Case blnObserveContextObj AndAlso objSbContextObj IsNot Nothing AndAlso
                     (objTrack Is objSbContextObj OrElse objTrack.Block Is objSbContextObj)
                    sctTrackDerivedColors.ActiveColor = My.Settings.SelectionColor
                    sctTrackDerivedColors.PassiveColor = My.Settings.SelectionColor

                Case blnObserveContextObj AndAlso
                     (TypeOf objSbContextObj Is Route AndAlso DirectCast(objSbContextObj, Route).RouteElements.Contains(objTrack))
                    sctTrackDerivedColors.ActiveColor = My.Settings.SelectionColor
                    sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.FreeBlockColor.SetAlpha(127))

                Case Else
                    sctTrackDerivedColors.ActiveColor = My.Settings.FreeBlockColor
                    sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.FreeBlockColor.SetAlpha(127))

            End Select
            sctTrackDerivedColors.StateColor = My.Settings.TurnoutSetColor
        Else
            If objTrack.Block Is Nothing Then
                sctTrackDerivedColors.ActiveColor = My.Settings.TrackNoBlockColor
                sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.TrackNoBlockColor.SetAlpha(127))
            Else
                Select Case True
                    Case Not objTrack.Block.Occupied AndAlso Not objTrack.Block.Reserved
                        sctTrackDerivedColors.ActiveColor = My.Settings.FreeBlockColor
                        sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.FreeBlockColor.SetAlpha(127))

                    Case Not objTrack.Block.Occupied AndAlso objTrack.Block.Reserved
                        sctTrackDerivedColors.ActiveColor = My.Settings.ResBlockColor
                        sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.FreeBlockColor.SetAlpha(127))

                    Case objTrack.Block.Occupied AndAlso objTrack.Block.Reserved
                        sctTrackDerivedColors.ActiveColor = My.Settings.ResOccBlockColor
                        sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.FreeBlockColor.SetAlpha(127))

                    Case objTrack.Block.Occupied AndAlso Not objTrack.Block.Reserved
                        sctTrackDerivedColors.ActiveColor = My.Settings.OccBlockColor
                        sctTrackDerivedColors.PassiveColor = sctBackColor.AlphaBlend(My.Settings.OccBlockColor.SetAlpha(127))
                End Select
            End If
            If objTrack.IsTurnout Then
                Select Case objTrack.States.Disposition
                    Case PkStatesList.StateDisposition.Pending
                        sctTrackDerivedColors.StateColor = My.Settings.TurnoutPendingColor

                    Case PkStatesList.StateDisposition.Locked
                        sctTrackDerivedColors.StateColor = My.Settings.TurnoutLockedColor

                    Case PkStatesList.StateDisposition.Set
                        sctTrackDerivedColors.StateColor = My.Settings.TurnoutSetColor

                End Select
            End If
        End If

        Return sctTrackDerivedColors

    End Function

#End Region

End Class
