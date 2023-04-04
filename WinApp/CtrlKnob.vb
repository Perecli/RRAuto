Namespace CustomControls

    Public Class Knob
        Inherits FrameworkElement

        Private _intMinimum As Integer = 0
        Private _intMaximum As Integer = 25
        Private _intLargeMarkerCnt As Integer = 5
        Private _intSmallMarkerCnt As Integer = 1
        Private _enuRampType As RampTypes = RampTypes.Linear
        Private _intValue As Integer = 0

        Private _blnLeftMouseDown As Boolean = False

        Private _sctKnobColor As Color = Colors.Gray
        Private _sctMarkerColor As Color = Colors.Black
        Private _sctKnobColorLight As Color
        Private _sctKnobColorDark As Color

        Private _objVisuals As New VisualCollection(Me)     'root visual; all other visuals are its children
        Private _objVisKnob As New DrawingVisual
        Private _objVisMarkers As New DrawingVisual
        Private _objVisDimple As New DrawingVisual
        Private _objScaleTransform As New ScaleTransform
        Private _objPanTransform As New TranslateTransform

        Public Enum RampTypes
            Linear
            ExpLow
            ExpHigh
        End Enum

        Public Sub New()
            Dim objTransGroup As New TransformGroup()
            objTransGroup.Children.Add(_objScaleTransform)
            objTransGroup.Children.Add(_objPanTransform)
            Me.RenderTransform = objTransGroup
            Me.RenderTransformOrigin = New Point(0, 0)

            _objVisuals.Add(_objVisKnob)
            _objVisuals.Add(_objVisMarkers)
            _objVisuals.Add(_objVisDimple)

            DrawKnob()
            DrawMarkers()
            DrawDimple()
        End Sub

#Region "Public Members"

        ''' <summary>Raised only when the knob value is set by this contol not externaly though the Value property</summary>
        Public Event ValueChangedInternally()

        Public Property Minimum() As Integer
            Get
                Return _intMinimum
            End Get
            Set(Value As Integer)
                _intMinimum = Value
                DrawDimple()
                DrawMarkers()
            End Set
        End Property

        Public Property Maximum() As Integer
            Get
                Return _intMaximum
            End Get
            Set(Value As Integer)
                _intMaximum = Value
                DrawDimple()
                DrawMarkers()
            End Set
        End Property

        Public Property LargeMarkerCnt() As Integer
            Get
                Return _intLargeMarkerCnt
            End Get
            Set(Value As Integer)
                _intLargeMarkerCnt = Value
                DrawMarkers()
            End Set
        End Property

        Public Property SmallMarkerCnt() As Integer
            Get
                Return _intSmallMarkerCnt
            End Get
            Set(Value As Integer)
                _intSmallMarkerCnt = Value
                DrawMarkers()
            End Set
        End Property

        Public Property RampType() As RampTypes
            Get
                Return _enuRampType
            End Get
            Set(Value As RampTypes)
                _enuRampType = Value
            End Set
        End Property

        Public Property Value() As Integer
            Get
                Return _intValue
            End Get
            Set(Value As Integer)
                SetKnobValue(Value, False)
            End Set
        End Property

        Public Property KnobColor() As Color
            Get
                Return _sctKnobColor
            End Get
            Set(Value As Color)
                _sctKnobColor = Value
                DrawKnob()
                DrawDimple()
            End Set
        End Property

        Public Sub StepUp()
            SetKnobValue(_intValue + GetIncrement(), True)
        End Sub

        Public Sub StepDown()
            SetKnobValue(_intValue - GetIncrement(), True)
        End Sub

#End Region

#Region "Rendering"

        'everything is rendered normalized so we can scale the output later as a whole
        Const _intCenterX As Integer = 0
        Const _intCenterY As Integer = 0
        Const _intNormalMaxRadius As Integer = 100      'maximum normal radius

        'these are percentages of _intNormalMaxRadius
        Const _dblMarkerRadius As Double = 100
        Const _dblSmallMarkerLen As Double = 4.5
        Const _dblSmallMarkerThickness As Double = 1.5
        Const _dblLargeMarkerLen As Double = 8
        Const _dblLargeMarkerThickness As Double = 3

        Const _dblKnobOuterRadius As Double = 89
        Const _dblKnobInnerRadius As Double = 76

        Const _dblDimplePadding As Double = 8
        Const _dblDimpleSize As Double = 3.5
        Const _dblDimplePenSize As Double = 2.5

        Private Sub DrawKnob()
            'configure knob color
            Const intGradDelta As Integer = 90

            _sctKnobColorLight = Color.FromRgb(
                Math.Min(_sctKnobColor.R + intGradDelta, 255),
                Math.Min(_sctKnobColor.G + intGradDelta, 255),
                Math.Min(_sctKnobColor.B + intGradDelta, 255))

            _sctKnobColorDark = Color.FromRgb(
                Math.Max(_sctKnobColor.R - intGradDelta, 0),
                Math.Max(_sctKnobColor.G - intGradDelta, 0),
                Math.Max(_sctKnobColor.B - intGradDelta, 0))

            Dim objDrawingContext As DrawingContext = _objVisKnob.RenderOpen()

            'outer circle
            Dim objBrush As New LinearGradientBrush(_sctKnobColorLight, _sctKnobColorDark, New Point(0, 0), New Point(1, 1))
            objBrush.Freeze()
            objDrawingContext.DrawEllipse(objBrush, DirectCast(Nothing, Pen), New Point(_intCenterX, _intCenterY), _dblKnobOuterRadius, _dblKnobOuterRadius)

            'inner circle
            objBrush = New LinearGradientBrush(_sctKnobColorDark, _sctKnobColorLight, New Point(0, 0), New Point(1, 1))
            objBrush.Freeze()
            objDrawingContext.DrawEllipse(objBrush, DirectCast(Nothing, Pen), New Point(_intCenterX, _intCenterY), _dblKnobInnerRadius, _dblKnobInnerRadius)

            objDrawingContext.Close()
        End Sub


        Private Sub DrawMarkers()
            Dim objDrawingContext As DrawingContext = _objVisMarkers.RenderOpen()

            'draw small markings
            For intValue As Integer = _intMinimum To _intMaximum Step _intSmallMarkerCnt
                objDrawingContext.DrawLine(New Pen(New SolidColorBrush(_sctMarkerColor), _dblSmallMarkerThickness),
                                           GetMarkerPoint(0, intValue), GetMarkerPoint(_dblSmallMarkerLen, intValue))
            Next

            'draw large markings
            For intValue As Integer = _intMinimum To _intMaximum Step _intLargeMarkerCnt
                objDrawingContext.DrawLine(New Pen(New SolidColorBrush(_sctMarkerColor), _dblLargeMarkerThickness),
                                           GetMarkerPoint(0, intValue), GetMarkerPoint(_dblLargeMarkerLen, intValue))
            Next

            objDrawingContext.Close()
        End Sub

        Private Function GetMarkerPoint(intLength As Integer, intValue As Integer) As Point
            Dim dblDegree As Double = 270 * intValue / (_intMaximum - _intMinimum)
            dblDegree = (dblDegree + 135) * Math.PI / 180
            Dim sctPos As New Point
            sctPos.X = CType(Math.Cos(dblDegree) * (_dblMarkerRadius - intLength) + _intCenterX, Integer)
            sctPos.Y = CType(Math.Sin(dblDegree) * (_dblMarkerRadius - intLength) + _intCenterY, Integer)
            Return sctPos
        End Function


        Private Sub DrawDimple()
            'get angle of knob's value
            Dim dblDegree As Double = 270 * _intValue / (_intMaximum - _intMinimum)
            dblDegree = (dblDegree + 135) * Math.PI / 180

            'get coordinate of dimple
            Dim sctDimpleCenter As Point = New Point(0, 0)
            sctDimpleCenter.X = CType(Math.Cos(dblDegree) * (_dblKnobInnerRadius - _dblDimplePadding) + _intCenterX, Integer)
            sctDimpleCenter.Y = CType(Math.Sin(dblDegree) * (_dblKnobInnerRadius - _dblDimplePadding) + _intCenterY, Integer)

            'get dimple coordonates for drawing arcs
            Dim sctDimpleLowerLeft As New Point(sctDimpleCenter.X - _dblDimpleSize, sctDimpleCenter.Y + _dblDimpleSize)
            Dim sctDimpleUpperRight As New Point(sctDimpleCenter.X + _dblDimpleSize, sctDimpleCenter.Y - _dblDimpleSize)

            Dim objDrawingContext As DrawingContext = _objVisDimple.RenderOpen()

            objDrawingContext.DrawGeometry(Nothing, New Pen(New SolidColorBrush(_sctKnobColorDark), _dblDimplePenSize), GetArcPathGeometry(sctDimpleLowerLeft, sctDimpleUpperRight))
            objDrawingContext.DrawGeometry(Nothing, New Pen(New SolidColorBrush(_sctKnobColorLight), _dblDimplePenSize), GetArcPathGeometry(sctDimpleUpperRight, sctDimpleLowerLeft))

            objDrawingContext.Close()
        End Sub

        Private Function GetArcPathGeometry(sctStart As Point, sctEnd As Point) As PathGeometry
            Dim objPGeometry As PathGeometry = New PathGeometry()
            Dim objPFigure As PathFigure = New PathFigure()
            objPGeometry.Figures.Add(objPFigure)
            objPFigure.StartPoint = sctStart
            objPFigure.Segments.Add(New ArcSegment(sctEnd, New Size(0.1, 0.1), 0, False, SweepDirection.Clockwise, True))
            Return objPGeometry
        End Function


        Protected Overloads Overrides ReadOnly Property VisualChildrenCount() As Integer
            Get
                Return _objVisuals.Count
            End Get
        End Property

        Protected Overloads Overrides Function GetVisualChild(index As Integer) As Visual

            If index < 0 OrElse index >= _objVisuals.Count Then Throw New ArgumentOutOfRangeException("index")

            Return _objVisuals(index)

        End Function


        Private Sub Knob_LayoutUpdated(sender As Object, e As System.EventArgs) Handles Me.LayoutUpdated
            Dim sctCenter As New Point(Me.RenderSize.Width / 2, Me.RenderSize.Height / 2)
            Dim dblActualMaxRadius As Double = Math.Max(0, Math.Min(Me.RenderSize.Width, Me.RenderSize.Height) / 2)

            _objScaleTransform.ScaleX = dblActualMaxRadius / _intNormalMaxRadius
            _objScaleTransform.ScaleY = _objScaleTransform.ScaleX

            _objPanTransform.X = sctCenter.X
            _objPanTransform.Y = sctCenter.Y
        End Sub

        Private Sub Knob_IsEnabledChanged(sender As Object, e As System.Windows.DependencyPropertyChangedEventArgs) Handles Me.IsEnabledChanged
            If Me.IsEnabled Then
                Me.Opacity = 1
            Else
                Me.Opacity = 0.3
            End If
        End Sub

#End Region

#Region "User Input"

        Private Sub Knob_MouseLeftButtonDown(sender As Object, e As System.Windows.Input.MouseButtonEventArgs) Handles Me.MouseLeftButtonDown
            _blnLeftMouseDown = True
        End Sub

        Private Sub Knob_MouseMove(sender As Object, e As System.Windows.Input.MouseEventArgs) Handles Me.MouseMove
            If _blnLeftMouseDown Then
                Me.Cursor = Cursors.Hand
                SetKnobValue(Me.GetValueFromPosition(e.GetPosition(Me)), True)
            End If
        End Sub

        Private Sub Knob_MouseLeftButtonUp(sender As Object, e As System.Windows.Input.MouseButtonEventArgs) Handles Me.MouseLeftButtonUp
            _blnLeftMouseDown = False
            Me.Cursor = Cursors.Arrow
            SetKnobValue(Me.GetValueFromPosition(e.GetPosition(Me)), True)
        End Sub

        Private Sub Knob_MouseLeave(sender As Object, e As System.Windows.Input.MouseEventArgs) Handles Me.MouseLeave
            _blnLeftMouseDown = False
            Me.Cursor = Cursors.Arrow
        End Sub

        Private Function GetValueFromPosition(sctPos As Point) As Integer
            'converts geometrical cursor position in to knob value
            Dim dblDegree As Double = 0.0
            Select Case sctPos.X
                Case Is < _intCenterX
                    dblDegree = CType(_intCenterY - sctPos.Y, Double) / CType(_intCenterX - sctPos.X, Double)
                    dblDegree = Math.Atan(dblDegree)
                    dblDegree = dblDegree * (180 / Math.PI) + 45
                    Return CType(dblDegree * (_intMaximum - _intMinimum) / 270, Integer)
                Case Is > _intCenterX
                    dblDegree = CType(sctPos.Y - _intCenterY, Double) / CType(sctPos.X - _intCenterX, Double)
                    dblDegree = Math.Atan(dblDegree)
                    dblDegree = 225 + dblDegree * (180 / Math.PI)
                    Return CType(dblDegree * (_intMaximum - _intMinimum) / 270, Integer)
                Case Else
                    Return _intValue
            End Select
        End Function

        Private Sub Knob_MouseWheel(sender As Object, e As System.Windows.Input.MouseWheelEventArgs) Handles Me.MouseWheel
            Select Case True
                Case e.Delta < 0
                    SetKnobValue(_intValue - GetIncrement(), True)
                Case e.Delta > 0
                    SetKnobValue(_intValue + GetIncrement(), True)
            End Select
        End Sub

        Private Function GetIncrement() As Integer
            Select Case _enuRampType
                Case RampTypes.Linear
                    Return 1
                Case RampTypes.ExpLow
                    Dim intDoublingValue As Integer = Math.Floor(Me.Maximum / 5)
                    Return Math.Floor(_intValue / intDoublingValue + 1)
                Case RampTypes.ExpHigh
                    Dim intDoublingValue As Integer = Math.Floor(Me.Maximum / 7)
                    Return Math.Floor(_intValue / intDoublingValue + 1)
            End Select
        End Function

        Private Sub SetKnobValue(intValue As Integer, blnInternal As Boolean)
            intValue = Math.Min(Math.Max(intValue, _intMinimum), _intMaximum)
            If _intValue <> intValue Then
                _intValue = intValue

                DrawDimple()

                If blnInternal Then RaiseEvent ValueChangedInternally()
            End If

        End Sub

#End Region

    End Class

End Namespace