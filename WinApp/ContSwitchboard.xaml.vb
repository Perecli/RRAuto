Imports Divelements.SandDock

Namespace CustomControls

    Partial Public Class ContSwitchboard

        Public Sub New()
            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            If Tablet.TabletDevices.Cast(Of TabletDevice).Any(Function(td) td.Type = TabletDeviceType.Touch) Then  'if touch devices found
                SetManipulation(False)   'in future possibly allow user to set this init value through options
            Else
                Me.ImgTouchManip.Visibility = Visibility.Collapsed
                Me.BtnTouchManip.Visibility = Visibility.Collapsed
            End If

            SbColorChanged()
            AddHandler My.Application.SbColorChanged, AddressOf SbColorChanged
        End Sub

        Private Sub Me_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
            'the docking system calls this event not only when the control is first opened but also every time the document window
            'comes into view after being out of view behind another tabbed document

            If DirectCast(Me.Parent, DocumentWindow).DockSite.LastActiveWindow Is Me.Parent Then
                'needed because when the document window is redocked in certain scenarios the focus is lost even if the window is shown as active
                Me.Switchboard.Focus()
            End If

        End Sub

        Private Sub SbColorChanged()
            Me.Background = New SolidColorBrush(My.Settings.BackColor)
        End Sub


        Protected Overrides Sub OnMouseUp(e As MouseButtonEventArgs)
            MyBase.OnMouseUp(e)

            Me.Switchboard._OnMouseUp(e)
        End Sub

        Protected Overrides Sub OnMouseDown(e As MouseButtonEventArgs)
            MyBase.OnMouseDown(e)

            Me.Switchboard._OnMouseDown(e)
        End Sub

        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)

            Me.Switchboard._OnMouseMove(e)
        End Sub

        Protected Overrides Sub OnMouseWheel(e As MouseWheelEventArgs)
            MyBase.OnMouseWheel(e)

            Me.Switchboard._OnMouseWheel(e)
        End Sub


        'Inadvertant effects of setting Me.IsManipulationEnabled to True:
        '- Prevents touch events from being promoted to mouse events (stylus events still get promoted).
        '- Prevents TouchUp on some controls from being raised (i.e. works on this user control but not on elements it contains).

        'The good and the bad about StylusSystemGesture:
        '- works regardless of IsManipulationEnabled value
        '- works for both touch and stylus dispite its name
        '- provides higher level gesture intent that the direct thouch and stylus events don't (i.e. Tap, RightTap (long hold), etc.).
        '- unfortunately setting e.Handled to True does not prevent touch and stylus events from being promoted to mouse, when they do

        'Conclusion: 
        'To capture all pointer events (mouse, thouch, stylus) and manipulations, we must use MouseLeftButtonUp in conjuction with StylusSystemGesture events.
        'But to prevent doubling events, on the mouse side, in those instances where the touch/stylus events do get promoted to mouse events,
        'we must check the e.StylusDevice and ensure we don't act on the mouse event if created by a stylus (touch or sylus) promotion and thus already handled.

        Protected Overrides Sub OnManipulationDelta(e As ManipulationDeltaEventArgs)
            MyBase.OnManipulationDelta(e)

            Me.Switchboard._OnManipulationDelta(e)
        End Sub

        Private Sub SetManipulation(blnEnabled As Boolean)

            Me.IsManipulationEnabled = blnEnabled
            Me.ImgTouchManip.Source = TryFindResource(If(blnEnabled, "IconPinchOn", "IconPinchOff"))

        End Sub


        Private Sub BtnTouchManip_MouseLeftButtonUp(sender As Object, e As MouseButtonEventArgs) Handles BtnTouchManip.MouseLeftButtonUp
            If e.StylusDevice Is Nothing Then
                BtnTouchManip_PointerEvent()
            End If
            e.Handled = True
        End Sub

        Private Sub BtnTouchManip_StylusSystemGesture(sender As Object, e As StylusSystemGestureEventArgs) Handles BtnTouchManip.StylusSystemGesture
            If e.SystemGesture = SystemGesture.Tap Then
                BtnTouchManip_PointerEvent()
            End If
            e.Handled = True
        End Sub

        Private Sub BtnTouchManip_PointerEvent()
            SetManipulation(Not Me.IsManipulationEnabled)
        End Sub

    End Class

End Namespace
