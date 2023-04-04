Namespace CustomControls

    Friend Class ContScriptTemplates
        Inherits TreeViewX

        Private _sctMouseDragStart As Point
        Private _strTemplate As String

        Public Sub New()

            Me.BeginInit()

            Me.ToolTip = "Drag a template to any script editor window."

            Dim objNode As New TreeViewNode1("General snippets")
            objNode.TextWeight = FontWeights.Bold
            Me.Items.Add(objNode)
            With objNode.Items
                .Add(New TreeViewNode1("Post a message", My.Resources.Snippet01))
                .Add(New TreeViewNode1("Act on a sensor", My.Resources.Snippet02))
                .Add(New TreeViewNode1("Set a turnout", My.Resources.Snippet03))
                .Add(New TreeViewNode1("Act on a turnout", My.Resources.Snippet04))
                .Add(New TreeViewNode1("Set a signal", My.Resources.Snippet05))
                .Add(New TreeViewNode1("Act on a signal", My.Resources.Snippet06))
                .Add(New TreeViewNode1("Set/Lock/Unlock a route", My.Resources.Snippet07))
                .Add(New TreeViewNode1("Act on a route", My.Resources.Snippet08))
                .Add(New TreeViewNode1("Send Loconet packets", My.Resources.Snippet09))
                .Add(New TreeViewNode1("Play a sequence", My.Resources.Snippet10))
                .Add(New TreeViewNode1("Operate an engine", My.Resources.Snippet11))
                .Add(New TreeViewNode1("Wait for some time", My.Resources.Snippet12))
                .Add(New TreeViewNode1("Wait for events", My.Resources.Snippet13))
                .Add(New TreeViewNode1("Simple iteration", My.Resources.Snippet14))
                .Add(New TreeViewNode1("Play a sound", My.Resources.Snippet15))
                .Add(New TreeViewNode1("Run a step script", My.Resources.Snippet16))
            End With
            objNode.IsExpanded = True

            objNode = New TreeViewNode1("Specific to event scripts")
            objNode.TextWeight = FontWeights.Bold
            Me.Items.Add(objNode)
            With objNode.Items
                .Add(New TreeViewNode1("Set signal based on turnout", My.Resources.EventScript01))
                .Add(New TreeViewNode1("Route unlock based on sensor", My.Resources.EventScript02))
                .Add(New TreeViewNode1("Label displaying transponder", My.Resources.EventScript03))
            End With
            objNode.IsExpanded = True

            objNode = New TreeViewNode1("Specific to step scripts")
            objNode.TextWeight = FontWeights.Bold
            Me.Items.Add(objNode)
            With objNode.Items
                .Add(New TreeViewNode1("Single line non-awaitable step", My.Resources.StepScript01))
                .Add(New TreeViewNode1("Single line awaitable step", My.Resources.StepScript02))
                .Add(New TreeViewNode1("Multi-line non-awaitable step", My.Resources.StepScript03))
                .Add(New TreeViewNode1("Multi-line awaitable step", My.Resources.StepScript04))
                .Add(New TreeViewNode1("Task cancellation within a step", My.Resources.StepScript05))
            End With
            objNode.IsExpanded = True

            objNode = New TreeViewNode1("Specific to global scripts")
            objNode.TextWeight = FontWeights.Bold
            Me.Items.Add(objNode)
            With objNode.Items
                .Add(New TreeViewNode1("Empty subroutine", My.Resources.GlobalScript01))
                .Add(New TreeViewNode1("Empty function", My.Resources.GlobalScript02))
                .Add(New TreeViewNode1("Empty extender sub", My.Resources.GlobalScript03))
                .Add(New TreeViewNode1("Empty extender func", My.Resources.GlobalScript04))
                .Add(New TreeViewNode1("Empty async function", My.Resources.GlobalScript05))
                .Add(New TreeViewNode1("Operation start event", My.Resources.GlobalScript06))
                .Add(New TreeViewNode1("Operation stop event", My.Resources.GlobalScript07))
                .Add(New TreeViewNode1("Basic class construct", My.Resources.GlobalScript08))
                .Add(New TreeViewNode1("Custom Loconet packet class", My.Resources.GlobalScript09))
                .Add(New TreeViewNode1("Global variable bag", My.Resources.GlobalScript10))
            End With
            objNode.IsExpanded = True

            Me.EndInit()

        End Sub

        Private Sub Me_PreviewMouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs) Handles Me.PreviewMouseLeftButtonDown
            _sctMouseDragStart = e.GetPosition(Nothing)

            'get template from clicked node
            Dim objNode As TreeViewNode1 = TreeViewX.ParseForNode(e)
            If objNode Is Nothing Then
                _strTemplate = Nothing
            Else
                _strTemplate = objNode.Tag
            End If
        End Sub

        Private Sub Me_PreviewMouseMove(sender As Object, e As MouseEventArgs) Handles Me.PreviewMouseMove
            Static blnIsDragging As Boolean = False

            If e.LeftButton = MouseButtonState.Pressed And Not blnIsDragging Then
                Dim sctPos As Point = e.GetPosition(Nothing)

                If (Math.Abs(sctPos.X - _sctMouseDragStart.X) > SystemParameters.MinimumHorizontalDragDistance Or
                    Math.Abs(sctPos.Y - _sctMouseDragStart.Y) > SystemParameters.MinimumVerticalDragDistance) And
                    _strTemplate <> Nothing Then

                    blnIsDragging = True
                    Dim objData As DataObject = New DataObject(DataFormats.Text.ToString(), _strTemplate)
                    Dim objEffects As DragDropEffects = DragDrop.DoDragDrop(Me, objData, DragDropEffects.Copy)
                    blnIsDragging = False
                End If
            End If
        End Sub

    End Class

End Namespace
