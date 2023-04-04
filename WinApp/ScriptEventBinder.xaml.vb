Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting
Imports RRAuto.CustomControls
Imports RRAuto.DrawHelper

Namespace Windows

    Partial Public Class ScriptEventBinder

        Friend Property EventScript As EventScript

        Friend Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            AddRootNote("IconTrack", "Tracks", CtcService.Tracks)
            AddRootNote("IconRoute", "Routes", CtcService.Routes)
            AddRootNote("IconSensor", "Sensors", CtcService.Sensors)
            AddRootNote("IconSignal", "Signals", CtcService.Signals)
            AddRootNote("IconAccessory", "Accessories", CtcService.Accessories)
            AddRootNote("IconLabel", "Labels", CtcService.Labels)
            AddRootNote("IconButton", "Buttons", CtcService.Buttons)
            AddRootNote("IconEngine", "Engines", CtcService.Engines)
            AddRootNote("IconDocStep", "Step Scripts", CtcService.StepScripts)
        End Sub

        Private Sub AddRootNote(strImage As String, strName As String, objList As ICtcObjectListBase)

            Dim objRootNode As New TreeViewNode2(TryFindResource(strImage), strName)
            Me.TreeViewEvents.Items.Add(objRootNode)
            For Each objItem As ISupportsScriptEvents In objList
                Dim objImageSource As ImageSource
                Select Case True
                    Case TypeOf objItem Is Track
                        objImageSource = GetTrackDrawingImage(objItem)
                    Case TypeOf objItem Is Signal
                        objImageSource = GetSignalDrawingImage(objItem)
                    Case TypeOf objItem Is Button
                        objImageSource = GetButtonDrawingImage(objItem)
                    Case Else
                        objImageSource = TryFindResource(strImage)
                End Select
                Dim objItemNode As New TreeViewNode2(objImageSource, DirectCast(objItem, CtcObjectBase).Name)
                objRootNode.Items.Add(objItemNode)
                For Each objEvent As EventScript.Event In objItem.ScriptEventBinder.PublishedEvents
                    Dim objEventNode As New TreeViewNode2(TryFindResource("IconEvent"), objEvent.Name)
                    objItemNode.Items.Add(objEventNode)
                    objEventNode.Tag = objEvent
                Next
            Next
            objRootNode.Sort()

        End Sub

        Private Sub ScriptEditor_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
            Me.Title = "Event Binding - " & Me.EventScript.Name

            For Each objEvent As EventScript.Event In Me.EventScript.EventBindings
                AddItemToEventList(objEvent)
            Next
        End Sub


        Private Sub AddItemToEventList(objEvent As EventScript.Event)

            Dim blnItemAdded As Boolean = True

            Select Case True
                Case TypeOf objEvent.Owner Is CtcObjectBase
                    Dim objCtcObject As CtcObjectBase = objEvent.Owner
                    Dim objCtcObjIcon As ImageSource
                    Select Case True
                        Case TypeOf objCtcObject Is Track
                            objCtcObjIcon = GetTrackDrawingImage(objCtcObject)
                        Case TypeOf objCtcObject Is Route
                            objCtcObjIcon = TryFindResource("IconRoute")
                        Case TypeOf objCtcObject Is Sensor
                            objCtcObjIcon = TryFindResource("IconSensor")
                        Case TypeOf objCtcObject Is Signal
                            objCtcObjIcon = GetSignalDrawingImage(objCtcObject)
                        Case TypeOf objCtcObject Is Accessory
                            objCtcObjIcon = TryFindResource("IconAccessory")
                        Case TypeOf objCtcObject Is Label
                            objCtcObjIcon = TryFindResource("IconLabel")
                        Case TypeOf objCtcObject Is Button
                            objCtcObjIcon = GetButtonDrawingImage(objCtcObject)
                        Case TypeOf objCtcObject Is Engine
                            objCtcObjIcon = TryFindResource("IconEngine")
                        Case TypeOf objCtcObject Is StepScript
                            objCtcObjIcon = TryFindResource("IconDocStep")
                    End Select
                    Dim objDataItem As Object = New With {
                        .CtcObjIcon = objCtcObjIcon,
                        .CtcObjName = objCtcObject.Name,
                        .EventName = objEvent.Name,
                        .Object = objEvent}
                    Me.ListViewEvents.Items.Add(objDataItem)

                    'Case ....
                    'other types that implement ISupportsScriptEvents might be added later

                Case Else
                    blnItemAdded = False

            End Select

            If blnItemAdded Then
                'hack to rezise the column width to widest event name in case the added item is wider than all the previous items (prevents visual truncation)
                Dim Columns As GridViewColumnCollection = DirectCast(Me.ListViewEvents.View, GridView).Columns
                'this sets the value to something else so setting it later to NaN will force the width recalc
                Columns(0).Width = 0
                Columns(1).Width = 0
                'this tells the column that its width should be auto resized; note that the auto resizer does not auto shrink but only auto grow
                Columns(0).Width = Double.NaN
                Columns(1).Width = Double.NaN
            End If

        End Sub

        Private Sub TreeViewEvents_NodeDoubleClick(objNode As TreeViewItem) Handles TreeViewEvents.NodeDoubleClick

            Dim objEvent As EventScript.Event = objNode.Tag
            If objEvent IsNot Nothing Then
                Dim blnEventInList As Boolean = False
                For Each objDataItem As Object In Me.ListViewEvents.Items
                    If objEvent.Equals(objDataItem.Object) Then
                        blnEventInList = True
                        Exit For
                    End If
                Next
                If Not blnEventInList Then AddItemToEventList(objEvent)
            End If

        End Sub

        Private Sub ListViewEvents_MouseDoubleClick(objDataItem As Object) Handles ListViewEvents.ItemDoubleClick

            Me.ListViewEvents.Items.Remove(objDataItem)

        End Sub


        Private Sub cmdOK_Click(sender As System.Object, e As RoutedEventArgs) Handles cmdOK.Click
            Me.EventScript.EventBindings.Clear()
            For Each objDataItem As Object In Me.ListViewEvents.Items
                Me.EventScript.EventBindings.Add(objDataItem.Object)
            Next
            Me.EventScript.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()

            Me.Close()
        End Sub

        Private Sub cmdCancel_Click(sender As System.Object, e As RoutedEventArgs) Handles cmdCancel.Click
            Me.Close()
        End Sub

    End Class

End Namespace

