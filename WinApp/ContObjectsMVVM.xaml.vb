Imports RRAutoLib.CTC

Namespace CustomControls

    'this was a partial attempt to switch the ContObjects class to use the MVVM pattern; in the end I opted to stay with direct node creation because I just did not see a
    'benefit with this MVVM model; it is elegant but requires more code and more complexity with the same result
    Public Class ContObjectsMVVM

        Public Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            AddHandler My.Application.CtcObjectsLoaded, AddressOf CtcObjectsLoaded
            AddHandler My.Application.ContextObjChanged, AddressOf ContextObjChanged
            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged

        End Sub

#Region "Internal Event Handling"

        Private Sub TreeViewItemSelected(sender As Object, e As RoutedEventArgs)
            DirectCast(e.OriginalSource, TreeViewItem).BringIntoView()

            If Not _blnExternalContextObjChange Then 'makes sure ContextObjChanged events are not echoed back out when originated externally
                My.Application.SetContextObj(Me, DirectCast(Me.TV.SelectedItem, TreeViewItemViewModel).DataItem)
            End If
        End Sub

        Private Sub OnNodeDoubleClick(objNode As TreeViewItem)
            Select Case My.Application.AppMode
                Case AppMode.Edit
                    Select Case True
                        Case TypeOf DirectCast(objNode.DataContext, TreeViewItemViewModel).DataItem Is EventScript
                            MsgBox("Test DoubleClick")
                            'ScriptEditor_Click(objNode, Nothing)

                    End Select
                Case AppMode.Operation

            End Select
        End Sub

#End Region

#Region "External Event Handling"

        Private _blnExternalContextObjChange As Boolean = False

        Private Sub CtcObjectsLoaded()
            _blnExternalContextObjChange = True   'makes sure ContextObjChanged events are not echoed back out when originated externally

            'don't like how MVVM model works here because after this line executes we are not guaranteed that the nodes have been built; the framework
            'seems to be building the nodes asynchronously when it finds some free thread time; this breaks the _blnExternalContextObjChange flag from working as intended
            Me.TV.DataContext = New CtcRootViewModel()

            _blnExternalContextObjChange = False
        End Sub

        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            With DirectCast(Me.TV.DataContext, CtcRootViewModel)
                Select Case True
                    Case objPreviousObject Is Nothing   'add
                        DirectCast(.FindChild(objChangedObject), CtcListViewModel).AddChild(objChangedObject)

                    Case objChangedObject.IsDeleted     'delete
                        Dim objViewModel As TreeViewItemViewModel =
                            DirectCast(.FindChild(objPreviousObject), CtcListViewModel).FindChild(objChangedObject)

                        'note that we check to make sure the node still exists because multiple NotifyObjectChanged() calls could attempt to delete this after already deleted
                        If objViewModel IsNot Nothing Then objViewModel.Delete()

                    Case Else                           'change
                        DirectCast(.FindChild(objPreviousObject), CtcListViewModel).FindChild(objChangedObject).Refresh()
                        'the framework automaticaly sorts the nodes with the SortConverter declared in XAML but only for initial node creation
                        'and inserts, not when the .Text of a node gets changed
                        'todo: resort the list since the name may have changed

                End Select
            End With
        End Sub

        Private Sub ContextObjChanged(objSender As Object, objPrevContextObj As Object, objCurrContextObj As Object)

            If objSender Is Me Then Exit Sub 'don't listen to my own broadcasted events

            _blnExternalContextObjChange = True   'makes sure ContextObjChanged events are not echoed back out when originated externally

            With DirectCast(Me.TV.DataContext, CtcRootViewModel)
                If TypeOf objCurrContextObj Is ICtcObjectListBase Then
                    .FindChild(objCurrContextObj).IsSelected = True
                Else
                    DirectCast(.FindChild(objCurrContextObj), CtcListViewModel).FindChild(objCurrContextObj).IsSelected = True
                End If
            End With

            _blnExternalContextObjChange = False

        End Sub

#End Region

    End Class

End Namespace
