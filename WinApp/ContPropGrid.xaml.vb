Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet
Imports RRAuto.PropertyGridHelpers

Namespace CustomControls

    Partial Public Class ContPropGrid

        Friend Sub New()
            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            AddHandler CtcService.BeforeModeChange, AddressOf BeforeModeChange
            AddHandler CtcService.ModeChanged, AddressOf ModeChanged
            AddHandler My.Application.ContextObjChanged, AddressOf ContextObjChanged
            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged
        End Sub

        Private Sub BeforeModeChange()
            If Me.PropertyGrid.ContainsFocus AndAlso Not CtcService.IsStarted Then
                'if going to operation mode with the shortcut keys and a PropertyGrid edit has not been comitted a crash will occur
                'caused by the fact that the property value commit for a CtcObject occurs after the CtcService is started which is now allowed
                Me.PropertyGrid.Refresh() 'this indirectly commits the edit
            End If
        End Sub

        Private Sub ModeChanged()
            Me.PropertyGrid.Refresh()
        End Sub

        Private Sub ContextObjChanged(objSender As Object, objPrevContextObj As Object, objCurrContextObj As Object)
            Select Case True
                Case TypeOf objCurrContextObj Is ICtcObjectListBase
                    Me.PropertyGrid.SelectedObject = New PgCtcObjectListBase(objCurrContextObj)

                    'CtcObjectBase types
                Case TypeOf objCurrContextObj Is Track
                    Me.PropertyGrid.SelectedObject = New PgTrack(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Block
                    Me.PropertyGrid.SelectedObject = New PgBlock(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Route
                    Me.PropertyGrid.SelectedObject = New PgRoute(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Sensor
                    Me.PropertyGrid.SelectedObject = New PgSensor(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Signal
                    Me.PropertyGrid.SelectedObject = New PgSignal(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Accessory
                    Me.PropertyGrid.SelectedObject = New PgAccessory(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Label
                    Me.PropertyGrid.SelectedObject = New PgLabel(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Button
                    Me.PropertyGrid.SelectedObject = New PgButton(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Engine
                    Me.PropertyGrid.SelectedObject = New PgEngine(objCurrContextObj)
                Case TypeOf objCurrContextObj Is Sequence
                    Me.PropertyGrid.SelectedObject = New PgSequence(objCurrContextObj)
                Case TypeOf objCurrContextObj Is EventScript
                    Me.PropertyGrid.SelectedObject = New PgEventScript(objCurrContextObj)
                Case TypeOf objCurrContextObj Is StepScript
                    Me.PropertyGrid.SelectedObject = New PgStepScript(objCurrContextObj)
                Case TypeOf objCurrContextObj Is GlobalScript
                    Me.PropertyGrid.SelectedObject = New PgGlobalScript(objCurrContextObj)

                Case TypeOf objCurrContextObj Is PkStatesList.State
                    Me.PropertyGrid.SelectedObject = New PgState(objCurrContextObj)

                    'Packet types
                Case TypeOf objCurrContextObj Is PkSetSwitch
                    Me.PropertyGrid.SelectedObject = New PgPkSetSwitch(objCurrContextObj)
                Case TypeOf objCurrContextObj Is PkInput
                    Me.PropertyGrid.SelectedObject = New PgPkInput(objCurrContextObj)
                Case TypeOf objCurrContextObj Is PkImmediate
                    Me.PropertyGrid.SelectedObject = New PgPkImmediate(objCurrContextObj)
                Case TypeOf objCurrContextObj Is PkPeerXfer
                    Me.PropertyGrid.SelectedObject = New PgLocoIO(objCurrContextObj)

                Case Else
                    Me.PropertyGrid.SelectedObject = objCurrContextObj
            End Select
        End Sub

        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            If objSender Is Me Then Exit Sub 'don't listen to my own broadcasted events
            Dim objSelectedObject As Object = GetPropertyGridObject()
            If (TypeOf objSelectedObject Is CtcObjectBase AndAlso objSelectedObject Is objChangedObject) Or
               (TypeOf objSelectedObject Is PkStatesList.State AndAlso DirectCast(objSelectedObject, PkStatesList.State).Parent Is objChangedObject) Or
               (TypeOf objSelectedObject Is Packet AndAlso DirectCast(objSelectedObject, Packet).Tag Is objChangedObject) Then
                Me.PropertyGrid.Refresh()
            End If
        End Sub

        Private Sub PropertyGrid_PropertyValueChanged(s As Object, e As System.Windows.Forms.PropertyValueChangedEventArgs) Handles PropertyGrid.PropertyValueChanged
            Dim objSelectedObject As Object = GetPropertyGridObject()
            Select Case True
                Case TypeOf objSelectedObject Is CtcObjectBase
                    DirectCast(objSelectedObject, CtcObjectBase).NotifyObjectChanged(Me)

                Case TypeOf objSelectedObject Is PkStatesList.State
                    DirectCast(objSelectedObject, PkStatesList.State).Parent.NotifyObjectChanged(Me)

                Case TypeOf objSelectedObject Is Packet
                    DirectCast(DirectCast(objSelectedObject, Packet).Tag, PkStatesList.State).Parent.NotifyObjectChanged(Me)

            End Select
            My.Application.SetLayoutDirty()
        End Sub

        Private Function GetPropertyGridObject() As Object
            If TypeOf Me.PropertyGrid.SelectedObject Is IWrapperObject Then
                Return DirectCast(Me.PropertyGrid.SelectedObject, IWrapperObject).WrappedObject
            Else
                Return Me.PropertyGrid.SelectedObject
            End If
        End Function

    End Class

End Namespace

