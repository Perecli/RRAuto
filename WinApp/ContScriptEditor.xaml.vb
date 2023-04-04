Imports System.Threading.Tasks
Imports Divelements.SandDock
Imports RRAutoLib.CTC

Namespace CustomControls

    Public Class ContScriptEditor

        Private _objScript As IScript
        Private WithEvents _objCodeEditor As New CodeEditor

        Friend Sub New(objScript As IScript)

            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            Me.FormsHost.Child = _objCodeEditor
            _objScript = objScript
            _objCodeEditor.Text = objScript.Script
            _objCodeEditor.MoveToBeginning()
            _objCodeEditor.ReadOnly = CtcService.IsStarted

            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged
            AddHandler CtcService.BeforeModeChange, AddressOf BeforeModeChange
            AddHandler CtcService.ModeChanged, AddressOf ModeChanged
        End Sub

        Private Sub Me_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
            'the docking system calls this event not only when the control is first opened but also every time the document window
            'comes into view after being out of view behind another tabbed document

            If DirectCast(Me.Parent, DocumentWindow).DockSite.LastActiveWindow Is Me.Parent Then
                SetFocusDelayed()   'this is a fire and forget async call
            End If

            'blnInit ensures statements are only executed when the control is first loaded
            Static blnInit As Boolean = True
            If blnInit Then
                'this statement is located in this event because the needed Me.Parent value is not populated until the control is Loaded
                SetHostScriptName()
                blnInit = False
            End If

        End Sub

        Private Async Sub SetFocusDelayed()
            'without this delay something seems to steal the focus away from the code editor after being set (occurs intermitently)
            'could be a strange interaction because the code editor hosted in the WindowsFormsHost which may not play well with native WPF
            Await Task.Delay(30)

            _objCodeEditor.Focus()
        End Sub

        Private Sub SetHostScriptName()

            Dim strScriptTypeName As String
            Select Case True
                Case TypeOf _objScript Is EventScript
                    strScriptTypeName = "Event Script"
                Case TypeOf _objScript Is StepScript
                    strScriptTypeName = "Step Script"
                Case TypeOf _objScript Is GlobalScript
                    strScriptTypeName = "Global Script"
            End Select

            DirectCast(Me.Parent, DocumentWindow).TabText = strScriptTypeName & " - " & _objScript.Name

        End Sub

#Region "Custom Event Handling"

        Private Sub ShortCutKeys(sender As Object, e As Forms.KeyEventArgs) Handles _objCodeEditor.KeyDown

            'observe a subset of the shortcuts supported by the Main Window 

            If Keyboard.Modifiers = ModifierKeys.Control Then
                Select Case e.KeyCode
                    Case Forms.Keys.N, Forms.Keys.O, Forms.Keys.S, Forms.Keys.K, Forms.Keys.Delete, Forms.Keys.Insert
                        ForwardShortCutKeys(e)
                End Select
            End If
            Select Case e.KeyCode
                Case Forms.Keys.F5
                    ForwardShortCutKeys(e)
            End Select

        End Sub

        Private Sub ForwardShortCutKeys(e As Forms.KeyEventArgs)

            'convert a Forms Key to a WPF Key and forward the event to the Main window's handler
            Dim enuKey As Input.Key = [Enum].Parse(GetType(Input.Key), e.KeyCode.ToString)
            Dim objKeyArgs As KeyEventArgs = New KeyEventArgs(Keyboard.PrimaryDevice, PresentationSource.FromVisual(My.Application.MainWindow), 0, enuKey) With {.RoutedEvent = Keyboard.KeyDownEvent}
            DirectCast(My.Application.MainWindow, Windows.Main).ShortCutKeys(Me, objKeyArgs)
            e.Handled = True   'pre-empts this key's default behavior for the code editor control

        End Sub


        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            If objChangedObject Is _objScript Then
                Select Case True
                    Case objChangedObject.IsDeleted
                        'close script editor content if script has been deleted
                        DirectCast(Me.Parent, DocumentWindow).Close()

                    Case objPreviousObject.Name <> objChangedObject.Name
                        SetHostScriptName()

                End Select
            End If
        End Sub


        Private Sub BeforeModeChange()
            If Not CtcService.IsStarted Then  'if going to operation mode
                _objCodeEditor.ReadOnly = True
                CommitScriptEdits()
            End If
        End Sub

        Private Sub ModeChanged()
            If Not CtcService.IsStarted Then  'if comming back to edit mode
                _objCodeEditor.ReadOnly = False
            End If
        End Sub

#End Region

        Friend Sub CommitScriptEdits()
            If _objScript.Script <> _objCodeEditor.Text Then
                _objScript.Script = _objCodeEditor.Text
                _objScript.NotifyObjectChanged(Me)
                My.Application.SetLayoutDirty()
            End If
        End Sub

        Public Sub Dispose()
            If Not CtcService.IsStarted Then CommitScriptEdits() 'for when the code editor document is closed

            'remove event handlers
            RemoveHandler CtcService.ObjectChanged, AddressOf ObjectChanged
            RemoveHandler CtcService.BeforeModeChange, AddressOf BeforeModeChange
            RemoveHandler CtcService.ModeChanged, AddressOf ModeChanged
        End Sub

    End Class

End Namespace
