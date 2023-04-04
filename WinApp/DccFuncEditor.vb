Imports RRAuto.PropertyGridHelpers
Imports System.Windows.Forms

Namespace UIEditors

    Public Class DccFunctionsEditorControl
        Inherits System.Windows.Forms.UserControl

#Region " Windows Form Designer generated code "

        Public Sub New(sctConfiguration As PgPkImmediate.DataPayload)
            MyBase.New()

            'This call is required by the Windows Form Designer.
            InitializeComponent()

            'Add any initialization after the InitializeComponent() call
            _sctConfiguration = sctConfiguration
            InitializeComponentExt()
        End Sub

        'UserControl overrides dispose to clean up the component list.
        Protected Overloads Overrides Sub Dispose(disposing As Boolean)
            If disposing Then
                TerminateComponent()
                If Not (components Is Nothing) Then
                    components.Dispose()
                End If
            End If
            MyBase.Dispose(disposing)
        End Sub

        'Required by the Windows Form Designer
        Private components As System.ComponentModel.IContainer

        'NOTE: The following procedure is required by the Windows Form Designer
        'It can be modified using the Windows Form Designer.  
        'Do not modify it using the code editor.
        Friend WithEvents Label1 As System.Windows.Forms.Label
        <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
            Me.Label1 = New System.Windows.Forms.Label()
            Me.SuspendLayout()
            '
            'Label1
            '
            Me.Label1.AutoSize = True
            Me.Label1.FlatStyle = System.Windows.Forms.FlatStyle.System
            Me.Label1.Location = New System.Drawing.Point(14, 8)
            Me.Label1.Name = "Label1"
            Me.Label1.Size = New System.Drawing.Size(53, 13)
            Me.Label1.TabIndex = 0
            Me.Label1.Text = "Functions"
            '
            'DccFunctionsEditorControl
            '
            Me.Controls.Add(Me.Label1)
            Me.Font = New System.Drawing.Font("Tahoma", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Name = "DccFunctionsEditorControl"
            Me.Size = New System.Drawing.Size(198, 60)
            Me.ResumeLayout(False)
            Me.PerformLayout()

        End Sub

#End Region

        Private _chkFunctions As CheckBox()
        Private _sctConfiguration As PgPkImmediate.DataPayload

        Private Sub InitializeComponentExt()
            Me.SuspendLayout()

            Select Case _sctConfiguration.Instruction
                Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func0to4
                    ReDim _chkFunctions(4)
                Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func5to8, RRAutoLib.Loconet.PkImmediate.DccInstrType.Func9to12
                    ReDim _chkFunctions(3)
                Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func13To20, RRAutoLib.Loconet.PkImmediate.DccInstrType.Func21To28
                    ReDim _chkFunctions(7)
            End Select

            Dim intLeftLoc As Integer = 5

            'render left label
            Dim objLabelLeft As New Label
            With objLabelLeft
                .FlatStyle = FlatStyle.System
                .Size = New System.Drawing.Size(19, 13)
                .Location = New System.Drawing.Point(5, 32)
                .TextAlign = System.Drawing.ContentAlignment.MiddleRight
                Select Case _sctConfiguration.Instruction
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func0to4
                        .Text = "0"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func5to8
                        .Text = "5"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func9to12
                        .Text = "9"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func13To20
                        .Text = "13"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func21To28
                        .Text = "21"
                End Select                
            End With
            Me.Controls.Add(objLabelLeft)

            'render check boxes
            intLeftLoc += 25
            For bytIdx As Byte = 0 To _chkFunctions.GetUpperBound(0)

                'add check box grouping spacing
                Select Case _sctConfiguration.Instruction
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func0to4
                        If bytIdx = 1 Then intLeftLoc += 10
                    Case Else
                        If bytIdx = 4 Then intLeftLoc += 10
                End Select

                'add check box control
                _chkFunctions(bytIdx) = New CheckBox
                With _chkFunctions(bytIdx)
                    .FlatStyle = System.Windows.Forms.FlatStyle.System
                    .Size = New System.Drawing.Size(16, 16)
                    .Location = New System.Drawing.Point(intLeftLoc, 32)
                    .Tag = bytIdx
                End With
                Me.Controls.Add(_chkFunctions(bytIdx))
                AddHandler _chkFunctions(bytIdx).Click, AddressOf FunctionsCheckBox_Click

                intLeftLoc += 16
            Next

            'render left label
            intLeftLoc += 3
            Dim objLabelRight As New Label
            With objLabelRight
                .FlatStyle = FlatStyle.System
                .Size = New System.Drawing.Size(19, 13)
                .Location = New System.Drawing.Point(intLeftLoc, 32)
                .TextAlign = System.Drawing.ContentAlignment.MiddleLeft
                Select Case _sctConfiguration.Instruction
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func0to4
                        .Text = "4"
                    Case (RRAutoLib.Loconet.PkImmediate.DccInstrType.Func5to8)
                        .Text = "8"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func9to12
                        .Text = "12"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func13To20
                        .Text = "20"
                    Case RRAutoLib.Loconet.PkImmediate.DccInstrType.Func21To28
                        .Text = "28"
                End Select
            End With
            Me.Controls.Add(objLabelRight)

            'resize the control to fit to the number of functions
            Me.Size = New System.Drawing.Size(intLeftLoc + 26, Me.Size.Height)

            'populate values
            For bytIdx As Byte = 0 To _chkFunctions.GetUpperBound(0)
                _chkFunctions(bytIdx).Checked = _sctConfiguration.FuncStates(bytIdx)
            Next

            Me.ResumeLayout(False)
        End Sub

        Private Sub FunctionsCheckBox_Click(sender As Object,  e As System.EventArgs)
            _sctConfiguration.FuncStates(sender.tag) = sender.Checked
        End Sub

        Public ReadOnly Property Configuration() As PgPkImmediate.DataPayload
            Get
                Return _sctConfiguration
            End Get
        End Property

        Private Sub TerminateComponent()
            For bytIdx As Byte = 0 To _chkFunctions.GetUpperBound(0)
                RemoveHandler _chkFunctions(bytIdx).Click, AddressOf FunctionsCheckBox_Click
            Next
        End Sub
    End Class

End Namespace