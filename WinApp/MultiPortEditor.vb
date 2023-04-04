Imports RRAuto.PropertyGridHelpers
Imports System.Windows.Forms

Namespace UIEditors

    Public Class MultiPortEditorControl
        Inherits System.Windows.Forms.UserControl

#Region " Windows Form Designer generated code "

        Public Sub New()
            MyBase.New()

            'This call is required by the Windows Form Designer.
            InitializeComponent()

            'Add any initialization after the InitializeComponent() call
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
        Friend WithEvents Label2 As System.Windows.Forms.Label
        <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
            Me.Label1 = New System.Windows.Forms.Label
            Me.Label2 = New System.Windows.Forms.Label
            Me.SuspendLayout()
            '
            'Label1
            '
            Me.Label1.AutoSize = True
            Me.Label1.FlatStyle = System.Windows.Forms.FlatStyle.System
            Me.Label1.Location = New System.Drawing.Point(14, 8)
            Me.Label1.Name = "Label1"
            Me.Label1.Size = New System.Drawing.Size(78, 13)
            Me.Label1.TabIndex = 0
            Me.Label1.Text = "Significant Ports"
            '
            'Label2
            '
            Me.Label2.AutoSize = True
            Me.Label2.FlatStyle = System.Windows.Forms.FlatStyle.System
            Me.Label2.Location = New System.Drawing.Point(14, 66)
            Me.Label2.Name = "Label2"
            Me.Label2.Size = New System.Drawing.Size(55, 13)
            Me.Label2.TabIndex = 1
            Me.Label2.Text = "Port States"
            '
            'MultiPortEditorControl
            '
            Me.Controls.Add(Me.Label2)
            Me.Controls.Add(Me.Label1)
            Me.Font = New System.Drawing.Font("Tahoma", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Name = "MultiPortEditorControl"
            Me.Size = New System.Drawing.Size(305, 120)
            Me.ResumeLayout(False)
            Me.PerformLayout()

        End Sub

#End Region

        Private _chkPortMask(15) As CheckBox
        Private _chkPortState(15) As CheckBox
        Private _sctConfiguration As PgLocoIO.MultiPortConfig

        Private Sub InitializeComponentExt()
            Me.SuspendLayout()
            Dim int4GroupOffset As Integer
            For bytIdx As Byte = 0 To 15
                int4GroupOffset = Math.Floor((bytIdx) / 4) * 8

                _chkPortMask(bytIdx) = New CheckBox
                With _chkPortMask(bytIdx)
                    .FlatStyle = System.Windows.Forms.FlatStyle.System
                    .Size = New System.Drawing.Size(16, 16)
                    .Location = New System.Drawing.Point(14 + bytIdx * 16 + int4GroupOffset, 32)
                    .Tag = bytIdx
                End With
                Me.Controls.Add(_chkPortMask(bytIdx))
                AddHandler _chkPortMask(bytIdx).Click, AddressOf MaskCheckBox_Click

                _chkPortState(bytIdx) = New CheckBox
                With _chkPortState(bytIdx)
                    .FlatStyle = System.Windows.Forms.FlatStyle.System
                    .Size = New System.Drawing.Size(16, 16)
                    .Location = New System.Drawing.Point(14 + bytIdx * 16 + int4GroupOffset, 88)
                    .Tag = bytIdx
                End With
                Me.Controls.Add(_chkPortState(bytIdx))
                AddHandler _chkPortState(bytIdx).Click, AddressOf StateCheckBox_Click
            Next
            Me.ResumeLayout(False)
        End Sub

        Public Property Configuration() As PgLocoIO.MultiPortConfig
            Get
                Return _sctConfiguration
            End Get
            Set(Value As PgLocoIO.MultiPortConfig)
                _sctConfiguration = Value
            End Set
        End Property

        Private Sub MultiPortEditorControl_Load(sender As Object,  e As System.EventArgs) Handles MyBase.Load
            Me.SuspendLayout()
            For bytIdx As Byte = 0 To 15
                _chkPortMask(bytIdx).Checked = _sctConfiguration.PortMask(bytIdx)
                _chkPortState(bytIdx).Checked = _sctConfiguration.PortState(bytIdx)
                _chkPortState(bytIdx).Enabled = _sctConfiguration.PortMask(bytIdx)
            Next
            Me.ResumeLayout(False)
        End Sub

        Private Sub MaskCheckBox_Click(sender As Object,  e As System.EventArgs)
            _sctConfiguration.PortMask(sender.tag) = sender.Checked
            _chkPortState(sender.tag).Enabled = sender.Checked
        End Sub

        Private Sub StateCheckBox_Click(sender As Object,  e As System.EventArgs)
            _sctConfiguration.PortState(sender.tag) = sender.Checked
        End Sub

        Private Sub TerminateComponent()
            For bytIdx As Byte = 0 To 15
                RemoveHandler _chkPortMask(bytIdx).Click, AddressOf MaskCheckBox_Click
                RemoveHandler _chkPortState(bytIdx).Click, AddressOf StateCheckBox_Click
            Next
        End Sub
    End Class

End Namespace