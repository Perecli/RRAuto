Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports System.ComponentModel
Imports RRAuto.PropertyGridHelpers
Imports System.Collections.Specialized

Namespace Windows

    Partial Public Class DccProg

        Public Sub New()

            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            Me.PropertyGrid.SelectedObject = New PgDccConfig
        End Sub

        Private Sub cmdWrite_Click(sender As Object, e As RoutedEventArgs) Handles cmdWrite.Click
            With Me.PropertyGrid.SelectedObject
                CtcService.LoconetService.TxPacket(New PkDccProgram(True, .ProgMode, .OpAddress, .CvNumber, .CvValue))
            End With
        End Sub

        <TypeConverter(GetType(DynamicPropGridConverter))> Private Class PgDccConfig
            Implements IDynamicPropertyGrid

            Private _enuProgMode As DccProgMode = DccProgMode.OperByteNoFeedback
            Private _srtOpAddress As Short = 3
            Private _srtCvNumber As Short = 1
            Private _bytCvValue As Byte = 0

            Public Property ProgMode() As DccProgMode
                Get
                    Return _enuProgMode
                End Get
                Set(value As DccProgMode)
                    _enuProgMode = value
                End Set
            End Property

            Public Property OpAddress() As Short
                Get
                    Return _srtOpAddress
                End Get
                Set(value As Short)
                    _srtOpAddress = value
                End Set
            End Property

            Public Property CvNumber() As Short
                Get
                    Return _srtCvNumber
                End Get
                Set(value As Short)
                    _srtCvNumber = value
                End Set
            End Property

            Public Property CvValue() As Byte
                Get
                    Return _bytCvValue
                End Get
                Set(value As Byte)
                    _bytCvValue = value
                End Set
            End Property

            Public Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
                Return New String() {"ProgMode", "OpAddress", "CvNumber", "CvValue"}
            End Function

            Public Function PropGridDynamicAttrib() As System.Collections.Specialized.ListDictionary Implements PropertyGridHelpers.IDynamicPropertyGrid.PropGridDynamicAttrib
                Dim objList As New ListDictionary

                Dim objaProgMode As Attribute() = {New DescriptionAttribute("The programming mode to be used.")}
                Dim objaOpAddress As Attribute() = {New DescriptionAttribute("The address of the decoder to be programmed. Applies only to operational programming modes.")}
                Dim objaCvNumber As Attribute() = {New DescriptionAttribute("The decoder configuration number to be programmed.")}
                Dim objaCvValue As Attribute() = {New DescriptionAttribute("The value to be programmed in the given configuration number.")}

                objList.Add("ProgMode", New DynamicDescriptor("Prog Mode", objaProgMode))
                objList.Add("OpAddress", New DynamicDescriptor("Op Address", objaOpAddress))
                objList.Add("CvNumber", New DynamicDescriptor("Cv Number", objaCvNumber))
                objList.Add("CvValue", New DynamicDescriptor("Cv Value", objaCvValue))
                Return objList
            End Function
        End Class

    End Class

End Namespace