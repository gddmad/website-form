Public Class Website

    Public Directory_GO As Boolean = False
    Public Each_Choice_GO As Boolean = False

    Private Sub YearPicker_ValueChanged(sender As Object, e As EventArgs) Handles YearPicker.ValueChanged, JanuaryMonth.Validated
		Dim Set_Date_Year As DateTime = YearPicker.Value
		What_Year = Year(YearPicker.Value)
		Dim setcurrentdate As Date = DateValue(What_Month.ToString & "/1/" & What_Year.ToString)
		JanuaryFirst = Val(Set_Date_Year.DayOfWeek)
		MonthFirst = Val(setcurrentdate.DayOfWeek)
		DayLabel.Text = Day_Long(JanuaryFirst)
		Month_Day.Text = Day_Long(MonthFirst)
		JanuaryMonth.TodayDate = YearPicker.Value
		JanuaryMonth.SetDate(YearPicker.Value)
		Current_Month.SetDate(setcurrentdate)
		JanuaryMonth.Update()
		Current_Month.Update()
	End Sub

    Private Sub Website_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim Set_Date_Year As DateTime = "1/1/" & CType(What_Year, String)
        YearPicker.Value = Set_Date_Year
        Directory.Text = Where_Directory
        If TESTING = 1 Then
            Testing_Button.Text = "TESTING"
            Testing_Button.BackColor = Color.OrangeRed
        End If
        If TESTING = 0 Then
            Testing_Button.Text = "WEBSITE"
            Testing_Button.BackColor = Color.GreenYellow
        End If
    End Sub

    Private Sub PinkComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles PinkComboBox.SelectedIndexChanged
        Color_Wishes(0) = PinkComboBox.SelectedIndex
    End Sub

    Private Sub RedComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles RedComboBox.SelectedIndexChanged
        Color_Wishes(1) = RedComboBox.SelectedIndex
    End Sub

    Private Sub OrangeComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles OrangeComboBox.SelectedIndexChanged
        Color_Wishes(2) = OrangeComboBox.SelectedIndex
    End Sub

    Private Sub YellowComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles YellowComboBox.SelectedIndexChanged
        Color_Wishes(3) = YellowComboBox.SelectedIndex
    End Sub

    Private Sub GreenComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles GreenComboBox.SelectedIndexChanged
        Color_Wishes(4) = GreenComboBox.SelectedIndex
    End Sub

    Private Sub BlueComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles BlueComboBox.SelectedIndexChanged
        Color_Wishes(5) = BlueComboBox.SelectedIndex
    End Sub

    Private Sub PurpleComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles PurpleComboBox.SelectedIndexChanged
        Color_Wishes(6) = PurpleComboBox.SelectedIndex
    End Sub

    Private Sub BrownComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles BrownComboBox.SelectedIndexChanged
        Color_Wishes(7) = BrownComboBox.SelectedIndex
    End Sub

    Private Sub GrayComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles GrayComboBox.SelectedIndexChanged
        Color_Wishes(8) = GrayComboBox.SelectedIndex
    End Sub

    Private Sub BlackComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles BlackComboBox.SelectedIndexChanged
        Color_Wishes(9) = BlackComboBox.SelectedIndex
    End Sub

    Private Sub PinkGroup_Layout(sender As Object, e As LayoutEventArgs) Handles PinkGroup.Layout
        PinkComboBox.SelectedIndex = Color_Wishes(0)
    End Sub

    Private Sub RedGroup_Layout(sender As Object, e As LayoutEventArgs) Handles RedGroup.Layout
        RedComboBox.SelectedIndex = Color_Wishes(1)
    End Sub

    Private Sub OrangeGroup_Layout(sender As Object, e As EventArgs) Handles OrangeGroup.Layout
        OrangeComboBox.SelectedIndex = Color_Wishes(2)
    End Sub

    Private Sub YellowGroup_Layout(sender As Object, e As EventArgs) Handles YellowGroup.Layout
        YellowComboBox.SelectedIndex = Color_Wishes(3)
    End Sub

    Private Sub GreenGroup_Layout(sender As Object, e As EventArgs) Handles GreenGroup.Layout
        GreenComboBox.SelectedIndex = Color_Wishes(4)
    End Sub

    Private Sub BlueGroup_Layout(sender As Object, e As EventArgs) Handles BlueGroup.Layout
        BlueComboBox.SelectedIndex = Color_Wishes(5)
    End Sub

    Private Sub PurpleGroup_Layout(sender As Object, e As EventArgs) Handles PurpleGroup.Layout
        PurpleComboBox.SelectedIndex = Color_Wishes(6)
    End Sub

    Private Sub BrownGroup_Layout(sender As Object, e As EventArgs) Handles BrownGroup.Layout
        BrownComboBox.SelectedIndex = Color_Wishes(7)
    End Sub

    Private Sub GrayGroup_Layout(sender As Object, e As LayoutEventArgs) Handles GrayGroup.Layout
        GrayComboBox.SelectedIndex = Color_Wishes(8)
    End Sub

    Private Sub BlackGroup_Layout(sender As Object, e As EventArgs) Handles BlackGroup.Layout
        BlackComboBox.SelectedIndex = Color_Wishes(9)
    End Sub

    Private Sub SaveQuit_Click(sender As Object, e As EventArgs) Handles SaveQuit.Click
        Save_Wishes()
        Me.Close()
    End Sub

    Private Sub Website_Activated(sender As Object, e As EventArgs) Handles MyBase.Activated
        Get_Wishes()
    End Sub

    Private Sub TextBox1_Enter(sender As Object, e As EventArgs) Handles Directory.Enter
        Directory_GO = False
        Each_Choice_GO = False
        If FolderBrowserDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Where_Directory = FolderBrowserDialog1.SelectedPath + "\"
            Directory.Text = Where_Directory
        End If
    End Sub

    Private Sub TextBox1_Enter(sender As Object, e As MouseEventArgs) Handles Directory.MouseClick, Directory.Enter
        Directory_GO = False
        Each_Choice_GO = False
        If FolderBrowserDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Where_Directory = FolderBrowserDialog1.SelectedPath + "\"
            Directory.Text = Where_Directory
        End If
    End Sub

    Private Sub Checking_Click(sender As Object, e As EventArgs) Handles Checking.Click
        Dim itworked As Boolean = False
        Dim Looping As Short
        Dim Path_Name As String = ""
        Dim Answer As String = ""
        Dim Counting As Short = 0
        GO_Button.Enabled = False
        Check_Website.Enabled = False
        For Looping = 0 To 9
            Path_Name = Where_Directory + Name_Colors(Looping)
            If System.IO.Directory.Exists(Path_Name) Then Answer = "READY" Else Answer = "NOT READY"
            If Answer = "READY" Then Counting += 1
            Select Case Looping
                Case 0
                    PinkLabel.ForeColor = Color.Red
                    PinkLabel.Text = Answer
                Case 1
                    RedLabel.ForeColor = Color.Red
                    RedLabel.Text = Answer
                Case 2
                    OrangeLabel.ForeColor = Color.Red
                    OrangeLabel.Text = Answer
                Case 3
                    YellowLabel.ForeColor = Color.Red
                    YellowLabel.Text = Answer
                Case 4
                    GreenLabel.ForeColor = Color.Red
                    GreenLabel.Text = Answer
                Case 5
                    BlueLabel.ForeColor = Color.Red
                    BlueLabel.Text = Answer
                Case 6
                    PurpleLabel.ForeColor = Color.Red
                    PurpleLabel.Text = Answer
                Case 7
                    BrownLabel.ForeColor = Color.Red
                    BrownLabel.Text = Answer
                Case 8
                    GrayLabel.ForeColor = Color.Red
                    GrayLabel.Text = Answer
                Case 9
                    BlackLabel.ForeColor = Color.Red
                    BlackLabel.Text = Answer
            End Select
        Next Looping
        If Counting = 10 Then itworked = True
        If Counting = 10 Then Check_Website.Enabled = True
        Directory_GO = itworked
    End Sub

    Private Sub Fix_It_Button_Click(sender As Object, e As EventArgs) Handles Fix_It_Button.Click
        If Directory_GO = False Then
            User_Box.Text = "Do you want me to create 10 color directories?"
            YES_NO.Visible = True
        End If
        If Directory_GO = True And Each_Choice_GO = False Then
            User_Box.Text = "Do you want me to create sample files?"
            YES_NO.Visible = True
        End If
    End Sub

    Private Sub Yes_Button_Click(sender As Object, e As EventArgs) Handles Yes_Button.Click
        Dim Looping As Short
        Dim Path_Name As String = ""
        Dim Answer As String = ""
        Dim End_File As String = ""
        Dim File_Name As String = ""
        Dim Number As Short
        Dim Fill_Me_In As String
        If Directory_GO = False Then
            YES_NO.Visible = False
            User_Box.Text = ""
            For Looping = 0 To 9
                Path_Name = Where_Directory + Name_Colors(Looping)
                System.IO.Directory.CreateDirectory(Path_Name)
                If System.IO.Directory.Exists(Path_Name) Then Answer = "CREATED" Else Answer = "ERROR"
                Select Case Looping
                    Case 0
                        PinkLabel.ForeColor = Color.DarkGreen
                        PinkLabel.Text = Answer
                    Case 1
                        RedLabel.ForeColor = Color.DarkGreen
                        RedLabel.Text = Answer
                    Case 2
                        OrangeLabel.ForeColor = Color.DarkGreen
                        OrangeLabel.Text = Answer
                    Case 3
                        YellowLabel.ForeColor = Color.DarkGreen
                        YellowLabel.Text = Answer
                    Case 4
                        GreenLabel.ForeColor = Color.DarkGreen
                        GreenLabel.Text = Answer
                    Case 5
                        BlueLabel.ForeColor = Color.DarkGreen
                        BlueLabel.Text = Answer
                    Case 6
                        PurpleLabel.ForeColor = Color.DarkGreen
                        PurpleLabel.Text = Answer
                    Case 7
                        BrownLabel.ForeColor = Color.DarkGreen
                        BrownLabel.Text = Answer
                    Case 8
                        GrayLabel.ForeColor = Color.DarkGreen
                        GrayLabel.Text = Answer
                    Case 9
                        BlackLabel.ForeColor = Color.DarkGreen
                        BlackLabel.Text = Answer
                End Select
            Next Looping
        End If
        If Directory_GO = True And Each_Choice_GO = False Then
            YES_NO.Visible = False
            User_Box.Text = ""
            For Looping = 0 To 9
                Select Case Color_Wishes(Looping)
                    Case 0 To 1
                        Answer = "READY"
                    Case 2
                        Answer = "Created"
                        End_File = ".links"
                        File_Name = Where_Directory + Name_Colors(Looping) + End_File
                        If System.IO.File.Exists(File_Name) Then Back_UP(File_Name)
                        Dim Sample As New System.IO.StreamWriter(File_Name)
                        Number = 3      ' 3 for a three columns as sample
                        Sample.WriteLine(Number)
                        Fill_Me_In = "H,7,MY LINKS"
                        Sample.WriteLine(Fill_Me_In)
                        Fill_Me_In = "T,www.gregorydavidson.com,Gregory Davidson's Website"
                        Sample.WriteLine(Fill_Me_In)
                        Fill_Me_In = "T,www.facebook.com,Facebook"
                        Sample.WriteLine(Fill_Me_In)
                        Fill_Me_In = "blank"
                        Sample.WriteLine(Fill_Me_In)
                        Fill_Me_In = "T,smile.amazon.com,Smile Amazon"
                        Sample.WriteLine(Fill_Me_In)
                        Sample.WriteLine("END")
                        Sample.Close()
                    Case 3
                        Answer = "UNKNOWN"
                        End_File = ".custom"
                        File_Name = Where_Directory + Name_Colors(Looping) + End_File
                        If System.IO.File.Exists(File_Name) Then Back_UP(File_Name)
                        Dim Sample As New System.IO.StreamWriter(File_Name)
                        Sample.WriteLine("UNKNOWN")
                        Sample.Close()
                End Select
                Select Case Looping
                    Case 0
                        PinkLabel.ForeColor = Color.Blue
                        PinkLabel.Text = Answer
                    Case 1
                        RedLabel.ForeColor = Color.Blue
                        RedLabel.Text = Answer
                    Case 2
                        OrangeLabel.ForeColor = Color.Blue
                        OrangeLabel.Text = Answer
                    Case 3
                        YellowLabel.ForeColor = Color.Blue
                        YellowLabel.Text = Answer
                    Case 4
                        GreenLabel.ForeColor = Color.Blue
                        GreenLabel.Text = Answer
                    Case 5
                        BlueLabel.ForeColor = Color.Blue
                        BlueLabel.Text = Answer
                    Case 6
                        PurpleLabel.ForeColor = Color.Blue
                        PurpleLabel.Text = Answer
                    Case 7
                        BrownLabel.ForeColor = Color.Blue
                        BrownLabel.Text = Answer
                    Case 8
                        GrayLabel.ForeColor = Color.Blue
                        GrayLabel.Text = Answer
                    Case 9
                        BlackLabel.ForeColor = Color.Blue
                        BlackLabel.Text = Answer
                End Select
            Next Looping
        End If
    End Sub

    Private Sub No_Button_Click(sender As Object, e As EventArgs) Handles No_Button.Click
        YES_NO.Visible = False
        User_Box.Text = ""
    End Sub

    Private Sub Check_Website_Click(sender As Object, e As EventArgs) Handles Check_Website.Click
        Dim looping As Short = 0
        Dim Answer As String = ""
        Dim Counting As Short = 0
        Dim End_File As String = ""
        Dim File_Name As String = ""
        Each_Choice_GO = False
        GO_Button.Enabled = False
        For looping = 0 To 9
            Answer = "NOT READY"
            Select Case Color_Wishes(looping)
                Case 0
                    Answer = "READY"
				Case 1
					Answer = "READY"
				Case 2
					Answer = "READY"
				Case 3
					End_File = ".links"
                    File_Name = Where_Directory + Name_Colors(looping) + End_File
                    If System.IO.File.Exists(File_Name) Then Answer = "READY" Else Answer = "NOT READY"
				Case 4
					End_File = ".custom"
            End Select
            Select Case looping
                Case 0
                    If Answer = "READY" Then
                        PinkLabel.ForeColor = Color.Brown
                        PinkLabel.Text = Answer
                    Else
                        PinkLabel.ForeColor = Color.Red
                        PinkLabel.Text = "FAILED"
                    End If
                Case 1
                    If Answer = "READY" Then
                        RedLabel.ForeColor = Color.Brown
                        RedLabel.Text = Answer
                    Else
                        RedLabel.ForeColor = Color.Red
                        RedLabel.Text = "FAILED"
                    End If
                Case 2
                    If Answer = "READY" Then
                        OrangeLabel.ForeColor = Color.Brown
                        OrangeLabel.Text = Answer
                    Else
                        OrangeLabel.ForeColor = Color.Red
                        OrangeLabel.Text = "FAILED"
                    End If
                Case 3
                    If Answer = "READY" Then
                        YellowLabel.ForeColor = Color.Brown
                        YellowLabel.Text = Answer
                    Else
                        YellowLabel.ForeColor = Color.Red
                        YellowLabel.Text = "FAILED"
                    End If
                Case 4
                    If Answer = "READY" Then
                        GreenLabel.ForeColor = Color.Brown
                        GreenLabel.Text = Answer
                    Else
                        GreenLabel.ForeColor = Color.Red
                        GreenLabel.Text = "FAILED"
                    End If
                Case 5
                    If Answer = "READY" Then
                        BlueLabel.ForeColor = Color.Brown
                        BlueLabel.Text = Answer
                    Else
                        BlueLabel.ForeColor = Color.Red
                        BlueLabel.Text = "FAILED"
                    End If
                Case 6
                    If Answer = "READY" Then
                        PurpleLabel.ForeColor = Color.Brown
                        PurpleLabel.Text = Answer
                    Else
                        PurpleLabel.ForeColor = Color.Red
                        PurpleLabel.Text = "FAILED"
                    End If
                Case 7
                    If Answer = "READY" Then
                        BrownLabel.ForeColor = Color.Brown
                        BrownLabel.Text = Answer
                    Else
                        BrownLabel.ForeColor = Color.Red
                        BrownLabel.Text = "FAILED"
                    End If
                Case 8
                    If Answer = "READY" Then
                        GrayLabel.ForeColor = Color.Brown
                        GrayLabel.Text = Answer
                    Else
                        GrayLabel.ForeColor = Color.Red
                        GrayLabel.Text = "FAILED"
                    End If
                Case 9
                    If Answer = "READY" Then
                        BlackLabel.ForeColor = Color.Brown
                        BlackLabel.Text = Answer
                    Else
                        BlackLabel.ForeColor = Color.Red
                        BlackLabel.Text = "FAILED"
                    End If
            End Select
            If Answer = "READY" Then Counting += 1
        Next looping
        If Counting = 10 Then
            Each_Choice_GO = True
            GO_Button.Enabled = True
        End If
    End Sub

    Private Sub GO_Button_Click(sender As Object, e As EventArgs) Handles GO_Button.Click
        Dim Isitgo As Boolean
        Dim Looping As Short
        Dim Yes_Idle As Boolean = False
        For Looping = 0 To 9
            Yes_Idle = False
            Select Case Color_Wishes(Looping)
                Case 0
                    Yes_Idle = True
                Case 1
					Isitgo = Make_Calendar(Looping)
				Case 2
					Isitgo = BIGandLittle(Looping)
				Case 3
					Isitgo = Make_Link(Looping)
				Case 4
					Isitgo = Make_Custom(Looping)
            End Select
            Select Case Looping
                Case 0
                    If Isitgo = False Then
                        PinkLabel.ForeColor = Color.Red
                        PinkLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        PinkLabel.ForeColor = Color.DarkOliveGreen
                        PinkLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        PinkLabel.ForeColor = Color.Violet
                        PinkLabel.Text = "IDLE"
                    End If
                Case 1
                    If Isitgo = False Then
                        RedLabel.ForeColor = Color.Red
                        RedLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        RedLabel.ForeColor = Color.DarkOliveGreen
                        RedLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        RedLabel.ForeColor = Color.Violet
                        RedLabel.Text = "IDLE"
                    End If
                Case 2
                    If Isitgo = False Then
                        OrangeLabel.ForeColor = Color.Red
                        OrangeLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        OrangeLabel.ForeColor = Color.DarkOliveGreen
                        OrangeLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        OrangeLabel.ForeColor = Color.Violet
                        OrangeLabel.Text = "IDLE"
                    End If
                Case 3
                    If Isitgo = False Then
                        YellowLabel.ForeColor = Color.Red
                        YellowLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        YellowLabel.ForeColor = Color.DarkOliveGreen
                        YellowLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        YellowLabel.ForeColor = Color.Violet
                        YellowLabel.Text = "IDLE"
                    End If
                Case 4
                    If Isitgo = False Then
                        GreenLabel.ForeColor = Color.Red
                        GreenLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        GreenLabel.ForeColor = Color.DarkOliveGreen
                        GreenLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        GreenLabel.ForeColor = Color.Violet
                        GreenLabel.Text = "IDLE"
                    End If
                Case 5
                    If Isitgo = False Then
                        BlueLabel.ForeColor = Color.Red
                        BlueLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        BlueLabel.ForeColor = Color.DarkOliveGreen
                        BlueLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        BlueLabel.ForeColor = Color.Violet
                        BlueLabel.Text = "IDLE"
                    End If
                Case 6
                    If Isitgo = False Then
                        PurpleLabel.ForeColor = Color.Red
                        PurpleLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        PurpleLabel.ForeColor = Color.DarkOliveGreen
                        PurpleLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        PurpleLabel.ForeColor = Color.Violet
                        PurpleLabel.Text = "IDLE"
                    End If
                Case 7
                    If Isitgo = False Then
                        BrownLabel.ForeColor = Color.Red
                        BrownLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        BrownLabel.ForeColor = Color.DarkOliveGreen
                        BrownLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        BrownLabel.ForeColor = Color.Violet
                        BrownLabel.Text = "IDLE"
                    End If
                Case 8
                    If Isitgo = False Then
                        GrayLabel.ForeColor = Color.Red
                        GrayLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        GrayLabel.ForeColor = Color.DarkOliveGreen
                        GrayLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        GrayLabel.ForeColor = Color.Violet
                        GrayLabel.Text = "IDLE"
                    End If
                Case 9
                    If Isitgo = False Then
                        BlackLabel.ForeColor = Color.Red
                        BlackLabel.Text = "FAILED"
                    End If
                    If Isitgo = True Then
                        BlackLabel.ForeColor = Color.DarkOliveGreen
                        BlackLabel.Text = "DONE"
                    End If
                    If Yes_Idle = True Then
                        BlackLabel.ForeColor = Color.Violet
                        BlackLabel.Text = "IDLE"
                    End If
            End Select
        Next Looping
    End Sub

    Private Sub Directory_TextChanged(sender As Object, e As EventArgs) Handles Directory.TextChanged
        GO_Button.Enabled = False
        Check_Website.Enabled = False
    End Sub

    Private Sub Testing_Button_Click(sender As Object, e As EventArgs) Handles Testing_Button.Click
        If Testing_Button.Text = "TESTING" Then Testing_Button.Text = "WEBSITE" Else Testing_Button.Text = "TESTING"
        If Testing_Button.Text = "TESTING" Then
            Testing_Button.BackColor = Color.OrangeRed
            TESTING = 1
        End If
        If Testing_Button.Text = "WEBSITE" Then
            Testing_Button.BackColor = Color.GreenYellow
            TESTING = 0
        End If
    End Sub

    Private Sub Back_UP(ByVal Please As String)
        Dim Original As String = Please
        Dim New_File As String = Please
        Dim Fixed As String = "000"
        Dim Count_Up As Integer = -1
        Dim Before As String = ""
        Dim Yes_Number As Boolean = False
        Dim zz As Integer = 0
        Dim cc As Integer = Len(Original)
        Dim story As String = ""
        Do
            zz = zz + 1
            If Mid$(Original, zz, 1) = "." Then Yes_Number = True
        Loop Until Yes_Number = True
        Yes_Number = False
        Before = Original.Substring(0, zz)
        Do Until Yes_Number = True
            If System.IO.File.Exists(New_File) Then Count_Up += 1
            If Not (System.IO.File.Exists(New_File)) Then Yes_Number = True
            Fixed = LTrim$(Str$(Count_Up))
            If Count_Up < 100 Then Fixed = "0" + LTrim$(Str$(Count_Up))
            If Count_Up < 10 Then Fixed = "00" + LTrim$(Str$(Count_Up))
            New_File = Before + Fixed
        Loop
        Dim Input As New System.IO.StreamReader(Original)
        Dim Output As New System.IO.StreamWriter(New_File)
        Do While Input.Peek() >= 0
            story = Input.ReadLine()
            Output.WriteLine(story)
        Loop
        Input.Close()
        Output.Close()
    End Sub

	Private Sub JanuaryMonth_DateChanged(sender As Object, e As DateRangeEventArgs) Handles JanuaryMonth.DateChanged

	End Sub

	Private Sub MonthCalendar1_DateChanged(sender As Object, e As DateRangeEventArgs) Handles Current_Month.DateChanged

	End Sub

	Private Sub Label1_Click(sender As Object, e As EventArgs) Handles MonthLabel.Click

	End Sub

	Private Sub MonthPicker_ValueChanged(sender As Object, e As EventArgs)

	End Sub

	Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles Month_Pick.SelectedIndexChanged, Current_Month.Validated

		What_Year = Year(YearPicker.Value)
		What_Month = 1 + Month_Pick.SelectedIndex
		Dim setcurrentdate As Date = DateValue(What_Month.ToString & "/1/" & What_Year.ToString)
		MonthFirst = Val(setcurrentdate.DayOfWeek)
		Month_Day.Text = Day_Long(MonthFirst)
		Current_Month.SetDate(setcurrentdate)
		Current_Month.Update()

	End Sub

	Private Sub Current_Month_Label_Click(sender As Object, e As EventArgs) Handles Current_Month_Label.Click

	End Sub
End Class
