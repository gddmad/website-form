Module Wishes
    Public Color_Wishes() As Short = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	Public What_Year As Short = Year(DateTime.Now)
	Public What_Month As Short = Month(DateTime.Now)
	Public JanuaryFirst As Short = Val(DateTime.Now.DayOfWeek)
	Public MonthFirst As Short = Val(DateTime.Now.DayOfWeek)
	Public TESTING As Short = 1
	Public Where_Directory As String = "c:\Gregory\Website\"

	Sub Get_Wishes()
        Dim looping As Short = 0
        Dim File_Ini As String
        Dim Whatisinfile As String = ""
		File_Ini = Where_Directory + "Wishes.ini"
		If Not (System.IO.File.Exists(File_Ini)) Then TESTING = 10
        If TESTING <> 10 Then
            Dim ReadWish As New System.IO.StreamReader(File_Ini)
            Whatisinfile = ReadWish.ReadLine()
            If Whatisinfile = "[COLOR CHOICE]" Then
                For looping = 0 To 9
                    Color_Wishes(looping) = Val(Chr(ReadWish.Read()))
                Next looping
            End If
            For looping = 1 To 3
                Whatisinfile = ReadWish.ReadLine()
            Next looping
            If Whatisinfile = "[TESTING]" Then TESTING = ReadWish.ReadLine()
            For looping = 1 To 2
                Whatisinfile = ReadWish.ReadLine()
            Next looping
            If Whatisinfile = "[WORKING YEAR]" Then What_Year = ReadWish.ReadLine()
            ReadWish.Close()
        End If
        If TESTING = 10 Then TESTING = 1
    End Sub

    Sub Save_Wishes()
        Dim Looping As Short = 0
        Dim File_Ini As String = ""

        File_Ini = Where_Directory + "Wishes.ini"
        Dim WriteWish As New System.IO.StreamWriter(File_Ini)
        WriteWish.WriteLine("[COLOR CHOICE]")
        For Looping = 0 To 9
            If Looping = 9 Then
                WriteWish.WriteLine(CType(Color_Wishes(Looping), String))
            Else
                WriteWish.Write(CType(Color_Wishes(Looping), String))
            End If
        Next Looping
        WriteWish.WriteLine("")
        WriteWish.WriteLine("[TESTING]")
        WriteWish.WriteLine(TESTING)
        WriteWish.WriteLine("")
        WriteWish.WriteLine("[WORKING YEAR]")
		WriteWish.WriteLine(What_Year)
		WriteWish.WriteLine("[WORKING MONTH]")
		WriteWish.WriteLine(What_Month)
		WriteWish.Close()
    End Sub
End Module
