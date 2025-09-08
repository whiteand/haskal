{$DEFINE SHOULD_LOG_NOT}

Program ChessSolver;

Uses crt;

Type 
  PlayerColor = (PlayerColorWhite, PlayerColorBlack);
  TCmdArgs = 
             Record
               ShowHelp: Boolean;
               Fen: string;
               Moves: longint;
               Color: PlayerColor;
               OutputFileName: string;
               BufferSize: longint;
               A1IsAtTheLeftBottomCorner: boolean;
               MovesGroupSize: integer;
               ShowEnemyMoves: boolean;
             End;
  Move = Record
    iStart: integer;
    iEnd: integer;
    jStart: integer;
    jEnd: integer;
    figureStart: integer;
    figureEnd: integer;
  End;
  mas = array [1..1000] Of Move;

Const n        = 8;
  peshka   = 1;
  loshad   = 2;
  officer  = 3;
  ladya    = 4;
  ferz     = 5;
  korol    = 6;
  bpeshka   = -peshka;
  bloshad   = -loshad;
  bofficer  = -officer;
  bladya    = -ladya;
  bferz     = -ferz;
  bkorol    = -korol;
  white    =  1;
  black    = -1;
  isLog = false;

Type TBoard = array [1..n, 1..n] Of shortint;

Type TBuffer = array [1..20000] Of string;

Var cmdArgs: TCmdArgs;
  board: TBoard;
  moves: mas;
  countOfPossibleMoves: longint;
  isCheckToWhite, isCheckToBlack: boolean;
  MakedMoves: mas;
  countOfMakedMoves: longint = 0;
  cVariants: int64 = 0;
  cSolving: int64 = 0;
  maxcountofpossibleMoves: int64 = 0;
  buffer: TBuffer;
  buffercursor: longint = 0;
  orientation: integer;
  lastMakedMoves: mas;
  operator = (a,b:Move)z: boolean;
Begin
  Exit(
       (a.iStart = b.iStart) and
  (a.jStart = b.jStart) and
  (a.iEnd = b.iEnd) and
  (a.jEnd = b.jEnd) and
  (a.figureStart = b.figureStart)
  );
End;
operator <>(a,b:Move)z: boolean;
Begin
  z := Not (a = b);
End;

Procedure ShowChessBoard(
  Var board: TBoard
);

Const COLOR_WHITE     = 15;
  COLOR_BLACK     = 0;
  COLOR_BLUE      = 1;
  COLOR_DARK_GRAY = 8;

Var r: integer;
  c: integer;
  cell: longint;
Begin
  For r := 1 To n Do
    Begin
      For c := 1 To n Do
        Begin
          cell := board[r, c];
          If (cell = 0) Then
            Begin
              TextColor(COLOR_DARK_GRAY);
              If ((r + c) Mod 2 = 0)
                Then write('▫ ')
              Else write('▪ ');
              TextColor(COLOR_WHITE)
            End
          Else Case cell Of 
                 bpeshka:
                      Begin
                        TextColor(Blue);
                        write('♙ ');
                        TextColor(COLOR_WHITE)
                      End;
                 peshka:
                         Begin
                           write('♟ ');
                         End;
                 bladya:
                         Begin
                           TextColor(Blue);
                           write('♖ ');
                           TextColor(COLOR_WHITE)
                         End;
                 ladya:
                        Begin
                          write('♜ ');
                        End;
                 bloshad:
                          Begin
                            TextColor(Blue);
                            write('♘ ');
                            TextColor(COLOR_WHITE)
                          End;
                 loshad:
                         Begin
                           write('♞ ');
                         End;
                 officer:
                          Begin
                            write('♝ ');
                          End;
                 bofficer:
                           Begin
                             TextColor(Blue);
                             write('♗ ');
                             TextColor(COLOR_WHITE)
                           End;
                 ferz:
                       Begin
                         write('♛ ');
                       End;
                 bferz:
                        Begin
                          TextColor(Blue);
                          write('♕ ');
                          TextColor(COLOR_WHITE)
                        End;
                 korol:
                        Begin
                          write('♚ ');
                        End;
                 bkorol:
                         Begin
                           TextColor(Blue);
                           write('♔ ');
                           TextColor(COLOR_WHITE)
                         End;
                 Else write('?');
            End;
        End;
      writeln;
    End;
End;

Procedure ShowHelp;
Begin
  writeln(
'Usage: ChessSolver --fen <fen-notation> -o <output-file-path> [--color <white|black>] [--moves <moves>] [--buffer-size <buffer size>] [--a1-at-top-right] [--moves-group-size <group-moves-size>] [--show-enemy-moves]'
  );
  writeln('Example:');
  writeln('  ChessSolver --fen r1b4k/b6p/2pp1r2/pp6/4P3/PBNP2R1/1PP3PP/7K --moves 1 --color white');
  writeln('Options:');
  writeln(
'  --fen              - Fen notation string describing the chess board. Read more: http://www.ee.unb.ca/cgi-bin/tervo/fen.pl'
  );
  writeln('  -o                 - File into which the output will be written');
  writeln(
'  --color            - Color of the player that should win. Possible values: white | black. Default is white'
  );
  writeln('  --moves            - Number of expected moves for checkmate to be found');
  writeln('  --buffer-size      - No one knows what is this parameter, TBD. Default to 10000');
  writeln('  --a1-at-top-right  - Should show A1 at the top right corner. Defaults to false');
  writeln(
'  --moves-group-size - How many moves should be grouped together. Defaults to 0 (means no grouping).'
  );
  writeln('  --show-enemy-moves - Shows enemy moves in the output. Defaults to false.');
End;
Function ParseArguments(Var args: TCmdArgs): boolean;

Var i: integer;
  argument: string;
  expectsFenString: boolean = false;
  expectsMoves: boolean = false;
  expectsBufferSize: boolean = false;
  expectsMovesGroupSize: boolean = false;
  expectsColor: boolean = false;
  expectsOutputFileName: boolean = false;
  fenArgumentProvided: boolean = false;
  outputFileNameProvided: boolean = false;
  bufferSizeProvided: boolean = false;
  code: Shortint;
Begin
  args.A1IsAtTheLeftBottomCorner := true;
  args.Color := PlayerColorWhite;

  For i := 1 To ParamCount() Do
    Begin
      argument := ParamStr(i);
      If (argument = '--help') Then
        Begin
          args.ShowHelp := true;
          Exit(true);
        End
      Else If (argument = '--fen') Then expectsFenString := true
      Else If (expectsFenString) Then
             Begin
               If Length(argument) < 15 Then
                 Begin
                   ShowHelp;
                   writeln('ERROR: Expected --fen <fen-notation-string>, but ', argument,
                           ' occurred');
                   Exit(false);
                 End;
            {TODO: Add validation of fen string}
               fenArgumentProvided := true;
               args.Fen := argument;
               expectsFenString := false;
             End
      Else If (argument = '--moves') Then expectsMoves := true
      Else If (expectsMoves) Then
             Begin
               Val(argument, args.Moves, code);
               If code <> 0 Then
                 Begin
                   ShowHelp;
                   writeln('ERROR: Expected --moves <moves-number>, but ', argument, ' is passed');
                   Exit(false);
                 End;
               expectsMoves := false;
             End
      Else If (argument = '-o') Then expectsOutputFileName := true
      Else If (expectsOutputFileName) Then
             Begin
               If Length(argument) <= 0 Then
                 Begin
                   ShowHelp;
                   writeln('ERROR: Output file name should not be empty');
                   Exit(false);
                 End;
               cmdArgs.OutputFileName := argument;
               expectsOutputFileName := false;
               outputFileNameProvided := true;
             End
      Else If argument = '--buffer-size' Then expectsBufferSize := true
      Else If expectsBufferSize Then
             Begin
               Val(argument, args.BufferSize, code);
               If code <> 0 Then
                 Begin
                   ShowHelp;
                   writeln('ERROR: Expected --buffer-size <buffer-size>, but ', argument,
                           ' is passed');
                   Exit(false);
                 End;
               expectsBufferSize := false;
             End
      Else If argument = '--color' Then expectsColor := true
      Else If expectsColor Then
             Begin
               If argument = 'white' Then args.Color := PlayerColorWhite
               Else If argument = 'black' Then args.Color := PlayerColorBlack
               Else
                 Begin
                   ShowHelp;
                   writeln('ERROR: Expected --color <black|white>, but ', argument, ' is passed');
                   Exit(false);
                 End;
               expectsColor := false
             End
      Else If argument = '--a1-at-top-right' Then args.A1IsAtTheLeftBottomCorner := false
      Else If argument = '--show-enemy-moves' Then args.ShowEnemyMoves := true
      Else If argument = '--moves-group-size' Then expectsMovesGroupSize := true
      Else If expectsMovesGroupSize Then
             Begin
               Val(argument, args.MovesGroupSize, code);
               If code <> 0 Then
                 Begin
                   ShowHelp;
                   writeln('ERROR: Expected --moves-group-size <group size>, but ', argument,
                           ' is passed');
                   Exit(false);
                 End;
               expectsMovesGroupSize := false;
             End;
    End;
  If expectsFenString Or Not fenArgumentProvided Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected --fen <fen-notation-string>, but nothing is passed');
      Exit(false);
    End;
  If expectsOutputFileName Or Not outputFileNameProvided Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected -o <output file path>, but nothing is passed');
      Exit(false);
    End;
  If expectsMoves Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected --steps <steps-count>, but nothing is passed');
      Exit(false);
    End;
  If expectsColor Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected --color <white|black>, but nothing is passed');
      Exit(false);
    End;
  If expectsBufferSize Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected --buffer-size <buffer-size>, but nothing is passed');
      Exit(false);
    End;
  If expectsMovesGroupSize Then
    Begin
      ShowHelp;
      writeln('ERROR: Expected --moves-group-size <group size>, but nothing is passed');
      Exit(false);
    End;
  If Not bufferSizeProvided Then args.BufferSize := 10000;
  Exit(true);
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure findKorol(color: longint; Var i0,j0: longint);

Var i,j: longint;
Begin
  For i:=1 To n Do
    Begin
      For j:=1 To n Do
        Begin
          If board[i,j] = korol*color Then
            Begin
              i0 := i;
              j0 := j;
              Exit();
            End;
        End;
    End;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure MakeBoardWithFen(Var board: TBoard; s:String);

Var k,z,i,j: integer;
Procedure next;
Begin
  j := j+1;
  If (j>8) Then
    Begin
      j := 1;
      i := i-1;
    End;
End;
Begin
  i := 8;
  j := 1;
  For k:=1 To length(s) Do
    Begin
      If (s[k]<>'') And (s[k]<>'/') Then
        Begin
          If (s[k] In ['1'..'8']) Then
            Begin
              For z:=1 To ord(s[k]) - ord('0') Do
                Begin
                  board[i,j] := 0;
                  next;
                End;
            End
          Else
            If (s[k] = 'K') Then
              Begin
                board[i,j] := 6;
                next;
              End
          Else
            If (s[k] = 'Q') Then
              Begin
                board[i,j] := 5;
                next;
              End
          Else
            If (s[k] = 'R') Then
              Begin
                board[i,j] := 4;
                next;
              End
          Else
            If (s[k] = 'B') Then
              Begin
                board[i,j] := 3;
                next;
              End
          Else
            If (s[k] = 'N') Then
              Begin
                board[i,j] := 2;
                next;
              End
          Else
            If (s[k] = 'P') Then
              Begin
                board[i,j] := 1;
                next;
              End
          Else
            If (s[k] = 'k') Then
              Begin
                board[i,j] := -6;
                next;
              End
          Else
            If (s[k] = 'q') Then
              Begin
                board[i,j] := -5;
                next;
              End
          Else
            If (s[k] = 'r') Then
              Begin
                board[i,j] := -4;
                next;
              End
          Else
            If (s[k] = 'b') Then
              Begin
                board[i,j] := -3;
                next;
              End
          Else
            If (s[k] = 'n') Then
              Begin
                board[i,j] := -2;
                next;
              End
          Else
            If (s[k] = 'p') Then
              Begin
                board[i,j] := -1;
                next;
              End;
        End;
    End;

End;
Procedure ReverseColors(Var board: TBoard);

Var i, j: shortint;
Begin
  For i:=1 To 8 Do
    Begin
      For j:=1 To 8 Do
        Begin
          board[i,j] := -board[i,j];
        End;
    End;
End;
Procedure ClearBoard(Var board: TBoard);

Var i,j: integer;
Begin
  For i:=1 To n Do
    Begin
      For j:=1 To n Do
        Begin
          board[i,j] := 0;
        End;
    End;
End;
Procedure Initialize(Var cmdArgs: TCmdArgs; Var board: TBoard);

Var outputFile: text;
Begin
  clrscr;
  ClearBoard(board);
  cmdArgs.Moves := 1;
  If Not ParseArguments(cmdArgs) Then Halt(1);
  If cmdArgs.ShowHelp Then
    Begin
      ShowHelp;
      Halt(0);
    End;
  writeln('Moves: ', cmdArgs.Moves);
  writeln('Color: ', cmdArgs.Color);
  makeBoardWithFen(board, cmdArgs.fen);
  If cmdArgs.Color = PlayerColorBlack Then ReverseColors(board);
  countOfPossibleMoves := 0;
  If cmdArgs.A1IsAtTheLeftBottomCorner Then orientation := -1
  Else orientation := 1;
  assign(outputFile, cmdArgs.OutputFileName);
  rewrite(outputFile);
  close(outputFile);
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function colorOf(f: integer): integer;
Begin
  If f > 0 Then Exit(1)
  Else If f < 0 Then Exit(-1)
  Else Exit(0)
End;
Function getFigureOn(i,j: longint): longint;
Begin
  If (i>=1) And (i<=n) And (j>=1) And (j<=n) Then
    Begin
      getFigureOn := board[i,j];
    End
  Else
    Begin
      getFigureOn := 0;
    End;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function searchTo(i0,j0,dn,dm: longint): longint;

Var i,j: longint;
  current: longint;
Begin
  i := i0 + dn;
  j := j0 + dm;

  While (i<=n) And (i>=1) And (j<=n) And (j>=1) Do
    Begin
      current := getFigureOn(i,j);
      If current <> 0 Then Exit(current);
      i := i + dn;
      j := j + dm;
    End;
  Exit(0);
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function isUnderAttackByFigure(figure, i0, j0: longint): boolean;

Var res: boolean;
Begin
  res := false;
  If (abs(figure) = peshka) Then
    Begin
      If (figure*orientation > 0) Then
        Begin
          If (getFigureOn(i0+1,j0+1) = figure) Or (getFigureOn(i0+1,j0-1) = figure) Then
            Begin
              res := true;
            End;
        End
      Else
        Begin
          If (getFigureOn(i0-1,j0+1) = figure) Or (getFigureOn(i0-1,j0-1) = figure) Then
            Begin
              res := true;
            End;
        End;
    End
  Else
    If (abs(figure) = loshad) Then
      Begin
        If ((getFigureOn(i0-2,j0-1) = figure) Or
           (getFigureOn(i0-2,j0+1) = figure) Or
           (getFigureOn(i0+2,j0-1) = figure) Or
           (getFigureOn(i0+2,j0+1) = figure) Or
           (getFigureOn(i0-1,j0-2) = figure) Or
           (getFigureOn(i0-1,j0+2) = figure) Or
           (getFigureOn(i0+1,j0-2) = figure) Or
           (getFigureOn(i0+1,j0+2) = figure)) Then
          Begin
            res := true;
          End
      End
  Else
    If (abs(figure) = officer) Then
      Begin
        If (searchTo(i0,j0,-1,-1) = figure) Then
          Begin
            //To The left top
            res := true;
          End
        Else If (searchTo(i0,j0,-1,1) = figure) Then
               Begin
                 //To The right Top
                 res := true;
               End
        Else If (searchTo(i0,j0,1,-1) = figure) Then
               Begin
                 //To the left bottom
                 res := true;
               End
        Else If (searchTo(i0,j0,1,1) = figure) Then
               Begin
                 //to the right bottom
                 res := true;
               End;

      End
  Else
    If (abs(figure) = ladya) Then
      Begin
        //Search
        If (searchTo(i0,j0,-1,0) = figure) Then
          Begin
            res := true;
          End
        Else If (searchTo(i0,j0,1,0) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,0,-1) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,0,1) = figure) Then
               Begin
                 res := true;
               End;

      End
  Else
    If (abs(figure) = ferz) Then
      Begin
        //Search
        If (searchTo(i0,j0,-1,-1) = figure) Then
          Begin
            res := true;
          End
        Else If (searchTo(i0,j0,-1,0) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,-1,1) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,0,1) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,1,1) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,1,0) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,1,-1) = figure) Then
               Begin
                 res := true;
               End
        Else If (searchTo(i0,j0,0,-1) = figure) Then
               Begin
                 res := true;
               End;
      End
  Else
    If (abs(figure) = korol) Then
      Begin
        If ((getFigureOn(i0-1,j0-1) = figure) Or
           (getFigureOn(i0-1,j0) = figure) Or
           (getFigureOn(i0-1,j0+1) = figure) Or
           (getFigureOn(i0,j0-1) = figure) Or
           (getFigureOn(i0,j0+1) = figure) Or
           (getFigureOn(i0+1,j0-1) = figure) Or
           (getFigureOn(i0+1,j0) = figure) Or
           (getFigureOn(i0+1,j0+1) = figure)) Then
          Begin
            res := true;
          End
      End;
  isUnderAttackByFigure := res;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function isUnderAttackBy(colorOfattacker, i0, j0: longint): boolean;

Var res: boolean;
Begin
  If isUnderAttackByFigure(peshka * colorOfattacker, i0, j0) Then Exit(true);
  If isUnderAttackByFigure(loshad * colorOfattacker, i0, j0) Then Exit(true);
  If isUnderAttackByFigure(officer * colorOfattacker, i0, j0) Then Exit(true);
  If isUnderAttackByFigure(ladya * colorOfattacker, i0, j0) Then Exit(true);
  If isUnderAttackByFigure(ferz * colorOfattacker, i0, j0) Then Exit(true);
  If isUnderAttackByFigure(korol * colorOfattacker, i0, j0) Then Exit(true);

  Exit(false)
End;
Procedure saveBoard;

Var f: text;
  i,j: longint;
Begin
  assign(f,'out.txt');
  append(f);
  For i:=1 To n Do
    Begin
      For j:=1 To n Do
        Begin
          write(f, board[i,j]:3);
        End;
      writeln(f);
    End;
  writeln(f);
  close(f);
End;
Function figureTOSTR(f: longint): string;

Var res: string;
Begin
  res := ' ';
  If (abs(f) = peshka) Then res := ' peshka';
  If (abs(f) = loshad) Then res := ' loshad';
  If (abs(f) = officer) Then res := ' officer';
  If (abs(f) = ladya) Then res := ' ladya';
  If (abs(f) = ferz) Then res := ' ferz';
  If (abs(f) = korol) Then res := ' korol';
  If (f<0) Then res[1] := 'b'
  Else res[1] := 'w';
  figuretoSTR := res;
End;
Function getCoordStr(i,j: longint): string;

Var res: string;
Begin
  str(i, res);
  Exit(chr(ord('a') + j - 1) + res)
End;
Function MoveToStr(m:Move): string;
Begin
  Exit(
       figureToStr(m.figureStart) + ' '
  + getCoordStr(m.iStart, m.jStart) + ' '
  + getCoordStr(m.iEnd, m.jEnd)
  )
End;
Procedure savebuf(Var buffer: TBuffer; Var buffercursor: longint; outputFileName: String);

Var f: text;
  i: longint;
Begin
  assign(f, outputFileName);
  append(f);
  For i:=1 To buffercursor Do
    Begin
      writeln(f,buffer[i]);
    End;
  close(f);
  buffercursor := 0;
End;
Procedure AddStrToBuffer(Var buffer: TBuffer; Var buffercursor: longint; s: String);
Begin
  inc(buffercursor);
  buffer[buffercursor] := s;
End;
Procedure saveMoves(
                    outputFileName: String;
                    Var buffer: TBuffer;
                    Var buffercursor: longint;
                    buffermax: longint;
                    showEnemyMoves: boolean;
                    movesGroupSize: longint
);

Var i: longint;
  s: string;
Begin
  i := 1;
  s := '';
  For i:=1 To countOfMakedMoves Do
    Begin
      If (i<=movesGroupSize) Then
        Begin
          If (lastMakedMoves[i] <> MakedMoves[i]) Then AddStrToBuffer(buffer, buffercursor, '');
        End;
    End;
  For i:=1 To countOfMakedMoves Do
    Begin
      If (i Mod 2 = 1) Or showEnemyMoves Then
        Begin
          s := s + MoveToSTr(makedmoves[i])+chr(9)
        End;
    End;
  AddStrToBuffer(buffer, buffercursor, s);
  If (buffercursor >= buffermax) Then
    Begin
      savebuf(buffer, buffercursor, outputFileName);
    End;
  lastMakedMoves := makedmoves;
End;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function CreateMove(
                    iStart: longint;
                    jStart: longint;
                    iEnd: longint;
                    jEnd: longint;
                    figureStart: longint
): Move;

Var res: Move;
Begin
  res.iStart := iStart;
  res.jStart := jStart;
  res.iEnd := iEnd;
  res.jEnd := jEnd;
  res.figureStart := figureStart;
  res.figureEnd := 0;
  CreateMove := res;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure DoMove(Var m: Move);
Begin
  With m Do
    Begin
      figureEnd := board[iEnd, jEnd];
      If (iEnd = 1) And (figureStart*orientation = peshka) Then
        Begin
          board[iEnd,jEnd] := ferz*colorOf(figureStart);
        End
      Else If (iEnd = 8) And (figureStart*orientation = bpeshka) Then
             Begin
               board[iEnd,jEnd] := ferz*colorOf(figureStart);
             End
      Else board[iEnd, jEnd] := figureStart;

      board[iStart, jStart] := 0;
      inc(countOfMakedMoves);
      MakedMoves[countOfMakedMoves] := m;
    End;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure UndoMove(Var m: Move);
Begin
  With m Do
    Begin
      board[iStart, jStart] := figureStart;
      board[iEnd, jEnd] := figureEnd;
      dec(countOfMakedMoves);
    End;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Function isCheckTo(color: longint): boolean;
//TOWRITE

Var res: boolean;
  i,j: longint;
Begin
  findKorol(color,i,j);
  res := isUnderAttackBy(-color, i,j);
  isCheckTo := res;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure AddMove(m: Move);
Begin
  If (m.iEnd>=1) And (m.iEnd<=8) And (m.jEnd>=1) And (m.jEnd<=8) Then
    Begin
      doMove(m);
      If (Not isCheckTo(colorOf(m.figureStart))) And (colorOf(m.figurestart)<>colorOf(m.figureEnd))
        Then
        Begin
          inc(countOfPossibleMoves);
          moves[countOfPossibleMoves] := m;
        End;
      UndoMove(m);
    End;
End;
Procedure AddMovesDist(i0,j0,dn,dm: longint);

Var i,j: longint;
  currentfigure: longint;
  current: longint;
  curmov: Move;
Begin
  i := i0 + dn;
  j := j0 + dm;
  currentFigure := getFigureOn(i0,j0);
  current := getFigureOn(i,j);
  If (dn <> 0) Or (dm <> 0) Then
    Begin
      While (current*currentFigure <= 0) And (i<=n) And (i>=1) And (j<=n) And (j>=1) Do
        Begin
          curmov := CreateMove(i0,j0,i,j, currentFigure);
          addMove(curmov);
          If (current*currentFigure < 0) Then break;
          i := i + dn;
          j := j + dm;
          current := getFigureOn(i,j);

        End;
    End;
End;
Procedure AddAllPossibleMoves(color: longint);
//TOWRITE

Var i,j: longint;
  curfig: longint;
  // Curent Figure
  curmov: Move;
Begin
  For i:=1 To n Do
    Begin
      For j:=1 To n Do
        Begin
          If (board[i,j]*color > 0) Then
            Begin
              curfig := board[i,j];
              If (abs(curfig) = peshka) Then
                Begin
                  If (color*orientation = white) Then
                    Begin
                      If (getFigureOn(i-1,j-1)*color<0) Then
                        Begin
                          curmov := CreateMove(i,j,i-1,j-1, curfig);
                          AddMove(curMov);
                        End;
                      If (getFigureOn(i-1,j+1)*color<0) Then
                        Begin
                          curmov := CreateMove(i,j,i-1,j+1, curfig);
                          AddMove(curMov);
                        End;
                      If (getFigureOn(i-1,j) = 0) Then
                        Begin
                          curmov := CreateMove(i,j,i-1,j, curfig);
                          AddMove(curMov);
                        End;
                      If (i = 7) Then
                        Begin
                          If (getFigureOn(i-2,j) = 0) Then
                            Begin
                              curmov := CreateMove(i,j,i-2,j, curfig);
                              AddMove(curMov);
                            End;
                        End;
                    End
                  Else
                    Begin
                      If (getFigureOn(i+1,j-1)*color<0) Then
                        Begin
                          curmov := CreateMove(i,j,i+1,j-1, curfig);
                          AddMove(curMov);
                        End;
                      If (getFigureOn(i+1,j+1)*color<0) Then
                        Begin
                          curmov := CreateMove(i,j,i+1,j+1, curfig);
                          AddMove(curMov);
                        End;
                      If (getFigureOn(i+1,j) = 0) Then
                        Begin
                          curmov := CreateMove(i,j,i+1,j, curfig);
                          AddMove(curMov);
                        End;
                      If (i = 2) Then
                        Begin
                          If (getFigureOn(i+2,j) = 0) Then
                            Begin
                              curmov := CreateMove(i,j,i+2,j, curfig);
                              AddMove(curMov);
                            End;
                        End;
                    End;
                End
              Else
                If (abs(curfig) = loshad) Then
                  //-----------------------------------------------------Loshad
                  Begin
                    If (getFigureOn(i-2,j-1)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i-2,j-1, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i-2,j+1)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i-2,j+1, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i+2,j-1)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i+2,j-1, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i+2,j+1)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i+2,j+1, curfig);
                        AddMove(curMov);
                      End;

                    If (getFigureOn(i-1,j-2)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i-1,j-2, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i-1,j+2)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i-1,j+2, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i+1,j-2)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i+1,j-2, curfig);
                        AddMove(curMov);
                      End;
                    If (getFigureOn(i+1,j+2)*color <= 0) Then
                      Begin
                        curmov := CreateMove(i,j,i+1,j+2, curfig);
                        AddMove(curMov);
                      End;
                  End
              Else
                If (abs(curfig) = officer) Then
                  Begin
                    addmovesDist(i,j,-1,-1);
                    addmovesDist(i,j,-1, 1);
                    addmovesDist(i,j, 1,-1);
                    addmovesDist(i,j, 1, 1);
                  End
              Else
                If (abs(curfig) = ladya) Then
                  Begin
                    addmovesDist(i,j,-1, 0);
                    addmovesDist(i,j, 0, 1);
                    addmovesDist(i,j, 1, 0);
                    addmovesDist(i,j, 0,-1);
                  End
              Else
                If (abs(curfig) = ferz) Then
                  Begin
                    addmovesDist(i,j,-1,-1);
                    addmovesDist(i,j,-1, 1);
                    addmovesDist(i,j, 1,-1);
                    addmovesDist(i,j, 1, 1);
                    addmovesDist(i,j,-1, 0);
                    addmovesDist(i,j, 0, 1);
                    addmovesDist(i,j, 1, 0);
                    addmovesDist(i,j, 0,-1);
                  End
              Else
                If (abs(curfig) = korol) Then
                  Begin
                    curmov := CreateMove(i,j,i-1,j-1, curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i-1,j,   curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i-1,j+1, curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i,  j-1, curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i,  j+1, curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i+1,j-1, curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i+1,j,   curfig);
                    AddMove(curmov);
                    curmov := CreateMove(i,j,i+1,j+1, curfig);
                    AddMove(curmov);
                  End;
            End;
        End;
    End;
End;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Procedure Solve(
                color, countOfMoves: longint;
                Var buffer: TBuffer;
                Var buffercursor: longint;
                buffermax: longint;
                movesGroupSize: longint;
                showEnemyMoves: boolean;
                outputFileName: String
);

Var FirstPossibleMoveIndex: longint;
  LastPossibleMoveIndex: longint;
  i: longint;
  checkWhite, checkBlack: boolean;
  isOk: boolean;
Begin
  If (countOfMoves > 0) Then
    Begin
      inc(cVariants);
      FirstPossibleMoveIndex := countOfPossibleMoves + 1;

      isOk := true;

      checkWhite := isCheckTo(white);
      checkBlack := isCheckTo(black);

      If (checkWhite) And (isCheckToWhite) Then isOk := false;
      If (checkBlack) And (isCheckToBlack) Then isOK := false;

      If (isOk) Then
        Begin
          isCheckToWhite := checkWhite;
          isCheckToBlack := checkBlack;
          AddAllPossibleMoves(color);
          LastPossibleMoveIndex := countOfPossibleMoves;
          If (LastPossibleMoveIndex < FirstPossibleMoveIndex) And (isCheckTo(black)) Then
            Begin
              inc(cSolving);
              If (maxcountOfPossiblemoves < countofPossibleMoves) Then maxcountofPossibleMoves := 
                                                                                countOfPossibleMoves
              ;
              Writeln('solved: ', cSolving, ' Solve: ', cVariants, '   max: ',
                      maxcountofpossibleMoves);
              Savemoves(outputFileName, buffer, buffercursor, buffermax, showEnemyMoves,
                        movesGroupSize);

            End
          Else
            For i := LastPossibleMoveIndex Downto FirstPossibleMoveIndex Do
              Begin
                DoMove(moves[i]);
        {$IFDEF SHOULD_LOG}
                writeln;
                ShowChessBoard(board);
        {$ENDIF}
                Solve(-color, countOfMoves -1, buffer, buffercursor, buffermax, movesGroupSize,
                      showEnemyMoves, outputFileName);
                UndoMove(moves[i]);
                moves[i] := CreateMove(0,0,0,0,0);
              End;
          countOfPossibleMoves := FirstPossibleMoveIndex - 1;

        End;
    End;
End;

Begin
  Initialize(cmdArgs, board);
  ShowChessBoard(board);
  readkey;
  textcolor(7);
  textbackground(0);
  clrscr;
  Solve(
        white,
        cmdArgs.Moves*2,
        buffer,
        buffercursor,
        cmdArgs.BufferSize,
        cmdArgs.MovesGroupSize,
        cmdArgs.ShowEnemyMoves,
        cmdArgs.OutputFileName
  );
  savebuf(
          buffer,
          buffercursor,
          cmdArgs.OutputFileName
  );
  writeln('Solved: ',cSolving);
  writeln('Watched: ', cVariants);
  readkey;
End.
