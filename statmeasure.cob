IDENTIFICATION DIVISION.
       PROGRAM-ID. STATMEASURE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO DYNAMIC-FILE
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  FILE-RECORD.
           02 FILE-X         PIC 9(6)V9(2).

       WORKING-STORAGE SECTION.
       77  DYNAMIC-FILE        PIC X(50).
       77  SUM-OF-X            PIC 9(10)V9(2) VALUE ZEROS.
       77  SUM-OF-X-SQR        PIC 9(14)V9(2) VALUE ZEROS.
       77  SUM-OF-LOG-X        PIC 9(14)V9(6) VALUE ZEROS.
       77  SUM-OF-RECIPROCAL   PIC 9(14)V9(6) VALUE ZEROS.
       77  PRODUCT-OF-X        PIC 9(14)V9(6) VALUE 1.
       77  N                   PIC 9(4) VALUE ZEROS.
       77  MEAN                PIC 9(10)V9(2) VALUE ZEROS.
       77  STD-DEVIATION       PIC 9(10)V9(2) VALUE ZEROS.
       77  GEOMETRIC-MEAN      PIC 9(10)V9(2) VALUE ZEROS.
       77  HARMONIC-MEAN       PIC 9(10)V9(2) VALUE ZEROS.
       77  ROOT-MEAN-SQUARE    PIC 9(10)V9(2) VALUE ZEROS.
       77  TEMP-MEAN           PIC 9(10)V9(2) VALUE ZEROS.
       77  SUM-OF-X-SQUARES    PIC 9(14)V9(2) VALUE ZEROS.
       77  TEMP-IN-X           PIC 9(6)V9(2) VALUE ZEROS.
       77  IN-X                PIC 9(6)V9(2) VALUE ZEROS.
       77  I                   PIC 9(4) VALUE ZEROS.
       77  WS-END-OF-FILE      PIC X VALUE 'N'.

       *> Output Formatting Variables
       77  FMT-SUM             PIC ZZZZ9.99.
       77  FMT-MEAN            PIC ZZZZ9.99.
       77  FMT-GEOMETRIC-MEAN  PIC ZZZZ9.99.
       77  FMT-HARMONIC-MEAN   PIC ZZZZ9.99.
       77  FMT-STD-DEVIATION   PIC ZZZZ9.99.
       77  FMT-ROOT-MEAN-SQ    PIC ZZZZ9.99.

       01  X                   PIC 9(6)V9(2) OCCURS 1000 TIMES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "******************************************".
           DISPLAY "*        STATISTICAL MEASURE TOOL        *".
           DISPLAY "******************************************".
           DISPLAY "Enter the input file name: ".
           ACCEPT DYNAMIC-FILE.

           OPEN INPUT INPUT-FILE.

           DISPLAY "******************************************".
           DISPLAY "*          MEAN AND STANDARD DEV         *".
           DISPLAY "******************************************".
           DISPLAY " DATA VALUES".

           MOVE ZERO TO SUM-OF-X, SUM-OF-X-SQR, SUM-OF-LOG-X, SUM-OF-RECIPROCAL.
           MOVE ZERO TO SUM-OF-X-SQUARES.
           MOVE 1 TO PRODUCT-OF-X.
           MOVE ZERO TO N.

           PERFORM INPUT-LOOP UNTIL WS-END-OF-FILE = 'Y'.

           PERFORM COMPUTE-MEAN.
           MOVE MEAN TO TEMP-MEAN.
           PERFORM COMPUTE-STD-DEVIATION.
           PERFORM COMPUTE-GEOMETRIC-MEAN.
           PERFORM COMPUTE-HARMONIC-MEAN.
           PERFORM COMPUTE-ROOT-MEAN-SQUARE.

           *> Format values for output
           MOVE SUM-OF-X TO FMT-SUM.
           MOVE MEAN TO FMT-MEAN.
           MOVE GEOMETRIC-MEAN TO FMT-GEOMETRIC-MEAN.
           MOVE HARMONIC-MEAN TO FMT-HARMONIC-MEAN.
           MOVE STD-DEVIATION TO FMT-STD-DEVIATION.
           MOVE ROOT-MEAN-SQUARE TO FMT-ROOT-MEAN-SQ.

           DISPLAY "******************************************".
           DISPLAY "*              RESULTS                   *".
           DISPLAY "******************************************".
           DISPLAY " SUM          = ", FMT-SUM.
           DISPLAY " MEAN         = ", FMT-MEAN.
           DISPLAY " GEOMETRIC MEAN = ", FMT-GEOMETRIC-MEAN.
           DISPLAY " HARMONIC MEAN  = ", FMT-HARMONIC-MEAN.
           DISPLAY " STANDARD DEV   = ", FMT-STD-DEVIATION.
           DISPLAY " ROOT MEAN SQ   = ", FMT-ROOT-MEAN-SQ.
           DISPLAY "******************************************".

           CLOSE INPUT-FILE.
           STOP RUN.

       INPUT-LOOP.
           READ INPUT-FILE
               AT END 
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   MOVE FILE-X TO IN-X
                   PERFORM PROCESS-DATA
           END-READ.

       PROCESS-DATA.
           COMPUTE TEMP-IN-X = IN-X
           ADD 1 TO N
           MOVE TEMP-IN-X TO X(N)
           ADD X(N) TO SUM-OF-X
           COMPUTE SUM-OF-X-SQUARES ROUNDED = SUM-OF-X-SQUARES + (X(N) ** 2)
           COMPUTE SUM-OF-RECIPROCAL ROUNDED = SUM-OF-RECIPROCAL + (1 / X(N))
           COMPUTE SUM-OF-LOG-X ROUNDED = SUM-OF-LOG-X + FUNCTION LOG10(X(N))
           MOVE X(N) TO FMT-SUM
           DISPLAY FMT-SUM.

       COMPUTE-MEAN.
           COMPUTE MEAN ROUNDED = SUM-OF-X / N.

       COMPUTE-STD-DEVIATION.
           MOVE ZERO TO SUM-OF-X-SQR.
           PERFORM CALCULATE-STD-DEV VARYING I FROM 1 BY 1 UNTIL I > N.
           COMPUTE STD-DEVIATION ROUNDED = (SUM-OF-X-SQR / N) ** 0.5.

       CALCULATE-STD-DEV.
           COMPUTE SUM-OF-X-SQR ROUNDED = SUM-OF-X-SQR + (X(I) - TEMP-MEAN) ** 2.

       COMPUTE-GEOMETRIC-MEAN.
           COMPUTE GEOMETRIC-MEAN ROUNDED = FUNCTION EXP10(SUM-OF-LOG-X / N).

       COMPUTE-HARMONIC-MEAN.
           COMPUTE HARMONIC-MEAN ROUNDED = N / SUM-OF-RECIPROCAL.

       COMPUTE-ROOT-MEAN-SQUARE.
           COMPUTE ROOT-MEAN-SQUARE ROUNDED = (SUM-OF-X-SQUARES / N) ** 0.5.
