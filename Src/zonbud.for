C     ******************************************************************
C     Program to compute and print volumetric budgets over subregions
C     of a flow system that is being simulated using the USGS Modular
C     Three-Dimensional Finite-Difference Ground-Water Flow Model.
C
C     This program is documented in USGS Open-File Report 90-392,
C     written by Arlen W. Harbaugh
C
C     Jan. 29, 2000 -- Updated to work with MODFLOW's COMPACT BUDGET
C     option.
C     ******************************************************************
C        SPECIFICATIONS:
      PARAMETER (NODDIM=1000000,NTRDIM=50,NZDIM=25,MXCOMP=25,MXZWCZ=10)
C-----   NODDIM must be greater than or equal to the product of the
C-----          number of layers, rows, and columns in the model grid.
C-----   NTRDIM must be greater than or equal to the number of budget
C-----          terms, other than flow between zones, that will appear
C-----          the budget.  In the original model, there is a maximum
C-----          of 8 terms -- constant-head, storage, wells, rivers,
C-----          drains, recharge, general-head boundaries, and
C-----          evapotranspiration.
C-----   NZDIM  must be greater than or equal to the maximum zone
C-----          number.
C-----   MXCOMP is the maximum number of composite zones, and must be
C-----          less than 81.
C-----   MXZWCZ is the maximum number of numeric zones within each
C-----          composite zone.
C      COMMON /BUFCOM/BUFF
C      COMMON /ZONCOM/IZONE
C      COMMON /CHCOM/ICH
C      COMMON /IBFCOM/IBUFF
      COMMON BUFF,IZONE,ICH,IBUFF
      DIMENSION BUFF(NODDIM),IZONE(NODDIM),ICH(NODDIM),IBUFF(NODDIM),
     1          VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM)
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP)
      DOUBLE PRECISION VBVL,VBZNFL
      DIMENSION ITIME(2,10)
      CHARACTER*80 TITLE
      CHARACTER*80 NAME
      CHARACTER*16 VBNM(NTRDIM),TEXT,CTMP
      CHARACTER*1 METHOD,IANS
      CHARACTER*40 VERSON
      DIMENSION VAL(10)
C     ------------------------------------------------------------------
      VERSON='ZONEBUDGET version 2.00'
C
C-----DEFINE INPUT AND OUTPUT UNITS AND INITIALIZE OTHER VARIABLES
      INZN1=10
      INZN2=11
      INBUD=12
      IOUT=13
      K1=0
      K2=0
      NZONES=0
      MSUM=0
      ZERO=0.0
      NLIST=0
      NVAL=1
C
C-----TELL THE USER WHAT THIS PROGRAM IS
      WRITE(*,*)
      WRITE(*,'(1X,A)') VERSON
      WRITE(*,*) ' Program to compute a flow budget for subregions of a 
     1model using'
      WRITE(*,*) ' cell-by-cell flow data from the USGS Modular Ground-W
     1ater Flow Model.'
C
C-----OPEN A LISTING FILE
      WRITE(*,*)
3     WRITE(*,*) ' Enter the name of a LISTING FILE for results:'
      READ(*,'(A)') NAME
      OPEN(UNIT=IOUT,FILE=NAME,ERR=3)
      WRITE(IOUT,4) VERSON
4     FORMAT(1X,/,1X,A/
     1' Program to compute a flow budget for subregions of a model using
     2'/' cell-by-cell flow data from the USGS Modular Ground-Water Flow
     3 Model.')
C
C-----OPEN THE CELL-BY-CELL BUDGET FILE
      WRITE(*,*)
10    WRITE(*,*) ' Enter the name of the file containing CELL-BY-CELL BU
     1DGET TERMS:'
      READ(*,'(A)') NAME
      OPEN(UNIT=INBUD,FILE=NAME,STATUS='OLD',FORM='UNFORMATTED',ERR=10)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' The cell-by-cell budget file is:'
      WRITE(IOUT,*) NAME
C
C-----READ GRID SIZE FROM BUDGET FILE AND REWIND
      READ(INBUD,END=2000) KSTP,KPER,TEXT,NCOL,NROW,NLAY
      IF(NLAY.LT.0) NLAY=-NLAY
      REWIND(UNIT=INBUD)
      WRITE(*,*)
      WRITE(*,14) NLAY,NROW,NCOL
      WRITE(IOUT,*)
      WRITE(IOUT,14) NLAY,NROW,NCOL
14    FORMAT(1X,I3,' layers',I10,' rows',I10,' columns')
C
C-----CHECK TO SEE IF NODIM IS LARGE ENOUGH
      NRC=NROW*NCOL
      NODES=NRC*NLAY
      IF(NODES.GT.NODDIM) THEN
         WRITE(*,*) ' PROGRAM ARRAYS ARE DIMENSIONED TOO SMALL'
         WRITE(*,*) ' PARAMETER NODDIM IS CURRENTLY',NODDIM
         WRITE(*,*) ' CHANGE NODDIM TO BE',NODES,' OR GREATER'
         STOP
      END IF
C
C-----READ A TITLE TO BE PRINTED IN THE LISTING
      WRITE(*,*)
      WRITE(*,*) ' Enter a TITLE to be printed in the listing:'
      READ(*,'(A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,'(1X,A)') TITLE
C
C-----OPEN THE ZONE FILE IF IT EXISTS
12    WRITE(*,*)
      WRITE(*,*) ' Enter the name of your ZONE INPUT FILE (CR for intera
     1ctive):'
      READ(*,'(A)') NAME
C
C-----IF NAME IS BLANK, INPUT ZONES INTERACTIVELY BY BLOCK
      IF(NAME.EQ.' ') THEN
         CALL BLOCK(IZONE,NLAY,NROW,NCOL,NZDIM,NZONES,IOUT)
         NCOMP=0
      ELSE
C
C-----WHEN NAME IS NOT BLANK, OPEN ZONE FILE, AND CHECK GRID DIMENSIONS
         OPEN(UNIT=INZN1,FILE=NAME,STATUS='OLD',ERR=12)
         WRITE(IOUT,*)
         WRITE(IOUT,*) ' The zone file is:'
         WRITE(IOUT,*) NAME
         READ(INZN1,*) NL,NR,NC
         IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
            WRITE(*,*) 'MISMATCH BETWEEN DIMENSIONS OF CELL-BY-CELL DATA
     1 AND ZONE DATA:'
            WRITE(*,*) 'LAYERS, ROWS, COLUMNS IN ZONE DATA:',NL,NR,NC
            WRITE(*,*) 'LAYERS, ROWS, COLUMNS IN CELL-BY-CELL FILE:',
     1                  NLAY,NROW,NCOL
            STOP
         END IF
C
C-----READ ZONE ARRAY
         CALL IZREAD(IZONE,NLAY,NROW,NCOL,NZDIM,INZN1,INZN2,IOUT,NZONES)
C
C-----READ COMPOSITE ZONES
         CALL INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,NZONES,IOUT)
C
C-----CLOSE ZONE FILE
         CLOSE(UNIT=INZN1)
      END IF
C
C-----DONE WITH ZONE DEFINITION
      WRITE(*,*)
      WRITE(*,*) NZONES,' is your highest zone.'
C
C-----CHECK WHAT METHOD TO USE FOR SPECIFYING WHEN TO CALCULATE BUDGETS
50    WRITE(*,*)
      WRITE(*,*) ' Choose the option for specifying when budgets are cal
     1culated:'
      WRITE(*,*) ' A = ALL times stored in the budget file.'
      WRITE(*,*) ' P = For each time stored in the budget file, PROMPT u
     1ser.'
      WRITE(*,*) ' L = Enter a LIST of times.'
      READ(*,'(A)') METHOD
      IF(METHOD.EQ.'A' .OR. METHOD.EQ.'a') THEN
         METHOD='A'
      ELSE IF(METHOD.EQ.'P' .OR. METHOD.EQ.'p') THEN
         METHOD='P'
      ELSE IF(METHOD.EQ.'L' .OR. METHOD.EQ.'l') THEN
         METHOD='L'
         DO 60 I=1,10
         WRITE(*,*) ' Enter a time step, stress period at which to calcu
     1late budgets (0,0=done):'
         READ(*,*) ITIME(1,I),ITIME(2,I)
         IF(ITIME(1,I).EQ.0 .AND. ITIME(2,I).EQ.0) GO TO 65
60       CONTINUE
         I=11
65       NTIMES=I-1
      ELSE
         WRITE(*,*) 'Invalid choice; you must enter "A", "P", or "L"'
         GO TO 50
      END IF
      WRITE(*,*)
      ICALC=0
C
C
C-----READ BUDGET DATA AND ACCUMULATE AS LONG AS TIME REMAINS CONSTANT.
C-----WHEN TIME CHANGES, PRINT THE BUDGET, REINITIALIZE, AND START OVER
100   READ(INBUD,END=1000) KSTP,KPER,TEXT,NC,NR,NL
      ITYPE=0
      IF(NL.LT.0) THEN
         READ(INBUD) ITYPE,DELT,PERTIM,TOTIM
         NVAL=1
         IF(ITYPE.EQ.5) THEN
            READ(INBUD) NVAL
            IF(NVAL.GT.1) THEN
               DO 101 N=2,NVAL
               READ(INBUD) CTMP
101            CONTINUE
            END IF
         END IF
         IF(ITYPE.EQ. 2 .OR. ITYPE.EQ.5) READ(INBUD) NLIST
      END IF
C
C-----CHECK IF STARTING A NEW TIME STEP
      IF(K1.NE.KSTP .OR. K2.NE.KPER) THEN
C
C-----IF STARTING A NEW TIME STEP, PRINT A BUDGET AND REINITIALIZE ALL
C-----BUDGET ACCUMULATORS
C-----AT THE VERY BEGINNING WHEN K1=K2=0, DON'T PRINT THE BUDGET BECAUSE
C-----NOTHING HAS BEEN ACCUMULATED YET
         IF(K1.NE.0 .AND. K2.NE.0 .AND. ICALC.NE.0) THEN
            CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,NZONES,IOUT,NTRDIM,
     1            NZDIM,TITLE)
            IF(NCOMP.GT.0) CALL COMPPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1            NZONES,IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,
     2            MXCOMP,MXZWCZ)
         END IF
C
C-----SET TIME CHANGE INDICATORS
         K1=KSTP
         K2=KPER
C
C-----DECIDE WHETHER OR NOT TO CALCULATE THE BUDGET FOR THIS TIME STEP
         ICALC=0
         IF(METHOD.EQ.'A') THEN
            ICALC=1
         ELSE IF(METHOD.EQ.'P') THEN
102         WRITE(*,105) KSTP,KPER
105         FORMAT(1X,'Do you want to calculate budgets for time step',
     1             I3,' in stress period',I3,' (Y/N)?')
            READ(*,'(A)') IANS
            IF(IANS.EQ.'Y' .OR. IANS.EQ.'y') THEN
               ICALC=1
            ELSE IF(IANS.EQ.'N' .OR. IANS.EQ.'n') THEN
            ELSE
               GO TO 102
            END IF
         ELSE
            DO 110 I=1,NTIMES
            IF(KSTP.NE.ITIME(1,I) .OR. KPER.NE.ITIME(2,I)) GO TO 110
            ICALC=1
            GO TO 120
110         CONTINUE
120         CONTINUE
         END IF
         IF(ICALC.EQ.0) THEN
            WRITE(*,121) KSTP,KPER
121         FORMAT(' Skipping the budget for time step',I3,
     1       ' in stress period',I3)
         ELSE
            MSUM=1
            DO 210 I=1,NZONES
            DO 210 J=1,NTRDIM
            DO 210 K=1,2
            VBVL(K,J,I)=ZERO
210         CONTINUE
            DO 220 I=0,NZONES
            DO 220 J=0,NZONES
            DO 220 K=1,2
            VBZNFL(K,J,I)=ZERO
220         CONTINUE
            WRITE(*,221) KSTP,KPER
221         FORMAT(' Computing the budget for time step',I3,
     1       ' in stress period',I3)
         END IF
      END IF
C
C-----READ THE BUDGET TERM DATA UNDER THE FOLLOWING CONDITIONS:
      IF(ITYPE.EQ.0 .OR. ITYPE.EQ.1) THEN
C  FULL 3-D ARRAY
         READ(INBUD) (BUFF(I),I=1,NODES)
      ELSE IF(ITYPE.EQ.3) THEN
C  1-LAYER ARRAY WITH LAYER INDICATOR ARRAY
         DO 260 I=1,NODES
         BUFF(I)=ZERO
260      CONTINUE
         READ(INBUD) (IBUFF(I),I=1,NRC)
         READ(INBUD) (BUFF(I),I=1,NRC)
         DO 270 I=1,NRC
         IF(IBUFF(I).NE.1) THEN
            LAYMOV=(IBUFF(I)-1)*NRC
            BUFF(LAYMOV+I)=BUFF(I)
            BUFF(I)=ZERO
         END IF
270      CONTINUE
      ELSE IF(ITYPE.EQ.4) THEN
C  1-LAYER ARRAY THAT DEFINES LAYER 1
         READ(INBUD) (BUFF(I),I=1,NRC)
         IF(NODES.GT.NRC) THEN
            DO 280 I=NRC+1,NODES
            BUFF(I)=ZERO
280         CONTINUE
         END IF
      ELSE IF(ICALC.EQ.0 .AND. NLIST.GT.0) THEN
C  LIST -- READ ONLY IF THE VALUES NEED TO BE SKIPPED
         DO 300 N=1,NLIST
         READ(INBUD) LOC,(VAL(I),I=1,NVAL)
300      CONTINUE
      END IF
C
C-----BEFORE PROCESSING A BUDGET TERM, CHECK IF THERE IS ENOUGH SPACE
      IF(MSUM.GT.NTRDIM) THEN
         WRITE(*,*) 'PROGRAM PARAMETER NTRDIM IS TOO SMALL'
         WRITE(*,*) 'PARAMETER NTRDIM IS CURRENTLY',NTRDIM
         WRITE(*,*) 'CHANGE NTRDIM TO BE EQUAL TO THE MAXIMUM NUMBER OF
     1BUDGET TERMS'
         STOP
      END IF
C
C-----PROCESS A BUDGET TERM AND THEN START THE READ PROCESS OVER
      IF(ICALC.NE.0) CALL ACCM(BUFF,IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,
     1           VBZNFL,MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2           ITYPE,NLIST,INBUD,NVAL)
      GO TO 100
C
C  PRINT BUDGET BECAUSE END OF FILE WAS REACHED
1000  IF(ICALC.NE.0) CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,NZONES,
     1        IOUT,NTRDIM,NZDIM,TITLE)
      IF(NCOMP.GT.0 .AND. ICALC.NE.0) CALL COMPPR(K1,K2,VBNM,VBVL,
     1      VBZNFL,MSUM,NZONES,IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,
     2      NCOMP,MXCOMP,MXZWCZ)
      STOP
C
C-----EMPTY BUDGET FILE
2000  WRITE(*,*) 'CELL-BY-CELL FLOW TERM FILE WAS EMPTY'
      STOP
C
      END
      SUBROUTINE IZREAD(IZONE,NLAY,NROW,NCOL,NZDIM,INZN1,INZN2,IOUT,
     1                  NZONES)
C     ******************************************************************
C     ROUTINE TO INPUT 3-D ZONE MATRIX, IZONE
C       INZN1 IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
      DIMENSION IZONE(NCOL,NROW,NLAY)
      CHARACTER*20 FMTIN
      CHARACTER*80 NAME,NAMPRV
      CHARACTER*10 LOCAT,LOCTMP
C     ------------------------------------------------------------------
      NAMPRV=' '
      DO 1000 K=1,NLAY
C
C-----READ ARRAY CONTROL RECORD.
      READ (INZN1,1) LOCAT,ICONST,FMTIN,IPRN
1     FORMAT(A,I10,A,I10)
C
C-----LEFT JUSTIFY LOCAT AND MAKE IT UPPER CASE SO USER DOES NOT HAVE TO
C-----BE CONCERNED WITH JUSTIFICATION OR CASE
      LOCTMP=LOCAT
      IDIFF=ICHAR('a')-ICHAR('A')
      DO 20 I=1,10
      IF(LOCTMP(I:I).GE.'a' .AND. LOCTMP(I:I).LE.'z')
     1        LOCTMP(I:I)=CHAR(ICHAR(LOCTMP(I:I))-IDIFF)
20    CONTINUE
      DO 30 I=1,10
      IF(LOCTMP(I:I).NE.' ') GO TO 40
30    CONTINUE
      I=1
40    LOCAT=LOCTMP(I:10)
C
C-----USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.
      IF(LOCAT.NE.'CONSTANT') GO TO 90
C
C-----LOCAT='CONSTANT' -- SET ALL ARRAY VALUES EQUAL TO ICONST.
      DO 80 I=1,NROW
      DO 80 J=1,NCOL
80    IZONE(J,I,K)=ICONST
      WRITE(IOUT,*)
      WRITE(IOUT,83) ICONST,K
83    FORMAT(13X,'Zone Array =',I3,' for layer',I3)
      IF(ICONST.GT.NZONES) NZONES=ICONST
      IF(ICONST.LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE NUMBER IS NOT ALLOWED'
         STOP
      END IF
      GO TO 1000
C
C-----LOCAT SHOULD BE EITHER 'EXTERNAL' OR 'INTERNAL'
C-----IF 'INTERNAL', READ ARRAY FROM SAME FILE
90    IF(LOCAT.EQ.'INTERNAL') THEN
         INUNIT=INZN1
         WRITE(IOUT,*)
         WRITE(IOUT,91) K
91       FORMAT(1X,'Zone Array for layer',I3,
     1      ' will be read from the Zone File')
C
C-----IF 'EXTERNAL', OPEN A SEPARATE FILE
      ELSE IF(LOCAT.EQ.'EXTERNAL') THEN
         READ(INZN1,'(A)') NAME
         INUNIT=INZN2
         WRITE(IOUT,*)
         WRITE(IOUT,93) K,NAME
93       FORMAT(1X,'Zone Array for layer',I3,
     1         ' will be read from file:'/1X,A)
         IF(NAME.NE.NAMPRV) THEN
            IF(NAMPRV.NE.' ') CLOSE(UNIT=INUNIT)
            OPEN(UNIT=INUNIT,FILE=NAME,STATUS='OLD')
            WRITE(IOUT,96)
96          FORMAT(1X,'The file was opened successfully.')
            NAMPRV=NAME
         ELSE
            WRITE(IOUT,97)
97          FORMAT(1X,'This file is already open -- will continue readi
     1ng from the current location.')
         END IF
C
C-----LOCAT IS INVALID
      ELSE
         WRITE(*,*) ' INVALID LOCAT IN ARRAY CONTROL RECORD:',LOCAT
         STOP
      END IF
C
C-----LOCAT>0 -- READ RECORDS USING FREE-FORMAT OR FMTIN.
      IF(FMTIN.EQ.' ') THEN
         WRITE(IOUT,98) K
98       FORMAT(1X,'Zone Array for layer',I3,
     1       ' will be read using free format.'/1X,55('-'))
         DO 100 I=1,NROW
         READ(INUNIT,*) (IZONE(J,I,K),J=1,NCOL)
100      CONTINUE
      ELSE
         WRITE(IOUT,104) K,FMTIN
104      FORMAT(1X,'Zone Array for layer',I3,
     1       ' will be read using format: ',A/1X,71('-'))
         DO 110 I=1,NROW
         READ (INUNIT,FMTIN) (IZONE(J,I,K),J=1,NCOL)
110      CONTINUE
      END IF
C
C-----FIND THE MAXIMUM ZONE AND CHECK FOR NEGATIVE IZONE VALUES
320   DO 400 I=1,NROW
      DO 400 J=1,NCOL
      IF(IZONE(J,I,K).LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE AT (LAYER,ROW,COLUMN):',K,I,J
         STOP
      END IF
      IF(IZONE(J,I,K).GT.NZONES) NZONES=IZONE(J,I,K)
400   CONTINUE
C
C-----IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.
      IF(IPRN.LT.0) GO TO 1000
C
C-----PRINT COLUMN NUMBERS AT THE TOP OF THE PAGE
      WRITE(IOUT,421) (I,I=1,NCOL)
421   FORMAT(/,(5X,25I3))
      WRITE(IOUT,422)
422   FORMAT(1X,79('-'))
C
C-----PRINT EACH ROW IN THE ARRAY.
      DO 430 I=1,NROW
      WRITE(IOUT,423) I,(IZONE(J,I,K),J=1,NCOL)
423   FORMAT(1X,I3,1X,25I3/(5X,25I3))
430   CONTINUE
C
1000  CONTINUE
      IF(NAMPRV.NE.' ') CLOSE(UNIT=INZN2)
C
C-----CHECK FOR TOO MANY ZONES
      IF(NZONES.GT.NZDIM) THEN
         WRITE(*,*) ' YOUR HIGHEST ZONE EXCEEDS THE PROGRAM MAXIMUM'
         WRITE(*,*) NZDIM,' IS THE HIGHEST ZONE ALLOWED'
         WRITE(*,*) NZONES,' IS YOUR HIGHEST ZONE'
         STOP
      END IF
C
C-----RETURN
      RETURN
      END
      SUBROUTINE INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,NZONES,
     1                  IOUT)
C     ******************************************************************
C     READ COMPOSITE ZONES
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP)
      CHARACTER*80 ALPHA
      DATA ALPHA/'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!"
     1#$%()*+,-./:;<=>?@[\]^{|}~'/
C     ------------------------------------------------------------------
C
C-----READ THE COMPOSITE ZONES
      DO 10 I=1,MXCOMP
      READ(INZN1,4,END=20) (ICOMP(J,I),J=1,MXZWCZ)
C-----Read 10 zones per record -- if MXZWCZ is greater than 10, multiple
C-----records will be required for each composite zone.
4     FORMAT(10I4)
10    CONTINUE
      I=MXCOMP+1
20    NCOMP=I-1
      IF(NCOMP.EQ.0) RETURN
C
C-----FIND HOW MANY ZONES MAKE UP EACH COMPOSITE ZONE
      DO 40 I=1,NCOMP
      DO 30 J=1,MXZWCZ
      IF(ICOMP(J,I).LE.0 .OR. ICOMP(J,I).GT.NZONES) GO TO 35
30    CONTINUE
      J=MXZWCZ+1
35    NZWCZ(I)=J-1
      IF(NZWCZ(I).EQ.0) THEN
         NCOMP=I-1
         IF(NCOMP.EQ.0) RETURN
         GO TO 50
      END IF
40    CONTINUE
C
C-----WRITE THE COMPOSITE ZONES
50    WRITE(IOUT,*)
      WRITE(IOUT,52) NCOMP
52    FORMAT(1X,I3,' Composite Zones:')
      DO 60 I=1,NCOMP
      WRITE(IOUT,54) ALPHA(I:I),(ICOMP(J,I),J=1,NZWCZ(I))
54    FORMAT(1X,'Composite Zone ',A,':',15I4/(18X,15I4))
60    CONTINUE
C
      RETURN
      END
      SUBROUTINE BLOCK(IZONE,NLAY,NROW,NCOL,NZDIM,NZONES,IOUT)
C     ******************************************************************
C     INPUT ZONE VALUES BY BLOCK
C     ******************************************************************
      DIMENSION IZONE(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C-----INITIALIZE THE IZONE ARRAY
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      IZONE(J,I,K)=0
5     CONTINUE
      WRITE(IOUT,*)
C
10    WRITE(*,*)
      WRITE(*,*) ' Enter the start layer, stop layer (0,0 means done):'
      READ(*,*) K1,K2
      IF(K1.EQ.0 .AND. K2.EQ.0) RETURN
      IF(K1.LT.1 .OR. K2.GT.NLAY) THEN
         WRITE(*,*) ' NON-EXISTENT LAYER -- TRY AGAIN'
         GO TO 10
      END IF
20    WRITE(*,*) ' Enter the start row, stop row:'
      READ(*,*) I1,I2
      IF(I1.LT.1 .OR. I2.GT.NROW) THEN
         WRITE(*,*) ' NON-EXISTENT ROW -- TRY AGAIN'
         GO TO 20
      END IF
30    WRITE(*,*) ' Enter the start column, stop column:'
      READ(*,*) J1,J2
      IF(J1.LT.1 .OR. J2.GT.NCOL) THEN
         WRITE(*,*) ' NON-EXISTENT COLUMN -- TRY AGAIN'
      END IF
C
40    WRITE(*,*) ' Enter the zone for this block:'
      READ(*,*) ICONST
      IF(ICONST.LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONES ARE NOT ALLOWED'
         GO TO 40
      END IF
      IF(ICONST.GT.NZDIM) THEN
         WRITE(*,*) ' THE HIGHEST ZONE ALLOWED IS',NZDIM
         GO TO 40
      END IF
      IF(ICONST.GT.NZONES) NZONES=ICONST
C
      WRITE(IOUT,3) K1,K2,I1,I2,J1,J2,ICONST
3     FORMAT(1X,'Zone block: LAYERS ',I2,'-',I2,
     1                  '      ROWS ',I3,'-',I3,
     2               '      COLUMNS ',I3,'-',I3,
     3                 '      VALUE:',I3)
C
      DO 50 K=K1,K2
      DO 50 I=I1,I2
      DO 50 J=J1,J2
      IZONE(J,I,K)=ICONST
50    CONTINUE
      GO TO 10
C
      END
      SUBROUTINE ACCM(BUFF,IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,VBZNFL,
     1                MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2                ITYPE,NLIST,INBUD,NVAL)
C     ******************************************************************
C     ACCUMULATE VOLUMETRIC BUDGET FOR ZONES
C     ******************************************************************
      DIMENSION VBVL(2,NTRDIM,NZDIM),BUFF(NCOL,NROW,NLAY),
     1  VBZNFL(2,0:NZDIM,0:NZDIM),IZONE(NCOL,NROW,NLAY),
     2  ICH(NCOL,NROW,NLAY)
      DOUBLE PRECISION VBVL,VBZNFL,DBUFF
      CHARACTER*16 VBNM(NTRDIM),TEXT
      DIMENSION VAL(10)
C     ------------------------------------------------------------------
      ZERO=0.0
      NRC=NROW*NCOL
C
C-----CHECK FOR INTERNAL FLOW TERMS, WHICH ARE USED TO CALCULATE FLOW
C-----BETWEEN ZONES, AND CONSTANT-HEAD TERMS
      IF(TEXT.EQ.'   CONSTANT HEAD') GO TO 200
      IF(TEXT.EQ.'FLOW RIGHT FACE ') GO TO 300
      IF(TEXT.EQ.'FLOW FRONT FACE ') GO TO 400
      IF(TEXT.EQ.'FLOW LOWER FACE ') GO TO 500
C
C-----NOT AN INTERNAL FLOW TERM, SO MUST BE A SOURCE TERM OR STORAGE
C-----ACCUMULATE THE FLOW BY ZONE
      IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
C  LIST
         IF(NLIST.GT.0) THEN
            DO 80 N=1,NLIST
            READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL +1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            NZ=IZONE(J,I,K)
            IF(NZ.EQ.0) GO TO 80
            RBUFF=VAL(1)
            DBUFF=RBUFF
            IF(RBUFF.EQ.ZERO) THEN
            ELSE IF(RBUFF.LT.ZERO) THEN
               VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
            ELSE
               VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
            END IF
80          CONTINUE
         END IF
      ELSE
         DO 100 K=1,NLAY
         DO 100 I=1,NROW
         DO 100 J=1,NCOL
         NZ=IZONE(J,I,K)
         IF(NZ.EQ.0) GO TO 100
         RBUFF=BUFF(J,I,K)
         DBUFF=RBUFF
         IF(RBUFF.EQ.ZERO) THEN
         ELSE IF(RBUFF.LT.ZERO) THEN
            VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
         ELSE
            VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
         END IF
  100    CONTINUE
      END IF
C
C-----SAVE THE TERM NAME AND KEEP TRACK OF THE NUMBER OF TERMS
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
      RETURN
C
C-----CONSTANT-HEAD FLOW -- DON'T ACCUMULATE THE CELL-BY-CELL VALUES FOR
C-----CONSTANT-HEAD FLOW BECAUSE THEY MAY INCLUDE PARTIALLY CANCELING
C-----INS AND OUTS.  USE CONSTANT-HEAD TERM TO IDENTIFY WHERE CONSTANT-
C-----HEAD CELLS ARE AND THEN USE FACE FLOWS TO DETERMINE THE AMOUNT OF
C-----FLOW.  STORE CONSTANT-HEAD LOCATIONS IN ICH ARRAY.
200   IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
         DO 240 K=1,NLAY
         DO 240 I=1,NROW
         DO 240 J=1,NCOL
         ICH(J,I,K)=0
240      CONTINUE
         IF(NLIST.GT.0) THEN
            DO 250 N=1,NLIST
            READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL +1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            ICH(J,I,K)=1
250         CONTINUE
         END IF
      ELSE
         DO 260 K=1,NLAY
         DO 260 I=1,NROW
         DO 260 J=1,NCOL
         ICH(J,I,K)=0
         IF(BUFF(J,I,K).NE.ZERO) ICH(J,I,K)=1
260      CONTINUE
      END IF
      VBNM(MSUM)=TEXT
      MSUMCH=MSUM
      MSUM=MSUM+1
      RETURN
C
C-----"FLOW RIGHT FACE"  COMPUTE FLOW BETWEEN ZONES ACROSS COLUMNS.
C-----COMPUTE FLOW ONLY BETWEEN A ZONE AND A HIGHER ZONE -- FLOW FROM
C-----ZONE 4 TO 3 IS THE NEGATIVE OF FLOW FROM 3 TO 4.
C-----1ST, CALCULATE FLOW BETWEEN NODE J,I,K AND J-1,I,K
300   IF(NCOL.LT.2) RETURN
      DO 340 K=1,NLAY
      DO 340 I=1,NROW
      DO 340 J=2,NCOL
      NZ=IZONE(J,I,K)
      JL=J-1
      NZL=IZONE(JL,I,K)
      IF(NZL.LE.NZ) GO TO 340
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J-1,I,K).EQ.1) GO TO 340
      RBUFF=BUFF(JL,I,K)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(2,NZ,NZL)=VBZNFL(2,NZ,NZL)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZL)=VBZNFL(1,NZ,NZL)+DBUFF
      END IF
  340 CONTINUE
C
C-----FLOW BETWEEN NODE J,I,K AND J+1,I,K
      DO 370 K=1,NLAY
      DO 370 I=1,NROW
      DO 370 J=1,NCOL-1
      NZ=IZONE(J,I,K)
      JR=J+1
      NZR=IZONE(JR,I,K)
      IF(NZR.LE.NZ) GO TO 370
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J+1,I,K).EQ.1) GO TO 370
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(1,NZ,NZR)=VBZNFL(1,NZ,NZR)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZR)=VBZNFL(2,NZ,NZR)+DBUFF
      END IF
  370 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      DO 395 K=1,NLAY
      DO 395 I=1,NROW
      DO 395 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 395
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 395
      IF(J.EQ.NCOL) GO TO 380
      IF(ICH(J+1,I,K).EQ.1) GO TO 380
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
380   IF(J.EQ.1) GO TO 395
      IF(ICH(J-1,I,K).EQ.1) GO TO 395
      RBUFF=BUFF(J-1,I,K)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
395   CONTINUE
      RETURN
C
C-----"FLOW FRONT FACE"
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I-1,K
400   IF(NROW.LT.2) RETURN
      DO 440 K=1,NLAY
      DO 440 I=2,NROW
      DO 440 J=1,NCOL
      NZ=IZONE(J,I,K)
      IA=I-1
      NZA=IZONE(J,IA,K)
      IF(NZA.LE.NZ) GO TO 440
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I-1,K).EQ.1) GO TO 440
      RBUFF=BUFF(J,IA,K)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(2,NZ,NZA)=VBZNFL(2,NZ,NZA)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZA)=VBZNFL(1,NZ,NZA)+DBUFF
      END IF
  440 CONTINUE
C
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I+1,K
      DO 470 K=1,NLAY
      DO 470 I=1,NROW-1
      DO 470 J=1,NCOL
      NZ=IZONE(J,I,K)
      IB=I+1
      NZB=IZONE(J,IB,K)
      IF(NZB.LE.NZ) GO TO 470
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I+1,K).EQ.1) GO TO 470
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(1,NZ,NZB)=VBZNFL(1,NZ,NZB)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZB)=VBZNFL(2,NZ,NZB)+DBUFF
      END IF
  470 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      DO 495 K=1,NLAY
      DO 495 I=1,NROW
      DO 495 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 495
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 495
      IF(I.EQ.NROW) GO TO 480
      IF(ICH(J,I+1,K).EQ.1) GO TO 480
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
480   IF(I.EQ.1) GO TO 495
      IF(ICH(J,I-1,K).EQ.1) GO TO 495
      RBUFF=BUFF(J,I-1,K)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
495   CONTINUE
      RETURN
C
C-----"FLOW LOWER FACE"
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I,K-1
500   IF(NLAY.LT.2) RETURN
      DO 540 K=2,NLAY
      DO 540 I=1,NROW
      DO 540 J=1,NCOL
      NZ=IZONE(J,I,K)
      KA=K-1
      NZA=IZONE(J,I,KA)
      IF(NZA.LE.NZ) GO TO 540
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I,K-1).EQ.1) GO TO 540
      RBUFF=BUFF(J,I,KA)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(2,NZ,NZA)=VBZNFL(2,NZ,NZA)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZA)=VBZNFL(1,NZ,NZA)+DBUFF
      END IF
  540 CONTINUE
C
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I,K+1
      DO 570 K=1,NLAY-1
      DO 570 I=1,NROW
      DO 570 J=1,NCOL
      NZ=IZONE(J,I,K)
      KB=K+1
      NZB=IZONE(J,I,KB)
      IF(NZB.LE.NZ) GO TO 570
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I,K+1).EQ.1) GO TO 570
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.LT.ZERO) THEN
         VBZNFL(1,NZ,NZB)=VBZNFL(1,NZ,NZB)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZB)=VBZNFL(2,NZ,NZB)+DBUFF
      END IF
  570 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      DO 595 K=1,NLAY
      DO 595 I=1,NROW
      DO 595 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 595
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 595
      IF(K.EQ.NLAY) GO TO 580
      IF(ICH(J,I,K+1).EQ.1) GO TO 580
      RBUFF=BUFF(J,I,K)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
580   IF(K.EQ.1) GO TO 595
      IF(ICH(J,I,K-1).EQ.1) GO TO 595
      RBUFF=BUFF(J,I,K-1)
      DBUFF=RBUFF
      IF(RBUFF.EQ.ZERO) THEN
      ELSE IF(RBUFF.LT.ZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
595   CONTINUE
      RETURN
C
      END
      SUBROUTINE SUBPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,NZONES,IOUT,
     1               NTRDIM,NZDIM,TITLE)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,TOTOUT,TOTIN,TOTBD,DHUN,DTWO
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
C     ------------------------------------------------------------------
      ZERO=0.0
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
      DO 50 K=0,NZONES-1
      DO 50 J=K+1,NZONES
      DO 50 I=1,2
      VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
50    CONTINUE
C
C-----FOR EACH ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 N=1,NZONES
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=ZERO
      TOTIN=ZERO
      DO 100 I=1,MTOT
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
      DO 150 I=0,NZONES
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.ZERO .AND. TOTOUT.EQ.ZERO) THEN
         TOTBD=ZERO
         PERCNT=ZERO
      ELSE
         TOTBD=TOTIN-TOTOUT
         PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
      END IF
C
C
C     ---PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) N,KSTP,KPER
C
C-----PRINT THE IN TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(1,I,N)
200   CONTINUE
      DO 250 I=0,NZONES
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1                    WRITE(IOUT,609) I,N,VBZNFL(1,N,I)
250   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----PRINT THE OUT TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(2,I,N)
300   CONTINUE
      DO 350 I=0,NZONES
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1                    WRITE(IOUT,609) N,I,VBZNFL(2,N,I)
350   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Zone',I3,
     1  ' at Time Step',I3,' of Stress Period',I3/5X,59('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(21X,'Zone',I3,' to',I3,' =',G14.5)
      END
      SUBROUTINE COMPPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,NZONES,IOUT,
     1             NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,MXCOMP,MXZWCZ)
C     ******************************************************************
C     COMPUTE BUDGET TOTALS FOR COMPOSITE ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM)
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP)
      DOUBLE PRECISION VBVL,VBZNFL
      DOUBLE PRECISION TOTOUT,TOTIN,TOTBD,DHUN,DTWO,TSUM1,TSUM2
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      CHARACTER*80 ALPHA
      DATA ALPHA/'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!"
     1#$%()*+,-./:;<=>?@[\]^{|}~'/
C     ------------------------------------------------------------------
      DHUN=100.
      DTWO=2.
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
C
C-----FOR EACH COMPOSITE ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 M=1,NCOMP
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=0.
      TOTIN=0.
C
C-----TOTAL THE BUDGET TERMS
      DO 100 I=1,MTOT
      DO 100 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
C
C-----TOTAL THE FLOW ACROSS ZONE BOUNDARIES
      DO 150 I=0,NZONES
C
C-----SKIP FLOW TO ANY ZONES THAT ARE PART OF COMPOSITE ZONE
      DO 130 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(I.EQ.N) GO TO 150
130   CONTINUE
      DO 140 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
140   CONTINUE
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.0. .AND. TOTOUT.EQ.0.) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C-----PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) ALPHA(M:M),KSTP,KPER
      WRITE(IOUT,*)
      WRITE(IOUT,611) ALPHA(M:M),(ICOMP(J,M),J=1,NZWCZ(M))
C
C-----TOTAL AND PRINT THE IN BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      TSUM1=0.
      DO 180 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBVL(1,I,N)
180   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM1
200   CONTINUE
C
C-----TOTAL AND PRINT THE IN FLOW ACROSS ZONE BOUNDARIES
      DO 250 I=0,NZONES
      DO 230 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 250
      TSUM1=0.
      TSUM2=0.
230   CONTINUE
      DO 240  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
240   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.) WRITE(IOUT,609) I,ALPHA(M:M),
     1          TSUM1
250   CONTINUE
C
C-----WRITE THE TOTALS OF ALL INS
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----TOTAL AND PRINT THE OUT BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      TSUM2=0.
      DO 280 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM2=TSUM2+VBVL(2,I,N)
280   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM2
300   CONTINUE
C
C-----TOTAL AND PRINT THE OUT FLOW ACROSS ZONE BOUNDARIES
      DO 350 I=0,NZONES
      DO 330 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 350
330   CONTINUE
      TSUM1=0.
      TSUM2=0.
      DO 340  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
340   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.) WRITE(IOUT,610) ALPHA(M:M),I,
     1               TSUM2
350   CONTINUE
C
C-----WRITE TOTAL OUTS
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Composite Zone ',A,
     1  ' at Time Step',I3,' of Stress Period',I3/5X,68('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(22X,'Zone',I3,' to ',A,' =',G14.5)
  610 FORMAT(22X,'Zone ',A,' to',I3,' =',G14.5)
  611 FORMAT(5X,'Zone ',A,' consists of the following numeric zones:'/
     1      (5X,15I4))
      END
