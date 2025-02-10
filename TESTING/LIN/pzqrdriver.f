      PROGRAM PZQRDRIVER
*
*  -- ScaLAPACK testing driver (version 1.7) --
*     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
*     and University of California, Berkeley.
*     May 28, 2001
*     Modifications Copyright (c) 2024-25 Advanced Micro Devices, Inc. All rights reserved.
*
*  Purpose
*  =======
*
*  PZQRDRIVER is the main test program for the COMPLEX*16
*  SCALAPACK QR factorization routines. This test driver performs a QR
*  QL, LQ, RQ, QP (QR factorization with column pivoting) or TZ
*  (complete unitary factorization) factorization and checks the
*  results.
*
*  The program must be driven by a short data file.  An annotated
*  example of a data file can be obtained by deleting the first 3
*  characters from the following 16 lines:
*  'ScaLAPACK QR factorizations input file'
*  'PVM machine'
*  'QR.out'                      output file name (if any)
*  6                             device out
*  6                             number of factorizations
*  'QR' 'QL' 'LQ' 'RQ' 'QP' 'TZ' factorization: QR, QL, LQ, RQ, QP, TZ
*  4                             number of problems sizes
*  55 17 31 201                  values of M
*  5 71 31 201                   values of N
*  3                             number of MB's and NB's
*  4 3 5                         values of MB
*  4 7 3                         values of NB
*  7                             number of process grids (ordered P & Q)
*  1 2 1 4 2 3 8                 values of P
*  7 2 4 1 3 2 1                 values of Q
*  1.0                           threshold
*
*  Internal Parameters
*  ===================
*
*  TOTMEM   INTEGER, default = 2000000
*           TOTMEM is a machine-specific parameter indicating the
*           maximum amount of available memory in bytes.
*           The user should customize TOTMEM to his platform.  Remember
*           to leave room in memory for the operating system, the BLACS
*           buffer, etc.  For example, on a system with 8 MB of memory
*           per process (e.g., one processor on an Intel iPSC/860), the
*           parameters we use are TOTMEM=6200000 (leaving 1.8 MB for OS,
*           code, BLACS buffer, etc).  However, for PVM, we usually set
*           TOTMEM = 2000000.  Some experimenting with the maximum value
*           of TOTMEM may be required.
*
*  INTGSZ   INTEGER, default = 4 bytes.
*  DBLESZ   INTEGER, default = 8 bytes.
*  ZPLXSZ   INTEGER, default = 16 bytes.
*           INTGSZ, DBLESZ and ZPLXSZ indicate the length in bytes on
*           the given platform for an integer, a double precision real
*           and a double precision complex.
*  MEM      COMPLEX*16 array, dimension ( TOTMEM / ZPLXSZ )
*
*           All arrays used by SCALAPACK routines are allocated from
*           this array and referenced by pointers.  The integer IPA,
*           for example, is a pointer to the starting element of MEM for
*           the matrix A.
*
*  =====================================================================
*
      use,intrinsic :: ieee_arithmetic
*     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_,
     $                   LLD_, MB_, M_, NB_, N_, RSRC_
      PARAMETER          ( BLOCK_CYCLIC_2D = 1, DLEN_ = 9, DTYPE_ = 1,
     $                     CTXT_ = 2, M_ = 3, N_ = 4, MB_ = 5, NB_ = 6,
     $                     RSRC_ = 7, CSRC_ = 8, LLD_ = 9 )
*
      INTEGER            INTGSZ
#ifdef ENABLE_ILP64
      PARAMETER          ( INTGSZ = 8 )
#else
      PARAMETER          ( INTGSZ = 4 )
#endif
*
      INTEGER            DBLESZ, MEMSIZ, NTESTS, TOTMEM, ZPLXSZ
#ifndef DYNAMIC_WORK_MEM_ALLOC
      PARAMETER          ( TOTMEM = 2000000 )
#else
      PARAMETER          ( TOTMEM = 2100000000 )
#endif
      COMPLEX*16         PADVAL
      PARAMETER          ( DBLESZ = 8,
     $                     ZPLXSZ = 16, MEMSIZ = TOTMEM / ZPLXSZ,
     $                     NTESTS = 20,
     $                     PADVAL = ( -9923.0D+0, -9923.0D+0 ) )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        FACT
      CHARACTER*6        PASSED
      CHARACTER*7        ROUT
      CHARACTER*8        ROUTCHK
      CHARACTER*80       OUTFILE
      LOGICAL            CHECK
      LOGICAL            M_INVALID, N_INVALID
      INTEGER            I, IAM, IASEED, ICTXT, IMIDPAD, INFO, IPA,
     $                   IPOSTPAD, IPPIV, IPREPAD, IPTAU, IPRW, IPW, J,
     $                   K, KFAIL, KPASS, KSKIP, KTESTS, L, LIPIV,
     $                   LRWORK, LTAU, LWORK, M, MAXMN, MB, MINMN, MNP,
     $                   MNQ, MP, MYCOL, MYROW, N, NB, NFACT, NGRIDS,
     $                   NMAT, NNB, NOUT, NPCOL, NPROCS, NPROW, NQ,
     $                   WORKFCT, WORKRFCT, WORKSIZ
      REAL               THRESH
      DOUBLE PRECISION   ANORM, FRESID, NOPS, TMFLOPS
      CHARACTER*8        API_NAME
*     ..
*     .. Arrays ..
      CHARACTER*2        FACTOR( NTESTS )
      INTEGER            DESCA( DLEN_ ), IERR( 1 ), MBVAL( NTESTS ),
     $                   MVAL( NTESTS ), NBVAL( NTESTS ),
     $                   NVAL( NTESTS ), PVAL( NTESTS ), QVAL( NTESTS )
      DOUBLE PRECISION   CTIME( 1 ), WTIME( 1 )
#ifndef DYNAMIC_WORK_MEM_ALLOC
      COMPLEX*16         MEM( MEMSIZ )
#else
      COMPLEX*16, allocatable :: MEM (:)
#endif
*     ..
*     .. External Subroutines ..
      EXTERNAL           BLACS_BARRIER, BLACS_EXIT, BLACS_GET,
     $                   BLACS_GRIDEXIT, BLACS_GRIDINFO, BLACS_GRIDINIT,
     $                   BLACS_PINFO, DESCINIT, IGSUM2D, PZCHEKPAD,
     $                   PZFILLPAD, PZGELQF, PZGELQRV,
     $                   PZGEQLF, PZGEQLRV, PZGEQPF,
     $                   PZQPPIV, PZGEQRF, PZGEQRRV,
     $                   PZGERQF, PZGERQ2, PZGERQRV, PZTZRZRV,
     $                   PZMATGEN, PZLAFCHK, PZQRINFO,
     $                   PZTZRZF, SLBOOT, SLCOMBINE, SLTIMER
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      INTEGER            ICEIL, NUMROC
      DOUBLE PRECISION   PZLANGE
      EXTERNAL           ICEIL, LSAMEN, NUMROC, PZLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data Statements ..
      DATA               KTESTS, KPASS, KFAIL, KSKIP /4*0/
*
*     Take command-line arguments if requested
      CHARACTER*80 arg
      INTEGER numArgs, count
      LOGICAL :: help_flag = .FALSE.
      LOGICAL :: EX_FLAG = .FALSE., RES_FLAG = .FALSE.
      INTEGER :: INF_PERCENT = 0
      INTEGER :: NAN_PERCENT = 0
      DOUBLE PRECISION :: X
*
*     ..
*     .. Executable Statements ..
*
*     Get starting information
*
#ifdef DYNAMIC_WORK_MEM_ALLOC
      allocate(MEM(MEMSIZ))
#endif
      CALL BLACS_PINFO( IAM, NPROCS )
      IASEED = 100
      CALL PZQRINFO( OUTFILE, NOUT, NFACT, FACTOR, NTESTS, NMAT, MVAL,
     $               NTESTS, NVAL, NTESTS, NNB, MBVAL, NTESTS, NBVAL,
     $               NTESTS, NGRIDS, PVAL, NTESTS, QVAL, NTESTS,
     $               THRESH, MEM, IAM, NPROCS )
      CHECK = ( THRESH.GE.0.0E+0 )
      M_INVALID = .TRUE.
      N_INVALID = .TRUE.
*     Get the number of command-line arguments
      numArgs = command_argument_count()

*     Process command-line arguments
      do count = 1, numArgs, 2
         call get_command_argument(count, arg)
         select case (arg)
            case ("-h", "--help")
                  help_flag = .true.
                  exit
            case ("-inf")
                  call get_command_argument(count + 1, arg)
                  read(arg, *) INF_PERCENT
                  IF (INF_PERCENT .GT. 0) THEN
                     EX_FLAG = .TRUE.
                  END IF
            case ("-nan")
                  call get_command_argument(count + 1, arg)
                  read(arg, *) NAN_PERCENT
                  IF (NAN_PERCENT .GT. 0) THEN
                     EX_FLAG = .TRUE.
                  END IF
            case default
                  print *, "Invalid option: ", arg
                  help_flag = .true.
                  exit
            end select
      end do
*
*
*     Loop over the different factorization types
*
      DO 40 I = 1, NFACT
*
         FACT = FACTOR( I )
*
*        Print headings
*
         IF( IAM.EQ.0 ) THEN
            WRITE( NOUT, FMT = * )
            IF( LSAMEN( 2, FACT, 'QR' ) ) THEN
               ROUT = 'PZGEQRF'
               ROUTCHK = 'PZGEQRRV'
               WRITE( NOUT, FMT = 9986 )
     $                'QR factorization tests.'
            ELSE IF( LSAMEN( 2, FACT, 'QL' ) ) THEN
               ROUT = 'PZGEQLF'
               ROUTCHK = 'PZGEQLRV'
               WRITE( NOUT, FMT = 9986 )
     $                'QL factorization tests.'
            ELSE IF( LSAMEN( 2, FACT, 'LQ' ) ) THEN
               ROUT = 'PZGELQF'
               ROUTCHK = 'PZGELQRV'
               WRITE( NOUT, FMT = 9986 )
     $                'LQ factorization tests.'
            ELSE IF( LSAMEN( 2, FACT, 'RQ' ) ) THEN
               ROUT = 'PZGERQF'
               ROUTCHK = 'PZGERQRV'
               WRITE( NOUT, FMT = 9986 )
     $                'RQ factorization tests.'
            ELSE IF( LSAMEN( 2, FACT, 'R2' ) ) THEN
               ROUT = 'PZGERQ2'
               ROUTCHK = 'PZGERQRV'
               WRITE( NOUT, FMT = 9986 )
     $                'RQ2 factorization tests.'
            ELSE IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
               ROUT = 'PZGEQPF'
               ROUTCHK = 'PZGEQRRV'
               WRITE( NOUT, FMT = 9986 )
     $                'QR factorization with column pivoting tests.'
            ELSE IF( LSAMEN( 2, FACT, 'TZ' ) ) THEN
               ROUT = 'PZTZRZF'
               ROUTCHK = 'PZTZRZRV'
               WRITE( NOUT, FMT = 9986 )
     $                'Complete unitary factorization tests.'
            END IF
            WRITE( NOUT, FMT = * )
            WRITE( NOUT, FMT = 9995 )
            WRITE( NOUT, FMT = 9994 )
            WRITE( NOUT, FMT = * )
         END IF
*
*        Loop over different process grids
*
         DO 30 J = 1, NGRIDS
*
            NPROW = PVAL( J )
            NPCOL = QVAL( J )
*
*           Make sure grid information is correct
*
            IERR( 1 ) = 0
            IF( NPROW.LT.1 ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9999 ) 'GRID', 'nprow', NPROW
               IERR( 1 ) = 1
            ELSE IF( NPCOL.LT.1 ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9999 ) 'GRID', 'npcol', NPCOL
               IERR( 1 ) = 1
            ELSE IF( NPROW*NPCOL.GT.NPROCS ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9998 ) NPROW*NPCOL, NPROCS
               IERR( 1 ) = 1
            END IF
*
            IF( IERR( 1 ).GT.0 ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9997 ) 'grid'
               KSKIP = KSKIP + 1
               GO TO 30
            END IF
*
*           Define process grid
*
            CALL BLACS_GET( -1, 0, ICTXT )
            CALL BLACS_GRIDINIT( ICTXT, 'Row-major', NPROW, NPCOL )
            CALL BLACS_GRIDINFO( ICTXT, NPROW, NPCOL, MYROW, MYCOL )
*
*           Go to bottom of loop if this case doesn't use my process
*
            IF( MYROW.GE.NPROW .OR. MYCOL.GE.NPCOL )
     $         GO TO 30
*
            DO 20 K = 1, NMAT
*
               M = MVAL( K )
               N = NVAL( K )
*
*              Make sure matrix information is correct
*
#ifdef ENABLE_DRIVER_CHECK
               IERR(1) = 0
               IF( M.LT.1 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9999 ) 'MATRIX', 'M', M
                  IERR( 1 ) = 1
               ELSE IF( N.LT.1 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9999 ) 'MATRIX', 'N', N
                  IERR( 1 ) = 1
               END IF
#endif
*
*              Make sure no one had error
*
               CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1, 0 )
*
#ifdef ENABLE_DRIVER_CHECK
               IF( IERR( 1 ).GT.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9997 ) 'matrix'
                  KSKIP = KSKIP + 1
                  GO TO 20
               END IF
#endif
*
*              Loop over different blocking sizes
*
               DO 10 L = 1, NNB
*
                  MB = MBVAL( L )
                  NB = NBVAL( L )
*
*                 Make sure mb is legal
*
                  IERR( 1 ) = 0
                  IF( MB.LT.1 ) THEN
                     IERR( 1 ) = 1
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9999 ) 'MB', 'MB', MB
                  END IF
*
*                 Check all processes for an error
*
                  CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1,
     $                          0 )
*
                  IF( IERR( 1 ).GT.0 ) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9997 ) 'MB'
                     KSKIP = KSKIP + 1
                     GO TO 10
                  END IF
*
*                 Make sure nb is legal
*
                  IERR( 1 ) = 0
                  IF( NB.LT.1 ) THEN
                     IERR( 1 ) = 1
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9999 ) 'NB', 'NB', NB
                  END IF
*
*                 Check all processes for an error
*
                  CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1,
     $                          0 )
*
                  IF( IERR( 1 ).GT.0 ) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9997 ) 'NB'
                     KSKIP = KSKIP + 1
                     GO TO 10
                  END IF
*
*                 Padding constants
*
                  MP  = NUMROC( M, MB, MYROW, 0, NPROW )
                  NQ  = NUMROC( N, NB, MYCOL, 0, NPCOL )
                  MNP = NUMROC( MIN( M, N ), MB, MYROW, 0, NPROW )
                  MNQ = NUMROC( MIN( M, N ), NB, MYCOL, 0, NPCOL )
                  IF( CHECK ) THEN
                     IPREPAD  = MAX( MB, MP )
                     IMIDPAD  = NB
                     IPOSTPAD = MAX( NB, NQ )
                  ELSE
                     IPREPAD  = 0
                     IMIDPAD  = 0
                     IPOSTPAD = 0
                  END IF
*
*                 Initialize the array descriptor for the matrix A
*
                  CALL DESCINIT( DESCA, M, N, MB, NB, 0, 0, ICTXT,
     $                           MAX( 1, MP ) + IMIDPAD, IERR( 1 ) )
*
*                 Check all processes for an error
*
                  CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1,
     $                          0 )
*
#ifdef ENABLE_DRIVER_CHECK
                  IF( IERR( 1 ).LT.0 ) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9997 ) 'descriptor'
                     KSKIP = KSKIP + 1
                     GO TO 10
                  END IF
#else
                  IF(N .LT. 0 .AND. (IERR(1) .EQ. -2 .OR.
     $              IERR(1) .EQ. -4 .OR. IERR(1) .EQ. -8 .OR.
     $              IERR(1) .EQ. -3 .OR. IERR(1) .EQ. -12 )) THEN
*                   DESCINIT returns the correct error code,
*                   -2, -3 incase of invalid M and N
*                   -4, -8 or -12 incase of incorrect grid info
*                   MAIN API can be validated.
*                   Do NOTHING
                    WRITE( NOUT, FMT = 9984 ) 'N'
*                   disable extreme value case when N < 0
                    EX_FLAG = .FALSE.
                  ELSE IF(M .LT. 0 .AND. (IERR(1) .EQ. -2 .OR.
     $               IERR(1) .EQ. -4 .OR. IERR(1) .EQ. -8 .OR.
     $               IERR(1) .EQ. -3 .OR. IERR(1) .EQ. -12  )) THEN
                    WRITE( NOUT, FMT = 9984 ) 'M'
*                   disable extreme value case when M < 0
                    EX_FLAG = .FALSE.
                  ELSE IF(M .EQ. 0 .OR. N .EQ. 0) THEN
*                   disable extreme value case when M < 0
                    EX_FLAG = .FALSE.
                  ELSE IF(IERR(1) .LT. 0) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9997 ) 'descriptor'
                     KSKIP = KSKIP + 1
                     GO TO 10
                  END IF
#endif
*
*                 Assign pointers into MEM for ScaLAPACK arrays, A is
*                 allocated starting at position MEM( IPREPAD+1 )
*
                  IPA   = IPREPAD+1
                  IPTAU = IPA + DESCA( LLD_ ) * NQ + IPOSTPAD + IPREPAD
*
                  IF( LSAMEN( 2, FACT, 'QR' ) ) THEN
*
                     LTAU = MNQ
                     IPW  = IPTAU + LTAU + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the QR
*                    factorization
*
                     LWORK = DESCA( NB_ ) * ( MP + NQ + DESCA( NB_ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     WORKSIZ = WORKFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZGEQRRV and
*                       PZLANGE
*
                        WORKSIZ = LWORK + MP*DESCA( NB_ ) + IPOSTPAD
*
                     END IF
*
                  ELSE IF( LSAMEN( 2, FACT, 'QL' ) ) THEN
*
                     LTAU = NQ
                     IPW = IPTAU + LTAU + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the QL
*                    factorization
*
                     LWORK = DESCA( NB_ ) * ( MP + NQ + DESCA( NB_ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     WORKSIZ = WORKFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZGEQLRV and
*                       PZLANGE
*
                        WORKSIZ = LWORK + MP*DESCA( NB_ ) + IPOSTPAD
*
                     END IF
*
                  ELSE IF( LSAMEN( 2, FACT, 'LQ' ) ) THEN
*
                     LTAU = MNP
                     IPW = IPTAU + LTAU + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the LQ
*                    factorization
*
                     LWORK = DESCA( MB_ ) * ( MP + NQ + DESCA( MB_ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     WORKSIZ = WORKFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZGELQRV and
*                       PZLANGE
*
                        WORKSIZ = LWORK +
     $                            MAX( MP*DESCA( NB_ ), NQ*DESCA( MB_ )
     $                            ) + IPOSTPAD
*
                     END IF
*
                  ELSE IF( LSAMEN( 2, FACT, 'RQ' ) .OR.
     $                     LSAMEN( 2, FACT, 'R2' ) ) THEN
*
                     LTAU = MP
                     IPW = IPTAU + LTAU + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the QR
*                    factorization
*
                     LWORK = DESCA( MB_ ) * ( MP + NQ + DESCA( MB_ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     WORKSIZ = WORKFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZGERQRV and
*                       PZLANGE
*
                        WORKSIZ = LWORK +
     $                            MAX( MP*DESCA( NB_ ), NQ*DESCA( MB_ )
     $                            ) + IPOSTPAD
*
                     END IF
*
                  ELSE IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
*
                     LTAU = MNQ
                     IPPIV = IPTAU + LTAU + IPOSTPAD + IPREPAD
                     LIPIV = ICEIL( INTGSZ*NQ, ZPLXSZ )
                     IPW = IPPIV + LIPIV + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the
*                    factorization i.e from IPW on.
*
                     LWORK = MAX( 3, MP + MAX( 1, NQ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     LRWORK = MAX( 1, 2 * NQ )
                     WORKRFCT = ICEIL( LRWORK*DBLESZ, ZPLXSZ ) +
     $                          IPOSTPAD
                     IPRW = IPW + WORKFCT + IPREPAD
                     WORKSIZ = WORKFCT + IPREPAD + WORKRFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZGEQRRV,
*                       PZLANGE.
*
                        WORKSIZ = MAX( WORKSIZ - IPOSTPAD,
     $                    DESCA( NB_ )*( 2*MP + NQ + DESCA( NB_ ) ) ) +
     $                    IPOSTPAD
                     END IF
*
                  ELSE IF( LSAMEN( 2, FACT, 'TZ' ) ) THEN
*
                     LTAU = MP
                     IPW = IPTAU + LTAU + IPOSTPAD + IPREPAD
*
*                    Figure the amount of workspace required by the TZ
*                    factorization
*
                     LWORK = DESCA( MB_ ) * ( MP + NQ + DESCA( MB_ ) )
                     WORKFCT = LWORK + IPOSTPAD
                     WORKSIZ = WORKFCT
*
                     IF( CHECK ) THEN
*
*                       Figure the amount of workspace required by the
*                       checking routines PZLAFCHK, PZTZRZRV and
*                       PZLANGE
*
                        WORKSIZ = LWORK +
     $                            MAX( MP*DESCA( NB_ ), NQ*DESCA( MB_ )
     $                            ) + IPOSTPAD
*
                     END IF
*
                  END IF
*
*                 Check for adequate memory for problem size
*
                  IERR( 1 ) = 0
                  IF( IPW+WORKSIZ.GT.MEMSIZ ) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9996 )
     $                         FACT // ' factorization',
     $                         ( IPW+WORKSIZ )*ZPLXSZ
                     IERR( 1 ) = 1
                  END IF
*
*                 Check all processes for an error
*
                  CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1,
     $                          0 )
*
                  IF( IERR( 1 ).GT.0 ) THEN
                     IF( IAM.EQ.0 )
     $                  WRITE( NOUT, FMT = 9997 ) 'MEMORY'
                     KSKIP = KSKIP + 1
                     GO TO 10
                  END IF
*
*                 Generate the matrix A
*
                  CALL PZMATGEN( ICTXT, 'N', 'N', DESCA( M_ ),
     $                           DESCA( N_ ), DESCA( MB_ ),
     $                           DESCA( NB_ ), MEM( IPA ),
     $                           DESCA( LLD_ ), DESCA( RSRC_ ),
     $                           DESCA( CSRC_ ), IASEED, 0, MP, 0, NQ,
     $                           MYROW, MYCOL, NPROW, NPCOL )
*
*                 Need the Infinity of A for checking
*
                  IF( CHECK .AND. (N .GT. 0 .AND. M .GT. 0) ) THEN
                     CALL PZFILLPAD( ICTXT, MP, NQ, MEM( IPA-IPREPAD ),
     $                               DESCA( LLD_ ), IPREPAD, IPOSTPAD,
     $                               PADVAL )
                     IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
                        CALL PZFILLPAD( ICTXT, LIPIV, 1,
     $                                  MEM( IPPIV-IPREPAD ), LIPIV,
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                     END IF
                     CALL PZFILLPAD( ICTXT, LTAU, 1,
     $                               MEM( IPTAU-IPREPAD ), LTAU,
     $                               IPREPAD, IPOSTPAD, PADVAL )
                     CALL PZFILLPAD( ICTXT, WORKSIZ-IPOSTPAD, 1,
     $                               MEM( IPW-IPREPAD ),
     $                               WORKSIZ-IPOSTPAD,
     $                               IPREPAD, IPOSTPAD, PADVAL )
                     ANORM = PZLANGE( 'I', M, N, MEM( IPA ), 1, 1,
     $                                DESCA, MEM( IPW ) )
                     CALL PZCHEKPAD( ICTXT, 'PZLANGE', MP, NQ,
     $                               MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                               IPREPAD, IPOSTPAD, PADVAL )
                     CALL PZCHEKPAD( ICTXT, 'PZLANGE',
     $                               WORKSIZ-IPOSTPAD, 1,
     $                               MEM( IPW-IPREPAD ),
     $                               WORKSIZ-IPOSTPAD, IPREPAD,
     $                               IPOSTPAD, PADVAL )
                     IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
                        CALL PZFILLPAD( ICTXT, WORKRFCT-IPOSTPAD, 1,
     $                                  MEM( IPRW-IPREPAD ),
     $                                  WORKRFCT-IPOSTPAD,
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                     END IF
                     CALL PZFILLPAD( ICTXT, WORKFCT-IPOSTPAD, 1,
     $                               MEM( IPW-IPREPAD ),
     $                               WORKFCT-IPOSTPAD,
     $                               IPREPAD, IPOSTPAD, PADVAL )
                  END IF
*
                  CALL SLBOOT()
                  CALL BLACS_BARRIER( ICTXT, 'All' )
*
*                 Perform QR factorizations
*
                  IF( LSAMEN( 2, FACT, 'QR' ) ) THEN
                     API_NAME = 'PZGEQRF'
                     CALL SLTIMER( 1 )
                     CALL PZGEQRF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPTAU ), MEM( IPW ), LWORK,
     $                             INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'QL' ) ) THEN
                     API_NAME = 'PZGEQLF'
                     CALL SLTIMER( 1 )
                     CALL PZGEQLF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPTAU ), MEM( IPW ), LWORK,
     $                             INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'LQ' ) ) THEN
                     API_NAME = 'PZGELQF'
                     CALL SLTIMER( 1 )
                     CALL PZGELQF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPTAU ), MEM( IPW ), LWORK,
     $                             INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'RQ' ) ) THEN
                     API_NAME = 'PZGERQF'
                     CALL SLTIMER( 1 )
                     CALL PZGERQF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPTAU ), MEM( IPW ), LWORK,
     $                             INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'R2' ) ) THEN
                     API_NAME = 'PZGERQ2'
                     CALL SLTIMER( 1 )
                     CALL PZGERQ2( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPTAU ), MEM( IPW ), LWORK,
     $                             INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
                     API_NAME = 'PZGEQPF'
                     CALL SLTIMER( 1 )
                     CALL PZGEQPF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                             MEM( IPPIV ), MEM( IPTAU ),
     $                             MEM( IPW ), LWORK, MEM( IPRW ),
     $                             LRWORK, INFO )
                     CALL SLTIMER( 1 )
                  ELSE IF( LSAMEN( 2, FACT, 'TZ' ) ) THEN
                     API_NAME = 'ZDTZRZF'
                     CALL SLTIMER( 1 )
#ifdef ENABLE_DRIVER_CHECK
                     IF( N.GE.M )
     $                  CALL PZTZRZF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                                MEM( IPTAU ), MEM( IPW ), LWORK,
     $                                INFO )
#else
                     IF( N .LT. M ) THEN
                       WRITE( NOUT, FMT = 9982 )
                     END IF
                     CALL PZTZRZF( M, N, MEM( IPA ), 1, 1, DESCA,
     $                                MEM( IPTAU ), MEM( IPW ), LWORK,
     $                                INFO )
#endif
                     CALL SLTIMER( 1 )
                  END IF
*
                  IF( CHECK  .AND. (.NOT.(EX_FLAG)) ) THEN
*
                     IF(INFO .EQ. 0 .AND. N .GT. 0 .AND.
     $                     M .GT. 0) THEN
*
*
*                       Check for memory overwrite in factorization
*
                        CALL PZCHEKPAD( ICTXT, ROUT, MP, NQ,
     $                               MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                               IPREPAD, IPOSTPAD, PADVAL )
                        CALL PZCHEKPAD( ICTXT, ROUT, LTAU, 1,
     $                               MEM( IPTAU-IPREPAD ), LTAU,
     $                               IPREPAD, IPOSTPAD, PADVAL )
                        IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
                           CALL PZCHEKPAD( ICTXT, ROUT, LIPIV, 1,
     $                                  MEM( IPPIV-IPREPAD ), LIPIV,
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                           CALL PZCHEKPAD( ICTXT, ROUT,
     $                                  WORKRFCT-IPOSTPAD,
     $                                  1, MEM( IPRW-IPREPAD ),
     $                                  WORKRFCT-IPOSTPAD,
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                        END IF
                        CALL PZCHEKPAD( ICTXT, ROUT, WORKFCT-IPOSTPAD,
     $                               1, MEM( IPW-IPREPAD ),
     $                               WORKFCT-IPOSTPAD, IPREPAD,
     $                               IPOSTPAD, PADVAL )
                        CALL PZFILLPAD( ICTXT, WORKSIZ-IPOSTPAD, 1,
     $                               MEM( IPW-IPREPAD ),
     $                               WORKSIZ-IPOSTPAD,
     $                               IPREPAD, IPOSTPAD, PADVAL )
*
                        IF( LSAMEN( 2, FACT, 'QR' ) ) THEN
*
*                          Compute residual = ||A-Q*R|| / (||A||*N*eps)
*
                           CALL PZGEQRRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                              MEM( IPA ), 1,
     $                              1, DESCA, IASEED, ANORM, FRESID,
     $                              MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'QL' ) ) THEN
*
*                          Compute residual = ||A-Q*L|| / (||A||*N*eps)
*
                           CALL PZGEQLRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                              MEM( IPA ), 1,
     $                              1, DESCA, IASEED, ANORM, FRESID,
     $                              MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'LQ' ) ) THEN
*
*                          Compute residual = ||A-L*Q|| / (||A||*N*eps)
*
                           CALL PZGELQRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                              MEM( IPA ), 1,
     $                              1, DESCA, IASEED, ANORM, FRESID,
     $                              MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'RQ' ) ) THEN
*
*                          Compute residual = ||A-R*Q|| / (||A||*N*eps)
*
                           CALL PZGERQRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                              MEM( IPA ), 1,
     $                              1, DESCA, IASEED, ANORM, FRESID,
     $                              MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'R2' ) ) THEN
*
*                          Compute residual = ||A-R*Q|| / (||A||*N*eps)
*
*                          Since PZGERQ2 computes RQ factorization,
*                          validation of PZGERQF can be used
                           CALL PZGERQRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                              MEM( IPA ), 1,
     $                              1, DESCA, IASEED, ANORM, FRESID,
     $                              MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
*
*                          Compute residual = ||AP-Q*R|| / (||A||*N*eps)
*
                           CALL PZGEQRRV( M, N, MEM( IPA ), 1, 1,
     $                                 DESCA,
     $                                 MEM( IPTAU ), MEM( IPW ) )
                        ELSE IF( LSAMEN( 2, FACT, 'TZ' ) ) THEN
*
*                          Compute residual = ||A-T*Z|| / (||A||*N*eps)
*
                           IF( N.GE.M ) THEN
                              CALL PZTZRZRV( M, N, MEM( IPA ), 1, 1,
     $                                    DESCA,
     $                                    MEM( IPTAU ), MEM( IPW ) )
                           END IF
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                                 MEM( IPA ), 1,
     $                                 1, DESCA, IASEED, ANORM, FRESID,
     $                                 MEM( IPW ) )
                        END IF
*
*                       Check for memory overwrite
*
                        CALL PZCHEKPAD( ICTXT, ROUTCHK, MP, NQ,
     $                               MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                               IPREPAD, IPOSTPAD, PADVAL )
                        CALL PZCHEKPAD( ICTXT, ROUTCHK, LTAU, 1,
     $                               MEM( IPTAU-IPREPAD ), LTAU,
     $                               IPREPAD, IPOSTPAD, PADVAL )
                        CALL PZCHEKPAD( ICTXT, ROUTCHK,
     $                               WORKSIZ-IPOSTPAD,
     $                               1, MEM( IPW-IPREPAD ),
     $                               WORKSIZ-IPOSTPAD, IPREPAD,
     $                               IPOSTPAD, PADVAL )
*
                        IF( LSAMEN( 2, FACT, 'QP' ) ) THEN
*
                           CALL PZQPPIV( M, N, MEM( IPA ), 1, 1, DESCA,
     $                                MEM( IPPIV ) )
*
*                          Check for memory overwrite
*
                           CALL PZCHEKPAD( ICTXT, 'PZQPPIV', MP, NQ,
     $                                  MEM( IPA-IPREPAD ),
     $                                  DESCA( LLD_ ),
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                           CALL PZCHEKPAD( ICTXT, 'PZQPPIV', LIPIV, 1,
     $                                  MEM( IPPIV-IPREPAD ), LIPIV,
     $                                  IPREPAD, IPOSTPAD, PADVAL )
*
                           CALL PZLAFCHK( 'No', 'No', M, N,
     $                                 MEM( IPA ), 1,
     $                                 1, DESCA, IASEED, ANORM, FRESID,
     $                                 MEM( IPW ) )
*
*                          Check for memory overwrite
*
                           CALL PZCHEKPAD( ICTXT, 'PZLAFCHK', MP, NQ,
     $                                  MEM( IPA-IPREPAD ),
     $                                  DESCA( LLD_ ),
     $                                  IPREPAD, IPOSTPAD, PADVAL )
                           CALL PZCHEKPAD( ICTXT, 'PZLAFCHK',
     $                                  WORKSIZ-IPOSTPAD, 1,
     $                                  MEM( IPW-IPREPAD ),
     $                                  WORKSIZ-IPOSTPAD, IPREPAD,
     $                                  IPOSTPAD, PADVAL )

                        END IF
                     END IF
*
*                    Test residual and detect NaN result
*
                     M_INVALID = M.LT.0 .AND.
     $                      ((INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'QR')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'QL')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'LQ')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'RQ')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'R2')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'QP')) .OR.
     $                      (INFO.EQ.-1 .AND.
     $                          LSAMEN( 2, FACT, 'TZ' )))
                     N_INVALID = N.LT.0 .AND.
     $                      ((INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'QR')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'QL')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'LQ')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'RQ')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'R2')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'QP')) .OR.
     $                      (INFO.EQ.-2 .AND.
     $                          LSAMEN( 2, FACT, 'TZ' )))
*
                     IF( (N.EQ.0 .AND. INFO.EQ.0) .OR.
     $                   (M.EQ.0 .AND. INFO.EQ.0) ) THEN
*                       If N =0 this is the case of
*                       early return from ScaLAPACK API.
*                       If there is safe exit from API; pass this case
                        KPASS = KPASS + 1
                        WRITE( NOUT, FMT = 9985 ) KPASS, API_NAME
                        PASSED = 'PASSED'
*                       RE-ENABLE for EX CASE
                        IF(INF_PERCENT .GT. 0 .OR.
     $                        NAN_PERCENT .GT. 0) THEN
                          EX_FLAG = .TRUE.
                        END IF
                        GO TO 10
                     ELSE IF(M_INVALID .OR. N_INVALID) THEN
*                       When N < 0/Invalid, INFO = -2
*                       When M < 0/Invalid, INFO = -1
*                       Expected Error code for N < 0
*                       Hence this case can be passed
                        KPASS = KPASS + 1
                        WRITE( NOUT, FMT = 9983 ) KPASS, API_NAME
                        PASSED = 'PASSED'
*                       RE-ENABLE for EX CASE
                        IF(INF_PERCENT .GT. 0 .OR.
     $                        NAN_PERCENT .GT. 0) THEN
                          EX_FLAG = .TRUE.
                        END IF
                     ELSE IF( LSAMEN( 2, FACT, 'TZ' ) .AND.
     $                    (N.LT.M .AND. INFO.EQ.-2 ) ) THEN
                        KPASS = KPASS + 1
                        WRITE( NOUT, FMT = 9983 ) KPASS, API_NAME
                     ELSE
                        IF( FRESID.LE.THRESH .AND.
     $                      (FRESID-FRESID).EQ.0.0D+0 ) THEN
                           KPASS = KPASS + 1
                           PASSED = 'PASSED'
                        ELSE
                           KFAIL = KFAIL + 1
                           PASSED = 'FAILED'
                        END IF
                     END IF
*
                  ELSE

*                    Extreme value cases
                     IF(EX_FLAG) THEN
*                       Check presence of INF/NAN in output
*                       Pass the case if present
                        DO IK = 0, M
                           DO JK = 1, N+1
                              X = MEM(IK*N + JK)
                              IF (isnan(X)) THEN
*                                NAN DETECTED
                                 RES_FLAG = .TRUE.
                                 EXIT
                              ELSE IF (.NOT.ieee_is_finite(
     $                                    X)) THEN
*                                INFINITY DETECTED
                                 RES_FLAG = .TRUE.
                                 EXIT
                              END IF
                           END DO
                           IF(RES_FLAG) THEN
                              EXIT
                           END IF
                        END DO
                        IF (.NOT.(RES_FLAG)) THEN
                           KFAIL = KFAIL + 1
                           PASSED = 'FAILED'
                        ELSE
                           KPASS = KPASS + 1
                           PASSED = 'PASSED'
*                          RESET RESIDUAL FLAG
                           RES_FLAG = .FALSE.
                        END IF
                        FRESID = FRESID - FRESID
*
*                    Don't perform the checking, only the timing
*                    operation
                     ELSE
                       KPASS = KPASS + 1
                       FRESID = FRESID - FRESID
                       PASSED = 'BYPASS'
                     END IF
*
                  END IF
*
*                 Gather maximum of all CPU and WALL clock timings
*
                  CALL SLCOMBINE( ICTXT, 'All', '>', 'W', 1, 1, WTIME )
                  CALL SLCOMBINE( ICTXT, 'All', '>', 'C', 1, 1, CTIME )
*
*                 Print results
*
                  IF( MYROW.EQ.0 .AND. MYCOL.EQ.0 ) THEN
*
                     MINMN = MIN( M, N )
                     MAXMN = MAX( M, N )
*
                     IF( LSAMEN( 2, FACT, 'TZ' ) ) THEN
                        IF( M.GE.N ) THEN
                           NOPS = 0.0D+0
                        ELSE
*
*                          9 ( M^2 N - M^3 ) + 13 M N - M^2 for
*                          complete unitary factorization (M <= N).
*
                           NOPS = 9.0D+0 * (
     $                            DBLE( N )*( DBLE( M )**2 ) -
     $                            DBLE( M )**3 ) +
     $                            13.0D+0*DBLE( N )*DBLE( M ) -
     $                            DBLE( M )**2
                        END IF
*
                     ELSE
*
*                       8 M N^2 - 8/3 N^2 + 6 M N + 8 N^2 for QR type
*                       factorization when M >= N.
*
                        NOPS = 8.0D+0 * ( DBLE( MINMN )**2 ) *
     $                     ( DBLE( MAXMN )-DBLE( MINMN ) / 3.0D+0 ) +
     $                     ( 6.0D+0 * DBLE( MAXMN ) +
     $                       8.0D+0 * DBLE( MINMN ) ) *
     $                     DBLE( MINMN )
                     END IF
*
*                    Print WALL time
*
                     IF( WTIME( 1 ).GT.0.0D+0 ) THEN
                        TMFLOPS = NOPS / ( WTIME( 1 ) * 1.0D+6 )
                     ELSE
                        TMFLOPS = 0.0D+0
                     END IF
                     IF( WTIME( 1 ).GE.0.0D+0 )
     $                  WRITE( NOUT, FMT = 9993 ) 'WALL', M, N, MB, NB,
     $                         NPROW, NPCOL, WTIME( 1 ), TMFLOPS,
     $                         PASSED, FRESID
*
*                    Print CPU time
*
                     IF( CTIME( 1 ).GT.0.0D+0 ) THEN
                        TMFLOPS = NOPS / ( CTIME( 1 ) * 1.0D+6 )
                     ELSE
                        TMFLOPS = 0.0D+0
                     END IF
                     IF( CTIME( 1 ).GE.0.0D+0 )
     $                  WRITE( NOUT, FMT = 9993 ) 'CPU ', M, N, MB, NB,
     $                         NPROW, NPCOL, CTIME( 1 ), TMFLOPS,
     $                         PASSED, FRESID
*
                  END IF
*
   10          CONTINUE
*
   20       CONTINUE
*
            CALL BLACS_GRIDEXIT( ICTXT )
*
   30    CONTINUE
*
   40 CONTINUE
*
*     Print out ending messages and close output file
*
      IF( IAM.EQ.0 ) THEN
         KTESTS = KPASS + KFAIL + KSKIP
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9992 ) KTESTS
         IF( CHECK ) THEN
            WRITE( NOUT, FMT = 9991 ) KPASS
            WRITE( NOUT, FMT = 9989 ) KFAIL
         ELSE
            WRITE( NOUT, FMT = 9990 ) KPASS
         END IF
         WRITE( NOUT, FMT = 9988 ) KSKIP
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9987 )
         IF( NOUT.NE.6 .AND. NOUT.NE.0 )
     $      CLOSE ( NOUT )
      END IF
*
#ifdef DYNAMIC_WORK_MEM_ALLOC
      deallocate(MEM)
#endif
      CALL BLACS_EXIT( 0 )
*
 9999 FORMAT( 'ILLEGAL ', A6, ': ', A5, ' = ', I3,
     $        '; It should be at least 1' )
 9998 FORMAT( 'ILLEGAL GRID: nprow*npcol = ', I4, '. It can be at most',
     $        I4 )
 9997 FORMAT( 'Bad ', A6, ' parameters: going on to next test case.' )
 9996 FORMAT( 'Unable to perform ', A, ': need TOTMEM of at least',
     $        I11 )
 9995 FORMAT( 'TIME      M      N  MB  NB     P     Q Fact Time ',
     $        '     MFLOPS  CHECK  Residual' )
 9994 FORMAT( '---- ------ ------ --- --- ----- ----- --------- ',
     $        '----------- ------  --------' )
 9993 FORMAT( A4, 1X, I6, 1X, I6, 1X, I4, 1X, I4, 1X, I5, 1X, I5, 1X,
     $        F12.2, 1X, F12.2, 1X, A6, 2X, G8.1 )
 9992 FORMAT( 'Finished ', I6, ' tests, with the following results:' )
 9991 FORMAT( I5, ' tests completed and passed residual checks.' )
 9990 FORMAT( I5, ' tests completed without checking.' )
 9989 FORMAT( I5, ' tests completed and failed residual checks.' )
 9988 FORMAT( I5, ' tests skipped because of illegal input values.' )
 9987 FORMAT( 'END OF TESTS.' )
 9986 FORMAT( A )
 9985 FORMAT( '----------Test-',I3,' Passed but no compute was ',
     $       'performed! [Safe exit from ', A,']-----------')
 9984 FORMAT(  A, ' < 0 case detected. ',
     $        'Instead of driver file, This case will be handled',
     $        'by the ScaLAPACK API.')
 9983 FORMAT( '----------Negative Test-',I3,' Passed with expected',
     $       ' ERROR CODE in INFO from ', A,']-----------')
 9982 FORMAT( ' N < M case detected. ',
     $        'Instead of driver file, This case will be handled',
     $        'by the PCTZRZF API.')
*
      STOP
*
*     End of PZQRDRIVER
*
      END
*
      SUBROUTINE PZQPPIV( M, N, A, IA, JA, DESCA, IPIV )
*
*  -- ScaLAPACK routine (version 1.7) --
*     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
*     and University of California, Berkeley.
*     May 1, 1997
*
*     .. Scalar Arguments ..
      INTEGER            IA, JA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            DESCA( * ), IPIV( * )
      COMPLEX*16         A( * )
*     ..
*
*  Purpose
*  =======
*
*  PZQPPIV applies to sub( A ) = A(IA:IA+M-1,JA:JA+N-1) the pivots
*  returned by PZGEQPF in reverse order for checking purposes.
*
*  Notes
*  =====
*
*  Each global data object is described by an associated description
*  vector.  This vector stores the information required to establish
*  the mapping between an object element and its corresponding process
*  and memory location.
*
*  Let A be a generic term for any 2D block cyclicly distributed array.
*  Such a global array has an associated description vector DESCA.
*  In the following comments, the character _ should be read as
*  "of the global array".
*
*  NOTATION        STORED IN      EXPLANATION
*  --------------- -------------- --------------------------------------
*  DTYPE_A(global) DESCA( DTYPE_ )The descriptor type.  In this case,
*                                 DTYPE_A = 1.
*  CTXT_A (global) DESCA( CTXT_ ) The BLACS context handle, indicating
*                                 the BLACS process grid A is distribu-
*                                 ted over. The context itself is glo-
*                                 bal, but the handle (the integer
*                                 value) may vary.
*  M_A    (global) DESCA( M_ )    The number of rows in the global
*                                 array A.
*  N_A    (global) DESCA( N_ )    The number of columns in the global
*                                 array A.
*  MB_A   (global) DESCA( MB_ )   The blocking factor used to distribute
*                                 the rows of the array.
*  NB_A   (global) DESCA( NB_ )   The blocking factor used to distribute
*                                 the columns of the array.
*  RSRC_A (global) DESCA( RSRC_ ) The process row over which the first
*                                 row of the array A is distributed.
*  CSRC_A (global) DESCA( CSRC_ ) The process column over which the
*                                 first column of the array A is
*                                 distributed.
*  LLD_A  (local)  DESCA( LLD_ )  The leading dimension of the local
*                                 array.  LLD_A >= MAX(1,LOCr(M_A)).
*
*  Let K be the number of rows or columns of a distributed matrix,
*  and assume that its process grid has dimension p x q.
*  LOCr( K ) denotes the number of elements of K that a process
*  would receive if K were distributed over the p processes of its
*  process column.
*  Similarly, LOCc( K ) denotes the number of elements of K that a
*  process would receive if K were distributed over the q processes of
*  its process row.
*  The values of LOCr() and LOCc() may be determined via a call to the
*  ScaLAPACK tool function, NUMROC:
*          LOCr( M ) = NUMROC( M, MB_A, MYROW, RSRC_A, NPROW ),
*          LOCc( N ) = NUMROC( N, NB_A, MYCOL, CSRC_A, NPCOL ).
*  An upper bound for these quantities may be computed by:
*          LOCr( M ) <= ceil( ceil(M/MB_A)/NPROW )*MB_A
*          LOCc( N ) <= ceil( ceil(N/NB_A)/NPCOL )*NB_A
*
*  Arguments
*  =========
*
*  M       (global input) INTEGER
*          The number of rows to be operated on, i.e. the number of rows
*          of the distributed submatrix sub( A ). M >= 0.
*
*  N       (global input) INTEGER
*          The number of columns to be operated on, i.e. the number of
*          columns of the distributed submatrix sub( A ). N >= 0.
*
*  A       (local input/local output) COMPLEX*16 pointer into the
*          local memory to an array of dimension (LLD_A, LOCc(JA+N-1)).
*          On entry, the local pieces of the M-by-N distributed matrix
*          sub( A ) which is to be permuted. On exit, the local pieces
*          of the distributed permuted submatrix sub( A ) * Inv( P ).
*
*  IA      (global input) INTEGER
*          The row index in the global array A indicating the first
*          row of sub( A ).
*
*  JA      (global input) INTEGER
*          The column index in the global array A indicating the
*          first column of sub( A ).
*
*  DESCA   (global and local input) INTEGER array of dimension DLEN_.
*          The array descriptor for the distributed matrix A.
*
*  IPIV    (local input) INTEGER array, dimension LOCc(JA+N-1).
*          On exit, if IPIV(I) = K, the local i-th column of sub( A )*P
*          was the global K-th column of sub( A ). IPIV is tied to the
*          distributed matrix A.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_,
     $                   LLD_, MB_, M_, NB_, N_, RSRC_
      PARAMETER          ( BLOCK_CYCLIC_2D = 1, DLEN_ = 9, DTYPE_ = 1,
     $                     CTXT_ = 2, M_ = 3, N_ = 4, MB_ = 5, NB_ = 6,
     $                     RSRC_ = 7, CSRC_ = 8, LLD_ = 9 )
*     ..
*     .. Local Scalars ..
      INTEGER            IACOL, ICOFFA, ICTXT, IITMP, IPVT, IPCOL,
     $                   IPROW, ITMP, J, JJ, JJA, KK, MYCOL, MYROW,
     $                   NPCOL, NPROW, NQ
*     ..
*     .. External Subroutines ..
      EXTERNAL           BLACS_GRIDINFO, IGEBR2D, IGEBS2D, IGERV2D,
     $                   IGESD2D, IGAMN2D, INFOG1L, PZSWAP
*     ..
*     .. External Functions ..
      INTEGER            INDXL2G, NUMROC
      EXTERNAL           INDXL2G, NUMROC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MOD
*     ..
*     .. Executable Statements ..
*
*     Get grid parameters
*
      ICTXT = DESCA( CTXT_ )
      CALL BLACS_GRIDINFO( ICTXT, NPROW, NPCOL, MYROW, MYCOL )
      CALL INFOG1L( JA, DESCA( NB_ ), NPCOL, MYCOL, DESCA( CSRC_ ), JJA,
     $              IACOL )
      ICOFFA = MOD( JA-1, DESCA( NB_ ) )
      NQ = NUMROC( N+ICOFFA, DESCA( NB_ ), MYCOL, IACOL, NPCOL )
      IF( MYCOL.EQ.IACOL )
     $   NQ = NQ - ICOFFA
*
      DO 20 J = JA, JA+N-2
*
         IPVT = JA+N-1
         ITMP = JA+N
*
*        Find first the local minimum candidate for pivoting
*
         CALL INFOG1L( J, DESCA( NB_ ), NPCOL, MYCOL, DESCA( CSRC_ ),
     $                 JJ, IACOL )
         DO 10 KK = JJ, JJA+NQ-1
            IF( IPIV( KK ).LT.IPVT )THEN
               IITMP = KK
               IPVT = IPIV( KK )
            END IF
   10    CONTINUE
*
*        Find the global minimum pivot
*
         CALL IGAMN2D( ICTXT, 'Rowwise', ' ', 1, 1, IPVT, 1, IPROW,
     $                 IPCOL, 1, -1, MYCOL )
*
*        Broadcast the corresponding index to the other process columns
*
         IF( MYCOL.EQ.IPCOL ) THEN
            ITMP = INDXL2G( IITMP, DESCA( NB_ ), MYCOL, DESCA( CSRC_ ),
     $                      NPCOL )
            CALL IGEBS2D( ICTXT, 'Rowwise', ' ', 1, 1, ITMP, 1 )
            IF( IPCOL.NE.IACOL ) THEN
               CALL IGERV2D( ICTXT, 1, 1, IPIV( IITMP ), 1, MYROW,
     $                       IACOL )
            ELSE
               IF( MYCOL.EQ.IACOL )
     $            IPIV( IITMP ) = IPIV( JJ )
            END IF
         ELSE
            CALL IGEBR2D( ICTXT, 'Rowwise', ' ', 1, 1, ITMP, 1, MYROW,
     $                    IPCOL )
            IF( MYCOL.EQ.IACOL .AND. IPCOL.NE.IACOL )
     $         CALL IGESD2D( ICTXT, 1, 1, IPIV( JJ ), 1, MYROW, IPCOL )
         END IF
*
*        Swap the columns of A
*
         CALL PZSWAP( M, A, IA, ITMP, DESCA, 1, A, IA, J, DESCA, 1 )
*
   20 CONTINUE
*
*     End of PZQPPIV
*
      END
