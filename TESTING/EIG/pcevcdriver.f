      PROGRAM PCEVCDRIVER
*
*  -- ScaLAPACK testing driver (version 1.7) --
*     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
*     and University of California, Berkeley.
*     June, 2000
*     Modifications Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
*
*  Purpose
*  =======
*
*  PCEVCDRIVER is the main test program for the COMPLEX
*  SCALAPACK PCTREVC routine.  This test driver performs a right and
*  left eigenvector calculation of a triangular matrix followed by
*  a residual checks of the calcuated eigenvectors.
*
*  The program must be driven by a short data file and uses the same
*  input file as the PCNEPDRIVER.  An annotated example of a data file
*  can be obtained by deleting the first 3 characters from the following
*  18 lines:
*  'SCALAPACK, Version 1.8, NEP (Nonsymmetric EigenProblem) input file'
*  'Intel iPSC/860 hypercube, gamma model.'
*  'NEP.out'            output file name (if any)
*  6                    device out
*  8                    number of problems sizes
*  1 2 3 4 6 10 100 200 vales of N
*  3                    number of NB's
*  6 20 40              values of NB
*  4                    number of process grids (ordered pairs of P & Q)
*  1 2 1 4              values of P
*  1 2 4 1              values of Q
*  20.0                 threshold
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
*  CPLXSZ   INTEGER, default = 8 bytes.
*           CPLXSZ indicate the length in bytes on the given platform
*           for a single precision complex.
*  MEM      COMPLEX array, dimension ( TOTMEM / CPLXSZ )
*
*           All arrays used by SCALAPACK routines are allocated from
*           this array and referenced by pointers.  The integer IPA,
*           for example, is a pointer to the starting element of MEM for
*           the matrix A.
*
*  Further Details
*  ==============
*
*  Contributed by Mark Fahey, June, 2000
*
*  =====================================================================
*
      use,intrinsic :: ieee_arithmetic
*     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DT_,
     $                   LLD_, MB_, M_, NB_, N_, RSRC_
      PARAMETER          ( BLOCK_CYCLIC_2D = 1, DLEN_ = 9, DT_ = 1,
     $                     CTXT_ = 2, M_ = 3, N_ = 4, MB_ = 5, NB_ = 6,
     $                     RSRC_ = 7, CSRC_ = 8, LLD_ = 9 )
      INTEGER            CPLXSZ, TOTMEM, MEMSIZ, NTESTS
#ifndef DYNAMIC_WORK_MEM_ALLOC
      PARAMETER (TOTMEM = 200000000)
#else
      PARAMETER (TOTMEM = 2100000000)
#endif
      PARAMETER          ( CPLXSZ = 8,
     $                   MEMSIZ = TOTMEM / CPLXSZ, NTESTS = 20 )
      COMPLEX         PADVAL, ZERO, ONE
      PARAMETER          ( PADVAL = ( -9923.0E+0, -9923.0E+0 ),
     $                   ZERO = ( 0.0E+0, 0.0E+0 ),
     $                   ONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            CHECK
      CHARACTER*6        PASSED
      CHARACTER*80       OUTFILE
      INTEGER            I, IAM, IASEED, ICOL, ICTXT, II, III, IMIDPAD,
     $                   INFO, IPA, IPOSTPAD, IPREPAD, IPVL, IPVR, IPW,
     $                   IPWR, IPC, IROW, J, JJ, JJJ, K, KFAIL, KPASS,
     $                   KSKIP, KTESTS, LDA, LDZ, LWORK, M, MYCOL,
     $                   MYROW, N, NB, NGRIDS, NMAT, NNB, NOUT, NP,
     $                   NPCOL, NPROCS, NPROW, NQ, WORKSIZ
      REAL               THRESH
      REAL               ANORM, FRESID, QRESID
      DOUBLE PRECISION   NOPS, TMFLOPS
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      INTEGER            DESCA( DLEN_ ), DESCZ( DLEN_ ), IERR( 2 ),
     $                   NBVAL( NTESTS ), NVAL( NTESTS ),
     $                   PVAL( NTESTS ), QVAL( NTESTS )
      REAL               RESULT( 2 ), RWORK( 5000 )
      DOUBLE PRECISION   CTIME( 2 ), WTIME( 2 )
#ifndef DYNAMIC_WORK_MEM_ALLOC
      COMPLEX            MEM( MEMSIZ )
#else
      COMPLEX, allocatable :: MEM (:)
#endif
*     ..
*     .. External Subroutines ..
      EXTERNAL           BLACS_BARRIER, BLACS_EXIT, BLACS_GET,
     $                   BLACS_GRIDEXIT, BLACS_GRIDINFO, BLACS_GRIDINIT,
     $                   BLACS_PINFO, DESCINIT, IGSUM2D, INFOG2L,
     $                   PCCHEKPAD, PCEVCINFO, PCFILLPAD, PCGET22,
     $                   PCLASET, PCMATGEN, PCTREVC, SLBOOT, SLCOMBINE,
     $                   SLTIMER, CGSUM2D
*     ..
*     .. External Functions ..
      INTEGER            ILCM, NUMROC
      REAL               PCLANHS
      EXTERNAL           ILCM, NUMROC, PCLANHS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          REAL, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               KFAIL, KPASS, KSKIP, KTESTS / 4*0 /
*     Take command-line arguments if requested
      CHARACTER*80 arg
      INTEGER numArgs, count
      LOGICAL :: help_flag = .FALSE.
      LOGICAL :: EX_FLAG = .FALSE., RES_FLAG = .FALSE.
      INTEGER :: INF_PERCENT = 0
      INTEGER :: NAN_PERCENT = 0
      DOUBLE PRECISION :: X
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
      CALL PCEVCINFO( OUTFILE, NOUT, NMAT, NVAL, NTESTS, NNB, NBVAL,
     $                NTESTS, NGRIDS, PVAL, NTESTS, QVAL, NTESTS,
     $                THRESH, MEM, IAM, NPROCS )
      CHECK = ( THRESH.GE.0.0E+0 )
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
*     Print headings
*
      IF( IAM.EQ.0 ) THEN
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9995 )
         WRITE( NOUT, FMT = 9994 )
         WRITE( NOUT, FMT = * )
      END IF
*
*     Loop over different process grids
*
      DO 40 I = 1, NGRIDS
*
         NPROW = PVAL( I )
         NPCOL = QVAL( I )
*
*        Make sure grid information is correct
*
#ifdef ENABLE_DRIVER_CHECK
         IERR( 1 ) = 0
         IF( NPROW.LT.1 ) THEN
            IF( IAM.EQ.0 )
     $         WRITE( NOUT, FMT = 9999 )'GRID', 'nprow', NPROW
            IERR( 1 ) = 1
         ELSE IF( NPCOL.LT.1 ) THEN
            IF( IAM.EQ.0 )
     $         WRITE( NOUT, FMT = 9999 )'GRID', 'npcol', NPCOL
            IERR( 1 ) = 1
         ELSE IF( NPROW*NPCOL.GT.NPROCS ) THEN
            IF( IAM.EQ.0 )
     $         WRITE( NOUT, FMT = 9998 )NPROW*NPCOL, NPROCS
            IERR( 1 ) = 1
         END IF
*
         IF( IERR( 1 ).GT.0 ) THEN
            IF( IAM.EQ.0 )
     $         WRITE( NOUT, FMT = 9997 )'grid'
            KSKIP = KSKIP + 1
            GO TO 40
         END IF
#endif
*
*        Define process grid
*
         CALL BLACS_GET( -1, 0, ICTXT )
         CALL BLACS_GRIDINIT( ICTXT, 'Row-major', NPROW, NPCOL )
         CALL BLACS_GRIDINFO( ICTXT, NPROW, NPCOL, MYROW, MYCOL )
*
*        Go to bottom of process grid loop if this case doesn't use my
*        process
*
         IF( MYROW.GE.NPROW .OR. MYCOL.GE.NPCOL )
     $      GO TO 40
*
         DO 30 J = 1, NMAT
*
            N = NVAL( J )
*
*           Make sure matrix information is correct
*
            IERR( 1 ) = 0
#ifdef ENABLE_DRIVER_CHECK
            IF( N.LT.1 ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9999 )'MATRIX', 'N', N
               IERR( 1 ) = 1
            END IF
#endif
*
*           Check all processes for an error
*
#ifdef ENABLE_DRIVER_CHECK
            CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1, 0 )
*
            IF( IERR( 1 ).GT.0 ) THEN
               IF( IAM.EQ.0 )
     $            WRITE( NOUT, FMT = 9997 )'matrix'
               KSKIP = KSKIP + 1
               GO TO 30
            END IF
#endif
*
            DO 20 K = 1, NNB
*
               NB = NBVAL( K )
*
*              Make sure nb is legal
*
               IERR( 1 ) = 0
               IF( NB.LT.6 ) THEN
                  IERR( 1 ) = 1
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9999 )'NB', 'NB', NB
               END IF
*
*              Check all processes for an error
*
               CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1, 0 )
*
#ifdef ENABLE_DRIVER_CHECK
               IF( IERR( 1 ).GT.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9997 )'NB'
                  KSKIP = KSKIP + 1
                  GO TO 20
               END IF
#endif
*
*              Padding constants
*
               NP = NUMROC( N, NB, MYROW, 0, NPROW )
               NQ = NUMROC( N, NB, MYCOL, 0, NPCOL )
               IF( CHECK ) THEN
                  IPREPAD = MAX( NB, NP )
                  IMIDPAD = NB
                  IPOSTPAD = MAX( NB, NQ )
                  IPREPAD = IPREPAD + 1000
                  IMIDPAD = IMIDPAD + 1000
                  IPOSTPAD = IPOSTPAD + 1000
               ELSE
                  IPREPAD = 0
                  IMIDPAD = 0
                  IPOSTPAD = 0
               END IF
*
*              Initialize the array descriptor for the matrix A
*
               CALL DESCINIT( DESCA, N, N, NB, NB, 0, 0, ICTXT,
     $                        MAX( 1, NP )+IMIDPAD, IERR( 1 ) )
*
*              Initialize the array descriptor for the matrix Z
*
               CALL DESCINIT( DESCZ, N, N, NB, NB, 0, 0, ICTXT,
     $                        MAX( 1, NP )+IMIDPAD, IERR( 2 ) )
*
               LDA = DESCA( LLD_ )
               LDZ = DESCZ( LLD_ )
*
*              Check all processes for an error
*
               CALL IGSUM2D( ICTXT, 'All', ' ', 2, 1, IERR, 2, -1, 0 )
*
#ifdef ENABLE_DRIVER_CHECK
               IF( IERR( 1 ).LT.0 .OR. IERR( 2 ).LT.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9997 )'descriptor'
                  KSKIP = KSKIP + 1
                  GO TO 20
               END IF
#else
*              If N < 0 in EVC.dat file then DESCINIT API sets IERR( 1 ) = -3
               IF( N.LT.0 .AND. (IERR( 1 ).EQ.-2 .OR.
     $                 IERR( 1 ).EQ. -8 .OR.
     $                 IERR( 1 ).EQ. -4 .OR.
     $                 IERR( 2 ).EQ.-2 .OR.
     $                 IERR( 2 ).EQ. -8 .OR.
     $                 IERR( 2 ).EQ. -4) ) THEN
*              If DESCINIT is returning correct error code we need to pass
*              and it will be ScaLAPACK API
                  WRITE( NOUT, FMT = 9983 ) 'N'
*                   disable extreme value case when N < 0
                  EX_FLAG = .FALSE.
               ELSE IF(N .EQ. 0 .AND. (IERR(1) .EQ. 0 .OR.
     $              IERR(1) .EQ. -5 .OR. IERR(1) .EQ. -10 .OR.
     $              IERR(1) .EQ. -15 .OR. IERR(1) .EQ. -20 )) THEN
*                   DESCINIT returns the correct error code,
*                   When N = 0,
*                   -5, -10 or -20 incase of incorrect grid info
*                   MAIN API can be validated.
*                   Do NOTHING
*                   disable extreme value case when N = 0
                    EX_FLAG = .FALSE.
                  WRITE ( NOUT, FMT = 9984 ) 'PCTREVC'
               ELSE IF( IERR( 1 ).LT.0 .OR. IERR( 2 ).LT.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9997 ) 'descriptor'
                  KSKIP = KSKIP + 1
                  GO TO 20
               END IF
#endif
*
*              Assign pointers into MEM for SCALAPACK arrays, A is
*              allocated starting at position MEM( IPREPAE+1 )
*
               IPA = IPREPAD + 1
               IPC = IPA + DESCA( LLD_ )*NQ + IPOSTPAD + IPREPAD
               IPWR = IPC + DESCZ( LLD_ )*NQ + IPOSTPAD + IPREPAD
               IPVL = IPWR + N + IPOSTPAD + IPREPAD
               IPVR = IPVL + DESCZ( LLD_ )*NQ + IPOSTPAD + IPREPAD
               IPW = IPVR + DESCZ( LLD_ )*NQ + IPOSTPAD + IPREPAD
               III = N / NB
               IF( III*NB.LT.N )
     $            III = III + 1
               III = 7*III / ILCM( NPROW, NPCOL )
*
*
               LWORK = 3*N + MAX( 2*MAX( LDA, LDZ )+2*NQ, III )
               LWORK = LWORK + MAX( 2*N, ( 8*ILCM( NPROW, NPCOL )+2 )**
     $                 2 )
*
               IF( CHECK ) THEN
*
*                 Figure the amount of workspace required by the
*                 checking routines PCEVCFCHK and PCLANHS
*
                  WORKSIZ = LWORK + MAX( NP*DESCA( NB_ ),
     $                      DESCA( MB_ )*NQ ) + IPOSTPAD
*
               ELSE
*
                  WORKSIZ = LWORK + IPOSTPAD
*
               END IF
*
*              Check for adequate memory for problem size
*
               IERR( 1 ) = 0
               IF( IPW+WORKSIZ.GT.MEMSIZ ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9996 )'Schur reduction',
     $               ( IPW+WORKSIZ )*CPLXSZ
                  IERR( 1 ) = 1
               END IF
*
*              Check all processes for an error
*
               CALL IGSUM2D( ICTXT, 'All', ' ', 1, 1, IERR, 1, -1, 0 )
*
               IF( IERR( 1 ).GT.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = 9997 )'MEMORY'
                  KSKIP = KSKIP + 1
                  GO TO 20
               END IF
*
*              Generate matrix Z = In
*
               CALL PCLASET( 'All', N, N, ZERO, ONE, MEM( IPC ), 1, 1,
     $                       DESCZ )
               CALL PCLASET( 'All', N, N, ZERO, ZERO, MEM( IPVR ), 1, 1,
     $                       DESCZ )
               CALL PCLASET( 'All', N, N, ZERO, ZERO, MEM( IPVL ), 1, 1,
     $                       DESCZ )
*
*              Generate matrix A upper Hessenberg
*
               CALL PCMATGEN( ICTXT, 'No transpose', 'No transpose',
     $                        DESCA( M_ ), DESCA( N_ ), DESCA( MB_ ),
     $                        DESCA( NB_ ), MEM( IPA ), DESCA( LLD_ ),
     $                        DESCA( RSRC_ ), DESCA( CSRC_ ), IASEED, 0,
     $                        NP, 0, NQ, MYROW, MYCOL, NPROW, NPCOL )
               CALL PCLASET( 'Lower', MAX( 0, N-1 ), MAX( 0, N-1 ),
     $                       ZERO, ZERO, MEM( IPA ), MIN( N, 2 ), 1,
     $                       DESCA )
*
*              Calculate inf-norm of A for residual error-checking
*
               IF( CHECK .AND. N.GE.0 ) THEN
                  CALL PCFILLPAD( ICTXT, NP, NQ, MEM( IPA-IPREPAD ),
     $                            DESCA( LLD_ ), IPREPAD, IPOSTPAD,
     $                            PADVAL )
                  CALL PCFILLPAD( ICTXT, NP, NQ, MEM( IPVR-IPREPAD ),
     $                            DESCZ( LLD_ ), IPREPAD, IPOSTPAD,
     $                            PADVAL )
                  CALL PCFILLPAD( ICTXT, NP, NQ, MEM( IPVL-IPREPAD ),
     $                            DESCZ( LLD_ ), IPREPAD, IPOSTPAD,
     $                            PADVAL )
                  CALL PCFILLPAD( ICTXT, NP, NQ, MEM( IPC-IPREPAD ),
     $                            DESCZ( LLD_ ), IPREPAD, IPOSTPAD,
     $                            PADVAL )
                  CALL PCFILLPAD( ICTXT, WORKSIZ-IPOSTPAD, 1,
     $                            MEM( IPW-IPREPAD ), WORKSIZ-IPOSTPAD,
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  ANORM = PCLANHS( 'I', N, MEM( IPA ), 1, 1, DESCA,
     $                    MEM( IPW ) )
                  CALL PCCHEKPAD( ICTXT, 'PCLANHS', NP, NQ,
     $                            MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCLANHS', WORKSIZ-IPOSTPAD, 1,
     $                            MEM( IPW-IPREPAD ), WORKSIZ-IPOSTPAD,
     $                            IPREPAD, IPOSTPAD, PADVAL )
*
                  CALL PCFILLPAD( ICTXT, N, 1, MEM( IPWR-IPREPAD ), N,
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCFILLPAD( ICTXT, LWORK, 1, MEM( IPW-IPREPAD ),
     $                            LWORK, IPREPAD, IPOSTPAD, PADVAL )
*
               END IF
*
*              Set eigenvalues from diagonal
*
               DO 10 JJJ = 1, N
                  CALL INFOG2L( JJJ, JJJ, DESCZ, NPROW, NPCOL, MYROW,
     $                          MYCOL, IROW, ICOL, II, JJ )
                  IF( MYROW.EQ.II .AND. MYCOL.EQ.JJ ) THEN
                     MEM( IPWR-1+JJJ ) = MEM( IPA-1+( ICOL-1 )*LDA+
     $                                   IROW )
                  ELSE
                     MEM( IPWR-1+JJJ ) = ZERO
                  END IF
   10          CONTINUE
*
*              In case of N<0, surpassing the call off ZGSUM2D
*
               IF(N.GT.0) THEN
               CALL CGSUM2D( ICTXT, 'All', ' ', N, 1, MEM( IPWR ), N,
     $                       -1, -1 )
               END IF
*
               SELECT( 1 ) = .TRUE.
               CALL SLBOOT()
               CALL BLACS_BARRIER( ICTXT, 'All' )
               CALL SLTIMER( 1 )
*
*              Perform eigenvector calculation
*
               CALL PCTREVC( 'B', 'A', SELECT, N, MEM( IPA ), DESCA,
     $                       MEM( IPVL ), DESCZ, MEM( IPVR ), DESCZ, N,
     $                       M, MEM( IPW ), RWORK, INFO )
*
               CALL SLTIMER( 1 )
*
               IF( INFO.NE.0 ) THEN
                  IF( IAM.EQ.0 )
     $               WRITE( NOUT, FMT = * )'PCTREVC INFO=', INFO
*                 If N < 0 in EVC.dat file then PCTREVC API sets
*                 INFO = -4
                  IF (N.LT.0 .AND. INFO.EQ.-4) THEN
*                    If PCTREVC is returning correct error
*                    code we need to pass this case
                     WRITE( NOUT, FMT = 9983 ) 'PCTREVC'
                  ELSE IF ( N.GT.1 .AND. INFO.NE.0 ) THEN
                     KFAIL = KFAIL + 1
                     GO TO 20
                  END IF
               ELSE IF (N.EQ.0) THEN
*                 If N =0 this is the case of
*                 early return from ScaLAPACK API.
                  WRITE( NOUT, FMT = 9982 ) 'PCTREVC'
               END IF
*
               IF( CHECK .AND. INFO.EQ.0  .AND. .NOT.(EX_FLAG) ) THEN
*
*                 Check for memory overwrite in NEP factorization
*
                  CALL PCCHEKPAD( ICTXT, 'PCTREVC (A)', NP, NQ,
     $                            MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCTREVC (VR)', NP, NQ,
     $                            MEM( IPVR-IPREPAD ), DESCZ( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCTREVC (VL)', NP, NQ,
     $                            MEM( IPVL-IPREPAD ), DESCZ( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCTREVC (WR)', N, 1,
     $                            MEM( IPWR-IPREPAD ), N, IPREPAD,
     $                            IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCTREVC (WORK)', LWORK, 1,
     $                            MEM( IPW-IPREPAD ), LWORK, IPREPAD,
     $                            IPOSTPAD, PADVAL )
*
                  CALL PCFILLPAD( ICTXT, WORKSIZ-IPOSTPAD, 1,
     $                            MEM( IPW-IPREPAD ), WORKSIZ-IPOSTPAD,
     $                            IPREPAD, IPOSTPAD, PADVAL )
*
*                 Compute || T * Z - Z * D || / ( N*|| T ||*EPS )
*
                  FRESID = 0.0E+0
                  QRESID = 0.0E+0
                  CALL PCGET22( 'N', 'N', 'N', N, MEM( IPA ), DESCA,
     $                          MEM( IPVR ), DESCZ, MEM( IPWR ),
     $                          MEM( IPC ), DESCZ, RWORK, RESULT )
                  FRESID = RESULT( 1 )
                  QRESID = RESULT( 2 )
*
*                 Compute || T^H * L - L * D^H || / ( N*|| T ||*EPS )
*
                  CALL PCGET22( 'C', 'N', 'C', N, MEM( IPA ), DESCA,
     $                          MEM( IPVL ), DESCZ, MEM( IPWR ),
     $                          MEM( IPC ), DESCZ, RWORK, RESULT )
                  FRESID = MAX( FRESID, RESULT( 1 ) )
                  QRESID = MAX( QRESID, RESULT( 2 ) )
*
                  CALL PCCHEKPAD( ICTXT, 'PCGET22 (A)', NP, NQ,
     $                            MEM( IPA-IPREPAD ), DESCA( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCGET22 (VR)', NP, NQ,
     $                            MEM( IPVR-IPREPAD ), DESCZ( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCGET22 (VL)', NP, NQ,
     $                            MEM( IPVL-IPREPAD ), DESCZ( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
                  CALL PCCHEKPAD( ICTXT, 'PCGET22 (Z)', NP, NQ,
     $                            MEM( IPC-IPREPAD ), DESCZ( LLD_ ),
     $                            IPREPAD, IPOSTPAD, PADVAL )
*
*                 Test residual and detect NaN result
*
*                  IF( ( FRESID.LE.THRESH ) .AND.
                  IF( N .EQ. 0 .AND. (INFO .EQ. -4 .OR.
     $                       INFO .EQ. 0)) THEN
*                       If N =0 this is the case of
*                       early return from ScaLAPACK API.
*                       If there is safe exit from API; pass this case
                           KPASS = KPASS + 1
                           WRITE( NOUT, FMT = 9984 ) 'PCTREVC'
                           PASSED = 'PASSED'
*                          Re-enable EX_FLAG
                           IF(NAN_PERCENT .GT. 0 .OR.
     $                                    INF_PERCENT .GT. 0) THEN
                              EX_FLAG = .TRUE.
                           END IF
                  ELSE IF(N .LT. 0 .AND. (INFO .EQ. -2 .OR.
     $                         INFO .EQ. -804 )) THEN
*                       When N < 0/Invalid, PCTREVC INFO = -1
*                       Expected Error code for N < 0
*                       Hence this case can be passed
                           KPASS = KPASS + 1
                           WRITE( NOUT, FMT = 9982 ) 'PCTREVC'
                           PASSED = 'PASSED'
*                          Re-enable EX_FLAG
                           IF(NAN_PERCENT .GT. 0 .OR.
     $                                    INF_PERCENT .GT. 0) THEN
                              EX_FLAG = .TRUE.
                           END IF
                  ELSE IF( ( FRESID.LE.THRESH ).AND.
     $                ( ( FRESID-FRESID ).EQ.0.0E+0 ) .AND.
     $                ( QRESID.LE.THRESH ) .AND.
     $                ( ( QRESID-QRESID ).EQ.0.0E+0 ) ) THEN
                     KPASS = KPASS + 1
                     PASSED = 'PASSED'
                  ELSE IF( N.EQ.0 ) THEN
*                 Passing residual checks for the case N = 0
                        KPASS = KPASS + 1
                        PASSED = 'PASSED'
                  ELSE
                     KFAIL = KFAIL + 1
                     PASSED = 'FAILED'
                     IF( IAM.EQ.0 ) THEN
                        WRITE( NOUT, FMT = 9986 )FRESID
                        WRITE( NOUT, FMT = 9985 )QRESID
                     END IF
                  END IF
*                    Extreme-value validation block
               ELSE IF(EX_FLAG) THEN
*                       Check presence of INF/NAN in output
*                       Pass the case if present
                        DO IK = 0, N-1
                           DO JK = 1, N
                              X = (MEM(IK*N + JK))
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
*
               ELSE
*
*                 Don't perform the checking, only timing
*
                  KPASS = KPASS + 1
                  FRESID = FRESID - FRESID
                  QRESID = QRESID - QRESID
                  IF (N.LT.0 .AND. INFO.EQ.-4) THEN
                     PASSED = 'PASSED'
                  ELSE
                     PASSED = 'BYPASS'
                  END IF
*
               END IF
*
*              Gather maximum of all CPU and WALL clock timings
*
               CALL SLCOMBINE( ICTXT, 'All', '>', 'W', 1, 1, WTIME )
               CALL SLCOMBINE( ICTXT, 'All', '>', 'C', 1, 1, CTIME )
*
*              Print results
*
               IF( MYROW.EQ.0 .AND. MYCOL.EQ.0 ) THEN
*
*                 2 N^2 flops for PxTREVC
*
                  NOPS = 2.0D+0*DBLE( N )**2
*
*                 Calculate total megaflops -- eigenvector calc only,
*                 -- for WALL and CPU time, and print output
*
*                 Print WALL time if machine supports it
*
                  IF( WTIME( 1 ).GT.0.0D+0 ) THEN
                     TMFLOPS = NOPS / ( WTIME( 1 )*1.0D+6 )
                  ELSE
                     TMFLOPS = 0.0D+0
                  END IF
                  IF( WTIME( 1 ).GE.0.0D+0 )
     $               WRITE( NOUT, FMT = 9993 )'WALL', N, NB, NPROW,
     $               NPCOL, WTIME( 1 ), TMFLOPS, PASSED
*
*                 Print CPU time if machine supports it
*
                  IF( CTIME( 1 ).GT.0.0D+0 ) THEN
                     TMFLOPS = NOPS / ( CTIME( 1 )*1.0D+6 )
                  ELSE
                     TMFLOPS = 0.0D+0
                  END IF
*
                  IF( CTIME( 1 ).GE.0.0D+0 )
     $               WRITE( NOUT, FMT = 9993 )'CPU ', N, NB, NPROW,
     $               NPCOL, CTIME( 1 ), TMFLOPS, PASSED
               END IF
*
   20       CONTINUE
*
   30    CONTINUE
*
         CALL BLACS_GRIDEXIT( ICTXT )
*
   40 CONTINUE
*
*     Print ending messages and close output file
*
      IF( IAM.EQ.0 ) THEN
         KTESTS = KPASS + KFAIL + KSKIP
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9992 )KTESTS
         IF( CHECK ) THEN
            WRITE( NOUT, FMT = 9991 )KPASS
            WRITE( NOUT, FMT = 9989 )KFAIL
         ELSE
            WRITE( NOUT, FMT = 9990 )KPASS
         END IF
         WRITE( NOUT, FMT = 9988 )KSKIP
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
     $      '; It should be at least 1' )
 9998 FORMAT( 'ILLEGAL GRID: nprow*npcol = ', I4, '. It can be at most',
     $      I4 )
 9997 FORMAT( 'Bad ', A6, ' parameters: going on to next test case.' )
 9996 FORMAT( 'Unable to perform ', A, ': need TOTMEM of at least',
     $      I11 )
 9995 FORMAT( 'TIME     N  NB    P    Q NEP Time   MFLOPS  CHECK' )
 9994 FORMAT( '---- ----- --- ---- ---- -------- -------- ------' )
 9993 FORMAT( A4, 1X, I5, 1X, I5, 1X, I4, 1X, I4, 1X, F8.2, 1X, F12.2,
     $      1X, A6 )
 9992 FORMAT( 'Finished ', I6, ' tests, with the following results:' )
 9991 FORMAT( I5, ' tests completed and passed residual checks.' )
 9990 FORMAT( I5, ' tests completed without checking.' )
 9989 FORMAT( I5, ' tests completed and failed residual checks.' )
 9988 FORMAT( I5, ' tests skipped because of illegal input values.' )
 9987 FORMAT( 'END OF TESTS.' )
 9986 FORMAT( '||H*Z - Z*D|| / (||T|| * N * eps) = ', G25.7 )
 9985 FORMAT( 'max_j(max|Z(j)| - 1) / ( N * eps ) ', G25.7 )
 9984 FORMAT(  A, ' < 0 case detected. ',
     $        'Instead of driver file, we will handle this case from ',
     $        'ScaLAPACK API.')
 9983 FORMAT(  A, ' returned correct error code. Passing this case.')
 9982 FORMAT(  'This is safe exit from ', A, ' API. Passing this case.')
*
      STOP
*
*     End of PCEVCDRIVER
*
      END
