      MODULE diaga4_I
      INTERFACE
!
      SUBROUTINE DIAGA4(JA1,JA2,K1,K2,KA,IRE,IAT,RECC)
      USE vast_kind_param,  ONLY:  DOUBLE
      INTEGER, INTENT(IN)       :: JA1,JA2,K1,K2,KA,IRE
!      INTEGER, INTENT(OUT)      :: IAT
      INTEGER, INTENT(INOUT)      :: IAT
      REAL(DOUBLE), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE
