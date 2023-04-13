
!==============================================================================
!     last edited November 1, 1996
      subroutine matain(org, lock, closed, varmax, skal, nmax, anel, par, low, &
         minj, maxj, lim, dubbel)
!==============================================================================
!     Editted by Charlotte Froese Fischer,    October, 2017
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: varmax
      integer , intent(out) :: skal
      integer  :: nmax
      integer , intent(out) :: anel
      integer , intent(out) :: par
      integer  :: minj
      integer  :: maxj
      integer  :: org(15,0:10)
      integer , intent(inout) :: low(15,0:10)
      integer  :: lim(15)
      logical , intent(inout) :: lock(15,0:10)
      logical , intent(inout) :: closed(15,0:10)
      logical , intent(out) :: dubbel(15,0:10)
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: logfil = 31
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: anela, anelb, resl, i, j, lmax, em, nenter, block, mshell, enn&
         , tmp, cormax
      logical :: all, log_all, lima, open_c, clos_c
      character :: x
      character , dimension(0:10) :: orb
      character , dimension(0:20) :: l
      character :: y*3

      data (l(i),i=0,20)/ 'S', 'P', 'D', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 'N'&
         , 'O', 'Q', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
      data (orb(i),i=0,10)/ 's', 'p', 'd', 'f', 'g', 'h', 'i', 'k', 'l', 'm', &
                            'n'/
!-----------------------------------------------

      do 10 i=1,15
         do 10 j=0,min(i-1,10)
   10       org(i,j) = 0
      skal = 20
   ! write(*,200) 'Highest principal quantum number, n? (1..15)'
   60 read(193,*,err=60) nmax
      nmax = max(nmax,1)
      nmax = min(nmax,15)
   70 read(193,1000) X
      lmax = -1
      do 71 i=0,min(10,nmax-1)
   71    if (X.EQ.orb(i)) lmax=i
      if (lmax.EQ.-1) goto 70
      read(193,1000) X
      all   = .NOT.(X.EQ.'n' .OR. X.EQ.'N')
      do 72 i=1,15
   72    lim(i) = 0
      if (nmax.GE.2) then
         read(193,1000) X
         lima = X.EQ.'y' .OR. X.EQ.'Y'
         if (lima) then
            mshell = 0
            do 85 i=1,nmax-1
               mshell = mshell + 2*i*i
   83          continue
               if (i.EQ.1) then
               elseif (i.LT.10) then
                  if (mshell.LT.100) then
                  else
                  endif
               else
               endif
               read(193,*,err=83) lim(i)
               if (lim(i).GT.mshell) lim(i) = mshell
   85       continue
         endif
      endif
   90 continue
      if (nmax.LT.10) then
      else
      endif
      read(193,*,err=90) nenter
      nenter = max(nenter,1)
      nenter = min(nenter,nmax)
      if (nenter.GE.1) then
         read(193,1000) X
         open_c = X.EQ.'O' .OR. X.EQ.'o'
         clos_c = X.EQ.'C' .OR. X.EQ.'c'
         if (open_c .OR. clos_c) then
   92       write(*,200) 'Select core,'
            if (nenter.GE.2) write(*,200)
            if (nenter.GE.3) write(*,200) 
            if (nenter.GE.4) write(*,200) 
            if (nenter.GE.5) write(*,200) 
            if (nenter.GE.6) write(*,200) 
            read(193,*,err=92) cormax
	    if (cormax.GT.nenter) goto 92
            if (cormax.GE.1) then
	       do 93 i=1,cormax
                  do 93 j= 0,min(3,i-1)
		     if (clos_c) closed(i,j) = .TRUE.
   93                org(i,j) = 2+4*j
	       if (cormax.EQ.3) then
		  org(3,2) = 0
                  if (clos_c) closed(3,2) = .FALSE.
               elseif (cormax.EQ.4) then
                  org(4,2) = 0
                  org(4,3) = 0
                  if (clos_c) then
		     closed(4,2) = .FALSE.
                     closed(4,3) = .FALSE.
		  endif
               elseif (cormax.EQ.5) then
                  org(4,3) = 0
                  org(5,2) = 0
                  org(5,3) = 0
                  if (clos_c) then
                     closed(4,3) = .FALSE.
		     closed(5,2) = .FALSE.
                     closed(5,3) = .FALSE.
                  endif
               elseif (cormax.EQ.6) then
                  org(5,3) = 0
                  org(6,2) = 0
                  org(6,3) = 0
                  if (clos_c) then
                     closed(5,3) = .FALSE.
                     closed(6,2) = .FALSE.
                     closed(6,3) = .FALSE.
                  endif
               endif
            else
            endif
         endif
      endif
      anela  = 0
      anelb  = 0
      anel   = 0
      par    = 0
      block  = 0
      do 160 i=1,15
         do 150 j=0,min(10,i-1)
            low(i,j)    = 0
            dubbel(i,j) = .FALSE.
            if (nmax.GE.i .AND. lmax.GE.j .AND. org(i,j).EQ.0) then
               if (nenter.GE.i) then
                  em = 2 + 4*j
                  if (em.LT.10) then
  100                continue
                     if (i.LE.9) then
                     else
                     endif
                     read(193,*,err=100) org(i,j)
                     if (org(i,j).LT.0 .OR. org(i,j).GT.em)            &
                              goto 100
                  else
  101                continue
                     if (i.LT.10) then
                     else
                     endif
                     read(193,*,err=101) org(i,j)
                     if (org(i,j).LT.0 .OR. org(i,j).GT.em)            &
                              goto 101
                  endif
                  anel = anel + org(i,j)
                  par  = mod(par+j*org(i,j),2)
                  if (all) then
                     lock(i,j)   = .FALSE.
                     closed(i,j) = .FALSE.
                  else
                     if (org(i,j).EQ.em) then
                        if (org(i,j).LE.10) then
                     else
                     endif
                        read(193,1000) Y
                        closed(i,j) = Y(1:1).EQ.'c' .OR. Y(1:1).EQ.'C'
                        lock(i,j)   = Y(1:1).EQ.'i' .OR. Y(1:1).EQ.'I' &
                                               .OR. closed(i,j)
                        if (closed(i,j)) block = block + em
                     else
                        if (org(i,j).GT.1) then
                           if (org(i,j).LE.10) then
                           else
                           endif
                        elseif (org(i,j).EQ.1) then
                        else
                        endif
                        read(193,1000) Y
                        if (org(i,j).EQ.0) then
                           dubbel(i,j) = Y(1:1).EQ.'d' .OR.            &
                                         Y(1:1).EQ.'D'
                        endif
                        lock(i,j)   = Y(1:1).EQ.'i' .OR. Y(1:1).EQ.'I'
                        closed(i,j) = .FALSE.
                     endif
                     if (Y(1:1).GE.'0' .AND. Y(1:1).LE.'9') then
                        if (org(i,j).GT.0) then
                           tmp = ICHAR(Y(1:1))-ICHAR('0')
                           if (Y(2:2).GE.'0' .AND. Y(2:2).LE.'9')      &
                              tmp = tmp*10 + ICHAR(Y(2:2))-ICHAR('0')
                           low(i,j) = min(org(i,j),tmp)
                        endif
                     endif
                  endif
                  if (.NOT. lock(i,j)) anela = anela + org(i,j)
               elseif (all) then
                  org(i,j) = 0
                  lock(i,j) = .FALSE.
                  closed(i,j) = .FALSE.
               else
                  org(i,j) = 0
                  closed(i,j) = .FALSE.
                  if (i.LT.10) then
                  else
                  endif
                  read(193,1000) X
                  dubbel(i,j) = X.EQ.'d' .OR. X.EQ.'D'
                  lock(i,j)   = X.EQ.'i' .OR. X.EQ.'I'
               endif
            elseif (org(i,j).NE.0) then
               if (open_c) then
                  if (all) then
                     closed(i,j) = .FALSE.
                     lock(i,j)   = .FALSE.
                  else
                     if (org(i,j).LE.10) then
                     else
                     endif
                     read(193,1000) Y
                     closed(i,j) = Y(1:1).EQ.'c' .OR. Y(1:1).EQ.'C'
                     lock(i,j)   = Y(1:1).EQ.'i' .OR. Y(1:1).EQ.'I'    &
                                               .OR. closed(i,j)
                     if (Y(1:1).GE.'0' .AND. Y(1:1).LE.'9') then
                        if (org(i,j).GT.0) then
                           tmp = ICHAR(Y(1:1))-ICHAR('0')
                           if (Y(2:2).GE.'0' .AND. Y(2:2).LE.'9')      &
                              tmp = tmp*10 + ICHAR(Y(2:2))-ICHAR('0')
                           low(i,j) = min(org(i,j),tmp)
                        endif
                     endif
                  endif
                  if (.NOT. lock(i,j)) anela = anela + org(i,j)
               else
                  lock(i,j) = closed(i,j)
               endif
               if (closed(i,j)) block = block + org(i,j)
               anel = anel + org(i,j)
            else
               org(i,j)  = 0
               lock(i,j) = .TRUE.
               closed(i,j) = .FALSE.
            endif
            anelb = anelb + low(i,j)
  150       continue
            lim(i) = lim(i) - block
            if (lim(i).LT.0) lim(i) = 0
  160 continue
 1100 write(*,400)
                 
      read(193,*,ERR=1100) minJ,maxJ
      if (anel .EQ. 2*(anel/2)) then
         if (minJ .NE. 2*(minJ/2) .OR. maxJ .NE. 2*(maxJ/2)) then
            goto 1100
         endif
      else
         if (minJ .EQ. 2*(minJ/2) .OR. maxJ .EQ. 2*(maxJ/2)) then
            goto 1100
         endif
      endif
      anelb = anela - anelb
 1200 continue
      if (anelb.LT.10) then
         read(193,*,err=1200) varmax
      else
         read(193,*,err=1200) varmax
      endif
  200 format(' ',A,I1,A,A,I1,A)
  201 format(' ',A,I1,A,A,I2,A)
  202 format(' ',A,I2,A,A,I1,A)
  203 format(' ',A,I2,A,A,I2,A)
  204 format(' ',I1,3A)
  205 format(' ',I2,3A)
  206 format(' ',I1,A,A,I2,A)
  207 format(' ',I2,A,A,I2,A)
  208 format(' ',A,I1,A,I2,A)
  300 format(' ',3A)
  400 format(' ',2A,I1,A)
  402 format(' ',2A,I2,A)
 1000 format(3A)
 2000 format(I1,2A)
 3000 format(A,I2,2A)
      return
      end
