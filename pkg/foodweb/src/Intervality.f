! =============================================================================
!
! CALCULATES FOOD WEB INTERVALITY AS FORMULATED BY 
! STOUFFER et al. 2006 (PNAS 103:19015)
!
! Implemented in R: Yangchen Lin  linyangchen@gmail.com
! Implemented in FORTRAN: Karline Soetaert
! 28 July 2011
!
! =============================================================================
      subroutine rprint(msg)
      character (len=*) msg
            call dblepr(msg, 150, 0, 0)
      end subroutine 


! -----------------------------------------------------------------------------
! Estimate sum of diet gaps for a certain flow matrix.
! -----------------------------------------------------------------------------

       SUBROUTINE DietGap (Flow, nrow, ncol, index, work, sumgap)
       IMPLICIT NONE
       INTEGER, INTENT (IN) :: nrow, ncol
       INTEGER, INTENT (IN) :: index(nrow) ! current permutation of rows of Flow
       DOUBLE PRECISION, INTENT (IN) :: Flow(nrow, ncol)
       DOUBLE PRECISION :: work(nrow)
       INTEGER, INTENT (OUT) :: sumgap
       INTEGER :: I, J, Istart, Istop 
       CHARACTER (LEN = 150) :: MSG
       
       SUMGAP  = 0
       
       DO J = 1, ncol
         DO I = 1, nrow
            work(i) = Flow(index(I), J)
         ENDDO                

         ! first nonzero element of flow
         istart = 0
         DO I = 1, nrow
           IF (work(I) .GT. 0) THEN
                istart = I
                EXIT
           ENDIF
         ENDDO
         
         ! last nonzero element of flow
         istop = 0
         DO I = nrow, 1, -1
           if (work(I) .GT. 0) THEN
                istop = I
                EXIT
           ENDIF
         ENDDO
         ! number of intermediate nonzero elements
         IF (istart .GT. 0 .AND. istop .GT. 0) THEN
           DO I = istart, istop
             IF (work(I) .EQ. 0.d0) THEN
               SUMGAP = SUMGAP + 1
             ENDIF
           ENDDO
         ENDIF   
       
!         write(msg,*) J, istart, istop,SUMGAP
!         CALL rwarn(msg)

       ENDDO ! with I
                    
       END SUBROUTINE DietGap

! -----------------------------------------------------------------------------
! Swapping rows
! -----------------------------------------------------------------------------
            
       SUBROUTINE SwapRows(Nrow, Index, Swap1, Swap2)    
       INTEGER, INTENT (IN) :: Nrow, Swap1, Swap2
       INTEGER, INTENT(INOUT) :: Index(Nrow)
       
       INTEGER :: Itmp
       
         Itmp         = Index(Swap1)
         Index(Swap1) = Index(Swap2)
         Index(Swap2) = Itmp
       
       END SUBROUTINE SwapRows                 

! -----------------------------------------------------------------------------
! INTERVALITY INDEX CALCULATION
! -----------------------------------------------------------------------------
            
       SUBROUTINE intervality(Flow, Nrow, Ncol, Swapind, Nswaps, Nloops,       &
     &  Niter, Verbose, StartTemp, Cool, runif, work, intervalSteps,           &
     &  index, bestindex, workindex, Bestinterval)

       IMPLICIT NONE
       INTEGER :: Nrow, Ncol, Nloops, Nsteps, Nswaps, Niter, Verbose
       INTEGER :: Swapind(Nswaps, 2), intervalSteps(Niter,Nloops)
       INTEGER :: Index(Nrow), Bestindex(Nrow), WorkIndex(Nrow)
       DOUBLE PRECISION :: Flow(Nrow, Ncol), work(Nrow), runif(Nswaps)                 
       DOUBLE PRECISION :: StartTemp, Cool
       
       INTEGER :: I, J, Iswap, swap1, swap2
       INTEGER :: StartInterval, WorkInterval, NewInterval, Bestinterval
       LOGICAL :: finished, accept
       DOUBLE PRECISION :: SumGap, T, Draw, Paccept
       CHARACTER *150 :: MSG

  ! initial interval index
         CALL DietGap (Flow, Nrow, Ncol, index, work, StartInterval)
 	       WorkInterval = StartInterval
 	       BestInterval = StartInterval
         
 	! SIMULATED ANNEALING TO MINIMIZE WorkInterval

	      T = StartTemp

   	    finished = .FALSE.
	      if (StartInterval .EQ. 0) finished = .TRUE.
	
	      Iswap = 1
        DO I = 1, Nloops
          IF (finished) EXIT 

  		    DO J = 1, Niter
			
			! swap two random species (rows)

            swap1 = swapind(iswap, 1)  ! or index(swapind(iswap, 1))
	    		  swap2 = swapind(iswap, 2)
            CALL SwapRows(Nrow, index, swap1, swap2)   ! changes index
            CALL DietGap (Flow, Nrow, Ncol, index, work, NewInterval)
            
 			      accept = .FALSE.
            
            IF (NewInterval .LE. WorkInterval) THEN
			        accept = .TRUE.
  		  	  ELSE ! probability of acceptance as defined by Stouffer et al.
	    			  Paccept = exp(-dble(NewInterval-WorkInterval)/T)
  	  			  draw = runif(Iswap)
			  	    IF (draw .LT. Paccept) THEN
                accept = .TRUE.
              ENDIF
            ENDIF     
			  
            Iswap = Iswap +1 
            if (Iswap > Nswaps) Iswap = 1

            IF (accept) THEN
  	  			  WorkInterval = NewInterval

		    	  	IF (WorkInterval .LT. BestInterval) THEN
			    	    BestInterval = WorkInterval
		  		      BestIndex  = Index
    			    ENDIF	

      			ELSE ! restore index
              CALL SwapRows(Nrow, index, swap2, swap1)
            ENDIF 
            				
   			    intervalSteps(J, I) = WorkInterval
    		    IF (WorkInterval == 0) THEN
              finished = .TRUE.
              EXIT
            ENDIF 			
		      ENDDO ! J
 		  IF (verbose ==1) THEN
         write(msg, *) 'annealing temperature', T,
     &   'current best index', BestInterval
         CALL Rprint (msg)
      ENDIF
		      T = T * Cool
        ENDDO ! I
        

       END SUBROUTINE

