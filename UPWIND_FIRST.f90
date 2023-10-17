PROGRAM UPWIND_FIRST

IMPLICIT NONE

!!!!DECLARATION OF VARIABLES !!!!

REAL::LB     !left boundary LOCATION
REAL::t      ! time counter
REAL::RB     !RIGHT BOUNDARY LOCATION
REAL::LENGTH !DOMAIN LENGTH
REAL::DX     !GRID SPACING
REAL::DT     ! TIME STEP SIZE
REAL::U	     !VELOCITY
REAL::INI_TM    ! CURRENT TIME LEVEL
REAL::FIN_TM  !FINAL TIME LEVEL
REAL::CFL    !CFL NUMBER
INTEGER::POINTS	!NUMBER OF GRID POINTS
INTEGER::IT	!ITERATIONS NUMBER
REAL,ALLOCATABLE, DIMENSION(:)::X !COORDINATES FOR EACH GRID POINT
REAL, ALLOCATABLE, DIMENSION (:,:)::FI !SOLUTION SCALAR
INTEGER::I    !LOOP COUNTER


LB=-40.0D0 !DOUBLE PRECISION NUMBER, IN WHICH "D" STANDS FOR "TIMES TEN TO THE POWER"
RB=40.0D0



U=1
LENGTH=ABS(LB)+ABS(RB)
POINTS=100
DX=LENGTH/(POINTS-1)
CFL=1
DT=CFL*DX/U

INI_TM = 0.0

FIN_TM = 5.0

ALLOCATE(X(0:points-1)); x=0.0D0 !A WAY TO DEFINE ARRAYS/LISTS IN FORTRAN
ALLOCATE(FI(0:points-1,1:2)) ; FI=0.0D0 
!write(*,*),"X", X
!write(*,*),"FI",FI

X(0)=LB !DEFINING BOUNDARY UNCHANGEABLE BOUNDARY CONDITIONS
X(POINTS-1)=RB !DEFINING BOUNDARY UNCHANGEABLE BOUNDARY CONDITIONS

DO I=1, POINTS-2 !CREATING A LOOP FROM "1" TO "POINTS-2"






!!!!!!!! INITIALISATION OF VARIABLES !!!!!!!!!!!!!!!!


! SET X COORDINATES

X(I)=X(0)+(DX*I)

!print*,X(I)

FI(I,1)=0.5*SIN(X(i)) ! "i" is detected as a counter automatically?



END DO

open(30,file="initial_data.dat", form="formatted",status="replace") !CREATE FILE AND WRITE DATA

DO I=0,POINTS-1


  WRITE(30,*)X(I), FI(I,1) 


END DO


 close(30)


! TIME LOOP


DO t = INI_TM, FIN_TM, DT



   DO i=1 , points-2
                        
                
           fi(i,2)=fi(i,1)-( ((U*dt/dx)*(fi(i,1)-fi(i-1,1)))  ) !Discretization method
    
           print*,fi(:,2)

   END DO


   print*, ""
   print*, ""
   print*, t
   print*, ""
   print*,""


   
   INI_TM = INI_TM + DT !Update time

   fi(:,1)=fi(:,2)
      
   

END DO



open(31,file="final_data.dat", form="formatted",status="replace")
DO I=0,POINTS-1
WRITE(31,*)X(I), FI(I,1)
END DO
 close(31)



END PROGRAM UPWIND_FIRST


