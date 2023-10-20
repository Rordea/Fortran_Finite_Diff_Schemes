PROGRAM LINEAR1

IMPLICIT NONE

!!!!DECLARATION OF VARIABLES!!!!
REAL::LB     !left boundary LOCATION
REAL::RB     !RIGHT BOUNDARY LOCATION
REAL::LENGTH !DOMAIN LENGTH
REAL::DX     !GRID SPACING
REAL::DT     ! TIME STEP SIZE
REAL::U	     !VELOCITY
REAL::T_C    ! CURRENT TIME LEVEL
REAL::T_END  !FINAL TIME LEVEL
REAL::CFL    !CFL NUMBER
INTEGER::POINTS	!NUMBER OF GRID POINTS
INTEGER::IT	!ITERATIONS NUMBER
REAL,ALLOCATABLE, DIMENSION(:)::X !COORDINATES FOR EACH GRID POINT
REAL, ALLOCATABLE, DIMENSION (:,:)::FI !SOLUTION SCALAR
REAL::PI_n
INTEGER::I    !LOOP COUNTER
!character(len=20)::tst

LB=-1.0D0
RB=0.0D0
u=1
LENGTH=ABS(RB)+ABS(LB)
POINTS=1000
DX=LENGTH/(POINTS-1)
cfl=0.5

dt=cfl*dx/u
t_end=1000.0
!!!MEMORY ALLOCATION

PI_n=4*ATAN(1.d0)

ALLOCATE(X(0:points-1)); X=0.0D0
ALLOCATE(FI(0:points-1,1:2)); FI=0.0D0

X(0)=LB
X(POINTS-1)=RB

DO I=1,POINTS-1

X(I)=X(0)+(DX*I)

END DO

!COORDINATES ALREADY SETUP

FI(0,:)=0.0D0
FI(points-1,:)=1.0D0

DO I=1,POINTS-1

FI(I,1)=SIN(2*PI_n*((X(I)-(U*dt))))

!FI(I,1)=0.5*SIN(X(i))

END DO

open(30,file="initial_sin.dat", form="formatted",status="replace")
DO I=0,POINTS-1
WRITE(30,*)X(I), FI(I,1)
END DO
 close(30)


T_C=0.0		!current time


do 	!start time loop

		dt=min(dt,t_end-t_c)
	!print*,t_c,dt

	!upwind scheme
	do i=1,points-1
		fi(i,2)=fi(i,1)-((U*dt/dx)*(fi(i,1)-fi(i-1,1)))
	end do
               

	!current time update
	t_c=t_c+dt

	!update solution (next time level copied to current)
	fi(:,1)=fi(:,2)
        fi(1,1)=fi(POINTS-1,2)
	if (t_c.ge.t_end)then
		exit		!exit loop
	end if




end do

!write(t_end,*) tst 

!write final solution
open(31,file="final_sin_1000.dat", form="formatted",status="replace")
DO I=0,POINTS-1
WRITE(31,*)X(I), FI(I,1)
END DO
 close(31)







END PROGRAM LINEAR1
