PROGRAM EDOl2
	implicit none
	REAL::a,b,c,d,x1,x2
	COMPLEX::x,dc
	!Explaining second-order linear ordinary differential equations
	PRINT*, "A second-order linear ordinary differential equation can be equivalent to:"
	PRINT*, "aY''+bY'+cY=0"

	!Collecting parameters
	PRINT*, "Now enter the parameters a, b and c:"
	PRINT*, "a:"
	READ*,a
	PRINT*,"b:"
	READ*,b
	PRINT*,"c:"
	READ*,c

	!Resolution:
	d=b**2-4*a*c
	if(d>0) then
		x1=(-b+sqrt(d))/2*a
		x2=(-b-sqrt(d))/2*a
		WRITE(*,*) "y(x)=C1*exp(",x1,"*x)+C2*exp(",x2,"*x)"
	END IF
	IF(d==0) then
		x=-b/2*a
		WRITE(*,*) "y(x)=exp(",real(x),"*x)*(C1+x*C2)"
	END IF
	IF(d<0) THEN
		dc=cmplx(d,0)
		x=(-b+sqrt(dc)/2*a)
		WRITE(*,*) "y(x)=exp(x*",real(x),")*(C1*cos(",aimag(x),"*x)+C2*sin(",aimag(x),"*x))"
	END IF
	PRINT*, "Where C1 and C2 are integration constants"
END PROGRAM EDOl2
