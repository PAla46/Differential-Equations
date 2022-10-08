Program rk4coupledw
    IMPLICIT NONE
    Integer :: i,j,k,niter 
    Real*8, parameter :: dt = 0.02d0
    Integer, parameter :: nop = 50
    Real*8 :: t 
    Real*8 :: x(nop),v(nop),f0(nop)
    Real*8 :: f1(nop),f2(nop),f3(nop)
    Real*8 :: x1(nop),x2(nop),x3(nop)
    Real*8 :: v1(nop),v2(nop),v3(nop)
    Real*8 :: f0v(nop),f1v(nop),f2v(nop),f3v(nop)
    t = 0
    ! print*, ' The value of x at t=0 is'
    ! read*, x
    ! print*, ' The value of v at t=0 is'
    ! read*, v

    do j = 1,nop
        v(j) = 0.d0
        x(j) = 0.d0
    end do

    x(1) = 0.6d0
    x(26) = 0.6d0 
    ! print*, x

    print*,'number of iterations is'
    read*, niter

    open(11,file='rk4_coupled_w7.dat',form = 'formatted')
    do i = 1,niter
        do k = 1,nop
            if(k .eq. 1) then

                f0(1) = v(1)
                x1(1) = x(1) + (dt*f0(1))*0.5d0
                f0v(1) = (x(2)+x(50)-2*x(1))
                v1(1) = v(1) + (dt*f0v(1))*0.5d0
                f1(1) = v1(1)
                x2(1) = x(1) + (dt*f1(1)) *0.5d0
                f1v(1) = (x1(2)+x1(50)-2*x1(1))
                v2(1) = v(1)+(dt*f1v(1))*0.5d0
                f2(1) = v2(1)
                x3(1) = x(1) + (dt*f2(1))
                f2v(1) = (x2(2)+x2(50)-2*x2(1))
                v3(1) = v(1) + (dt*f2v(1))
                f3(1) = v3(1)
                f3v(1) = (x3(2)+x3(50)-2*x3(1))

            else if (k .eq. 50) then

                f0(50) = v(50)
                x1(50) = x(50) + (dt*f0(50))*0.5d0
                f0v(50) = (x(1)+x(49)-2*x(50))
                v1(50) = v(50) + (dt*f0v(50))*0.5d0
                f1(50) = v1(50)
                x2(50) = x(50) + (dt*f1(50)) *0.5d0
                f1v(50) = (x1(1)+x1(49)-2*x1(50))
                v2(50) = v(50)+(dt*f1v(50))*0.5d0
                f2(50) = v2(50)
                x3(50) = x(50) + (dt*f2(50))
                f2v(50) = (x2(1)+x2(49)-2*x2(50))
                v3(50) = v(50) + (dt*f2v(50))
                f3(50) = v3(50)
                f3v(50) = (x3(1)+x3(49)-2*x3(50))

            else

                f0(k) = v(k)
                x1(k) = x(k) + (dt*f0(k))*0.5d0
                f0v(k) = (x(k+1)+x(k-1)-2*x(k))
                v1(k) = v(k) + (dt*f0v(k))*0.5d0
                f1(k) = v1(k)
                x2(k) = x(k) + (dt*f1(k)) *0.5d0
                f1v(k) = (x1(k+1)+x1(k-1)-2*x1(k))
                v2(k) = v(k)+(dt*f1v(k))*0.5d0
                f2(k) = v2(k)
                x3(k) = x(k) + (dt*f2(k))
                f2v(k) = (x2(k+1)+x2(k-1)-2*x2(k))
                v3(k) = v(k) + (dt*f2v(k))
                f3(k) = v3(k)
                f3v(k) = (x3(k+1)+x3(k-1)-2*x3(k))

            end if

            x(k) = x(k) + dt*(f0(k) + 2*f1(k) + 2*f2(k) + f3(k))/6.d0
            v(k) = v(k) + dt*(f0v(k) + 2*f1v(k) + 2*f2v(k) + f3v(k))/6.d0

            write(11,*) dfloat(i)*dt , k ,x(k) ,v(k) 

        end do 

    end do 

    print*, 'Value of x(1) at the end of 2000 iterations is', x(1)

    close(11)

End Program rk4coupledw
