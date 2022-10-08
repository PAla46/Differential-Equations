Program runge_kutta_2
    IMPLICIT NONE
    Real*8 :: f,x,v,h,f1,f2,f3,f_v,f_v1,f_v2,f_v3 
    Real*8:: x_1,x_2,x_3,v_1,v_2,v_3
    Integer :: i,T,n_iter

    x = 0.1
    v = 1.0
    h=0.01
    T = 50
    n_iter = int(T/h) 

    open(10,file='values_5.dat')

    do i= 1,n_iter
    !   Setting the initial functions

        f = v 
        f_v = -sin(x) 

        x_1 = x + ((f*h)/2)
        v_1 = v + ((f_v*h)/2)

        f1 = v_1
        f_v1 = -sin(x_1)

        x_2 = x + ((f1*h)/2)
        v_2 = v + ((f_v1*h)/2)

        f2 = v_2
        f_v2 = -sin(x_2)

        x_3 = x + (f2*h)
        v_3 = v + (f_v2*h)

        f3 = v_3
        f_v3 = -sin(x_3)

        x = x + ((h*(f + (2*f1) + (2*f2) + f3))/6)
        v = v + ((h*(f_v + (2*f_v1) + (2*f_v2) + f_v3))/6)

        write(10,*) x,v

    end do
    
close(10)


End Program runge_kutta_2
