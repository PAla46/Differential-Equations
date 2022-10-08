Program runge_kutta
    IMPLICIT NONE
    Real*8 :: f,x,y,h,y_a,error,y_ac,f1,f2,f3 
    Real*8:: y_1,y_2,y_3
    Integer :: i

    x = 0
    y = 0
    y_a = 92.620
    h=0.001

    open(10,file='values_4.dat')
    open(20,file='actual_values.dat')

    do i= 1,1560
        f = (y*y) + 1 
        x = x + h
        y_1 = y + ((h*f)/2)
        f1 = (y_1*y_1) + 1
        y_2 = y + ((h*f1)/2)
        f2 = (y_2*y_2) + 1
        y_3 = y + (h*f2)
        f3 = (y_3*y_3) + 1
        y = y + ((h*(f+(2*f1)+(2*f2)+f3))/6)
        y_ac = tan(x)
        write(10,*) x,y 
        write(20,*) x,y_ac
    end do
    
close(10)
close(20)

    error = y_a - y  
    print*,'The error difference is', error

End Program runge_kutta