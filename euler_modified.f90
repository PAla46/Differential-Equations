Program euler_modified
    IMPLICIT NONE
    Real*8 :: f,x,y,h,y_a,error,y_ac,f1,y_o 
    Integer :: i

    x = 0
    y = 0
    y_a = 92.620
    h=0.001

    open(10,file='values_1.dat')
    open(20,file='actual_values.dat')

    do i= 1,1560
        f = (y*y) + 1 
        x = x + (h/2)
        y_o = y + ((h*f)/2)
        f1 = (y_o*y_o) + 1
        y = y + (h*f1)
        y_ac = tan(x)
        write(10,*) x,y 
        write(20,*) x,y_ac
    end do
    
close(10)
close(20)

    error = y_a - y 
    print*,'The error difference is', error

End Program euler_modified

