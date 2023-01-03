
program rhapson
implicit none
real::e,x1,x2,xn1,xn2,mod1,mod2
e=0.0001
x1=3.0

100 xn1=x1-(((x1**3)+x1-10)/(3*(x1**2)+1))
mod1=abs((xn1-x1)/xn1)
if (mod1.gt.e) then
x1=xn1
goto 100
else
write(*,*)"The root of f(x)=x**3+x-10 is",xn1 
end if

x2=1/2
200 xn2=x2-((2.718282**((-0.5)*x2))+(5*sin(2*x2))+(2*cos(5*x2)))/((-1/2)*(2.718282**((-0.5)*x2))+(10*cos(2*x2))-(10*sin(5*x2)))
mod2=abs((xn2-x2)/xn2)
if (mod2.gt.e) then
x2=xn2
goto 200
else
write(*,*)"The root of f(x)=(e**((-0.5)*x))+(5*sin(2*x))+(2*cos(5*x)) is",xn2 
end if

end program rhapson
