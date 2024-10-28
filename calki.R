
gamma = function(t) ifelse(t <= 1, -1i*sqrt(1-t^2), sqrt(t^2-1))
gamma_ = function(t) ifelse(t <= 1, 1i*t/sqrt(1-t^2), t/sqrt(t^2-1))

t = seq(0,2,len=300)

plot_complex = function(x,y,type="l",lty=1,...) matplot(t,cbind(Re(y),Im(y)),type=type,lty=lty,...)

c2n = function(t,n) ifelse(t<=1,cos(2*n*asin(t)),(-1)^n*cosh(2*n*acosh(t)))

k = pi
d = 0.5

q = 1
n = 1
f = function(t) exp(-k*gamma(t)*d) * c2n(t,q) * c2n(t,n)
g = function(t) gamma(t)*sinh(k*gamma(t)*d)
g_ = function(t) gamma_(t)*sinh(k*gamma(t)*d) + gamma(t)*cosh(k*gamma(t)*d)*k*gamma_(t)*d



plot_complex(t,g(t))

#t = seq(0,1,len=100)
plot_complex(t,g(t))
plot_complex(t,f(t))



plot_complex(t,f(t)/g(t) - f(1)/(g_(1)*(t-1)))

