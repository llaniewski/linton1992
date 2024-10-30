# gamma from paper:
gamma = function(t) ifelse(t <= 1, -1i*sqrt(1-t^2), sqrt(t^2-1))
# gamma':
gamma_ = function(t) ifelse(t <= 1, 1i*t/sqrt(1-t^2), t/sqrt(t^2-1))
# c2n from paper:
c2n = function(t,n) ifelse(t<=1,cos(2*n*asin(t)),(-1)^n*cosh(2*n*acosh(t)))

# example parameters
k = pi
d = 0.5
q = 1
n = 1

# f and g from paper:
f = function(t) exp(-k*gamma(t)*d) * c2n(t,q) * c2n(t,n)
g = function(t) gamma(t)*sinh(k*gamma(t)*d)
# g'
g_ = function(t) ifelse(t==1,2*d*k,gamma_(t)*sinh(k*gamma(t)*d) + gamma(t)*cosh(k*gamma(t)*d)*k*gamma_(t)*d)

# function for plotting complex functions
plot_complex = function(x,y,type="l",lty=1,...) matplot(t,cbind(Re(y),Im(y)),type=type,lty=lty,...)

# some plots
t = seq(0,2,len=300)
plot_complex(t,g(t))
plot_complex(t,f(t))
plot_complex(t,f(t)/g(t))
plot_complex(t,f(t)/g(t) - f(1)/(g_(1)*(t-1)))

# function for integrating complex functions
integrate_complex = function(fun, ...) {
    remove_nan = function(x) ifelse(is.finite(x),x,0)
    ret = list()
    ret$Re = integrate(function(x) remove_nan(Re(fun(x))),...)
    ret$Im = integrate(function(x) remove_nan(Im(fun(x))),...)
    ret$value = ret$Re$value + 1i*ret$Im$value
    ret
}

# the function under integral without modification
h = function(t) f(t)/g(t)

# First integral from 2x0 to Infty 
ret = integrate_complex(h, lower=2, upper=Inf)
Integral1 = ret$value

# Naive integral of the 0 to 2x0 is divergent:
ret = integrate_complex(h, lower=0, upper=2)

# We can remove singularities:
h = function(t) f(t)/g(t) - f(1)/(g_(1)*(t-1))
# And now the integral can be computed:
ret = integrate_complex(h, lower=0, upper=2)
Integral2 = ret$value

# But we need a "correction" for the subtracted singularity
Integral3 = f(1)/g_(1) * log(2/1-1)

#Result:
Integral1 + Integral2 + Integral3
