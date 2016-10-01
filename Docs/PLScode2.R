#Better version of Cabrerra's PLS code

# Generate some data
y0 = rnorm(100)      # y: response
y = y0 - mean(y0)    # y: centered - not strictly needed
x1 = rnorm(100)      # define x1 the first predictor 
x1 = (x1 - mean(x1))/sd(x1)  # x1 standardized 
x2 = y+x1+rnorm(100)         # define x2 the second predictor  
x2 = (x2 - mean(x2))/sd(x2)  

# We have defined the data: 3 variables y x1 x2 

# Start First iteration 
pi1 = sum(y*x1)              # define the coef of the 1st PLS 
pi2 = sum(y*x2)              # 
z1 = pi1*x1 + pi2*x2         # z1 is first PLS
z1 = (z1 - mean(z1))/sd(z1)  # z1 standardized 
th1 = lsfit(z1,y,int=F)$coef # calculate reg coef of z1 

pairs(cbind(y,x1,x2,z1,y1))

# Finish first iteration 
y1 = y - th1*z1                   # calculate new responses 
x11 = x1 - sum(x1*z1)*z1/sum(z1*z1) # orthogonal to z1  
x21 = x2 - sum(x2*z1)*z1/sum(z1*z1) # orthogonal to z1

# Now we do the second iteration. 
phi1 = sum(y1*x11) 
phi2 = sum(y1*x21)
z2 = phi1*x11 + phi2*x21
z2 = (z2 - mean(z2))/sd(z2)
th2 = lsfit(z2,y1,int=F)$coef 

y2 = y1 - th2*z2
# Another way to calculate z2:
z2 = (x11-mean(x11))/sd(x11) 
pairs(cbind(y1,x11,x21,z1,z2)) 


# write a function that does it

fpls = function(x,y,k) { 
  x1 = x
  z = x[,1:k]*0
  theta = NULL 
  phi = array(NA, dim=c(k,ncol(x)) )  
  for(i in 1:k) {
    #  start by standardizing the variables
    y1 = y - mean(y)
    for( j in 1:ncol(x))  x1[,j] = (x1[,j] - 
                                      mean(x1[,j]))/sd(x1[,j])
    phi[i,] = apply(x1*y1,2,sum)
    .
    . 
  }