#################################
# The likelihood monster:  A cautionary tale of pathological proportions
# February 27, 2016
#################################

# The problem likelihood:  0.5*(1 + theta*x); -1 <= x <= 1; -1 <= theta <= 1
# Plot it for some values of theta

# set some x values between -1 and 1 
#(Note:  Not samples (that's why I call them test.x), just values for plotting the pdf)
test.x = seq(-1,1,by=0.1)

# Define theta and the pdf.y value
theta = 0
pdf.y = 0.5*(1 + theta*test.x)

plot(test.x,pdf.y,type="l",xlim=c(-1,1),ylim=c(0,1))
title("PDF, theta=0 (black), theta<0 (red), theta>0 (blue)")

theta = 1
pdf.y = 0.5*(1 + theta*test.x)
lines(test.x,pdf.y, col="red")

theta = -1
pdf.y = 0.5*(1 + theta*test.x)
lines(test.x,pdf.y, col="blue")

theta = 0.5
pdf.y = 0.5*(1 + theta*test.x)
lines(test.x,pdf.y, col="red", lty=3)

theta = -0.5
pdf.y = 0.5*(1 + theta*test.x)
lines(test.x,pdf.y, col="blue", lty=3)

#######################################
# Now, the likelihood function is a function of *theta* based on an observed sample of x's from 
# a fixed value of theta.
#
# In a real data analysis situation, we don't know theta and the MLE gives us the most likely value
# of theta given the observations and the functional form of the pdf.
#
# To see how this works, we'll use simulation in the following steps:
#  1. Set a value of theta 
#  2. Generate a sample of x's given that theta value
#  3. Calculate the likelihood function based on those x's
#  4. See where the maximum of that likelihood function falls (that will be the MLE).
#  Most of the time this works really well, but let's see what happens here.
########################################
#

########################################
# Example 1:  theta = 0.0 (Uniform(-1,1))

#Get a sample of, say 100, values from a uniform(-1,1) distribution.
theta = 0.0
x.sample = runif(100,min=-1,max=1)

# So with our sample of 100 points from a fixed value of theta, calculate the likelihood function for
# a range of values of theta (remember, a likelihood function is based on the functional form of the pdf
# and a semple of values...we want to evaluate the likelihood function for each possible (or a wide range of) 
# potential values of theta and find the value that maximizes the likelihood function.)

possible.thetas = seq(-1,1,by=0.01)
# Set the likelihood for each theta value to zero (we'll change this to the right values in the loop)
lik.from.thetas = 0.0*possible.thetas

# Loop through each theta and calculate the likelihood value based on the observed set of x.sample values
for (i in 1:length(possible.thetas)) {
	# calculate the likelihood for this possible value of theta as the product of the 
	# pdf evaluated for theta = possible.thetas[i] at each of the values in x.sample
	lik.from.thetas[i] = prod( (0.5)*(1+possible.thetas[i]*x.sample) )
}

# Plot it
plot(possible.thetas,lik.from.thetas,type="l",ylim=c(0,max(lik.from.thetas)))
title(paste("Likelihood for theta = ",theta,sep=""))
# Add points of the observed values of x.sample as a "rug"
points(x.sample,rep(0,length(x.sample)),pch="|")
# What do you see?
##############################################


########################################
# Example 2:  theta = 0.5.
# Set theta

theta = -0.5

##############################
# Simulation from pdf using acceptance rejection sampling
# Generate a uniform on -1,1, then flip a coin with probability of keeping the value
# set to be the pdf.

sample.size = 100
x.sample = NULL
while(length(x.sample)<sample.size) {
	test = runif(1,min=-1,max=1)
	accept.prob = prod( (0.5)*(1+theta*test) )
#	print(accept.prob)
	if (accept.prob > 1.0) {x.sample = c(x.sample,test)}
	if (accept.prob < 1.0) {
		coin = runif(1,min=0,max=1)
		if(coin < accept.prob) {x.sample = c(x.sample,test)}
		}
	}
# Plot it if you want to check...
par(mfrow=c(2,1))
hist(x.sample,freq=F,ylim=c(0,1),nclass=10)
pdf.y = 0.5*(1 + theta*test.x)
lines(test.x,pdf.y, col="red", lty=3)

# So with our sample of sample.size points from a fixed value of theta, calculate the likelihood function for
# a range of values of theta (remember, a likelihood function is based on the functional form of the pdf
# and a semple of values...we want to evaluate the likelihood function for each possible (or a wide range of) 
# potential values of theta and find the value that maximizes the likelihood function.)

possible.thetas = seq(-1,1,by=0.01)
# Set the likelihood for each theta value to zero (we'll change this to the right values in the loop)
lik.from.thetas = 0.0*possible.thetas

# Loop through each theta and calculate the likelihood value based on the observed set of x.sample values
for (i in 1:length(possible.thetas)) {
	# calculate the likelihood for this possible value of theta as the product of the 
	# pdf evaluated for theta = possible.thetas[i] at each of the values in x.sample
	lik.from.thetas[i] = prod( (0.5)*(1+possible.thetas[i]*x.sample) )
}

# Plot it
plot(possible.thetas,lik.from.thetas,type="l",ylim=c(0,max(lik.from.thetas)))
title(paste("Likelihood for theta = ",theta,sep=""))
# Add points of the observed values of x.sample as a "rug"
points(x.sample,rep(0,length(x.sample)),pch="|")

# Put true value of theta (which we know from our simulations) on plot
segments(theta,0,theta,1,col="green")
# What do you see?
##############################################

