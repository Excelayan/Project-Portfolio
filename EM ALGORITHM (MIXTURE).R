# EM Algorithm to find MLE: Estimation of Mixing proportions Example: Eample 1.2 page 13 McLachlan and Krishnan (2008) 
# worked out on Numerical example given on page no. 17

pi1=0.8       
pi2=0.2
n=50
R=0;k=0;j=0;r=0
u1=0;u2=2;sigma=1;
z=matrix(0,2,n)

# random sample from mixture of two univariate Normal population

for(i in 1:n)
{
	r[i]=runif(1,0,1)
	if(r[i]>pi2)
	{
		k=1
		j=j+1
		z[k,j]=1
		R[i]=rnorm(1,u1,sigma)
	}
	else
	{
		k=2
		j=j+1
		z[k,j]=1
		R[i]=rnorm(1,u2,sigma)
	}
}
R		
p=(mean(R)-u2)/(u1-u2);p	#initial value of pi1
z					
e=0.00001;k=0
p0=p;s=0
for(i in 1:n)
	{
		d=dnorm(R[i],u1,sigma)/(p0*dnorm(R[i],u1,sigma)+(1-p0)*dnorm(R[i],u2,sigma))
		s=s+d
	}
	p1=p0*s/n
while(abs(p0-p1)>e)
{
 	k=k+1
	s=0
	p0=p1
	for(i in 1:n)
	{
		d=dnorm(R[i],u1,sigma)/(p0*dnorm(R[i],u1,sigma)+(1-p0)*dnorm(R[i],u2,sigma))
		s=s+d
	}
	p1=p0*s/n
}
p1
p2=1-p1
k
sum(z[1,])	# no of observations from 1st populaton