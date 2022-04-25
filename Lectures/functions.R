
Epan.kernel <- function(phi,...) {
  0.75*(1-phi^2)*(abs(phi)<=1)
}

GGZSim=function(N=100,pi=0.5,a,b)
{
  rel_share=c()
  for (n in 1:N)
  {
    sample=rbinom(n,1,pi)
    rel_share[n]=mean(sample)
  }
  IntervallAnt=sum(a<rel_share&rel_share<b)/N
  return=list(rel=rel_share,Interval=IntervallAnt)
}
set.seed(50000)
###2000 works well, 3500, 45000
n=50
x<-rnorm(n,0,4)
eps<-rnorm(n)
y<-0.5*x+rnorm(n,0,5)
plot(x,y)


np.estim <- function(x,h,Kern=Epan.kernel,y){
  # Number of sample observations
  N <- length(x)
  x=sort(x)
  # Number of points at which to evaluate
  n.grid <- length(x)
  dens.est <- vector("double",length=n.grid)
  for(i in 1:length(x)){
    phi <- ((x - x[i])/h) 
    nom <- mean(Kern(((x - x[i])/h))*y) 
    denom <- mean(Kern(phi))
    dens.est[i]<-nom/denom
  } 
  return(dens.est)
}

np_est<-np.estim(x,0.2,Kern=Epan.kernel,y)
plot(x,y)
lines(x,np_est,type="p")
Xy<-data.frame(y,x)

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Xy)), size = n/2)
train <- Xy[train_ind, ]
test <- Xy[-train_ind, ]

####make the fit for the training sample############
np_est<-np.estim(train[,2],0.2,Kern=Epan.kernel,train[,1])
plot(train[,2],train[,1],col="red")
lines(train[,2],np_est,type="p")
lines(test[,2],test[,1],type="p",col="blue")


