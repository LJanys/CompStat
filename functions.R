
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