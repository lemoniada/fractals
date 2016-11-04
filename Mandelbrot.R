x=seq(-2,2,0.01)
iter=15 #im wiecej tym dokladniejszy fraktal
cl=heat.colors(iter)
win.graph(width=20,height=20)
plot(0,type="l",xlim=c(-2,1),ylim=c(-1.5,1.5),ann=F)
M=matrix(0,length(x),length(x))
for (a in 1:length(x))
{
	for (b in 1:length(x))
	{
		M[a,b]=complex(real=x[a],imaginary=x[b])	#przypisujemy wartosci poczatkowe
		for (k in 1:iter)	#iterujemy zgodnie ze wzorem
		{
			M[a,b]=M[a,b]^2+complex(real=x[a],imaginary=x[b])
			if (Mod(M[a,b])>2)
			{
				M[a,b]=k
				break
			}
			if (Mod(M[a,b])<2 && k==iter) M[a,b]=iter
		}

	}
}
cat('Program dzia³a,cierpliwoœci')
for (a in 1:length(x))
{
	for (b in 1:length(x))
	{
		
		points(x[a],x[b],type="p",pch=20,cex=0.1,col=cl[Re(M[a,b])])
	}
}
#Wyznaczenie pola metoda Monte-Carlo
t=100000
p=0
for (k in 1:t)
{
	a=round(runif(1,min=1,max=length(x)))
	b=round(runif(1,min=1,max=length(x)))
	
   if (Re(M[a,b])==iter) 
   {
      p=p+1
      #points(x[a],x[b],type="p",pch=20,cex=0.1,col="blue")
   }
   # else points(x[a],x[b],type="p",pch=20,cex=0.1,col="black")
}
pole=16*p/t
pole
