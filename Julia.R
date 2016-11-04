x=seq(-2,2,0.01)
iter=15
cl=terrain.colors(iter)
julia.point=complex(real=-0.2,imaginary=-0.8)
win.graph(width=20,height=20)
plot(0,type="l",xlim=c(-2,2),ylim=c(-2,2),ann=F)
M=matrix(0,length(x),length(x))
cat('Program dzia³a,cierpliwoœci')
for (a in 1:length(x))
{
	for (b in 1:length(x))
	{
		M[a,b]=complex(real=x[a],imaginary=x[b])	#przypisujemy wartosci poczatkowe
		for (k in 1:iter)	#iterujemy zgodnie ze wzorem
		{
			M[a,b]=M[a,b]^2+julia.point
			if (Mod(M[a,b])>2)
			{
				M[a,b]=k
				break
			}
			if (Mod(M[a,b])<2 && k==iter) M[a,b]=iter
		}

	}
}

for (a in 1:length(x))
{
	for (b in 1:length(x))
	{
		
		points(x[a],x[b],type="p",pch=20,cex=0.1,col=cl[Re(M[a,b])])
	}
}
