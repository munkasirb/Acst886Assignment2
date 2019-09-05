##2
agegroup=c(1:11)
EtR=c(15518,19428,21594,21890,19174,15775,11414,6993,3276,1096,2001)
Actdeath=c(65,144,219,378,465,557,685,644,471,217,67)
totaldeath=sum(Actdeath)
Expdeath=c(73.9,134.6,223.9,346.3,468.1,600.2,675.5,637.4,458.7,240.6,61.4)
qx=Expdeath/EtR
Vardeath=Expdeath*(1-qx)

#Z-score
Zx=(Actdeath-Expdeath)/sqrt(Vardeath)
chi_2=Zx^2
sumchi_2=sum(chi_2)
qchisq(.95,11)
## chi-square value witht 11 degree of freedom is higher than the sum of all chi-square with 
## 1 degree freedom each. Therefore we cannot reject the null hypothesis. We can also calculate 
## the p-value.
## However, the problem of aggregate chi-square distribution is that it does not tell the
## individual behaviour of the data. 
pchisq(sumchi_2,11)

## Fot the Z-scores, we ecpect to have the ranges of (-3,-2),(-2,-1),(-1,0),(0,1),(1,2),(2,3)
## and we generate the probabilities of being in these ranges.
r=c()
r[1]=pnorm(-3)
r[2]=pnorm(-2)-pnorm(-3)
r[3]=pnorm(-1)-pnorm(-2)
r[4]=pnorm(0)-pnorm(-1)
r[5]=pnorm(1)-pnorm(0)
r[6]=pnorm(2)-pnorm(1)
r[7]=pnorm(3)-pnorm(2)
r[8]=1-pnorm(3)
r
## Now we calculate the expected numbers falling in these ranges.
Er=11*r
Er

Ar=c()
Ar[1]=sum(Zx<(-3))
Ar[2]=sum((Zx>(-3))*(Zx<(-2)))
Ar[3]=sum((Zx>(-2))*(Zx<(-1)))
Ar[4]=sum((Zx>(-1))*(Zx<(0)))
Ar[5]=sum((Zx>(0))*(Zx<(1)))
Ar[6]=sum((Zx>(1))*(Zx<(2)))
Ar[7]=sum((Zx>(2))*(Zx<(3)))
Ar[8]=sum(Zx>(3))
Ar

# SD test
t1=c()
t1[1]=(sum(Ar[1:3])-sum(Er[1:3]))^2/sum(Er[1:3])
t1[2]=(Ar[4]-Er[4])^2/Er[4]
t1[3]=(Ar[5]-Er[5])^2/Er[5]
t1[4]=(sum(Ar[6:8])-sum(Er[6:8]))^2/sum(Er[6:8])
t1
## The p-value of the chi-square statistics with dof 3 is not significant at the 0.5% level.
## Therefore we can say taht the S.D distribution is unit normal. 
1-pchisq(sum(t1),3)

# Absolute Deviation test
## The probability that a unit normal lies between 2/3 and 02/3 is almost 0.5.
pnorm(2/3)-pnorm(-2/3)
## We then count the absolute Z-scores(SD) that are greater than 2/3
AbDev=sum(abs(Zx)>2/3)
AbDev
## Using a binomial (11,0.5) distribution, we can work out what the probability of such a count is.  
1-pbinom(AbDev,11,0.5)
## it is not significant, which gives not enough evidence to reject the null hypothesis, which 
## is the data used to calculate the expected death is suitable. 

# Cumulative Deviation
CV=(sum(Actdeath)-sum(Expdeath))/sqrt(sum(Vardeath))
CV
## it is smaller than the 1.96 absolute value = therefore we can say that the adherence to data 
## is good. 

# Sign test or serial correlation????
zi=Zx[1:10]
ziplus1=Zx[2:11]
zilesszbar=zi-mean(Zx)
ziplus1lesszbar=ziplus1-mean(Zx)
unknown=(Zx-mean(Zx))^2
sum=sum(unknown)
result=sum(zilesszbar*ziplus1lesszbar)/10/sum(unknown)*11*sqrt(10)
