
## Q1
A=as.Date(c("17march1964","20june1992",""),"%d%B%Y")
B=as.Date(c("06may1964","06august1992","12june1993"),"%d%B%Y")
C=as.Date(c("12august1964","18december1992","18june1995"),"%d%B%Y")
D=as.Date(c("27october1964","04january1993",""),"%d%B%Y")
E=as.Date(c("04january1865","28april1993","29august1996"),"%d%B%Y")
F=as.Date(c("18april1965","16june1993","12december1995"),"%d%B%Y")
G=as.Date(c("20may1965","29october1993","21april1996"),"%d%B%Y")
H=as.Date(c("04july1965","16february1994",""),"%d%B%Y")
I=as.Date(c("16september1965","22august1994",""),"%d%B%Y")
J=as.Date(c("11december1965","06march1995","17february1997",""),"%d%B%Y")

((A[1]+(31*365.25))-(A[1]+(30*365.25)))/365.25
# a
life=c()
life[1]=(((A[1]+(31*365.25))-(A[1]+(30*365.25)))/365.25) ## one year of exposed to risk
life[2]=0  #the date of exit is earlier than the date of reaching age 30, therefore life B is not exposed to risk.
life[3]=(C[3]-(C[1]+(30*365.25)))/365.25
life[4]=(((D[1]+(31*365.25))-(D[1]+(30*365.25)))/365.25)
life[5]=(((E[1]+(31*365.25))-(E[1]+(30*365.25)))/365.25)
life[6]=(F[3]-(F[1]+(30*365.25)))/365.25
life[7]=(G[3]-(G[1]+(30*365.25)))/365.25
life[8]=(((H[1]+(31*365.25))-(H[1]+(30*365.25)))/365.25)
life[9]=(((I[1]+(31*365.25))-(I[1]+(30*365.25)))/365.25)
life[10]=(((J[1]+(31*365.25))-(J[1]+(30*365.25)))/365.25)

q30=SOMETHING/sum(life)

# b
