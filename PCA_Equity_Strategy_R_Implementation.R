#PCA ANALYSIS - PORTFOLIO ALLOCATION:  ASSET CLASS - UK EQUITIES#

# R script performing a PCA on the correlation matrix of a portfolio of UK stock returns. 

#Once the "principal portfolios" were defined, we evaluated their absolute and relative performance 
#against an equally weighted benchmark portfolio. The asset allocation strategy that stems from 
#PCA is based on the coefficients of each eigenvector of the correlation matrix. 
#Each of them identifies the TARGET ALLOCATION VECTOR of a principal portfolio. 

require(quantmod) 
require(PerformanceAnalytics) #MAIN PACKAGE USED#
require(xtable) #export of tables in Latex
require(corrplot) #correlation matrix with heat map
require(scatterplot3d) #3D scatterplot
require(factoextra) #fancy screeplots
require(tseries) #creation of syntetic portfolios returns
require(randomcoloR) #palette

##Portfolio of 22 UK stocks, geometric returns were calculated in the specified time frame. ##

####fetch stocks' tickers into a list####
tickers<-c("BLT.L","RIO.L","VOD.L","BT-A.L","REL.L","CCL.L","ULVR.L","BATS.L","RDSA.L","BP.L","HSBA.L","LLOY.L","GSK.L","AZN.L","BA.L","EXPN.L","LAND.L",
"BLND.L","ARM.L","SGE.L","NG.L","SSE.L")

####set time frame for our analysis####
date_begin<-as.Date("2010-01-01")
date_end<-as.Date("2016-01-01")

####obtain stocks' prices, each in a separate xts object####
stocks<-getSymbols(tickers,from=date_begin,to=date_end,auto.assign=TRUE)

####extract adjusted closing prirces for the stocks and combine all stocks into one data set####
dataset<-Ad(get(stocks[1]))
for (i in 2:length(stocks)) {
  dataset<-merge(dataset,Ad(get(stocks[i])))
}

#adjust for NAs by carrying forward the last observation (locf) if there is a missing value#
data_locf=na.locf(dataset)

####calculate daily log returns and create a single xts object####
return_lag=1
data<-na.omit(ROC(data_locf,return_lag,type="continuous"))
names(data)<-tickers

#annualise for the argument "scale" (see next)
p=nyears(data)
str(data) 
x=1555    ##########CHECK THE NUMBER OF OBSERVATIONS (x) under Data:num [1:x]##########
scale=x/p #rude way to find the average number of trading days over multiple years

#####descriptive statistics of portfolio constituents#####
chart.CumReturns(data,geometric=TRUE,colorset=rainbow(22),legend="topleft",main="Portfolio Constituents Cumulative Returns",
ylab="Cumulative return (1£)",begin="1",wealth.index=TRUE,cex.lab=TRUE)
chart.Boxplot(data,colorset=rainbow(22),legend="topleft",main="Boxplot of Portfolio Constituents Returns")
chart.RiskReturnScatter(data,colorset=rainbow(22),method="calc",Rf=.0,main="Portfolio Relative Risk and Return",
add.boxplots=TRUE,scale=scale,add.sharpe=c(1,2,3))

#Tables 1
#xtable(SharpeRatio.annualized(data[,1:6]),scale=scale)
#xtable(SharpeRatio.annualized(data[,7:12]),scale=scale)
#xtable(SharpeRatio.annualized(data[,13:18]),scale=scale)
#xtable(SharpeRatio.annualized(data[,19:22]),scale=scale)

#Tables 2
#xtable(table.Stats(data[,1:6]),digits=4)
#xtable(table.Stats(data[,7:12]),digits=4)
#xtable(table.Stats(data[,13:18]),digits=4)
#xtable(table.Stats(data[,19:22]),digits=4)

#plot correlation matrix (lower triangle matrix)
mcor<-round(cor(data),2)
#upper<-mcor
#upper[upper.tri(mcor)]<-""
#xtable(upper)

#####plot correlation matrix heat map with values#####
corrplot(mcor,method="number",type="lower",tl.col=rainbow(22),cl.lim=(0:1),bg="gold2")

#PCA with the covariance matrix using spectral decomposition (princomp) - vs prcomp: sing. value decomposition -
#pca.cov<-princomp(data)
#summary(pca.cov)

#####PCA with the correlation matrix using spectral decomposition (princomp) - vs prcomp: sing. value decomposition -#####
pca.corr<-princomp(data,cor=TRUE)
#names(pca.corr)
#summary(pca.corr)
#pca.corr$loadings

#PCA with eigen function (spectral decomposition)
#a<-eigen(cor(data))
#var<-a$values
#plot(var,type="l",ylab="Eigenvalues",xlab="Component Number")
#points(var,col="red")

#####fancy screeplots#####
fviz_eig(pca.corr,choice="eigenvalue",addlabels=T,barfill="white",barcolor="blue",linecolor="red",ncp=22)
fviz_eig(pca.corr,choice="variance",addlabels=T,barfill="white",barcolor="blue",linecolor="red",ncp=22)

#####cumulative PCs variance graph#####
plot(cumsum(pca.corr$sd^2/sum(pca.corr$sd^2)),main="Plot of Cumulative Variance Explained by Principal Compoents",type="l",ylab="Cumulative Variance",
xlab="Component Number",col="red")
points(cumsum(pca.corr$sd^2/sum(pca.corr$sd^2)),col="blue")
grid()
n<-15
abline(v=n,col="red")

#Tables 3
#xtable(a$vectors[,1:6],digits=4)
#xtable(a$vectors[,7:12],digits=4)
#xtable(a$vectors[,13:15],digits=4)

####bi plots with colors corresponding to each sector's stock#####
sectors<-c("Basic Materials","Basic Materials","Communication Services","Communication Services","Consumer Cyclical","Consumer Cyclical",
"Consumer Defensive","Consumer Defensive","Energy","Energy","Financial Services","Financial Services","Healthcare","Healthcare","Industrials",
"Industrials","Real Estate","Real Estate","Technology","Technology","Utilities","Utilities")

#2D scatterplot 1,2
n1=1
n2=2

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC1",ylab="PC2",pch=20,col=cols[X12$sectors],main="Bi Plot of PC1 and PC2's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 3,4
n1=3
n2=4

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC3",ylab="PC4",pch=20,col=cols[X12$sectors],main="Bi Plot of PC3 and PC4's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 5,6
n1=5
n2=6

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC5",ylab="PC6",pch=20,col=cols[X12$sectors],main="Bi Plot of PC5 and PC6's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 7,8
n1=7
n2=8

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC7",ylab="PC8",pch=20,col=cols[X12$sectors],main="Bi Plot of PC7 and PC8's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 9,10
n1=9
n2=10

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC9",ylab="PC10",pch=20,col=cols[X12$sectors],main="Bi Plot of PC9 and PC10's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 11,12
n1=11
n2=12

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC11",ylab="PC12",pch=20,col=cols[X12$sectors],main="Bi Plot of PC11 and PC12's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 13,14
n1=13
n2=14

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC13",ylab="PC14",pch=20,col=cols[X12$sectors],main="Bi Plot of PC13 and PC14's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#2D scatterplot 15,16
n1=15
n2=16

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
X12<-cbind(x,y,sectors)
X12<-as.data.frame(X12)
color_pallete_function<-colorRampPalette(colors=rainbow(11),space="Lab")
num_colors<-nlevels(X12$sectors)
cols<-color_pallete_function(num_colors)
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
plot(x,y,xlab="PC15",ylab="PC16",pch=20,col=cols[X12$sectors],main="Bi Plot of PC15 and PC16's Weights ",xlim =c(xmin,xmax),ylim =c(ymin,ymax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=0.5)
text(x,y,label=tickers,cex=0.7)
grid()
abline(h=0,v=0)
arrows(0,0,x,y,length=0,col=cols[X12$sectors])

#3D scatterplot 1,2,3
n1=1
n2=2
n3=3

x<-pca.corr$loadings[,n1]
y<-pca.corr$loadings[,n2]
z<-pca.corr$loadings[,n3]
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
zmin=min(z)
zmax=max(z)
s3d=scatterplot3d(x,y,z,grid=T,type="h",xlab="PC1",ylab="PC2",zlab="PC3",color=cols[X12$sectors],main="3D Plot of Relative Weights of each Stock in PC1, PC2 and PC3",xlim =c(xmin,xmax),ylim =c(ymin,ymax),zlim=c(zmin,zmax))
legend(x ="topleft",legend=paste(levels(X12$sectors)),col=cols,pch=20,cex=.5)
s3d.coords=s3d$xyz.convert(x,y,z)
text(s3d.coords$x,s3d.coords$y,labels=tickers,cex=.8,pos=4)

#####create returns of each syntetic portfolios (PPs)#####
PPret<-data%*%pca.corr$loadings[]
dates<-index(data)
PPret<-xts(PPret,order.by=dates)

#add "PPi" names
names(PPret)<-c("PP1","PP2","PP3","PP4","PP5","PP6","PP7","PP8","PP9","PP10","PP11","PP12","PP13","PP14","PP15","PP16","PP17","PP18","PP19","PP20","PP21","PP22")

####descriptive statistics of selected principal portfolios (specify the number of PCs to be retained: n)####
n=15
palette<-distinctColorPalette(n)
chart.RiskReturnScatter(PPret,colorset=palette,Rf=.0,method="calc",main="Principal Portfolios Relative Risk and Return",
add.boxplots=TRUE,scale=scale,add.sharpe=c(1,2,3))
chart.RiskReturnScatter(PPret[,1:n],colorset=palette,Rf=.0,method="calc",main="Retained Principal Portfolios Relative 
Risk and Return",add.boxplots=TRUE,scale=scale,add.sharpe=c(1,2,3))
chart.Boxplot(PPret[,1:n],colorset=palette,legend="topleft",main="Boxplot of Retained Principal Portfolios Returns") 

#####performance of PPs#####
chart.CumReturns(PPret[,1:n],geometric=TRUE,colorset=palette,legend="topleft",main="Retained Principal Portfolios Cumulative Returns",
ylab="Cumulative return (1£)",begin="1",wealth.index=TRUE,cex.lab=TRUE)
chart.Drawdown(PPret[,1:n],colorset=palette,legend="topleft",ylab="Drawdown",main="Retained Principal Portfolios Drawdown Chart")

#####benchmarking with a 1/N optimisation from the original stock sample, rebalancing daily#####
#NOTE: the function Return.portfolio assumes as default geometric chaining 
Pr<-Return.portfolio(data,weights=NULL,geometric=TRUE,rebalance_on="days")
names(Pr)<-c("EqualWeighted")

####backtesting PPs against the 1/N optimisation#####
chart.RelativePerformance(PPret[,1:n],Pr,colorset=palette,legend="topleft",ylab="Cumulative return (1£)",main="Relative Performance
 of the Retained Principal Portfolios aginst the 1/N Portfolio")
#The PPs that exhibited a net positive absolute return (relative to the Benchmark) for the whole study period are PPn1, PPn2;
we now evaluate their performances only
n1=5
n2=6

newdata<-cbind(PPret[,n1],PPret[,n2])
chart.RelativePerformance(newdata,Pr,colorset=palette,legend="topleft",ylab="Cumulative return (1£)",main="Relative Performance of the 
PP5 and PP6 aginst the 1/N Portfolio")
newdataBench<-merge(newdata,Pr)
chart.CumReturns(newdataBench,colorset=palette,legend="topleft",main="PP5, PP6 and 1/N Portfolio Cumulative Returns",
ylab="Cumulative return (1£)",begin="1",wealth.index=TRUE,cex.lab=TRUE)
chart.Drawdown(newdataBench,colorset=palette, legend="topleft",ylab="Drawdown",main="PP5, PP6 and 1/N Portfolio Drawdown Chart")

#Table 5
#xtable(SharpeRatio.annualized(newdataBench))

#######END#######

#xtable(table.AnnualizedReturns(PPret))

chart.CumReturns(merge(dailyReturn(Ad(ARM.L)),dailyReturn(Ad(SSE.L))),colors=palette,main="Cumulative performance of stock 1 and 2",legend="topleft")

