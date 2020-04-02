################## 
### download data
# https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv
# https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# https://github.com/nytimes/covid-19-data/blob/master/us-states.csv
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv

### read table from online source
# url1<-"https://github.com/nytimes/covid-19-data/blob/master/us-states.csv"
# XML::readHTMLTable(RCurl::getURL(url1))[[1]]

### download raw data
# url2<-'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
# download.file(url2,'us-states.csv')


### substract US in March,2020
# us <- read.csv("us-states.csv", header=TRUE, stringsAsFactors=FALSE)
# us<-subset(us,date>='2020-02-29' & date<='2020-03-31')
# us_cases <- tapply(us$cases, us$date, sum)
# us_new <- us_cases[-1] - us_cases[-length(us_cases)]
# us_cases<-us_cases[-1]
# us_deaths <- tapply(us$deaths, us$date, sum)[-1]
# save(us,us_cases,us_new,us_deaths,file='us.rda')
############################################################################

plot_fit<-function(x,y,a,b,log){
    options(scipen = 20)
    par(mar=c(4.5,5,1,1))
    plot(x,a*b^x,type='b',col='blue',lwd=2,xlab='Day in March',
         yaxt='n',xaxt='n',main=NA,cex.lab=1.2,ylab=NA,bty="l",log=log)
    mtext('Count',side=1,at=0,padj=1,adj=1,cex=1.2)
    if (log=="y"){
        breaks<-axTicks(2,nintLog=10)
    }else{
        breaks<-pretty(range(1,a*b^max(x)),10)
    }
    breaks[breaks==0]<-1
    axis(2,breaks,format(breaks,big.mark = ','),las=1)
    abline(h=breaks,lty=2,col='gray')
    axis(1,x,labels = F)
    axis(1,c(1,seq(5,max(x),by=5)),lwd.ticks = 2,col.ticks = 'darkblue')
    legend(max(x)*0.05,a*b^max(x)*0.9,c("Predicted","Actual"),lty=1,pch=1,
           col=c(4,2),lwd=2,bty ='n',cex=1.2)
    text(max(x)*0.75,a*b^max(x)*0.98,cex=2,col=4,xpd=T,
         labels = substitute(italic(y==a%*%b^x),list(a=round(a,2),b=round(b,2))))
    points(1:length(y),y,type='l',col='red',lwd=2)
    points(1:length(y),y,col='red',lty=2,lwd=2)
    text(x=c(2,10,19,27),y=y[c(2,10,19,27)],y[c(2,10,19,27)],
         col='red',adj=c(0.6,-0.8),cex=1.2)
}

urls<-c("https://www.usnews.com/news/health-news/articles/2020-03-03/more-than-100-coronavirus-cases-6-deaths-reported-in-us",
        "https://www.bbc.com/news/world-us-canada-51882381",
        "https://www.cnbc.com/2020/03/19/us-coronavirus-cases-surpass-10000-doubling-in-two-days.html",
        "https://www.usatoday.com/story/news/health/2020/03/27/coronavirus-us-hits-100-000-confirmed-cases-1-500-deaths/2925968001/"
)

links<-c("More Than 100 Coronavirus Cases, 9 Deaths Reported in U.S.",
         "Trump declares national emergency",
         "US coronavirus cases surpass 10,000, doubling in two days",
         "US hits to 100,000 confirmed cases, 1500 deaths"
)
