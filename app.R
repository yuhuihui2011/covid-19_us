if (!require('shiny')) install.packages("shiny")
library(shiny)

urls<-c("https://www.usnews.com/news/health-news/articles/2020-03-03/more-than-100-coronavirus-cases-6-deaths-reported-in-us",
        "https://www.bbc.com/news/world-us-canada-51882381",
        "https://www.cnbc.com/2020/03/19/us-coronavirus-cases-surpass-10000-doubling-in-two-days.html",
        "https://www.usatoday.com/story/news/health/2020/03/27/coronavirus-us-hits-100-000-confirmed-cases-1-500-deaths/2925968001/"
)

links<-c("More Than 100 Coronavirus Cases...",
         "Trump declares national emergency...",
         "US coronavirus cases surpass 10,000...",
         "US hits to 100,000 confirmed cases..."
)

ui <- fluidPage(
    includeCSS('styles.css'),
    h1(style = "background: url('coronavirus-1.jpg') top;",
        "COVID-19 in the U.S. (March, 2020)"
    ),
    fluidRow(
        div(style='width:40%;float:left;padding-left:20px',align='left',
               radioButtons('type',h6(""),
                            list("Cases","New","Deaths"),
                            inline = T,selected = 'Cases'
                )
        ),
        div(style='width:60%;float:right;padding-right:20px',align='right',
               h5(a(href="https://github.com/nytimes/covid-19-data",
                    'Data source')
               ),
               h5("by:",
                  a(href="https://github.com/yuhuihui2011/covid-19_us",
                    'Huihui Yu')
               )
        )
    ),
    
    fluidRow(
        wellPanel(align="center",
            style = "background: url('coronavirus-2.jpg') center;height:80%",
            tabsetPanel(id='tabset',#type='pills',
                        tabPanel(("Linear value"),
                                 plotOutput('plot1', hover = hoverOpts("hover1",delay=100) )
                        ),
                        tabPanel(("Log10 scale"),
                                 plotOutput('plot2', hover = hoverOpts("hover2",delay=100) )
                        )
            )
        ),
        div(style="width:50%;float:left",align='right',
               tableOutput("vip")
        ),
        div(style="width:50%;float:right",align='left',
               h4("Note",style='padding-left:20px'),
                hr(style="border-bottom: 2px solid lightgray"),
               a(href=urls[1],links[1]),hr(),
               a(href=urls[2],links[2]),hr(),
               a(href=urls[3],links[3]),hr(),
               a(href=urls[4],links[4])
        )
    )
)

server <- function(input, output, session){
    source('plot_fit.R')
    load('us.rda')
    fit<-reactive({
        y<-switch(input$type,
                  'Cases' = us_cases,
                  'New' = us_new,
                  'Deaths' = us_deaths
        )
        x<-1:length(y)
        fit<-summary(lm(log10(y)~x))
        a<-10^fit$coef[1,1]
        b<-10^fit$coef[2,1]
        list(x=x,y=y,a=a,b=b)
    })
    output$plot1<-renderPlot({
        x<-fit()$x;y<-fit()$y;a<-fit()$a;b<-fit()$b
        plot_fit(x,y,a,b,log="")
    })
    observeEvent(input$hover1, {
        xx<-round(input$hover1$x)
        xx<-ifelse(xx<=1,1,ifelse(xx>=31,31,xx))
        output$plot1<-renderPlot({
            x<-fit()$x;y<-fit()$y;a<-fit()$a;b<-fit()$b
            plot_fit(x,y,a,b,log="")
            abline(v=xx,col="black")
            values<-c(round(a*b^(xx)),y[xx])
            points(c(xx,xx),values,col=c(4,2),pch=16,cex=2)
            values<-format(values,digits = 0,big.mark = ',')
            legend(xx+ifelse(xx<25,0.5,-0.5),a*b^(xx),values,pt.cex=2,
                   title = paste(3,xx,sep='/'), title.adj = 0.2,
                   xjust = ifelse(xx<25,0,1), yjust = ifelse(xx<28,-0.2,1.2),
                   xpd=T,pch=16,bg='lightblue',col=c('blue','red'),cex=1.2
            )
        })
    })
    output$plot2<-renderPlot({
        x<-fit()$x;y<-fit()$y;a<-fit()$a;b<-fit()$b
        plot_fit(x,y,a,b,log='y')
    })
    observeEvent(input$hover2, {
        xx<-round(input$hover2$x)
        xx<-ifelse(xx<=1,1,ifelse(xx>=31,31,xx))
        output$plot2<-renderPlot({
            x<-fit()$x;y<-fit()$y;a<-fit()$a;b<-fit()$b
            plot_fit(x,y,a,b,log='y')
            abline(v=xx,col="black")
            values<-c(round(a*b^(xx)),y[xx])
            points(c(xx,xx),values,col=c(4,2),pch=16,cex=2)
            values<-format(values,digits = 0,big.mark = ',')
            legend(xx+ifelse(xx<25,0.5,-0.5),a*b^(xx),values,
                   title = paste(xx,3,sep='/'), title.adj = 0.2,pt.cex = 2,
                   xjust = ifelse(xx<25,0,1), yjust = ifelse(xx<28,-0.2,1.2),
                   xpd=T,pch=16,bg='lightblue',col=c('blue','red'),cex=1.2
            )
        })
    })
    output$vip<-renderTable(hover = T,rownames = T, {
        days<-c(2,13,19,27)
        vip<-data.frame(
            Date=sprintf("2020-03-%02d",days),
            Cases=us_cases[days],
            New=us_new[days],
            Deaths=us_deaths[days]
        )
        rownames(vip)<-1:nrow(vip)
        xtable::xtable(vip)
    })
    if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    }
}

shinyApp(ui, server)
