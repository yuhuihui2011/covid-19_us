library(shiny)
source('plot_fit.R')
load('us.rda')

ui <- fluidPage(
    h1("COVID-19 in the U.S. (March, 2020)",
       style = "text-align: center; color: #fff;
                font-family: 'Source Sans Pro';
                background-image: url('coronavirus-1.jpg');
                padding: 20px;"
    ),
    fluidRow(style='padding: 0px',
        column(4,
               radioButtons('type',h6(""),
                            list("Cases","New","Deaths"),
                            inline = T,selected = 'Cases'
                )
        ),
        column(8,align='right',
               h5("Data source: ",
                  a(href="https://github.com/nytimes/covid-19-data",
                        'https://github.com/nytimes/covid-19-data')
               ),
               h5("by: ",
                  a(href="https://github.com/yuhuihui2011/",
                    'Huihui Yu')
               )
        )
    ),
    
    fluidRow(
        wellPanel(align="center",
            tags$style("li a{background-color: lightgray;}"),
            style = "text-align: center;background-image: url('coronavirus-2.jpg');
                padding: 20px",
            tabsetPanel(id='tabset',#type='pills',
                tabPanel(("Linear Value"),
                    plotOutput('plot1', hover = hoverOpts("hover1",delay=100) )
                ),
                tabPanel(("Log10 Scale"),
                    plotOutput('plot2', hover = hoverOpts("hover2",delay=100) )
                )
            )
        ),
        #h5('Important days',style='padding: 0px',align='center'),
        column(5,align='right',
               tableOutput("vip")
        ),
        column(7,align='left',
               h4("Note"),
               a(href=urls[1],links[1]),br(),h6(),
               a(href=urls[2],links[2]),br(),h6(),
               a(href=urls[3],links[3]),br(),h6(),
               a(href=urls[4],links[4])
        )
    )
)

server <- function(input, output, session){
    fit<-eventReactive(input$type,{
        if (input$type=='Cases') {
            y<-us_cases
        }else if (input$type=='New') {
            y<-us_new
        }else {
            y<-us_deaths
        }
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
                   title = paste(xx,3,sep='/'), title.adj = 0.2,
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
        vip<-data.frame(Date=paste(2020,3,days,sep='-'),
            Cases=us_cases[days],
            New=us_new[days],
            Death=us_deaths[days]
        )
        rownames(vip)<-1:nrow(vip)
        xtable::xtable(vip)
    })
}

shinyApp(ui, server)
