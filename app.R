library(tidyverse)
library(lubridate)
library(DBI)
library(rJava)
library(DT)
library(RPresto)
library(dbplyr)
library(pool)
library(shinydashboard)
library(shiny)
library(shinymanager)
library(purrr)
library(rlang)
library(r2d3)
library(config)
library(plotly)
library(daterangepicker)
library(RMySQL)
library(data.table)


dtt<- seq(ymd('2021-08-31'), ymd('2021-12-31'), "day")

client <-list(Amazon = c("AMAZON B2B FRACS","AMAZONINDIA","AMAZONCRETURNS"),
              Flipkart = c("Flipkart", "FLIPKART - E2E SURFACE", "FLIPKART E2E","FLIPKART SURFACE",
                           "FLIPKART - E2E FOOD","Flipkart Heavy"),
              Meesho = c("FTPL SURFACE","FTPL","FTPL SB","POPSHOP EXPRESS","POPSHOP SB","POPSHOP SURFACE"),
              Myntra = c("Myntra"),
              Nykaa = c("NYKAA EXPRESS", "Nykaa E Retail Surface", "Nykaa E Retail B2B","NYKAA1 SURFACE",
                        "NYKAA2 SURFACE","NYKAAFASHION B2B"),
              Reliance=c("AJIO","AJIO SURFACE","AJIOFMB2C SURFACE","AJIOWO SURFACE",
                         "QWIK SUPPLY CHAIN F&L B2B FM","QWIKSUPPLY SURFACE","FYND SURFACE",
                         "FYNDNDD SURFACE","FYND1 SURFACE","FYND – NDD","HAMLEYS EXPRESS",
                         "MARKS EXPRESS","STEVEMADDEN EXPRESS","SATYA EXPRESS","FCPL Digital",
                         "JIO","RELIANCEBRANDS EXPRESS","NETMEDS","JIOMART EXPRESS",
                         "FCPLDIGITALHEAVY SURFACE","FCPLDIGITAL HEAVY","FCPL DX MINI REVERSE B2B",
                         "FCPL DX MINI B2B","AJIOFM B2B"),
              Snapdeal = c("SNAPDEAL SURFACE", "SNAPDEALRL SURFACE"),
              UrbanLadder= c('URBANLADDER B2B'),
              Zivame = c("Zivame","ZIVAME SURFACE")
)

p_type<- list(COD = c("COD"),
              PrePaid = c("Pre-paid"),
              Both = c("COD","Pre-paid"))
mode<- list(E = "E",
            S = "S",
            Both = c("E","S"))
regn<- list(North = "North",
            South = "South",
            East = "East",
            West = "West",
            All = c("North","South","East","West"))
freq<- list(Daily = "Dt",
            Weekly = "week")


rds <- dbPool(drv=RMySQL::MySQL(), 
              host     = "business-analytics-database.ceypiyhweprx.us-east-1.rds.amazonaws.com",
              username = "admin", 
              password = "arJR68Q3xU5V", 
              port     = 3306,
              dbname = "business_analytics",
              idleTimeout = 360)



ui <- dashboardPage(
  dashboardHeader(title = "Strategic Clients Dashbaord"),
  dashboardSidebar(sidebarMenu(
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 95vh; overflow-y: auto;
                    }"
      ) # close HTML       
      )            # close tags$style
    ),
    
    menuItem("Speed", tabName = "speed" ,icon = icon("th"),badgeLabel = "1",badgeColor = "green"),    
    menuItem("FAD/FDDS", tabName = "fad",icon = icon("th"),badgeLabel = "2",badgeColor = "green"),
    menuItem("Breach", tabName = "brch",icon = icon("th"),badgeLabel = "3",badgeColor = "green"),
    menuItem("Manifest & Pickup", tabName = "pikup",icon = icon("th"),badgeLabel = "4",badgeColor = "green"),
    menuItem("DB Filters",icon = icon("filter"),startExpanded = TRUE,
             
             selectInput(
               inputId = "client",
               label = "Client:",
               choices = names(client),
               size = 8,
               selected = "Nykaa",
               selectize = FALSE
             ),
             dateRangeInput(inputId = "dateRange", 
                            label = "Date Select", 
                            start=today()-10, end=today()-1,
                            min = min(dtt),separator = "-"
             ),
             
             actionButton(inputId = "btn",label = "Apply Filter")),
    
    menuItem("UI Filters",icon = icon("filter"),startExpanded = TRUE,
             
             selectInput("period","Frequency",choices = names(freq)),
             
             selectizeInput(
               inputId = "rgn", 
               label = "Select a region", 
               choices = names(regn), 
               selected = "All"
             ),
             
             selectizeInput(
               inputId = "pt",
               label = "Pt:",
               choices = names(p_type),
               size = 3,
               selected = "Both"
             ),
             selectizeInput(
               inputId = "mot",
               label = "MOT:",
               choices = names(mode),
               size = 3,
               selected = "Both"
             ))
    
  )),
  dashboardBody(
    tags$head(
      tags$style(HTML(".sidebar {
                      position: fixed; overflow-y: auto;
                      width: 230px;
                    }"
      ) # close HTML       
      )            # close tags$style
    ),
    
    tabItems(
      tabItem(tabName = "speed",
              fluidRow(infoBoxOutput("del"),infoBoxOutput("avg_s2d"),infoBoxOutput("avg_s2dc")),
              fluidRow(plotlyOutput("speed")),br(),
              fluidRow(dataTableOutput("rs2d")),br(),
              fluidRow(
                tabBox(title = "DC wise FAD/FDDS",selected = "Graph",width = 12,
                       tabPanel("Graph",plotlyOutput("speed2")),
                       tabPanel("Table",dataTableOutput("speed3"))
                ))),
      
      tabItem(tabName = "fad",
              fluidRow(
                infoBoxOutput("o_ofd"),infoBoxOutput("o_fad"),infoBoxOutput("p_fad")),
              fluidRow(plotlyOutput("fad1")),br(),
              fluidRow(dataTableOutput("fad2")),br(),
              fluidRow(plotlyOutput("fad3")),br(),
              fluidRow(
                tabBox(title = "DC wise FAD/FDDS",selected = "Graph",width = 12,
                       tabPanel("Graph",plotlyOutput("fad4",height = 600)),
                       tabPanel("Table",dataTableOutput("fad5"))))
      ),
      tabItem(tabName = "pikup",
              fluidRow(infoBoxOutput("o_man"),infoBoxOutput("o_pik")),
              fluidRow(plotlyOutput("mfest")),br(),
              fluidRow(plotlyOutput("mfest1")),br(),
              fluidRow(plotlyOutput("mfest2")),br()
      )
      
    )
  )
)

server <- function(input, output,session) {
  
  # output$freq<- renderUI({
  #   selectInput("period","Frequency",choices = if (length(seq(input$dateRange[1], input$dateRange[2], "day"))<14) {c("Daily")} else{c("Daily","Weekly")})
  # })
  
  # frequency <- reactive({
  #   ifelse(input$period=="Daily", "Dt",
  #          ifelse(input$period=="Weekly", "week",
  #                 ifelse(input$period=="Monthly", "month",
  #                        )))
  # })
  
  base_data<-eventReactive(input$btn,{
    
    a<-rds%>%tbl("s_analytics")%>%
      filter(cl %in% !!client[[input$client]] & pt %in% !!p_type[["Both"]] & mot %in% !!mode[["Both"]] & Dt %in% !!seq(input$dateRange[1], input$dateRange[2], "day"))%>%
      select(-CRD_NonOTP,-CRD_OTP,-Others,-CNA,-CRR,-year,-month,-ANF,-ODA,-Self_Collect,-Bulkout)%>%collect()
    setDT(a)
    a$Dt<- ymd(a$Dt)
    a<- left_join(a,rds%>%tbl("facility")%>%select(name,city,region,state)%>%collect(),by = c("cn"="name"))
    a[cn=="NSZ",region:="North"]
    
  },ignoreInit = FALSE,ignoreNULL = FALSE)
  
  
  base<- reactive({base_data()%>%filter(pt %in% !!p_type[[input$pt]] & mot %in% !!mode[[input$mot]] & region %in% !!regn[[input$rgn]])})
  
  
  
  output$del<- renderInfoBox({
    base()%>%summarise(sum(delivered,na.rm = TRUE))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "Delivered",icon = icon("gift"),color = "red",fill=FALSE)
    
  })
  
  output$o_ofd<- renderInfoBox({
    base()%>%summarise(sum(OFD,na.rm = TRUE))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "OFD",icon = icon("gift"))
    
  })
  
  output$o_fad<- renderInfoBox({
    base()%>%summarise(sum(FAD,na.rm = TRUE))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "FAD#",icon = icon("list"),color = "purple")
    
  })
  
  output$p_fad<- renderInfoBox({
    
    x<-base()%>%summarise((sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)
    ic<- 'thumbs-down'
    clr<- 'yellow'
    if(x > 90) {
      ic <- 'thumbs-up'
      clr<- 'green'}
    infoBox(value = x%>%round(digits = 2) %>% paste0("%") ,title = "FAD%",icon = icon(ic),color = clr,fill=FALSE)
    
  })
  
  
  
  output$avg_s2d<- renderInfoBox({
    base()%>%summarise(round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pull() %>% round(digits = 2)%>%
      infoBox(title = "S2D",icon = icon("dashboard"),color = "red",fill=FALSE)
    
  })
  
  output$avg_s2dc<- renderInfoBox({
    base()%>%summarise(round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pull() %>% round(digits = 2)%>%
      infoBox(title = "S2DC",icon = icon("dashboard"),color = "red",fill=FALSE)
    
  })
  
  output$o_man<- renderInfoBox({
    base()%>%summarise(sum(manifested,na.rm = TRUE))%>%
      pull() %>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Manifested",icon = icon("list"),color = "orange",fill=FALSE)
    
  })
  
  output$o_pik<- renderInfoBox({
    base()%>%summarise(sum(pickup,na.rm = TRUE))%>%
      pull() %>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Pick-Up",icon = icon("list"),color = "purple",fill=FALSE)
    
  })
  
  
  
  output$speed <- renderPlotly({
    sp<- base()%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(Del = sum(delivered,na.rm = TRUE),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2),S2DC = round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2),S2Pro = round(sum((S2Pro*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))
    plot_ly(data = sp,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~Del,type = "bar", name = "Delivered",textposition = 'outside',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)')))%>%
      add_trace(y = ~S2D,mode = "lines+text", type='scatter', yaxis = "y2", name = "Avg S2D",text=~round(S2D,2),textposition= "top center") %>%
      add_trace(y = ~S2DC,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2DC") %>%
      add_trace(y = ~S2Pro,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2Pro") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$rs2d <- renderDataTable(
    base()%>%
      group_by(!!sym(freq[[input$period]]),region)%>%
      summarise(S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pivot_wider(id_cols = region,names_from = !!sym(freq[[input$period]]),values_from = S2D),options = list(scrollX = T)
    
  )
  
  l<- reactive({base()%>%
      group_by(Dt,oc,cn)%>%summarise(Del=sum(delivered,na.rm = TRUE),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      filter(oc!="" &  !is.nan(S2D))%>%arrange(Dt,desc(Del))})
  
  
  output$speed2 <- renderPlotly({
    l1<-l()%>%filter(Del>=5)%>%group_by(Dt)%>%mutate(rank = row_number(desc(Del)))%>%filter(rank<=20)%>%mutate(ln= paste(oc,"-",cn))%>%
      pivot_wider(id_cols = (ln),names_from = Dt,values_from = S2D)%>%
      mutate(cnt = rowSums(!is.na(select(., -ln))))%>%arrange((cnt))
    
    l2<- as.matrix(l1[,c(-1,-ncol(l1))])
    # k <- which(is.na(f2), arr.ind=TRUE)
    # f2[k] <- rowMeans(f2, na.rm=TRUE)[k[,1]]
    dimnames(l2)[1]<- list((l1$ln))
    plot_ly(x=dimnames(l2)[[2]],y=dimnames(l2)[[1]],z = l2, type = "heatmap",colors = colorRamp(c("blue","yellow", "red")))%>%
      layout(yaxis = list(title = '<b>Lane</b>'),xaxis = list(title = "Day"),title="<b>S2D by Lane</b>")
    
  })
  
  output$speed3<- renderDataTable(
    l()%>%filter(Del>=5),server = FALSE,filter = 'top',rownames=FALSE,
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      autoWidth = TRUE,
      fixedColumns = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    )
  )
  
  
  output$fad1 <- renderPlotly({
    fd<- base()%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(OFD = sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)
    plot_ly(data = fd,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~OFD,type = "bar", name = "OFD",textposition = 'auto',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)'))) %>%
      add_trace(y = ~FAD,mode = "lines+text", type='scatter', yaxis = "y2", name = "FAD%",text=~round(FAD,2),textposition= "auto")%>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$fad2 <- renderDataTable(
    base()%>%group_by(!!sym(freq[[input$period]]),region)%>%
      summarise(FAD = round((sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100,2))%>%
      pivot_wider(id_cols = region,names_from = !!sym(freq[[input$period]]),values_from = FAD), options = list(scrollX = T)
  )
  
  output$fad3 <- renderPlotly({
    plot_ly(base()%>%group_by(Dt,wday)%>%summarise(FAD = (sum(FAD,na.rm = TRUE)),OFD=sum(OFD,na.rm = TRUE)),x = ~wday) %>%
      add_trace(y = ~OFD, type='bar',name = 'Out for delivery',textposition = 'outside',texttemplate="%{y:.2s}")%>%
      add_trace(y = ~FAD,name = 'FAD',type='bar',textposition = 'outside',texttemplate="%{y:.2s}")%>%
      layout(yaxis = list(title = 'Count'),xaxis = list(title = "Day") ,barmode = 'group')
  })
  
  
  f<-reactive({base()%>%
      group_by(Dt,cn)%>%summarise(OFD=sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
      arrange(Dt,cn,desc(OFD))})
  
  
  output$fad4 <- renderPlotly({
    f1<-f()%>%filter(OFD>=5)%>%group_by(Dt)%>%mutate(rank = row_number(desc(OFD)))%>%filter(rank<=40)%>%select(cn,Dt,FAD)%>%
      pivot_wider(id_cols = (cn),names_from = Dt,values_from = FAD)%>%
      mutate(total = sum(c_across(where(is.numeric)),na.rm = TRUE),cnt = rowSums(!is.na(select(., -cn))))%>%arrange(cnt,desc(total))
    
    f2<- as.matrix(f1[,c(-1,-(ncol(f1)-1),-ncol(f1))])
    # k <- which(is.na(f2), arr.ind=TRUE)
    # f2[k] <- rowMeans(f2, na.rm=TRUE)[k[,1]]
    dimnames(f2)[1]<- list((f1$cn))
    plot_ly(x=dimnames(f2)[[2]],y=dimnames(f2)[[1]],z = f2, type = "heatmap",colors = colorRamp(c("red","yellow" ,"blue")),height = 600)%>%
      layout(yaxis = list(title = '<b>Dispatch Center</b>'),xaxis = list(title = "Day"),title="<b>FAD by DC</b>")
    
  })
  
  output$fad5<- renderDataTable(
    f()%>%filter(OFD>=5)%>%mutate(FAD=round(FAD,2)),server = FALSE,filter = 'top',rownames=FALSE,
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    )
  )
  
  output$mfest<- renderPlotly({
    plot_ly(data = base()%>%group_by(!!sym(freq[[input$period]]))%>%
              summarise(manifest = sum(manifested,na.rm = TRUE),pickup = sum(pickup,na.rm = TRUE)),x = as.formula(paste0("~", freq[[input$period]])))%>%
      add_trace(y = ~manifest,mode = "lines+text", type='scatter', name = "manifest",text=~manifest,textposition= "auto",texttemplate="%{y:.2s}")%>%
      add_trace(y = ~pickup,mode = "lines+text", type='scatter', name = "pickup",text=~pickup,textposition= "auto",texttemplate="%{y:.2s}")%>%
      layout(yaxis = list(title = '<b>Count</b>'))
    
  })
  
  
  output$mfest1<- renderPlotly({
    plot_ly(data = base()%>%group_by(wday)%>%
              summarise(manifest = sum(manifested,na.rm = TRUE),pickup = sum(pickup,na.rm = TRUE)),x = ~wday)%>%
      add_trace(y = ~manifest, type='bar',name = 'Manifest',textposition = 'outside',texttemplate="%{y:.2s}")%>%
      add_trace(y = ~pickup,name = 'Pick-up',type='bar',textposition = 'outside',texttemplate="%{y:.2s}")%>%
      layout(yaxis = list(title = 'Count'),xaxis = list(title = "Period") ,barmode = 'group')
    
  })
  
}

shinyApp(ui = ui, server = server)

