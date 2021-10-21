credentials <- data.frame(
  user = c("delhivery"), # mandatory
  password = c("delhivery@strategic"), # mandatory
  start = c("2021-09-01"), # optional (all others)
  expire = c("2021-12-31"),
  admin = c(FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


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
library(RMySQL)
library(data.table)
library(stringr)
library(shinycssloaders)
library(shinyjs)
library(RcppRoll)


# Initial list ------------------------------------------------------------

dtt<- seq(ymd('2021-08-31'), ymd('2021-12-31'), "day")
#dtt1<- seq(today()-15,today()-1,"day")

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
              Pickup = c("Pickup"),
              REPL = c("REPL"),
              All = c("COD","Pre-paid","Pickup","REPL"))

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

zone <- list(A="A",B=c("B","B_Shared","B_SPL"),C=c("C","C1","C2","C_SPL"),
             D=c("D","D1","D2","DMY","D_Shared","D_SPL"),
             E="E",F="F",International="INTL",
             
             All=c("A","B","B_Shared","B_SPL","C","C1","C2","C_SPL","D","D1","D2","D_Shared","D_SPL","DMY","E","F","INTL"))

vol<- list("Top 25" = 25,
           "Top 50" = 50,
           "Top 100" = 100,
           "Top 200" = 200,
           "Top 400" = 400)
 



rds <- dbPool(drv=RMySQL::MySQL(), 
              host     = "business-analytics-database.ceypiyhweprx.us-east-1.rds.amazonaws.com",
              username = "admin", 
              password = "arJR68Q3xU5V", 
              port     = 3306,
              dbname = "business_analytics",
              idleTimeout = 360)


# UI Section --------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Strategic Clients Dashbaord"),
  dashboardSidebar(sidebarMenu(id = "sb",
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 95vh; overflow-y: auto;
                    }"
      )        
      )            
    ),
    
    menuItem("Speed", tabName = "speed" ,icon = icon("th")),    
    menuItem("FAD/FDDS", tabName = "fad",icon = icon("th")),
    menuItem("Breach", tabName = "brch",icon = icon("th")),
    menuItem("Manifest & Pickup", tabName = "pikup",icon = icon("th")),br(),
    menuItem("Filters",icon = icon("filter"),startExpanded = TRUE,
             
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
                            start=today()-8, end=today()-1,
                            min = min(dtt),separator = "-"
             ),
             
             actionButton(inputId = "btn",label = "Apply Filter"))
    
   
  )),
  

# UI Body -----------------------------------------------------------------

  
  dashboardBody(shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(".sidebar {
                      position: fixed; overflow-y: auto;
                      width: 230px;
                    }"
      )       
      )           
    ),
    
    fluidRow(column(2,selectInput("period","Frequency",choices = names(freq))),
             div(id="dr", column(2,selectInput(
      inputId = "rgn", 
      label = "Destination region", 
      choices = names(regn), 
      selected = "All"
    ))),
    shinyjs::hidden(div(id="or",column(2,selectInput(
      inputId = "rgn1", 
      label = "Origin region", 
      choices = names(regn), 
      selected = "All"
    )))),
    column(2,selectizeInput(
      inputId = "pt",
      label = "Payment type",
      choices = names(p_type),
      size = 5,
      selected = "All"
    )),
    column(2,selectizeInput(
      inputId = "mot",
      label = "Mode of transport",
      choices = names(mode),
      size = 3,
      selected = "Both"
    )),
    column(2,selectInput("bzn","Billing Zone",choices = names(zone),selected = "All")),
    column(2,
           selectInput('clnt',"HQ Name",choices = as.list(c(client[["Nykaa"]],"All")),selected = 'All',multiple = TRUE)
           )
    ),
    tabItems(
      tabItem(tabName = "speed",withSpinner(tagList(
              fluidRow(infoBoxOutput("del",width = 3),infoBoxOutput("att",width = 3),infoBoxOutput("avg_s2d",width = 2),infoBoxOutput("avg_s2dc",width = 2),infoBoxOutput("avg_s2a",width = 2)),
              fluidRow(plotlyOutput("speed")),br(),
              fluidRow(plotlyOutput("speed6")),br(),
              uiOutput("speed4"),
              fluidRow(dataTableOutput("rs2d"))),color="#0dc5c1"),br(),
              fluidRow(
                tabBox(title = "DC wise FAD/FDDS",selected = "Graph",width = 12,
                       tabPanel("Graph",plotlyOutput("speed2")),
                       tabPanel("Table",dataTableOutput("speed3"))
                ))),
      
      tabItem(tabName = "fad",withSpinner(tagList(
              fluidRow(
                infoBoxOutput("o_ofd"),infoBoxOutput("o_fad"),infoBoxOutput("p_fad")),
              fluidRow(plotlyOutput("fad1")),br(),
              fluidRow(dataTableOutput("fad2"))),color="#0dc5c1"),br(),
              # fluidRow(plotlyOutput("fad3")),br(),
              uiOutput("fad3"),
              fluidRow(
                tabBox(title = "DC wise FAD/FDDS",selected = "Graph",width = 12,
                       tabPanel("Graph",plotlyOutput("fad4",height = 600)),
                       tabPanel("Table",dataTableOutput("fad5"))))
      ),
      
      tabItem(tabName="brch",
              withSpinner(tagList(
              fluidRow(infoBoxOutput("promised",width = 3),infoBoxOutput("breach",width = 3),infoBoxOutput("promisedundel",width = 3),infoBoxOutput("breachp",width = 3)),
              
              fluidRow(plotlyOutput("Breachplt")),br(),
                       uiOutput("brch1"),
                       fluidRow(dataTableOutput("brch2")),
                       uiOutput("brch3"),
                       fluidRow(plotlyOutput("mapbr", height=600))
                       
              ),color="#0dc5c1")),
      
      
      tabItem(tabName = "pikup",
              withSpinner(tagList(
              fluidRow(infoBoxOutput("o_man"),infoBoxOutput("o_pik")),
              fluidRow(plotlyOutput("mfest")),br(),
              fluidRow(plotlyOutput("mfest1")),br(),
              #uiOutput("mfest3"),
              #fluidRow(plotlyOutput("mfest2")),br(),
              fluidRow(column(3,uiOutput("p_day")),column(3,sliderInput("adays",label = "Days to compare",min = 2,max = 5,step = 1,value = 3)),
                       column(3,selectInput("pvol","Volume Filter",choices = names(vol),selected = "Top 50"))),
              fluidRow(plotlyOutput("mfest4",height = 600))),color="#0dc5c1")
      )
      
    )
  )
)
ui <- secure_app(ui)

server <- function(input, output,session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
 
  
  base_data<-eventReactive(input$btn,{shinyjs::disable("btn")
    
    a<-rds%>%tbl("s_analytics")%>%
      filter(cl %in% !!client[[input$client]] & pt %in% !!p_type[["All"]] & mot %in% !!mode[["Both"]] & Dt %in% !!seq(input$dateRange[1]-5, input$dateRange[2], "day"))%>%
      select(-CRD_NonOTP,-CRD_OTP,-Others,-CNA,-CRR,-year,-month,-ANF,-ODA,sh_opt,-Bulkout,-pdt)%>%collect()
    setDT(a)
    
    a$Dt<- ymd(a$Dt)
    a<- left_join(a,rds%>%tbl("facility")%>%select(name,city,region,state,lat,lon)%>%collect(),by = c("cn"="name"))
    a<- left_join(a,rds%>%tbl("facility")%>%select(name,city,region,state,lat,lon)%>%collect(),by = c("oc"="name"))
    
    a[cn=="NSZ",region.x:="North"]
    a[is.na(region.x) & str_detect(cn,"Rajasthan|Punjab|Ladakh|Haryana|Uttar Pradesh|Chandigarh"),region.x:="North"]
    a[is.na(region.x) & str_detect(cn,"Maharashtra|Madhya Pradesh"),region.x:="West"]
    a[is.na(region.x) & str_detect(cn,"Kerala|Tamil Nadu|Karnataka|Telangana|Andhra Pradesh"),region.x:="South"]
    a[is.na(region.x) & str_detect(cn,"Manipur|West Bengal|Orissa"),region.x:="East"]
    
    # b<-rds%>%tbl("s_analytics")%>%
    #   filter(cl %in% !!client[[input$client]] & pt %in% !!p_type[["All"]] & mot %in% !!mode[["Both"]] & Dt %in% !!seq(input$dateRange[1]-7, input$dateRange[2], "day"))%>%
    #   group_by(Dt,week)%>%summarise(pickup = sum(pickup))%>%collect()
    # setDT(b)
    # 
    # b$Dt<- ymd(b$Dt)
    # c<- list(a=a,b=b)
    
  },ignoreInit = FALSE,ignoreNULL = FALSE)
  

  observeEvent(input$btn,{updateSelectInput(session,"clnt","HQ Name",choices = as.list(c(client[[input$client]],"All")),selected = 'All' )
    })
    

  base<- reactive({ 
      if (input$clnt == 'All') {
     a<- base_data()[Dt >= input$dateRange[1] & pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]] & region.y %in% regn[[input$rgn1]] & bzn %in% zone[[input$bzn]],]
     b<- base_data()[pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]] & region.y %in% regn[[input$rgn1]] & bzn %in% zone[[input$bzn]],]
     c<- list(a=a,b=b)
     }
  else{  
   a<-base_data()[Dt >= input$dateRange[1] & pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]] & region.y %in% regn[[input$rgn1]] & bzn %in% zone[[input$bzn]] & cl %in% input$clnt,]
   b<- base_data()[pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]] & region.y %in% regn[[input$rgn1]] & bzn %in% zone[[input$bzn]] & cl %in% input$clnt,] 
   c<- list(a=a,b=b)
   }
    })
  
  
  observeEvent({input$dateRange
                input$client},{shinyjs::enable("btn")})
 

  
  output$del<- renderInfoBox({ 
    x<- base()$a[!is.na(delivered),.(sum(delivered))]
    x%>%round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "Delivered",icon = icon("gift"),color = "blue",fill=FALSE)
    
  })
  
  output$att<- renderInfoBox({ 
    x<- base()$a[!is.na(attempted),.(sum(attempted))]
    x%>%round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "Attempted",icon = icon("exclamation-circle"),color = "blue",fill=FALSE)
    
  })
  
  # FAD Infobox ----------------------------------------------------------
  output$o_ofd<- renderInfoBox({
    x<- base()$a[!is.na(OFD),.(sum(OFD))]
    x%>%round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "OFD",icon = icon("gift"))
    
  })
  
  output$o_fad<- renderInfoBox({
    x<- base()$a[!is.na(OFD),.(sum(FAD,na.rm = TRUE))]
    x%>%round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "FAD#",icon = icon("list"),color = "purple")
    
  })
  
  output$p_fad<- renderInfoBox({
    
    x<-base()$a[!is.na(OFD),.((sum(FAD,na.rm = TRUE)/sum(OFD))*100)]
    ic<- 'thumbs-down'
    clr<- 'yellow'
    if(x > 90) {
      ic <- 'thumbs-up'
      clr<- 'green'}
    infoBox(value = x%>%round(digits = 2) %>% paste0("%") ,title = "FAD%",icon = icon(ic),color = clr,fill=FALSE)
    
  })
  
  
  
  output$avg_s2d<- renderInfoBox({
    x<- base()$a[!is.na(delivered),.(round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered),2))]
    x%>%round(digits = 2)%>%
      infoBox(title = "S2D",color = "purple",fill=FALSE)
    
  })
  
  output$avg_s2a<- renderInfoBox({
    x<- base()$a[!is.na(delivered),.(round(sum((S2A*attempted),na.rm = TRUE)/sum(delivered),2))]
    x%>%round(digits = 2)%>%
      infoBox(title = "S2A",color = "purple",fill=FALSE)
    
  })
  
  output$avg_s2dc<- renderInfoBox({
    x<- base()$a[!is.na(delivered),.(round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered),2))]
    x %>% round(digits = 2)%>%
      infoBox(title = "S2DC",color = "purple",fill=FALSE)
    
  })
  
  output$o_man<- renderInfoBox({
    x<- base()$a[!is.na(manifested),.(sum(manifested))]
    x%>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Manifested",icon = icon("list"),color = "orange",fill=FALSE)
    
  })
  
  output$o_pik<- renderInfoBox({
    x<- base()$a[!is.na(pickup),.(sum(pickup))]
    x%>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Pick-Up",icon = icon("list"),color = "purple",fill=FALSE)
    
  })
  
  # Promised Infobox --------------------------------------------------------
  
  
  output$promised<- renderInfoBox({
    x<- base()$a[!is.na(Total_Promised),.(sum(Total_Promised))]
      x %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "Promised",icon = icon("gift"))
    
  })
  
  # Breach Infobox ----------------------------------------------------------
  
  
  output$breach<- renderInfoBox({
    x<- base()$a[!is.na(Total_Promised),.(sum(PDD_Breach,na.rm = TRUE))]
     x%>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "PDD Breach",icon = icon("exclamation-circle"))
    
  })
  
  
  output$breachp<- renderInfoBox({
    
    pbp<-base()$a%>%summarise((sum(PDD_Breach,na.rm = TRUE)/sum(Total_Promised,na.rm = TRUE))*100)
    ic<- 'dashboard'
    
    infoBox(value = pbp%>%round(digits = 2) %>% paste0("%") ,title = "PDD Breach%",icon = icon(ic),color = 'orange',fill=FALSE)
    
  })
  
  
  output$promisedundel<- renderInfoBox({
    x<- base()$a[!is.na(Total_Promised),.(sum(Promised_undel,na.rm=TRUE))]
    x%>% round(digits = 1) %>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Promised Undelivered",icon = icon("gift"))
    
  })
  
  # Speed ----------------------------------------------------------
  
  output$speed <- renderPlotly({
     sp<- base()$a%>%filter(!is.na(delivered))%>%group_by(!!sym(freq[[input$period]]))%>%
       summarise(Del = sum(delivered),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered),2),S2DC = round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered),2),S2Pro = round(sum((S2Pro*delivered),na.rm = TRUE)/sum(delivered),2))
    
    
     plot_ly(data = sp,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~Del,type = "bar", name = "Delivered",textposition = 'outside',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)')))%>%
      add_trace(y = ~S2D,mode = "lines+text", type='scatter', yaxis = "y2", name = "Avg S2D",text=~round(S2D,2),textposition= "top center") %>%
      add_trace(y = ~S2DC,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2DC") %>%
      add_trace(y = ~S2Pro,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2Pro") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
     })
  
  output$speed6 <- renderPlotly({
    sp<- base()$a%>%filter(!is.na(attempted))%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(Attempted = sum(attempted),S2A = round(sum((S2A*attempted),na.rm = TRUE)/sum(attempted),2))
    
    
    plot_ly(data = sp,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~Attempted,type = "bar", name = "Attempted",textposition = 'outside',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)')))%>%
      add_trace(y = ~S2A,mode = "lines+text", type='scatter', yaxis = "y2", name = "Avg S2A",text=~round(S2A,2),textposition= "top center") %>%
      layout(title="Attempts",yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  
  
  output$speed4<- renderUI({selectInput('orgn',"Origin Region",choices = names(regn),selected = 'All')})
  
  output$rs2d <- renderDataTable(
    base()$a%>%filter(!is.na(delivered) & region.y %in% !!regn[[input$orgn]])%>%
      group_by(!!sym(freq[[input$period]]),region.x)%>%
      summarise(S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pivot_wider(id_cols = region.x,names_from = !!sym(freq[[input$period]]),values_from = S2D),options = list(scrollX = T)
    
  )
  
  l<- reactive({base()$a%>%filter(!is.na(delivered))%>%
      group_by(Dt,oc,cn)%>%summarise(Del=sum(delivered),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      filter(oc!="" &  !is.nan(S2D))%>%arrange(Dt,desc(Del))})
  
  
  output$speed2 <- renderPlotly({
    l1<-l()%>%filter(Del>=10)%>%group_by(Dt)%>%mutate(rank = row_number(desc(Del)))%>%filter(rank<=20)%>%mutate(ln= paste(oc,"-",cn))%>%
      pivot_wider(id_cols = (ln),names_from = Dt,values_from = S2D)%>%
      mutate(cnt = rowSums(!is.na(select(., -ln))))%>%arrange((cnt))
    
    l2<- as.matrix(l1[,c(-1,-ncol(l1))])

    dimnames(l2)[1]<- list((l1$ln))
    plot_ly(x=dimnames(l2)[[2]],y=dimnames(l2)[[1]],z = l2, type = "heatmap",colors = colorRamp(c("blue","yellow", "red")))%>%
      layout(yaxis = list(title = '<b>Lane</b>'),xaxis = list(title = "Day"),title="<b>S2D by Lane</b>")
    
  })
  
  output$speed3<- renderDataTable(
    l()%>%filter(Del>=10),server = FALSE,filter = 'top',rownames=FALSE,
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
  
  # FDDS ----------------------------------------------------------
  output$fad1 <- renderPlotly({
    fd<- base()$a%>%filter(!is.na(OFD))%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(OFD = sum(OFD),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)
    plot_ly(data = fd,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~OFD,type = "bar", name = "OFD",textposition = 'auto',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)'))) %>%
      add_trace(y = ~FAD,mode = "lines+text", type='scatter', yaxis = "y2", name = "FAD%",text=~round(FAD,2),textposition= "auto")%>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  
  
  output$fad2 <- renderDataTable(
    base()$a%>%filter(!is.na(OFD))%>%
      group_by(!!sym(freq[[input$period]]),region.x)%>%
      summarise(FAD = round((sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100,2))%>%
      pivot_wider(id_cols = region.x,names_from = !!sym(freq[[input$period]]),values_from = FAD), options = list(scrollX = T)
  )
  

  
  output$fad3<- renderUI({selectInput('st',"State",choices = as.list(c(unique(base()$a%>%select(state.x)),"All")),selected = 'All')})
  
  f<-reactive({req(input$st)
    if (input$st == 'All') {
      base()$a%>%group_by(Dt,cn)%>%
        summarise(OFD=sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
        arrange(Dt,cn,desc(OFD))  
    }
    else{
    base()$a%>%filter(state.x %in% (input$st))%>%
      group_by(Dt,cn)%>%summarise(OFD=sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
      arrange(Dt,cn,desc(OFD))}})
  
  
  
  output$fad4 <- renderPlotly({
    f1<-f()%>%filter(OFD>=10)%>%group_by(Dt)%>%mutate(rank = row_number(desc(OFD)))%>%filter(rank<=40)%>%select(cn,Dt,FAD)%>%
      pivot_wider(id_cols = (cn),names_from = Dt,values_from = FAD)%>%
      mutate(total = sum(c_across(where(is.numeric)),na.rm = TRUE),cnt = rowSums(!is.na(select(., -cn))))%>%arrange(cnt,desc(total))

    f2<- as.matrix(f1[,c(-1,-(ncol(f1)-1),-ncol(f1))])
    # k <- which(is.na(f2), arr.ind=TRUE)
    # f2[k] <- rowMeans(f2, na.rm=TRUE)[k[,1]]
    dimnames(f2)[1]<- list((f1$cn))
    plot_ly(x=dimnames(f2)[[2]],y=dimnames(f2)[[1]],z = f2, type = "heatmap",colors = colorRamp(c("red","yellow" ,"blue")),height = 600)%>%
      layout(yaxis = list(title = '<b>Dispatch Center</b>'),xaxis = list(title = "Day"),title="<b>FAD by DC</b>")

  })
  
  # output$fad4<- renderPlotly({
  #   fchng<- base()$b%>%filter(Dt>= today()-4 & Dt<= today()-2 & !is.na(OFD))%>%
  #     group_by(cn)%>%
  #     summarize(avg=(sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)
  #   
  #   fvol<- base()$b%>%filter(Dt== today()-1 & !is.na(OFD))%>%
  #     group_by(cn,lat.x,lon.x)%>%
  #     summarize(ofd=sum(OFD,na.rm = TRUE),fad=(sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
  #     filter(ofd>10)%>%
  #     left_join(fchng,by = "cn")%>%
  #     mutate(chng= round(((fad-avg)/avg)*100),1)%>%ungroup()%>%slice_max(order_by = ofd,n=100)
  #   
  #   fvol%>%plot_ly(lat = fvol$lat.x,
  #                  lon = fvol$lon.x,
  #                  size  = fvol$ofd,
  #                  color = fvol$chng,
  #                  height = 600,
  #                  hovertemplate = paste("DC :", fvol$cn,"<br> OFD :", fvol$ofd,"<br> FAD :", fvol$fad,"<br> FDDS% :",fvol$chng,"%"),
  #                  colors = c("red","springgreen","cyan","dark blue" ),
  #                  type = 'scattermapbox'
  #   )%>%layout(
  #     title = "FDDS Change by DC",
  #     mapbox = list(
  #       
  #       style = 'open-street-map',
  #       zoom =2,
  #       center = list(lon =85.15743, lat = 25.600421))
  #   )
  #   
  # })
  
  
  
  
  
  output$fad5<- renderDataTable(
    f()%>%filter(OFD>=10)%>%mutate(FAD=round(FAD,2)),server = FALSE,filter = 'top',rownames=FALSE,
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
  
  
  # Breach ----------------------------------------------------------
  
  output$Breachplt <- renderPlotly({
    sp<- base()$a%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(Promise = sum(Total_Promised,na.rm = TRUE),Promise_UD =sum(Promised_undel,na.rm = TRUE), Breach=round(sum(PDD_Breach,na.rm = TRUE)/sum(Total_Promised,na.rm = TRUE)*100,2),na.rm = TRUE)
    plot_ly(data = sp,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~Promise,type = "bar", name = "Promised",textposition = 'outside',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)'))) %>%
      add_trace(y = ~Breach,mode = "lines+text", type='scatter', yaxis = "y2", name = "Breach%",color='rgb(220,20,60)',text=~round(Breach,2),textposition= "top center") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  
  output$brch1<- renderUI({selectInput('borgn',"Origin Region",choices = names(regn),selected = 'All')})
  
  output$brch2 <- renderDataTable(
    base()$a%>%filter(region.y %in% !!regn[[req(input$borgn)]])%>%
      group_by(!!sym(freq[[input$period]]),region.x)%>%
      summarise(Breach = round((sum(PDD_Breach,na.rm = TRUE)/sum(Total_Promised,na.rm = TRUE))*100,2))%>%
      pivot_wider(id_cols = region.x,names_from = !!sym(freq[[input$period]]),values_from =Breach),options = list(scrollX = T)
    
  )
  
  
  
  output$brch3 <- renderUI({ dateRangeInput(inputId = "dr2", 
                                             label = "Date Range", 
                                             start=input$dateRange[1], end=input$dateRange[2],
                                             min = input$dateRange[1],max = input$dateRange[2],
                                             separator = "-"
  )})

  # Map Breach --------------------------------------------------------------
  output$mapbr<- renderPlotly({req(input$dr2)
    mp<-base()$a%>%filter(Dt %in% seq(input$dr2[1], input$dr2[2], "day"))%>%group_by(cn,lat.x,lon.x)%>%
      summarize(Brh= round((sum(PDD_Breach,na.rm = TRUE)/sum(Total_Promised,na.rm = TRUE))*100,2),Prms=sum(Total_Promised,na.rm = TRUE))%>%
      ungroup%>%filter(Prms>=median(Prms))
    
    mp%>%plot_ly(lat = ~lat.x,
                 height=600,
                 lon = ~lon.x,
                 color = ~Brh,
                 size  = ~Prms+10,
                 hovertemplate = paste("DC :", mp$cn,"<br> Promised :", mp$Prms,"<br> Breach% :",paste0(mp$Brh,"%")),
                 colors = c("springgreen","palevioletred","sandybrown","orange", "orangered", "red"),
                 type = 'scattermapbox'
    )%>%layout(
      
      mapbox = list(
        
        style = 'open-street-map',
        zoom =3,
        center = list(lon =85.15743, lat = 25.600421))
        )
    
  })
  
  
  
  
  output$mfest<- renderPlotly({
    if (freq[[input$period]]=="Dt") {
      d<-base()$b%>%filter(!is.na(pickup))%>%group_by(Dt)%>%summarise(pickup = sum(pickup))%>%
        mutate(avg=roll_mean(pickup, n = 5, align = "right", fill = 0))%>%filter(Dt %in% seq(input$dateRange[1], input$dateRange[2], "day"))
      
      
      r<- base()$a%>%filter(!is.na(pickup))%>%group_by(Dt)%>%
        summarise(manifest = sum(manifested,na.rm = TRUE),pickup = sum(pickup,na.rm = TRUE))
      #!!sym(freq[[input$period]])  
      #x = as.formula(paste0("~", freq[[input$period]]))
      plot_ly()%>%
        add_trace(data = r,y = ~manifest,mode = "lines+text",x= ~Dt,type='scatter', name = "manifest",text=r$manifest,textposition= "auto",texttemplate="%{y:.2s}")%>%
        add_trace(data = r,y = r$pickup,mode = "lines+text",x= ~Dt ,type='scatter', name = "pickup",text=r$pickup,textposition= "auto",texttemplate="%{y:.2s}")%>%
        add_trace(data = d,y = round(d$avg,0), mode = "lines+text", x = ~Dt,type = 'scatter', name = "5D-moving average",text=d$avg,textposition= "auto",texttemplate="%{y:.2s}")%>%
        layout(yaxis = list(title = '<b>Count</b>'))
    }
    
    else { 
      d<-base()$b%>%filter(!is.na(pickup))%>%group_by(week)%>%summarise(pickup = sum(pickup))%>%
      mutate(avg=roll_mean(pickup, n =2, align = "right", fill = 0))%>%filter(week %in% seq(week(input$dateRange[1]), week(input$dateRange[2])))
    
    r<- base()$a%>%filter(!is.na(pickup))%>%group_by(week)%>%
      summarise(manifest = sum(manifested,na.rm = TRUE),pickup = sum(pickup,na.rm = TRUE))
    
    plot_ly()%>%
      add_trace(data = r,y = ~manifest,mode = "lines+text",x= ~week,type='scatter', name = "manifest",text=r$manifest,textposition= "auto",texttemplate="%{y:.2s}")%>%
      add_trace(data = r,y = r$pickup,mode = "lines+text",x= ~week ,type='scatter', name = "pickup",text=r$pickup,textposition= "auto",texttemplate="%{y:.2s}")%>%
      add_trace(data = d,y = round(d$avg,0), mode = "lines+text", x = ~week,type = 'scatter', name = "5D-moving average",text=d$avg,textposition= "auto",texttemplate="%{y:.2s}")%>%
      layout(yaxis = list(title = '<b>Count</b>'))}
    
  })
  
  
  
  
  
  
  
  output$mfest1<- renderPlotly({
    plot_ly(data = base()$a%>%group_by(wday)%>%
              summarise(manifest = sum(manifested,na.rm = TRUE),pickup = sum(pickup,na.rm = TRUE)))%>%
      add_trace(values = ~pickup, type='pie',labels = ~wday)%>%
      #add_trace(y = ~pickup,name = 'Pick-up',type='bar',textposition = 'outside',texttemplate="%{y:.2s}")%>%
      layout(title = 'Pickups by Day of the week',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  output$mfest3 <- renderUI({ dateRangeInput(inputId = "dr1", 
                                             label = "Date Range", 
                                             start=input$dateRange[1], end=input$dateRange[2],
                                             min = input$dateRange[1],max = input$dateRange[2],
                                             separator = "-"
  )})
  
  # Map Pickup --------------------------------------------------------------
  # output$mfest2<- renderPlotly({ req(input$dr1)
  #   mp<-base()$a%>%filter(!is.na(pickup) & Dt %in% seq(input$dr1[1], input$dr1[2], "day"))%>%
  #     group_by(oc,lat.y,lon.y)%>%
  #     summarize(pickup = sum(pickup,na.rm = TRUE))
  #   
  #   mp%>%plot_ly(lat = ~lat.y,
  #                lon = ~lon.y,
  #                size  = ~pickup,
  #                hovertemplate = paste("OC :", mp$oc,"<br> Pickup :", mp$pickup),
  #                #colors = c("springgreen","palevioletred","sandybrown","orange", "orangered", "red"),
  #                type = 'scattermapbox'
  #   )%>%layout(
  #     
  #     mapbox = list(
  #       
  #       style = 'open-street-map',
  #       zoom =3,
  #       center = list(lon =85.15743, lat = 25.600421))
  #   )
  #   
  # })
  

  output$p_day<- renderUI({dateInput("pday","Pickup Day",value = input$dateRange[2],min = input$dateRange[1],max = input$dateRange[2] )})

  # Map Pickup2 --------------------------------------------------------------  
  
output$mfest4<- renderPlotly({req(input$pday)
  pchng<- base()$b%>%filter(Dt>= today()-(input$adays+1) & Dt<= today()-(input$adays-1) & !is.na(pickup) & pt!='Pickup')%>%
    group_by(Dt,cn)%>%
    summarize(sm=sum(pickup,na.rm = TRUE))%>%ungroup()%>%group_by(cn)%>%summarise(avg= round(mean(sm),0))
  
  pvol<- base()$b%>%filter(Dt== input$pday & !is.na(pickup) & pt!='Pickup')%>%
    group_by(cn,lat.x,lon.x)%>%
    summarize(pickup = sum(pickup,na.rm = TRUE))%>%filter(pickup>5)%>%
    left_join(pchng,by = "cn")%>%
    mutate(chng= round(((pickup-avg)/avg)*100),1)%>%ungroup()%>%slice_max(order_by = pickup,n = vol[[input$pvol]])
  
  pvol%>%plot_ly(lat = pvol$lat.x,
               lon = pvol$lon.x,
               size  = pvol$pickup,
               color = pvol$chng,
               height = 600,
               hovertemplate = paste("DC :", pvol$cn,"<br> Pickup :", pvol$pickup,"<br> Change% :",pvol$chng,"%"),
               colors = c("red","springgreen","cyan","dark blue" ),
               type = 'scattermapbox'
  )%>%layout(
    title = "Volume Change by DC",
    mapbox = list(
      
      style = 'open-street-map',
      zoom =2,
      center = list(lon =85.15743, lat = 25.600421))
  )
  
})
  

  observe({
    if(isTruthy(input$sb == "pikup")) {
      shinyjs::show("or")
      shinyjs::hide("dr")
      
    } 
    else {shinyjs::hide("or")
      shinyjs::show("dr")}
    
  })
  
  
}

shinyApp(ui = ui, server = server)