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


# Initial list ------------------------------------------------------------

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
    
    fluidRow(column(3,selectInput("period","Frequency",choices = names(freq))),
             div(id="dr", column(3,selectInput(
      inputId = "rgn", 
      label = "Destination region", 
      choices = names(regn), 
      selected = "All"
    ))),
    shinyjs::hidden(div(id="or",column(3,selectInput(
      inputId = "rgn1", 
      label = "Origin region", 
      choices = names(regn), 
      selected = "All"
    )))),
    column(3,selectizeInput(
      inputId = "pt",
      label = "Payment type",
      choices = names(p_type),
      size = 3,
      selected = "Both"
    )),
    column(3,selectizeInput(
      inputId = "mot",
      label = "Mode of transport",
      choices = names(mode),
      size = 3,
      selected = "Both"
    ))),
    tabItems(
      tabItem(tabName = "speed",withSpinner(tagList(
              fluidRow(infoBoxOutput("del"),infoBoxOutput("avg_s2d"),infoBoxOutput("avg_s2dc")),
              fluidRow(plotlyOutput("speed")),br(),
              uiOutput("speed4"),
              fluidRow(dataTableOutput("rs2d"))),color="#0dc5c1",hide.ui = FALSE),br(),
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
      
      
      tabItem(tabName = "pikup",
              withSpinner(tagList(
              fluidRow(infoBoxOutput("o_man"),infoBoxOutput("o_pik")),
              fluidRow(plotlyOutput("mfest")),br(),
              fluidRow(plotlyOutput("mfest1")),br(),
              fluidRow(plotlyOutput("mfest2"))),color="#0dc5c1"),br()
      )
      
    )
  )
)
ui <- secure_app(ui)

server <- function(input, output,session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  base_data<-eventReactive(input$btn,{
   
    a<-rds%>%tbl("s_analytics")%>%
      filter(cl %in% !!client[[input$client]] & pt %in% !!p_type[["Both"]] & mot %in% !!mode[["Both"]] & Dt %in% !!seq(input$dateRange[1], input$dateRange[2], "day"))%>%
      select(-CRD_NonOTP,-CRD_OTP,-Others,-CNA,-CRR,-year,-month,-ANF,-ODA,-Self_Collect,-Bulkout)%>%collect()
    setDT(a)
    
    a$Dt<- ymd(a$Dt)
    a<- left_join(a,rds%>%tbl("facility")%>%select(name,city,region,state)%>%collect(),by = c("cn"="name"))
    a<- left_join(a,rds%>%tbl("facility")%>%select(name,city,region,state)%>%collect(),by = c("oc"="name"))
    
    a[cn=="NSZ",region.x:="North"]
    a[is.na(region.x) & str_detect(cn,"Rajasthan|Punjab|Ladakh|Haryana|Uttar Pradesh|Chandigarh"),region.x:="North"]
    a[is.na(region.x) & str_detect(cn,"Maharashtra|Madhya Pradesh"),region.x:="West"]
    a[is.na(region.x) & str_detect(cn,"Kerala|Tamil Nadu|Karnataka|Telangana|Andhra Pradesh"),region.x:="South"]
    a[is.na(region.x) & str_detect(cn,"Manipur|West Bengal|Orissa"),region.x:="East"]
    
  },ignoreInit = FALSE,ignoreNULL = FALSE)
  
  
  # base<- reactive({base_data()%>%
  #     filter(pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]])})
  
  
  base<- reactive({ 
    b<-base_data()[pt %in% p_type[[input$pt]] & mot %in% mode[[input$mot]] & region.x %in% regn[[input$rgn]] & region.y %in% regn[[input$rgn1]],]
    })
  
  
  
 

  
  output$del<- renderInfoBox({
    base()%>%filter(!is.na(delivered))%>%summarise(sum(delivered,na.rm = TRUE))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "Delivered",icon = icon("gift"),color = "blue",fill=FALSE)
    
  })
  
  output$o_ofd<- renderInfoBox({
    base()%>%filter(!is.na(OFD))%>%summarise(sum(OFD))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "OFD",icon = icon("gift"))
    
  })
  
  output$o_fad<- renderInfoBox({
    base()%>%filter(!is.na(OFD))%>%summarise(sum(FAD,na.rm = TRUE))%>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      infoBox(title = "FAD#",icon = icon("list"),color = "purple")
    
  })
  
  output$p_fad<- renderInfoBox({
    
    x<-base()%>%filter(!is.na(OFD))%>%summarise((sum(FAD,na.rm = TRUE)/sum(OFD))*100)
    ic<- 'thumbs-down'
    clr<- 'yellow'
    if(x > 90) {
      ic <- 'thumbs-up'
      clr<- 'green'}
    infoBox(value = x%>%round(digits = 2) %>% paste0("%") ,title = "FAD%",icon = icon(ic),color = clr,fill=FALSE)
    
  })
  
  
  
  output$avg_s2d<- renderInfoBox({
    base()%>%filter(!is.na(delivered))%>%summarise(round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pull() %>% round(digits = 2)%>%
      infoBox(title = "S2D",icon = icon("dashboard"),color = "purple",fill=FALSE)
    
  })
  
  output$avg_s2dc<- renderInfoBox({
    base()%>%filter(!is.na(delivered))%>%summarise(round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pull() %>% round(digits = 2)%>%
      infoBox(title = "S2DC",icon = icon("dashboard"),color = "purple",fill=FALSE)
    
  })
  
  output$o_man<- renderInfoBox({
    base()%>%filter(!is.na(manifested))%>%summarise(sum(manifested))%>%
      pull() %>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Manifested",icon = icon("list"),color = "orange",fill=FALSE)
    
  })
  
  output$o_pik<- renderInfoBox({
    base()%>%summarise(sum(pickup,na.rm = TRUE))%>%
      pull() %>% prettyNum(big.mark = ",")%>%
      infoBox(title = "Pick-Up",icon = icon("list"),color = "purple",fill=FALSE)
    
  })
  
  
  
  output$speed <- renderPlotly({
     sp<- base()%>%filter(!is.na(delivered))%>%group_by(!!sym(freq[[input$period]]))%>%
       summarise(Del = sum(delivered),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered),2),S2DC = round(sum((S2DC*delivered),na.rm = TRUE)/sum(delivered),2),S2Pro = round(sum((S2Pro*delivered),na.rm = TRUE)/sum(delivered),2))
    
    
     plot_ly(data = sp,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~Del,type = "bar", name = "Delivered",textposition = 'outside',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)')))%>%
      add_trace(y = ~S2D,mode = "lines+text", type='scatter', yaxis = "y2", name = "Avg S2D",text=~round(S2D,2),textposition= "top center") %>%
      add_trace(y = ~S2DC,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2DC") %>%
      add_trace(y = ~S2Pro,mode = "line", type='scatter', yaxis = "y2", name = "Avg S2Pro") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$speed4<- renderUI({selectInput('orgn',"Origin Region",choices = names(regn),selected = 'All')})
  
  output$rs2d <- renderDataTable(
    base()%>%filter(!is.na(delivered) & region.y %in% !!regn[[input$orgn]])%>%
      group_by(!!sym(freq[[input$period]]),region.x)%>%
      summarise(S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
      pivot_wider(id_cols = region.x,names_from = !!sym(freq[[input$period]]),values_from = S2D),options = list(scrollX = T)
    
  )
  
  l<- reactive({base()%>%filter(!is.na(delivered))%>%
      group_by(Dt,oc,cn)%>%summarise(Del=sum(delivered),S2D = round(sum((S2D*delivered),na.rm = TRUE)/sum(delivered,na.rm = TRUE),2))%>%
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
    fd<- base()%>%filter(!is.na(OFD))%>%group_by(!!sym(freq[[input$period]]))%>%
      summarise(OFD = sum(OFD),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)
    plot_ly(data = fd,x = as.formula(paste0("~", freq[[input$period]]))) %>%
      add_trace(y = ~OFD,type = "bar", name = "OFD",textposition = 'auto',texttemplate="%{y:.2s}",marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)'))) %>%
      add_trace(y = ~FAD,mode = "lines+text", type='scatter', yaxis = "y2", name = "FAD%",text=~round(FAD,2),textposition= "auto")%>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  
  
  output$fad2 <- renderDataTable(
    base()%>%filter(!is.na(OFD))%>%
      group_by(!!sym(freq[[input$period]]),region.x)%>%
      summarise(FAD = round((sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100,2))%>%
      pivot_wider(id_cols = region.x,names_from = !!sym(freq[[input$period]]),values_from = FAD), options = list(scrollX = T)
  )
  

  
  output$fad3<- renderUI({selectInput('st',"State",choices = as.list(c(unique(base()%>%select(state.x)),"All")),selected = 'All')})
  
  f<-reactive({
    if (input$st == 'All') {
      base()%>%group_by(Dt,cn)%>%
        summarise(OFD=sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
        arrange(Dt,cn,desc(OFD))  
    }
    else{
    base()%>%filter(state.x %in% (input$st))%>%
      group_by(Dt,cn)%>%summarise(OFD=sum(OFD,na.rm = TRUE),FAD = (sum(FAD,na.rm = TRUE)/sum(OFD,na.rm = TRUE))*100)%>%
      arrange(Dt,cn,desc(OFD))}})
  
  
  
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