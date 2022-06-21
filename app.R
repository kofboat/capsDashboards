

#library(shiny)
library(bs4Dash)
library(tidyverse)
library(ggplot2)
library(DT)
#----------------------------------------------------------
#     IMPORT DATA
#----------------------------------------------------------

msd  <- read_csv("master.data.csv")
med  <- read_csv("expense.account.csv")
mtd  <- read_csv("Traffic.csv") 
mibd   <- read_csv("inventory.opening.balance.csv") 
ipa    <- read_csv("inventory.purchase.account.csv")
#-----------------------------------------------------------
#     DATA CLEANING AND PREPARATION
#------------------------------------------------------------

#msd <- msd %>%
  #select(!c(12:19))  # delete empty columns
# create new , reorganize and delete variables
msd1 <- msd %>%
  mutate(prod_cat = category,
         gross_margin = round((gross_profit/ total_sale_price *100),2),
         sub_cat = sub_category,
         quarter = case_when(month=='NOV_21' ~ 1, month=='DEC_21' ~ 1,
                             month=='JAN_22' ~ 1, month=='FEB_22' ~ 2,
                             month=='MAR_22' ~ 2, month=='APR_22' ~ 2,
                             month=='MAY_22' ~ 3, month=='JUN_22' ~ 3,
                             month=='JUL_22' ~ 3, month=='AUG_22' ~ 4,
                             month=='SEP_22' ~ 4, month=='OCT_22' ~ 4
         ),
         category = NULL,
         sub_category = NULL,
         margin = NULL
  )%>% 
  relocate(prod_cat, .before = qty)%>% 
  relocate(sub_cat,.after = prod_cat)%>%
  relocate(quarter, .before = product)
msd1<- as_tibble(msd1)
mibd <- mibd %>%
  mutate(
    quarter = (
      
      case_when(month=='NOV_21' ~ 1, month=='DEC_21' ~ 1,
                month=='JAN_22' ~ 1, month=='FEB_22' ~ 2,
                month=='MAR_22' ~ 2, month=='APR_22' ~ 2,
                month=='MAY_22' ~ 3, month=='JUN_22' ~ 3,
                month=='JUL_22' ~ 3, month=='AUG_22' ~ 4,
                month=='SEP_22' ~ 4, month=='OCT_22' ~ 4
      )
      
    )
  )

mtd1 <- mtd %>%
  mutate(
    month =
      case_when(str_sub(Date,1,1) == 1 ~ 'JAN_22', 
                str_sub(Date,1,1) == 2 ~ 'FEB_22',
                str_sub(Date,1,1) == 3 ~ 'MAR_22', 
                str_sub(Date,1,1) == 4 ~ 'APR_22',
                str_sub(Date,1,1) == 5 ~ 'MAY_22', 
                str_sub(Date,1,1) == 6 ~ 'JUN_22',
                str_sub(Date,1,1) == 7 ~ 'JUL_22', 
                str_sub(Date,1,1) == 8 ~ 'AUG_22',
                str_sub(Date,1,1) == 9 ~ 'SEP_22', 
                str_sub(Date,1,1) == 10 ~'OCT_22',
                str_sub(Date,1,1) == 11 ~ 'NOV_21', 
                str_sub(Date,1,1) == 12 ~ 'DEC_21'
      ),
    
    
    
    quarter = (
      
      case_when(month=='NOV_21' ~ 1, month=='DEC_21' ~ 1,
                month=='JAN_22' ~ 1, month=='FEB_22' ~ 2,
                month=='MAR_22' ~ 2, month=='APR_22' ~ 2,
                month=='MAY_22' ~ 3, month=='JUN_22' ~ 3,
                month=='JUL_22' ~ 3, month=='AUG_22' ~ 4,
                month=='SEP_22' ~ 4, month=='OCT_22' ~ 4
      )
      
    )
    
  ) %>%
  
  relocate(month, .before = Date)  %>%
  relocate (quarter, .before = month)

mtd1 <- as_tibble(mtd1)
mtd1

         
#  Dashboard UI
#------------------------------------
ui <- dashboardPage(
  
      dashboardHeader(title = "CAPS PHARMACY"),
      
      dashboardSidebar(
        
        sidebarMenu(
          
         menuItem("HOME", tabName = "home", 
                  icon = icon("home", lib="font-awesome")), 
         
         menuItem("MONTHLY VIEW", tabName = "monthly", 
                  icon = icon("binoculars", lib = "font-awesome")), 
                  
         menuItem("QUARTERLY VIEW", tabName = "quarter",
                  icon = icon("binoculars", lib = "font-awesome")), 
         
         menuItem("YEAR TO DATE VIEW", tabName = "YTD", 
                  icon = icon("binoculars", lib = "font-awesome"))
         
        )
        
      ),
      
      dashboardBody(
        tabItems(
       
       tabItem(tabName = "home", 
            fluidRow(
              
              bs4Card(title=NULL,footer="DESIGN PROPOSALS",status = "maroon",solidHeader = T,
                      background = NULL,height = NULL,icon = NULL,
                      collapsible = TRUE,collapsed = FALSE,
                      closable = FALSE,maximizable = FALSE, label = NULL,
                      gradient = FALSE,elevation = 4,boxToolSize = "sm",
                      headerBorder = TRUE,dropdownMenu = NULL,
                      sidebar = NULL, width = 6,
                 
                 bs4Carousel(
                    id="design.images",
                  
                   bs4CarouselItem(caption = "Design 1", img(src="images.1.jpg")),
                   
                   bs4CarouselItem(caption = "Design 2" , img(src="images.2.jpg")),
                   
                   bs4CarouselItem(caption = "Design 3", img(src="images.3.jpg")),
                   
                   bs4CarouselItem(caption = "Design 4", img(src="images.4.jpg")),
                   
                   bs4CarouselItem(caption = "Design 5", img(src="images.8.jpg")),
                   
                   bs4CarouselItem(caption = "Design 6", img(src="images.9.jpg")),
                   
                   bs4CarouselItem(caption = "Design 7" , img(src="images.11.jpg")),
                   
                   bs4CarouselItem(caption = "Design 8", img(src="images.12.jpg")),
                   
                   bs4CarouselItem(caption = "Design 9", img(src="images.13.jpg"))
                   
                  
               )
               
               
               
               
             ),
          
             bs4Card(title=NULL,footer="PRODUCT IMAGES",status = "maroon",solidHeader = T,
                     background = NULL,height = NULL,icon = NULL,
                     collapsible = TRUE,collapsed = FALSE,
                     closable = FALSE,maximizable = FALSE, label = NULL,
                     gradient = FALSE,elevation = 4,boxToolSize = "sm",
                     headerBorder = TRUE,dropdownMenu = NULL,
                     sidebar = NULL, width = 6,
                     
                     bs4Carousel(
                       id="product.images",
                       
                       bs4CarouselItem(caption = "prod 1", img(src="advil.jpg")),
                       
                       bs4CarouselItem(caption = "prod 2" , img(src="child.cough.jpg")),
                       
                       bs4CarouselItem(caption = "prod 3", img(src="condom.jpg")),
                       
                       bs4CarouselItem(caption = "prod 4", img(src="cough.jpg")),
                       
                       bs4CarouselItem(caption = "prod 5", img(src="ibu.gel.jpg")),
                       
                       bs4CarouselItem(caption = "prod 6", img(src="lidocaine.jpg")),
                       
                       bs4CarouselItem(caption = "prod 7", img(src="lidocaine.patch.jpg")),
                       
                       bs4CarouselItem(caption = "prod 8" , img(src="lotrisone.jpg")),
                       
                       bs4CarouselItem(caption = "prod 9", img(src="tampons.jpg")),
                       
                       bs4CarouselItem(caption = "prod 10", img(src="tylenol.jpg")),
                       
                       bs4CarouselItem(caption = "prod 11", img(src="vitamins.jpg")),
                       
                       bs4CarouselItem(caption = "prod 12", img(src="voltaren.jpg"))
                     )
                     
                     
                     
                     
             )        
 
         
       ),
       
       
       
       fluidRow(
         
         bs4Card(title=NULL,footer="ANNOUNCEMENTS",status = "maroon",solidHeader = T,
                 background = NULL,height = NULL,icon = NULL,
                 collapsible = TRUE,collapsed = FALSE,
                 closable = FALSE,maximizable = FALSE, label = NULL,
                 gradient = FALSE,elevation = 4,boxToolSize = "sm",
                 headerBorder = TRUE,dropdownMenu = NULL,
                 sidebar = NULL, width = 6,
                 
                 bs4Carousel(
                   id="design.images",
                   
                   bs4CarouselItem(caption = "Design 1", img(src="images.1.jpg")),
                   
                   bs4CarouselItem(caption = "Design 2" , img(src="images.2.jpg")),
                   
                   bs4CarouselItem(caption = "Design 3", img(src="images.3.jpg")),
                   
                   bs4CarouselItem(caption = "Design 4", img(src="images.4.jpg")),
                   
                   bs4CarouselItem(caption = "Design 5", img(src="images.5.jpg")),
                   
                   bs4CarouselItem(caption = "Design 6", img(src="images.6.jpg"))
                 )
                 
                 
                 
                 
         ),
         
         bs4Card(title=NULL,footer="TASKS",status = "maroon",solidHeader = T,
                 background = NULL,height = NULL,icon = NULL,
                 collapsible = TRUE,collapsed = FALSE,
                 closable = FALSE,maximizable = FALSE, label = NULL,
                 gradient = FALSE,elevation = 4,boxToolSize = "sm",
                 headerBorder = TRUE,dropdownMenu = NULL,
                 sidebar = NULL, width = 6,
                 
                 bs4Carousel(
                   id="product.images",
                   
                   bs4CarouselItem(caption = "product 1", img(src="images.1.jpg")),
                   
                   bs4CarouselItem(caption = "product 2" , img(src="images.2.jpg")),
                   
                   bs4CarouselItem(caption = "product 3", img(src="images.3.jpg")),
                   
                   bs4CarouselItem(caption = "product 4", img(src="images.4.jpg")),
                   
                   bs4CarouselItem(caption = "product 5", img(src="images.5.jpg")),
                   
                   bs4CarouselItem(caption = "product 6", img(src="images.6.jpg"))
                 )
                 
                 
                 
                 
         )        
         
         
       )
       
       
       
       
       
       
       
       
       ),
       
       tabItem(tabName = "monthly", 
               fluidRow(
                 valueBox("MONTHLY DATA OVERVIEW", "",
                          icon = icon("binoculars") , 
                          color = "maroon", width = 4, elevation = 4),
                 
                 bs4Card(title = "Month", footer = NULL,
                         status = "maroon", solidHeader = T,width = 4,
                         background = NULL,height = NULL,icon = NULL,
                         collapsible = TRUE,collapsed = FALSE,
                         closable = FALSE,maximizable = FALSE, label = NULL,
                         gradient = FALSE,elevation = 4,boxToolSize = "sm",
                         headerBorder = TRUE,dropdownMenu = NULL,
                         sidebar = NULL,id = "month.selector",
                         selectInput(inputId = "month",
                                     label = h6("Select Month"), 
                                     choices = c("NOV_21","DEC_21",
                                                 "JAN_22","FEB_22",
                                                 "MAR_22","APR_22",
                                                 "MAY_22","JUN_22",
                                                 "JUL_22","AUG_22",
                                                 "SEP_22","OCT_22"), 
                                     selected = "NOV_21",width = "200px")) ,
                 
                 bs4Card(title = "Year", footer = NULL,
                         status = "maroon", solidHeader = T,width = 4,
                         background = NULL,height = NULL,icon = NULL,
                         collapsible = TRUE,collapsed = FALSE,
                         closable = FALSE,maximizable = FALSE, label = NULL,
                         gradient = FALSE,elevation = 4,boxToolSize = "sm",
                         headerBorder = TRUE,dropdownMenu = NULL,
                         sidebar = NULL,id = "year.selector",
                         selectInput(inputId = "year.month",
                                     label = h6("Select Year"), 
                                     choices = c(2021,2022,2023,2024,2025), 
                                     selected = 2021 ,width = "200px"))
                 
               ),
               
               
               fluidRow(
                 
                 infoBoxOutput("monthly.revenue", width = 4),
                 infoBoxOutput("monthly.cost.of.sale", width = 4),
                 infoBoxOutput("monthly.gross.profit", width = 4)
                 
               ),
               
               fluidRow(
                 
                 infoBoxOutput("monthly.expense", width = 4),
                 infoBoxOutput("monthly.items.sold", width = 4),
                 infoBoxOutput("monthly.visitors", width = 4)
                 
               ),
               
               fluidRow(
                 infoBoxOutput("monthly.gross.profit.percent", width = 4),
                 infoBoxOutput("monthly.net.profit", width = 4),
                 infoBoxOutput("monthly.net.profit.percent", width = 4)
                 
                 
               ),   
               
               fluidRow(
                 infoBoxOutput("monthly.inventory.closing.bal", width = 4),
                 infoBoxOutput("monthly.bankrec", width = 4),
                 infoBoxOutput("monthly.items.transac", width = 4)
                 
                 
               ),
               
               fluidRow(
                 
                 bs4Card(title = "Daily visitors per month",
                         footer = NULL,
                         status = "warning",
                         solidHeader = T,
                         background = NULL,
                         width = 12,
                         height = NULL,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = FALSE,
                         icon = NULL,
                         gradient = FALSE,
                         boxToolSize = "sm",
                         elevation = 4,
                         headerBorder = TRUE,
                         label = NULL,
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         id = "plot.daily.visitors",
                         
                         plotOutput("daily.visitors")
                 )
               ),
               
               fluidRow(
                 tabBox(
                   id = "monthly.rev.data",
                   collapsible = T,
                   elevation = 4,
                   title = "Revenue",
                   selected = "Product category",
                   status = "info",
                   side ="right", 
                   solidHeader = T,
                   type = "tabs",
                   tabPanel(
                     title = "Product category",
                     tableOutput("monthly.revenue.prod.cate")
                     
                   ),
                   tabPanel(
                     title = "Product sub-category",
                     DTOutput("monthly.revenue.prod.subcate")
                     
                   ),
                   tabPanel(
                     title = "Product",
                     DTOutput("monthly.revenue.product")
                     
                   )
                   
                 ),
                 tabBox(
                   id = "monthly.items.data",
                   collapsible = T,
                   elevation = 4,
                   title = "Items sold",
                   side = "right",
                   selected = "Product category",
                   status = "info",
                   solidHeader = T,
                   
                   type = "tabs",
                   
                   tabPanel(
                     title = "Product category",
                     tableOutput("monthly.sales.prod.cate")
                   ),
                   tabPanel(
                     title = "Product sub-category",
                     DTOutput("monthly.sales.prod.subcate")
                     
                   ),
                   tabPanel(
                     title = "Product",
                     DTOutput("monthly.sales.product"))
                   
                 ))),
       
       tabItem(tabName = "quarter", 
           
               fluidRow(
                 valueBox("QUARTERLY DATA OVERVIEW", "",
                          icon = icon("binoculars"), color = "maroon", 
                          width = 4, elevation = 4),
                       
                  bs4Card(title = "Quarter", footer = NULL,
                          status = "maroon", solidHeader = T,width = 4,
                          background = NULL,height = NULL,icon = NULL,
                          collapsible = TRUE,collapsed = FALSE,
                          closable = FALSE,maximizable = FALSE, label = NULL,
                          gradient = FALSE,elevation = 4,boxToolSize = "sm",
                          headerBorder = TRUE,dropdownMenu = NULL,
                          sidebar = NULL,id = "quarter.selector",
                          selectInput(inputId = "quarter",
                                      label = h6("Select Quarter"), 
                                      choices =c(1,2,3,4 ), selected = 1,
                                      width="200px")),       
           
                 bs4Card(title = "Year", footer = NULL,
                         status = "maroon", solidHeader = T,width = 4,
                         background = NULL,height = NULL,icon = NULL,
                         collapsible = TRUE,collapsed = FALSE,
                         closable = FALSE,maximizable = FALSE, label = NULL,
                         gradient = FALSE,elevation = 4,boxToolSize = "sm",
                         headerBorder = TRUE,dropdownMenu = NULL,
                         sidebar = NULL,id = "year.selector",
                         selectInput(inputId = "year.quarter",
                                     label = h6("Select Year"), 
                                     choices = c(2021,2022,2023,2024,2025), 
                                     selected = 2021 ,width = "200px"))
                 
               ),
               
               fluidRow(
                 
                 infoBoxOutput("quarterly.revenue", width = 4),
                 infoBoxOutput("quarterly.cost.of.sale", width = 4),
                 infoBoxOutput("quarterly.gross.profit", width = 4)
                 
               ),
           
           fluidRow(
             infoBoxOutput("quarterly.gross.profit.percent", width = 4),
             infoBoxOutput("quarterly.expense", width = 4),
             infoBoxOutput("quarterly.net.profit", width = 4)
             
           ),
           
           fluidRow(
             infoBoxOutput("quarterly.net.profit.percent", width = 4),           
             infoBoxOutput("quarterly.items.sold", width = 4),
             infoBoxOutput("quarterly.visitors", width = 4)            
           ),
           
           fluidRow(
             infoBoxOutput("quarterly.inventory.closing.bal", width = 4),
             infoBoxOutput("quarterly.bankrec", width = 4), 
             infoBoxOutput("quarterly.items.transac", width = 4)
           ),
            
          fluidRow(
            bs4Card(title = "Monthly visitors per quarter", footer = NULL,
                    status = "warning", solidHeader = T,width = 12,
                    background = NULL,height = NULL,icon = NULL,
                    collapsible = TRUE,collapsed = FALSE,
                    closable = FALSE,maximizable = FALSE, label = NULL,
                    gradient = FALSE,elevation = 4,boxToolSize = "sm",
                    headerBorder = TRUE,dropdownMenu = NULL,
                    sidebar = NULL,id = "monthly.transac.quarter",
                    plotOutput("monthly.visitors.quarter")
                   )
            
          ),
           
        fluidRow(  
                  
                  tabBox(
                    id = "quarterly.rev.data",
                    collapsible = F,
                    elevation = 4,
                    title = "Revenue",
                    selected = "Product category",
                    status = "info",
                    solidHeader = T,
                    type = "tabs",
                    side = "right",
                    tabPanel(
                      title = "Product category",
                      tableOutput("quarterly.revenue.prod.cate")
                      
                    ),
                    tabPanel(
                      title = "Product sub-category",
                      DTOutput("quarterly.revenue.prod.subcate")
                      
                    ),
                    tabPanel(
                      title = "Product",
                      DTOutput("quarterly.revenue.product")
                      
                    )
                  )
                    
                  ,
          
               tabBox(
                    id = "quarterly.items.data",
                    title = "Items sold",
                    side ="right",
                    collapsible = F,
                    elevation = 4,
                    selected = "Product category",
                    status = "info",
                    solidHeader = T,
                    type = "tabs",
                    tabPanel(
                      title = "Product category",
                      tableOutput("quarterly.sales.prod.cate")
                      
                    ),
                    tabPanel(
                      title = "Product sub-category",
                      DTOutput("quarterly.sales.prod.subcate")
                      
                    ),
                    tabPanel(
                      title = "Product",
                      DTOutput("quarterly.sales.product")
                      
                      
                    )
                  )	
                  
          )),
       
       
       tabItem(tabName = "YTD", 
               
               fluidRow(
                 valueBox("YEAR-TO-DATE OVERVIEW", "",
                          icon = icon("binoculars"), color = "maroon", 
                          width = 4, elevation = 4),
                
                 bs4Card(title = "Year", footer = NULL,
                         status = "maroon", solidHeader = T,width = 4,
                         background = NULL,height = NULL,icon = NULL,
                         collapsible = TRUE,collapsed = FALSE,
                         closable = FALSE,maximizable = FALSE, label = NULL,
                         gradient = FALSE,elevation = 4,boxToolSize = "sm",
                         headerBorder = TRUE,dropdownMenu = NULL,
                         sidebar = NULL,id = "year.selector",
                         selectInput(inputId = "year.ytd",
                                     label = h6("Select Year"), 
                                     choices = c(2021,2022,2023,2024,2025), 
                                     selected = 2021 ,width = "200px"))
               ), 
               
               fluidRow(
                 
                 infoBoxOutput("ytd.revenue", width = 4),
                 infoBoxOutput("ytd.cost.of.sale", width = 4),
                 infoBoxOutput("ytd.gross.profit", width = 4)
                 
               ), 
               
               fluidRow(
                 infoBoxOutput("ytd.gross.profit.percent", width = 4),
                 infoBoxOutput("ytd.expense", width = 4),
                 infoBoxOutput("ytd.net.profit", width = 4)
                 
               ),
               
               fluidRow(
                 infoBoxOutput("ytd.net.profit.percent", width = 4),           
                 infoBoxOutput("ytd.items.sold", width = 4),
                 infoBoxOutput("ytd.visitors", width = 4)            
               ),
               
               fluidRow(
                 infoBoxOutput("ytd.inventory.closing.bal", width = 4),
                 infoBoxOutput("ytd.bankrec", width = 4), 
                 infoBoxOutput("ytd.items.transac", width = 4)
               ),
               
               fluidRow(
                 bs4Card(title = "Monthly visitors ytd", footer = NULL,
                         status = "warning", solidHeader = T,width = 12,
                         background = NULL,height = NULL,icon = NULL,
                         collapsible = TRUE,collapsed = FALSE,
                         closable = FALSE,maximizable = FALSE, label = NULL,
                         gradient = FALSE,elevation = 4,boxToolSize = "sm",
                         headerBorder = TRUE,dropdownMenu = NULL,
                         sidebar = NULL,id = "YTD.transac",
                         plotOutput("ytd.total.visitors.month")
                 )),
               
               fluidRow(  
                 
                 tabBox(
                   id = "ytd.rev.data",
                   collapsible = F,
                   elevation = 4,
                   title = "Revenue",
                   selected = "Product category",
                   status = "info",
                   solidHeader = T,
                   type = "tabs",
                   side = "right",
                   tabPanel(
                     title = "Product category",
                     tableOutput("ytd.revenue.prod.cate")
                     
                   ),
                   tabPanel(
                     title = "Product sub-category",
                     DTOutput("ytd.revenue.prod.subcate")
                     
                   ),
                   tabPanel(
                     title = "Product",
                     DTOutput("ytd.revenue.product")
                     
                   )
                 ),
                 
                 tabBox(
                   id = "ytd.items.data",
                   title = "Items sold",
                   side ="right",
                   collapsible = F,
                   elevation = 4,
                   selected = "Product category",
                   status = "info",
                   solidHeader = T,
                   type = "tabs",
                   tabPanel(
                     title = "Product category",
                     tableOutput("ytd.sales.prod.cate")
                     
                   ),
                   tabPanel(
                     title = "Product sub-category",
                     DTOutput("ytd.sales.prod.subcate")
                     
                   ),
                   tabPanel(
                     title = "Product",
                     DTOutput("ytd.sales.product")
                     
                   )
                   
                 )
                 )
               
              
                 
               )
       
          )
        )
       
      )
 
      

 
                                 
server <- function(input, output) {
  # Business logic - calculations
  
  #---------------month
  
  #--------
  # Monthly overview  data 
  #--------
  
  # revenue
  monthly.rev <- function(x){
    total.monthly.rev<-  msd1 %>% filter(month == x)  %>% 
      summarise(total_sales=sum(total_sale_price))
    total.monthly.rev }
  
  # item sales
  monthly.item.sales <- function(x){
    total.sales.month <-  msd1 %>% filter(month == x)  %>% 
      summarise(total_sales=sum(qty))
    total.sales.month  }
  
  # transactions
  monthly.transactions <- function(x){
    monthly.unique.visitors <- mtd1 %>% filter (month == x) %>%
      summarise(monthly.visitors = n_distinct(transactions)) 
    monthly.unique.visitors <- as_tibble(monthly.unique.visitors)
    monthly.unique.visitors   }
  
  # expense
  monthly.expense <- function(x){
    monthly.expense <- med %>% filter( month == x) %>%
      summarise( expense= sum(amount))
    monthly.expense  }
  
  # gross profit percent
  monthly.avg.gross.profit.percent<- function(x){
    gross.profit.margin <- msd1 %>% filter( month == x) %>%
      summarise(avg.gross.profit.margin = round(mean(gross_margin), 2))
    gross.profit.margin  }
  
  #gross profit
  monthly.gross.prof <- function(x){
    
    gross.prof <- monthly.rev(x) - monthly.cost.of.sale(x) 
    round(gross.prof,2)
  }
  
  # cost of sale
  monthly.cost.of.sale <- function(x){
    cost.of.sale <- msd1 %>% filter( month == x) %>%
      summarise(cost_of_sale = sum(total_cost_price))
    cost.of.sale  }
  
  # net profit percent  
  monthly.net.prof.percent <- function(x){
    net.profit.percent <- round( ((monthly.rev(x) - (monthly.cost.of.sale(x)
                                                     + monthly.expense(x)))
                                  /monthly.rev(x)) * 100, 2)
    net.profit.percent
  } 
  
  # net profit    
  monthly.net.prof <- function(x){
    
    net.profit <- (monthly.rev(x) - (monthly.cost.of.sale(x) + monthly.expense(x)))
    round(net.profit,2)
  }
  
  # items per transaction
  monthly.items.transaction <- function(x){
    items.transaction <- monthly.item.sales(x) /  monthly.transactions(x)
    items.transaction
  } 
  
  
  
  # opening inventory balance
  monthly.open.bal <- function(x){
    open.bal <- mibd %>% filter(month == x) 
    open.bal[,3] }
  
  # inventory purchases
  monthly.inventory.purchase <- function(x){
    inventory.purchase <- ipa %>% filter(month == x) %>%
      summarise(total = sum(amount))
    inventory.purchase }
  
  #bank rec
  monthly.bank.reconiliation <- function(x){
    bank <- monthly.rev(x) - (monthly.expense(x)+ monthly.inventory.purchase(x))
    round(bank,2) }
  
  #inventory closing balance
  monthly.closing.balance <- function(x){
    monthly.open.bal(x) + monthly.inventory.purchase(x) - monthly.cost.of.sale(x)}
  
  
  # Monthly drill-down 
  # revenue by product category
  month.rev.prod.cat <- function(x){(
    monthly.rev.prod.cat<-  msd1 %>% filter(month == x)  %>% 
      group_by(prod_cat)%>%
      summarise(total_sales=sum(total_sale_price))
  )
    monthly.rev.prod.cat <- monthly.rev.prod.cat[order(
      -monthly.rev.prod.cat$total_sales),]
    monthly.rev.prod.cat}
  
  # revenue by product sub-category
  month.rev.sub.cat <- function(x){(
    monthly.rev.sub.cat<-  msd1 %>% filter(month == x)  %>% 
      group_by(sub_cat)%>%
      summarise(total_sales=sum(total_sale_price))
  )
    monthly.rev.sub.cat<- monthly.rev.sub.cat[order(
      -monthly.rev.sub.cat$total_sales),]
    monthly.rev.sub.cat }
  
  
  # revenue by product 
  month.rev.prod <- function(x){(
    monthly.rev.prod<-  msd1 %>% filter(month == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=(total_sale_price))
  )
    monthly.rev.prod <- monthly.rev.prod[order(
      -monthly.rev.prod$total_sales),]
    monthly.rev.prod }  
  
  
  # items sold by product category
  month.sales.prod.cat <- function(x){
    monthly.prod.cat.sales <-  msd1 %>% filter(month == x)  %>% 
      group_by(prod_cat)%>%
      summarise(total_sales=sum(qty))
    monthly.prod.cat.sales }
  
  # items sold by product sub-category
  month.sales.sub.cat <- function(x){
    (monthly.sales.sub.cat <-  msd1 %>% filter(month == x)  %>% 
       group_by(sub_cat)%>%
       summarise(total_sales=sum(qty))
    )
    monthly.sales.sub.cat <- monthly.sales.sub.cat[order(
      -monthly.sales.sub.cat$total_sales),]
    monthly.sales.sub.cat }             
  
  # items sold by product              
  month.sales.prod <- function(x){
    
    (monthly.sales.prod <-  msd1 %>% filter(month == x)  %>% 
       group_by(product,sub_cat)%>%
       summarise(total_sales=(qty))
    )
    monthly.sales.prod<- monthly.sales.prod[order(
      -monthly.sales.prod$total_sales),]
    monthly.sales.prod }             
  
  # daily transactions
  daily.transactions <- function(x){
    visitors <- mtd1 %>% filter (month == x) %>%
      count(Date) 
    (visitors <- visitors[order(-visitors$n),])
    
    visitors %>% mutate(Date=fct_reorder(Date, -n), visitors = n) %>%
      ggplot +
      geom_col(mapping = aes(Date,visitors)) }
  
  #monthly
  output$monthly.visitors <- renderInfoBox({
    infoBox("Transactions",monthly.transactions(input$month), 
            icon= icon("list"),
            color = "success")})
  
  output$monthly.revenue <- renderInfoBox({
    infoBox("Revenue", monthly.rev(input$month), 
            icon = icon("credit-card"), color ="success")})
  
  output$monthly.expense <- renderInfoBox({
    infoBox("Expense",monthly.expense(input$month), icon = icon("money-bill"), 
            color = "success")})
  
  output$monthly.gross.profit.percent <- renderInfoBox({
    infoBox("Gross profit %",monthly.avg.gross.profit.percent(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  
  output$monthly.cost.of.sale <- renderInfoBox({
    infoBox("Cost of goods sold",monthly.cost.of.sale(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  output$monthly.items.sold <- renderInfoBox({
    infoBox("Items sold",monthly.item.sales(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  output$monthly.net.profit.percent <- renderInfoBox({
    infoBox("Net profit %", monthly.net.prof.percent(input$month), 
            icon=icon("money-bill"),
            color = "success")
  })
  
  output$monthly.net.profit <- renderInfoBox({
    infoBox("Net profit", monthly.net.prof(input$month), 
            icon=icon("money-bill"),
            color = "success")
  })
  
  output$monthly.gross.profit <- renderInfoBox({
    infoBox("Gross profit", monthly.gross.prof(input$month), 
            icon=icon("money-bill"),
            color = "success")
  })
  
  output$monthly.items.transac <- renderInfoBox({
    infoBox("Items per transaction", monthly.items.transaction(input$month), 
            icon=icon("money-bill"),
            color = "success")
  })
  
  
  output$monthly.bankrec <- renderInfoBox({
    infoBox("Bank", monthly.bank.reconiliation(input$month), 
            icon=icon("money-bill"),
            color = "success")
  })
  
  output$monthly.inventory.closing.bal <- renderInfoBox({
    infoBox("Closing Inventory", monthly.closing.balance(input$month),
            icon=icon("money-bill"),
            color = "success")
  })
  
  # monthly transactions drill-down
  output$daily.visitors <- renderPlot({
    daily.transactions(input$month)
  })
  
  #monthly revenue drill-down
  output$monthly.revenue.prod.cate <- renderTable({
    month.rev.prod.cat(input$month) },colnames= FALSE )
  
  output$monthly.revenue.prod.subcate <- renderDT((
    month.rev.sub.cat(input$month)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales")
  )
  
  
  output$monthly.revenue.product <- renderDT((
    month.rev.prod(input$month)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  #monthly item sales drill-down
  output$monthly.sales.prod.cate <- renderTable((
    month.sales.prod.cat(input$month)), colnames= FALSE  )
  
  output$monthly.sales.prod.subcate <- renderDT((
    month.sales.sub.cat(input$month)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$monthly.sales.product <- renderDT((
    month.sales.prod(input$month)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  
  #-----------------quarter
  
  # qly drill-down 
  # revenue by product category
  qly.rev.prod.cat <- function(x){
    quarter.rev.prod.cat<-  msd1 %>% filter(quarter == x)  %>% 
      group_by(prod_cat)%>%
      summarise(total_sales=sum(total_sale_price))           
    quarter.rev.prod.cat <- quarter.rev.prod.cat[order(
      -quarter.rev.prod.cat$total_sales),]
    quarter.rev.prod.cat}
  
  # revenue by product sub-category
  qly.rev.sub.cat <- function(x){
    quarter.rev.sub.cat<-  msd1 %>% filter(quarter == x)  %>% 
      group_by(sub_cat)%>%
      summarise(total_sales=sum(total_sale_price))     
    quarter.rev.sub.cat<- quarter.rev.sub.cat[order(
      -quarter.rev.sub.cat$total_sales),]
    quarter.rev.sub.cat }    
  
  # revenue by product 
  qly.rev.prod <- function(x){
    quarter.rev.prod<-  msd1 %>% filter(quarter == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=(total_sale_price))    
    quarter.rev.prod <- quarter.rev.prod[order(
      -quarter.rev.prod$total_sales),]
    quarter.rev.prod } 
  
  
  # items sold by product category
  qly.sales.prod.cat <- function(x){
    quarter.prod.cat.sales <-  msd1 %>% filter(quarter == x)  %>% 
      group_by(prod_cat)%>%
      summarise(total_sales=sum(qty))
    quarter.prod.cat.sales }
  
  # items sold by product sub-category
  qly.sales.sub.cat <- function(x){
    (quarter.sales.sub.cat <-  msd1 %>% filter(quarter == x)  %>% 
       group_by(sub_cat)%>%
       summarise(total_sales=sum(qty))
    )
    quarter.sales.sub.cat <- quarter.sales.sub.cat[order(
      -quarter.sales.sub.cat$total_sales),]
    quarter.sales.sub.cat }             
  
  # items sold by product              
  qly.sales.prod <- function(x){      
    quarter.sales.prod <-  msd1 %>% filter(quarter == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=(qty))      
    quarter.sales.prod<- quarter.sales.prod[order(
      -quarter.sales.prod$total_sales),]
    quarter.sales.prod } 
  
  #monthly  transactions per quarter
  quarterly.transactions <- function(x){
    quarter.visitors <- mtd1 %>% filter (quarter == x) %>%
      group_by(month) %>%
      count(month)
    quarter.visitors <- quarter.visitors[order(-quarter.visitors$n),]
    quarter.visitors %>% mutate(month=fct_reorder(month, -n), visitors=n) %>%
      ggplot +
      geom_col(mapping = aes(month,visitors)) }
  
  # items per transaction
  qly.items.transaction <- function(x){
    items.transaction <- round(qly.item.sales(x) /  qly.transactions(x),2)
    items.transaction
  } 
  
  
  
  # opening inventory balance
  qly.open.bal <- function(x){
    open.balance <- mibd[1,3]
    open.balance}
  
  # inventory purchases
  qly.inventory.purchase <- function(x){
    inventory.purchase <- ipa %>% filter(quarter == x) %>%
      summarise(total = sum(amount))
    inventory.purchase }
  
  
  #bank rec
  qly.bank.reconiliation <- function(x){
    bank <- qly.rev(x) - (qly.expense(x)+ qly.inventory.purchase(x))
    round(bank,2) }
  
  #inventory closing balance
  qly.closing.balance <- function(x){
    quarter.closing.bal <- qly.open.bal(x) + qly.inventory.purchase(x) - qly.cost.of.sale(x)
    quarter.closing.bal
    }
  
  # total transactions per quarter
  qly.transactions <- function(x){
    qly.unique.visitors <- mtd1 %>% filter (quarter == x) %>%
      summarise(qly.visitors = n_distinct(transactions)) 
    qly.unique.visitors <- as_tibble(qly.unique.visitors)
    qly.unique.visitors   }  
  
  
  # item sales
  qly.item.sales <- function(x){
    total.sales.quarter <-  msd1 %>% filter(quarter == x)  %>% 
      summarise(total_sales=sum(qty))
    total.sales.quarter  }
  
  
  # net profit percent  
  qly.net.prof.percent <- function(x){
    net.profit.percent <- round( ((qly.rev(x) - (qly.cost.of.sale(x)
                                                 + qly.expense(x)))
                                  /qly.rev(x)) * 100, 2)
    net.profit.percent
  }  
  # net profit    
  qly.net.prof <- function(x){
    net.profit <- (qly.rev(x) - (qly.cost.of.sale(x) 
                                 + qly.expense(x)))
    round(net.profit,2)
  }
  
  # gross profit percent
  qly.avg.gross.profit.percent<- function(x){
    gross.profit.margin <- msd1 %>% filter( quarter == x) %>%
      summarise(avg.gross.profit.margin = round(mean(gross_margin), 2))
    gross.profit.margin  }
  
 
  # expense
  qly.expense <- function(x){
    qly.expense <- med %>% filter( quarter == x) %>%
      summarise( expense= sum(amount))
    qly.expense  }
  
  # revenue
  qly.rev <- function(x){
    total.qly.rev<-  msd1 %>% filter(quarter == x)  %>% 
      summarise(total_sales=sum(total_sale_price))
    total.qly.rev }
  
  # cost of sale
  qly.cost.of.sale <- function(x){
    cost.of.sale <- msd1 %>% filter( quarter == x) %>%
      summarise(cost_of_sale = sum(total_cost_price))
    cost.of.sale  }  
  
  
  #gross profit
  qly.gross.prof <- function(x){
    
    gross.prof <- qly.rev(x) - qly.cost.of.sale(x) 
    round(gross.prof,2) }
  

    
  #output logic
  
  #quarterly
  
  #quarterly revenue drill-down
  output$quarterly.revenue.prod.cate <- renderTable({
    qly.rev.prod.cat(input$quarter) },colnames= FALSE )
  
  output$quarterly.revenue.prod.subcate <- renderDT((
    qly.rev.sub.cat(input$quarter)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  
  output$quarterly.revenue.product <- renderDT((
    qly.rev.prod(input$quarter)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  #monthly item sales drill-down
  output$quarterly.sales.prod.cate <- renderTable((
    qly.sales.prod.cat(input$quarter)), colnames= FALSE)
  
  output$quarterly.sales.prod.subcate <- renderDT((
    qly.sales.sub.cat(input$quarter)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$quarterly.sales.product <- renderDT((
    qly.sales.prod(input$quarter)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  output$monthly.visitors.quarter <- renderPlot({
    quarterly.transactions(input$quarter) })
  
  output$quarterly.items.transac <- renderInfoBox({
    infoBox("Items per transaction", qly.items.transaction(input$quarter), 
            icon=icon("money-bill"),
            color = "success")})
  
  
  output$quarterly.bankrec <- renderInfoBox({
    infoBox("Bank", qly.bank.reconiliation(input$quarter), 
            icon=icon("money-bill"),
            color = "success")})
  
  output$quarterly.inventory.closing.bal <- renderInfoBox({
    infoBox("Closing Inventory", qly.closing.balance(input$quarter),
            icon=icon("money-bill"),
            color = "success")})
  
  output$quarterly.items.sold <- renderInfoBox({
    infoBox("Items sold",qly.item.sales(input$quarter), 
            icon = icon("money-bill"), color = "success")})
  
  output$quarterly.net.profit.percent <- renderInfoBox({
    infoBox("Net profit %", qly.net.prof.percent(input$quarter), 
            icon=icon("money-bill"),color = "success")})
  
  output$quarterly.visitors <- renderInfoBox({
    infoBox("Transactions",qly.transactions(input$quarter), 
            icon= icon("list"),color = "success")})
  
  output$quarterly.net.profit <- renderInfoBox({
    infoBox("Net profit", qly.net.prof(input$quarter), 
            icon=icon("money-bill"),color = "success")})
  
  output$quarterly.gross.profit.percent <- renderInfoBox({
    infoBox("Gross profit %",qly.avg.gross.profit.percent(input$quarter), 
            icon = icon("money-bill"), color = "success")})
  
  output$quarterly.expense <- renderInfoBox({
    infoBox("Expense",qly.expense(input$quarter), icon = icon("money-bill"), 
            color = "success")})
  
  
  
  output$quarterly.revenue <- renderInfoBox({
    infoBox("Revenue", qly.rev(input$quarter), 
            icon = icon("credit-card"), color ="success")})
  
  output$quarterly.cost.of.sale <- renderInfoBox({
    infoBox("Cost of goods sold",qly.cost.of.sale(input$quarter), 
            icon = icon("money-bill"), color = "success")})
  
  output$quarterly.gross.profit <- renderInfoBox({
    infoBox("Gross profit", qly.gross.prof(input$quarter), 
            icon=icon("money-bill"),
            color = "success")})

  
  #YTD
  
  #CALCULATIONS
  
  ytd.rev <- function(x){
    total.ytd.rev<-  msd1 %>% filter(year == x)  %>% 
      summarise(total_sales=sum(total_sale_price))
    total.ytd.rev }
  
  # cost of sale
  ytd.cost.of.sale <- function(x){
    total.cost.of.sale <- msd1 %>% filter( year == x) %>%
      summarise(cost_of_sale = sum(total_cost_price))
    total.cost.of.sale  }
  
  #gross profit
  ytd.gross.prof <- function(x){
    total.gross.prof <- ytd.rev(x) - ytd.cost.of.sale(x) 
    round(total.gross.prof,2) }
  
  #NET PROFIT
  ytd.net.prof <- function(x){
    total.net.profit <- (ytd.rev(x) - (ytd.cost.of.sale(x) + ytd.expense(x)))
    round(total.net.profit,2) }
  
  # expense
  ytd.expense <- function(x){
    total.expense <- med %>% filter( year == x) %>%
      summarise( expense= sum(amount))
    total.expense  }
  
  # gross profit percent
  ytd.avg.gross.profit.percent<- function(x){
    ytd.gross.profit.margin <-  (ytd.rev(x) - ytd.cost.of.sale(x)) / ytd.rev(x)
    ytd.gross.profit.margin <- round(ytd.gross.profit.margin,2) * 100  }
      
  
  # item sales
  ytd.item.sales <- function(x){
    total.sales.ytd <-  msd1 %>% filter(year == x)  %>% 
      summarise(total_sales=sum(qty))
    total.sales.ytd  }
  
  # transactions
  ytd.transactions <- function(x){
    ytd.unique.visitors <- mtd1 %>% filter (year == x) %>%
      summarise(year.visitors = n_distinct(transactions)) 
    ytd.unique.visitors <- as_tibble(ytd.unique.visitors)
    ytd.unique.visitors   }  
  
  # net profit percent  
  ytd.net.prof.percent <- function(x){
    total.net.profit.percent <- round( ((ytd.rev(x) - (ytd.cost.of.sale(x)
                                                       + ytd.expense(x)))
                                        /ytd.rev(x)) * 100, 2)
    total.net.profit.percent}
  
  # items per transaction
  ytd.items.transaction <- function(x){
    total.items.transaction <- round(ytd.item.sales(x) /  ytd.transactions(x),2) 
    total.items.transaction
  } 
  
  
  
  # opening inventory balance
  ytd.open.bal <- function(x){
    open.bal.td <- mibd[1,3] 
    open.bal.td }
  
  # inventory purchases
  ytd.inventory.purchase <- function(x){
    total.inventory.purchase <- ipa %>% filter(year == x) %>%
      summarise(total = sum(amount))
    total.inventory.purchase }
  
  #bank rec
  ytd.bank.reconiliation <- function(x){
    total.bank <- ytd.rev(x) - (ytd.expense(x)+ ytd.inventory.purchase(x))
    round(total.bank,2) }
  
  #inventory closing balance
  ytd.closing.balance <- function(x){
    ytd.open.bal(x) + ytd.inventory.purchase(x) - ytd.cost.of.sale(x)}
  
  #monthly  transactions YTD
  ytd.transactions.month <- function(x){
    ytd.visitors <- mtd1 %>% filter (year == x) %>%
      group_by(month) %>%
      count(month)
    ytd.visitors <- ytd.visitors[order(-ytd.visitors$n),]
    ytd.visitors %>% mutate(month=fct_reorder(month, -n), visitors = n) %>%
      ggplot +
      geom_col(mapping = aes(month,visitors)) }
  
  
  #drill down
  #----------------------------------------------------------
  #        B. YTD sales  by product category
  #------------------------------------------------------      
  ytd.sales.prod.cat <- function(x){(
    ytd.prod.sales.cat<- msd1 %>% filter(year ==x) %>%
      group_by(prod_cat) %>% 
      summarise(total_sales = sum(qty))
  )
    ytd.prod.sales.cat <- ytd.prod.sales.cat[order(
      -ytd.prod.sales.cat$total_sales),]
    ytd.prod.sales.cat }
  
  #--------------------------------------------------------------  
  #       C. YTD sales  by product sub_category     
  #---------------------------------------------------------           
  ytd.sales.sub.cat <- function(x){(
    ytd.prod.sales.sub.cat <- msd1 %>% filter(year ==x) %>%
      group_by(sub_cat) %>%
      summarise(total_sales = sum(qty))
  )
    ytd.prod.sales.sub.cat <-  ytd.prod.sales.sub.cat[order(
      - ytd.prod.sales.sub.cat$total_sales),]
    ytd.prod.sales.sub.cat }
  
  #-----------------------------------------------------------------
  #       D. YTD sales by product     
  #----------------------------------------------------------------          
  ytd.sales.prod <- function(x){(
    ytd.prod.sales <- msd1 %>% filter(year ==x) %>%
      group_by(product,sub_cat) %>% 
      summarise(total_sales = sum(qty)) 
  )
    ytd.prod.sales <- ytd.prod.sales[order(-ytd.prod.sales$total_sales),]
    ytd.prod.sales  }
  
  #----------------------------------------------------------
  #    B. YTD REV by product category
  #--------------------------------------------------------------
  ytd.rev.prod.cat <-function(x){(
    
    ytd.prod.rev.cat <-  msd1 %>% filter(year ==x) %>%
      group_by(prod_cat) %>%
      summarise(total_sales = sum(total_sale_price))
  )
    ytd.prod.rev.cat
    
  }
  
  #------------------------------------------------------------
  #    C. YTD REV by product sub_cat        
  #----------------------------------------------------------- 
  ytd.rev.sub.cat <- function(x){(
    
    ytd.prod.rev.sub.cat <-  msd1 %>% filter(year ==x) %>%
      group_by(sub_cat)%>%
      summarise(total_sales=sum(total_sale_price))
  )
    ytd.prod.rev.sub.cat <- ytd.prod.rev.sub.cat[order(
      -ytd.prod.rev.sub.cat$total_sales),]
    ytd.prod.rev.sub.cat
  }             
  
  #--------------------------------------------------------------
  #    D. YTD REV by product     
  #-------------------------------------------------------------- 
  ytd.rev.prod <- function(x){(
    ytd.prod.rev <-  msd1  %>% filter(year ==x) %>%
      group_by(product,sub_cat)%>%
      summarise(total_sales=sum(total_sale_price))
  )
    ytd.prod.rev <-  ytd.prod.rev[order(- ytd.prod.rev$total_sales),]
    ytd.prod.rev
  }  
  
  #OUTPUT
  output$ytd.revenue <- renderInfoBox({
    infoBox("Revenue",ytd.rev(input$year.ytd), 
            icon = icon("credit-card"), color ="success")})
  
  output$ytd.cost.of.sale <- renderInfoBox({
    infoBox("Cost of goods sold",ytd.cost.of.sale(input$year.ytd), 
            icon = icon("money-bill"), color = "success")})
  
  output$ytd.gross.profit <- renderInfoBox({
    infoBox("Gross profit", ytd.gross.prof(input$year.ytd), 
            icon=icon("money-bill"),
            color = "success")})
  
  output$ytd.net.profit <- renderInfoBox({
    infoBox("Net profit", ytd.net.prof(input$year.ytd), 
            icon=icon("money-bill"),color = "success")})
  
  output$ytd.expense <- renderInfoBox({
    infoBox("Expense",ytd.expense(input$year.ytd), icon = icon("money-bill"), 
            color = "success")}) 
  
  output$ytd.gross.profit.percent <- renderInfoBox({
    infoBox("Gross profit %",ytd.avg.gross.profit.percent(input$year.ytd), 
            icon = icon("money-bill"), color = "success")})
  
  output$ytd.items.sold <- renderInfoBox({
    infoBox("Items sold",ytd.item.sales(input$year.ytd), 
            icon = icon("money-bill"), color = "success")})
  
  output$ytd.net.profit.percent <- renderInfoBox({
    infoBox("Net profit %", ytd.net.prof.percent(input$year.ytd), 
            icon=icon("money-bill"),color = "success")})
  
  output$ytd.visitors <- renderInfoBox({
    infoBox("Transactions",ytd.transactions(input$year.ytd), 
            icon= icon("list"),color = "success")})
  
  output$ytd.items.transac <- renderInfoBox({
    infoBox("Items per transaction", ytd.items.transaction(input$year.ytd), 
            icon=icon("money-bill"),
            color = "success")})
  
  
  output$ytd.bankrec <- renderInfoBox({
    infoBox("Bank", ytd.bank.reconiliation(input$year.ytd), 
            icon=icon("money-bill"),
            color = "success")})
  
  output$ytd.inventory.closing.bal <- renderInfoBox({
    infoBox("Inventory", ytd.closing.balance(input$year.ytd),
            icon=icon("money-bill"),
            color = "success")})
  
  output$ytd.revenue.prod.cate <- renderTable({
    ytd.rev.prod.cat(input$year.ytd) },colnames= FALSE )
  
  output$ytd.revenue.prod.subcate <- renderDT((
    ytd.rev.sub.cat(input$year.ytd)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))  
  
  output$ytd.revenue.product <- renderDT((
    ytd.rev.prod(input$year.ytd)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  #monthly item sales drill-down
  output$ytd.sales.prod.cate <- renderTable((
    ytd.sales.prod.cat(input$year.ytd)), colnames= FALSE)
  
  output$ytd.sales.prod.subcate <- renderDT((
    ytd.sales.sub.cat(input$year.ytd)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$ytd.sales.product <- renderDT((
    ytd.sales.prod(input$year.ytd)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  output$ytd.total.visitors.month <- renderPlot({
    ytd.transactions.month(input$year.ytd) })
  
  
  }
  

shinyApp(ui = ui, server = server)
