

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

msd <- msd %>%
  select(!c(12:19))  # delete empty columns
# create new , reorganize and delete variables
msd1 <- msd%>%
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
       
       tabItem(tabName = "home", "home data"),
       
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
                         selectInput(inputId = "year",
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
                   selected = "category",
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
                                      label = h6("Select Month"), 
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
                         selectInput(inputId = "year",
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
             infoBoxOutput("quarterly.items.transac", width = 4),
             infoBoxOutput("quarterly.inventory.closing.bal", width = 4),
             infoBoxOutput("quarterly.bankrec", width = 4)
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
       
       
       tabItem(tabName = "YTD","ytd adat", 
               
               fluidRow(
                 valueBox("YEAR-TO-DATE DATA OVERVIEW", "",
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
                                     label = h6("Select Month"), 
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
                         selectInput(inputId = "year",
                                     label = h6("Select Year"), 
                                     choices = c(2021,2022,2023,2024,2025), 
                                     selected = 2021 ,width = "200px"))
                 
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
    open.bal[,2] }
  
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
    
    visitors %>% mutate(Date=fct_reorder(Date, -n)) %>%
      ggplot +
      geom_col(mapping = aes(Date,n)) }
  
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
    infoBox("Inventory", monthly.closing.balance(input$month),
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
    quarter.visitors %>% mutate(month=fct_reorder(month, -n)) %>%
      ggplot +
      geom_col(mapping = aes(month,n)) }
  
  # items per transaction
  qly.items.transaction <- function(x){
    items.transaction <- round(qly.item.sales(x) /  qly.transactions(x),2)
    items.transaction
  } 
  
  
  
  # opening inventory balance
  qly.open.bal <- function(x){
    open.balance <- mibd[1,2]
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
    infoBox("Inventory", qly.closing.balance(input$quarter),
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
  

  }
  

shinyApp(ui = ui, server = server)
