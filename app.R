

#library(shiny)
library(bs4Dash)
#library(shinydashboardPlus)
library(tidyverse)
library(ggplot2)
library(DT)
#----------------------------------------------------------
#     IMPORT DATA
#----------------------------------------------------------

msd  <- read_csv("master.data.csv")
med  <- read_csv("expense.account.csv")%>% as_tibble()
mtd  <- read_csv("Traffic.csv") 
mibd <- read.csv("inventory.balance.csv")
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
msd1

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
          h4(p("Welcome to CAPS Pharmacy performance site. The home page 
                provides an overview of the financial and operational 
                performance of the business from a monthy, quarterly and 
                year-to-date view.
                Additional detail is provided in the other tabs for both
                financial and operational data." ))
                  
                  ),
          
          tabItem(tabName = "monthly", 
                  fluidRow(
                    valueBox("MONTHLY DATA OVERVIEW", "",
                             icon = icon("binoculars") , 
                             color = "success", width = 5),
                    
                    box(title = "Month", width = 5 ,status = "success" , 
                        solidHeader = T, height = "100",
                        selectInput(inputId = "month",
                                    label = h6("Select Month"), 
                                    choices = c("NOV_21","DEC_21",
                                                "JAN_22","FEB_22",
                                                "MAR_22","APR_22",
                                                "MAY_22","JUN_22",
                                                "JUL_22","AUG_22",
                                                "SEP_22","OCT_22"), 
                                    selected = "NOV_21",width = "300px")) ),
                  
                  
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
                   infoBoxOutput("monthly.net.profit.percent", width = 4),
                   
                   
                 ),   
                 
                 fluidRow(
                   infoBoxOutput("monthly.inventory.closing.bal", width = 4),
                   infoBoxOutput("monthly.bankrec", width = 4),
                   infoBoxOutput("monthly.items.transac", width = 4),
                   
                   
                 ),
                 
                 fluidRow(
                   
                   bs4Card(title = "Daily visitors",
                           footer = NULL,
                           status = "success",
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
                           elevation = NULL,
                           headerBorder = TRUE,
                           label = NULL,
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           id = "plot.visitore",
                           
                           plotOutput("daily.visitors")
                   )
                 ),
                 
                 fluidRow(
                   tabBox(
                     id = "monthly.rev.data",
                     collapsible = T,
                     elevation = 2,
                     title = "Revenue",
                     selected = "category",
                     status = "primary",
                     solidHeader = FALSE,
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
                     title = "items sold",
                     selected = "Tab 2",
                     status = "primary",
                     solidHeader = FALSE,
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
                       DTOutput("monthly.sales.product")
                       
                     )
                     
                   
                 )
                  
            )
                
               
                 
                 
                  )
          
        
                  
             )
           
      )
      
)
       
   
server <- function(input, output) {
  # Business logic - calculations
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
    
    net.profit <- (monthly.rev(x) - (monthly.cost.of.sale(x) 
                                     + monthly.expense(x)))
    round(net.profit,2)
  }
  
  # items per transaction
  monthly.items.transaction <- function(x){
    items.transaction <- monthly.item.sales(x) /  monthly.transactions(x)
    items.transaction
  } 
  
  #bank rec
  monthly.bank.reconiliation <- function(x){
    bank <- monthly.rev(x) - (monthly.expense(x) 
                              + monthly.inventory.purchase(x))
    round(bank,2) }
  
  #inventory closing balance
  monthly.closing.balance <- function(x){
    monthly.open.bal(x) + monthly.inventory.purchase(x) 
     -  monthly.cost.of.sale(x)  }
  
  
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
    
    
  
  #output logic
  
  
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
  
 
  
  }
  

shinyApp(ui = ui, server = server)
