

library(shiny)
library(bs4Dash)
#library(shinydashboardPlus)
library(tidyverse)
library(ggplot2)
library(DT)
#----------------------------------------------------------
#     IMPORT DATA
#----------------------------------------------------------

msd <- read_csv("master.data.csv")
med <- read_csv("expense.account.csv")%>% as_tibble()
mtd <- read_csv("Traffic.csv") 

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
      case_when(str_sub(Date,1,1) == 1 ~ 'JAN_22', str_sub(Date,1,1) == 2 ~ 'FEB_22',
                str_sub(Date,1,1) == 3 ~ 'MAR_22', str_sub(Date,1,1) == 4 ~ 'APR_22',
                str_sub(Date,1,1) == 5 ~ 'MAY_22', str_sub(Date,1,1) == 6 ~ 'JUN_22',
                str_sub(Date,1,1) == 7 ~ 'JUL_22', str_sub(Date,1,1) == 8 ~ 'AUG_22',
                str_sub(Date,1,1) == 9 ~ 'SEP_22', str_sub(Date,1,1) == 10 ~'OCT_22',
                str_sub(Date,1,1) == 11 ~ 'NOV_21', str_sub(Date,1,1) == 12 ~ 'DEC_21'
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
          
         menuItem("HOME", tabName = "home", icon = icon("home", lib="font-awesome")), 
         
         menuItem("MONTHLY VIEW", tabName = "monthly", icon = icon("binoculars", lib = "font-awesome")), 
                  
         menuItem("QUARTERLY VIEW", tabName = "quarter",icon = icon("binoculars", lib = "font-awesome")),  
                  
         
         menuItem("YEAR TO DATE VIEW", tabName = "YTD", icon = icon("binoculars", lib = "font-awesome"))
         
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          tabItem(tabName = "home", h4(p("Welcome to CAPS Pharmacy performance site. The home page 
                                          provides an overview of the financial and operational 
                                          performance of the business from a monthy, quarterly and 
                                          year-to-date view.
                                           Additional detail is provided in the other tabs for both
                                           financial and operational data." ))
                  
                  ),
          
          tabItem(tabName = "monthly", 
                  
                  fluidRow(
                    
                    valueBox("MONTHLY DATA OVERVIEW", "",icon = icon("binoculars") , color = "success", width = 5),
                    
                    box(title = "Month", width = 5 ,status = "success" , solidHeader = T, height = "100",
                        selectInput(inputId = "month", label = h6("Select Month"), 
                                    choices = c("NOV_21","DEC_21","JAN_22","FEB_22",
                                                "MAR_22","APR_22","MAY_22","JUN_22",
                                                "JUL_22","AUG_22","SEP_22","OCT_22"), 
                                    selected = "NOV_21",width = "300px")) ),
                  
                  
                 fluidRow(
                   
                   infoBoxOutput("monthly.revenue", width = 4),
                   infoBoxOutput("monthly.cost.of.sale", width = 4),
                   infoBoxOutput("monthly.gross.profit.margin", width = 4)
                   
                   ),
                  
                 fluidRow(
                   
                   infoBoxOutput("monthly.expense", width = 4),
                   infoBoxOutput("monthly.items.sold", width = 4),
                   infoBoxOutput("monthly.visitors", width = 4)
                   
                 ),
                 
                 fluidRow(
                   
                   infoBoxOutput("monthly.net.profit", width = 4),
                   infoBoxOutput("monthly.closing.inventory", width = 4),
                   infoBoxOutput("monthly.closing.bankrec", width = 4)
                   
                 )   
                 
                 
                 
                 
                  )
          
        
                  
                  )
           
      )
      
)
       
   
server <- function(input, output) {
  # Business logic
  #--------
  # Monthly overview  data output
  #--------
  monthly.rev <- function(x){
    total.monthly.rev<-  msd1 %>% filter(month == x)  %>% 
      summarise(total_sales=sum(total_sale_price))
       total.monthly.rev }
  
  monthly.item.sales <- function(x){
    total.sales.month <-  msd1 %>% filter(month == x)  %>% 
      summarise(total_sales=sum(qty))
        total.sales.month  }
  
  monthly.transactions <- function(x){
    monthly.unique.visitors <- mtd1 %>% filter (month == x) %>%
      summarise(monthly.visitors = n_distinct(transactions)) 
       monthly.unique.visitors <- as_tibble(monthly.unique.visitors)
          monthly.unique.visitors   }
  
  monthly.expense <- function(x){
    monthly.expense <- med %>% filter( month == x) %>%
      summarise( expense= sum(amount))
        monthly.expense  }
  
  monthly.avg.gross.margin <- function(x){
     gross.profit.margin <- msd1 %>% filter( month == x) %>%
      summarise(avg.gross.profit.margin = round(mean(gross_margin), 2))
        gross.profit.margin  }
  
  monthly.cost.of.sale <- function(x){
    cost.of.sale <- msd1 %>% filter( month == x) %>%
      summarise(cost_of_sale = sum(total_cost_price))
      cost.of.sale  }
  
  monthly.net.prof <- monthly.rev(x) - 
    (monthly.cost.of.sale(x) + monthly.expense(x))
    
    
    
  
  
  
  
  
  #output logic
  
  
  output$monthly.visitors <- renderInfoBox({
    infoBox("Transactions",monthly.transactions(input$month), icon= icon("list"),
            color = "success")})
  
  output$monthly.revenue <- renderInfoBox({
    infoBox("Revenue", monthly.rev(input$month), 
             icon = icon("credit-card"), color ="success")})
  
  output$monthly.expense <- renderInfoBox({
    infoBox("Expense",monthly.expense(input$month), icon = icon("money-bill"), 
            color = "success")})
  
  output$monthly.gross.profit.margin <- renderInfoBox({
    infoBox("Gross Margin",monthly.avg.gross.margin(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  
  output$monthly.cost.of.sale <- renderInfoBox({
    infoBox("Cost of goods sold",monthly.cost.of.sale(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  output$monthly.items.sold <- renderInfoBox({
    infoBox("Items sold",monthly.item.sales(input$month), 
            icon = icon("money-bill"), color = "success")})
  
  output$monthly.net.profit <- renderInfoBox({
    infoBox("Net profit", monthly.net.prof, icon=icon("money-bill"),
            color = "success")
  })
  
  
  
  
  
  
  
  
  
  
  
  }
  

shinyApp(ui = ui, server = server)
