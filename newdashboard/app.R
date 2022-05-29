

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(ggplot2)
library(DT)
#----------------------------------------------------------
#     IMPORT DATA
#----------------------------------------------------------

msd <- read_csv("master.data.csv")


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

#-----------------------------------------------------------
#     GENERATING BUSINESS INSIGHT
#-----------------------------------------------------------
#     Sales Volume Analysis
#
#------------------------------------------------------------
#      1. Monthly Volume Analysis
#-----------------------------------------------------------
#         A. Monthly sales by product category
#            
#-----------------------------------------------------------

month.sales.prod.cat <- function(x){
  
  monthly.prod.cat.sales <-  msd1 %>% filter(month == x)  %>% 
    group_by(prod_cat)%>%
    summarise(total_sales=sum(qty))
  monthly.prod.cat.sales
  
}

total.monthly.sales <- function(x){
  total.sales.month <-  msd1 %>% filter(month == x)  %>% 
  summarise(total_sales=sum(qty))
total.sales.month

}


#--------------------------------------------------------------
#         B. Monthly sales by product sub_category
#            
#-------------------------------------------------------------- 
month.sales.sub.cat <- function(x){
  
  (monthly.sales.sub.cat <-  msd1 %>% filter(month == x)  %>% 
     group_by(sub_cat)%>%
     summarise(total_sales=sum(qty))
  )
  monthly.sales.sub.cat <- monthly.sales.sub.cat[order(
                                    -monthly.sales.sub.cat$total_sales),]
  monthly.sales.sub.cat 
    
}             

 

#--------------------------------------------------------------
#         C. Monthly sales by product 
#             
#--------------------------------------------------------------                
month.sales.prod <- function(x){
  
  (monthly.sales.prod <-  msd1 %>% filter(month == x)  %>% 
     group_by(product,sub_cat)%>%
     summarise(total_sales=(qty))
  )
  monthly.sales.prod<- monthly.sales.prod[order(
                                      -monthly.sales.prod$total_sales),]
  monthly.sales.prod
}             

#-----------------------------------------------------------
#     4. Monthly Revenue Analysis
#-----------------------------------------------------------
#     A.  Monthly sales by product category
#           
#-----------------------------------------------------------

month.rev.prod.cat <- function(x){(
  
  monthly.rev.prod.cat<-  msd1 %>% filter(month == x)  %>% 
    group_by(prod_cat)%>%
    summarise(total_sales=sum(total_sale_price))
)
  monthly.rev.prod.cat <- monthly.rev.prod.cat[order(-monthly.rev.prod.cat$total_sales),]
  monthly.rev.prod.cat
}


#--------------------------------------------------------------
#     B. Monthly sales by product sub_category
#         
#-------------------------------------------------------------- 
month.rev.sub.cat <- function(x){(
  
  monthly.rev.sub.cat<-  msd1 %>% filter(month == x)  %>% 
    group_by(sub_cat)%>%
    summarise(total_sales=sum(total_sale_price))
)
  monthly.rev.sub.cat<- monthly.rev.sub.cat[order(-monthly.rev.sub.cat$total_sales),]
  monthly.rev.sub.cat
}        


#--------------------------------------------------------------
#     C.  Monthly sales by product 
#         
#--------------------------------------------------------------                
month.rev.prod <- function(x){(
  
  monthly.rev.prod<-  msd1 %>% filter(month == x)  %>% 
    group_by(product,sub_cat)%>%
    summarise(total_sales=(total_sale_price))
)
  monthly.rev.prod <- monthly.rev.prod[order(-monthly.rev.prod$total_sales),]
  monthly.rev.prod
}  



#--------------------------------------------------------------
#     D.  Monthly sales by product 
#         
#--------------------------------------------------------------                

total.monthly.rev <- function(x){(
  
  monthly.rev<-  msd1 %>% filter(month == x)  %>% 
    
    summarise(total_sales=sum(total_sale_price))
)
  monthly.rev <- monthly.rev[order(-monthly.rev$total_sales),]
  monthly.rev
}

#--------------------------------------------------------------
#    A. total quarterly sales  
#         
#--------------------------------------------------------------

total.q.sales <- function(x){
  
  total.qly.sales<-  msd1 %>% filter(quarter == x)  %>% 
    summarise(total_sales=sum(qty))
  total.qly.sales
}



#         B. Quarterly sales volume by product category     
#             
#-------------------------------------------------------------- 


q.sales.prod.cat <- function(x){
  
  qly.sales.prod.cat<-  msd1 %>% filter(quarter == x)  %>% 
    group_by(prod_cat)%>%
    summarise(total_sales=sum(qty))
  qly.sales.prod.cat
}




#------------------------------------------------------------
#         C. Quarterly sales by product sub_cat
#             
#----------------------------------------------------------- 
q.sales.sub.cat <- function(x){
  
  ( qly.sales.sub.cat<-  msd1 %>% filter(quarter == x)  %>% 
      group_by(sub_cat)%>%
      summarise(total_sales=sum(qty))
  )
  qly.sales.sub.cat <- qly.sales.sub.cat[order(-qly.sales.sub.cat$total_sales),]
  qly.sales.sub.cat
} 



#--------------------------------------------------------------
#        D. Quarterly sales by product 
#          
#--------------------------------------------------------------    
q.sales.prod <- function(x){
  
  ( qly.sales.prod  <-  msd1 %>% filter(quarter == x)  %>% 
      group_by(product,sub_cat)%>%
      summarise(total_sales=sum(qty))
  )
  qly.sales.prod  <- qly.sales.prod[order(-qly.sales.prod$total_sales),]
  qly.sales.prod
}  

#------------------------------------------------
#    A. Total Quarterly revenue 
#-------------------------------------------------------------- 
total.q.rev <- function(x){(
  
  qtly.rev <-  msd1 %>% filter(quarter == x)  %>% 
    summarise(total_sales=sum(total_sale_price))
)
  qtly.rev
}


#----------------------------------------------------------
#    B. Quarterly sales by product category       
#-------------------------------------------------------------- 
q.rev.prod.cat <- function(x){(
  
  qtly.rev.prod.cat <-  msd1 %>% filter(quarter == x)  %>% 
    group_by(prod_cat)%>%
    summarise(total_sales=sum(total_sale_price))
)
  qtly.rev.prod.cat
}

#------------------------------------------------------------
#    C. Quarterly sales by product sub_cat
#       1.filter data by quarter
#       2.group by sub_cat 
#----------------------------------------------------------- 
q.rev.sub.cat <- function(x){
  
  (qtly.rev.sub.cat <-  msd1 %>% filter(quarter == x)  %>% 
     group_by(sub_cat)%>%
     summarise(total_sales=sum(total_sale_price))
  )
  qtly.rev.sub.cat <- qtly.rev.sub.cat[order(-qtly.rev.sub.cat$total_sales),]
  qtly.rev.sub.cat
}             


#--------------------------------------------------------------
#    D. Quarterly sales by product 
#--------------------------------------------------------------       
 
q.rev.prod <- function(x){(
  
  qtly.sales.rev.prod <-  msd1 %>% filter(quarter == x)  %>% 
    group_by(product,sub_cat)%>%
    summarise(total_sales=sum(total_sale_price))
)
  qtly.sales.rev.prod <-  qtly.sales.rev.prod[order(
                                   - qtly.sales.rev.prod$total_sales),]
  qtly.sales.rev.prod
}         

#----------------------------------------------------------
#        A. YTD total sales
#----------------------------------------------          
total.ytd.sales <- function(){(
  ytd.sales<- msd1 %>%      
    summarise(total_sales = sum(qty))
)
  
  ytd.sales
}

#----------------------------------------------------------
#        B. YTD sales  by product category
#------------------------------------------------------      
ytd.sales.prod.cat <- function(){(
  ytd.prod.sales.cat<- msd1 %>% 
    group_by(prod_cat) %>% 
    summarise(total_sales = sum(qty))
)
  ytd.prod.sales.cat <- ytd.prod.sales.cat[order(
                                           -ytd.prod.sales.cat$total_sales),]
  ytd.prod.sales.cat
}



#--------------------------------------------------------------  
#       C. YTD sales  by product sub_category     
#---------------------------------------------------------           
ytd.sales.sub.cat <- function(){(
  ytd.prod.sales.sub.cat <- msd1 %>% 
    group_by(sub_cat) %>%
    summarise(total_sales = sum(qty))
)
  ytd.prod.sales.sub.cat <-  ytd.prod.sales.sub.cat[order(
                                        - ytd.prod.sales.sub.cat$total_sales),]
  ytd.prod.sales.sub.cat
}

#-----------------------------------------------------------------
#       D. YTD sales by product     
#----------------------------------------------------------------          
ytd.sales.prod <- function(){(
  ytd.prod.sales <- msd1 %>% 
    group_by(product,sub_cat) %>% 
    summarise(total_sales = sum(qty)) 
)
  ytd.prod.sales <- ytd.prod.sales[order(-ytd.prod.sales$total_sales),]
  ytd.prod.sales
}


#----------------------------------------------------------
#    A. YTD TOTAL REV    
#      
#--------------------------------------------------------------
total.ytd.rev <-function(){(
  
  total.rev <-  msd1 %>% 
    
    summarise(total_sales = sum(total_sale_price))
)
  total.rev
  
}

#----------------------------------------------------------
#    B. YTD REV by product category
#--------------------------------------------------------------
ytd.rev.prod.cat <-function(){(
  
  ytd.prod.rev.cat <-  msd1 %>% 
    group_by(prod_cat) %>%
    summarise(total_sales = sum(total_sale_price))
)
  ytd.prod.rev.cat
  
}

#------------------------------------------------------------
#    C. YTD REV by product sub_cat        
#----------------------------------------------------------- 
ytd.rev.sub.cat <- function(){(
  
  ytd.prod.rev.sub.cat <-  msd1 %>% 
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
ytd.rev.prod <- function(){(
  ytd.prod.rev <-  msd1  %>% 
    group_by(product,sub_cat)%>%
    summarise(total_sales=sum(total_sale_price))
)
  ytd.prod.rev <-  ytd.prod.rev[order(- ytd.prod.rev$total_sales),]
  ytd.prod.rev
}  
#------------------------------------
#  Dashboard UI
#------------------------------------
ui <- dashboardPage(
  
  
      dashboardHeader(title = "CAPS PHARMACY"),
      
      dashboardSidebar(
        
        sidebarMenu(
         menuItem("HOME", tabName = "home", icon = icon("home", lib="font-awesome")),    
         menuItem("MONTHLY VIEW", tabName = "month", icon = icon("binoculars", lib = "font-awesome"), 
                  menuSubItem("ITEM SALES", tabName="item_sales",icon = icon("capsules", lib = "font-awesome")),
                  menuSubItem("SALES REVENUE", tabName = "sales_revenue", icon = icon("credit-card", lib = "font-awesome"))),
         
         menuItem("QUARTERLY VIEW", tabName = "quarter",icon = icon("binoculars", lib = "font-awesome"),  
                  menuSubItem("ITEM SALES", tabName="item_sales_quarter",icon = icon("capsules", lib = "font-awesome")),
                  menuSubItem("SALES REVENUE", tabName = "sales_revenue_quarter", icon = icon("credit-card", lib = "font-awesome"))),
         
         menuItem("YEAR TO DATE VIEW", tabName = "YTD", icon = icon("binoculars", lib = "font-awesome"), 
                  menuSubItem("ITEM SALES", tabName="item_sales_ytd",icon = icon("capsules", lib = "font-awesome")),
                  menuSubItem("SALES REVENUE", tabName = "sales_revenue_ytd", icon = icon("credit-card", lib = "font-awesome")))
         
         
        )
      ),
      
      dashboardBody(
        
        tabItems(
          tabItem(tabName = "home", h4(p("Welcome to CAPS Pharmacy performance site 
                                      where you can drill down to the business 
                                      peformance from monthly to year to date data analysis." 
                                      
                                      ))),
          tabItem(tabName = "item_sales", h3("SALES VOLUME ANALYSIS"),
                  
                  fluidRow(
                    
                    box(title = "Month", width = 3 ,status = "primary" , solidHeader = T,
                        selectInput(inputId = "month", label = h5("Select Month"), 
                                    choices = c("NOV_21","DEC_21","JAN_22","FEB_22",
                                                "MAR_22","APR_22","MAY_22","JUN_22",
                                                "JUL_22","AUG_22","SEP_22","OCT_22"), 
                                    selected = "NOV_21",width = "120px")),
                    
                    box(title = "Total Monthly Sales",status = "primary" , solidHeader = T, width =4, 
                        tableOutput("monthly.sales")),
                    
                    box(title = "Sales By Product Category",status = "primary" , solidHeader = T,width =5, 
                        tableOutput("monthly.sales.prod.cate"))
                  ),
                  
                  fluidRow(                   
                    box(title = "Sales By  Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                        DTOutput("monthly.sales.prod.subcate")),
                    
                    box(title = "Sales By Product ",status = "primary" , solidHeader = T,width =7,
                        DTOutput("monthly.sales.product"))
                    
                  )
                  
                  ),
          
         tabItem(tabName = "sales_revenue",h3("SALES REVENUE ANALYSIS"),
               
                 fluidRow(
                   
                   box(title = "Month",status = "primary" , solidHeader = T, width = 3,
                       selectInput(inputId = "month.rev", label = h5("Select Month"), 
                                   choices = c("NOV_21","DEC_21","JAN_22","FEB_22",
                                               "MAR_22","APR_22","MAY_22","JUN_22",
                                               "JUL_22","AUG_22","SEP_22","OCT_22"), 
                                   selected = "NOV_21",width = "120px")),
                  
                   
                   box(title = "Total Monthly Revenue",status = "primary" , solidHeader = T, width =4, 
                       tableOutput("monthly.revenue")),
                   
                   box(title = "Revenue By Product Category",status = "primary" , solidHeader = T,width =5, 
                       tableOutput("monthly.revenue.prod.cate"))
                   
                     ),
                   
                   fluidRow(
                   
                   box(title = "Revenue By Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                       DTOutput("monthly.revenue.prod.subcate")),
                   
                   box(title = "Revenue By Product",status = "primary" , solidHeader = T,width =7,
                       DTOutput("monthly.revenue.product"))
                 
                 )
        
                 ),
         
         
         tabItem( tabName="item_sales_quarter",
                  fluidRow(
                    box(title = "Quarter", status = "primary" , solidHeader = T,width =3,
                      selectInput(inputId = "quarter.sales", label = h5("Select Quarter"), 
                                  choices = c(1,2,3,4), selected = 1, width = "120px")),
                      
                      box(title = "Total Quarterly Sales",status = "primary" , solidHeader = T,width = 4, 
                          tableOutput("quarterly.sales")),
                      
                      box(title = "Quarterly Sales by Product Category",status = "primary" , solidHeader = T,width = 5,
                          tableOutput("quarterly.sales.prod.cate"))  
                    ),
                  
                  
                  fluidRow(
                    
                    box(title = "Quarterly Sales by Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                        DTOutput("quarterly.sales.prod.subcate")),
                        
                    
                    box(title = "Sales by Product ",status = "primary" , solidHeader = T,width =7,
                        DTOutput("quarterly.sales.product"))
                    
                  )),
         
         tabItem(tabName = "sales_revenue_quarter",
                 fluidRow(
                   box(title = "Quarter",status = "primary" , solidHeader = T, width = 3,
                     selectInput(inputId = "quarter.revenue", label = h5("Select Quarter"),
                                 choices = c(1,2,3,4), selected = 1, width = "120px")),
                     
                     
                     box(title = "Total Quarterly Revenue", status = "primary" , solidHeader = T,width =4,
                         tableOutput("quarterly.rev")),
                     
                     box(title = "Quarterly Revenue By Product Category",status = "primary" , solidHeader = T,width =5,
                         tableOutput("quarterly.rev.prod.cate")) 
                     
                   ),
                 
                 fluidRow(
                  
                   
                   box(title = "Quarterly Revenue By Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                       DTOutput("quarterly.rev.prod.subcate")),
                   
                   
                   box(title = "Quarterly Revenue By Product ",status = "primary" , solidHeader = T,width =7,
                       DTOutput("quarterly.rev.product"))
                   
                 )
                 
                 ),
         
         
         tabItem( tabName="item_sales_ytd",
                  
                 
                  fluidRow(
                    
                    box(title = "Total YTD Sales",status = "primary" , solidHeader = T, width =3,
                        tableOutput("ytd.sales")),
                    
                    box(title = "YTD Sales By Product Category",status = "primary" , solidHeader = T,width =5,
                        tableOutput("ytd.sales.prod.cate"))
                    ), 
                    
                  fluidRow(
                    
                    box(title = "YTD Sales By Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                        DTOutput("ytd.sales.prod.subcate")),
                    
                    
                    box(title = "YTD Sales By Product ",status = "primary" , solidHeader = T,width =7,
                        DTOutput("ytd.sales.product"))
                    
                  )),
         
         tabItem( tabName = "sales_revenue_ytd",
                  
                  fluidRow(
                    
                    box(title = "Total YTD Revenue", status = "primary" , solidHeader = T,width =3,
                        tableOutput("ytd.rev")),
                    
                    box(title = "YTD Revenue By Product Category",status = "primary" , solidHeader = T,width =5,
                        tableOutput("ytd.rev.prod.cate"))),
                  
                  
                  fluidRow(box(title = "YTD Revenue By Product Sub-Category",status = "primary" , solidHeader = T,width =5,
                               DTOutput("ytd.rev.prod.subcate")),
                           
                           
                           box(title = " YTD Revenue By Product",status = "primary" , solidHeader = T,width =7,
                               DTOutput("ytd.rev.product")))
                  
                  
                  )
           
      )
      
)
       
)   
server <- function(input, output) {
  
#------------------
# monthly item volume data output
#---------------------  
  output$monthly.sales <- renderTable((
    total.monthly.sales(input$month)) ,colnames= FALSE )
  
  
  output$monthly.sales.prod.cate <- renderTable((
    month.sales.prod.cat(input$month)), colnames= FALSE  )
  
  
  
  output$monthly.sales.prod.subcate <- renderDT((
    month.sales.sub.cat(input$month)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  
  output$monthly.sales.product <- renderDT((
    month.sales.prod(input$month)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  
#------------------
# monthly revenue data output
#---------------------  
  
  output$monthly.revenue <- renderTable((
    
    total.monthly.rev(input$month.rev)), colnames= FALSE   )
  
  output$monthly.revenue.prod.cate <- renderTable({
    month.rev.prod.cat(input$month.rev) },colnames= FALSE )
  
  output$monthly.revenue.prod.subcate <- renderDT((
    month.rev.sub.cat(input$month.rev)), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales")
    )
  
  
  output$monthly.revenue.product <- renderDT((
    month.rev.prod(input$month.rev)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))



#------------------
# quarterly sales volume data output
#---------------------  

  output$quarterly.sales <- renderTable({
    total.q.sales(input$quarter.sales) },colnames= FALSE)
  
  output$quarterly.sales.prod.cate <- renderTable({
    q.sales.prod.cat(input$quarter.sales) },colnames= FALSE)
  
  
  output$quarterly.sales.prod.subcate <- renderDT((
    q.sales.sub.cat(input$quarter.sales) ), options = list(pageLength = 8)
    ,colnames = c("Sub-Category","Total Sales"))
  
  output$quarterly.sales.product <- renderDT((
    q.sales.prod(input$quarter.sales)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  

#------------------
# quarterly revenue data output
#---------------------  

  output$quarterly.rev <- renderTable({
    total.q.rev(input$quarter.revenue) },colnames= FALSE)
  
  output$quarterly.rev.prod.cate <- renderTable({
    q.rev.prod.cat(input$quarter.revenue) },colnames= FALSE)
  
  
  output$quarterly.rev.prod.subcate <- renderDT((
    q.rev.sub.cat(input$quarter.revenue) ), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$quarterly.rev.product <- renderDT((
    q.rev.prod(input$quarter.revenue)), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))
  
  #------------------
  # YTD SALES  data output
  #---------------------  
  
  output$ytd.sales <- renderTable({
    total.ytd.sales()},colnames= FALSE)
  
  output$ytd.sales.prod.cate <- renderTable({
    ytd.sales.prod.cat()},colnames= FALSE)
  
  
  output$ytd.sales.prod.subcate <- renderDT((
    ytd.sales.sub.cat()), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$ytd.sales.product <- renderDT((
    ytd.sales.prod()), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))  
  
  #------------------
  # YTD REVENUE  data output
  #---------------------  
  
  output$ytd.rev <- renderTable({
    total.ytd.rev()},colnames= FALSE)
  
  output$ytd.rev.prod.cate <- renderTable({
    ytd.rev.prod.cat()},colnames= FALSE)
  
  
  output$ytd.rev.prod.subcate <- renderDT((
    ytd.rev.sub.cat()), options = list(pageLength = 8),
    colnames = c("Sub-Category","Total Sales"))
  
  output$ytd.rev.product <- renderDT((
    ytd.rev.prod()), options = list(pageLength = 8),
    colnames = c("Product","Sub-Category","Total Sales"))   
}
shinyApp(ui = ui, server = server)
