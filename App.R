# Load packages ----
library(tidyverse)
library(lvplot)
library(markdown)
house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')

house <- house |> 
  rename(Age_Group =`Age Group`)

States <- house|> 
  distinct(State)|>
  pull(State)

Zones <- house|> 
  distinct(Zone)|>
  pull(Zone)

Age_Groups <- c("0-17","18-25","26-35","36-45","46-50","51-55","55+")

Prop_female<-sum(house$Gender=="F")/length(house$Gender)

# User interface ----
ui <- navbarPage("Diwali Sales",
                 
                
                 

# Tab panel gender frequency ----------------------------------------------


  tabPanel("Gender frequency",
        sidebarLayout(
          sidebarPanel(
            p(style="text-align: justify;",
              "Overall there are significantly more female costumers. 
              The age group for whch this difference is less pronounced is the \"0-17\" one.
              For this age group in some states such as Delhi there are more male costumers than female ones."),
              selectInput("State", 
                  label = "Choose a Sate",
                  choices = c("All" = regex(".*"),
                              States), 
                    selected = "All"),


              
                selectInput("Age_Group", 
                    label = "Choose an age group",
                    choices = c("All" = regex(".*"),
                              Age_Groups),
                    selected = "All"),
                              ),
            mainPanel(
                plotOutput("plot1")
                )
              )
            ),
               

# Tab panel gender and amount spent ---------------------------------------


  tabPanel("Gender and amount spent",
             sidebarLayout(
               sidebarPanel(
                 p(style="text-align: justify;",
                   "Overall the amount spent is similar between males and females.
                   There are some differences looking at particular state and age group combinations."),
                 selectInput("State2", 
                             label = "Choose a Sate",
                             choices = c("All" = regex(".*"),
                                         States), 
                             selected = "All"),
                 
                 selectInput("Age_Group2", 
                             label = "Choose an age group",
                             choices = c("All" = regex(".*"),
                                         Age_Groups),
                             selected = "All"),
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )
           ),           
         


# Tab panel occupation and amount spent -----------------------------

  tabPanel("Occupation and amount spent",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the costumers working in the Governament and Media sector seem to spend on average 
               the highest amount, while the ones working in construction the lowest.    
               There are significant differences looking at particular state, age group and gender combinations.
               The vertical lines indicate the 25, 50 and 75 quantiles."),
             selectInput("State3", 
                         label = "Choose a Sate",
                         choices = c("All" = regex(".*"),
                                     States <- house|> 
                                       distinct(State)|>
                                       pull(State)), 
                         selected = "All"),
             
             selectInput("Age_Group3", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
             
             selectInput("Gender1", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                    "Female" = "F",
                                      "Male" ="M"),
                         selected = "All"),
             
             
           ),
           mainPanel(
             plotOutput("plot3")
           )
         )
),     
      


# Tab panel age group and amount spent -----------------------------------


  tabPanel("Age group and amount spent",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the costumers working in the age groups \"51-55\" and \"55+\" Media  seem to spend on average 
               the highest amount, while the ones in the age groups \"0-17\" and \"18-25\"working in construction the lowest.    
               There are significant differences looking at particular state and gender combinations.
               The vertical lines indicate the 25, 50 and 75 quantiles."),
             selectInput("State4", 
                         label = "Choose a Sate",
                         choices = c("All" = regex(".*"),
                                     States), 
                         selected = "All"),
             
             
             selectInput("Gender2", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                     "Female" = "F",
                                     "Male" ="M"),
                         selected = "All"),
             
             
           ),
           mainPanel(
             plotOutput("plot4")
           )
         )
),    




# Tab panel geographic zone and amount spent ------------------------------


tabPanel("Geographic zone and amount spent",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the costumers from the Central and Southern zones seem to spend on average 
               the highest amount, while the ones in the Eastern the lowest.    
               There are some differences looking at particular age group and gender combinations.
               The vertical lines indicate the 25, 50 and 75 quantiles."),
          
             selectInput("Age_Group4", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
             
             selectInput("Gender3", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                     "Female" = "F",
                                     "Male" ="M"),
                         selected = "All"),
             
             
           ),
           mainPanel(
             plotOutput("plot5")
           )
         )
), 



# Tab panel state and amount spent ----------------------------------------


  tabPanel("State and amount spent",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the costumers from the Dehli seem to spend on average 
               the highest amount, while the ones from Uttarakhand the lowest.    
               There are some differences looking at particular age group and gender combinations
                The vertical lines indicate the 25, 50 and 75 quantiles."),
             
             selectInput("Age_Group5", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
             
             selectInput("Gender4", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                     "Female" = "F",
                                     "Male" ="M"),
                         selected = "All"),
             
             
           ),
           mainPanel(
             plotOutput("plot6")
           )
         )
),



# Tab panel proportion of female costumers per product category -----------


tabPanel("Proportion of female costumers per product category",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "The red line indicates the average proportion of female costumers among product categoreis 
               weigthed by the number of costumers for each category. Overall there are some category, such as \"Decor\",
               for which the proportion of female costumers is higher than expected and other, such as \"Hand & Power Tools\",
               for which is lower.There are some differences looking at particular age group and state combinations."),
             
             
             selectInput("Age_Group6", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
             
             selectInput("State5", 
                         label = "Choose a Sate",
                         choices = c("All" = regex(".*"),
                                     States), 
                         selected = "All"),
           ),
           mainPanel(
             plotOutput("plot7")
           )
         )
),  

# Tab panel geographic zone and product category  -----------------------------


tabPanel("Geographic zone and product category",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the most frequent product categories are  \"Clothing & Apparel\"  and \"Food\".
               There are some differences looking at particular age group and gender combinations."
               ),
             
             selectInput("Zone", 
                         label = "Choose a geographic zone",
                         choices = c("All" = regex(".*"),
                                     Zones),
                         selected = "All"),
             
             selectInput("Age_Group7", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
             
             selectInput("Gender5", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                     "Female" = "F",
                                     "Male" ="M"),
                         selected = "All")
             
             
           ),
           mainPanel(
             plotOutput("plot8")
           )
         )
    ),

 


# Tab panel state and product category  -----------------------------


  tabPanel("State and product category",
         sidebarLayout(
           sidebarPanel(
             p(style="text-align: justify;",
               "Overall the most frequent product categories are  \"Clothing & Apparel\"  and \"Food\".
               It is intersting to note how in Dehli where the average amount spent by costumer is the highest the most common
               product category is \"Footwear and shoes\", while in Uttarakhand, where the amount spent is the lowest,
               almost half of the products bought are from the  \"Clothing & Apparel\" category"
             ),
             selectInput("State6", 
                         label = "Choose a state",
                         choices = c("All" = regex(".*"),
                                     States),
                         selected = "All"),
             
             selectInput("Age_Group8", 
                         label = "Choose an age group",
                         choices = c("All" = regex(".*"),
                                     Age_Groups),
                         selected = "All"),
            
             selectInput("Gender6", 
                         label = "Choose a gender",
                         choices = c("All" = regex(".*"),
                                     "Female" = "F",
                                     "Male" ="M"),
                         selected = "All")
             
             
           ),
           mainPanel(
             plotOutput("plot9")
           )
         )
  )
    
  
)     
            
              
server <- function(input, output){
  

# Output Gender frequency -------------------------------------------------

  
  output$plot1<- renderPlot({
      house |> 
      filter(str_detect(State, input$State)) |> 
      filter(str_detect(Age_Group, input$Age_Group)) |> 
      group_by(Gender) |> 
      summarise(n = n()) |> 
      mutate(freq = n / sum(n))|> 
      ggplot(aes(y=freq, x =Gender, fill=Gender))+
        geom_col(show.legend = F)+
        theme_light()+
        labs(y="Frequency of costumers", x ="Customer's Gender")+
        scale_x_discrete(labels=c("Female", "Male"))})
    

# Output gender and amount spent ------------------------------------------

  
  output$plot2 <- renderPlot({
    house |> 
      filter(str_detect(State, input$State2)) |> 
      filter(str_detect(Age_Group, input$Age_Group2)) |> 
      group_by(Gender) |> 
      ggplot(aes(x= Gender, y = Amount, fill = Gender))+
      geom_violin(draw_quantiles = c(0.5), show.legend = F)+
      labs(y="Amount spent by the customer", x ="Customer's Gender")+
      scale_x_discrete(labels=c("Female", "Male"))+
      theme_light()})
 
  

# Output occupation and amount spent ------------------------------------------

output$plot3 <- renderPlot({
    house |> 
      filter(str_detect(State, input$State3)) |> 
      filter(str_detect(Gender, input$Gender1)) |>
      filter(str_detect(Age_Group, input$Age_Group3)) |>
      mutate(Occupation= fct_reorder(Occupation, Amount)) |> 
      ggplot(aes(y= Occupation,  x= Amount, fill = Occupation)) +
      geom_violin(show.legend = F, draw_quantiles = c(0.25,0.5, 0.75), linewidth=1.2, alpha =0.5)+
      labs(x="Amount spent by the customer", y ="Customer's occupation")+
      theme_light()})
    
  

# Output age group and amount spent ---------------------------------------


output$plot4 <- renderPlot({
  house |> 
    filter(str_detect(State, input$State4)) |> 
    filter(str_detect(Gender, input$Gender2)) |>
    ggplot(aes(x =Amount, y = Age_Group, fill =Age_Group)) + 
    geom_violin(scale =T,show.legend = F, draw_quantiles = c(0.25,0.5, 0.75), linewidth=1, alpha = 0.5)+
    theme_light()+
    labs(y="Costumer's age group", x ="Amount spent by the customer")})
    


# Output geographic zone and amount spent ---------------------------------


output$plot5 <- renderPlot({
  house |> 
    filter(str_detect(Gender, input$Gender3)) |>
    filter(str_detect(Age_Group, input$Age_Group4)) |>
    mutate(Zone = fct_reorder(Zone, Amount) ) |> 
    ggplot(aes(y= Zone, x= Amount, fill = Zone))+
    geom_violin(scale =T,show.legend = F, draw_quantiles = c(0.25,0.5, 0.75), linewidth=1, alpha = 0.5)+
    labs(x="Amount spent by the customer", y ="Geographic zone of the customer")+
    theme_light()})
  


# Output state and amount spent -------------------------------------------

output$plot6 <- renderPlot({
  house |> 
    filter(str_detect(Gender, input$Gender4)) |>
    filter(str_detect(Age_Group, input$Age_Group5)) |>
    mutate(State = fct_reorder(State, Amount) ) |> 
    ggplot(aes(y= State, x= Amount, fill = State))+
    geom_violin(scale =T,show.legend = F, draw_quantiles = c(0.25,0.5, 0.75), linewidth=1, alpha = 0.5)+
    labs(x="Amount spent by the customer", y ="State of the customer")+
    theme_light()})



# Output proportion of female costumers per product category --------------


output$plot7 <- renderPlot({
  house2<-house|> 
    filter(str_detect(Age_Group, input$Age_Group6)) |>
    filter(str_detect(State, input$State5)) |> 
    group_by(Product_Category) |>
    summarize(n =n(), prop_female = mean(Gender =="F") ) |> 
    mutate(Product_Category = fct_reorder(Product_Category, prop_female)) 
    

    ggplot(house2,aes(y  = Product_Category,  fill = Product_Category, x = prop_female))+
    geom_bar(stat = "identity",show.legend = F) +
    geom_vline(xintercept =   weighted.mean(house2$prop_female,w= house2$n), col="red")+
    theme_light()})



# Output zone and product category -----------------------------------


output$plot8 <- renderPlot({
  house |> 
    filter(str_detect(Age_Group, input$Age_Group7)) |>
    filter(str_detect(Zone, input$Zone)) |> 
    filter(str_detect(Gender, input$Gender5)) |>
    group_by(Product_Category) |> 
    summarise(n = n()) |> 
    mutate(freq = n / sum(n))|> 
    mutate(Product_Category= fct_reorder(Product_Category, n) |> fct_rev() ) |> 
    ggplot(aes(x  =Product_Category , fill = Product_Category, y =freq))+
    geom_col(show.legend = F)+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 0.5))+
    labs(x="Product Category", y ="Number of costumers")})



# Output state and product category ---------------------------------------



output$plot9 <- renderPlot({
  house |> 
    filter(str_detect(Age_Group, input$Age_Group8)) |>
    filter(str_detect(State, input$State6)) |> 
    filter(str_detect(Gender, input$Gender6)) |>
    group_by(Product_Category) |> 
    summarise(n = n()) |> 
    mutate(freq = n / sum(n))|> 
    mutate(Product_Category= fct_reorder(Product_Category, n) |> fct_rev() ) |> 
    ggplot(aes(x  =Product_Category , fill = Product_Category, y =freq))+
    geom_col(show.legend = F)+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 0.5))+
    labs(x="Product Category", y ="Number of costumers")})








}
shinyApp(ui, server)

