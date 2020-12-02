
ui <- fluidPage(
  titlePanel("Analysis of Yelp review for fast food restaurants"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("We made a word frequency table of reviews whose star ranging from 1 to 5."),
      
      selectInput("var", 
                  label = "Choose the star level you interested of the restaurants",
                  choices = list("1 star", 
                                 "2 stars",
                                 "3 stars", 
                                 "4 stars",
                                 "5 stars",
                                 "Overall"),
                  selected = "five stars"),
      selectInput("var2",
                  label = "estimated coefficients of significant parameters in regression ",
                  choices = list("Service",
                                 "Waiting time",
                                 "Food",
                                 "Sanitary condition"),
                  selected="Service")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Image", imageOutput("myImage")), 
        tabPanel("Plot", plotOutput("myplot"))
      )
      
      
      
      
      
    )
  )
  )
server <- function(input, output ) {
  
  output$myImage <- renderImage({
    
    if(input$var=="1 star"){
      list(src = "www/1star.png", height = 400, width = 572)
    }
    else if(input$var=="2 stars"){
      list(src = "www/2stars.png", height = 400, width = 572)
    }
    else if(input$var=="3 stars"){
      list(src = "www/3stars.png", height = 400, width = 572)
    }
    else if(input$var=="4 stars"){
      list(src = "www/4stars.png", height = 400, width = 572)
    }
    else if(input$var=="5 stars"){
      list(src = "www/5stars.png", height = 400, width = 572)
    }
    else if(input$var=="Overall"){
      list(src = "www/allstars.png", height = 400, width = 572)
    }
    
    
    
    
  },deleteFile = FALSE)
  
  output$myplot = renderPlot({
    
    if(input$var2=="Service"){
      y = c(-1.07,0.62,-1.79,-0.73,2.10,-0.23,-1.35,0.09,-1.05,0.63,0.34,-0.33)
      x = c("Poor_service","decent_price","apologies","horrible_service","friendliness","attitude","rude","the_service","incompetent","Delivery","greet","their_job")
      z = c()
      
      for (ele in y)
      {
        if (ele > 0)
        {
          z = c(z,"red")
        }
        else{
          z = c(z,"green")
        }
        
      }
      barplot(y,names.arg = x,las=2,col=z)
    }
    if(input$var2=="Waiting time"){
      y = c(-0.64,2.10,-0.38,-0.79,-0.73,0.68,-0.23,-0.31,-0.91,-0.19)
      x = c("40_minutes","Quick","for_15","for_20","slow_and","Fast","minutes_to","in_line","over_15","waited")
      z = c()
      
      for (ele in y)
      {
        if (ele > 0)
        {
          z = c(z,"red")
        }
        else{
          z = c(z,"green")
        }
        
      }
      barplot(y,names.arg = x,las=2,col=z)
    }
    if(input$var2=="Food"){
      y = c(-1.64,-1.81,0.61,1.02,0.74,0.37,0.80)
      x = c("is_cold","not_fresh","Huge","kids_meal","ingredients_and","were_delicious","Great_food")
      z = c()
      
      for (ele in y)
      {
        if (ele > 0)
        {
          z = c(z,"red")
        }
        else{
          z = c(z,"green")
        }
        
      }
      barplot(y,names.arg = x,las=2,col=z)
    }
    if(input$var2=="Sanitary condition"){
      y = c(0.21,1.29)
      x = c("clean_and","dirty_tables")
      z = c()
      
      for (ele in y)
      {
        if (ele > 0)
        {
          z = c(z,"red")
        }
        else{
          z = c(z,"green")
        }
        
      }
      barplot(y,names.arg = x,las=2,col=z)
    }
    
    
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
