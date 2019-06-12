# Visualization of CBS data on NL map
library(shiny)
library(dplyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(ggmap)
library(data.table)
library(shinythemes)
library(ggiraph)

# Color scheme
my_blue <- "dodgerblue2"

# Read CBS data
my_data <- read.csv("clean_data.csv", na.strings = c(""," ","NA","NULL"), stringsAsFactors = F)
var_list <- setdiff(colnames(my_data),c("Freq", "PROVINCIE","Year"))
var_list <- var_list[order(var_list)]

# Load NL-region map
NL_map_states <- readOGR("Provincie2014","Provincie2014")
NL_map_states_trans <- spTransform(NL_map_states, CRS("+proj=longlat +datum=WGS84"))
NL_map_states_trans <- fortify(NL_map_states_trans)

province_list <- names(table(my_data$PROVINCIE))
year_list <- names(table(my_data$Year))

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("CBS demographics data in the Netherlands"),
                tabsetPanel(
                  tabPanel("NL map",
                           fluidRow(
                             sidebarLayout(
                               # Input(s)
                               sidebarPanel(
                                 wellPanel(
                                   selectInput(inputId = "year",
                                               label = "Select a year:",
                                               choices = year_list,
                                               selected = "2010"),
                                   
                                   br(),  
                                   selectInput(inputId = "factor_name",
                                               label = "Choose a variable:",
                                               choices = var_list,
                                               selected = "Gender"),
                                   
                                   br(),
                                   radioButtons("value_name", "Select a value:",
                                                c("Male" = "Male",
                                                  "Female" = "Female"),
                                                selected = "Male")
                                 )
                               ),
                               # Output(s)
                               mainPanel(
                                 ggiraphOutput(outputId = "mapplot_NL",height = "450px"),
                                 h5("Significantly associated regions according to chi-square test (shown by pink)", align = "left"),
                                 plotOutput(outputId = "significance_province", width = "75%", height = "200px")
                               )
                             )
                             
                           ),
                           fluidRow(
                             h6("CBS data: 2010 - 2017", align = "right")
                           )
                  )
                  ,
                  tabPanel("Province",
                           fluidRow(
                             sidebarLayout(
                               # Input(s)
                               sidebarPanel(
                                 wellPanel(
                                   selectInput(inputId = "province_name",
                                               label = "Choose a province:",
                                               choices = province_list,
                                               selected = "Drenthe")
                                 )
                               ),
                               # Output(s)
                               mainPanel(
                                 htmlOutput(outputId = "table_name"),
                                 tableOutput(outputId = "freq_NL"),
                                 plotOutput(outputId = "yearly_demographics", height = "200px")
                               )
                             )
                           ),
                           fluidRow(
                             h6("CBS data: 2010 - 2017", align = "right")
                           )
                  )
                  
                )
)

# Server
server <- function(input, output, session) {
  # Update NL-variable radio buttons
  observeEvent(input$factor_name,{
    mylist <- names(table(my_data[,input$factor_name]))
    if (is.null(mylist)){
      x <- character(0)
    } else{
      x <- mylist
    }
    
    updateRadioButtons(session, inputId = "value_name", label = "Select a value:",
                       choices = x)
    
  })
  
  # Map
  observe({
    output$mapplot_NL <- renderggiraph({
      req(input$factor_name, input$value_name, input$year)
      my_factorname <- as.name(input$factor_name)
      tt <- my_data %>% filter(Year == input$year) %>% group_by(PROVINCIE,eval(my_factorname)) %>% 
        summarise(Frequency = sum(Freq)) %>% 
        mutate(Percentage = round(Frequency/sum(Frequency)*100,2))
      colnames(tt) <- c("PROVINCIE",input$factor_name,"Frequency","Percentage")
      
      
      tt <- tt %>% filter(eval(my_factorname) == input$value_name)
      
      # select only the subcategory defined by input$value_name
      merge_data_map <- merge(data.frame(id = rownames(NL_map_states@data), 
                                         PROVINCIE = NL_map_states$PROVINCIE),
                              tt, 
                              by = "PROVINCIE",
                              all.x = T)
      
      
      final_map <- merge(NL_map_states_trans,merge_data_map, by = "id",all.x = T)
      if (grepl("Inf",max(final_map$Percentage, na.rm = T)) == FALSE && (max(final_map$Percentage, na.rm = T) > 0)){
        my_color <- my_blue
        
        # Tints for selected color
        temp <- as.vector((255-col2rgb(my_color)))/255*0.85 + as.vector(col2rgb(my_color))/255
        my_color_light <- rgb(temp[1],temp[2],temp[3])
        
        max_N <- max(final_map$Percentage,na.rm = T)
        min_N <- min(final_map$Percentage,na.rm = T)
        
        #Plot
        fig <- ggplot(final_map) + 
          theme_minimal() + 
          geom_polygon_interactive(aes(x = long, y = lat, group = group, fill = Percentage, tooltip=sprintf("%s<br/>%s%%",PROVINCIE,Percentage)),color = "lightgrey") +  
          coord_map() + theme(text = element_text(size=9))+
          scale_fill_gradient(low = my_color_light, high = my_color, na.value = "grey50",
                              name = paste(input$value_name,": \nPeople% in the province for year ",input$year, sep = ""),
                              breaks=c(min_N,max_N),
                              labels=c(min_N,max_N)) +
          theme(axis.title = element_blank(),
                axis.line = element_blank(),
                panel.grid = element_blank(),
                axis.text = element_blank())
        fig <- ggiraph(code=print(fig))
        
      } else {
        
        fig <- ggplot(final_map) + 
          theme_minimal() + 
          geom_polygon(aes(x = long, y = lat, group = group), color = "lightgrey") +  
          coord_map() + theme(text = element_text(size=9))+
          labs(caption = paste("No people with ",input$factor_name, sep = ""))
      }
      fig
    })
    
  })
  # Significance analysis - chi square test, standard residual -/+1.96
  observe({ 
    req(input$factor_name,input$year)
    my_factorname <- as.name(input$factor_name)
    tt <- my_data %>% filter(Year == input$year) %>% group_by(PROVINCIE,eval(my_factorname)) %>% 
      summarise(Frequency = sum(Freq))
    colnames(tt) <- c("PROVINCIE",input$factor_name,"Frequency")
    #Convert to contingency table
    cont_table <- xtabs(Frequency ~ eval(my_factorname)+PROVINCIE, data = tt)
    
    #Apply chi square test
    chi_profile <- chisq.test(cont_table)
    #Positively associated provinces if any
    pos <- which(chi_profile$stdres > (1.96),arr.ind = TRUE)
    if (dim(pos)[1] > 0){ # There is a chance that no residual is greater than 1.96
      pos_group <- as.data.frame(cbind(Color = "pink",
                                       Source = colnames(cont_table)[pos[,2]], 
                                       residual = chi_profile$stdres[pos],
                                       Profile = rownames(pos)), 
                                 stringsAsFactors = F)

    }

    my_significance <- ggplot(pos_group, (aes(x = 1, y = 1, fill = Color))) + 
      geom_col(position = "fill") + 
      scale_fill_manual(values = c("pink" = "pink", "lightgrey" = "lightgrey")) +
      facet_grid(Profile ~ Source, as.table = FALSE, switch = "both") + 
      theme(strip.text.y = element_text(angle = 180),strip.text.x = element_text(angle = 0)) + scale_y_discrete(expand = c(0, 0)) + 
      scale_x_continuous(expand = c(0, 0)) +
      theme(panel.grid = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_blank(), 
            strip.background = element_blank(), 
            axis.title = element_blank(),
            axis.line = element_blank(),
            text = element_text(size = 9)) + 
      theme(legend.position = "none")  
    output$significance_province <- renderPlot({my_significance})
  })
  # Data table name
  observe({ 
    output$table_name = renderText({
      req(input$factor_name,input$province_name,input$year)
      paste("Data table:",input$factor_name,"-",input$province_name,"for year",input$year)
    })
  })
  # Data table to be displayed for the selected province, year and factor
  observe({
    req(input$factor_name, input$province_name, input$year)
    tt <- my_data %>% filter(PROVINCIE == input$province_name & Year == input$year) %>% group_by(eval(as.name(input$factor_name))) %>% 
      summarise(Frequency = sum(Freq)) %>% 
      mutate(Percentage = round(Frequency/sum(Frequency)*100,2))
    colnames(tt) <- c(input$factor_name, "Frequency","Percentage")
    output$freq_NL <- renderTable({tt})
  })
  
  # Yearly graph to be displayed for selected province and factor
  observe({
    req(input$factor_name, input$province_name, input$value_name)
    # Take factor_name as variable name
    my_factorname <- as.name(input$factor_name)
    
    tt <- my_data %>% filter(PROVINCIE == input$province_name) %>% group_by(Year,eval(my_factorname)) %>% 
      summarise(Frequency = sum(Freq)) %>% 
      mutate(Percentage = round(Frequency/sum(Frequency)*100,2))
    # Change column names of tt since selected factor name column will be named as `eval(my_factorname` which will be 
    # problematic for filter
    colnames(tt) <- c("Year",input$factor_name,"Frequency","Percentage")
    
    # Filter based on value_name
    tt <- tt %>% filter(eval(my_factorname) == input$value_name)
    
    #Plot
    my_barplot <- ggplot(tt,aes(x = as.character(Year), y = Percentage, fill = Percentage)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(position = "identity", label = sprintf("%.1f%%", tt$Percentage), fontface = "bold") +
      scale_fill_gradient(low = "lightgrey", high = my_blue) +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none") +
      ggtitle(paste(input$factor_name,"-",input$value_name,"percentage by year for",input$province_name)) + ylim(0,100)
    
    output$yearly_demographics <- renderPlot({my_barplot})
  })
  
  session$onSessionEnded(stopApp)
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

