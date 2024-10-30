
library(tidyverse)
library(ggtext)
library(patchwork)
library(png)
library(magrittr)
library(readr)
library(ggplot2)
library(ggforce)
library(ggtext)
library(extrafont)
library(ggstream)
library(ggsci)
library(ggdist)
library(colorspace)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggsankey)
library(wesanderson)
library(packrat)
library(rsconnect)
library(bslib)

#=============before app=================
years <- seq(2003,2023,by=1)

  #import data
load("AUT_dist_hr.Rdata")
load("Avg_rec_byYear.Rdata")


#=============app=================
#UI

ui <- fluidPage(shinytheme("paper"), style = "margin:0;",
                setBackgroundColor("seashell"),
                navbarPage( "How They Live",
                            #panel 1
                            tabPanel("No Time for Self Care",id = "panel1",
                                     page_fillable(
                                            layout_columns(
                                              wellPanel(
                                                selectInput("selected_year", "Select Year:", 
                                                            choices = years, selected = years[length(years)]),
                                                  (tags$h4("No Time for Self Care", style = "color: #8b5a2b; font-family: Georgia;
                                                        font-size: 20px;margin-top: 10px; font-weight:bold;")),
                                                  tags$p("The average person in America spends around 8 hours per day sleeping. However, as 
                                                     they get older, more and more of these precious hours seems to be sacrificed for work.
                                                     The median time spent on selfcare, which includes personal time and personal health care, was less than half an hour.
                                                     Most of the rest of the daily hours is spent on recreational and socialising activites. 
                                                     This is a common landscape seen across all 21 years of the survey.",
                                                         style = "color: #8b5a2b; font-family: Lato; font-size: 12px; 
                                                     margin-right: 20px;
                                                     text-align: justify;"),
                                                  tags$p("A person who is willing to record all their daily activites, including more than 200 actions or variables, 
                                                     is surely deilligent. Your are now invited to imagine how worse a 'real' average person could spent their day.",
                                                         style = "color: #8b5a2b; font-family: Lato; font-size: 12px; 
                                                     margin-right: 20px; margin-top: 5px;
                                                     text-align: justify;"),
                                                  tags$p("A hopeful glint is the increase in recreational activities for those above 60 years old.
                                                     But for some, this is too late. And are we really sure that all recreational activities are good?",
                                                         style = "color: #8b5a2b; font-family: Lato; font-size: 12px; 
                                                     margin-right: 20px; margin-top: 5px;
                                                     text-align: justify;")),
                                              mainPanel(
                                                plotOutput("activity_plot", height = "600px", width = "900px")),
                                              col_widths = c(3,9)
                                                          )
                                                  )
                            ),
                            #panel 2
                            tabPanel("Another Side of the Story", id = "panel2",
                                     page_fillable(
                                            layout_columns(col_widths = c(3,9),
                                              wellPanel(
                                              tags$h4("An Era of Inactivity", style = "color: #8b5a2b; font-family: Georgia;
                                                      font-size: 18px; font-weight:bold; margin-top: 20px;
                                                      margin-left:10px;"),
                                              tags$p("Across the 21-year period from 2003 to 2023, the most prominent leisurely activity was watching TV and movies,
                                                     which accounts for around half of all recreational time spent. Computer use and gaming have risen to the ranks of top 3
                                                     in 2023, overtaking socialising activities in some years. Hobbies involving arts, literature and sport have be on 
                                                     a downward trajectory since 2016, being overtaken in 2023 by the mysterious relaxation and thinking time. While 
                                                     socialisation remains in the top 2, nearly half of the activity entails obligatory attendence.",
                                                     style = "color: #8b5a2b; font-family: Lato; font-size: 11px;
                                                     margin-left: 10px;
                                                     text-align: justify;"),
                                              tags$p("Regardless of our prespective, it is clear that much of recreational time is spent being inactive, 
                                                     a fact only amplified by the increased media exposure of the modern era, and the people's willing
                                                     acceptence. How long will it be before the time for relaxation and socialisation, too, dwindle like 
                                                     sports and other activities before being supplanted by new sedentary or digital persuits. Whether these
                                                     passions are healthy is an argument for another time, but one thing is certain: all this passivity is a 
                                                     step backward in growth.",
                                                     style = "color: #8b5a2b; font-family: Lato; font-size: 11px;
                                                     margin-left: 10px; margin-top: 5px;
                                                     text-align: justify;")
                                                       )
                                              ,
                                              img(src = "subplot.png", style = "height: 600px; width: 100%;")
                                             )
                                             )
                                     ),
                            #panel3
                            tabPanel("Resources & More", id = "panel3",
                                     fluidRow(
                                        tags$h4("Resources:", style = "color: #8b5a2b; font-family: Georgia;
                                                      font-size: 22px; font-weight:bold;
                                                      margin-left:20px;"),
                                        
                                        tags$ul(
                                          tags$li(tags$p("Link to the original survey: ",
                                                         tags$a(href = "https://www.bls.gov/tus/data/datafiles-0323.htm",
                                                                "American Time Spent Survey. ", target = "_blank"),
                                                         "The file used was ❛ATUS 2003–2023 Activity summary file(zip)❜. Read the Sum0323_info after downloading to understand the data.",
                                                         style = "color: #8b5a2b; font-family: Lato; font-size: 14px;
                                                               margin-left: 20px; margin-top: 15px;
                                                               text-align: justify;")),
                                          tags$li(tags$p("The original dataset and codes use to produce this app, including the data used, can be found at: ",
                                                         tags$a(href = "https://github.com/SothearaotTat/HowTheyLive/tree/main",
                                                                        "https://github.com/SothearaotTat/HowTheyLive/tree/main",
                                                                target = "_blank"),
                                                         style = "color: #8b5a2b; font-family: Lato; font-size: 14px;
                                                               margin-left: 20px; margin-top: 15px;
                                                               text-align: justify;"))
                                                )
                                              ),
                                     fluidRow(
                                       tags$h4("Reference:", style = "color: #8b5a2b; font-family: Georgia;
                                                      font-size: 22px; font-weight:bold;
                                                      margin-left:20px; margin-top: 15px;"),
                                       tags$ul(
                                         tags$li(tags$p("U.S. Bureau of Labor Statistics. (2024). American Time Use Survey[Data set]. 
                                                        https://www.bls.gov/tus/datafiles/atussum-0323.zip",
                                                        style = "color: #8b5a2b; font-family: Lato; font-size: 14px;
                                                               margin-left: 20px; margin-top: 15px;
                                                               text-align: justify;"))
                                              )
                                             ),
                                     fluidRow(
                                       tags$h4("Credits:", style = "color: #8b5a2b; font-family: Georgia;
                                                      font-size: 22px; font-weight:bold;
                                                      margin-left:20px; margin-top: 15px;"),
                                       tags$ul(
                                         tags$li(tags$p("Credits to Dr. Cédric Scherer whose visualisations published on week
                                                        2020/48 and 2022/36 greatly inspires this visualisation. Link to his github:",
                                                        tags$a(href = "https://github.com/z3tt/TidyTuesday",
                                                               "https://github.com/z3tt/TidyTuesday",
                                                               target = "_blank"),
                                                        style = "color: #8b5a2b; font-family: Lato; font-size: 14px;
                                                               margin-left: 20px; margin-top: 15px;
                                                               text-align: justify;"))
                                       )
                                             )
                                      )
                                )
                      ) 
# Define server logic
server <- function(input, output, session) {
  
  output$activity_plot <- renderPlot({
    filtered_data <- AUT_dist_hr %>%
      filter(TUYEAR == input$selected_year)
    
    #setting color
    
    activities <-c("Household", "Self Care", "Sleep", "Traveling",
                   "Work", "Recreation" )
    
    color <- c("#5C88DA99", "#84BD0099", "#CC0C0099", "#00AF6699", "#00B5E299","#FFCD0099")
    
    
    act_color <- setNames(color,activities)
    
    #The plot itself
    ggplot(
      data = filtered_data,
      aes(x = Time_spent, y = TEAGE))+
      stat_dots(position = "dodge",
                aes(fill = activities,
                    color = activities),
                dotsize = 1)+
      stat_pointinterval(position = "dodgejust",
                         aes(fill = activities, color = activities),
                         size = 2,
                         linewidth = 1,
                         point_interval = mean_qi,
                         shape = 21,
                         point_color = "grey20",
                         show.legend = FALSE)+
      stat_pointinterval(position = "dodgejust",
                         aes(fill = activities, color = activities),
                         size = 2,
                         linewidth = 1,
                         point_color = "grey20",
                         point_interval = median_qi,
                         shape = 24,
                         show.legend = FALSE)+
      theme_void() +
      #scales
      scale_x_continuous(limits = c(-1,18.1),
                         expand = c(0,0),
                         breaks = c(0,2.5,5,7.5,10,12.5,15),
                         labels = c(0,2.5,5,7.5,10,12.5,15))+
      scale_color_manual(values = act_color)+
      scale_fill_manual(values = act_color)+
      scale_y_discrete(limits = factor(c(1,2,3,4)),
                       breaks = c(1,2,3,4),
                       labels = c("10-20", "21-40","41-60", "61+"),
                       expand = c(0,0))+
      labs(
        title = "Time Spent by Activities and Age Group by Those Who Participate in the Activities",
        subtitle = "The **American Time Use Survey** provides a summary data containing more than 240,000 observsations with 456 variables 
        across a 21-year period on<br>how participants spent a day in their lives. This plot presents the
        distributions, mean and median summary of hours spent on 6 main activities featuring<br>  different age groups.",
        caption = "***Visualization by Sothearaot Tat  |  Source: ATU Survey***"
      )+
      
      #background aesthetics
      theme(
        plot.background = element_rect(fill = "seashell", 
                                       color = NA),
        panel.background = element_rect(fill = "seashell",
                                        color = NA),
        axis.text.x = element_markdown(margin = margin(rep(0.5, 4)),
                                       size = 9,
                                       family = "Lato"),
        #Title,Subtitle,  
        plot.title = element_text(family = "Georgia", color = "tan4", 
                                  size = 15, face = "bold",
                                  margin = margin(t = 10, l = 40, r = 15)),
        plot.subtitle = element_markdown(color = "tan4", size = 11,
                                         family = "Lato",
                                         margin = margin(t = 10, b = 15, l = 40),
                                         lineheight = 1.25),
        plot.caption = element_markdown(color = "tan4", size = 8,
                                         family = "Lato"),
        
        #legend
        legend.position = c(0.92,0.85),
        legend.background = element_rect(fill = "seashell",
                                         color = NA),
        legend.title = element_blank(),
        legend.key.size = unit(0.8,"cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.text = element_text(size = 9,
                                   margin = margin(t=0, b =0),
                                   family = "Lato")
      )+ 
      guides(fill = guide_legend(override.aes = list(shape = 16, size = 3)))+
      
      #the cord and axis
      coord_cartesian(clip ="off") +
      #x-axis grid
      geom_vline(
        xintercept = 0:8 * 2.5 ,
        color = "grey70",
        size = .2,
        linetype = "dashed"
      ) +
      #axis title
                #y-axis
      annotate(
        "text", x = -0.25, y = 4.5,
        family = "Lato",
        label = "Age Group",
        color = "grey20",
        fontface = "bold",
        size = 3.5
      )+        #x-axis
      annotate(
        "text", x = 8.75, y = 0.5,
        family = "Lato",
        label = "Hour Spent",
        color = "grey20",
        fontface = "bold",
        size = 3.5
      )+
      
      
      #manual facet
      geom_segment(
        data = data.frame(x = rep(0,5), xend = rep(18,5),
                          y = c(0.55,1.5, 2.5, 3.5, 4.455), yend = c(0.55,1.5, 2.5, 3.5,4.455)),
        aes(x = x, xend = xend,
            y = y, yend = yend),
        size = 0.5,
        linetype = "solid",
        color = "black") +
      
      geom_segment(
        aes(x = 0, xend = 0,
            y = 0.55, yend= 4.455),
        color = "black",
        size = 0.5,
        linetype = "solid"
      ) +
      geom_segment(
        aes(x = 18, xend = 18,
            y = 0.55, yend= 4.455),
        color = "black",
        size = 0.5,
        linetype = "solid"
      ) + 
      
      annotate(
        "text", 
        x = rep(-0.5,4),
        y = c(1,2,3,4),
        label = c("10-20", "21-40","41-60", "61+"),
        size = 3, 
        color = "black",
        family = "Lato") +
      
      #legend for triangle and circle
      #circle/mean
      annotate(
        "text", x = mean((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent),
        y = 4.5+0.1,
        label = "Mean\nhrs",
        family = "Lato",
        size = 2.5, 
        color = "grey30",
        fontface = "bold", 
        lineheight = .85
      ) +
      annotate(
        "curve", x = mean((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent),
        xend = mean((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent)-0.05,
        y = 4.5+0.05, 
        yend = 4.5-0.05,
        curvature = .25,
        color = "grey30",
        size = .55,
        arrow = arrow(length = unit(0.06, "inches"),
                      type = "closed")
      )+
      #triangle/median
      annotate(
        "text", x = median((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent)+0.1,
        y = 4.5+0.1,
        label = "Median\nhrs",
        family = "Lato",
        size = 2.5, 
        color = "grey30",
        fontface = "bold", 
        lineheight = .85
      ) +
      annotate(
        "curve", x = median((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent),
        xend = median((filter(filtered_data, activities == "Work" & TEAGE == 4 ))$Time_spent)+0.05,
        y = 4.5+0.05, 
        yend = 4.5-0.05,
        curvature = -.25,
        color = "grey30",
        size = .55,
        arrow = arrow(length = unit(0.06, "inches"),
                      type = "closed")
      )
  })
  
  session$onSessionEnded(function() {stopApp()})
}


# Run the application 
shinyApp(ui = ui, server = server)

