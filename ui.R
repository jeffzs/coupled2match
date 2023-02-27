{\rtf1\ansi\ansicpg1252\cocoartf2708
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Coupled2Match - an app to generate NRMP couples match rank lists\
\
\
library(shiny)\
library(sortable)\
library(rhandsontable)\
library(shinydashboard)\
library(DT)\
library(tidyverse)\
library(reshape2)\
\
last_update_date <- "02/26/2023"\
author_email <- "zhifei.sun@gmail.com"\
author_name <- "Zhifei Sun, MD"\
\
strategy_list <- list("top_top_program_same_location", \
                   "top_top_program_close_location" ,\
                   "top_top_program_far_location" ,\
                   \
                   "top_bot_program_same_location", \
                   "top_bot_program_close_location" ,\
                   "top_bot_program_far_location" ,\
                   \
                   "bot_top_program_same_location", \
                   "bot_top_program_close_location" ,\
                   "bot_top_program_far_location" ,\
                   \
                   "bot_bot_program_same_location",\
                   "bot_bot_program_close_location" ,\
                   "bot_bot_program_far_location",\
                   \
                   "top_nomatch",\
                   "bot_nomatch",\
                   "nomatch_top",\
                   "nomatch_bot")\
\
dashboardPage(title = "Coupled2Match: an app to generate NRMP couples match rank lists",\
  # UI HEADER  \
  dashboardHeader(title = span(tagList(icon("link"), "Coupled2Match"), \
                               style = "font-weight: bold;")),\
  # UI SIDEBAR \
  dashboardSidebar(\
    div(style="text-align:center",\
        HTML(paste0("<h4><b><u>Instructions</b></u></h4>")),\
        HTML(paste0("<p>1. Enter personal rank lists</p>")),\
        HTML(paste0("<p>2. Choose the ranking strategy</p>")),\
        HTML(paste0("<p>3. Click <b>GO</b> to generate the rank list</p>"))\
        ),\
    br(),br(),br(),\
    div(style="text-align:center",\
        HTML(paste0("<h4><b><u>Current Limitations</b></u></h4>")),\
        HTML(paste0("<p>2. Typos are treated as different entities</p>")),\
        HTML(paste0("<p>3. Personal tier (top vs bot) must be entered</p>")),\
        HTML(paste0("<p>4. Changes to ordering of strategies do not save</p>"))\
    ),\
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),\
    div(style="font-size: 18px;text-align:center; font-weight:bold;font-variant: small-caps;",\
        HTML(paste0("<a href='mailto:",author_email,"?subject=Feedback for Coupled2Match'>Send me feedback and bugs!</a>"))\
        ),\
    br(),\
\
    div(style="text-align:center",\
        code(paste("Last updated:",last_update_date)),\
        br(),\
        HTML(paste0("Created by <a href='mailto:",author_email,"?subject=Hello!'>",author_name,"</a>")),\
        br(),\
        HTML(paste0("Disclaimer: I am not responsible for errors generated in the final list, please double check it yourselves.")))\
\
   ),\
   # UI MAIN BODY\
   dashboardBody(\
      tabsetPanel(\
        id = "TabSet",\
        tabPanel(\
          title = "Input",\
              fluidRow(\
                 box(\
                   status = "primary",\
                   fileInput("file", accept = ".csv" , label = "Choose a previously saved rank list"),\
                   actionButton("load", "Load Selected List", icon = icon("upload"))\
                 ),\
                 box(\
                  status = "primary",\
                   actionButton("clear", "Clear Current List", icon = icon("broom")), \
                   downloadButton("save", "Save Current List")\
                 ),\
                 box(\
                   status = "warning",\
                   actionButton("go", "Go!",width = "100%",icon = icon("play"))\
                 ),\
               ),\
            box(\
              title = "Student A", width = 12, solidHeader = TRUE, status = "primary",\
              actionButton("student_A_add_row", "Add Row"),\
              actionButton("student_A_delete_row", "Delete Selected Row"),\
              DTOutput('student_A_rank_list'), \
            ),\
            box(\
              title = "Student B", width = 12, solidHeader = TRUE, status = "warning",\
              actionButton("student_B_add_row", "Add Row"),\
              actionButton("student_B_delete_row", "Delete Selected Row"),\
              DTOutput('student_B_rank_list'), \
            ),\
    \
      ), \
      tabPanel(title = "Strategy", \
               rank_list(text = "Strategy Ranking", labels = strategy_list, input_id = "strategy_list")\
      ),\
      tabPanel("Run", \
               br(),\
               DTOutput("final_rank_list_DT")\
      )\
    ) \
    # END tabsetPanel\
  ) \
  # END dashboardBody\
) \
\
}
