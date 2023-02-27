{\rtf1\ansi\ansicpg1252\cocoartf2708
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Coupled2Match server logic\
#\
# versions\
# 01/20/23 - initialized app, first draft of the logic working for creating rank lists; see test.R\
# 02/26/23 - got the student A personal list entry working, alot of cosmetic work\
\
\
\
\
# takes pre-formatted data frame and strategy list to produce a paired rank list, returns a data frame\
generate_couples_rank_list <- function(rank_db, strategy_list) \{\
  # initialize variables\
  final_list <- NA   \
  \
  # generate program look up table\
  program_lookup <-NA\
  program_lookup <- data.frame(unique(rank_db$program_name))\
  names(program_lookup) <- c("program_name")\
  program_lookup <- merge(program_lookup, rank_db[rank_db$student=="A",], by.x="program_name", by.y="program_name", all.x = TRUE,all.y = FALSE)\
  \
  program_lookup <- merge(program_lookup, rank_db[rank_db$student=="B",], by.x="program_name", by.y="program_name", all.x = TRUE,all.y = FALSE)\
  \
  \
  names(program_lookup) <- c("program_name",\
                             "student_A","program_rank_A","program_tier_A","program_city_A","program_state_A",\
                             "student_B","program_rank_B","program_tier_B","program_city_B","program_state_B")   \
  \
  # for loop to iterate through strategies\
  for (i in strategy_list) \{\
    \
    # initializing variables\
    working_db <- NA\
    step1<-NA\
    step2<-NA\
    step3<-NA\
    \
    # get all permutation of program lists\
    program_list_A <- rank_db$program_name[rank_db$student=="A"]\
    program_list_B <- rank_db$program_name[rank_db$student=="B"]\
    program_permutation <- as.data.frame(expand.grid(program_list_A, program_list_B))\
    names(program_permutation) <- c("program_A","program_B")\
    \
    #print(program_lookup)\
    #print(program_permutation)\
    \
    step1 <- merge(program_permutation, program_lookup[program_lookup$student_B == "B", c("program_name","program_rank_B","program_tier_B","program_city_B","program_state_B")], by.x="program_B", by.y="program_name", all.x = TRUE)\
    step2 <- merge(step1,               program_lookup[program_lookup$student_A == "A", c("program_name","program_rank_A","program_tier_A","program_city_A","program_state_A")], by.x="program_A", by.y="program_name", all.x = TRUE)\
    \
    working_db <- step2\
    \
    # modify dataframe based on current strategy\
    if (i == "top_top_program_same_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "top",\
               program_city_A == program_city_B) %>%\
        as.data.frame()\
    \}\
    if (i == "top_top_program_close_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "top",\
               program_state_A == program_state_B) %>%\
        as.data.frame()\
      \
    \}\
    if (i == "top_top_program_far_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "top",\
               program_state_A != program_state_B) %>%\
        as.data.frame()\
      \
    \}\
    if (i == "top_bot_program_same_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "bot",\
               program_city_A == program_city_B) %>%\
        as.data.frame()\
    \}\
    if (i == "top_bot_program_close_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "bot",\
               program_state_A == program_state_B) %>%\
        as.data.frame()\
    \}\
    if (i == "top_bot_program_far_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top",\
               program_tier_B == "bot",\
               program_state_A != program_state_B) %>%\
        as.data.frame()\
    \}\
    if (i == "bot_top_program_same_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "top",\
               program_city_A == program_city_B) %>%\
        as.data.frame()\
      \
    \}\
    if (i == "bot_top_program_close_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "top",\
               program_state_A == program_state_B) %>%\
        as.data.frame()\
    \}\
    if (i == "bot_top_program_far_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "top",\
               program_state_A != program_state_B) %>%\
        as.data.frame()\
    \}\
    \
    if (i == "bot_bot_program_same_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "bot",\
               program_city_A == program_city_B) %>%\
        as.data.frame()\
    \}\
    \
    if (i == "bot_bot_program_close_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "bot",\
               program_state_A == program_state_B) %>%\
        as.data.frame()\
    \}\
    if (i == "bot_bot_program_far_location") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot",\
               program_tier_B == "bot",\
               program_state_A != program_state_B) %>%\
        as.data.frame()\
    \}\
    if (i == "top_nomatch") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "top") %>%\
        as.data.frame()\
      working_db$program_B <- "NO MATCH"\
      working_db$program_city_B <- "-"\
      working_db$program_rank_B <- 9999\
    \}\
    if (i == "bot_nomatch") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_A == "bot") %>%\
        as.data.frame()\
      working_db$program_B <- "NO MATCH"\
      working_db$program_city_B <- "-"\
      working_db$program_rank_B <- 9999\
    \}\
    if (i == "nomatch_top") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_B == "top") %>%\
        as.data.frame()\
      working_db$program_A <- "NO MATCH"\
      working_db$program_city_A <- "-"\
      working_db$program_rank_A <- 9999\
    \}\
    if (i == "nomatch_bot") \{\
      \
      working_db <- working_db %>%\
        filter(program_tier_B == "bot") %>%\
        as.data.frame()\
      working_db$program_A <- "NO MATCH"\
      working_db$program_city_A <- "-"\
      working_db$program_rank_A <- 9999\
    \}\
    \
    # calculate rank_sum by taking 1/rank\
    working_db$program_rank_sum <- 1/working_db$program_rank_A + 1/working_db$program_rank_B\
    working_db <- working_db[order(-working_db$program_rank_sum),] \
    step3<-working_db\
    \
    # concatenate to the final list    \
    if (!is.data.frame(final_list)) \{\
      final_list <- step3\
    \}\
    else \{\
      final_list <- rbind(final_list, step3)\
    \}\
  \}\
  \
  # final list\
  final_rank_list <- final_list %>%\
    distinct(program_A,program_B,.keep_all = T) %>%\
    mutate(rank =1:n()) %>%\
    select(program_A,program_B, program_city_A,program_city_B, rank)\
  \
  return(final_rank_list)\
\}\
\
extract_input_rank_list <- function(input) \{\
  rank_db <- data.frame()\
  #print(input$program_name_A_25)\
  #browser()\
  rank_db <- data.frame(\
    student = c("A","A","A","A","A",\
                "A","A","A","A","A",\
                "A","A","A","A","A",\
                "A","A","A","A","A",\
                "A","A","A","A","A",\
                "B","B","B","B","B",\
                "B","B","B","B","B",\
                "B","B","B","B","B",\
                "B","B","B","B","B",\
                "B","B","B","B","B"\
                ), \
    program_name = c(input$program_name_A_1, input$program_name_A_2, input$program_name_A_3,input$program_name_A_4,input$program_name_A_5,\
                     input$program_name_A_6, input$program_name_A_7, input$program_name_A_8,input$program_name_A_9,input$program_name_A_10,\
                     input$program_name_A_11, input$program_name_A_12, input$program_name_A_13,input$program_name_A_14,input$program_name_A_15,\
                     input$program_name_A_16, input$program_name_A_17, input$program_name_A_18,input$program_name_A_19,input$program_name_A_20,\
                     input$program_name_A_21, input$program_name_A_22, input$program_name_A_23,input$program_name_A_24,input$program_name_A_25,\
                     input$program_name_B_1, input$program_name_B_2, input$program_name_B_3,input$program_name_B_4,input$program_name_B_5,\
                     input$program_name_B_6, input$program_name_B_7, input$program_name_B_8,input$program_name_B_9,input$program_name_B_10,\
                     input$program_name_B_11, input$program_name_B_12, input$program_name_B_13,input$program_name_B_14,input$program_name_B_15,\
                     input$program_name_B_16, input$program_name_B_17, input$program_name_B_18,input$program_name_B_19,input$program_name_B_20,\
                     input$program_name_B_21, input$program_name_B_22, input$program_name_B_23,input$program_name_B_24,input$program_name_B_25\
                     ), \
    program_rank = c(1,2,3,4,5,6,7,8,9,10,\
                     11,12,13,14,15,16,17,18,19,20,\
                     21,22,23,24,25,\
                     1,2,3,4,5,6,7,8,9,10,\
                     11,12,13,14,15,16,17,18,19,20,\
                     21,22,23,24,25),\
    program_tier_temp = c(input$program_tier_A_1, input$program_tier_A_2, input$program_tier_A_3,input$program_tier_A_4,input$program_tier_A_5,\
                     input$program_tier_A_6, input$program_tier_A_7, input$program_tier_A_8,input$program_tier_A_9,input$program_tier_A_10,\
                     input$program_tier_A_11, input$program_tier_A_12, input$program_tier_A_13,input$program_tier_A_14,input$program_tier_A_15,\
                     input$program_tier_A_16, input$program_tier_A_17, input$program_tier_A_18,input$program_tier_A_19,input$program_tier_A_20,\
                     input$program_tier_A_21, input$program_tier_A_22, input$program_tier_A_23,input$program_tier_A_24,input$program_tier_A_25,\
                     input$program_tier_B_1, input$program_tier_B_2, input$program_tier_B_3,input$program_tier_B_4,input$program_tier_B_5,\
                     input$program_tier_B_6, input$program_tier_B_7, input$program_tier_B_8,input$program_tier_B_9,input$program_tier_B_10,\
                     input$program_tier_B_11, input$program_tier_B_12, input$program_tier_B_13,input$program_tier_B_14,input$program_tier_B_15,\
                     input$program_tier_B_16, input$program_tier_B_17, input$program_tier_B_18,input$program_tier_B_19,input$program_tier_B_20,\
                     input$program_tier_B_21, input$program_tier_B_22, input$program_tier_B_23,input$program_tier_B_24,input$program_tier_B_25\
                      ),\
    program_city = c(input$program_city_A_1, input$program_city_A_2, input$program_city_A_3,input$program_city_A_4,input$program_city_A_5,\
                     input$program_city_A_6, input$program_city_A_7, input$program_city_A_8,input$program_city_A_9,input$program_city_A_10,\
                     input$program_city_A_11, input$program_city_A_12, input$program_city_A_13,input$program_city_A_14,input$program_city_A_15,\
                     input$program_city_A_16, input$program_city_A_17, input$program_city_A_18,input$program_city_A_19,input$program_city_A_20,\
                     input$program_city_A_21, input$program_city_A_22, input$program_city_A_23,input$program_city_A_24,input$program_city_A_25,\
                     input$program_city_B_1, input$program_city_B_2, input$program_city_B_3,input$program_city_B_4,input$program_city_B_5,\
                     input$program_city_B_6, input$program_city_B_7, input$program_city_B_8,input$program_city_B_9,input$program_city_B_10,\
                     input$program_city_B_11, input$program_city_B_12, input$program_city_B_13,input$program_city_B_14,input$program_city_B_15,\
                     input$program_city_B_16, input$program_city_B_17, input$program_city_B_18,input$program_city_B_19,input$program_city_B_20,\
                     input$program_city_B_21, input$program_city_B_22, input$program_city_B_23,input$program_city_B_24,input$program_city_B_25\
                    ),\
    program_state = c(input$program_state_A_1, input$program_state_A_2, input$program_state_A_3,input$program_state_A_4,input$program_state_A_5,\
                      input$program_state_A_6, input$program_state_A_7, input$program_state_A_8,input$program_state_A_9,input$program_state_A_10,\
                      input$program_state_A_11, input$program_state_A_12, input$program_state_A_13,input$program_state_A_14,input$program_state_A_15,\
                      input$program_state_A_16, input$program_state_A_17, input$program_state_A_18,input$program_state_A_19,input$program_state_A_20,\
                      input$program_state_A_21, input$program_state_A_22, input$program_state_A_23,input$program_state_A_24,input$program_state_A_25,\
                      input$program_state_B_1, input$program_state_B_2, input$program_state_B_3,input$program_state_B_4,input$program_state_B_5,\
                      input$program_state_B_6, input$program_state_B_7, input$program_state_B_8,input$program_state_B_9,input$program_state_B_10,\
                      input$program_state_B_11, input$program_state_B_12, input$program_state_B_13,input$program_state_B_14,input$program_state_B_15,\
                      input$program_state_B_16, input$program_state_B_17, input$program_state_B_18,input$program_state_B_19,input$program_state_B_20,\
                      input$program_state_B_21, input$program_state_B_22, input$program_state_B_23,input$program_state_B_24,input$program_state_B_25\
                      )\
    )\
  \
  rank_db$program_tier [rank_db$program_tier_temp == TRUE] <- "top"\
  rank_db$program_tier [rank_db$program_tier_temp == FALSE] <- "bot"\
  # remove blank\
  rank_db <- subset(rank_db, program_name != "")\
\
  return(rank_db)\
\}  \
\
# server\
function(input, output, session) \{\
  #\
  # https://stackoverflow.com/questions/70155520/how-to-make-datatable-editable-in-r-shiny\
 \
  # initialize values\
  blank_rank_db <- data.frame(\
    student = character(),\
    program_name = character(),\
    program_rank = numeric(), \
    program_tier  = logical(), \
    program_city  = character(), \
    program_state  = character()\
  )\
  rv_A <- reactiveValues(data = blank_rank_db, orig=NULL)\
  rv_B <- reactiveValues(data = blank_rank_db, orig=NULL)\
  \
  # STUDENT A\
  output$student_A_rank_list <- renderDT(\{\
    datatable(rv_A$data,\
              colnames =c( "Student"="student", \
                           "Rank" = "program_rank" , \
                           "Program" = "program_name" , \
                           "Personal Tier (top/bot)" = "program_tier" ,\
                           "City" = "program_city" ,\
                           "State" = "program_state"),\
              editable = 'cell', selection = 'single' , options = list(dom = 't', paging=FALSE) ,rownames = F)\
  \}, server = T )\
  \
  observeEvent(input$student_A_rank_list_cell_edit, \{\
    row  <- input$student_A_rank_list_cell_edit$row\
    clmn <- input$student_A_rank_list_cell_edit$col\
    rv_A$data[row, clmn+1] <- input$student_A_rank_list_cell_edit$value\
  \})\
  \
  observeEvent(input$student_A_add_row, \{\
    nextrank <- 1\
    if(!is_empty(rv_A$data$program_rank)) \{\
      nextrank <- max(rv_A$data$program_rank) +1\
    \}\
    empty_rank_db_A <- data.frame(\
      student = "A",\
      program_name = " ",\
      program_rank = nextrank,\
      program_tier  = "top",\
      program_city  = " ",\
      program_state  = " "\
    )\
    rv_A$data <- rbind(rv_A$data, empty_rank_db_A )\
  \})\
  \
  observeEvent(input$student_A_delete_row,\{\
    if (!is.null(input$student_A_rank_list_rows_selected)) \{\
      rv_A$data <- rv_A$data[-as.numeric(input$student_A_rank_list_rows_selected),]\
      \
      ####### NEED TO DECREASE NEXT RANKS BY 1 ######\
    \}\
  \})\
  \
  \
  # STUDENT B\
  output$student_B_rank_list <- renderDT(\{\
    datatable(rv_B$data, \
              colnames =c( "Student"="student", \
                           "Rank" = "program_rank" , \
                           "Program" = "program_name" , \
                           "Personal Tier (top/bot)" = "program_tier" ,\
                           "City" = "program_city" ,\
                           "State" = "program_state"),\
              \
              editable = 'cell', selection = 'single', options = list(dom = 't' ,paging=FALSE),rownames = F)\
  \}, server = T )\
  \
  observeEvent(input$student_B_rank_list_cell_edit, \{\
    ##### EDITING FOR B DOESN'T WORK RIGHT ######\
    row  <- input$student_B_rank_list_cell_edit$row\
    clmn <- input$student_B_rank_list_cell_edit$col\
    rv_B$data[row, clmn+1] <- input$student_B_rank_list_cell_edit$value\
  \})\
  \
  observeEvent(input$student_B_add_row, \{\
    nextrank <- 1\
    if(!is_empty(rv_B$data$program_rank)) \{\
      nextrank <- max(rv_B$data$program_rank) +1\
    \}\
    empty_rank_db_B <- data.frame(\
      student = "B",\
      program_name = " ",\
      program_rank = nextrank,\
      program_tier  = "top",\
      program_city  = " ",\
      program_state  = " "\
    )\
    rv_B$data <- rbind(rv_B$data, empty_rank_db_B )\
  \})\
  \
  observeEvent(input$student_B_delete_row,\{\
    \
    ##### DELETING FOR B DOESN'T WORK RIGHT ######\
    if (!is.null(input$student_B_rank_list_rows_selected)) \{\
      rv_B$data <- rv_B$data[-as.numeric(input$student_B_rank_list_rows_selected),]\
      \
      ####### NEED TO DECREASE NEXT RANKS BY 1 ######\
    \}\
  \})\
  \
\
  # LOADING MECHANISM\
  observeEvent(input$load, \{\
    \
    print("load")\
    \
    file <- input$file\
    ext <- tools::file_ext(file$datapath)\
    req(file)\
    validate(need(ext == "csv", "Please upload a csv file"))\
    input_file <- read.csv(file$datapath, header = T, sep = "," ,na.strings =c(NA, ""))\
    \
    ######### NEED TO CHECK CORRECT FORMAT#########\
    # if(is_correct_format(rv$orig)) \{ \}\
    rv_A$data <- subset(input_file, student == "A")\
    rv_B$data <- subset(input_file, student == "B")\
    #rv$data <- rv$orig\
  \})\
  \
  # CLEAR PERSONAL LIST TABLES\
  observeEvent(input$clear, \{\
    print("clear")\
    rv_A$data <- blank_rank_db\
    rv_B$data <- blank_rank_db\
  \})\
  \
  \
  # SAVING PERSONAL LIST MECHANISM\
  output$save <- downloadHandler(\
    filename = function() \{ \
      paste0("rank_list_input",".csv") \
      \},\
    contentType = "text/csv",\
    content = function(file) \{\
      rank_db <- rbind(rv_A$data, rv_B$data)\
      write.csv(rank_db, file, na = "" ,row.names = F)\
    \}\
  )\
  \
 # RUNNING ALGORITHM  \
 observeEvent(input$go, \{\
\
   strategy_list <- input$strategy_list\
   \
   rank_db <- rbind(rv_A$data, rv_B$data)\
\
   final_rank_list <- generate_couples_rank_list(rank_db, strategy_list)\
   \
   output$final_rank_list_DT <- renderDT(\{\
     datatable(final_rank_list,\
               colnames =c( "Program Name (A)"="program_A", \
                            "Program Name (B)" = "program_B" , \
                            "City (A)" = "program_city_A" , \
                            "City (B)" = "program_city_B" ,\
                            "Coupled Rank" = "rank"),\
               selection = 'none' , \
               rownames = F,\
               extensions = 'Buttons',\
               options = list(\
                  dom = 'Bfrtip', \
                  paging=FALSE, \
                  buttons = list('copy', 'print', \
                                 list(\
                                   extend = 'collection',\
                                   buttons = c('csv', 'excel', 'pdf'),\
                                   text = 'Download'\
                                     ))))\
   \}, server = T )\
   updateTabsetPanel(session, "TabSet", selected = "Run")\
  \})\
  \
  \
\} # end server\
}