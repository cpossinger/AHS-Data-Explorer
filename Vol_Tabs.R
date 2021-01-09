library(foreach)
library(tibble)
library(ggplot2)
library(shiny)
library(dplyr)
library(magrittr)
library("DT")
library(tidyr)
library(readxl)
library(purrr)
library(readr)
library(fs)
library(stringr)
library(RCurl)
library(xtable)
library(broom)
library(treemapify)
library(reshape2)
#library(devtools)
library(plyr)
library(plotly)


# User Interface #####

ui <- fluidPage(
  
  titlePanel("Adventist Health Study Data Explorer"),
  
  tabsetPanel(id = 'vol_tabs',type = "tabs",
              # : Pages Tab #### 
              tabPanel("Pages",value = 'Pages', 
                       sidebarLayout(
                         sidebarPanel( 
                           fluidRow(column(6, selectInput("page", "Page List", page.list),
                                    actionButton("next_page","Next Page"),
                                    actionButton("previous_page","Previous Page")),
                                    column(6, sliderInput("zoom_slider","Zoom In",min = 750,max = 1200,value = 800)) 
                           ),
                           tags$h4("Questions"),
                           dataTableOutput("questions_on_page"),
                           actionButton("view_page_question", "View Question"),
                           
                          ),
                         mainPanel(
                           
                           tabsetPanel(
                               tabPanel("Version 1", value="v1_page",
                                        uiOutput("v1_page_img")
                                       
                               ),
                              tabPanel("Version 2", value="v2_page",
                                       uiOutput("v2_page_img")
                                       
                                       ),
                               tabPanel("Version 3", value="v3_page",
                                        uiOutput("v3_page_img")
                                        
                               )
                             )
                           )
                       )
              ),
              # :FFQ Questions Tab ####
              tabPanel("FFQ Questions",
                       verticalLayout(
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(column(6, selectInput("ffq_nutr", "Columns Selected:", choices = names(vol_ffqrecipe) %>% 
                                        setdiff(c("VNAME", "fcmb_name", "fcmb_id", "ingr_id", "ingr_name","recp_id","recp_name")),
                                        selected = "kcal",multiple = TRUE)),
                                    column(6, sliderInput("ffq_digits", "Round to:", min = 0, max = 5, value = 3)) 
                           
                                    ),
                           tags$hr(),
                                      tabsetPanel(
                                      tabPanel("Bar Plot",
                                      tags$strong("Response Distribution"),
                                      plotlyOutput("dietplot")),
                                      tabPanel("Table",
                                      tags$hr(),
                                      tags$strong("Response Distribution"),
                                      DT::dataTableOutput("response_dist_table")
                                                )
                         )),
                         
                         mainPanel( 
                           tags$strong("Food Combination Names"),
                           DT::dataTableOutput("fcmb_table"),
                           tags$strong("Recipes"),
                           DT::dataTableOutput("fcmb_recp_table"),
                           actionButton("go_to_recipe", "View Ingredients"))
                         
                         
                       )
                       )
              ),
              
              # :Write-In Questions Tab ####
              tabPanel("Write-In Questions",
                       verticalLayout(
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow(column(6, selectInput("writein_nutr", "Columns Selected:", choices = names(vol_ffqrecipe) %>% 
                                                              setdiff(c("VNAME", "fcmb_name", "fcmb_id", "ingr_id", "ingr_name","recp_id","recp_name")),
                                                            multiple = TRUE,selected = "kcal")),
                                      column(6, sliderInput("writein_digits", "Round to:", min = 0, max = 5, value = 3)) 
                                      
                             ),
                             tags$hr(),
                             tags$strong("Write-in Questions"),
                             DT::dataTableOutput("writein_table"),
                           ),
                           
                           mainPanel( 
                             tags$strong("Responses"),
                             DT::dataTableOutput("writein_recp_table"),
                             actionButton("go_to_recipe_write", "View Ingredients"),
                             textOutput("no_recp_id_write")
                           )
                           
                         )
                       )
              ),
              
              # :Recipes Tab ####
              
              tabPanel("Recipes", 
                       sidebarLayout(
                       sidebarPanel(
                         fluidRow(column(6, selectInput("recipe_nutr", "Columns Selected:",
                                                        choices = names(vol_ffqrecipe) %>% setdiff(c("VNAME", "fcmb_name", "fcmb_id", "ingr_id", "ingr_name", "recp_id", "recp_name")),
                                                        multiple = TRUE,selected = "kcal")),
                                  column(6, sliderInput("recp_digits", "Round to:", min = 0, max = 5, value = 3)) 
                                  
                       )),
                       mainPanel(
                       tabsetPanel(id = "recipe_type",
                         tabPanel("FFQ Recipes", 
                                  tags$h4("Recipes"),
                                  actionButton("recp_to_question","View FFQ Question"),
                                  DT::dataTableOutput("hard_coded_recp"),
                                  tags$h4("Ingredients"),
                                  DT::dataTableOutput("hard_coded_ingr")),
                                  
                                
                         tabPanel("Write-in Recipes",
                                  tags$h4("Recipes"),
                                  actionButton("writein_recp_to_question","View Write-In Question"),
                                  textOutput("no_writein_question"),
                                  DT::dataTableOutput("writein_recp"),
                                  tags$h4("Ingredients"),
                                  DT::dataTableOutput("writein_ingr"))
                                  )
                       ))
                       ),
              
                       
              # : Nutrients Tab ####
              
              tabPanel("Nutrients",value = 'Nutrients', 
                       verticalLayout(
                         sidebarLayout(
                         sidebarPanel(
                           fluidRow(column(6, selectInput("nutr", "Nutrient List", nutrient.list,selected = "kcal")), 
                                    column(6, sliderInput("nutr_digits", "Round to:", min = 0, max = 5, value = 3)) 
                           ),
                         
                         tabsetPanel(  
                          tabPanel("Per-Subject", 
                         tags$strong("Per-Subject Nutrient Totals"),
                         DT::dataTableOutput("subj_table"),
                         tags$strong("Subject Nutrient Summary"),
                         DT::dataTableOutput("subj_sum")),
                         tabPanel("Summary",
                                  checkboxInput("nutrlog", "Log Transform"),
                                  plotlyOutput("nutrplot"),
                                  tags$strong("Summary"),
                                  tableOutput("nutrplot_sum")
                         ))),
                         mainPanel(
                           titlePanel("Nutrient Sources"),
                         tabsetPanel(
                           tabPanel("Hard-Coded Questions",
                             DT::dataTableOutput("subj_select"),
                             actionButton("nutr_button","View FFQ Question"),
                             tags$hr(),
                             tableOutput("range_vals"),
                             tags$hr(),
                             plotlyOutput("treemap")
                           ),
                           
                           tabPanel("Write-In Questions",
                                    DT::dataTableOutput("writein_subj_select"),
                                    actionButton("write_in_to_recp","View Write-In Recipe"),
                                    tags$hr(),
                                    tableOutput("writein_range_vals")
                           )
                         ),
                         )
                         )
                         )
                         ),
                       
                       
              
              # : Food Groups Tab ####
              
              tabPanel("Food Groups",
                       sidebarLayout(
                         sidebarPanel(
                           tags$strong("Food Group Type"),
                           tabsetPanel(id = "non_or_ovr_tabs",
                             tabPanel("Recipe-Based", value = "non_tab",
                           selectInput("nonoverlap_groups", "Food Groups", names(fg_data$nonoverlapping))
                           ),
                           tabPanel("Ingredient-Based", value = "ovr_tab",
                             selectInput("ovr_groups","Food Groups", names(fg_data$overlapping))
                           )
                           )
                           ),
                         
                       mainPanel(
                       tags$strong("Food Group Components"),
                       dataTableOutput("food_groups_table"),
                       tags$strong("Matching Recipes"),
                       tabsetPanel(
                         tabPanel("FFQ Recipes",
                                  DT::dataTableOutput("hard_coded_food_groups"),
                                  actionButton("hard_coded_to_recipe", "View FFQ Question")
                                  ),
                         tabPanel("Write-in Recipes",
                                  DT::dataTableOutput("writein_food_groups"),
                                  actionButton("write_to_questions","View Write-in Recipe")),
                         tabPanel("Recall Recipes",
                                  DT::dataTableOutput("recall_food_groups"),
                                  actionButton("to_recalls","View Recall Recipe"),
                                
                                 )
                       )
                       )
                       )
  ),
  
# : Recalls Tab ####

  tabPanel("Recalls",  
           verticalLayout(
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(6, selectInput("recall_nutr", "Nutrient List", nutrient.list.g,selected = "kcal")), 
                          column(6, sliderInput("recall_digits", "Round to:", min = 0, max = 5, value = 3)) 
                 
                 
                 ),
                 tags$hr(),
                 tags$h4("Subject Nutrient Summary"),
                 DT::dataTableOutput("recall_subj"),
                 tags$h4("Recall Instance"),
                 DT::dataTableOutput("recall_inst")
                 
               ),
               mainPanel(
                            tags$h4("Selected Recipes"),
                            DT::dataTableOutput("recall_recp"),
                            #actionButton("recall_nutr_button","View FFQ Question"),
                            tags$h4("Selected Ingredients"),
                            DT::dataTableOutput("recall_ingr"),
                            tags$hr(),
                            plotlyOutput("recall_treemap")
               )
             )
           )
  )
  
)
)


# Server Code ####

server <- function(input, output, session) {
  
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")
  
  # Pages Tab ####
  
  # Example img src : https://wiki.ahs2.org/_media/baseline:ahs_baseline_v1_-_page_e4.jpg
  output$v1_page_img <- renderUI(tags$img(src=paste0("https://wiki.ahs2.org/_media/baseline:ahs_baseline_v1_-_page_", input$page, ".jpg"), width= as.integer(input$zoom_slider)/1.5,height = as.integer(input$zoom_slider)))
  output$v2_page_img <- renderUI(tags$img(src=paste0("https://wiki.ahs2.org/_media/baseline:ahs_baseline_v2_-_page_", input$page, ".jpg"), width= as.integer(input$zoom_slider)/1.5,height = as.integer(input$zoom_slider)))
  output$v3_page_img <- renderUI(tags$img(src=paste0("https://wiki.ahs2.org/_media/baseline:ahs_baseline_v3_-_page_", input$page, ".jpg"), width= as.integer(input$zoom_slider)/1.5,height = as.integer(input$zoom_slider)))
 
 observeEvent(input$next_page,{
   next_selected <- page.list[which(page.list %in% input$page)+1]
   print(next_selected)
   if(next_selected %>% is.na){
     next_selected <- "B2" 
     updateSelectInput(session,"page", selected = next_selected)
     }
   else{updateSelectInput(session,"page", selected = next_selected)}
 })
  
 
 observeEvent(input$previous_page,{
   previous_selected <- page.list[which(page.list %in% input$page)-1]
   print(previous_selected)
   if(length(previous_selected) == 0){
     previous_selected <- "F10" 
     updateSelectInput(session,"page", selected = previous_selected)
   }
   else{updateSelectInput(session,"page", selected = previous_selected )}
 })
  
  # Question variables from page table
  questions_on_page <- reactive({
    metadata  %>% 
      filter(str_detect(SectionAndPage,paste0(input$page,"\\b"))) %>%  # Grab all rows that match selected page from input$page
      filter(Frequency == 1) %>% 
      select(Freq=QName, Amount=AmountCol, `Write-in`=WriteInName)
  })
  
  # Render Questions on Page table
  output$questions_on_page <- renderDataTable({ 
    datatable(questions_on_page(), selection = list(mode = "single", 
                                                   selected = c(1),
                                                  target = 'row'), 
                                  options = list(lengthChange = FALSE, 
                                                 pageLength = 5), callback = JS(js))
      })
  
  
  # When the View Question button is pressed on the Page Tab
  observeEvent(input$view_page_question,{
    
    selected <- questions_on_page()[input$questions_on_page_rows_selected,]
    
    if(selected$`Write-in` %>% is.na){
      selected_fcmb(selected$Freq)
      updateTabsetPanel(session, "vol_tabs", "FFQ Questions")
    } else{
      selected_writein(selected$`Write-in`)
      updateTabsetPanel(session, "vol_tabs", "Write-In Questions")
    }
    
  })
  
  observe({
    pagenum <- ceiling(input$fcmb_table_write_rows_selected/10)
    selectPage(writein_table_proxy, pagenum)
  })
 
   # Questions Tab #### 
  
  ### Output for FFQ Recipe tab
 
  # : FCMB table #### 
  # List of VNAMEs/fcmbs
  
  fcmb_table <- reactive({
    
    fcmb_select %>% 
      left_join(metadata, by = c("VNAME"="QName")) %>% 
      select(Page = SectionAndPage.x,
             `FFQ Question` = VNAME,
             `FCMB Name`=fcmb_name,
             one_of(input$ffq_nutr)
             
             
      )
     
  })
  
  # Which FCMB is selected? Defaults to NA
  # Example value: "GRAPE1F"
  selected_fcmb <- reactiveVal(value = NA)
  
  # Render the table of FFQ Questions (Food Combinations thereof)
  output$fcmb_table <- DT::renderDataTable({
    
    if(selected_fcmb() %>% is.na) { selection_row <- 1}
    else { selection_row <- which((fcmb_table()[,"FFQ Question"]) == selected_fcmb()) }
  
    datatable(data = fcmb_table() ,
                selection = list(mode = "single", selected = selection_row,target = 'row'),
              options = list(lengthChange = FALSE,pageLength = 10, stateSave = TRUE,order = fcmb_order$order)) %>%
      formatRound(columns = input$ffq_nutr,digits = input$ffq_digits)
    
  }, server=FALSE)
  
  
  fcmb_table_proxy = DT::dataTableProxy("fcmb_table")
  
  fcmb_selected_page <- reactiveVal(0)

  fcmb_order <- reactiveValues()
  
  observe({
    #input$fcmb_table_state
    isolate({
      fcmb_order$val <- input$fcmb_table_state$start / input$fcmb_table_state$length + 1
      if(!is.null(input$fcmb_table_state$order)){
        fcmb_order$order <- input$fcmb_table_state$order
      }
    })
  })
  
  
  
  fcmb_sort_col  <- reactive({
    #if(length(input$fcmb_table_rows_selected) != 0){
    input$fcmb_table_state$order[[1]][[1]]#}
      })
  fcmb_sort_dir  <- reactive({input$fcmb_table_state$order[[1]][[2]]})
  fcmb_sort_name <- reactive({names(fcmb_table())[fcmb_sort_col()]})

  fcmb_data <- reactive({fcmb_table() %>%  mutate(rownum = 1:n())})

  fcmb_sort_data <- reactive({
    if(fcmb_sort_dir() == "desc"){fcmb_data()[order(fcmb_data()[[fcmb_sort_name()]],decreasing = TRUE),"rownum"]}
    else{fcmb_data()[order(fcmb_data()[[fcmb_sort_name()]]),"rownum"]}
    })
  fcmb_selection_row <- reactive({
    if(length(input$fcmb_table_state$order) > 0){
    which(fcmb_sort_data() == input$fcmb_table_rows_selected)
      }
    else{input$fcmb_table_rows_selected}
    })


  fcmb_pagenum <- reactive({ceiling(fcmb_selection_row()/10)})

  observe({

    if(length(input$fcmb_table_rows_selected) != 0){

      print(input$fcmb_table_state$order)

        # cat("sort_name:", fcmb_sort_name(), "\n")
        # cat("SORTED SELECTION ROW\n")
        # cat("RAW SELECTION ROW\n")

      if(input$fcmb_table_state$start == 0 & fcmb_selected_page() != fcmb_pagenum() ){
        cat("updating page\n")
        selectPage(fcmb_table_proxy, fcmb_pagenum())
        fcmb_selected_page(fcmb_pagenum())
      }
      cat("input$fcmb_table_rows_selected:", input$fcmb_table_rows_selected,
          "selection_row:", fcmb_selection_row(),
          "pagenum:", fcmb_pagenum(),
          "fcmb_selected_page:", fcmb_selected_page(),
          "input$fcmb_table_state$start:", input$fcmb_table_state$start,
          "\n")
    }
  })


  # observeEvent({input$ffq_nutr;input$ffq_digits},{
  #   if(length(input$fcmb_table_rows_selected) != 0){
  #     if(input$fcmb_table_state$start == 0 & fcmb_selected_page() != fcmb_pagenum() ){
  #     selectRows(fcmb_table_proxy,fcmb_selection_row())
  #     selectPage(fcmb_table_proxy,fcmb_pagenum())
  #     }
  #     else{selectRows(fcmb_table_proxy,input$fcmb_table_rows_selected)
  #       selectPage(fcmb_table_proxy,ceiling(input$fcmb_table_rows_selected/10))}
  #     #fcmb_selected_page(fcmb_pagenum())
  #   }

  #})

  # observe({
  #   
  #   if(length(input$fcmb_table_rows_selected) != 0){
  #     
  #     print(input$fcmb_table_state$order)     
  #     if(length(input$fcmb_table_state$order) > 0){
  #       sort_col <- input$fcmb_table_state$order[[1]][[1]]    
  #       sort_dir <- input$fcmb_table_state$order[[1]][[2]]
  #       sort_name <- names(fcmb_table())[sort_col]
  #       cat("sort_name:", sort_name, "\n")
  #       data <- fcmb_table() %>%  mutate(rownum = 1:n())
  #       if(sort_dir == "desc"){sort_data <- data[order(data[[sort_name]],decreasing = TRUE),"rownum"]}
  #       else{sort_data <- data[order(data[[sort_name]]),"rownum"]}
  #       selection_row <- which(sort_data == input$fcmb_table_rows_selected)
  #       cat("SORTED SELECTION ROW\n")
  #     } else {
  #       selection_row <- input$fcmb_table_rows_selected
  #       cat("RAW SELECTION ROW\n")
  #     }
  #     
  #     pagenum <- ceiling(selection_row/10)
  #     
  #     if(input$fcmb_table_state$start == 0 & fcmb_selected_page() != pagenum ){
  #       cat("updateing page\n")
  #       selectPage(fcmb_table_proxy, pagenum)
  #       fcmb_selected_page(pagenum)
  #     }
  #     cat("input$fcmb_table_rows_selected:", input$fcmb_table_rows_selected,
  #         "selection_row:", selection_row, 
  #         "pagenum:", pagenum, 
  #         "fcmb_selected_page:", fcmb_selected_page(), 
  #         "input$fcmb_table_state$start:", input$fcmb_table_state$start, 
  #         "\n")
  #   }
  # })
  
  output$dietplot <-renderPlotly({ 
    
    data <- fcmb_select %>% 
      left_join(metadata, by = c("VNAME"="QName"))
    
    c <- fcmb_select[input$fcmb_table_rows_selected,"VNAME"] %>% as.character
    
    d <- vol_dietary[[c]]  
    if(is.null(d)){ return(NULL)}
    d %<>% na.omit %>% table  %>% data.frame 
    
    d %<>% select(.data = d,`Frequency Number` = .,Subjects = Freq)
    
    if(!is.null(input$fcmb_table_rows_selected) && !is.na(input$fcmb_table_rows_selected)){
      res_cat_dict <- res_cat_dict %>% filter(ResponseCategory %in% as.character(data[input$fcmb_table_rows_selected,"ResponseCategory"])) %>% 
        select(starts_with("T")) %>% 
        select(-Type,-Trend)
      res_cat_dict <- res_cat_dict[,colSums(is.na(res_cat_dict))<nrow(res_cat_dict)]  
      names(res_cat_dict) <- gsub("T", '', names(res_cat_dict))
      res_cat_dict %>% mutate(`Response Category` = data[input$fcmb_table_rows_selected,"ResponseCategory"] %>% as.character) %>%
        select(`Response Category`, everything())
      
    }
    
    range_vals <- data.frame(freq = colnames(res_cat_dict), freq_values = t(res_cat_dict[1,]))
    range_vals$freq_values %<>% factor(levels = na.omit(range_vals$freq_values %>% as.character))
    d %<>% left_join(range_vals,by = c("Frequency Number"="freq")) %>% select(`Frequency Value` = freq_values,everything())
    
    d$`Frequency Value`
    
    percentage <- d$Subjects/sum(d$Subjects)*100 
    plot <- ggplot(d, mapping = aes(x = Subjects, 
                                    y =`Frequency Value`,
                                    fill = `Frequency Value`,
                                    text = paste(round(percentage,digits = 2),"%"))) +
      geom_col() + 
      ylab("Frequency values") +
      xlab("Subjects") +
      theme(legend.position = "none")
    
    ggplotly(plot,tooltip=c("text","x","y"))
      
    })  
  
  response_dist_table <- reactive({
    
    data <- fcmb_select %>% 
      left_join(metadata, by = c("VNAME"="QName"))
    
    if(!is.null(input$fcmb_table_rows_selected) && !is.na(input$fcmb_table_rows_selected)){
      res_cat_dict <- res_cat_dict %>% filter(ResponseCategory %in% as.character(data[input$fcmb_table_rows_selected,"ResponseCategory"])) %>% 
        select(starts_with("T")) %>% 
        select(-Type,-Trend)
      res_cat_dict <- res_cat_dict[,colSums(is.na(res_cat_dict))<nrow(res_cat_dict)]  
      names(res_cat_dict) <- gsub("T", '', names(res_cat_dict))
      res_cat_dict %>% mutate(`Response Category` = data[input$fcmb_table_rows_selected,"ResponseCategory"] %>% as.character) %>%
        select(`Response Category`, everything())
      
    }
    
    range_vals <- data.frame(freq = colnames(res_cat_dict), freq_values = t(res_cat_dict[1,]))
    
    c <- fcmb_select[input$fcmb_table_rows_selected,"VNAME"] %>% as.character
    
    d <- vol_dietary[[c]]  
    if(is.null(d)){ return(NULL)}
    d %<>% na.omit %>% table  %>% data.frame 
    d %<>% select(.data = d,`Frequency Number` = .,Subjects = Freq)
    percentage <- data.frame(Percentage = d$Subjects/sum(d$Subjects)*100,`Frequency Number`= d$`Frequency Number`) 
    print(percentage)
    d %<>% left_join(percentage,by = c("Frequency Number" = "Frequency.Number"))
    d %<>% left_join(range_vals,by = c("Frequency Number"="freq")) %>% select(`Frequency Value` = freq_values,everything()) %>% select(-`Frequency Number`)
    d
  })
  
  
  output$response_dist_table <- renderDataTable({
     datatable(response_dist_table(),selection = "none",options = list(lengthChange = FALSE,stateSave = TRUE,dom = "t")) %>% 
       formatRound(columns = "Percentage",digits = 2)
  })
  
  
  
  fcmb_recp_table <- reactive({
    fcmb_selected <- fcmb_select[input$fcmb_table_rows_selected,"VNAME"]
    recp_select %>% filter(VNAME %in% fcmb_selected)
  })
  
  
  output$fcmb_recp_table <- DT::renderDataTable({
    
    datatable(fcmb_recp_table() %>% select(`Recipe ID`=recp_id,`Recipe Name`=recp_name,input$ffq_nutr),selection = list(mode = "single",selected = 1,target = 'row'), 
              options = list(dom = "t", pageLength = 10,stateSave = TRUE,lengthChange = FALSE)) %>% 
      formatRound(columns = input$ffq_nutr,digits = input$ffq_digits)
  },server = FALSE)
  
  
  observeEvent(input$go_to_recipe, {
    selected_fcmb      <- fcmb_table()[input$fcmb_table_rows_selected,]
    selected_fcmb_recp <- fcmb_recp_table()[input$fcmb_recp_table_rows_selected,]
    
    selected_hard_coded$VNAME     <- selected_fcmb$`FFQ Question`
    selected_hard_coded$recp_id   <- selected_fcmb_recp$recp_id
    selected_hard_coded$recp_name <- selected_fcmb_recp$recp_name
    
    updateTabsetPanel(session, "vol_tabs", "Recipes")
    
  })
  
  observe({
    page <- ceiling(input$hard_coded_recp_rows_selected/10)
    selectPage(hard_coded_recp_proxy,page)
  })

  # Write-In Questions Tab #### 
  
  # : Write-Ins In Sidebar #### 
  # List of VNAMEs/fcmbs
  
  writein_table <- reactive({
    main_table <-metadata %>% 
       filter(Writein == 1) %>% 
      select(Page = SectionAndPage,
             `FFQ Question` = QName,
             `Write-In Name`=QLongName) %>% 
      relocate(`FFQ Question`, .after = Page)
      
      without_recp_id <- writein_data %>% group_by(writeincol) %>% 
        filter(is.na(recp_id)) %>% tally
      
      with_recp_id <- writein_data %>% group_by(writeincol) %>% 
        filter(!is.na(recp_id)) %>% tally
      
      main_table %<>% 
        left_join(   with_recp_id,by=c(`FFQ Question`= "writeincol")) %>% 
        left_join(without_recp_id,by=c(`FFQ Question`= "writeincol")) %>% 
        select(everything(),`With Recipe ID`= n.x, `Without Recipe ID`= n.y) %>% 
        replace_na(list(`Without Recipe ID`=0,`With Recipe ID` = 0))
      
  })
 
  # Selected write in question. Defaults to NA 
  # Example: "OTHSWT1L"
  selected_writein           <- reactiveVal(NA)
  
  # Render table of write-in questions on Writein Tab
  output$writein_table <- DT::renderDataTable({
    
    if(selected_writein() %>% is.na){
      
       selection_row <- 1; 
       selected_writein(writein_table()[[1,"FFQ Question"]])
      }
    
    else { selection_row <- which(writein_table()$`FFQ Question` == selected_writein()) 
            } 
    
    datatable(data = writein_table(), 
              selection = list(mode = "single", selected = selection_row, target = 'row'), 
              options = list(lengthChange = FALSE, pageLength = 10, stateSave = TRUE)) 
    
  }, server=FALSE)
  
  
  reindex_fcmb_table_write <- reactive({
    if(length(input$fcmb_table_write_rows_all) != 0){
    c(1:max(input$fcmb_table_write_rows_all)) %>% as.character 
    }
    })
  
  
  writein_table_proxy = DT::dataTableProxy("writein_table")
  
  
  observe({
    if(length(input$writein_table_rows_selected) == 1)
      selected_writein(writein_table()[[input$writein_table_rows_selected, "FFQ Question"]])
  })
  
  # : Recipes table #### 
  # List of recipes in a VNAME
  
  writein_recp_table <- reactive({
    writein_data %>% filter(writeincol %in% selected_writein()) %>%
      group_by(recp_name,recp_id,kcal,sfa,fat,gram,fiber,ca,fe) %>% 
      tally %>% 
      ungroup
    })
  
 
  output$writein_recp_table <- DT::renderDataTable({
    datatable(
      writein_recp_table() %>% select(Count = n,`Recipe ID`=recp_id,`Recipe Name`=recp_name,input$writein_nutr) %>% unique,
      selection = list(mode = "single", 
                       selected = 1,
                       target = 'row'), 
      options = list(dom = "fpt", 
                     pageLength = 10,
                     stateSave = TRUE,
                     lengthChange = FALSE, 
                     order = list(1, 'desc'))) %>% 
      formatRound(columns = input$writein_nutr, digits = input$writein_digits)
  }, server = FALSE)
  

writein_recp_table_proxy <- dataTableProxy("writein_recp_table")  
  
  no_recp_id_write <- reactive({
    "No Matching Recipe"
  })
 
observeEvent(input$go_to_recipe_write, {
  if(is.na(writein_recp_table()[input$writein_recp_table_rows_selected,"recp_id"])){ 
  output$no_recp_id_write <- renderText({
    no_recp_id_write() 
      })
    }
  else{ 
   print("else")
   selected <- writein_recp_table()[input$writein_recp_table_rows_selected,]
   selected_writein_recp$`Recipe ID` <- selected$recp_id
   selected_writein_recp$`Recipe Name` <- selected$recp_name
   
   updateTabsetPanel(session,"vol_tabs","Recipes")
   updateTabsetPanel(session,"recipe_type","Write-in Recipes")
    }
   })

  observe({
    page <- ceiling(input$writein_recp_rows_selected/10)
    selectPage(writein_recp_proxy,page)
    
  })
  
  # Observe Events for Reselection
  observeEvent(input$writein_nutr,{
    selected_page <- ceiling(input$writein_recp_table_row_last_clicked/10)
    selected_page <- ceiling(input$writein_recp_table_rows_selected/10)
    
    selectPage(writein_recp_table_proxy, selected_page)
    selectRows(writein_recp_table_proxy, input$writein_recp_table_row_last_clicked)
    selectRows(writein_recp_table_proxy, input$writein_recp_table_rows_selected)
  })
  
  
  observeEvent(input$writein_digits,{
    selected_page <- ceiling(input$writein_recp_table_row_last_clicked/10)
    selected_page <- ceiling(input$writein_recp_table_rows_selected/10)
    
    selectPage(writein_recp_table_proxy, selected_page)
    selectRows(writein_recp_table_proxy, input$writein_recp_table_row_last_clicked)
    selectRows(writein_recp_table_proxy, input$writein_recp_table_rows_selected)
  })
  
   # : Ingredients table #### 
  # Selected recipe(s) within a VNAME/fcmb
  # output$ingr_table <- DT::renderDataTable({
  #   if(!is.null(input$recp_table_rows_selected)){
  #     recp_selected <- recp_table() %>% slice(input$recp_table_rows_selected) %>% select(VNAME,recp_id,recp_name)
  #     recp_selected %<>% left_join(vol_ffqrecipe %>% mutate(VNAME = tolower(VNAME))) %>% select(ingr_id, ingr_name, one_of(input$ffqrecipe_cols))
  #     
  #   } else { return(NULL)}
  #   
  # })
  
  # Recipes Tab ####
  
  # : Hard-coded Recipes ####
  
  # . . Data Table ####
  hard_coded_recp <- reactive({
    recp_select %>% select(`FFQ Question` = VNAME,`Recipe ID`=recp_id,`Recipe Name`= recp_name, input$recipe_nutr) %>% 
      relocate(`FFQ Question`, .before = `Recipe ID`)
  })
  
  # . . Selection ####
  selected_hard_coded           <- reactiveValues(VNAME=NA_character_, recp_id = NA_character_, recp_name = NA_character_)
  
  # . . Output Table ####
  output$hard_coded_recp <- DT::renderDataTable({
     
    if( (selected_hard_coded$VNAME     %>% is.na) | 
        (selected_hard_coded$recp_id   %>% is.na) |
        (selected_hard_coded$recp_name %>% is.na)   ) {selection_row <- 1}
    else{selection_row <- which(hard_coded_recp()$`FFQ Question` == selected_hard_coded$VNAME   &
                                hard_coded_recp()$`Recipe ID`    == selected_hard_coded$recp_id &
                                hard_coded_recp()$`Recipe Name`  == selected_hard_coded$recp_name ) 
    }
    cat("FFQ Recipe Selected Row:", selection_row, "\n")
    
    datatable(hard_coded_recp(),
                selection = list(mode ="single",selected = selection_row,target = 'row'),options = list(stateSave = TRUE,lengthChange = FALSE)) %>% 
    formatRound(columns = input$recipe_nutr,digits = input$recp_digits)
    },server = F)
  
  

  hard_coded_recp_proxy = DT::dataTableProxy("hard_coded_recp")
  
  observeEvent(input$recp_to_question,{
    
    selected <- hard_coded_recp()[input$hard_coded_recp_rows_selected,]
    
    selected_fcmb(selected$`FFQ Question`)
    
    updateTabsetPanel(session,"vol_tabs","FFQ Questions")
  
  })
  
  output$hard_coded_ingr <- DT::renderDataTable({
    if(!is.null(input$hard_coded_recp_rows_selected)){
      recp_selected <- recp_select %>% slice(input$hard_coded_recp_rows_selected) %>% select(VNAME,recp_id,recp_name)
      recp_selected %<>% left_join(vol_ffqrecipe %>% 
                         select(`Ingredient ID`=ingr_id,`Ingredient Name`= ingr_name,VNAME, recp_id, recp_name, one_of(input$recipe_nutr)),by = c("VNAME", "recp_id", "recp_name")) %>% 
                         select(-recp_id,-VNAME,-recp_name) %>% unique
      datatable(recp_selected,selection = 'none',options = list(dom = 'pt')) %>% formatRound(columns = input$recipe_nutr,digits = input$recp_digits)    
     } else { return(NULL)}
    
  })
  
  # : Write-in Recipes ####
  
 
  # . . Data table  ####
  
  writein_recp <- reactive({
    
    all_recp_select %>% left_join(writein_data %>% select(recp_id,recp_name,writeincol),by = c("recp_id","recp_name")) %>% 
      select(`Recipe ID`=recp_id,`Recipe Name`= recp_name, input$recipe_nutr, writeincol) %>% unique
  })
 
  # . . Selection #####
  selected_writein_recp <- reactiveValues(`Recipe ID` = NA, `Recipe Name` = NA)
  
  # . . Output Table ####
  output$writein_recp <- DT::renderDataTable({
    
    if((selected_writein_recp$`Recipe ID`   %>% is.na)|
       (selected_writein_recp$`Recipe Name` %>% is.na)){selection_row <- 1}
  
    else{selection_row <- which(writein_recp()[,"Recipe ID"]   == selected_writein_recp$`Recipe ID` &
                                writein_recp()[,"Recipe Name"] == selected_writein_recp$`Recipe Name`)}
    
    datatable(writein_recp(),
              selection = list(mode = "single",
                               selected = selection_row,
                               target = 'row'),
              options = list(lengthChange = FALSE, 
                             stateSave = TRUE)) %>% 
      formatRound(columns = input$recipe_nutr,digits = input$recp_digits)
  },server = F)
  
  writein_recp_proxy <- dataTableProxy("writein_recp")
  
  no_writein_question <- reactive({
    "No Matching Write-In Question"
  })
  
  observeEvent(input$writein_recp_to_question,{
    if(is.na(writein_recp()[input$writein_recp_rows_selected,"writeincol"])){ 
      output$no_writein_question <- renderText({
        no_writein_question() 
      })
    }
    else{
    selected <- writein_recp()[input$writein_recp_rows_selected,]
    selected_writein(selected$`writeincol`)
    updateTabsetPanel(session,"vol_tabs","Write-In Questions")
    }
  })
  
  observe({
    selected_page <- ceiling(input$writein_recp_row_last_clicked/5)
    selectPage(writein_table_proxy, selected_page)
    selectRows(writein_table_proxy, input$writein_table_row_last_clicked)
  })
  
  # : Reselect Row (nutr upd) ####
  observeEvent({input$recipe_nutr; input$recp_digits},{
    # FFQ Question - reselection
    selected_page <- ceiling(input$hard_coded_recp_row_last_clicked/10)
    selected_page <- ceiling(input$hard_coded_recp_rows_selected/10)
    
    selectPage(hard_coded_recp_proxy, selected_page)
    selectRows(hard_coded_recp_proxy, input$hard_coded_recp_row_last_clicked)
    selectRows(hard_coded_recp_proxy, input$hard_coded_recp_rows_selected)
    
    # Write-in Recipe - reselection
    selected_page <- ceiling(input$writein_recp_row_last_clicked/10)
    selected_page <- ceiling(input$writein_recp_rows_selected/10)
    
    selectPage(writein_recp_proxy, selected_page)
    selectRows(writein_recp_proxy, input$writein_recp_row_last_clicked)
    selectRows(writein_recp_proxy, input$writein_recp_rows_selected)
  })
  
  
  output$writein_ingr <- DT::renderDataTable( {
    if(!is.null(input$writein_recp_rows_selected)){
      recp_selected <- all_recp_select %>% slice(input$writein_recp_rows_selected) %>% select(recp_id,recp_name)
      #cat("writein_recp rows selected",input$writein_recp_rows_selected)
      recp_selected %<>% left_join(vol_allrecipes) %>% select(`Ingredient ID`=ingr_id,`Ingredient Name`= ingr_name, one_of(input$recipe_nutr))
      datatable(recp_selected,selection = "none",options = list(dom = 'pf')) %>% 
      formatRound(columns = input$recipe_nutr,digits = input$recp_digits)
    } else { return(NULL)}
    
  })
  # Nutrients Tab ####
  
  # Nutrient Distribution 
  output$nutrplot <- renderPlotly({
    # Construct vector of nutrient data
    nutr_col <- vol_nutr %>% select(one_of(input$nutr))
    names(nutr_col) <- "nutr"
    # Select transformation function 
      if(input$nutrlog == TRUE) {
        trans_nutr <- log
      } else{
        trans_nutr <- identity
      }
    #})
   
    # Summarize Nutrient Distribution 
  nutr.sum <- summary(nutr_col$nutr) %>% trans_nutr

    # Created transformed data column
    nutr_col$nutr.t <- trans_nutr(nutr_col$nutr)
    
    # Plot distribution and summary lines
    plot <- ggplot(data = nutr_col, aes_string(x = "nutr.t")) + geom_histogram(aes(y = ..density..))+ 
      geom_density(lwd = 2, colour = "purple") +
      geom_vline(xintercept = nutr.sum) +
      annotate(geom = "label", x = nutr.sum, y = 0, 
               label = nutr.sum %>% round(3))
    ggplotly(plot)
     }
    )
  
  output$nutrplot_sum <- renderTable(options = list(dom = 't'),{
    # Construct vector of nutrient data
    nutr_col <- vol_nutr %>% select(one_of(input$nutr))
    
    names(nutr_col) <- "nutr"
    
    
    # Select transformation function 
    if(input$nutrlog == TRUE) {
      trans_nutr <- log
    } else{
      trans_nutr <- identity
    }
    #})
    
    # Summarize Nutrient Distribution 
    nutr.sum <- summary(nutr_col$nutr) %>% trans_nutr %>% tidy
    data.frame(nutr.sum)
  })
  
  
  # : Subject FFQ Nutrient Intake Table ####
  
  subj_table <- reactive({
    vol_nutr %>% select(vid,input$nutr)
    
  })
  
  output$subj_table <- DT::renderDataTable({
    datatable(subj_table(),selection = list(mode = 'single',selected = c(1),target = 'row'),
              options = list(lengthChange = FALSE, pageLength = 5)) %>% formatRound(input$nutr, input$nutr_digits)
  },server = FALSE)
  
  subj_table_proxy <- dataTableProxy("subj_table")
  

  observe({
    selected_page <- ceiling(input$subj_table_row_last_clicked/5)
    selectPage(subj_table_proxy, selected_page)
    selectRows(subj_table_proxy, input$subj_table_row_last_clicked)
  })
  
  
  
  observeEvent({input$nutr;input$nutr_digits},{
    
    selected_page <- ceiling(input$subj_table_rows_selected/5)
    
    selectPage(subj_table_proxy, selected_page)
    selectRows(subj_table_proxy, input$subj_table_rows_selected)
  })
  
  
  
  # . .  Data Table ####
  subj_select <- reactive({
    if(is.null(input$subj_table_rows_selected)) return(NULL)
    
    # Get single subject's frequency data
    #x <- vol_dietary[vol_dietary$vid %in% (vol_nutr[input$subj_table_rows_selected,'vid']),]
    single_subj_data <- vol_dietary %>% filter(vid %in% vol_nutr[input$subj_table_rows_selected,'vid'])
    #single_subj_data <- vol_dietary %>% filter(vid == vol_nutr %>% slice(4) %$% vid)
   
    # Get the per serving nutrient data
    y=data.frame(varname=names(single_subj_data), freq = t(single_subj_data) %>% as.vector,stringsAsFactors = FALSE) %>% 
      right_join(vol_recipe %>% select(VNAME, fcmb_name, one_of(input$nutr)), by=c("varname"="VNAME"))
    
    # Get the associated amount column
    y <- y %>% left_join(metadata %>% select(QName, AmountCol, ResponseCategory, SectionLetter,Subsection), by = c("varname" = "QName"))
  
    # Get the associated amount data
    z <- vol_dietary %>% filter(vid == single_subj_data$vid) %>% select(one_of(y$AmountCol))
    z <- data.frame(AmountCol = names(z), amt = t(z) %>% as.vector,stringsAsFactors = FALSE)
    y <- y %>% left_join(z)
    
    #y %>% mutate(nfreq = ifelse(!is.na(ResponseCategory) & !is.na(freq), get_freq(ResponseCategory, freq), NA ))
    y %<>% mutate(nfreq = get_freq_vec(ResponseCategory, freq, impute = 0),
                  namt  = get_freq_vec(16, amt,  impute = 1) %>% as.numeric,
                  srv   = nfreq*namt
                 ) 
    y$intake = y$srv * y[[input$nutr]]
    
    return(y)
  })
  
 
  # . . Output Table ####
  output$subj_select <- DT::renderDataTable({
    if(!is.null(input$subj_table_rows_selected)){
      nutr_per_srv <- paste0(input$nutr, "/srv")
      nutr_per_day <- paste0(input$nutr, "/day")
      r <- subj_select() %>% 
        select(-Subsection, -ResponseCategory) %>% 
        select(Section = SectionLetter,`Freq. Col.` = varname, `Amount Col.`=AmountCol, Description = fcmb_name,
               Freq = freq,Amount = amt,`Freq Value` = nfreq, `Amount Value`=namt,`srv/day` = srv,  input$nutr, intake) %>% 
        mutate(across(c("Freq Value", "Amount Value", "srv/day", "intake", input$nutr), ~round(.x, input$nutr_digits)))
      names(r)[10]  <- nutr_per_srv
      names(r)[11] <- nutr_per_day
      
      datatable(r, selection = list(mode = "single", selected = c(1),target = 'row'), 
                options = list(lengthChange = FALSE, pageLength = 5)) 
    
      
      }
  },server = FALSE) 

  
  subj_select_proxy <- dataTableProxy("subj_select")
  
  
  
  observeEvent(input$nutr_button,{
    
   selected <- subj_select()[input$subj_select_rows_selected,]
   selected_fcmb(selected$varname)
   updateTabsetPanel(session, "vol_tabs", "FFQ Questions")
    
  })
  
  observeEvent({input$nutr;input$nutr_digits},{
      selected_page <- ceiling(input$subj_select_rows_selected/5)
    
      selectPage(subj_select_proxy, selected_page)
      selectRows(subj_select_proxy, input$subj_select_rows_selected)
  })
  
  range_vals <- reactive({
    if(!is.null(input$subj_select_rows_selected) && !is.na(input$subj_select_rows_selected)){
    res_cat_dict <- res_cat_dict %>% filter(res_cat_dict[,1] == subj_select()[input$subj_select_rows_selected,"ResponseCategory"]) %>% select(starts_with("T")) 
    res_cat_dict <- res_cat_dict[,colSums(is.na(res_cat_dict))<nrow(res_cat_dict)] %>% select(-Trend,-Type) 
    names(res_cat_dict) <- gsub("T", '', names(res_cat_dict))
    res_cat_dict %>% mutate(`Response Category` = subj_select()[input$subj_select_rows_selected,"ResponseCategory"] %>% as.character) %>%
      select(`Response Category`, everything())
    
  }
  })
  
  
  # . . Response Table Output ####i
  output$range_vals <- renderTable({
    
    range_vals()
    
  })
  
    
  # . . Tree Map ####
  output$treemap <- renderPlotly({
    if(is.null(subj_select())) return(NULL)
    plot_subsections <- subj_select()$Subsection %>% unique %>% as.vector
    plot_varnames <- subj_select()[["varname"]]
    plot_ly(subj_select(),
            type = "treemap",
            labels = c(plot_subsections, plot_varnames),
            values = c(rep.int(0, times = 29), subj_select()$intake),
            parents = c(rep.int("", times = 29), subj_select()$Subsection),
            ids = c(plot_subsections, plot_varnames)
            
    )  
  })
  
  
    # : Write-in Nutrient Sources ####

  # . . Data Table ####
  writein_subj_select <- reactive({
    if(is.null(input$subj_table_rows_selected)) return(NULL)
    writein_freq %<>% filter(vid %in% vol_nutr[input$subj_table_rows_selected, "vid"]) %>% select(-vid)
    writein_amt %<>% filter(vid %in% vol_nutr[input$subj_table_rows_selected, "vid"]) %>% select(-vid)
    write_amt <- data.frame(amt = t(writein_amt) %>% as.vector, amt_column = names(writein_amt)) %>% filter(!is.na(amt))
    write_freq <- data.frame(freq = t(writein_freq) %>% as.vector, freq_column = names(writein_freq)) %>% filter(!is.na(freq))
    writein_data %<>% filter(vid %in% vol_nutr[input$subj_table_rows_selected,'vid']) %>% 
      select(vid, SectionLetter, writeincol, recp_id, recp_name, ResponseCategory, one_of(input$nutr), FrequencyCol, AmountCol,Subsection) %>%
      mutate(across(input$nutr, ~round(.x, digits = 2) ) ) %>% 
      left_join(write_amt, by =c("AmountCol" = "amt_column")) %>% 
      left_join(write_freq, by = c( "FrequencyCol" = "freq_column")) %>%
       mutate(nfreq = get_freq_vec(ResponseCategory, freq, impute = 0),
                  namt  = get_freq_vec(16, amt,  impute = 1) %>% as.numeric,
                  srv   = nfreq*namt) 
    writein_data$intake = writein_data$srv * writein_data[[input$nutr]] 
    
    return(writein_data)
    })
  
  # . . Output Table ####
  output$writein_subj_select <- DT::renderDataTable({
    if(!is.null(input$subj_table_rows_selected)){
      nutr_per_srv <- paste0(input$nutr, "/srv")
      nutr_per_day <- paste0(input$nutr, "/day")
      r <- writein_subj_select() %>% 
        select(-vid, -ResponseCategory) %>% select(Section = SectionLetter,`Write-in Col.`=writeincol,`Freq. Col.` = FrequencyCol, `Amount Col.` = AmountCol,
               `Recipe ID`=recp_id,`Recipe Name`=recp_name, Freq = freq,Amount = amt,`Freq Value` = nfreq,
                `Amount Value`=namt,`srv/day` = srv, input$nutr, intake) %>%
        mutate(across(c("Freq Value", "Amount Value", "srv/day", "intake", input$nutr), ~round(.x, input$nutr_digits)))
      names(r)[12]  <- nutr_per_srv
      names(r)[13] <- nutr_per_day
      
      datatable(r, selection = list(mode = "single", selected = c(1),target = 'row'), 
                   options = list(lengthChange = FALSE, pageLength = 5))
      
    }
  })
  
  writein_subj_select_proxy <- dataTableProxy("writein_subj_select")
  
  
  
  observeEvent(input$write_in_to_recp,{
     
    selected <- writein_subj_select()[input$writein_subj_select_rows_selected,]
    selected_writein(selected$writeincol)
    updateTabsetPanel(session,"vol_tabs","Write-In Questions")
  })
  
  observe({
    pagenum <- ceiling(input$writein_table_rows_selected/10)
    selectPage(writein_table_proxy, pagenum)
  })
  
  
  observe({
    selected_page <- ceiling(input$writein_subj_select_rows_selected/5)
    selected_page <- ceiling(input$writein_subj_select_row_last_clicked/5)
    
    selectPage(writein_subj_select_proxy, selected_page)
    selectRows(writein_subj_select_proxy, input$writein_subj_select_rows_selected)
    selectRows(writein_subj_select_proxy, input$writein_subj_select_row_last_clicked)
  })
  
  
  
  # . . Response Table Output ####
  
  output$writein_range_vals <- renderTable({
    if(!is.null(input$writein_subj_select_rows_selected) && !is.na(input$writein_subj_select_rows_selected)){
      res_cat_dict <- res_cat_dict %>% filter(res_cat_dict[,1] == writein_subj_select()[input$writein_subj_select_rows_selected,"ResponseCategory"]) %>% select(starts_with("T")) 
      res_cat_dict <- res_cat_dict[,colSums(is.na(res_cat_dict))<nrow(res_cat_dict)] %>% select(-Type, -Trend) 
      names(res_cat_dict) <- gsub("T", '', names(res_cat_dict))
      res_cat_dict %>% mutate(`Response Category` = writein_subj_select()[input$writein_subj_select_rows_selected,"ResponseCategory"] %>% as.character) %>%
        select(`Response Category`, everything())
    }
  })

  subj_sum_B_intake <- reactive({
    if(is.null(subj_select())) return(NA)
     subj_select() %>% filter(SectionLetter == "B") %>% select(intake) %>% sum(na.rm = TRUE)
   })  
   
  subj_sum_F_intake <- reactive({
    if(is.null(subj_select())) return(NA)
    subj_select() %>% filter(SectionLetter == "F") %>% select(intake) %>% sum(na.rm = TRUE)
  })  
  write_subj_sum_B_intake <- reactive({
    if(is.null(writein_subj_select())) return(NA)
    writein_subj_select() %>% filter(SectionLetter == "B") %>% select(intake) %>% sum(na.rm = TRUE)
  })  
  write_subj_sum_F_intake <- reactive({
    if(is.null(writein_subj_select())) return(NA)
    writein_subj_select() %>% filter(SectionLetter == "F") %>% select(intake) %>% sum(na.rm = TRUE)
  })  
  dataset_total_write <- reactive({
    writein_sum_select %>% filter(vid %in% vol_nutr[input$subj_table_rows_selected,'vid']) %>% extract2(input$nutr) %>% round(input$nutr_digits)
  })
  dataset_total_hard_coded <- reactive({
    vol_nutr[input$subj_table_rows_selected, input$nutr] %>% as.numeric 

  })
  
  #c("Section B","Section F","Supplements","Cereals","Meat Analogues","Soy Milks","Calculated Total","Dataset Total","Discrepancy"
  subj_sum <- reactive({
    tibble::tribble(
      ~Section, ~`Hard Coded`, ~`Write-ins`, ~`Total`,
      "Section B", subj_sum_B_intake(), write_subj_sum_B_intake(), subj_sum_B_intake() + write_subj_sum_B_intake(),
      "Section F", subj_sum_F_intake(), write_subj_sum_F_intake(), subj_sum_F_intake() + write_subj_sum_F_intake(),
      "Supplements", NA, NA, NA,
      "Calculated Total",  subj_sum_B_intake() + subj_sum_F_intake(), write_subj_sum_B_intake() + write_subj_sum_F_intake(), subj_sum_B_intake() + subj_sum_F_intake() + write_subj_sum_B_intake() + write_subj_sum_F_intake(),
      "Dataset Total",  dataset_total_hard_coded(), dataset_total_write(), dataset_total_write() + dataset_total_hard_coded(),
      "Discrepancy", -dataset_total_hard_coded() + (subj_sum_B_intake() + subj_sum_F_intake()), -dataset_total_write() + (write_subj_sum_B_intake() - write_subj_sum_F_intake()) ,  (-dataset_total_hard_coded() + (subj_sum_B_intake()+ subj_sum_F_intake()))+(-dataset_total_write() + (write_subj_sum_B_intake() - write_subj_sum_F_intake()))
      
      
    )
    
  })
  
  
  
  output$subj_sum <- DT::renderDataTable({
    datatable(selection = 'none',options = list(dom = 't'),subj_sum()) %>% formatRound("Hard Coded",input$nutr_digits) %>% 
      formatRound(columns = setdiff(names(subj_sum()), "Section"),digits = input$nutr_digits)
    
  
     
     
   })
  
  
  # Handle button for jumping to diet tab for selected VNAME 
   # observeEvent(input$nutr_button,{
   #     if(!is.null(input$subj_select_rows_selected)){
   #       updateTabsetPanel(session,'vol_tabs','FFQ Questions')
   #       selectRows(fcmb_table_proxy, which(fcmb_select$VNAME %>% tolower %in% subj_select()[input$subj_select_rows_selected,"FFQ Question"]))
   #       }
   # })
  
  # Food Groups Tab ####
    
  # : Non-overlapping Table ####
    
  #check to see which tab is selected, and then use non_table() or ovr_table() (using an if/else)
   
  non_table <- reactive({
    fg_data$nonoverlapping[[input$nonoverlap_groups]]
  })
  
  
  # : Overlapping Table ####
  ovr_table <- reactive({
    fg_data$overlapping[[input$ovr_groups]]
    
  })
  
  # : Table Output####
  output$food_groups_table <- DT::renderDataTable(selection = 'none', options = list(dom = "fp",pageLength = 5), {
    if(input$non_or_ovr_tabs == "non_tab"){
      non_table() %>% select(`Food ID`=foodid,fname)
    } else {
      ovr_table() %>% select(`Food ID`=foodid,fname)
    } 
  })  
       
  # : Matching VNAMEs Table ####
    # Check to see which tab is selected, and then use recp_select/non_table() or vol_ffqrecpie/ovr_table() (using an if/else)
  food_groups_recipe_ids <- reactive({
    if(input$non_or_ovr_tabs == "non_tab"){
      recp_select %>% filter(recp_select$recp_id %in% (non_table()$foodid)) %>% 
        select(`FFQ Question` = VNAME,`Recipe ID`=recp_id,`Recipe Name`=recp_name) %>% 
        unique %>% 
        relocate(`FFQ Question`,.before = `Recipe ID`)
    } else {
      vol_ffqrecipe %>% filter(vol_ffqrecipe$recp_id %in% (ovr_table()$foodid)) %>% 
        select(`FFQ Question` = VNAME,`Recipe ID`=recp_id,`Recipe Name`=recp_name,`Ingredient ID`=ingr_id,`Ingredient Name`=ingr_name) %>% 
        unique %>% 
        relocate(`FFQ Question`,.before = `Recipe ID`)
    }
  })
  
  # : Matching VNAMEs Table Output####
  output$hard_coded_food_groups<-DT::renderDataTable(selection = "single",options = list(dom = "fp", pageLength = 5),{
      food_groups_recipe_ids()
    })
  
  # : View VNAME Recipe Button ####
  
  observeEvent(input$hard_coded_to_recipe,{
    
      selected <- food_groups_recipe_ids()[input$hard_coded_food_groups_rows_selected,]

       selected_hard_coded$VNAME      <- selected$`FFQ Question`
       selected_hard_coded$recp_id    <- selected$`Recipe ID`
       selected_hard_coded$recp_name  <- selected$`Recipe Name`
       
       updateTabsetPanel(session,"vol_tabs","Recipes")
       updateTabsetPanel(session,"recipe_type","FFQ Recipes")
     })
  
  observe({
    page <- ceiling(input$hard_coded_recp_rows_selected/10)
    selectPage(hard_coded_recp_proxy,page)  
  })
  
  
  # : Matching Write-in Table ####
  # Check to see which tab is selected, and then use recp_select/non_table() or vol_ffqrecpie/ovr_table() (using an if/else)
  non_food_groups_recipe_ids_write <- reactive({
    all_recp_select %>% filter(all_recp_select$recp_id %in% (non_table()$foodid)) %>% 
      select(`Recipe ID`=recp_id,`Recipe Name`=recp_name) %>% 
      unique
  })
  
  ovr_food_groups_recipe_ids_write <- reactive({
    vol_allrecipes %>% filter(vol_allrecipes$recp_id %in% (ovr_table()$foodid)) %>% 
      select(`Recipe ID`=recp_id,`Recipe Name`=recp_name,`Ingredient ID`=ingr_id,`Ingredient Name`=ingr_name) %>% 
      unique
    
  })
  
  # : Matching Write-in Table Output####
  output$writein_food_groups<-DT::renderDataTable(selection = list(mode = "single", selected = c(1),target = 'row'), 
                                                  options = list(lengthChange = FALSE, pageLength = 5),{
    
    if(input$non_or_ovr_tabs == "non_tab"){
      non_food_groups_recipe_ids_write()
    }
    else{
      ovr_food_groups_recipe_ids_write()
    }
    
  })
  
  # : View Write-in Recipe Button ####
  observeEvent(input$write_to_questions,{
    if(input$non_or_ovr_tabs == "non_tab"){
      selected <- non_food_groups_recipe_ids_write()[input$writein_food_groups_rows_selected,]
    }
    else{
      selected <- ovr_food_groups_recipe_ids_write()[input$writein_food_groups_rows_selected,]
    }   
    
    selected_writein_recp$`Recipe ID`    <- selected$`Recipe ID`
    selected_writein_recp$`Recipe Name`  <- selected$`Recipe Name`
    
    updateTabsetPanel(session,"vol_tabs","Recipes")
    updateTabsetPanel(session,"recipe_type","Write-in Recipes")
    
  }) 
  
  observe({
    pagenum <- ceiling(input$writein_recp_rows_selected/10)
    selectPage(writein_recp_proxy, pagenum)
  })
  
 
  # : Matching Recalls Table  ####
  
  food_groups_recipe_ids_recall <- reactive({
    
    if(input$non_or_ovr_tabs == "non_tab"){
      data <- vol_dr_ingr %>% filter(vol_dr_ingr$recp_id %in% (non_table()$foodid)) 
    } else {
      data <- vol_dr_ingr %>% filter(vol_dr_ingr$ingr_id %in% (ovr_table()$foodid))
    }
    
    data %>%
      select(recp_id,recp_name,vid,recall_set, dayofweek, mealname) %>% 
      unique 
  })
  
  
  # : Matching Recalls Table Output ####
  output$recall_food_groups <- DT::renderDataTable(selection = list(mode = "single", selected = c(1),target = 'row'), 
                                                   options = list(lengthChange = FALSE, pageLength = 5),{
      food_groups_recipe_ids_recall() %>%
        mutate(dayofweek = case_when(dayofweek == "S" ~ "Saturday",
                                     dayofweek == "M" ~ "Sunday",
                                     dayofweek == "W" ~ "Weekday",
                                     TRUE ~ "(Missing)"),
               mealname = case_when(mealname == 1 ~ "Breakfast",
                                    mealname == 3 ~ "Lunch",
                                    mealname == 4 ~ "Snack",
                                    mealname == 5 ~ "Supper",
                                    mealname == 6 ~ "Other",
                                    mealname == 7 ~ "School Lunch",
                                    TRUE ~ "(Blank)")) %>%
        plyr::rename(c("recp_id" = "Recipe ID", 
                 "recp_name" = "Recipe Name", 
                 "recall_set" = "Synthetic Week",
                 "dayofweek" = "Day of the Week",
                 "mealname" = "Meal Name"))
    
  })
  
  # : View Recall Recipe Button ####
  observeEvent(input$to_recalls,{
    food_group_row <- food_groups_recipe_ids_recall()[input$recall_food_groups_rows_selected,]
    
    selected$vid       <- food_group_row$vid
    selected$dayofweek <- food_group_row$dayofweek
    selected$week      <- food_group_row$recall_set
    selected$recp_id   <- food_group_row$recp_id
    selected$mealname  <- food_group_row$mealname
    print(selected$vid)
    print(selected$week)
    print(selected$dayofweek)
    print(selected$mealname)
    print(selected$recp_id)
    print("Yahhh")
   
    updateTabsetPanel(session,"vol_tabs","Recalls") 
  })
  
   
# : Recalls Tab ####
   
   # rc_subject_list
   # rc_recall_list
   # rc_recipe_list
   # rc_ingr_list
      
   # . . Recall Selection Object ####
   selected <- reactiveValues(
     vid = NA_integer_,
     dayofweek = NA_character_,
     week = NA_real_,
     recp_id = NA_character_,
     mealname = NA_character_
   )
      
   # . . Recall Subject Table ####
   rc_subject_list <- reactive({
     
       rc_sum_table %>%
         mutate(recall_set = as.character(recall_set)) %>%
         rbind(rc_avg_table) %>% 
         select(vid, `Synthetic Week`=recall_set, one_of(input$recall_nutr)) %>% 
         pivot_wider(id_cols = "vid", names_from = "Synthetic Week", names_prefix = "Week ", values_from=input$recall_nutr) %>%
         mutate(across(c("Week 1", "Week 2", "Week Avg"), ~round(., input$recall_digits)))
       
   })
   
   # . . Recall Subject Table Output ####
   output$recall_subj <- renderDataTable({
     
     if(selected$vid %>% is.na){selection_row <- 1; selected$vid <- rc_subject_list()[[1,"vid"]]}
     
     else{selection_row <- which(rc_subject_list()$vid == selected$vid)}
     cat("Output recall subject table:", nrow(rc_subject_list()), "rows.\n")
     cat("recall subj row last clicked", input$recall_subj_row_last_clicked,"\n")
     datatable(rc_subject_list(), 
               selection = list(mode = "single", selected = selection_row,target = 'row'),
               options = list(lengthChange = FALSE, pageLength = 5))
     
   })
   
   recall_subj_proxy <- dataTableProxy("recall_subj")
   
   
   observeEvent(input$recall_subj_rows_selected,{
     selected_page <- ceiling(input$recall_subj_rows_selected/5)
     selectPage(recall_subj_proxy, selected_page)
     selectRows(recall_subj_proxy, input$recall_subj_rows_selected)
   })
   
   # Tag 1
   observeEvent(input$recall_subj_row_last_clicked, {
      if(length(input$recall_subj_rows_selected) != 0) { 
        cat("recall subj if","\n")
        selection_row <- input$recall_subj_rows_selected
        cat("recall subj: selection_row", selection_row, "\n")
        selected$vid <- rc_subject_list()[[selection_row,"vid"]] 
        cat("recall subj: selected$vid", selected$vid, "\n")
        selected$week      <- NA
        selected$dayofweek <- NA
        selected$mealname  <- NA
        selected$recp_id   <- NA
      }
   })

   
   # . . Recall Instance Table ####
   recall_inst <- reactive({
     
     rc_recall_table %>% 
       filter(vid %in% selected$vid) %>% 
       select(-vid) %>% 
       select(dayofweek, recall_set, one_of(input$recall_nutr)) 
   }) 
   
   
   # . . Recall Instance Table Output ####
  output$recall_inst <- renderDataTable( {

    if((selected$dayofweek     %>% is.na) | 
       (selected$week          %>% is.na)) {
      selection_row <- 1
      selected$dayofweek <- recall_inst()[[1,"dayofweek"]]
      selected$week      <- recall_inst()[[1,"recall_set"]]
    }  else{
      selection_row <- which(recall_inst()$dayofweek   == selected$dayofweek & 
                             recall_inst()$recall_set  == selected$week)
    }
    cat("recall inst selected$dayofweek", selected$dayofweek, "\n")
    cat("recall inst selected$week", selected$week, "\n")
    cat("recall inst selection row", selection_row, "\n")
    
    data <- recall_inst() %>%
       mutate(dayofweek = case_when(dayofweek == "S" ~ "Saturday",
                                    dayofweek == "M" ~ "Sunday",
                                    dayofweek == "W" ~ "Weekday",
                                    TRUE ~ "(Missing)")) %>%
         select(`Day of the Week`= dayofweek,
                `Synthetic Week`= recall_set, 
                one_of(input$recall_nutr))
    
    cat("Output Recall Instances:",nrow(data),"\n")

   datatable(data,
             selection = list(mode = "single", selected = selection_row, target = 'row'),
             options = list(dom = "t", pageLength = 6, stateSave = TRUE)) %>% 
     formatRound(columns = input$recall_nutr,digits = input$recall_digits)
  })
   
  recall_inst_proxy <- dataTableProxy("recall_inst")
  
   # Tag 2
   observeEvent(input$recall_inst_row_last_clicked,{
      if(length(input$recall_inst_rows_selected) != 0) {
        cat("recall inst if","\n")
        selection_row <- input$recall_inst_rows_selected
        selected$week <- recall_inst()[[selection_row,"recall_set"]] 
        cat("recall inst: selection_row", selection_row, "\n")
        selected$dayofweek <- recall_inst()[[selection_row,"dayofweek"]] 
        cat("recall inst: selected$week", selected$week, "\n") 
        cat("recall inst: selected$dayofweek", selected$dayofweek, "\n") 
        # If a different recall instance was clicked, we reset the selected meal/recp
        selected$mealname <- NA
        selected$recp_id <- NA
      }
   })
 
  # . . Instance Recipes Table #### 

  recall_recp <- reactive({
    
    cat("Updating recall_recp(): ")
    
    data <- rc_recipe_table %>% 
      filter(       vid %in% selected$vid) %>% 
      filter(recall_set %in% selected$week) %>% 
      filter( dayofweek %in% selected$dayofweek) %>% 
      select(-vid) %>% 
      select(mealname,recp_id,recp_name, one_of(input$recall_nutr)) %>%
      unique
    
    cat("selected", nrow(data), "rows.\n")
    
    return(data)
    
  }) 
  
  # . . Instance Recipes Table Output #### 
   output$recall_recp <- renderDataTable({
       if((selected$mealname         %>% is.na) | 
          (selected$recp_id          %>% is.na)) {
           selection_row     <- 1;
           selected$mealname <- recall_recp()[[1,"mealname"]];
           selected$recp_id  <- recall_recp()[[1,"recp_id"]]
           cat("recall recp selection if","\n")
              }
       else{
         cat("recall recp selection else","\n")
         selection_row <- which(recall_recp()$mealname == selected$mealname & 
                                   recall_recp()$recp_id  == selected$recp_id    )
         cat("recall recp selection row:",selection_row,"\n")
       } 
            
        data <- recall_recp() %>%
           mutate(mealname = case_when(mealname == 1 ~ "Breakfast",
                                       mealname == 3 ~ "Lunch",
                                       mealname == 4 ~ "Snack",
                                       mealname == 5 ~ "Supper",
                                       mealname == 6 ~ "Other",
                                       mealname == 7 ~ "School Lunch",
                                       TRUE ~ "(Blank)"))  %>%
        plyr::rename(c("mealname" = "Meal Name",
                 "recp_id" = "Recipe ID",
                 "recp_name" = "Recipe Name"))
        
        if(nrow(recall_recp()) == 0) selection_row <- NULL
        
        cat("Output recall instance recipes:", nrow(data), "rows.\n")
      
       datatable(data,
                 selection = list(mode = "single", selected = selection_row,target = 'row'),
                 options = list(lengthChange = FALSE, pageLength = 5, stateSave = TRUE)) %>% 
        formatRound(columns = input$recall_nutr,digits = input$recall_digits) 
       
   },server = FALSE)
   
    
  recall_recp_proxy <- dataTableProxy("recall_recp")
  
   observeEvent(input$recall_recp_rows_selected, {
      cat("recall recp: input$recall_recp_rows_selected '", input$recall_recp_rows_selected , "'\n")
      if(length(input$recall_recp_rows_selected) != 0) { 
        selection_row <- input$recall_recp_rows_selected
        cat("recall recp: selection_row", selection_row, "\n")
        selected$mealname <- recall_recp()[[selection_row,"mealname"]] 
        selected$recp_id <- recall_recp()[[selection_row,"recp_id"]] 
        cat("recall recp: selected$mealname", selected$mealname, "\n") 
        cat("recall recp: selected$recp_id", selected$recp_id, "\n") 
      }
   })
  
  
  observeEvent({input$recall_nutr; input$recall_digits;input$recall_recp_rows_selected;input$recall_inst_rows_selected;input$recall_subj_rows_selected}, {
    if(length(input$recall_recp_rows_selected) != 0) {
      cat("select page/row recall recp: input$recall_recp_rows_selected '", input$recall_recp_rows_selected , "'\n")
      selected_page <- ceiling(input$recall_recp_rows_selected/5)
      cat("recall recp selected page:",selected_page,"\n")
      selectPage(recall_recp_proxy, selected_page)
      #selectRows(recall_recp_proxy, input$recall_recp_rows_selected)
    }
  })
  

  
  # . . Ingredient Table Output ####
   output$recall_ingr <- renderDataTable({
     
     data <- vol_dr_ingr %>% 
       filter(       vid %in% selected$vid) %>% 
       filter(recall_set %in% selected$week) %>% 
       filter( dayofweek %in% selected$dayofweek) %>% 
       filter(  mealname %in% selected$mealname) %>%
       filter(   recp_id %in% selected$recp_id) %>% 
       select(`Ingredient ID`=ingr_id,`Ingredient Name`= ingr_name, one_of(input$recall_nutr))
     
     cat("Output Ingredient Table:", nrow(data), "rows.\n")
     datatable(data, selection = "none",options = list(dom = "f",pageLength = 100)) %>% 
       formatRound(columns = input$recall_nutr,digits = input$recall_digits)
   })
  
  recall_ingr_proxy <- dataTableProxy("recall_ingr")
   
}


shinyApp(ui, server)