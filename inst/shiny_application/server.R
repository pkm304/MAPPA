
options(shiny.maxRequestSize=10000*1024^2)



ggdata = data.frame() ## how to make this local? to be able to load data by user input

server <- function(input, output, session) {


  #####################################
  ######### Workspace##################
  #####################################

  PPM <- reactiveValues()

  ##create a new PPM
  observeEvent(input$new_PPM,{
    PPM$object <- func_create_PPM(PPM.name = input$PPM_name)
    #new(Class = "PPM", input$PPM_name)
  })


  shinyFiles::shinyFileChoose(input,'file_PPM', session=session,roots= c("root"= "~/", "cwd" = getwd()), defaultRoot = 'root')

  observeEvent(input$file_PPM, {
    #inFile <- parseFilePaths(roots=c(wd='.'), input$file_PPM)
    inFile <- shinyFiles::parseFilePaths(roots=c("root"= "~/","cwd" = getwd()), input$file_PPM)
    print(inFile$datapath)

    if(length(inFile$datapath) != 0){
      temp.path <- sub(inFile$name,replacement = "",x = inFile$datapath)
      setwd(temp.path)
      PPM$object <- readRDS(as.character(inFile$datapath))
    }
  })



  ##save PPM object as .rds
  # output$save_PPM <- downloadHandler(
  #     filename = function() {
  #       input$file_PPM_name
  #     },
  #     content = function(file) {
  #       saveRDS(PPM$object, file)
  #     }
  #
  #   )

  observe({
    volumes <- c("root"= "~/", "cwd" = getwd())
    shinyFiles::shinyFileSave(input, "save_PPM", roots = volumes)
    #print(input$save_PPM)
    file.info <- shinyFiles::parseSavePath(volumes, input$save_PPM)
    #print(file.info)

    if(nrow(file.info) > 0){
      saveRDS(PPM$object, file = as.character(file.info$datapath))
      temp.path <- sub(file.info$name,replacement = "",x = file.info$datapath)
      setwd(temp.path)
    }
    })





  output$current_PPM_name <- renderUI({
    PPM.name <-NULL
    if(!is.null(PPM$object)){
      PPM.name <- get.PPM.name(PPM$object)
    }
    isolate({
      list(
        selectInput("show_PPM",label = h4("Current PPM"),choices = PPM.name, size = 1, multiple = TRUE, selectize = FALSE)
      )
    })
  })



  output$list_prm_ranges <- renderUI({
    prm.ranges.names <- NULL
    if(!is.null(PPM$object)){
      prm.ranges.names <- get.prm.ranges.name(PPM$object)
    }
    isolate({
      list(
           selectInput("show_prm_ranges_list",label = h4("Parameter ranges"),choices =  prm.ranges.names,size = 3,multiple = TRUE, selectize = FALSE))
    })
  })

  output$list_init_prm_combs <- renderUI({
    init.prm.combs.names <- NULL
    if(!is.null(PPM$object) && !is.null(get.init.prm.combs.name(PPM$object))){
      init.prm.combs.names <- get.init.prm.combs.name(PPM$object)
      init.prm.combs.names <- unlist(init.prm.combs.names )
      names(init.prm.combs.names) <- NULL
    }
    isolate({
      list(
        selectInput("show_prm_combinations_list",label = h4("Initial parameter sets"),choices =   init.prm.combs.names, size = 3,multiple = TRUE, selectize = FALSE)
        )
    })

  })


  output$list_addit_prm_combs <- renderUI({
    addit.prm.combs.names <- NULL
    if(!is.null(PPM$object) && !is.null(get.addit.prm.combs.name(PPM$object))){
      addit.prm.combs.names <- get.addit.prm.combs.name(PPM$object)
      addit.prm.combs.names <- unlist(addit.prm.combs.names)
      names(addit.prm.combs.names) <- NULL
    }
    isolate({
      list(
        selectInput("show_addit_prm_combs_list",label = h4("Additional parameter sets"),choices =   addit.prm.combs.names, size = 3,multiple = TRUE, selectize = FALSE)
      )
    })
  })

  output$list_phenotypes <- renderUI({
    phenotypes.names <- NULL
    if(!is.null(PPM$object)){

      phenotypes.names <-  get.phenotypes.name(PPM$object)
      phenotypes.names <- append("None", phenotypes.names)
    }

    # if(!is.null(PPM$object)){# && input$with_ml.models_ml == T){
    #   phenotypes.names <- phenotypes.names[phenotypes.names%in% get.phenotypes.with.ml.models(PPM$object)]
    # }
    isolate({
      list(
        selectInput("show_phenotypes",label = h4("Phenotypes"),choices =   phenotypes.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })


  })


  output$list_ML.models <- renderUI({
    ml.models.names <- NULL
    if(!is.null(PPM$object)){
      if(length(PPM$object@ml.models) != 0 ){
      if(!is.null(get.ml.models.name(PPM$object))){
        ml.models.names <- get.ml.models.name(PPM$object)
        ml.models.names <- unlist(ml.models.names)
        names(ml.models.names) <- NULL
      }}
    }else{
      ml.models.names <- NULL
    }
    isolate({
      list(
        selectInput("show_ml.model_ml",label = h4("ML models"),choices =   ml.models.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })

  })





  #####################################
  ######### Initial sampling ##########
  #####################################

  prm.ranges <- reactiveValues()
  prm.grids <- reactiveValues()
  prm.combinations <- reactiveValues()



  ###side panel###
  output$list_init_prm_combs_ui <- renderUI({
    init.prm.combs.names <- NULL
    if(!is.null(PPM$object)){
      init.prm.combs.names <- get.init.prm.combs.name(PPM$object)
      init.prm.combs.names <- unlist(init.prm.combs.names)
      names(init.prm.combs.names) <- NULL
    }
    isolate({
      list(
        selectInput("show_prm_combinations_list_tab",label = h4("Initial parameter combinations"),choices =   init.prm.combs.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })
  })


  observeEvent(input$init_prm_combs_save2ps,{

    if(!is.null(prm.combinations$DF) && !is.null(input$init_prm_combs_name)){
      prm.ranges$DF <- rhandsontable::hot_to_r(input$parameter_ranges)

      if(input$sampling_meth == "unif_grid"){
        temp.prm.combs.z <- fun.scale.conv(sample_meth = input$sampling_meth, prm.ranges = prm.ranges$DF, prm.grids = prm.grids$DF, prm.combs = prm.combinations$DF[,-1], z.to.org = FALSE )
        temp.prm.combs.z <- cbind( prm.combinations$DF[,1], temp.prm.combs.z)
        names(temp.prm.combs.z) <-c("pkey", prm.ranges$names)
        add.init.prm.combs(PPM$object) <- list(prm.ranges.name = prm.combinations$prm.ranges.name ,
                                                      name = input$init_prm_combs_name,
                                                      method = input$sampling_meth,
                                                      log.scale = prm.ranges$DF[,c("names","log.scale")],
                                                      num.grids = prm.ranges$DF[,c("names","number of grids")],
                                                      prm.grids = prm.grids$DF,
                                                      prm.combs = prm.combinations$DF,
                                                      prm.combs.z =  temp.prm.combs.z,
                                                      rd_seed = prm.combinations$rd_seed )
      }else{

        if(!is.null( prm.combinations$prm.combs.z)){
          add.init.prm.combs(PPM$object) <- list(prm.ranges.name = prm.combinations$prm.ranges.name ,
                                                        name = input$init_prm_combs_name,
                                                        method = input$sampling_meth,
                                                        log.scale = prm.ranges$DF[,c("names","log.scale")],
                                                        #num.grids = prm.ranges$DF[,c("names","number of grids")],
                                                        #prm.grids = prm.grids$DF,
                                                        raw.smpl = prm.combinations$raw.smpl,
                                                        prm.combs = prm.combinations$DF,
                                                        prm.combs.z =  prm.combinations$prm.combs.z,
                                                        rd_seed = prm.combinations$rd_seed )
        }else{
          temp.init.prmset <- get.init.prm.combs(object = PPM$object,name = input$show_prm_combinations_list_tab, prm.ranges.name = input$prm_ranges_select)
          temp.prm.combs.z <- fun.scale.conv(sample_meth = input$sampling_meth, prm.ranges = prm.ranges$DF, raw.smpl = temp.init.prmset$raw.smpl   ,prm.combs = prm.combinations$DF[,-1], z.to.org = FALSE )
          temp.prm.combs.z <- cbind( prm.combinations$DF[,1], temp.prm.combs.z)
          add.init.prm.combs(PPM$object) <- list(prm.ranges.name = prm.combinations$prm.ranges.name ,
                                                        name = input$init_prm_combs_name,
                                                        method = input$sampling_meth,
                                                        log.scale = prm.ranges$DF[,c("names","log.scale")],
                                                        #num.grids = prm.ranges$DF[,c("names","number of grids")],
                                                        #prm.grids = prm.grids$DF,
                                                        #raw.smpl = prm.combinations$raw.smpl,
                                                        prm.combs = prm.combinations$DF,
                                                        prm.combs.z = temp.prm.combs.z,
                                                        rd_seed = prm.combinations$rd_seed )

        }

      }



      }
  })

  ###main panel###

  ##action for sampling option and  reset button.
  observe({
    if(input$reset[[1]] == 0){
      isolate(prm.ranges$DF <-data.frame(names = paste0("k",(1:10)), min = rep(0,10), max = rep(0,10),log.scale = rep(FALSE,10),stringsAsFactors = F ))
    }else if(input$reset[[1]] != 0){
      isolate({
        nrow.temp <- nrow(prm.ranges$DF)
        print("a")
        prm.ranges$DF <-data.frame(names = paste0("k",(1:nrow.temp)), min = rep(0,nrow.temp), max = rep(0,nrow.temp),log.scale = rep(FALSE,nrow.temp), stringsAsFactors = F)
        #prm.ranges$DF <-data.frame(names = paste0("k",(1:10)), min = rep(0,10), max = rep(0,10),log.scale = rep(FALSE,10),stringsAsFactors = F )
        #print(prm.ranges$DF)
      })
    }


    isolate(prm.grids$DF <- NULL)
    isolate(prm.combinations$DF <- NULL)


  })

  observe({
    if(!is.null(input$file_prm_ranges)){
      temp.df <- cbind(read.csv(input$file_prm_ranges$datapath, header = T, stringsAsFactors = F), log.scale = FALSE)
      colnames(temp.df)[1:3] <- c('names', 'min', 'max')
      #prm.ranges$DF <- cbind(read.table(input$file_prm_ranges$datapath, header = F, stringsAsFactors = F), log.scale = FALSE)
      prm.ranges$DF <- temp.df
      }
  })



  #adding or removing rows
  observeEvent(input$prm_num,{
    prm.ranges$DF = rhandsontable::hot_to_r(input$parameter_ranges)
    current.nrow = nrow(prm.ranges$DF)
    if(input$prm_num - current.nrow  > 0 ){

      if( input$sampling_meth == "unif_grid"){
        add.prm.ranges = data.frame(names = paste0("k",(current.nrow+1):input$prm_num), min = rep(0,input$prm_num - current.nrow), max = rep(0,input$prm_num - current.nrow),log.scale = rep(FALSE,input$prm_num - current.nrow), stringsAsFactors = F)
        add.prm.ranges = cbind(add.prm.ranges,  "number of grids" = 10)
      } else if ( input$sampling_meth != "unif_grid"){
        add.prm.ranges = data.frame(names = paste0("k",(current.nrow+1):input$prm_num), min = rep(0,input$prm_num - current.nrow), max = rep(0,input$prm_num - current.nrow),log.scale = rep(FALSE,input$prm_num - current.nrow), stringsAsFactors = F)
      }
      prm.ranges$DF = rbind(prm.ranges$DF, add.prm.ranges)
    }else if(input$prm_num - current.nrow  < 0){
      prm.ranges$DF = prm.ranges$DF[-((input$prm_num+1):current.nrow),]
    }
    row.names(prm.ranges$DF) <- 1:nrow(prm.ranges$DF)
  })

  output$file_ui <- renderUI({
    input$reset
    fileInput("file_prm_ranges", h5("Input from a file (.txt, .csv)."))
  })

  output$prm_ranges_select_ui <- renderUI({
    prm.ranges.names <- NULL
    if(!is.null(PPM$object)){
      prm.ranges.names <- get.prm.ranges.name(PPM$object)
    }

    isolate({
      list(
        selectInput("prm_ranges_select",label = h5("Parameter ranges"),choices =  prm.ranges.names,selected = NULL, size = 3,multiple = FALSE, selectize = FALSE))
    })
  })

  ##update prm.ranges table in accordance with the selection
  observeEvent(input$load,{
    if(!is.null(input$prm_ranges_select)){
      temp.DF <- get.prm.ranges(PPM$object,input$prm_ranges_select)
      if(nrow(temp.DF) == nrow(prm.ranges$DF)){
        prm.ranges$DF[, c("names", "min","max")] <-  temp.DF
      }else{
        prm.ranges$DF = data.frame( temp.DF, log.scale = FALSE, stringsAsFactors = F)
        if( input$sampling_meth == "unif_grid"){
          prm.ranges$DF = cbind(prm.ranges$DF,  "number of grids" = 10)
        } else {}
      }

    }


  })


  output$prm_num_ui <- renderUI({
    numericInput("prm_num", label = h5("Number of parameters"), value = nrow(prm.ranges$DF),min = 1, max = 1000)
  })

  output$prm_grids_gen_ui <- renderUI({
    if(input$sampling_meth == "unif_grid"){
      actionButton("gen_prm_grids", "Generate parameter grids")
    }
  })



  #display of parameter ranges
  output$parameter_ranges <- rhandsontable::renderRHandsontable({
    input$reset
    df <- prm.ranges$DF
    # print(DF)
    ncol.temp = ncol(df)
    isolate({
      if(input$sampling_meth == "unif_grid"){
        #rhandsontable::rhandsontable(DF, digit = 10, contextMenu = FALSE )  %>% hot_col(col = "log.scale", type = "checkbox") %>% hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))  %>% hot_col(col = ncol.temp,renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits
        MAPPA::func_rhandsontable_prm_ranges(df, option = input$sampling_meth, ncol.temp)
      } else {
        #rhandsontable::rhandsontable(DF, digit = 10, contextMenu = FALSE )  %>% hot_col(col = "log.scale", type = "checkbox") %>% hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))
        MAPPA::func_rhandsontable_prm_ranges(df, option = input$sampling_meth)
      }
    })
  })

  #setting for log scale
  observeEvent(input$sel_desel_all, {
    prm.ranges$DF <- rhandsontable::hot_to_r(input$parameter_ranges)
    if(all.equal(prm.ranges$DF[["log.scale"]], rep(TRUE, nrow(prm.ranges$DF))) != TRUE){
      prm.ranges$DF["log.scale"] = TRUE
    }else{
      prm.ranges$DF["log.scale"] = FALSE
    }
  })

  #setting for uniform grid
  observe({
    #input$parameter_ranges
    # input$sampling_meth
    # print(input$sampling_meth)
    if(is.null(prm.ranges$DF$"number of grids") && input$sampling_meth == "unif_grid"){
      isolate(prm.ranges$DF <- cbind(prm.ranges$DF, "number of grids" = 10))
      #isolate(print(prm.ranges$DF))
    } else if ( input$sampling_meth != "unif_grid"){
      isolate(prm.ranges$DF <- prm.ranges$DF[,c("names", "min", "max", "log.scale")])
    }
  })


  ##Saving again without changing file path does not work.
  observe({
    volumes = c("root"= "~/", "cwd" = getwd())
    shinyFiles::shinyFileSave(input, "save", roots = volumes)
    #print(input$save)
    file.info = shinyFiles::parseSavePath(volumes, input$save)
    #print(file.info)
    if(nrow(file.info) > 0){
      if(file.info$type == "text"){
        isolate({prm.ranges$DF <- rhandsontable::hot_to_r(input$parameter_ranges)
        write.table(prm.ranges$DF[,c("names", "min", "max")],file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
      }else if (file.info$type == "csv"){
        isolate({prm.ranges$DF <- rhandsontable::hot_to_r(input$parameter_ranges)
        write.csv(prm.ranges$DF[,c("names", "min", "max")],file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
      }
    }
  })

  ##add to PPM##
  observeEvent(input$prm_ranges_save2ps,{
    if(!is.null(input$prm_ranges_name)){
      prm.ranges$DF <-  rhandsontable::hot_to_r(input$parameter_ranges)
      add.prm.ranges(PPM$object) <- list(prm.ranges =prm.ranges$DF[,c("names", "min", "max")], name = input$prm_ranges_name )
      #add.prm.ranges(PPM$object, prm.ranges$DF[,c("names", "min", "max")], input$prm_ranges_name)
    }
  })



  output$prm_grids_ui <- renderUI({
    input$gen_prm_grids
    if(input$sampling_meth == "unif_grid" && !is.null(prm.grids$DF) && !input$check_prm_grids_mod){
      list(DT::dataTableOutput("prm_grids"),
           #shinySaveButton("save_prm_grids", "Save parameter grids", "Save parameter combinations as ...", filetype=list(text="txt", csv = "csv")))
           downloadButton("save_prm_grids", "Save parameter grids"))
    }else if(input$sampling_meth == "unif_grid" && !is.null(prm.grids$DF) && input$check_prm_grids_mod){
      list(rhandsontable::rHandsontableOutput("prm_grids_mod"),
           downloadButton("save_prm_grids", "Save parameter grids"))
    }
  })






  #generate parameter grids, later implement filter for nonzero value
  observeEvent(input$gen_prm_grids,{
    prm.ranges$DF <- rhandsontable::hot_to_r(input$parameter_ranges)
    prm.grids$DF <-  func_gen_prm_grids(prm.ranges = prm.ranges$DF)
  })

  output$prm_grids <- DT::renderDataTable({
    prm.grids$DF
  })

  output$prm_grids_mod <- rhandsontable::renderRHandsontable({
    DF <- prm.grids$DF
    # print(DF)
    ncol.temp = ncol(DF)
    isolate({
      #rhandsontable::rhandsontable(DF, digit = 10, contextMenu = FALSE )   %>% hot_col(col = c(2:ncol.temp),renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits
      func_rhandsontable_prm_grids(DF, ncol.temp)
    })
  })

  observeEvent(input$check_prm_grids_mod,{
    if(!input$check_prm_grids_mod && !is.null(input$prm_grids_mod)){
      prm.grids$DF <- rhandsontable::hot_to_r(input$prm_grids_mod)
    }
  })


  output$save_prm_grids <- downloadHandler(
    filename = function() {
      paste("Untitled", ".txt", sep = "")
    },
    content = function(file) {
      if(input$check_prm_grids_mod){
        prm.grids$DF <- rhandsontable::hot_to_r(input$prm_grids_mod)
      }
      write.table(prm.grids$DF,file  ,quote = FALSE, col.names = TRUE, row.names = FALSE)
    }
  )




  # observe({
  #   volumes = c("roots"= "~/")
  #   shinyFileSave(input, "save_prm_grids", roots = volumes)
  #   print(input$save_prm_grids)
  #   file.info = parseSavePath(volumes, input$save_prm_grids)
  #   print(file.info)
  #   if(nrow(file.info) > 0){
  #     if(file.info$type == "text"){
  #       isolate({
  #       write.table(prm.grids$DF,file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
  #     }else if (file.info$type == "csv"){
  #       isolate({
  #       write.csv(prm.grids$DF,file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
  #     }
  #   }
  # })
  #


  output$prm_comb_ui <- renderUI({
    input$gen_prm_combs
    if(!is.null(prm.combinations$DF)){
      list(hr(),
           h4("Parameter combinations"),
           DT::dataTableOutput("prm_combs"))
    }
  })

  output$prm_combs <-DT::renderDataTable({
    prm.combinations$DF
  })




  #generate parameter combinations

  observeEvent(input$gen_prm_combs,{


    ##1.generate parameter keys (convention: date + "_" + 8 digit LETTER barcodes)
    let.to.num = c(0:25)
    names(let.to.num) = LETTERS
    p.index = as.numeric(let.to.num[strsplit(input$pkey_digits,"")[[1]]])

    temp.date = input$pkey_date
    temp.date = format(temp.date, "%m%d%Y")
    temp.date = as.character(temp.date)
    temp.pkey = gen_prm_keys(input$prm_comb_num,  temp.date, p.index, nchar(input$pkey_digits))

    prm.combinations$rd_seed = temp.pkey$rd_seeds
    ##2.generate parameter combinations
    isolate(prm.ranges$DF <-  rhandsontable::hot_to_r(input$parameter_ranges))

    if(input$check_prm_grids_mod){
      prm.grids$DF <-  rhandsontable::hot_to_r(input$prm_grids_mod)
    }
    temp.DF <- func_gen_prm_combs(prm.ranges$DF, input$prm_comb_num, input$sampling_meth, prm.grids$DF, continue = input$continue, count = get.prm.combs.count(PPM$object, smpl_method = input$sampling_meth, prm.ranges.name = input$prm_ranges_select) )
    add.prm.combs.count(PPM$object) <- list( smpl_method = input$sampling_meth, addit.count = input$prm_comb_num, prm.ranges.name = input$prm_ranges_select)
    if(input$sampling_meth == "unif_grid"){
      prm.combinations$DF <- data.frame(pkey = temp.pkey$pkey, temp.DF$prm.combs, stringsAsFactors = F)
      prm.combinations$method = input$sampling_meth
      names(prm.combinations$DF)[-1] = prm.ranges$DF$names
      prm.combinations$prm.ranges.name <- input$prm_ranges_select
    }else{
      prm.combinations$DF <- data.frame(pkey = temp.pkey$pkey, temp.DF$prm.combs, stringsAsFactors = F)
      if(!is.null(temp.DF$raw.smpl)){
        prm.combinations$raw.smpl <- data.frame(pkey = temp.pkey$pkey,temp.DF$raw.smpl, stringsAsFactors = F)
        names(prm.combinations$raw.smpl)[-1] = prm.ranges$DF$names
      }

      if(!is.null(temp.DF$prm.combs.z)){
        prm.combinations$prm.combs.z <- data.frame(pkey = temp.pkey$pkey,temp.DF$prm.combs.z, stringsAsFactors = F)
        names(prm.combinations$prm.combs.z)[-1] = prm.ranges$DF$names
      }
      prm.combinations$method = input$sampling_meth
      names(prm.combinations$DF)[-1] = prm.ranges$DF$names
      prm.combinations$prm.ranges.name <- input$prm_ranges_select
    }


  })

  ##save parameter combinations
  # output$save_prm_combs_ui <- renderUI({
  # shiny save file doesn't work
  #   if(!is.null( prm.combinations$DF)){
  #     shinySaveButton("save_prm_combs", "Save parameter combinations", "Save parameter combinations as ...", filetype=list(text="txt", csv = "csv"))
  #   }
  # })
  #
  shinyFiles::shinyFileSave(input, "save_prm_combs", roots = c("root"= "~/", "cwd" = getwd()))
  observe({
    # if(!is.null(prm.combinations$DF)){
    #
    # }
   # print(input$save_prm_combs)
    file.info = shinyFiles::parseSavePath(c("roots"= "~/"), input$save_prm_combs)
    #print(file.info)
    if(nrow(file.info) > 0){
      if(file.info$type == "text"){
        isolate({
          write.table( prm.combinations$DF,file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
      }else if (file.info$type == "csv"){
        isolate({
          write.csv(prm.combinations$DF,file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
      }
    }
  })




  output$test <- renderPrint({
    input$file_prm_ranges
    input$reset
    input$pkey_date[1]
  })


  #####################################
  ######## Additional sampling ########
  #####################################

  prm.ranges.add <- reactiveValues()
  prm.grids.add <- reactiveValues()
  #prm.combinations.add <- reactiveValues()
  init.prm.combs <- reactiveValues()
  prm.combs.selected <- reactiveValues()
  addit.prm.combs <-reactiveValues()


  ###side panel###
  output$list_addit_prm_combs_ui <- renderUI({
    addit.prm.combs.names <- NULL
    if(!is.null(PPM$object)){
      addit.prm.combs.names <- get.addit.prm.combs.name(PPM$object)
      addit.prm.combs.names <- unlist(addit.prm.combs.names)
      names(addit.prm.combs.names) <- NULL
    }
    isolate({
      list(
        selectInput("show_addit_prm_combs_list_tab",label = h4("Additional parameter sets"),choices =   addit.prm.combs.names, size = 3,multiple = TRUE, selectize = FALSE)
      )
    })
  })


  observeEvent(input$addit_prm_combs_save2ps,{
    if(!is.null(addit.prm.combs$DF)){
      if(input$add_sampling_meth == "unif_grid"){
        temp.addit.prms.combs.z <- fun.scale.conv(sample_meth = addit.prm.combs$method,
                                                  prm.ranges = prm.ranges.add$DF,
                                                  prm.grids = prm.grids.add$DF,
                                                  prm.combs = addit.prm.combs$DF[,prm.ranges.add$DF$names],
                                                  z.to.org = FALSE
        )
        temp.addit.prms.combs.z <- cbind(addit.prm.combs$DF[,1], temp.addit.prms.combs.z)
        names(temp.addit.prms.combs.z) <- c("pkey", prm.ranges.add$names)
        add.additional.prm.combs(PPM$object) <- list(prm.ranges.name = input$load_prm_ranges,
                                                            init.prm.combs.name = input$load_init_prm_combs,
                                                            name = input$addit_prm_combs_name,
                                                            method = addit.prm.combs$method,
                                                            log.scale = prm.ranges.add$DF$log.scale,
                                                            frac.range =  prm.ranges.add$DF$frac.range,
                                                            num.grids =  prm.ranges.add$DF$"number of grids",
                                                            prm.combs.selected = prm.combs.selected$DF,
                                                            prm.combs = addit.prm.combs$DF,
                                                            prm.combs.z = temp.addit.prms.combs.z,
                                                            rd_seed = addit.prm.combs$rd_seed)

      }else{
        init.prm.combs$DF$raw.smpl
        temp.addit.prms.combs.z <- fun.scale.conv(sample_meth = addit.prm.combs$method,
                                                  prm.ranges = prm.ranges.add$DF,
                                                  raw.smpl = init.prm.combs$DF$raw.smpl,
                                                  prm.combs = addit.prm.combs$DF[,prm.ranges.add$DF$names],
                                                  z.to.org = FALSE
                                                  )
        temp.addit.prms.combs.z <- cbind(addit.prm.combs$DF[,1], temp.addit.prms.combs.z)
        names(temp.addit.prms.combs.z) <- c("pkey", prm.ranges.add$names)
        add.additional.prm.combs(PPM$object) <- list(prm.ranges.name = input$load_prm_ranges,
                                                            init.prm.combs.name = input$load_init_prm_combs,
                                                            name = input$addit_prm_combs_name,
                                                            method = addit.prm.combs$method,
                                                            log.scale = prm.ranges.add$DF$log.scale,
                                                            frac.range =  prm.ranges.add$DF$frac.range,
                                                            num.grids =  prm.ranges.add$DF$"number of grids",
                                                            prm.combs.selected = prm.combs.selected$DF,
                                                            prm.combs = addit.prm.combs$DF,
                                                            prm.combs.z = temp.addit.prms.combs.z,
                                                            rd_seed = addit.prm.combs$rd_seed)
      }

    }
  })



  ###main panel###
  ##Load existing parameter space (ranges and initial combinations)
  output$laad_prm_ranges_ui <- renderUI({
    prm.ranges.names <- NULL
    if(!is.null(PPM$object)){
      prm.ranges.names <- get.prm.ranges.name(PPM$object)
    }
    isolate({
      list(
        selectInput("load_prm_ranges",label = h5("Parameter ranges"),choices =  prm.ranges.names,selected = NULL, size = 3,multiple = FALSE, selectize = FALSE))
    })
  })

  output$load_init_prm_combs_ui <- renderUI({
    init.prm.combs.names <- NULL
    if(!is.null(PPM$object)){
      if(!is.null(input$load_prm_ranges)){
        init.prm.combs.names <- get.init.prm.combs.name(PPM$object)
        init.prm.combs.names <- init.prm.combs.names[[input$load_prm_ranges]]
      }
    }
      list(
        selectInput("load_init_prm_combs",label = h5("Initial parameter combinations"),choices = init.prm.combs.names ,selected = NULL, size = 3,multiple = FALSE, selectize = FALSE))

  })

  observeEvent(input$load_prm_ranges,{
    print(input$load_prm_ranges)
    prm.ranges.add$DF <- get.prm.ranges(PPM$object,input$load_prm_ranges)
  })

  observeEvent(input$load_init_prm_combs,{
    print(input$load_init_prm_combs)
    if(!is.null(input$load_init_prm_combs)){
      init.prm.combs$DF <- get.init.prm.combs(PPM$object,input$load_init_prm_combs,input$load_prm_ranges)
      prm.ranges.add$DF <- data.frame(prm.ranges.add$DF[,c("names", "min", "max")], log.scale =  init.prm.combs$DF$log.scale$log.scale)
        if(init.prm.combs$DF$method == "unif_grid"){
          prm.ranges.add$DF$frac.range = 1
          prm.ranges.add$DF$"number of grids" = init.prm.combs$DF$num.grids$"number of grids"
        }else{
          prm.ranges.add$DF$frac.range = 0.1
        }
      prm.grids.add$DF <-init.prm.combs$DF$prm.grids
    }else{
      prm.ranges.add$DF$log.scale <- NULL
      prm.ranges.add$DF$frac.range <- NULL
      prm.ranges.add$DF$"number of grids" <- NULL
      prm.grids.add$DF <- NULL
    }
  })

  # #setting for uniform grid
  # observe({
  #   if(!is.null(init.prm.combs$DF)){
  #     prm.ranges.add$DF$log.scale <- init.prm.combs$DF$log.scale
  #     prm.ranges.add$DF$"number of grids" <- init.prm.combs$DF$"number of grids"
  #     if(init.prm.combs$DF$method == "unif_grid" ){
  #       prm.ranges.add$DF$frac.range <- 1
  #     }else{
  #       prm.ranges.add$DF$frac.range <- 0.1
  #     }
  #   }else{
  #     prm.ranges.add$DF$log.scale <- NULL
  #     prm.ranges.add$DF$"number of grids" <- NULL
  #     prm.ranges.add$DF$frac.range <- NULL
  #   }
  # )}

  # if(is.null(prm.ranges.add$DF$"number of grids") && input$add_sampling_meth == "unif_grid"){
  #   isolate({prm.ranges.add$DF <- cbind(prm.ranges.add$DF, "number of grids" = 10)
  #   prm.ranges.add$DF$frac.range <- 1})
  #   #isolate(print(prm.ranges.add$DF))
  # } else if ( input$add_sampling_meth != "unif_grid"){
  #   isolate({prm.ranges.add$DF <- prm.ranges.add$DF[,c("names", "min", "max", "log.scale", 'frac.range')]
  #   prm.ranges.add$DF$frac.range = 0.1})
  # }
# })


  ##input selected parameter combinations
  output$file_prm_selected_ui <- renderUI({
    input$add_reset
    fileInput("file_prm_selected", h5("Input from a file (.txt, .csv)."))
  })


  observe({
    if(!is.null(input$file_prm_selected)){
      isolate(prm.combs.selected$DF <- read.table(input$file_prm_selected$datapath, header = T, stringsAsFactors = F)
      )
    }
    print(prm.combs.selected$DF)
  })



  output$prm_space_selected_tab_ui <- renderUI({
    if(!is.null(input$load_prm_ranges) && is.null(input$load_init_prm_combs) && is.null(prm.combs.selected$DF)){
      shinydashboard::tabBox(id = "prm_space_selected", selected = NULL, width = 12,
                tabPanel(title = "Parameter ranges", value = "tab_prm_ranges_select",
                         uiOutput("add_prm_num_ui"),
                         fluidRow(
                           column(6,rhandsontable::rHandsontableOutput("parameter_ranges_add")
                           ),
                           column(3,h5("Log scale"),
                                  actionButton("add_sel_desel_all", label = "Select/Deselect All"))

                           ),
                         br(),
                         fluidRow(
                           column(3, shinyFiles::shinySaveButton("add_save", "Save parameter ranges", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv")))
                           #column(3, uiOutput("add_prm_grid_gen"))
                           )
                         )
             )
    }else if(!is.null(input$load_prm_ranges) && !is.null(input$load_init_prm_combs) && is.null(prm.combs.selected$DF)){
      if(init.prm.combs$DF[["method"]] == "unif_grid") {
        shinydashboard::tabBox(id = "prm_space_selected", selected = NULL, width = 12,
               tabPanel(title = "Parameter ranges", value = "tab_prm_ranges_select",
                        uiOutput("add_prm_num_ui"),
                        fluidRow(
                          column(6,rhandsontable::rHandsontableOutput("parameter_ranges_add")
                          ),
                          column(3,h5("Log scale"),
                                 actionButton("add_sel_desel_all", label = "Select/Deselect All"))

                        ),
                        br(),
                        fluidRow(
                          column(3, shinyFiles::shinySaveButton("add_save", "Save parameter ranges", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv")))
                          #column(3, uiOutput("add_prm_grid_gen"))
                        )
               ),
               tabPanel(title = "Parameter grids", value = "tab_prm_grids_select",
                        uiOutput("file_prm_grids_ui")
               ),
               tabPanel(title = "Initial parameter set", value = "tab_init_prm_combs_select",
                       uiOutput("init_prm_combs_add_ui")
               )
        )
      }else{
        shinydashboard::tabBox(id = "prm_space_selected", selected = NULL, width = 12,
               tabPanel(title = "Parameter ranges", value = "tab_prm_ranges_select",
                        uiOutput("add_prm_num_ui"),
                        fluidRow(
                          column(6,rhandsontable::rHandsontableOutput("parameter_ranges_add")
                          ),
                          column(3,h5("Log scale"),
                                 actionButton("add_sel_desel_all", label = "Select/Deselect All"))

                        ),
                        br(),
                        fluidRow(
                          column(3, shinyFiles::shinySaveButton("add_save", "Save parameter ranges", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv")))
                          #column(3, uiOutput("add_prm_grid_gen"))
                        )
               ),
               tabPanel(title = "Initial parameter set", value = "tab_init_prm_combs_select",
                        uiOutput("init_prm_combs_add_ui")
               )
        )
      }

    }else if(!is.null(input$load_prm_ranges) && !is.null(input$load_init_prm_combs) && !is.null(prm.combs.selected$DF)){
      if(init.prm.combs$DF[["method"]] == "unif_grid") {
        shinydashboard::tabBox(id = "prm_space_selected", selected = NULL, width = 12,
               tabPanel(title = "Parameter ranges", value = "tab_prm_ranges_select",
                        uiOutput("add_prm_num_ui"),
                        fluidRow(
                          column(6,rhandsontable::rHandsontableOutput("parameter_ranges_add")
                          ),
                          column(3,h5("Log scale"),
                                 actionButton("add_sel_desel_all", label = "Select/Deselect All"))

                        ),
                        br(),
                        fluidRow(
                          column(3, shinyFiles::shinySaveButton("add_save", "Save parameter ranges", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv")))
                          #column(3, uiOutput("add_prm_grid_gen"))
                        )
               ),
               tabPanel(title = "Parameter grids", value = "tab_prm_grids_select",
                        uiOutput("file_prm_grids_ui")
               ),
               tabPanel(title = "Initial parameter set", value = "tab_init_prm_combs_select",
                        uiOutput("init_prm_combs_add_ui")
               ),
               tabPanel(title = "selected parameter combinations", value = "tab_selected_prm_combs",
                        uiOutput("prm_comb_sel_ui")
               )
        )
      }else{
        shinydashboard::tabBox(id = "prm_space_selected", selected = NULL, width = 12,
               tabPanel(title = "Parameter ranges", value = "tab_prm_ranges_select",
                        uiOutput("add_prm_num_ui"),
                        fluidRow(
                          column(6,rhandsontable::rHandsontableOutput("parameter_ranges_add")
                          ),
                          column(3,h5("Log scale"),
                                 actionButton("add_sel_desel_all", label = "Select/Deselect All"))

                        ),
                        br(),
                        fluidRow(
                          column(3, shinyFiles::shinySaveButton("add_save", "Save parameter ranges", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv")))
                          #column(3, uiOutput("add_prm_grid_gen"))
                        )
               ),
               tabPanel(title = "Initial parameter set", value = "tab_init_prm_combs_select",
                        uiOutput("init_prm_combs_add_ui")
               ),
               tabPanel(title = "selected parameter combinations", value = "tab_selected_prm_combs",
                        uiOutput("prm_comb_sel_ui")
               )
        )
      }

    }

  })




  ##action for sampling option and  reset button.
  # observe({
  #   if(input$add_reset[[1]] == 0){
  #     isolate(prm.ranges.add$DF <-data.frame(names = paste0("k",(1:10)), min = rep(0,10), max = rep(0,10),log.scale = rep(FALSE,10), frac.range = rep(0.1,10), stringsAsFactors = F ))
  #   }else if(input$add_reset[[1]] != 0){
  #     isolate({
  #       nrow.temp <- nrow(prm.ranges.add$DF)
  #       # print("a")
  #       prm.ranges.add$DF <-data.frame(names = paste0("k",(1:nrow.temp)), min = rep(0,nrow.temp), max = rep(0,nrow.temp),log.scale = rep(FALSE,nrow.temp), frac.range = rep(0.1,nrow.temp), stringsAsFactors = F)
  #       #prm.ranges.add$DF <-data.frame(names = paste0("k",(1:10)), min = rep(0,10), max = rep(0,10),log.scale = rep(FALSE,10),stringsAsFactors = F )
  #       #print(prm.ranges.add$DF)
  #     })
  #   }
  #
  #   isolate(prm.grids.add$DF <- NULL)
  #   #isolate(prm.combinations.add$DF <- NULL)
  #   isolate(prm.combs.selected$DF <- NULL)
  #   isolate(addit.prm.combs$DF <- NULL)
  #   isolate(init.prm.combs$DF <- NULL)
  #
  # })


  #adding or removing rows
  # observeEvent(input$add_prm_num,{
  #   prm.ranges.add$DF = hot_to_r(input$parameter_ranges_add)
  #   current.nrow = nrow(prm.ranges.add$DF)
  #   if(input$add_prm_num - current.nrow  > 0 ){
  #
  #     if( input$add_sampling_meth == "unif_grid"){
  #       add.prm.ranges.add = data.frame(names = paste0("k",(current.nrow+1):input$add_prm_num), min = rep(0,input$add_prm_num - current.nrow), max = rep(0,input$add_prm_num - current.nrow),log.scale = rep(FALSE,input$add_prm_num - current.nrow), frac.range = rep(0.1,input$add_prm_num - current.nrow), stringsAsFactors = F)
  #       add.prm.ranges.add = cbind(add.prm.ranges.add,  "number of grids" = 10)
  #     } else if ( input$add_sampling_meth != "unif_grid"){
  #       add.prm.ranges.add = data.frame(names = paste0("k",(current.nrow+1):input$add_prm_num), min = rep(0,input$add_prm_num - current.nrow), max = rep(0,input$add_prm_num - current.nrow),log.scale = rep(FALSE,input$add_prm_num - current.nrow), frac.range = rep(0.1,input$add_prm_num - current.nrow), stringsAsFactors = F)
  #     }
  #     prm.ranges.add$DF = rbind(prm.ranges.add$DF, add.prm.ranges.add)
  #   }else if(input$add_prm_num - current.nrow  < 0){
  #     prm.ranges.add$DF = prm.ranges.add$DF[-((input$add_prm_num+1):current.nrow),]
  #   }
  #   row.names(prm.ranges.add$DF) <- 1:nrow(prm.ranges.add$DF)
  # })


  output$add_prm_num_ui <- renderUI({
    numericInput("add_prm_num", label = h5("Number of parameters"), value = nrow(prm.ranges.add$DF),min = 1, max = 1000)
  })


  #display of parameter ranges
  output$parameter_ranges_add <- rhandsontable::renderRHandsontable({
    input$add_reset
    if(!is.null(prm.ranges.add$DF)){
      df <- prm.ranges.add$DF
      ncol.temp = ncol(df)
      isolate({
        if(ncol.temp == length(c("names", "min", "max"))){
          #rhandsontable::rhandsontable(DF, digit = 10, contextMenu = FALSE )   %>% hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))
          func_rhandsontable_prm_ranges_add1(df)
        }else if(!is.null(input$load_init_prm_combs)){ #if(!is.null(init.prm.combs$DF)){
            if(init.prm.combs$DF$method == "unif_grid"){
              #rhandsontable::rhandsontable(DF, digit = 10, contextMenu = FALSE )  %>% hot_col(col = "log.scale", type = "checkbox") %>% hot_col(col = c("min","max","frac.range"),renderer=htmlwidgets::JS("safeHtmlRenderer"))  %>% hot_col(col = ncol.temp,renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits
              func_rhandsontable_prm_ranges_add2(df, ncol.temp)
            } else {
              func_rhandsontable_prm_ranges_add3(df)
              #rhandsontable(DF, digit = 10, contextMenu = FALSE )  %>% hot_col(col = "log.scale", type = "checkbox") %>% hot_col(col = c("min","max","frac.range"),renderer=htmlwidgets::JS("safeHtmlRenderer"))
            }
        }
      })
    }
  })

  #setting for log scale
  observeEvent(input$add_sel_desel_all, {
    prm.ranges.add$DF <- rhandsontable::hot_to_r(input$parameter_ranges_add)
    if(all.equal(prm.ranges.add$DF[["log.scale"]], rep(TRUE, nrow(prm.ranges.add$DF))) != TRUE){
      prm.ranges.add$DF["log.scale"] = TRUE
    }else{
      prm.ranges.add$DF["log.scale"] = FALSE
    }
  })

  # #setting for uniform grid
  # observe({
  #   if(!is.null(init.prm.combs$DF)){
  #     prm.ranges.add$DF$log.scale <- init.prm.combs$DF$log.scale
  #     prm.ranges.add$DF$"number of grids" <- init.prm.combs$DF$"number of grids"
  #     if(init.prm.combs$DF$method == "unif_grid" ){
  #       prm.ranges.add$DF$frac.range <- 1
  #     }else{
  #       prm.ranges.add$DF$frac.range <- 0.1
  #     }
  #   }else{
  #     prm.ranges.add$DF$log.scale <- NULL
  #     prm.ranges.add$DF$"number of grids" <- NULL
  #     prm.ranges.add$DF$frac.range <- NULL
  #   }

    # if(is.null(prm.ranges.add$DF$"number of grids") && input$add_sampling_meth == "unif_grid"){
    #   isolate({prm.ranges.add$DF <- cbind(prm.ranges.add$DF, "number of grids" = 10)
    #   prm.ranges.add$DF$frac.range <- 1})
    #   #isolate(print(prm.ranges.add$DF))
    # } else if ( input$add_sampling_meth != "unif_grid"){
    #   isolate({prm.ranges.add$DF <- prm.ranges.add$DF[,c("names", "min", "max", "log.scale", 'frac.range')]
    #   prm.ranges.add$DF$frac.range = 0.1})
    # }
#  })

  output$file_prm_grids_ui <- renderUI({
    if(init.prm.combs$DF[["method"]] == "unif_grid"){
      input$add_reset
      list(
           DT::dataTableOutput("prm_grids_add")
      )

    }
  })

  output$init_prm_combs_add_ui <- renderUI({
    list(
      h5("Initial parameter set"),
      DT::dataTableOutput("init_prm_combs_add")
    )
  })



  output$prm_grids_add <- DT::renderDataTable({
    if(!is.null(init.prm.combs$DF[["prm.grids"]])){
      init.prm.combs$DF[["prm.grids"]]
    }
  })

  output$init_prm_combs_add <- DT::renderDataTable({
    init.prm.combs$DF[["prm.combs"]]

  })



  ##Saving again without changing file path does not work.
  observe({
    volumes = c("root"= "~/", "cwd" = getwd())
    shinyFiles::shinyFileSave(input, "add_save", roots = volumes)
    #print(input$add_save)
    file.info = shinyFiles::parseSavePath(volumes, input$add_save)
    # print(file.info)
    if(nrow(file.info) > 0){
      if(file.info$type == "text"){
        isolate({prm.ranges.add$DF <- rhandsontable::hot_to_r(input$parameter_ranges_add)
        write.table(prm.ranges.add$DF[,c("names", "min", "max")],file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
      }else if (file.info$type == "csv"){
        isolate({prm.ranges.add$DF <- rhandsontable::hot_to_r(input$parameter_ranges_add)
        write.csv(prm.ranges.add$DF[,c("names", "min", "max")],file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
      }
    }
  })


  ##display selected parameter combinations
  output$prm_comb_sel_ui <- renderUI({
    input$file_prm_selected
    isolate({
      if(!is.null(prm.combs.selected$DF)){
        list(DT::dataTableOutput("prm_combs_selected"))
      }
    })
  })

  output$prm_combs_selected <- DT::renderDataTable({
    prm.combs.selected$DF
  })



  #generate zoom-in parameter combinations

  observeEvent(input$gen_prm_combs_zoomin,{

    if(!is.null(prm.combs.selected$DF)){


      ##2.generate zoom-in parameter combinations
      ##2.1.generate subranges
      temp.DF <- list()
      #addit.prm.combs$DF <- NULL
      prm.ranges.add$DF <-  rhandsontable::hot_to_r(input$parameter_ranges_add)
      if(input$add_sampling_meth != "unif_grid"){
        prm.grids.add$DF = NULL
      }
      for(i in 1:nrow(prm.combs.selected$DF)){
        temp.subranges <- func_gen_prm_subranges(prm.comb = prm.combs.selected$DF[i,2:ncol(prm.combs.selected$DF)],prm.ranges = prm.ranges.add$DF, sampling.meth = input$add_sampling_meth,prm.grids = prm.grids.add$DF )
        if(input$add_sampling_meth == "unif_grid"){
          temp.subgrids  = func_gen_prm_grids(temp.subranges)
        } else{
          temp.subgrids <- NULL
        }
        temp.DF1 <- func_gen_prm_combs(temp.subranges, input$add_prm_comb_num, input$add_sampling_meth, temp.subgrids, count = get.prm.combs.count(object = PPM$object, smpl_method = input$add_sampling_meth))#, prm.ranges.org = prm.ranges.add$DF)
        #temp.DF <- rbind(temp.DF, temp.DF1$prm.combs )
        temp.DF[[i]] <- temp.DF1$prm.combs
        #addit.prm.combs$DF <- rbind(addit.prm.combs$DF, temp.DF1$prm.combs )
        print(i)
      }

      temp.DF <- do.call("rbind", temp.DF)



      ##1.generate parameter keys (convention: date + "_" + 8 digit LETTER barcodes)
      let.to.num = c(0:25)
      names(let.to.num) = LETTERS
      p.index = as.numeric(let.to.num[strsplit(input$add_pkey_digits,"")[[1]]])

      temp.date = input$add_pkey_date
      temp.date = format(temp.date, "%m%d%Y")
      temp.date = as.character(temp.date)
      temp.pkey = gen_prm_keys(nrow(  temp.DF),  temp.date, p.index, nchar(input$add_pkey_digits))
      temp.subranges = NULL
      temp.subgrids = NULL



      addit.prm.combs$DF <- data.frame(pkey = temp.pkey$pkey,  temp.DF,stringsAsFactors = F)
      addit.prm.combs$method <- input$add_sampling_meth
      #01/25/2020 preserve random seed
      addit.prm.combs$rd_seed <- temp.pkey$rd_seeds

    }


  })

  output$prm_comb_zoomin_ui <- renderUI({
    if(!is.null(addit.prm.combs$DF)){
      list(hr(),
           h4("Zoom-in parameter combinations"),
           DT::dataTableOutput("prm_combs_zoomin"))
    }
  })
  output$prm_combs_zoomin <-DT::renderDataTable({
   addit.prm.combs$DF
  })





  ##save parameter combinations
  # output$save_prm_combs_ui <- renderUI({
  # shiny save file doesn't work
  #   if(!is.null( prm.combinations.add$DF)){
  #     shinySaveButton("save_prm_combs", "Save parameter combinations", "Save parameter combinations as ...", filetype=list(text="txt", csv = "csv"))
  #   }
  # })
  #
  shinyFiles::shinyFileSave(input, "save_prm_combs_zoomin", roots = c("roots"= "~/"))
  observe({
    # if(!is.null(prm.combinations.add$DF)){
    #
    # }
    # print(input$save_prm_combs_zoomin)
    file.info = shinyFiles::parseSavePath(c("roots"= "~/"), input$save_prm_combs_zoomin)
    # print(file.info)
    if(nrow(file.info) > 0){
      if(file.info$type == "text"){
        isolate({
          write.table(addit.prm.combs$DF,file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
      }else if (file.info$type == "csv"){
        isolate({
          write.csv(addit.prm.combs$DF,file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
      }
    }
  })


  output$add_test <- renderPrint({
    input$file_prm_ranges
    input$add_reset
    input$add_pkey_date[1]
  })
  #####################################
  ######## ML model training  #########
  #####################################


  ###data loading###
  phenotype.loaded.ml <- reactiveValues()
  prm.sets.selected.ml <- reactiveValues()
  phenotype.values.selected.ml <- reactiveValues()
  ml.models.new <- reactiveValues()
  ml.model.trained <- reactiveValues()
  phenotype.filter <- reactiveValues()


  output$list_phenotypes_ml_tab_ui <- renderUI({
    phenotypes.names <- NULL
    if(!is.null(PPM$object)){

      phenotypes.names <-  get.phenotypes.name(PPM$object)
      phenotypes.names <- append("None", phenotypes.names)
    }

    if(!is.null(PPM$object) && input$with_ml.models_ml == T){
      phenotypes.names <- phenotypes.names[phenotypes.names%in% get.phenotypes.with.ml.models(PPM$object)]
    }
    isolate({
      list(
        selectInput("load_phenotype_ml",label = h4("Select a phenotype"),choices =   phenotypes.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })

  })

  observe({
    if(!is.null(input$load_phenotype_ml)){
      if(input$load_phenotype_ml != "None"){
        phenotype.loaded.ml$list <- get.phenotypes(PPM$object,input$load_phenotype_ml)
      }else{
        phenotype.loaded.ml$list <- NULL
      }
    }else{
      phenotype.loaded.ml$list <- NULL
    }
  })


  output$list_prm_sets_ml_tab_ui <- renderUI({
    prm.sets.names <- NULL
    if(!is.null(input$load_phenotype_ml)&& input$load_phenotype_ml != "None"){
      prm.sets.names <- names(phenotype.loaded.ml$list)
    }else if(!is.null(input$load_phenotype_ml) && input$load_phenotype_ml == "None"){
      prm.sets.names <- append(unlist(get.init.prm.combs.name(PPM$object)),
                               unlist(get.addit.prm.combs.name(PPM$object)))
      names(prm.sets.names) <- NULL
    }
    if(!is.null(PPM$object) && input$with_tsne_ml == T){#show only ones with tsne
      prm.sets.names <- prm.sets.names[prm.sets.names%in% get.tsne.coord.name(PPM$object)]
    }
    if(!is.null(prm.sets.names)){
      prm.sets.names <- prm.sets.names[order(prm.sets.names)]
    }
    isolate({
      list(
        selectInput("load_parameter_sets_ml",label = h4("Select parameter sets"),choices =   prm.sets.names, size = 3,multiple = TRUE, selectize = FALSE)
      )
    })


  })
  output$list_ml.models_ml_tab_ui<- renderUI({
    ml.models.names <- NULL
    if(!is.null(PPM$object)){
      if(length(PPM$object@ml.models) != 0){
      ml.models.names <- get.ml.models.name(PPM$object)
      ml.models.names <- unlist(ml.models.names)
      names(ml.models.names) <- NULL}
    }else{ml.models.names <- NULL}
    isolate({
      list(
        selectInput("load_ml.model_ml",label = h4("ML models in PPM"),choices =   ml.models.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })

  })



  ###ML model specification
  output$list_parameters_ml_tab_ui <- renderUI({
    parameters <- NULL
    if(!is.null(PPM$object)& !is.null(input$load_parameter_sets_ml)){

      if(length(get.prm.ranges.name(object = PPM$object))==1){
        temp.ranges.names <- get.prm.ranges.name(object = PPM$object)
      }else{
        temp.init.prm.combs.name <- get.init.prm.combs.name(PPM$object)
        for(i in 1:length(temp.init.prm.combs.name)){
          if(any(temp.init.prm.combs.name[[i]] %in% input$load_parameter_sets_ml)){
            temp.ranges.names <- get.prm.ranges.name(PPM$object)[i] # Mar12 2020
          }
        }
        #temp.ranges.names <- get.prm.ranges.name(PPM$object)[get.init.prm.combs.name(PPM$object) %in% input$load_parameter_sets_ml] #Nov 6 2019
      }

      # temp.ranges.names <- c(temp.ranges.names,
      #                        get.prm.ranges.name(PPM$object)[
      #                          unlist(lapply(get.addit.prm.combs.name(PPM$object), function(list){
      #                            if(length(list %in% input$load_parameter_sets_ml) == 0){
      #                              FALSE
      #                            }else{
      #                              list %in% input$load_parameter_sets_ml
      #                            }
      #                          }))
      #                          ]
      #)
      temp.ranges.names <- unique(temp.ranges.names)
      #temp.ranges.names<- append(temp.ranges.names, temp.ranges.names)
      parameters <- as.character(unique(t(apply(t(temp.ranges.names),2, function(object, name ){ get.prm.ranges(object,name)[,"names"]}, object =  PPM$object))))
      #parameters <- get.prm.ranges(object = PPM$object, name = names(unlist(get.init.prm.combs.name(PPM$object))))[,"names"]
      temp.prms <- get.custom.scale.prms(PPM$object)
      temp.prms <- unlist( temp.prms )
      names(temp.prms) <- NULL
      parameters <- append(parameters, temp.prms)
    }



    isolate({
      list(
        selectInput("select_prms_ml",label = h4("Select predictors for ML model"),choices =   parameters, size = 10,multiple = TRUE, selectize = FALSE)
      )
    })
  })


  output$bias_correct_ui <- renderUI({
    print(input$ml_model_mode)
    if(input$ml_model_mode == "reg"){
      radioButtons("bias_corr", h4("Bias correction"),choices = c("Yes", "No") )
    }else if(input$ml_model_mode == "class"){
      radioButtons("balanced", h4("Balanced training"), choices = c("Yes", "No"))
    }
  })


  output$filter_ml_ui <- renderUI({
    if(!is.null(phenotype.filter$condition)){
      list(
        h4("Applied filter"),
        verbatimTextOutput("filter_ml"),
        actionButton("remove_filter_ml", h5("Remove filter"))
      )
    }else{
      return()
    }
  })


  output$class_def_ml_ui <- renderUI({
    if(!is.null(phenotype.values.selected.ml$DF.class)){
      list(
        h4("Defined classes"),
        tableOutput("class_ml"),
        actionButton("remove_class_def_ml", h5("Remove class definition"))
      )
    }else{
      return()
    }
  })

  output$filter_ml <- renderText({
    phenotype.filter$condition
  })

  output$class_ml <- renderTable({
    phenotype.values.selected.ml$DF.class.info
  })



  observeEvent(input$remove_filter_ml,{
    phenotype.filter$condition <- NULL
    phenotype.filter$condition.exc <- NULL
    phenotype.filter$idx.filt <- NULL

    removeUI(
      selector = "div:has(> #check_scatter_filter)"
    )

    #reset
    phenotype.values.selected.ml$DF <-data.frame(stringsAsFactors = F)
    for(temp.name in input$load_parameter_sets_ml){
      phenotype.values.selected.ml$DF <- rbind(phenotype.values.selected.ml$DF,  phenotype.loaded.ml$list[[temp.name]])
    }
    phenotype.values.selected.ml$DF <- phenotype.values.selected.ml$DF[order(phenotype.values.selected.ml$DF$pkey),]
  })

  observeEvent(input$remove_class_def_ml,{
    phenotype.values.selected.ml$DF.class <- NULL
    phenotype.values.selected.ml$DF.class.info <- NULL
  })




  observe({
    if(!is.null(PPM$object) & !is.null(input$load_parameter_sets_ml) &  !is.null(phenotype.loaded.ml$list)){

      isolate({
      ##load selected parameter sets
      temp.prm.ranges.names <- get.prm.ranges.name(object = PPM$object)
      temp.prm.names.init <- get.init.prm.combs.name(object = PPM$object)
      temp.prm.names.addit <- get.addit.prm.combs.name(object = PPM$object)
      prm.sets.selected.ml$original <- NULL
      prm.sets.selected.ml$rescaled <- NULL

      for(i in 1:length(input$load_parameter_sets_ml)){
        ##to obtain corresponding parameter ranges for selected initial parameter space
        if(any(unlist( temp.prm.names.init) == input$load_parameter_sets_ml[i]) ){
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.names = temp.prm.names.init, input.name = input$load_parameter_sets_ml[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.prm.combs <- get.init.prm.combs(PPM$object,input$load_parameter_sets_ml[i], temp.range.name )


          prm.sets.selected.ml$original <-rbind(prm.sets.selected.ml$original, temp.prm.combs$prm.combs )
          prm.sets.selected.ml$rescaled <-rbind(prm.sets.selected.ml$rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL

        }else{
          ##to obtain corresponding parameter ranges and inital parameter set for selected additional parameter space
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(unlist(prm.names[[name]]) == input.name) },
                  prm.names = temp.prm.names.addit, input.name = input$load_parameter_sets_ml[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]

          temp.idx <- unlist(
            apply(matrix(unlist(temp.prm.names.init[[temp.range.name]])), 1,
                  function(name, prm.range.name, prm.names, input.name){
                    any(prm.names[names(prm.names) == name] == input.name) },
                  prm.range.name = temp.range.name, prm.names = temp.prm.names.addit[[temp.range.name]], input.name = input$load_parameter_sets_ml[i])
          )


          temp.prm.name.init <- unlist(temp.prm.names.init[[temp.range.name]])[temp.idx]

          temp.prm.combs <- get.addit.prm.combs(PPM$object,input$load_parameter_sets_ml[i], temp.range.name,temp.prm.name.init )
          temp.prm.combs.init <- get.init.prm.combs(PPM$object, temp.prm.name.init, temp.range.name )

          prm.sets.selected.ml$original <-rbind(prm.sets.selected.ml$original, temp.prm.combs$prm.combs )
          names(temp.prm.combs$prm.combs.z) <- names(temp.prm.combs$prm.combs)
          prm.sets.selected.ml$rescaled <-rbind(prm.sets.selected.ml$rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL
          temp.prm.combs.init <- NULL

        }
      }
      prm.sets.selected.ml$original <- prm.sets.selected.ml$original[order(prm.sets.selected.ml$original$pkey),]
      prm.sets.selected.ml$rescaled <- prm.sets.selected.ml$rescaled[order(prm.sets.selected.ml$rescaled$pkey),]



      })

      # phenotype.loaded.ml$list <- NULL
      # phenotype.loaded.ml$list <- get.phenotypes(PPM$object,name =  input$load_phenotype_ml)
      isolate({
      phenotype.values.selected.ml$DF <-data.frame(stringsAsFactors = F)
      for(temp.name in input$load_parameter_sets_ml){
        phenotype.values.selected.ml$DF <- rbind(phenotype.values.selected.ml$DF,  phenotype.loaded.ml$list[[temp.name]])
      }
      phenotype.values.selected.ml$DF$pkey <- as.character(phenotype.values.selected.ml$DF$pkey)

      phenotype.values.selected.ml$DF <- phenotype.values.selected.ml$DF[order(phenotype.values.selected.ml$DF$pkey),]
      })
    }

  })


  observeEvent(input$train_ml_model,{
    ##initialize ml.models.new
    if(is.null(ml.models.new$models)){
      ml.models.new$models <- list()
    }

    ##filter parameter combinations having the selected phenotype
    prm.sets.selected.ml$original <- prm.sets.selected.ml$original[prm.sets.selected.ml$original$pkey %in% phenotype.values.selected.ml$DF$pkey,]
    prm.sets.selected.ml$rescaled <- prm.sets.selected.ml$rescaled[prm.sets.selected.ml$rescaled$pkey %in% phenotype.values.selected.ml$DF$pkey,]

    # prm.sets.selected.ml$original <- prm.sets.selected.ml$original[order( prm.sets.selected.ml$original$pkey),]
    # prm.sets.selected.ml$rescaled <- prm.sets.selected.ml$rescaled[order( prm.sets.selected.ml$rescaled$pkey),]

    ## sample train set
    ml.model.trained$seed.num <- sample(1:100000,1)
    set.seed(ml.model.trained$seed.num)

    temp.idx.train <- sample(1:nrow( prm.sets.selected.ml$rescaled), nrow( prm.sets.selected.ml$rescaled)* input$ratio_train_test/100)
    temp.idx.train <- 1:nrow( prm.sets.selected.ml$rescaled) %in%  temp.idx.train
    temp.idx.train[is.na(phenotype.values.selected.ml$DF[,input$load_phenotype_ml])] <- FALSE

    ## train ML model
    ml.models.new$models[[input$ml_model_name]] <- list()


    temp.prms.custom <- setdiff(input$select_prms_ml, names(prm.sets.selected.ml$rescaled))
    temp.train.input <- prm.sets.selected.ml$rescaled[temp.idx.train ,setdiff(input$select_prms_ml,temp.prms.custom)]

    if(length(temp.prms.custom) != 0){
      temp.prms <- get.custom.scale.prms(PPM$object)
      temp.prms.df <- data.frame(pkey = prm.sets.selected.ml$original$pkey, stringsAsFactors = F)
      for(name in names(temp.prms)){
        if(any(temp.prms.custom %in% temp.prms[[name]])){
          temp.func <- get.custom.scale.func.obj(object = PPM$object,name = name)
          temp.prms.df <- cbind( temp.prms.df,
                                 temp.func(prm.combs = prm.sets.selected.ml$original,
                                           other.vals = get.custom.scale.func.obj.other.vals(PPM$object, name),
                                           cs.to.org = F)[,-1])
        }
      }
      temp.train.input <- cbind(temp.train.input, temp.prms.df[temp.idx.train, temp.prms.custom ])
    }


    if(any(is.na(phenotype.values.selected.ml$DF[temp.idx.train ,input$load_phenotype_ml]))){

    }


    ##regression
    if(input$ml_model_mode == "reg"){
      ml.models.new$models[[input$ml_model_name]]$ml.model<- randomForest::randomForest(temp.train.input,phenotype.values.selected.ml$DF[temp.idx.train ,input$load_phenotype_ml],
                                                                          keep.inbag = TRUE,
                                                                          importance = TRUE,
                                                                          ntree =  500,
                                                                          localImp = TRUE,
                                                                          corr.bias=FALSE
      )
      if(input$ml_model_mode == "reg" & input$bias_corr == "Yes"){
        #ml.model.trained$ml.model.res
        ml.models.new$models[[input$ml_model_name]]$ml.model.res  <- randomForest::randomForest(temp.train.input,  ml.models.new$models[[input$ml_model_name]]$ml.model$predicted - phenotype.values.selected.ml$DF[temp.idx.train ,input$load_phenotype_ml],
                                                                                  keep.inbag = TRUE,
                                                                                  importance = TRUE,
                                                                                  ntree =  500,
                                                                                  localImp = TRUE,
                                                                                  corr.bias=FALSE)
      }
    }else if(input$ml_model_mode == "class"){
      if(input$balanced == "Yes"){
        temp.min <- min(table(phenotype.values.selected.ml$DF.class[temp.idx.train ,input$load_phenotype_ml]))
        ml.models.new$models[[input$ml_model_name]]$ml.model<- randomForest::randomForest(temp.train.input,phenotype.values.selected.ml$DF.class[temp.idx.train ,input$load_phenotype_ml],
                                                                            keep.inbag = TRUE,
                                                                            importance = TRUE,
                                                                            ntree =  500,
                                                                            localImp = TRUE,
                                                                            sampsize = c(temp.min , temp.min )
        )
      }else{
        ml.models.new$models[[input$ml_model_name]]$ml.model<- randomForest::randomForest(temp.train.input,phenotype.values.selected.ml$DF.class[temp.idx.train ,input$load_phenotype_ml],
                                                                            keep.inbag = TRUE,
                                                                            importance = TRUE,
                                                                            ntree =  500,
                                                                            localImp = TRUE,
                                                                            corr.bias=FALSE
        )

      }

      temp.pred.perform <- list()
      temp.pred.perform$OOB <- list()
      temp.pred.perform$OOB[[ phenotype.values.selected.ml$DF.class.info$Class[1]]] <- rocpr(ml.models.new$models[[input$ml_model_name]]$ml.model,
                                                                                             NULL,
                                                                                             NULL,
                                                                                             phenotype.values.selected.ml$DF.class.info$Class[1])

      temp.pred.perform$OOB[[ phenotype.values.selected.ml$DF.class.info$Class[2]]] <- rocpr(ml.models.new$models[[input$ml_model_name]]$ml.model,
                                                                                             NULL,
                                                                                             NULL,
                                                                                             phenotype.values.selected.ml$DF.class.info$Class[2])

      temp.pred.perform$test.set <- list()

      if(length(temp.prms.custom ) != 0){
        temp.test.input <- cbind(prm.sets.selected.ml$rescaled[!temp.idx.train ,setdiff(input$select_prms_ml,temp.prms.custom)],
                                 temp.prms.df[!temp.idx.train, temp.prms.custom ])
      }else{
        temp.test.input <- prm.sets.selected.ml$rescaled[!temp.idx.train ,setdiff(input$select_prms_ml,temp.prms.custom)]

      }


      if(nrow(temp.test.input)!=0){
        temp.pred.perform$test.set[[ phenotype.values.selected.ml$DF.class.info$Class[1]]] <- rocpr(ml.models.new$models[[input$ml_model_name]]$ml.model,
                                                                                                    temp.test.input,
                                                                                                    phenotype.values.selected.ml$DF.class[!temp.idx.train ,input$load_phenotype_ml],
                                                                                                    phenotype.values.selected.ml$DF.class.info$Class[1])

        temp.pred.perform$test.set[[ phenotype.values.selected.ml$DF.class.info$Class[2]]] <- rocpr(ml.models.new$models[[input$ml_model_name]]$ml.model,
                                                                                                    temp.test.input,
                                                                                                    phenotype.values.selected.ml$DF.class[!temp.idx.train ,input$load_phenotype_ml],
                                                                                                    phenotype.values.selected.ml$DF.class.info$Class[2])
      }

    }

    #meta data for the trained ml model
    ml.models.new$models[[input$ml_model_name]]$phenotype <- input$load_phenotype_ml
    ml.models.new$models[[input$ml_model_name]]$name <- input$ml_model_name
    ml.models.new$models[[input$ml_model_name]]$mode <- input$ml_model_mode
    ml.models.new$models[[input$ml_model_name]]$prm.sets.used <- input$load_parameter_sets_ml
    ml.models.new$models[[input$ml_model_name]]$ml.model$pkey <-prm.sets.selected.ml$rescaled$pkey[temp.idx.train]
    if(!is.null( ml.model.trained$ml.model.res)){
      ml.models.new$models[[input$ml_model_name]]$ml.model.res$pkey <- prm.sets.selected.ml$rescaled$pkey[temp.idx.train]
    }
    ml.models.new$models[[input$ml_model_name]]$train.data <- prm.sets.selected.ml$rescaled$pkey[temp.idx.train]
    ml.models.new$models[[input$ml_model_name]]$test.data <- prm.sets.selected.ml$rescaled$pkey[!temp.idx.train]

    ml.models.new$models[[input$ml_model_name]]$note <- phenotype.filter$condition
    ml.models.new$models[[input$ml_model_name]]$seed.num <- ml.model.trained$seed.num

    if(length(temp.prms.custom ) != 0){
      for(name in names(temp.prms)){
        if(any(temp.prms.custom %in% temp.prms[[name]])){
          temp.custom.scale <- get.custom.scale(PPM$object, name)
          ml.models.new$models[[input$ml_model_name]]$custom.scale <- list()
          ml.models.new$models[[input$ml_model_name]]$custom.scale$name <- name
          ml.models.new$models[[input$ml_model_name]]$custom.scale$parameters <- temp.custom.scale$parameters
          ml.models.new$models[[input$ml_model_name]]$custom.scale$values <- temp.prms.df[,c("pkey",temp.prms.custom )]
        }
      }
    }

    if(input$ml_model_mode == "class"){
      ml.models.new$models[[input$ml_model_name]]$class.def <- list(DF = phenotype.values.selected.ml$DF.class,
                                                                    info = phenotype.values.selected.ml$DF.class.info,
                                                                    pred.perform = temp.pred.perform)

    }
  })

  output$new_ml_models_ml_ui <- renderUI({

    ml.models.names <- NULL
    if(!is.null(ml.models.new$models)){
      ml.models.names <- names(ml.models.new$models)
    }else{}
    isolate({
      list(
        selectInput("new_ml_models_ml",label = h4("Newly trained ML models"),choices =   ml.models.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })
  })

  observeEvent(input$new_ml_models_ml,{
    if(!is.null(input$new_ml_models_ml)){
      ml.model.trained$ml.model <-  NULL
      ml.model.trained$ml.model.res <-   NULL
      ml.model.trained$train.data <-  NULL
      ml.model.trained$test.data <-   NULL
      ml.model.trained$prm.sets.used <-   NULL
      ml.model.trained$phenotype <-  NULL
      ml.model.trained$name <- NULL
      ml.model.trained$class.def <- NULL
      ml.model.trained$custom.scale <-NULL
      ml.model.trained$mode <- NULL

      ml.model.trained$ml.model <-  ml.models.new$models[[input$new_ml_models_ml]]$ml.model
      ml.model.trained$ml.model.res <-   ml.models.new$models[[input$new_ml_models_ml]]$ml.model.res
      ml.model.trained$train.data <-  ml.models.new$models[[input$new_ml_models_ml]]$train.data
      ml.model.trained$test.data <-   ml.models.new$models[[input$new_ml_models_ml]]$test.data
      ml.model.trained$prm.sets.used <-   ml.models.new$models[[input$new_ml_models_ml]]$prm.sets.used
      ml.model.trained$phenotype <-   ml.models.new$models[[input$new_ml_models_ml]]$phenotype
      ml.model.trained$name <- ml.models.new$models[[input$new_ml_models_ml]]$name
      ml.model.trained$class.def <-  ml.models.new$models[[input$new_ml_models_ml]]$class.def
      ml.model.trained$custom.scale <- ml.models.new$models[[input$new_ml_models_ml]]$custom.scale
      ml.model.trained$mode <- ml.models.new$models[[input$new_ml_models_ml]]$mode
    }

  })



  observeEvent(input$remove_ml,{
    ml.models.new$models[[input$new_ml_models_ml]] <- NULL
    if(ml.model.trained$name == input$new_ml_models_ml){
      ml.model.trained$ml.model <-  NULL
      ml.model.trained$ml.model.res <-   NULL
      ml.model.trained$train.data <-  NULL
      ml.model.trained$test.data <-   NULL
      ml.model.trained$prm.sets.used <-   NULL
      ml.model.trained$phenotype <-  NULL
      ml.model.trained$name <- NULL
      ml.model.trained$class.def <- NULL
      ml.model.trained$custom.scale <- NULL
      ml.model.trained$mode <- NULL
    }
  })


  ##register to PPM
  observeEvent(input$register_ml,{
    temp.size.ml.model <- object.size(ml.models.new$models[[input$new_ml_models_ml]]$ml.model)
    temp.size.ml.model.res <- object.size(ml.models.new$models[[input$new_ml_models_ml]]$ml.model.res)

    #save as Rds files
    #if(temp.size.ml.model + temp.size.ml.model.res > 400000000){
    if(temp.size.ml.model + temp.size.ml.model.res > 0){
      temp.ml.model.path <- paste0("ml.models/", ml.models.new$models[[input$new_ml_models_ml]]$name,".Rds")
      temp.ml.model.res.path  <- paste0("ml.models/", ml.models.new$models[[input$new_ml_models_ml]]$name,".res.Rds")

      saveRDS(ml.models.new$models[[input$new_ml_models_ml]]$ml.model, file = temp.ml.model.path )
      saveRDS(ml.models.new$models[[input$new_ml_models_ml]]$ml.model.res, file =temp.ml.model.res.path)


      add.ml.model(PPM$object) <- list(phenotype.name =  ml.models.new$models[[input$new_ml_models_ml]]$phenotype ,
                                              name =  ml.models.new$models[[input$new_ml_models_ml]]$name,
                                              ml.model.path = temp.ml.model.path,
                                              ml.model.res.path = temp.ml.model.res.path,
                                              mode =  ml.models.new$models[[input$new_ml_models_ml]]$mode,
                                              prm.sets.used =  ml.models.new$models[[input$new_ml_models_ml]]$prm.sets.used ,
                                              train.data =   ml.models.new$models[[input$new_ml_models_ml]]$train.data,
                                              test.data =  ml.models.new$models[[input$new_ml_models_ml]]$test.data,
                                              class.def =  ml.models.new$models[[input$new_ml_models_ml]]$class.def,
                                              note = ml.models.new$models[[input$new_ml_models_ml]]$note,
                                              seed.num = ml.models.new$models[[input$new_ml_models_ml]]$seed.num,
                                              custom.scale = ml.models.new$models[[input$new_ml_models_ml]]$custom.scale)

    }else{
      add.ml.model(PPM$object) <- list(phenotype.name =  ml.models.new$models[[input$new_ml_models_ml]]$phenotype ,
                                              name =  ml.models.new$models[[input$new_ml_models_ml]]$name,
                                              ml.model =ml.models.new$models[[input$ml_model_name]]$ml.model,
                                              ml.model.res = ml.models.new$models[[input$ml_model_name]]$ml.model.res,
                                              mode =  ml.models.new$models[[input$new_ml_models_ml]]$mode,
                                              prm.sets.used =  ml.models.new$models[[input$new_ml_models_ml]]$prm.sets.used ,
                                              train.data =   ml.models.new$models[[input$new_ml_models_ml]]$train.data,
                                              test.data =  ml.models.new$models[[input$new_ml_models_ml]]$test.data,
                                              class.def =  ml.models.new$models[[input$new_ml_models_ml]]$class.def,
                                              note = ml.models.new$models[[input$new_ml_models_ml]]$note,
                                              seed.num = ml.models.new$models[[input$new_ml_models_ml]]$seed.num,
                                              custom.scale = ml.models.new$models[[input$new_ml_models_ml]]$custom.scale)
    }
  })


  ##Further manipulation
  output$manual_curation_button_ml_ui <- renderUI({
    if(is.null(input$load_phenotype_ml) | is.null(input$load_parameter_sets_ml)){
      return()
    }else if(input$load_phenotype_ml != "None" & !is.null(input$load_parameter_sets_ml)){
      actionButton("manual_curation_button_ml", h5("Further manipulation"))
    }

  })


  condition.counter <- reactiveValues()

  observeEvent(input$manual_curation_button_ml,{
    condition.counter$value <- 0
  })

  output$manual_curation_hist_ml_ui <- renderUI({
    if(!is.null(input$manual_curation_button_ml)){
      if(input$manual_curation_button_ml[[1]]%%2 == 0 | is.null(phenotype.values.selected.ml$DF) ){
        return()
      }else{
        if(!is.null(phenotype.values.selected.ml$DF)){
          box(width = 12,
           column(4,
                     h4("Histogram for a selected phenotype"),
                     sliderInput(inputId = "hist_phen_range_ml",
                                 label = h5("Range to plot"),
                                 min = signif(min(phenotype.values.selected.ml$DF[,2], na.rm=T),3),
                                 max = signif(max(phenotype.values.selected.ml$DF[,2],na.rm = T),3),
                                 step = signif((max(phenotype.values.selected.ml$DF[,2]) - min(phenotype.values.selected.ml$DF[,2])),3)/500,
                                 round = -1,
                                 dragRange = TRUE,
                                 value = c( min(phenotype.values.selected.ml$DF[,2]), max(phenotype.values.selected.ml$DF[,2]))
                     ),
                     sliderInput(inputId = "hist_phen_breaks", label = h5("Number of breaks (10~1000)"),min = 10, max = 1000, step = 1, value = 100),
                     plotOutput("hist_phen_ml")
                     ),
              column(4,
                     h4("Scatter plot for selected phenotypes"),
                     selectInput(inputId = "phenotype_choice1_ml",
                                 label = "Choose the first phenotype:",
                                 choices =  get.phenotypes.name(PPM$object) ),
                     selectInput(inputId = "phenotype_choice2_ml",
                                 label = "Choose the second phenotype:",
                                 choices =  get.phenotypes.name(PPM$object) ),
                     uiOutput("check_scatter_filter_ml_ui"),
                     plotOutput("scatter_phen_ml")
                     ),
              column(4, style = 'padding:0px;',
                     uiOutput("filter_class_ui")
                     )
              )
        }else{ return()}
      }
    }else{return()}
  })

  output$check_scatter_filter_ml_ui<- renderUI({
    if(!is.null(phenotype.filter$condition)){
      checkboxInput("check_scatter_filter", h5("With filter"))
    }
  })


  output$filter_class_ui <- renderUI({
    if(input$ml_model_mode == "reg"){
      shinydashboard::tabBox(id = "filter_class_tab", selected = NULL, width = 12,
             tabPanel(title = "Filtering Condition", value = "filter_cond_select",
                      h4("Filtering condition"),
                      actionButton("add_condition","+"),
                      actionButton("remove_condition", "-"),
                      # uiOutput("add_remove_ui_1"),
                      # uiOutput("add_remove_ui_2"),
                      # uiOutput("add_remove_ui_3"),
                      # uiOutput("add_remove_ui_4"),
                      # uiOutput("add_remove_ui_5"),
                      # uiOutput("add_remove_ui_6"),
                      # uiOutput("add_remove_ui_7"),
                      # uiOutput("add_remove_ui_8"),
                      # uiOutput("add_remove_ui_9"),
                      # uiOutput("add_remove_ui_10"),
                      uiOutput("filter_conditions_ml_1"),
                      uiOutput("filter_conditions_ml_2"),
                      uiOutput("filter_conditions_ml_3"),
                      uiOutput("filter_conditions_ml_4"),
                      uiOutput("filter_conditions_ml_5"),
                      uiOutput("filter_conditions_ml_6"),
                      uiOutput("filter_conditions_ml_7"),
                      uiOutput("filter_conditions_ml_8"),
                      uiOutput("filter_conditions_ml_9"),
                      uiOutput("filter_conditions_ml_10"),
                      actionButton("filt_apply_ml", h5("Apply!"))
             )
      )
    }else if(input$ml_model_mode == "class"){
      temp.max = max(phenotype.values.selected.ml$DF[,2],na.rm = T)
      temp.min = min(phenotype.values.selected.ml$DF[,2],na.rm = T)

      shinydashboard::tabBox(id = "filter_class_tab", selected = NULL, width = 12,
             tabPanel(title = "Define classes of phenotype", value = "def_class",
                      sliderInput(inputId = "phen_range_class_bound_ml",
                                  label = h5(paste0("Adjust class boundary of ",input$load_phenotype_ml, ".")),
                                  min = signif(temp.min,3),
                                  max = signif(temp.max,3),
                                  step = signif((temp.max -temp.min),3)/500,
                                  round = -1,
                                  dragRange = FALSE,
                                  value = (temp.max + temp.min)/2
                                  ),
                      fluidRow(column(6,h5("Input class boundary: ")),
                               column(6,uiOutput("class_bound_ml_ui")) ),
                      fluidRow(column(6,h5("Input name for class1:")),
                               column(6,textInput("name_class1_ml", label = NULL, value = "Class1")) ),
                      fluidRow(column(6,h5("Input name for class2:")),
                               column(6,textInput("name_class2_ml", label = NULL, value = "Class2"))),
                      tableOutput("classes_ml"),

                      #
                      # column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
                      # column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
                      # column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
                      # column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
                      # column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
                      # column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
                      actionButton("class_apply_ml", h5("Apply!"))

                      ),
             tabPanel(title = "Filtering Condition", value = "filter_cond_select",
                      h4("Filtering condition"),
                      actionButton("add_condition","+"),
                      actionButton("remove_condition", "-"),
                      # uiOutput("add_remove_ui_1"),
                      # uiOutput("add_remove_ui_2"),
                      # uiOutput("add_remove_ui_3"),
                      # uiOutput("add_remove_ui_4"),
                      # uiOutput("add_remove_ui_5"),
                      # uiOutput("add_remove_ui_6"),
                      # uiOutput("add_remove_ui_7"),
                      # uiOutput("add_remove_ui_8"),
                      # uiOutput("add_remove_ui_9"),
                      # uiOutput("add_remove_ui_10"),
                      uiOutput("filter_conditions_ml_1"),
                      uiOutput("filter_conditions_ml_2"),
                      uiOutput("filter_conditions_ml_3"),
                      uiOutput("filter_conditions_ml_4"),
                      uiOutput("filter_conditions_ml_5"),
                      uiOutput("filter_conditions_ml_6"),
                      uiOutput("filter_conditions_ml_7"),
                      uiOutput("filter_conditions_ml_8"),
                      uiOutput("filter_conditions_ml_9"),
                      uiOutput("filter_conditions_ml_10"),
                      actionButton("filt_apply_ml", h5("Apply!"))
             )
      )
    }
  })

  output$class_bound_ml_ui <- renderUI({
    numericInput("class_bound_ml",
                 label = NULL,
                 value = input$phen_range_class_bound_ml,
                 min = min(phenotype.values.selected.ml$DF[,2]),
                 max = max(phenotype.values.selected.ml$DF[,2]),
                 width = "150px")
  })

  output$classes_ml <- renderTable({
    temp.t.f <- table(phenotype.values.selected.ml$DF[,2] >= input$class_bound_ml)
    if(is.na(temp.t.f["TRUE"])){
      temp.num.class1 <- 0
    }else{
      temp.num.class1 <- temp.t.f["TRUE"]
    }

    if(is.na(temp.t.f["FALSE"])){
      temp.num.class2 <- 0
    }else{
      temp.num.class2 <- temp.t.f["FALSE"]
    }

    temp.df = data.frame(Class = c(input$name_class1_ml, input$name_class2_ml),
                         Definition = c(paste0(input$load_phenotype_ml, " >= ", input$phen_range_class_bound_ml),
                                        paste0(input$load_phenotype_ml, " < ", input$phen_range_class_bound_ml)),
                         Size = c(temp.num.class1, temp.num.class2),
                         stringsAsFactors = F)


    return(temp.df)
  })


  observeEvent(input$add_condition,{
    if( condition.counter$value < 10){
      condition.counter$value <- condition.counter$value +1
    }

  })

  observeEvent(input$remove_condition,{
    if( condition.counter$value >= 1){
      condition.counter$value <- condition.counter$value -1
    }


  })
  #
  # output$add_remove_ui_1 <- renderUI({
  #   if(is.null(input$add_condition_2)){
  #     list(actionButton("add_condition_1","+"),
  #          actionButton("add_condition_2","-"))
  #   }
  # })
  #
  # output$add_remove_ui_2 <- renderUI({
  #   if(input$add_condition_1[[1]] > 0 & is.null(input$add_condition_3)){
  #     list(actionButton("add_condition_2","+"),
  #          actionButton("add_condition_2","-"))
  #   }
  # })
  #
  #
  ###
  output$filter_conditions_ml_1 <- renderUI({
    i = 1
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml) & condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
     }

  })


  output$filter_conditions_ml_2 <- renderUI({
    i = 2
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml) &condition.counter$value >= i ){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })



  output$filter_conditions_ml_3 <- renderUI({
    i = 3
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_4 <- renderUI({
    i = 4
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_5 <- renderUI({
    i = 5
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_6 <- renderUI({
    i = 6
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_7 <- renderUI({
    i = 7
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_8 <- renderUI({
    i = 8
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_9 <- renderUI({
    i = 9
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })

  output$filter_conditions_ml_10 <- renderUI({
    i = 10
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml)& condition.counter$value >= i){
      list(tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_open_",i), "(")),
           column(width =4,offset = 0, style='padding:0px;',selectInput(paste0("phen_filt_",i), label = NULL, choices =  get.phenotypes.name(PPM$object))),
           column(width =2,offset = 0, style='padding:0px;',selectInput(paste0("compare_filt_",i),label = NULL, choices =  c(">",">=", "<", "<=", "==", "!="))),
           column(width =3,offset = 0, style='padding:0px;',numericInput(paste0("thresh_filt",i), value = NULL,label = NULL )),
           column(width =1,offset = 0, style='padding:0px;',checkboxInput(paste0("perenth_close_",i), ")")),
           column(width =1,offset = 0, style='padding:0px;',radioButtons(paste0("and_or_",i), label= NULL ,choices = c("and", "or"), inline = T ))
      )
    }

  })


  # output$parameters <- renderUI({
  #   if(!is.null(numPrm$ml.model)){
  #     prmInput = vector("list", numPrm$ml.model)
  #     for(i in 1:numPrm$ml.model){
  #       prmInput[[i]] <- list(sliderInput(paste0("prm",i),
  #                                         parameters$ml.model[i],
  #                                         min = signif(min(prm.sets.selected$rescaled[,2:(numPrm$ml.model+1)]),3),
  #                                         max = signif(max(prm.sets.selected$rescaled[,2:(numPrm$ml.model+1)]),3),
  #                                         value = 0, step =.01))
  #     }
  #     return(prmInput)
  #   }
  # })
  #

  observeEvent(input$filt_apply_ml,{
    #Overwrite
    phenotype.values.selected.ml$DF <-data.frame(stringsAsFactors = F)
    for(temp.name in input$load_parameter_sets_ml){
      phenotype.values.selected.ml$DF <- rbind(phenotype.values.selected.ml$DF,  phenotype.loaded.ml$list[[temp.name]])
    }
    phenotype.values.selected.ml$DF <- phenotype.values.selected.ml$DF[order(phenotype.values.selected.ml$DF$pkey),]


    #parse conditions
    perenth_open <- paste0("perenth_open_", 1:condition.counter$value)
    phen_filt <- paste0("phen_filt_", 1:condition.counter$value)
    compare_filt <- paste0("compare_filt_", 1:condition.counter$value)
    thresh_filt <- paste0("thresh_filt", 1:condition.counter$value)
    perenth_close <- paste0("perenth_close_", 1:condition.counter$value)
    and_or <- paste0("and_or_", 1:condition.counter$value)


    temp.condition <- NULL
    temp.condition.exc <- NULL

    temp.phenotype.names <-NULL

    for(i in 1:length(phen_filt)){
      temp.phenotype.names <- append(temp.phenotype.names, input[[phen_filt[i]]])
    }

    temp.phenotypes.values.selected <- get.phenotypes.for.selected.prm.sets(object = PPM$object, phenotypes = temp.phenotype.names, prm.sets = input$load_parameter_sets_ml )
    temp.pkey <- phenotype.values.selected.ml$DF$pkey

    for(idx.phen in temp.phenotype.names){
      temp.pkey <- intersect(temp.pkey , temp.phenotypes.values.selected[[idx.phen ]]$pkey)
    }

    phenotype.values.selected.ml$DF <-  phenotype.values.selected.ml$DF[phenotype.values.selected.ml$DF$pkey %in% temp.pkey,]

    for(idx.phen in temp.phenotype.names){
      temp.phenotypes.values.selected[[idx.phen]] <- temp.phenotypes.values.selected[[idx.phen]][temp.phenotypes.values.selected[[idx.phen]]$pkey %in% temp.pkey,]
    }


    for(i in 1:condition.counter$value){

      if(i>1){
        if(input[[ and_or[i-1]]] == "and"){
          temp.condition <- paste0(temp.condition, "& ")
          temp.condition.exc <- paste0(temp.condition.exc, "& ")

        }else if(input[[ and_or[i-1]]] == "or"){
          temp.condition <- paste0(temp.condition, "| ")
          temp.condition.exc <- paste0(temp.condition.exc, "| ")
        }
      }

      if(input[[perenth_open[i]]]){
        temp.condition <- paste0(temp.condition, "( ")
        temp.condition.exc <- paste0(temp.condition.exc, "( ")
      }

      temp.condition <- paste0(temp.condition, input[[phen_filt[i]]], " ")
      temp.condition <- paste0(temp.condition, input[[compare_filt[i]]], " ")
      temp.condition <- paste0(temp.condition, input[[thresh_filt[i]]], " ")

      temp.condition.exc <- paste0(temp.condition.exc, "temp.phenotypes.values.selected[['", input[[phen_filt[i]]],"']][,2]" ," ")
      temp.condition.exc <- paste0(temp.condition.exc, input[[compare_filt[i]]], " ")
      temp.condition.exc <- paste0(temp.condition.exc, input[[thresh_filt[i]]], " ")

      if(input[[perenth_close[i]]]){
        temp.condition <- paste0(temp.condition, ") ")
        temp.condition.exc <- paste0(temp.condition.exc, ") ")
      }

    }
    print(temp.condition)
    print(temp.condition.exc)

    phenotype.filter$condition <- temp.condition
    phenotype.filter$condition.exc <- temp.condition.exc



    phenotype.filter$idx.filt <- eval(parse(text=temp.condition.exc))

    phenotype.values.selected.ml$DF <-  phenotype.values.selected.ml$DF[phenotype.filter$idx.filt, ]
    phenotype.values.selected.ml$DF <- na.omit(phenotype.values.selected.ml$DF )
  })

  observeEvent(input$class_apply_ml,{
    phenotype.values.selected.ml$DF <- na.omit(phenotype.values.selected.ml$DF)
    phenotype.values.selected.ml$DF.class <-  phenotype.values.selected.ml$DF
    phenotype.values.selected.ml$DF.class[,2] <-  input$name_class2_ml
    phenotype.values.selected.ml$DF.class[phenotype.values.selected.ml$DF[,2] >= input$class_bound_ml,2] <- input$name_class1_ml
    phenotype.values.selected.ml$DF.class[,2] <- factor(phenotype.values.selected.ml$DF.class[,2])

    temp.t.f <- table(phenotype.values.selected.ml$DF.class[,2])
    if(is.na(temp.t.f[input$name_class1_ml])){
      temp.num.class1 <- 0
    }else{
      temp.num.class1 <- temp.t.f[input$name_class1_ml]
    }

    if(is.na(temp.t.f[input$name_class2_ml])){
      temp.num.class2 <- 0
    }else{
      temp.num.class2 <- temp.t.f[input$name_class2_ml]
    }

    temp.df = data.frame(Class = c(input$name_class1_ml, input$name_class2_ml),
                         Definition = c(paste0(input$load_phenotype_ml, " >= ", input$phen_range_class_bound_ml),
                                        paste0(input$load_phenotype_ml, " < ", input$phen_range_class_bound_ml)),
                         Size = c(temp.num.class1, temp.num.class2),
                         stringsAsFactors = F)

    phenotype.values.selected.ml$DF.class.info <- temp.df
  })





  output$hist_phen_ml <- renderPlot(
    if(!is.null(phenotype.values.selected.ml$DF)){
      hist(phenotype.values.selected.ml$DF[,2], breaks = input$hist_phen_breaks, xlim = c(input$hist_phen_range_ml[1], input$hist_phen_range_ml[2]), main = paste0("Histogram of ",input$load_phenotype_ml ), xlab = input$load_phenotype_ml)
    }
  )

  output$scatter_phen_ml<- renderPlot({
    if(!is.null(phenotype.values.selected.ml$DF) & !is.null(input$load_parameter_sets_ml) & input$phenotype_choice1_ml != input$phenotype_choice2_ml ){
      temp.phenotype.values <- get.phenotypes(object = PPM$object, name = input$phenotype_choice1_ml)
      temp.phenotype.values1 <- data.frame(stringsAsFactors = F)
      for(temp.name in input$load_parameter_sets_ml){
        temp.phenotype.values1  <- rbind(temp.phenotype.values1, temp.phenotype.values[[temp.name]])
      }
      temp.phenotype.values1 <- temp.phenotype.values1[order(temp.phenotype.values1$pkey),]


      temp.phenotype.values <- get.phenotypes(object = PPM$object, name = input$phenotype_choice2_ml)
      temp.phenotype.values2 <- data.frame(stringsAsFactors = F)

      for(temp.name in input$load_parameter_sets_ml){
        temp.phenotype.values2  <- rbind(temp.phenotype.values2, temp.phenotype.values[[temp.name]])
      }

      temp.phenotype.values2 <- temp.phenotype.values2[order(temp.phenotype.values2$pkey),]

      temp.phenotype.values <- NULL
      if(is.null(phenotype.filter$condition)){
        plot(x =  temp.phenotype.values1[temp.phenotype.values1$pkey %in% temp.phenotype.values2$pkey ,2],
             y =  temp.phenotype.values2[temp.phenotype.values2$pkey %in% temp.phenotype.values1$pkey,2],
             xlab =input$phenotype_choice1_ml,
             ylab = input$phenotype_choice2_ml)
      }else{
        if( input$check_scatter_filter){
          temp.phenotype.values1 <- temp.phenotype.values1[temp.phenotype.values1$pkey %in% temp.phenotype.values2$pkey,]
          temp.phenotype.values2 <- temp.phenotype.values2[temp.phenotype.values2$pkey %in% temp.phenotype.values1$pkey,]

          plot(x =  temp.phenotype.values1[temp.phenotype.values1$pkey %in% phenotype.values.selected.ml$DF$pkey,2],
               y =  temp.phenotype.values2[temp.phenotype.values2$pkey %in% phenotype.values.selected.ml$DF$pkey,2],
               xlab =input$phenotype_choice1_ml,
               ylab = input$phenotype_choice2_ml)
        }else{
          plot(x =  temp.phenotype.values1[temp.phenotype.values1$pkey %in% temp.phenotype.values2$pkey,2],
               y =  temp.phenotype.values2[temp.phenotype.values2$pkey %in% temp.phenotype.values1$pkey,2],
               xlab =input$phenotype_choice1_ml,
               ylab = input$phenotype_choice2_ml)
        }
      }
    }
  })



##information for trained ml models
  output$performance <- renderPlot({
    if(!is.null(ml.model.trained$ml.model)){
      if(ml.model.trained$mode == "reg"){
        temp.main <- "Error rates for regression ML model"
      }else{
        temp.main <- "Error rates for classification ML model"
      }
      plot(ml.model.trained$ml.model, main = temp.main)
    }
  })

  output$varImp <- renderPlot({
    if(!is.null(ml.model.trained$ml.model)){
      randomForest::varImpPlot(ml.model.trained$ml.model,  main = paste0(ml.model.trained$name,": Global Variable Importance"), scale = F)
    }
  })


  output$options_bias_pred_ui <- renderUI({
    if(!is.null(ml.model.trained$ml.model)){
      if(ml.model.trained$mode == "reg" & !is.null(ml.model.trained$ml.model.res)){
        list(column(6,checkboxInput("plot_bias_corr","Bias correction")),
             column(6, radioButtons("oob_test", label= NULL ,choices = c("OOB", "Test set"), inline = T )) ## for test sets
        )
      }else if(ml.model.trained$mode== "reg" & is.null(ml.model.trained$ml.model.res)){
        column(6, radioButtons("oob_test", label= NULL ,choices = c("OOB", "Test set"), inline = T )) ## for test sets
      }else if(ml.model.trained$mode == "class"){
        list(
          fluidRow(
            column(6, radioButtons("oob_test", label= h5("Prediction for") ,choices = c(OOB = "OOB", 'Test set' = "test.set"), inline = FALSE )),## for test sets
           # column(6, selectInput("confusion_roc", label = h5("Choose display mode"), choices = c("Text", "Plot") ))
            column(6, selectInput("positive_class", label = h5("Positive class"), choices = ml.model.trained$class.def$info$Class))
          ),
          fluidRow(
            column(6, sliderInput("pred_cut_off", h5("Cut-off"), min = 0, max = 1, step = 0.01, value = 0.5))
          ),
          fluidRow(
            column(6,style='padding-left:0px;',
                   h5("Confusion matrix"),
                   tableOutput("conf_mat")),
            column(5,offset = 1,
                   tableOutput("conf_stats"))
          )
        )
      }
    }
  })


  output$conf_mat<- renderTable({

    temp.table  <-  ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$conf.mat[[(input$pred_cut_off*100+1)]]$table
    temp.df <- data.frame(pred_ref = row.names( temp.table),  temp.table[,1],  temp.table[,2])
    row.names(temp.df) <- NULL
    colnames(temp.df)[2:3] <- row.names( temp.table)
    return(temp.df)
  },rownames = F,align = "c")

  output$conf_stats<- renderTable({
    temp.conf.mat <- ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$conf.mat[[(input$pred_cut_off*100+1)]]
    temp.df <- temp.conf.mat$byClass[1:4]
    temp.df <- append( temp.df, c(temp.conf.mat$overall[1], temp.conf.mat$byClass[11]))
    temp.df <- data.frame(temp.df)
    return(temp.df)
  },align = "c", colnames = F, rownames = T)



  output$OOB_test_pred_ui <-renderUI({
    if(!is.null(ml.model.trained$ml.model)){
      if(ml.model.trained$mode == "reg"){
        if(input$oob_test == "OOB"){
          column(6,plotOutput("OOB_prediction"))
        }else if(input$oob_test != "OOB" & (!is.null(ml.model.trained$test.data) & length(ml.model.trained$test.data) != 0)){
          column(6,plotOutput("test_set_prediction"))
        }
      }else if (ml.model.trained$mode == "class"){
        list(
          column(6,plotOutput("prediction_roc_plot_class")),
          column(6,plotOutput("prediction_pr_plot_class"))
        )
      }
    }
  })


  output$prediction_roc_plot_class <- renderPlot({
    if(length(ml.model.trained$class.def$pred.perform[[input$oob_test]]) != 0 ){
      plot(ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$roc,  type= "b", main = "ROC", xlab = "False Positive", ylab = "True Positive", col= colfunc(101), pch = 19, xlim = c(0,1), ylim=c(0,1))
      points(t(ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$roc[(input$pred_cut_off*100+1),1:2]))
    }
  })

  output$prediction_pr_plot_class <- renderPlot({
    if(length(ml.model.trained$class.def$pred.perform[[input$oob_test]]) != 0 ){
      plot(ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$prec.recall, type= "b", main = "Precision-Recall", xlab = "Recall", ylab = "Precision", col= colfunc(101), pch = 19, xlim = c(0,1), ylim=c(0,1))
      points(t(ml.model.trained$class.def$pred.perform[[input$oob_test]][[input$positive_class]]$prec.recall[(input$pred_cut_off*100+1),1:2]))
    }
  })








  ##regression
  output$OOB_prediction <- renderPlot({
    if(is.null( ml.model.trained$ml.model) & is.null( ml.model.trained$ml.model.res) ){
      return()
    }else if(!is.null( ml.model.trained$ml.model) & is.null( ml.model.trained$ml.model.res) ){
      df <- data.frame(x = ml.model.trained$ml.model$y, y = ml.model.trained$ml.model$predicted )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.trained$ml.model$y, ml.model.trained$ml.model$predicted ,use = "complete.obs"), 3)))+ggplot2:: theme_bw() + ggplot2::xlab("Simulated      ") +ggplot2:: ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.trained$ml.model) & !is.null( ml.model.trained$ml.model.res) & input$plot_bias_corr == FALSE){
      df <- data.frame(x = ml.model.trained$ml.model$y, y = ml.model.trained$ml.model$predicted )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.trained$ml.model$y, ml.model.trained$ml.model$predicted ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.trained$ml.model) & input$plot_bias_corr == TRUE){
      df <- data.frame(x = ml.model.trained$ml.model$y, y = ml.model.trained$ml.model$predicted -ml.model.trained$ml.model.res$predicted)
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.trained$ml.model$y, ml.model.trained$ml.model$predicted -ml.model.trained$ml.model.res$predicted ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") +ggplot2:: ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }
  })

  output$test_set_prediction <- renderPlot({
    isolate({
      ##load selected parameter sets
      temp.prm.ranges.names <- get.prm.ranges.name(object = PPM$object)
      temp.prm.names.init <- get.init.prm.combs.name(object = PPM$object)
      temp.prm.names.addit <- get.addit.prm.combs.name(object = PPM$object)
      prm.sets.test.original <- data.frame(stringsAsFactors = F)
      prm.sets.test.rescaled <- data.frame(stringsAsFactors = F)

      for(i in 1:length(ml.model.trained$prm.sets.used)){
        ##to obtain corresponding parameter ranges for selected initial parameter space
        if(any(unlist( temp.prm.names.init) ==ml.model.trained$prm.sets.used[i]) ){
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.names = temp.prm.names.init, input.name = ml.model.trained$prm.sets.used[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.prm.combs <- get.init.prm.combs(PPM$object,ml.model.trained$prm.sets.used[i], temp.range.name )


          prm.sets.test.original  <-rbind( prm.sets.test.original , temp.prm.combs$prm.combs )
          prm.sets.test.rescaled <-rbind(prm.sets.test.rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL

        }else{
          ##to obtain corresponding parameter ranges and inital parameter set for selected additional parameter space
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(unlist(prm.names[[name]]) == input.name) },
                  prm.names = temp.prm.names.addit, input.name = ml.model.trained$prm.sets.used[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.idx <- unlist(
            apply(matrix(unlist(temp.prm.names.init[[temp.range.name]])), 1,
                  function(name, prm.range.name, prm.names, input.name){
                    any(prm.names[[ prm.range.name]][[name]] == input.name) },
                  prm.range.name = temp.range.name, prm.names = temp.prm.names.addit[[temp.range.name]], input.name = ml.model.trained$prm.sets.used[i])
          )
          temp.prm.name.init <- unlist(temp.prm.names.init[[temp.range.name]])[temp.idx]

          temp.prm.combs <- get.addit.prm.combs(PPM$object,ml.model.trained$prm.sets.used[i], temp.range.name,temp.prm.name.init )
          temp.prm.combs.init <- get.init.prm.combs(PPM$object, temp.prm.name.init, temp.range.name )

          prm.sets.test.original  <-rbind( prm.sets.test.original , temp.prm.combs$prm.combs )
          names(temp.prm.combs$prm.combs.z ) <- names(temp.prm.combs$prm.combs)
          prm.sets.test.rescaled <-rbind( prm.sets.test.rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL
          temp.prm.combs.init <- NULL

        }
      }
      prm.sets.test.original<- prm.sets.test.original[order(prm.sets.test.original$pkey),]
      prm.sets.test.rescaled <- prm.sets.test.rescaled[order(prm.sets.test.rescaled$pkey),]

      prm.sets.test.original<- prm.sets.test.original[prm.sets.test.original$pkey %in% ml.model.trained$test.data,]
      prm.sets.test.rescaled <- prm.sets.test.rescaled[prm.sets.test.rescaled$pkey  %in% ml.model.trained$test.data ,]

      ##load phenotype values
      phenotype.values.test <-data.frame(stringsAsFactors = F)
      phenotype.loaded.ml.list <- get.phenotypes(PPM$object, ml.model.trained$phenotype)
      for(temp.name in ml.model.trained$prm.sets.used){
        phenotype.values.test <- rbind(phenotype.values.test, phenotype.loaded.ml.list[[temp.name]])
      }
      phenotype.values.test <- phenotype.values.test[order(phenotype.values.test$pkey),]
      phenotype.values.test <- phenotype.values.test[phenotype.values.test$pkey %in% ml.model.trained$test.data,]

      if(!is.null(ml.model.trained$custom.scale)){
        prm.sets.test.rescaled <- cbind(prm.sets.test.rescaled, ml.model.trained$custom.scale$values[ ml.model.trained$custom.scale$values$pkey %in%  ml.model.trained$test.data,names(ml.model.trained$custom.scale$parameters)])
      }

  })

    if(is.null( ml.model.trained$ml.model) & is.null( ml.model.trained$ml.model.res) ){
      return()
    }else if(!is.null( ml.model.trained$ml.model) & is.null( ml.model.trained$ml.model.res) ){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.trained$ml.model,prm.sets.test.rescaled[,-1]))
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("Test set Pred VS Sim: corr = ", signif(cor( df$x, df$y,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") +ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.trained$ml.model) & !is.null( ml.model.trained$ml.model.res) & input$plot_bias_corr == FALSE){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.trained$ml.model,prm.sets.test.rescaled[,-1]) )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("Test set VS Sim: corr = ", signif(cor( df$x, df$y ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") +ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.trained$ml.model) & input$plot_bias_corr == TRUE){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.trained$ml.model,prm.sets.test.rescaled[,-1]) -  predict(ml.model.trained$ml.model.res,prm.sets.test.rescaled[,-1]) )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) +ggplot2::ylim(min(ml.model.trained$ml.model$y),max(ml.model.trained$ml.model$y)) + ggplot2::ggtitle(paste0("Test set VS Sim: corr = ", signif(cor(df$x, df$y,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }
  })




  #####################################
  ######## Explore Phase space ########
  #####################################
  #load data
  phenotype.loaded <- reactiveValues()
  output$list_phenotypes_tab_ui <- renderUI({
    phenotypes.names <- NULL
    if(!is.null(PPM$object)){

      phenotypes.names <-  get.phenotypes.name(PPM$object)
      phenotypes.names <- append("None", phenotypes.names)
    }

    if(!is.null(PPM$object) && input$with_ml.models == T){
      phenotypes.names <- phenotypes.names[phenotypes.names%in% get.phenotypes.with.ml.models(PPM$object)]
    }
    isolate({
      list(
        selectInput("load_phenotype",label = h4("Select a phenotype"),choices =   phenotypes.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })
  })

  observe({
    if(!is.null(input$load_phenotype)){
      if(input$load_phenotype != "None"){
        phenotype.loaded$list <- get.phenotypes(PPM$object,input$load_phenotype)
      }else{
        phenotype.loaded$list <- NULL
      }
    }else{
      phenotype.loaded$list <- NULL
    }
  })

  output$list_prm_sets_tab_ui <- renderUI({
    prm.sets.names <- NULL
    if(!is.null(input$load_phenotype)&& input$load_phenotype != "None"){
      prm.sets.names <- names(phenotype.loaded$list)
    }else if(!is.null(input$load_phenotype) && input$load_phenotype == "None"){
      prm.sets.names <- append(unlist(get.init.prm.combs.name(PPM$object)),
                               unlist(get.addit.prm.combs.name(PPM$object)))
      names(prm.sets.names) <- NULL
    }

    if(!is.null(PPM$object) && input$with_tsne == T){#show only ones with tsne
      prm.sets.names <- prm.sets.names[prm.sets.names%in% get.tsne.coord.name(PPM$object)]

    }
    if(!is.null(prm.sets.names)){
      prm.sets.names <- prm.sets.names[order(prm.sets.names)]
    }
    isolate({
      list(
        selectInput("load_parameter_sets",label = h4("Select parameter sets"),choices =   prm.sets.names, size = 3,multiple = TRUE, selectize = FALSE)
      )
    })
  })

  output$list_ml.models_tab_ui <- renderUI({
    ml.models.names <- NULL
    if(!is.null(PPM$object)){
      if(!is.null(input$load_phenotype) && input$load_phenotype != "None"){
        if(!is.null(get.ml.models.name(PPM$object))){
          ml.models.names <- get.ml.models.name(PPM$object)
          names(ml.models.names) <- get.phenotypes.name(PPM$object)
          ml.models.names <- ml.models.names[[input$load_phenotype]]
          names(ml.models.names) <- NULL
        }else{}

      }
    }else{}
    isolate({
      list(
        selectInput("load_ml.model",label = h4("Select a ML model"),choices =   ml.models.names, size = 3,multiple = FALSE, selectize = FALSE)
      )
    })
  })


  output$test_exp_phase_tab <- renderText({
    #input$launch_exp_phase
    if(!is.null(ml.model.selected$name)){
      ml.model.selected$name
    }
  })


  #phase space
  #tsne_range = reactiveValues(x=c(-30,30), y = c(-30,30))
  tsne_range = reactiveValues()
  prm.sets.selected <- reactiveValues()
  phenotype.values.selected <- reactiveValues()
  prm.ranges.selected <- reactiveValues()
  prm.ranges.z.selected <- reactiveValues() # The ranges of perturbation. If ML models use custom scale for some parameter, then replace with custum ranges

  prm.grids.selected <- reactiveValues()
  prm.set.method <- reactiveValues()
  ml.model.selected <- reactiveValues()
  local.importance <- reactiveValues()

  #parameters
  ##number of parameters
  numPrm <- reactiveValues() # = 10 ## later to be detected automatically
  parameters <- reactiveValues()  #names(data.clust.z.combined)[2:(numPrm+1)]



  ##loading parameter sets with accordance with the selection above
  observeEvent(input$launch_exp_phase,{
    prm.sets.selected$tsne <- data.frame(stringsAsFactors = F)
    prm.sets.selected$original <- data.frame(stringsAsFactors = F)
    prm.sets.selected$rescaled <- data.frame(stringsAsFactors = F)
    phenotype.values.selected$DF <- data.frame(stringsAsFactors = F)

    ##launch parameter space
    if(!is.null(input$load_parameter_sets)){
      for(i in 1:length(input$load_parameter_sets)){
        prm.sets.selected$tsne <- rbind(prm.sets.selected$tsne, get.tsne.coord(PPM$object,input$load_parameter_sets[i] ) )
        temp.prm.ranges.names <- get.prm.ranges.name(object = PPM$object)
        temp.prm.names.init <- get.init.prm.combs.name(object = PPM$object)
        temp.prm.names.addit <- get.addit.prm.combs.name(object = PPM$object)

        tsne_range$x = c(-30,30)
        tsne_range$y = c(-30,30)
        #tsne_range$x <- 1.05*range(prm.sets.selected$tsne$tSNE1)
        #tsne_range$y <- 1.05*range(prm.sets.selected$tsne$tSNE2)

        ##to obtain corresponding parameter ranges for selected initial parameter space
        if(any(unlist( temp.prm.names.init) == input$load_parameter_sets[i]) ){
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.names = temp.prm.names.init, input.name = input$load_parameter_sets[i])
            )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.prm.combs <- get.init.prm.combs(PPM$object,input$load_parameter_sets[i], temp.range.name )



          prm.ranges.z.selected$DF <- apply( temp.prm.combs$prm.combs.z[-1], 2, range)
          prm.ranges.selected$DF <- get.prm.ranges(object = PPM$object,name = temp.range.name )
          prm.ranges.selected$DF <- data.frame(prm.ranges.selected$DF, log.scale = temp.prm.combs$log.scale$log.scale, stringsAsFactors = F)

          prm.set.method$method <- temp.prm.combs$method
          if(temp.prm.combs$method == "unif_grid"){
            prm.ranges.selected$DF <- data.frame(prm.ranges.selected$DF, temp.prm.combs$num.grids$`number of grids`, stringsAsFactors = F)
            names(prm.ranges.selected$DF)[5] = "number of grids"
            prm.grids.selected$DF <- temp.prm.combs$prm.grids
          }else{
            prm.sets.selected$raw.smpl <-  temp.prm.combs$raw.smpl
          }





          prm.sets.selected$original <-rbind(prm.sets.selected$original, temp.prm.combs$prm.combs )
          prm.sets.selected$rescaled <-rbind(prm.sets.selected$rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL

        }else{
          ##to obtain corresponding parameter ranges and inital parameter set for selected additional parameter space
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(unlist(prm.names[[name]]) == input.name) },
                  prm.names = temp.prm.names.addit, input.name = input$load_parameter_sets[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.idx <- unlist(
            apply(matrix(unlist(temp.prm.names.init[[temp.range.name]])), 1,
                  function(name, prm.range.name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.range.name = temp.range.name, prm.names = temp.prm.names.addit[[temp.range.name]], input.name = input$load_parameter_sets[i])
          )


          temp.prm.name.init <- unlist(temp.prm.names.init[[temp.range.name]])[temp.idx]

          temp.prm.combs <- get.addit.prm.combs(PPM$object,input$load_parameter_sets[i], temp.range.name,temp.prm.name.init )
          temp.prm.combs.init <- get.init.prm.combs(PPM$object, temp.prm.name.init, temp.range.name )

          if(is.null(prm.ranges.z.selected$DF)){
            prm.ranges.z.selected$DF <- apply( temp.prm.combs.init$prm.combs.z[,-1],2, range)
          }
          if(is.null( prm.ranges.selected$DF)){
            prm.ranges.selected$DF <- get.prm.ranges(object = PPM$object,name = temp.range.name)
            prm.ranges.selected$DF <- data.frame(prm.ranges.selected$DF, log.scale = temp.prm.combs.init$log.scale$log.scale, stringsAsFactors = F)
            prm.set.method$method <- temp.prm.combs.init$method

            if(temp.prm.combs.init$method == "unif_grid"){
              prm.ranges.selected$DF <- data.frame(prm.ranges.selected$DF, temp.prm.combs.init$num.grids$`number of grids`, stringsAsFactors = F)
              names(prm.ranges.selected$DF)[5] = "number of grids"
              prm.grids.selected$DF <- temp.prm.combs.init$prm.grids
            }else{
              prm.sets.selected$raw.smpl <-  temp.prm.combs.init$raw.smpl
            }
          }


          prm.sets.selected$original <-rbind(prm.sets.selected$original, temp.prm.combs$prm.combs )
          names(temp.prm.combs$prm.combs.z) <- names(temp.prm.combs$prm.combs)
          prm.sets.selected$rescaled <-rbind(prm.sets.selected$rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL
          temp.prm.combs.init <- NULL

        }
        if(input$load_phenotype != "None"){
          phenotype.values.selected$DF <- rbind( phenotype.values.selected$DF, phenotype.loaded$list[[input$load_parameter_sets[i]]] )
        }else{
          phenotype.values.selected$DF <- NULL
        }
      }
    }else{
      prm.sets.selected$tsne <- NULL
      prm.sets.selected$original <- NULL
      prm.sets.selected$rescaled <- NULL
      phenotype.values.selected$DF <- NULL
      prm.ranges.selected$DF <- NULL
      prm.ranges.z.selected$DF <- NULL
      prm.grids.selected$DF <- NULL
      prm.set.method$method <- NULL

    }

    ##launch ml.model
    if(!is.null(input$load_ml.model)){
      temp.ml.model <- get.ml.model(object = PPM$object, phenotype.name = input$load_phenotype, ml.model.name = input$load_ml.model  )
      if(!is.null(temp.ml.model$custom.scale)){
        ml.model.selected$custom.scale <- temp.ml.model$custom.scale
      }

      if(!is.null( temp.ml.model$ml.model.path )){
        ml.model.selected$ml.model <- readRDS(temp.ml.model$ml.model.path)
        if(!is.null(temp.ml.model$ml.model.res.path)){
          ml.model.selected$ml.model.res <- readRDS(temp.ml.model$ml.model.res.path)
        }
      }else{

        ml.model.selected$ml.model <- temp.ml.model[["ml.model"]]
        if(!is.null(temp.ml.model[[2]])){
          ml.model.selected$ml.model.res <- temp.ml.model[["ml.model.res"]]
        }
      }
      ml.model.selected$train.data <- temp.ml.model$train.data
      ml.model.selected$test.data <- temp.ml.model$test.data
      ml.model.selected$mode <- temp.ml.model$mode
      ml.model.selected$prm.sets.used <- temp.ml.model$prm.sets.used
      ml.model.selected$class.def <- temp.ml.model$class.def
      ml.model.selected$note <- temp.ml.model$note
      ml.model.selected$phenotype <-  input$load_phenotype
      ml.model.selected$name <- temp.ml.model$name

      ##ml.model specific tsne
      if(get.tsne.coord.ml.exist(object = PPM$object, ml.model.name = input$load_ml.model  )){
        if(!is.null(input$load_parameter_sets)){
          ml.model.selected$tsne <- data.frame(stringsAsFactors = F)
          for(i in 1:length(input$load_parameter_sets)){
            ml.model.selected$tsne <- rbind(ml.model.selected$tsne, get.tsne.coord.ml(PPM$object, ml.model.name = input$load_ml.model, name = input$load_parameter_sets[i]))
          }
          ml.model.selected$tsne <-  ml.model.selected$tsne[order( ml.model.selected$tsne$pkey),]
        }
      }

      rm(temp.ml.model)
      local.importance$DF <- data.frame(pkey =  ml.model.selected$ml.model$pkey, t( ml.model.selected$ml.model$localImportance), stringsAsFactors = F)

    }else{
      ml.model.selected$name <- NULL
      ml.model.selected$ml.model <-NULL
      ml.model.selected$ml.model.res <-NULL
      ml.model.selected$train.data <- NULL
      ml.model.selected$test.data <- NULL
      ml.model.selected$custom.scale <- NULL
      ml.model.selected$mode <- NULL
      ml.model.selected$prm.sets.used <- NULL
      ml.model.selected$class.def <- NULL
      ml.model.selected$note <- NULL
      ml.model.selected$phenotype <-  NULL
      ml.model.selected$tsne <- NULL
      local.importance$DF <- NULL

    }


    if(!is.null(prm.sets.selected$original)){
      numPrm$prmset <- nrow(prm.ranges.selected$DF)
      parameters$prmset <- prm.ranges.selected$DF$names
    }else{
      numPrm$prmset <- NULL
      parameters$prmset <- NULL
    }

    if(!is.null(ml.model.selected$ml.model)){
      numPrm$ml.model <- nrow(ml.model.selected$ml.model$localImportance)
      parameters$ml.model <- row.names(ml.model.selected$ml.model$localImportance)
    }else{
      numPrm$ml.model <- NULL
      parameters$ml.model <- NULL
    }

    if(!is.null( ml.model.selected$custom.scale )){
      prm.ranges.z.selected$DF <- prm.ranges.z.selected$DF[,intersect(parameters$ml.model, parameters$prmset)]
      prm.ranges.z.selected$DF <- cbind(prm.ranges.z.selected$DF, apply(  ml.model.selected$custom.scale$values[,-1], 2, range))
    }else{}

  })

  #####side bar
  phen.range <- reactiveValues()

  observeEvent(input$launch_exp_phase,{
    if(!is.null(phenotype.values.selected$DF)){
      phen.range$DF <- signif(range(phenotype.values.selected$DF[,2], na.rm = T),1)
      temp.range <- range(phenotype.values.selected$DF[,2],na.rm = T)

      if(phen.range$DF[1]>temp.range[1]){

      }

      if(phen.range$DF[2] < temp.range[2]){

      }

    }else{
      phen.range$DF <- NULL
    }
  })



  output$exp_phase_side_ui <- renderUI({
    if(!is.null(phen.range$DF) ){
      list(sliderInput("phen.range", label = h5("Range of phenotype"),
                       min = phen.range$DF[1], max = phen.range$DF[2], step = (phen.range$DF[2]-phen.range$DF[1])/500, value = c(phen.range$DF[1],phen.range$DF[2])),# min = -0.5, max = 1, step = 0.01, value = c(0.5, 1)),
           sliderInput("phen.mid.color", label = h5("Value of mid color"),
                       min = phen.range$DF[1], max = phen.range$DF[2], step = (phen.range$DF[2]-phen.range$DF[1])/500, value = (phen.range$DF[1] + phen.range$DF[2])/2),
           sliderInput("point.size", label = h5("Size"),
                       min = 0, max = 3, step = 0.1, value = 1),
           sliderInput("point.alpha", label = h5("Alpha"),
                       min = 0, max = 1, step = 0.1, value = 0.5),
           uiOutput("cluster_phase_exp")
           # verbatimTextOutput("info")
           )
    }else{
      list(
           sliderInput("point.size", label = h5("Size"),
                       min = 0, max = 2, step = 0.1, value = 1),
           sliderInput("point.alpha", label = h5("Alpha"),
                       min = 0, max = 1, step = 0.1, value = 0.5)#,
          # verbatimTextOutput("info"))
      )

    }

  })

  output$tsne_ml_ui <- renderUI({
    if(!is.null(ml.model.selected$tsne)){
      checkboxInput("tsne_ml", h5("ML model-specific tSNE"))
    }

  })


  output$cluster_phase_exp <- renderUI({
    if(!is.null(hclust$locImp.row.cut)){
      list(checkboxInput("show_clusters_phase_exp", h5("LVI clusters")),
           uiOutput("cluster_select_input_ui")
      )
    }
  })

  output$cluster_select_input_ui<- renderUI({
    #if(input$show_clusters_phase_exp){
      list(
        selectInput("cluster_select_input", h5("Choose cluster(s)"),choices = c(1:input$num_clust),selected = c(1:input$num_clust)   ,multiple = TRUE)
        #actionButton("cluster_apply_tsne", h5("Apply!"))
      )
    #}

  })



  output$t_SNE <- renderPlot({

    #updating global variable

    if(!is.null(prm.sets.selected$tsne) ){

      if(isolate(input$load_phenotype) == "None"){
        ggdata <<- data.frame(prm.sets.selected$tsne, stringsAsFactors = F)
        names(ggdata) <<- c("pkey", "tSNE1","tSNE2")
        p1 = ggplot2::ggplot(ggdata, ggplot2::aes_string(x = "tSNE1", y = "tSNE2"))#, color = input$load_phenotype))
        p1 = p1+ggplot2::geom_point(size = input$point.size, alpha = input$point.alpha, color = "black") +# +scale_colour_gradient2(low="blue", high="red", midpoint = 0.4) + #labs(title =paste0( "tsne for original data"))  +
          ggplot2::xlim(-30,30) +ggplot2::ylim(-30,30) +
          ggplot2::coord_cartesian(xlim = tsne_range$x, ylim = tsne_range$y, expand = FALSE) +
          ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
        p1

      } else {

        if(!is.null(ml.model.selected$tsne)){

          #model specific
          if(input$tsne_ml){
            temp.pkey <- intersect(ml.model.selected$tsne$pkey, phenotype.values.selected$DF$pkey)
            #all.equal(as.character(prm.sets.selected$tsne$pkey[prm.sets.selected$tsne$pkey%in%temp.pkey]), phenotype.values.selected$DF$pkey[phenotype.values.selected$DF$pkey%in%temp.pkey],check.attributes = F)
            temp.tsne <-ml.model.selected$tsne
            temp.phen.values <- phenotype.values.selected$DF

            temp.tsne <- temp.tsne[temp.tsne$pkey %in%temp.pkey,  ]
            temp.tsne <- temp.tsne[order(temp.tsne$pkey),  ]
            temp.phen.values <- temp.phen.values[temp.phen.values$pkey %in% temp.pkey, ]
            temp.phen.values <- temp.phen.values[order(temp.phen.values$pkey), ]
          }else{
            temp.pkey <- intersect(prm.sets.selected$tsne$pkey, phenotype.values.selected$DF$pkey)
            all.equal(as.character(prm.sets.selected$tsne$pkey[prm.sets.selected$tsne$pkey%in%temp.pkey]), phenotype.values.selected$DF$pkey[phenotype.values.selected$DF$pkey%in%temp.pkey],check.attributes = F)
            temp.tsne <-prm.sets.selected$tsne
            temp.phen.values <- phenotype.values.selected$DF

            temp.tsne <- temp.tsne[temp.tsne$pkey %in%temp.pkey,  ]
            temp.tsne <- temp.tsne[order(temp.tsne$pkey),  ]
            temp.phen.values <- temp.phen.values[temp.phen.values$pkey %in% temp.pkey, ]
            temp.phen.values <- temp.phen.values[order(temp.phen.values$pkey), ]
          }
        }else{
          temp.pkey <- intersect(prm.sets.selected$tsne$pkey, phenotype.values.selected$DF$pkey)
          all.equal(as.character(prm.sets.selected$tsne$pkey[prm.sets.selected$tsne$pkey%in%temp.pkey]), phenotype.values.selected$DF$pkey[phenotype.values.selected$DF$pkey%in%temp.pkey],check.attributes = F)
          temp.tsne <-prm.sets.selected$tsne
          temp.phen.values <- phenotype.values.selected$DF

          temp.tsne <- temp.tsne[temp.tsne$pkey %in%temp.pkey,  ]
          temp.tsne <- temp.tsne[order(temp.tsne$pkey),  ]
          temp.phen.values <- temp.phen.values[temp.phen.values$pkey %in% temp.pkey, ]
          temp.phen.values <- temp.phen.values[order(temp.phen.values$pkey), ]

        }


        ##show data only used in ML model trainig and test
        if(!is.null(ml.model.selected$ml.model) & input$within_ml.model == "All" ){
          phen.range$DF <- signif(range(temp.phen.values[,2], na.rm = T),1)
        }else if(!is.null(ml.model.selected$ml.model) & input$within_ml.model == "Training and test sets" ){
          if(ml.model.selected$train.data[1] == "whole"){
          }else{
            temp.tsne <- temp.tsne[temp.tsne$pkey %in% c(ml.model.selected$train.data,ml.model.selected$test.data),  ]
            temp.tsne <- temp.tsne[order(temp.tsne$pkey),  ]
            temp.phen.values <- temp.phen.values[temp.phen.values$pkey %in% c(ml.model.selected$train.data,ml.model.selected$test.data), ]
            temp.phen.values <- temp.phen.values[order(temp.phen.values$pkey), ]
          }
          phen.range$DF <- signif(range(temp.phen.values[,2], na.rm = T),1)
        }else if(!is.null(ml.model.selected$ml.model) & input$within_ml.model == "Training set" ){
          if(ml.model.selected$train.data[1] == "whole"){
          }else{
            temp.tsne <- temp.tsne[temp.tsne$pkey %in% ml.model.selected$train.data,  ]
            temp.tsne <- temp.tsne[order(temp.tsne$pkey),  ]
            temp.phen.values <- temp.phen.values[temp.phen.values$pkey %in% ml.model.selected$train.data, ]
            temp.phen.values <- temp.phen.values[order(temp.phen.values$pkey), ]
          }
          phen.range$DF <- signif(range(temp.phen.values[,2], na.rm = T),1)
        }

        ggdata <<- data.frame(temp.tsne, temp.phen.values[,isolate(input$load_phenotype)], stringsAsFactors = F)
        names(ggdata) <<- c("pkey", "tSNE1","tSNE2", isolate(input$load_phenotype))
        ggdata <<- ggdata[ggdata[[isolate(input$load_phenotype)]] >= input$phen.range[1] & ggdata[[isolate(input$load_phenotype)]] < input$phen.range[2] , ]

        input$show_clusters_phase_exp
        if(isolate(is.null(hclust$locImp.row.cut))){
          p1 = ggplot2::ggplot(ggdata, ggplot2::aes_string(x = "tSNE1", y = "tSNE2", color = isolate(input$load_phenotype)))
          p1 = p1+ggplot2::geom_point(size = input$point.size, alpha = input$point.alpha) +ggplot2::scale_colour_gradient2(low="blue", high="red", midpoint = input$phen.mid.color) + #labs(title =paste0( "tsne for original data"))  +
            #xlim(-30,30) +ylim(-30,30) +
            ggplot2::coord_cartesian(xlim = tsne_range$x, ylim = tsne_range$y, expand = FALSE) +
            ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
          p1
        }else{
          if(!input$show_clusters_phase_exp){
            p1 = ggplot2::ggplot(ggdata, ggplot2::aes_string(x = "tSNE1", y = "tSNE2", color = isolate(input$load_phenotype)))
            p1 = p1+ggplot2::geom_point(size = input$point.size, alpha = input$point.alpha) +ggplot2::scale_colour_gradient2(low="blue", high="red", midpoint = input$phen.mid.color) + #labs(title =paste0( "tsne for original data"))  +
              #xlim(-30,30) +ylim(-30,30) +
              ggplot2::coord_cartesian(xlim = tsne_range$x, ylim = tsne_range$y, expand = FALSE) +
              ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
            p1
          }else{


            ggdata <<- ggdata[ggdata$pkey %in% intersect(ggdata$pkey, row.names(brush.points$loc.imp)),]
            ggdata <<- ggdata[order(ggdata$pkey),]
            temp.clust <- isolate(hclust$locImp.row.cut[row.names(brush.points$loc.imp) %in% intersect(ggdata$pkey, row.names(brush.points$loc.imp))])
            ggdata$cluster <<- as.factor(temp.clust)
            rm(temp.clust)
            ggdata <<- ggdata[ggdata$cluster %in% input$cluster_select_input ,]
            color.gradient = colorRampPalette(c("blue", "green","yellow","red"))

            p1 = ggplot2::ggplot(ggdata, ggplot2::aes_string(x = "tSNE1", y = "tSNE2", color = "cluster"))
            p1 = p1+ggplot2::geom_point(size = input$point.size, alpha = input$point.alpha) + ggplot2::scale_colour_manual(values =color.gradient(isolate(input$num_clust))[as.numeric(input$cluster_select_input)[order(as.numeric(input$cluster_select_input))]] )+ #labs(title =paste0( "PNP t_SNE plot with var imp clusters")) + #labs(title =paste0( "tsne for original data"))  +
              #xlim(-30,30) +ylim(-30,30) +
              ggplot2::coord_cartesian(xlim = tsne_range$x, ylim = tsne_range$y, expand = FALSE) +
              ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
            p1

          }
        }


        }
    }
  })



  ##zoom-in
  ##brush and double click
  observeEvent(input$tsne_dblclick,{
    brush <- input$tsne_brush
    if(!is.null(brush)){
      tsne_range$x = c(brush$xmin,brush$xmax)
      tsne_range$y = c(brush$ymin,brush$ymax)
    }else{
       #tsne_range$x = c(-30,30)
       #tsne_range$y = c(-30,30)
      tsne_range$x <- 1.05*range(prm.sets.selected$tsne$tSNE1)
      tsne_range$y <- 1.05*range(prm.sets.selected$tsne$tSNE2)
    }
  })


  ##specifying parameter ranges
  # prm.z.ranges = matrix(NA, nrow = 2, ncol = numPrm$ml.model )
  # colnames(prm.z.ranges) = parameters$ml.model
  # for(i in 2:(numPrm$ml.model+1)){
  #   prm.z.ranges[1,i-1] = signif(min(data.clust.z.combined[,i]), 4)
  #   prm.z.ranges[2,i-1] = signif(max(data.clust.z.combined[,i]), 4)
  # }
  #
  #

  ##point-select
  selected = reactiveValues()



  observeEvent(input$tsne_click,{
    selected_temp = nearPoints(ggdata, input$tsne_click,  maxpoints = 1, xvar = "tSNE1", yvar = "tSNE2")
    selected_pkey = selected_temp$pkey


    if(nrow(selected_temp) !=0){
      selected$pkey = selected_pkey
      selected$point <- data.frame(Parameter = parameters$prmset,
                                   Rescaled = t(prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey == selected_pkey, -1]),
                                   Original = t(prm.sets.selected$original[prm.sets.selected$original$pkey == selected_pkey, -1]), stringsAsFactors = F)
      names(selected$point) <- c("parameter", "rescaled", "original")

      if(!is.null(ml.model.selected$custom.scale)){
        temp.prms <- c(intersect(parameters$ml.model, parameters$prmset), setdiff(parameters$ml.model, parameters$prmset))
        selected$point.custom.scale <- t(prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey == selected_pkey, -1])[intersect(parameters$ml.model, parameters$prmset),]
        if(any(ml.model.selected$custom.scale$values$pkey == selected_pkey)){
          selected$point.custom.scale <- as.numeric(append(selected$point.custom.scale, t(ml.model.selected$custom.scale$values[ml.model.selected$custom.scale$values$pkey == selected_pkey,])[ setdiff(parameters$ml.model, parameters$prmset),]  ))
          names(selected$point.custom.scale) <- temp.prms
        }else{
          temp.cs.func <- get.custom.scale.func.obj(PPM$object, ml.model.selected$custom.scale$name)
          selected$point.custom.scale  <- as.numeric(append(selected$point.custom.scale,
                                                t(temp.cs.func(prm.combs = prm.sets.selected$original[prm.sets.selected$original$pkey == selected_pkey,],
                                                               other.vals = ml.model.selected$custom.scale$other.vals,
                                                               cs.to.org = F))[ setdiff(parameters$ml.model, parameters$prmset),]
                                                ))
          names(selected$point.custom.scale) <- temp.prms

        }


      }else{
        selected$point.custom.scale <- NULL
      }

    }else {
      selected$pkey <- NULL
      selected$point <- NULL
      selected$point.custom.scale <- NULL
    }
  })

  observeEvent(input$tsne_manual_pt_select,{
    #selected_temp = nearPoints(ggdata, input$tsne_click,  maxpoints = 1)
    selected_pkey = input$tsne_manual_pt_select


    if(selected_pkey %in% prm.sets.selected$original$pkey){
      selected$pkey = selected_pkey
      selected$point <- data.frame(Parameter = parameters$prmset,
                                   Rescaled = t(prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey == selected_pkey, -1]),
                                   Original = t(prm.sets.selected$original[prm.sets.selected$original$pkey == selected_pkey, -1]), stringsAsFactors = F)
      names(selected$point) <- c("parameter", "rescaled", "original")

      if(!is.null(ml.model.selected$custom.scale)){
        temp.prms <- c(intersect(parameters$ml.model, parameters$prmset), setdiff(parameters$ml.model, parameters$prmset))
        selected$point.custom.scale <- t(prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey == selected_pkey, -1])[intersect(parameters$ml.model, parameters$prmset),]
        if(any(ml.model.selected$custom.scale$values$pkey == selected_pkey)){
          selected$point.custom.scale <- as.numeric(append(selected$point.custom.scale, t(ml.model.selected$custom.scale$values[ml.model.selected$custom.scale$values$pkey == selected_pkey,])[ setdiff(parameters$ml.model, parameters$prmset),]  ))
          names(selected$point.custom.scale) <- temp.prms
        }else{
          temp.cs.func <- get.custom.scale.func.obj(PPM$object, ml.model.selected$custom.scale$name)
          selected$point.custom.scale  <- as.numeric(append(selected$point.custom.scale,
                                                            t(temp.cs.func(prm.combs = prm.sets.selected$original[prm.sets.selected$original$pkey == selected_pkey,],
                                                                           other.vals = ml.model.selected$custom.scale$other.vals,
                                                                           cs.to.org = F))[ setdiff(parameters$ml.model, parameters$prmset),]
          ))
          names(selected$point.custom.scale) <- temp.prms

        }


      }else{
        selected$point.custom.scale <- NULL
      }

    }else {
      selected$pkey <- NULL
      selected$point <- NULL
      selected$point.custom.scale <- NULL
    }
  })
  # output$info <- renderPrint({
  #   #nearPoints(ggdata, input$tsne_click,  maxpoints = 1)$pkey
  #   cat(selected$pkey)
  # })

  ## output of selected point

  output$selected_point_ui <- renderUI({
    if(!is.null(selected$point)){
      list(h4("Selected point"),
           verbatimTextOutput("selected_pkey_exp_phase_tab"),
           wellPanel(id = "tPanel",
                     style = "overflow-x:scroll",
                     tableOutput("parmeter_values"))
           )
    }
  })


  ##Further Info for selected ML model
  output$further_info_ml_model_button_ui <- renderUI({
    if(!is.null(ml.model.selected$ml.model)){
      actionButton("further_info_ml_model_button",h5("Further info for selected ML model"))
    }
  })

  output$further_info_ml_model_ui <- renderUI({
    if(!is.null(input$further_info_ml_model_button)){
      if(input$further_info_ml_model_button[[1]]%%2 == 0  ){
        return()
      }else{
        shinydashboard::tabBox(id = "selection_ml_phase_exp", selected = NULL, width = 12, #type = "pills",
               tabPanel("Info of trained ML model", value = "tab_info_ml",
                        column(4,
                               plotOutput("performance_phase_exp")
                        ),
                        column(4,
                               plotOutput("varImp_phase_exp")
                        )

               ),
               tabPanel("Prediction performance", value = "tab_pred_perform",
                        column(4,
                               uiOutput("options_bias_pred_phase_exp_ui")
                        ),
                        column(8,
                               uiOutput("OOB_test_pred_phase_exp_ui")
                        )
               )
        )
      }
    }else{return()}
  })

  output$selected_pkey_exp_phase_tab <- renderText({
    selected$pkey
  })
  output$parmeter_values <- renderTable({
    if(!is.null(selected$point)){
      temp.point <- data.frame(parameter = c("Original", "Scaled"),
                               stringsAsFactors = F)
      temp.point <- cbind(temp.point,t(selected$point)[c(3,2),])
      names(temp.point)[-1] <- selected$point$parameter
      return( temp.point)

    }

  },
  align = "c", spacing = "xs"
  )

  ##Further Info for ML model detailed codes
  output$performance_phase_exp <- renderPlot({
    if(!is.null(ml.model.selected$ml.model)){
      if(ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression"){
        temp.main <- "Error rates for regression ML model"
      }else{
        temp.main <- "Error rates for classification ML model"
      }
      plot(ml.model.selected$ml.model, main = temp.main)
    }
  })

  output$varImp_phase_exp <- renderPlot({
    if(!is.null(ml.model.selected$ml.model)){
      randomForest::varImpPlot(ml.model.selected$ml.model,  main = paste0(ml.model.selected$name,": Global Variable Importance"), scale = F)
    }
  })


  output$options_bias_pred_phase_exp_ui <- renderUI({
    if(!is.null(ml.model.selected$ml.model)){
      if((ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression") & !is.null(ml.model.selected$ml.model.res)){
        list(column(6,checkboxInput("plot_bias_corr_phase_exp","Bias correction")),
             column(6, radioButtons("oob_test_phase_exp", label= NULL ,choices = c("OOB", "Test set"), inline = T )) ## for test sets
        )
      }else if((ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression") & is.null(ml.model.selected$ml.model.res)){
        column(6, radioButtons("oob_test_phase_exp", label= NULL ,choices = c("OOB", "Test set"), inline = T )) ## for test sets
      }else if(ml.model.selected$mode == "class" | ml.model.selected$mode == "classification"){
        list(
          fluidRow(
            column(6, radioButtons("oob_test_phase_exp", label= h5("Prediction for") ,choices = c(OOB = "OOB", 'Test set' = "test.set"), inline = FALSE )),## for test sets
            # column(6, selectInput("confusion_roc", label = h5("Choose display mode"), choices = c("Text", "Plot") ))
            column(6, selectInput("positive_class_phase_exp", label = h5("Positive class"), choices = ml.model.selected$class.def$info$Class))
          ),
          fluidRow(
            column(6, sliderInput("pred_cut_off_phase_exp", h5("Cut-off"), min = 0, max = 1, step = 0.01, value = 0.5))
          ),
          fluidRow(
            column(6,style='padding-left:0px;',
                   h5("Confusion matrix"),
                   tableOutput("conf_mat_phase_exp")),
            column(5,offset = 1,
                   tableOutput("conf_stats_phase_exp"))
          )
        )
      }
    }
  })


  output$conf_mat_phase_exp<- renderTable({

    temp.table  <-  ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$conf.mat[[(input$pred_cut_off_phase_exp*100+1)]]$table
    temp.df <- data.frame(pred_ref = row.names( temp.table),  temp.table[,1],  temp.table[,2])
    row.names(temp.df) <- NULL
    colnames(temp.df)[2:3] <- row.names( temp.table)
    return(temp.df)
  },rownames = F,align = "c")

  output$conf_stats_phase_exp<- renderTable({
    temp.conf.mat <- ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$conf.mat[[(input$pred_cut_off_phase_exp*100+1)]]
    temp.df <- temp.conf.mat$byClass[1:4]
    temp.df <- append( temp.df, c(temp.conf.mat$overall[1], temp.conf.mat$byClass[11]))
    temp.df <- data.frame(temp.df)
    return(temp.df)
  },align = "c", colnames = F, rownames = T)



  output$OOB_test_pred_phase_exp_ui <-renderUI({
    if(!is.null(ml.model.selected$ml.model)){
      if(ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression"){
        if(input$oob_test_phase_exp == "OOB"){
          column(6,plotOutput("OOB_prediction_phase_exp"))
        }else if(input$oob_test_phase_exp != "OOB" & (!is.null(ml.model.selected$test.data) & length(ml.model.selected$test.data) != 0)){
          column(6,plotOutput("test_set_prediction_phase_exp"))
        }
      }else if (ml.model.selected$mode == "class" | ml.model.selected$mode == "classification"){
        list(
          column(6,plotOutput("prediction_roc_plot_class_phase_exp")),
          column(6,plotOutput("prediction_pr_plot_class_phase_exp"))
        )
      }
    }
  })


  output$prediction_roc_plot_class_phase_exp <- renderPlot({
    plot(ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$roc,  type= "b", main = "ROC", xlab = "False Positive", ylab = "True Positive", col= colfunc(101), pch = 19, xlim = c(0,1), ylim=c(0,1))
    points(t(ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$roc[(input$pred_cut_off_phase_exp*100+1),1:2]))
  })

  output$prediction_pr_plot_class_phase_exp <- renderPlot({
    plot(ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$prec.recall, type= "b", main = "Precision-Recall", xlab = "Recall", ylab = "Precision", col= colfunc(101), pch = 19, xlim = c(0,1), ylim=c(0,1))
    points(t(ml.model.selected$class.def$pred.perform[[input$oob_test_phase_exp]][[input$positive_class_phase_exp]]$prec.recall[(input$pred_cut_off_phase_exp*100+1),1:2]))
  })








  ##regression
  output$OOB_prediction_phase_exp <- renderPlot({
    if(is.null( ml.model.selected$ml.model) & is.null( ml.model.selected$ml.model.res) ){
      return()
    }else if(!is.null( ml.model.selected$ml.model) & is.null( ml.model.selected$ml.model.res) ){
      df <- data.frame(x = ml.model.selected$ml.model$y, y = ml.model.selected$ml.model$predicted )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.selected$ml.model$y, ml.model.selected$ml.model$predicted ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() +ggplot2:: xlab("Simulated      ") + ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.selected$ml.model) & !is.null( ml.model.selected$ml.model.res) & input$plot_bias_corr_phase_exp == FALSE){
      df <- data.frame(x = ml.model.selected$ml.model$y, y = ml.model.selected$ml.model$predicted )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.selected$ml.model$y, ml.model.selected$ml.model$predicted ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.selected$ml.model) & input$plot_bias_corr_phase_exp == TRUE){
      df <- data.frame(x = ml.model.selected$ml.model$y, y = ml.model.selected$ml.model$predicted -ml.model.selected$ml.model.res$predicted)
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("OOB Pred VS Sim: corr = ", signif(cor( ml.model.selected$ml.model$y, ml.model.selected$ml.model$predicted -ml.model.selected$ml.model.res$predicted ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("OOB Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }
  })

  output$test_set_prediction_phase_exp <- renderPlot({
    input$oob_test_phase_exp
    isolate({
      ##load selected parameter sets
      temp.prm.ranges.names <- get.prm.ranges.name(object = PPM$object)
      temp.prm.names.init <- get.init.prm.combs.name(object = PPM$object)
      temp.prm.names.addit <- get.addit.prm.combs.name(object = PPM$object)
      prm.sets.test.original <- data.frame(stringsAsFactors = F)
      prm.sets.test.rescaled <- data.frame(stringsAsFactors = F)

      for(i in 1:length(ml.model.selected$prm.sets.used)){
        ##to obtain corresponding parameter ranges for selected initial parameter space
        if(any(unlist( temp.prm.names.init) ==ml.model.selected$prm.sets.used[i]) ){
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.names = temp.prm.names.init, input.name = ml.model.selected$prm.sets.used[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.prm.combs <- get.init.prm.combs(PPM$object,ml.model.selected$prm.sets.used[i], temp.range.name )


          prm.sets.test.original  <-rbind( prm.sets.test.original , temp.prm.combs$prm.combs )
          prm.sets.test.rescaled <-rbind(prm.sets.test.rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL

        }else{
          ##to obtain corresponding parameter ranges and inital parameter set for selected additional parameter space
          temp.idx <- unlist(
            apply(matrix(temp.prm.ranges.names), 1,
                  function(name, prm.names, input.name){
                    any(unlist(prm.names[[name]]) == input.name) },
                  prm.names = temp.prm.names.addit, input.name = ml.model.selected$prm.sets.used[i])
          )
          temp.range.name <- temp.prm.ranges.names[temp.idx]
          temp.idx <- unlist(
            apply(matrix(unlist(temp.prm.names.init[[temp.range.name]])), 1,
                  function(name, prm.range.name, prm.names, input.name){
                    any(prm.names[[name]] == input.name) },
                  prm.range.name = temp.range.name, prm.names = temp.prm.names.addit[[temp.range.name]], input.name = ml.model.selected$prm.sets.used[i])
          )
          temp.prm.name.init <- unlist(temp.prm.names.init[[temp.range.name]])[temp.idx]

          temp.prm.combs <- get.addit.prm.combs(PPM$object,ml.model.selected$prm.sets.used[i], temp.range.name,temp.prm.name.init )
          temp.prm.combs.init <- get.init.prm.combs(PPM$object, temp.prm.name.init, temp.range.name )


          prm.sets.test.original  <-rbind( prm.sets.test.original , temp.prm.combs$prm.combs )
          names(temp.prm.combs$prm.combs.z ) <- names(temp.prm.combs$prm.combs)
          prm.sets.test.rescaled <-rbind( prm.sets.test.rescaled, temp.prm.combs$prm.combs.z )
          temp.prm.combs <- NULL
          temp.prm.combs.init <- NULL

        }
      }
      prm.sets.test.original<- prm.sets.test.original[order(prm.sets.test.original$pkey),]
      prm.sets.test.rescaled <- prm.sets.test.rescaled[order(prm.sets.test.rescaled$pkey),]

      prm.sets.test.original<- prm.sets.test.original[prm.sets.test.original$pkey %in% ml.model.selected$test.data,]
      prm.sets.test.rescaled <- prm.sets.test.rescaled[prm.sets.test.rescaled$pkey  %in% ml.model.selected$test.data ,]

      ##load phenotype values
      phenotype.values.test <-data.frame(stringsAsFactors = F)
      phenotype.loaded.ml.list <- get.phenotypes(PPM$object, ml.model.selected$phenotype)
      for(temp.name in ml.model.selected$prm.sets.used){
        phenotype.values.test <- rbind(phenotype.values.test, phenotype.loaded.ml.list[[temp.name]])
      }
      phenotype.values.test$pkey <- as.character(phenotype.values.test$pkey)
      phenotype.values.test <- phenotype.values.test[order(phenotype.values.test$pkey),]
      phenotype.values.test <- phenotype.values.test[phenotype.values.test$pkey %in% ml.model.selected$test.data,]

      if(!is.null(ml.model.selected$custom.scale)){
        temp.custom.scale <- ml.model.selected$custom.scale$values[ ml.model.selected$custom.scale$values$pkey %in%  ml.model.selected$test.data,]
        temp.custom.scale <- temp.custom.scale[order(temp.custom.scale$pkey),]
        prm.sets.test.rescaled <- cbind(prm.sets.test.rescaled, temp.custom.scale[,names(ml.model.selected$custom.scale$parameters)])
      }

    })

    if(is.null( ml.model.selected$ml.model) & is.null( ml.model.selected$ml.model.res) ){
      return()
    }else if(!is.null( ml.model.selected$ml.model) & is.null( ml.model.selected$ml.model.res) ){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.selected$ml.model,prm.sets.test.rescaled[,-1]))
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("Test set Pred VS Sim: corr = ", signif(cor( df$x, df$y,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("Test Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.selected$ml.model) & !is.null( ml.model.selected$ml.model.res) & input$plot_bias_corr_phase_exp == FALSE){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.selected$ml.model,prm.sets.test.rescaled[,-1]) )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("Test set VS Sim: corr = ", signif(cor( df$x, df$y ,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("Test Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }else if(!is.null( ml.model.selected$ml.model) & input$plot_bias_corr_phase_exp == TRUE){
      df <- data.frame(x = phenotype.values.test[,-1], y = predict(ml.model.selected$ml.model,prm.sets.test.rescaled[,-1]) -  predict(ml.model.selected$ml.model.res,prm.sets.test.rescaled[,-1]) )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) +ggplot2::ylim(min(ml.model.selected$ml.model$y),max(ml.model.selected$ml.model$y)) + ggplot2::ggtitle(paste0("Test set VS Sim: corr = ", signif(cor(df$x, df$y,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") + ggplot2::ylab("Test Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }
  })


  ######



  ##automatic generation of sliders
  output$parameters <- renderUI({
    if(!is.null(numPrm$ml.model)){
      prmInput = vector("list", numPrm$ml.model)
      for(i in 1:numPrm$ml.model){
        prmInput[[i]] <- list(sliderInput(paste0("prm",i),
                                          parameters$ml.model[i],
                                          min = signif(min(prm.sets.selected$rescaled[,2:(numPrm$ml.model+1)]),3),
                                          max = signif(max(prm.sets.selected$rescaled[,2:(numPrm$ml.model+1)]),3),
                                          value = 0, step =.01))
      }
      return(prmInput)
    }
  })


  #Variable importance
  library(gridExtra)
  output$global_varImp <- renderPlot({
    if(!is.null( ml.model.selected$ml.model)){
      imp = randomForest::importance( ml.model.selected$ml.model, scale = F)
      ggdata.imp = data.frame( imp)

      if(ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression"){
        ggdata.imp$names = factor(row.names(imp), levels=row.names(ggdata.imp)[order(ggdata.imp$X.IncMSE)])
        p1 = ggplot2::ggplot(data=ggdata.imp[order(ggdata.imp$X.IncMSE, decreasing = T),], ggplot2::aes(x =names , y =  X.IncMSE))
        p1 = p1+ggplot2::geom_point(size = 2, color= "black", stat = "identity") + ggplot2::xlab("") + ggplot2::ylab("")+ ggplot2::labs(title =paste0( "Permutation")) +
          ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=10, face = "bold"), axis.title=ggplot2::element_text(size=10,face="bold")) +ggplot2::coord_flip()
      }else{
        ggdata.imp$names = factor(row.names(imp), levels=row.names(ggdata.imp)[order(ggdata.imp$MeanDecreaseAccuracy)])
        p1 = ggplot2::ggplot(data=ggdata.imp[order(ggdata.imp$MeanDecreaseAccuracy, decreasing = T),], ggplot2::aes(x =names , y =  MeanDecreaseAccuracy))
        p1 = p1+ggplot2::geom_point(size = 2, color= "black", stat = "identity") + ggplot2::xlab("") + ggplot2::ylab("")+ ggplot2::labs(title =paste0( "Permutation")) +
          ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=10, face = "bold"), axis.title=ggplot2::element_text(size=10,face="bold")) +ggplot2::coord_flip()
      }

      ggdata.imp = data.frame( imp)
      if(ml.model.selected$mode == "reg" | ml.model.selected$mode == "regression"){
        ggdata.imp$names = factor(row.names(imp), levels=row.names(ggdata.imp)[order(ggdata.imp$IncNodePurity)])
        p2 = ggplot2::ggplot(data=ggdata.imp[order(ggdata.imp$IncNodePurity, decreasing = T),], ggplot2::aes(x =names , y =IncNodePurity ))
        p2 = p2+ ggplot2::geom_point(size = 2, color= "black", stat = "identity")+ ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::labs(title =paste0( "Gini")) +
          ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=10, face = "bold"), axis.title=ggplot2::element_text(size=10,face="bold")) +ggplot2::coord_flip()
      }else{
        ggdata.imp$names = factor(row.names(imp), levels=row.names(ggdata.imp)[order(ggdata.imp$MeanDecreaseGini)])
        p2 = ggplot2::ggplot(data=ggdata.imp[order(ggdata.imp$MeanDecreaseGini, decreasing = T),], ggplot2::aes(x =names , y = MeanDecreaseGini ))
        p2 = p2+ggplot2::geom_point(size = 2, color= "black", stat = "identity")+ ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::labs(title =paste0( "Gini")) +
          ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=10, face = "bold"), axis.title=ggplot2::element_text(size=10,face="bold")) +ggplot2::coord_flip()
      }
      gridExtra::grid.arrange(p1,p2, ncol = 2, nrow =1)
    }

  })

  #local.imp.combined = data.frame(pkey = data.clust.z.combined$pkey, t(rf.two.gene.combined.bc$localImportance))

  output$local_varImp <- renderPlot({
    if(!is.null(selected$pkey) && !is.null(ml.model.selected$ml.model)){
      #rf.two.gene.combined$localImportance[order(rf.two.gene.combined$localImportance[,1027], decreasing = TRUE),1027]
      idx.pt = which(local.importance$DF$pkey == selected$pkey)
      if(length(idx.pt) != 0){
        ggdata.imp = data.frame( imp = as.numeric(t(local.importance$DF[idx.pt ,-1])))
        ggdata.imp$names = colnames(local.importance$DF)[-1]
        ggdata.imp = ggdata.imp[order(ggdata.imp$imp),]
        ggdata.imp$names = factor(ggdata.imp$names, levels=ggdata.imp$names)
        p1 = ggplot2::ggplot(data=ggdata.imp, ggplot2::aes(x =names , y =  imp))
        p1 = p1+ggplot2::geom_point(size = 2, color= "black", stat = "identity") + ggplot2::xlab("Parameters") + ggplot2::ylab("Avg. increase in squared OOB residuals")+ ggplot2::labs(title = selected$pkey) +
          ggplot2::theme_bw() + ggplot2::theme(axis.text=ggplot2::element_text(size=10, face = "bold"), axis.title=ggplot2::element_text(size=10,face="bold")) + ggplot2::coord_flip()
        p1
      }else{
        return(0)
      }
    }
  })



  #perturbation

  #validation parameter combinations
  prm.combs.val.z <- reactiveValues()
  prm.combs.val <- reactiveValues()



  output$parameter_choice1 <- renderUI({
    selectInput(inputId = "parameter_choice1",
                label = "Choose the first parameter:",
                choices = parameters$ml.model, selected = parameters$ml.model[1])
  })

  output$parameter_choice2 <- renderUI({
    selectInput(inputId = "parameter_choice2",
                label = "Choose the second parameter:",
                choices = parameters$ml.model, selected = parameters$ml.model[1])
  })

  output$plot_range_ui <- renderUI({
    if(!is.null(ml.model.selected$ml.model)){
      if(ml.model.selected$ml.model$type == "regression"){
        list(
          sliderInput("plot_range", label = h5("Plot range"),
                      min = phen.range$DF[1], max = phen.range$DF[2], step = (phen.range$DF[2]-phen.range$DF[1])/500, value = c(phen.range$DF[1],phen.range$DF[2]))
        )
      }else{
        list(
          selectInput("pred_type_exp", label = h5("Prediction type"), choices = c("Probability", "Binary")),
          uiOutput("cut_off_exp_ui"),
          selectInput("posit_class_exp", label = h5("Positive class"), choices = ml.model.selected$class.def$info$Class)
        )
      }
    }
  })

  output$cut_off_exp_ui <- renderUI({
    if(!is.null(input$pred_type_exp) & input$pred_type_exp == "Binary"){
      sliderInput("cut_off_exp", label = h5("Cut-off"),
                  min =0, max = 1, step = 0.01, value = 0.5)
    }

  })



  ##generate a plot only when a point is selected and two selected parameters are different.
  observeEvent(input$plot_gen,{
    if(!is.null(ml.model.selected$ml.model) & is.null(ml.model.selected$custom.scale)){
      selected$point.perturbed <- t(selected$point[parameters$ml.model,"rescaled"])
      colnames( selected$point.perturbed) <- parameters$ml.model
    }else if(!is.null(ml.model.selected$ml.model) & !is.null(ml.model.selected$custom.scale)){
      selected$point.perturbed <- t(selected$point.custom.scale[parameters$ml.model])
      colnames( selected$point.perturbed) <- parameters$ml.model
    }else if(is.null(ml.model.selected$ml.model)){
      selected$point.perturbed <- NULL
    }

  })

  output$perturb_plot <- renderUI({
    input$plot_gen
    isolate(if( !is.null(ml.model.selected$ml.model)){
      if(input$plot_type == "2D"){
        return(plotOutput("perturb_plot_2d",height = "450px", width = "450px"))
      } else if(input$plot_type == "3D"){
        return(rglwidgetOutput("perturb_plot_3d",height = "450px", width = "450px"))
      }
    })
  })

  output$perturb_plot_2d <- renderPlot({
    input$plot_gen
    isolate(if( !is.null(ml.model.selected$ml.model) && input$plot_type == "2D"){
      if(is.null(input$parameter_choice1) | is.null(input$parameter_choice2)  ){
        return()
      }else if((input$parameter_choice1 != input$parameter_choice2) & ((nrow(nearPoints(ggdata, input$tsne_click,  maxpoints = 1, xvar = "tSNE1", yvar = "tSNE2")) != 0) | !is.null(selected$pkey))){



        prm.combs.val.z$DF <- vec.plot.bc.mod( ml.model.selected$ml.model, ml.model.selected$ml.model.res, selected$point.perturbed,
                                              c(input$parameter_choice1, input$parameter_choice2),prm.ranges.z.selected$DF[,parameters$ml.model], grid.lines = input$num_grids_val, zoom = 1, zlim = c(input$plot_range[1],input$plot_range[2]), gap = 0 , three.dim = F, posit.class = input$posit_class_exp,
                                              pred.type = input$pred_type_exp, cut.off = input$cut_off_exp)
        prm.combs.val.z$pkey = selected$pkey
        prm.combs.val.z$prm.comb.selected.rescaled = selected$point$rescaled
        prm.combs.val.z$prm.comb.selected.original = selected$point$original
        names(prm.combs.val.z$prm.comb.selected.rescaled) <-  selected$point$parameter
        names(prm.combs.val.z$prm.comb.selected.original) <-  selected$point$parameter
        #print( selected$point$original )
      }


    })

  })

  output$perturb_plot_3d <- rgl::renderRglwidget({

    input$plot_gen
    isolate({if( !is.null(ml.model.selected$ml.model) && input$plot_type == "3D"){
      if(is.null(input$parameter_choice1) |is.null(input$parameter_choice2)  ){
        return()
      }else if((input$parameter_choice1 != input$parameter_choice2) & ((nrow(nearPoints(ggdata, input$tsne_click,  maxpoints = 1, xvar = "tSNE1", yvar = "tSNE2")) != 0) | !is.null(selected$pkey))){
        try(rgl.close(), silent = TRUE)
        prm.combs.val.z$DF <-  vec.plot.bc.mod( ml.model.selected$ml.model, ml.model.selected$ml.model.res,selected$point.perturbed,
                                               c(input$parameter_choice1, input$parameter_choice2),prm.ranges.z.selected$DF[,parameters$ml.model], grid.lines =input$num_grids_val, zoom = 1, zlim =  c(input$plot_range[1],input$plot_range[2]), gap = 0 , three.dim = T, posit.class = input$posit_class_exp,
                                               pred.type = input$pred_type_exp, cut.off = input$cut_off_exp)
        prm.combs.val.z$pkey = selected$pkey

        prm.combs.val.z$prm.comb.selected.rescaled = selected$point$rescaled
        prm.combs.val.z$prm.comb.selected.original = selected$point$original
        names(prm.combs.val.z$prm.comb.selected.rescaled) <-  selected$point$parameter
        names(prm.combs.val.z$prm.comb.selected.original) <-  selected$point$parameter

        scene1<- scene3d()
        rglwidget(scene1)

      }
    }})
  })


  output$gen_prm_combs_val_ui <- renderUI(
    if(!is.null(prm.combs.val.z$DF)){
      list(actionButton("gen_prm_combs_val", "Generate validation parameter combinations"),
           fluidRow(
             column(4,dateInput("pkey_date_val", label = h6("Current date"), format = "mmddyyyy")),
             column(4,textInput("pkey_digits_val", label = h6("Starting digit"))),
             #column(4,numericInput("num_grids_val", label = h6("Number of grids"),value = 30, min = 2, max = 100)),
             DT::dataTableOutput("prm_combs_val"),
             downloadButton("save_prm_combs_val", "Save validation parameter combinations")
           ))

    }
  )

  observeEvent(input$gen_prm_combs_val,{


    ##1.generate parameter keys (convention: date + "_" + 8 digit LETTER barcodes)
    let.to.num = c(0:25)
    names(let.to.num) = LETTERS
    p.index = as.numeric(let.to.num[strsplit(input$pkey_digits_val,"")[[1]]])
    temp.date = input$pkey_date_val
    temp.date = format(temp.date, "%m%d%Y")
    temp.date = as.character(temp.date)
    temp.pkey = gen_prm_keys(input$num_grids_val^2,  temp.date, p.index, nchar(input$pkey_digits_val))

    temp.pkey$pkey = append(prm.combs.val.z$pkey ,temp.pkey$pkey) ## adding the pkey of the selcted parameter combination
    temp.chosen.prms = c(input$parameter_choice1, input$parameter_choice2)
    temp.prms.perturb <- setdiff(c(input$parameter_choice1, input$parameter_choice2), names(ml.model.selected$custom.scale$parameters))

    ##2.generate validation parameter combinations
    #adding the selected parameter combination
    #temp.prm.combs.val.z = rbind( t(selected$point[,-1])["rescaled",parameters$ml.model], prm.combs.val.z$DF)

    temp.prm.combs.val.z = rbind( selected$point.perturbed , prm.combs.val.z$DF)

    ##whether the values of parameters are negative
    temp.neg <- t(selected$point[,-1])["original",] < 0

    ##negative prms -> multiply by -1
    for(prm.choice  in temp.prms.perturb){
      if(temp.neg[prm.choice]){
        temp.prm.combs.val.z[,prm.choice] <- -1*temp.prm.combs.val.z[,prm.choice]
      }else{}
    }

    # if(temp.neg[input$parameter_choice2]){
    #   temp.prm.combs.val.z[,input$parameter_choice2] <- -1*temp.prm.combs.val.z[,input$parameter_choice2]
    # }else{}

    temp.prm.combs.val<- data.frame(t(replicate(input$num_grids_val^2, t(selected$point[,-1])["original",])))

    if(prm.set.method$method =="unif_grid"){
      row.names(prm.ranges.selected$DF) <- prm.ranges.selected$DF$names
      row.names(prm.grids.selected$DF) <- prm.grids.selected$DF$names

      if(length(temp.prms.perturb) != 0){
        temp.prm.combs.val[,temp.prms.perturb] <-
          fun.scale.conv(sample_meth = prm.set.method$method,
                         prm.ranges = prm.ranges.selected$DF[temp.prms.perturb,],
                         prm.grids =  prm.grids.selected$DF[temp.prms.perturb,],
                         prm.combs = temp.prm.combs.val.z[-1,temp.prms.perturb],
                         z.to.org = TRUE)
      }

    }else{
      row.names(prm.ranges.selected$DF) <- prm.ranges.selected$DF$names
      if(length(temp.prms.perturb) != 0){
        temp.prm.combs.val[,temp.prms.perturb] <-
          fun.scale.conv(sample_meth = prm.set.method$method,
                         prm.ranges = prm.ranges.selected$DF[temp.prms.perturb,],
                         raw.smpl = prm.sets.selected$raw.smpl[,c("pkey",temp.prms.perturb)],
                         prm.combs = temp.prm.combs.val.z[-1,temp.prms.perturb],
                         z.to.org = TRUE)
      }
    }


    #custom to original
    if(!is.null(ml.model.selected$custom.scale)){
      temp.prm.combs.val <- cbind(temp.prm.combs.val,prm.combs.val.z$DF[, names(ml.model.selected$custom.scale$parameters)])
      temp.prm.combs.val$pkey <- NA
      temp.cs.func <- get.custom.scale.func.obj(PPM$object, ml.model.selected$custom.scale$name)
      temp.prm.combs.val[,ml.model.selected$custom.scale$parameters] <-  temp.cs.func(temp.prm.combs.val, other.vals = ml.model.selected$custom.scale$other.vals, cs.to.org = TRUE )[, ml.model.selected$custom.scale$parameters]
      temp.prm.combs.val <- temp.prm.combs.val[,parameters$prmset]
    }



    ##negative prms -> multiply by -1 again to turn back to negative sign.
    for(prm.choice  in temp.prms.perturb){
      if(temp.neg[prm.choice]){
        temp.prm.combs.val.z[,prm.choice] <- -1*temp.prm.combs.val.z[,prm.choice]
        temp.prm.combs.val[,prm.choice] <- -1* temp.prm.combs.val[,prm.choice]
      }else{}
    }
    # if(temp.neg[input$parameter_choice1]){
    #   temp.prm.combs.val.z[,input$parameter_choice1] <- -1*temp.prm.combs.val.z[,input$parameter_choice1]
    #   temp.prm.combs.val[,input$parameter_choice1] <- -1* temp.prm.combs.val[,input$parameter_choice1]
    # }else{}
    #
    # if(temp.neg[input$parameter_choice2]){
    #   temp.prm.combs.val.z[,input$parameter_choice2] <- -1*temp.prm.combs.val.z[,input$parameter_choice2]
    #   temp.prm.combs.val[,input$parameter_choice2] <- -1* temp.prm.combs.val[,input$parameter_choice2]
    # }else{}

    temp.prm.combs.val  = rbind( t(selected$point[,-1])["original",],  temp.prm.combs.val )
    prm.combs.val$DF  <- data.frame(pkey = temp.pkey$pkey, temp.prm.combs.val, stringsAsFactors = F)
    prm.combs.val$DF[,-1] <- signif(prm.combs.val$DF[,-1], digits = 6) ## as in "~/Dropbox/Codes/project_sim_ml/analysis/visualization/vec.plot.R"

    prm.combs.val$DF <-  rbind( prm.combs.val$DF[1,],prm.combs.val$DF)
    prm.combs.val$DF[1,] = NA
    prm.combs.val$DF[1,1] = "perturbed_prms"

    #for custom.scaled parameters, not yet resolved


    if(any(temp.prms.perturb == input$parameter_choice1)){
      prm.combs.val$DF[1,input$parameter_choice1] = 1
    }else{
      prm.combs.val$DF[1,ml.model.selected$custom.scale$parameters[input$parameter_choice1]] =1
    }

    if(any(temp.prms.perturb == input$parameter_choice2)){
      prm.combs.val$DF[1,input$parameter_choice2] = 2
    }else{
      prm.combs.val$DF[1,ml.model.selected$custom.scale$parameters[input$parameter_choice2]] =2
    }


    # prm.combs.val$DF[1,input$parameter_choice1] = 1
    # prm.combs.val$DF[1,input$parameter_choice2] = 2

  })


  # shinyFileSave(input, "save_prm_combs", roots = c("roots"= "~/"))
  # observe({
  #   # if(!is.null(prm.combinations$DF)){
  #   #
  #   # }
  #   print(input$save_prm_combs)
  #   file.info = parseSavePath(c("roots"= "~/"), input$save_prm_combs)
  #   print(file.info)
  #   if(nrow(file.info) > 0){
  #     if(file.info$type == "text"){
  #       isolate({
  #         write.table( prm.combinations$DF,file = as.character(file.info$datapath) ,quote = FALSE, col.names = TRUE, row.names = FALSE)})
  #     }else if (file.info$type == "csv"){
  #       isolate({
  #         write.csv(prm.combinations$DF,file = as.character(file.info$datapath) ,quote = FALSE, row.names = FALSE)})
  #     }
  #   }
  # })
  #
  output$prm_combs_val <- renderDataTable({
    input$gen_prm_combs_val
    if(!is.null(prm.combs.val$DF )){
      isolate({
        prm.combs.val$DF[3:nrow(prm.combs.val$DF),]
      })
    }


  })


  output$save_prm_combs_val <- downloadHandler(
    filename = function() {
      paste0(selected$pkey, "_",input$parameter_choice1, "_", input$parameter_choice2,  ".txt")
    },
    content = function(file) {
      write.table(prm.combs.val$DF, file, row.names = FALSE,quote =FALSE)
    }

  )


  ##Generate validation plots
  prms.combs.val.sim <-reactiveValues()


  output$gen_validation_ui <- renderUI({
    if(!is.null(phen.range$DF)){
      list(
        column(
          selectInput(inputId = "plot_type_val",
                      label = "Plot type:",
                      choices = c("2D", "3D")),
          actionButton("gen_val_plots", "Generate validation plots"),
          width = 3),
        column(
          sliderInput("plot_range_val", label = h5("Plot range"),
                      min = phen.range$DF[1],
                      max = phen.range$DF[2],
                      step = (phen.range$DF[2]-phen.range$DF[1])/500,
                      value = c(phen.range$DF[1],phen.range$DF[2])
          ),
          width = 3)
      )
    }
  })


  ## implement for retaining prm.val.z from generation.
  observeEvent(input$gen_val_plots,{
    if(!is.null(input$file_validation)){
      prms.combs.val.sim$DF <- read.table(input$file_validation$datapath, header = T, stringsAsFactors = F)
    }





    if(!is.null( ml.model.selected$ml.model) && !is.null(prms.combs.val.sim$DF) ){
      prms.combs.val.sim$selected_pkey = prms.combs.val.sim$DF[2,1]

      idx.not.custom <- !(prm.ranges.selected$DF$names %in% ml.model.selected$custom.scale$parameters)
      prms.not.custom <- prm.ranges.selected$DF$names[idx.not.custom]


      ##whether the values of parameters are negative
      temp.neg <-prms.combs.val.sim$DF[2,prms.not.custom] < 0

      ##negative prms -> multiply by -1
      prms.combs.val.sim$DF[-1,prms.not.custom] <-
        apply( matrix((colnames(temp.neg)),nrow =1),2,
                 function(prm, temp.neg, DF) {
                   if(temp.neg[,prm]){
                     DF[,prm] <- -1* DF[,prm]
                   }else{
                     DF[,prm]
                   }},
                 temp.neg = temp.neg,
                 DF = prms.combs.val.sim$DF[-1,prms.not.custom])

      if(prm.set.method$method == "unif_grid" && is.null(ml.model.selected$custom.scale)){
        prms.combs.val.sim$DF_z = fun.scale.conv(sample_meth = prm.set.method$method,
                                                 prm.ranges = prm.ranges.selected$DF,
                                                 prm.grids = prm.grids.selected$DF,
                                                 prm.combs = prms.combs.val.sim$DF[-1,c(2:(numPrm$prmset+1))],
                                                 z.to.org = FALSE)

      }else if(prm.set.method$method == "unif_grid" && !is.null(ml.model.selected$custom.scale)){

        prms.combs.val.sim$DF_z = fun.scale.conv(sample_meth = prm.set.method$method,
                                                 prm.ranges = prm.ranges.selected$DF[idx.not.custom,],
                                                 prm.grids = prm.grids.selected$DF[idx.not.custom,],
                                                 prm.combs = prms.combs.val.sim$DF[-1,prms.not.custom],
                                                 z.to.org = FALSE)
        temp.custom.scale.func <- get.custom.scale.func.obj(object = PPM$object,name = ml.model.selected$custom.scale$name)
        prms.combs.val.sim$DF_z <- cbind(prms.combs.val.sim$DF_z,
                                         temp.custom.scale.func(prm.combs = prms.combs.val.sim$DF[-1,], other.vals = ml.model.selected$custom.scale$other.vals,cs.to.org = F)[,names(ml.model.selected$custom.scale$parameters)]
                                         )


      }else if(prm.set.method$method != "unif_grid" && is.null(ml.model.selected$custom.scale)) {
        prms.combs.val.sim$DF_z = fun.scale.conv(sample_meth = prm.set.method$method,
                                                 prm.ranges = prm.ranges.selected$DF,
                                                 raw.smpl = prm.sets.selected$raw.smpl,
                                                 prm.combs = prms.combs.val.sim$DF[-1,c(2:(numPrm$prmset+1))],
                                                 z.to.org = FALSE)

      }else if(prm.set.method$method != "unif_grid" && !is.null(ml.model.selected$custom.scale)) {

        prms.combs.val.sim$DF_z = fun.scale.conv(sample_meth = prm.set.method$method,
                                                 prm.ranges = prm.ranges.selected$DF[idx.not.custom,],
                                                 raw.smpl = prm.sets.selected$raw.smpl[,idx.not.custom],
                                                 prm.combs = prms.combs.val.sim$DF[-1,prms.not.custom],
                                                 z.to.org = FALSE)
        temp.custom.scale.func <- get.custom.scale.func.obj(object = PPM$object,name = ml.model.selected$custom.scale$name)
        prms.combs.val.sim$DF_z <- cbind(prms.combs.val.sim$DF_z,
                                         temp.custom.scale.func(prm.combs = prms.combs.val.sim$DF[-1,], other.vals = ml.model.selected$custom.scale$other.vals,cs.to.org = F)[,names(ml.model.selected$custom.scale$parameters)]
        )

      }

      ##negative prms -> multiply by -1 again
      ##original
      prms.combs.val.sim$DF[-1,prms.not.custom] <-
        apply( matrix((colnames(temp.neg)),nrow =1),2,
               function(prm, temp.neg, DF) {
                 if(temp.neg[,prm]){
                   DF[,prm] <- -1* DF[,prm]
                 }else{
                   DF[,prm]
                 }},
               temp.neg = temp.neg,
               DF = prms.combs.val.sim$DF[-1,prms.not.custom])

      ##rescaled
      prms.combs.val.sim$DF_z[,prms.not.custom] <-
        apply( matrix((colnames(temp.neg)),nrow =1),2,
               function(prm, temp.neg, DF) {
                 if(temp.neg[,prm]){
                   DF[,prm] <- -1* DF[,prm]
                 }else{
                   DF[,prm]
                 }},
               temp.neg = temp.neg,
               DF =  prms.combs.val.sim$DF_z[,prms.not.custom] )
      prms.combs.val.sim$DF_z <- data.frame( prms.combs.val.sim$DF_z )
      names(prms.combs.val.sim$DF_z)   <- c( prms.not.custom, names(ml.model.selected$custom.scale$parameters))




      #print(prms.combs.val.sim$DF[2,2:(nrow(prm.ranges.phase)+1)])
      prms.combs.val.sim$selected_prm_comb_z = prms.combs.val.sim$DF_z[1,]

      #print( prms.combs.val.sim$DF_z[1,])
      prms.combs.val.sim$DF_z = prms.combs.val.sim$DF_z[-1,]

      prms.combs.val.sim$perturbed_prm1 = colnames(prms.combs.val.sim$DF)[which(prms.combs.val.sim$DF[1,] == 1)]
      prms.combs.val.sim$perturbed_prm2 = colnames(prms.combs.val.sim$DF)[which(prms.combs.val.sim$DF[1,] == 2)]
      if(!is.null(ml.model.selected$custom.scale)){
        if(any(ml.model.selected$custom.scale$parameters == prms.combs.val.sim$perturbed_prm1)){
          prms.combs.val.sim$perturbed_prm1 = names(ml.model.selected$custom.scale$parameters)[ml.model.selected$custom.scale$parameters == prms.combs.val.sim$perturbed_prm1]
        }else{}

        if(any(ml.model.selected$custom.scale$parameters == prms.combs.val.sim$perturbed_prm2)){
          prms.combs.val.sim$perturbed_prm2 = names(ml.model.selected$custom.scale$parameters)[ml.model.selected$custom.scale$parameters == prms.combs.val.sim$perturbed_prm2]
        }
      }



      prms.combs.val.sim$num_grids = sqrt(nrow(prms.combs.val.sim$DF)-2)

      prms.combs.val.sim$phenotype = input$load_phenotype
      #colnames(prms.combs.val.sim$DF)[ncol(prms.combs.val.sim$DF)]

      prms.combs.val.sim$selected_prm_comb_z_pred = predict( ml.model.selected$ml.model,  prms.combs.val.sim$selected_prm_comb_z[,parameters$ml.model]) - predict( ml.model.selected$ml.model.res,  prms.combs.val.sim$selected_prm_comb_z[,parameters$ml.model])
      prms.combs.val.sim$selected_prm_comb_z_simulated = prms.combs.val.sim$DF[2, prms.combs.val.sim$phenotype]
      prms.combs.val.sim$pred = predict( ml.model.selected$ml.model,  prms.combs.val.sim$DF_z[,parameters$ml.model]) - predict( ml.model.selected$ml.model.res,  prms.combs.val.sim$DF_z[, parameters$ml.model])
      prms.combs.val.sim$simulated = prms.combs.val.sim$DF[-c(1,2), prms.combs.val.sim$phenotype]

      print( prms.combs.val.sim$perturbed_prm1 )
      print(prms.combs.val.sim$perturbed_prm2)
    }
  })





  output$validation_plots <- renderUI({
    # if(input$gen_val_plots[[1]] == 0){
    #   return(0)
    # }else{
    input$gen_val_plots
    if(!is.null(prms.combs.val.sim$DF)){
      isolate({
        if(input$plot_type_val == "2D"){
          list(
            column(4,plotOutput("val_plot_pred_2d", width = "400px")),
            column(4,plotOutput("val_plot_sim_2d", width = "400px")),
            column(4,plotOutput("val_plot_corr", width = "400px"))
          )
        }else if(input$plot_type_val == "3D"){
          list(
            column(4,rglwidgetOutput("val_plot_pred_3d", width = "400px")),
            column(4,rglwidgetOutput("val_plot_sim_3d", width = "400px")),
            column(4,plotOutput("val_plot_corr", width = "400px"))
          )
        }
      })
    }
  })





  output$val_plot_pred_2d <- renderPlot({
    input$gen_val_plots
    print(prms.combs.val.sim$selected_prm_comb_z)
    #since rf models were trained without kYp, dYp, n, K
    isolate({if(!is.null( ml.model.selected$ml.model)){
      if(0){
        vec.plot.bc.mod( ml.model.selected$ml.model, ml.model.selected$ml.model.res,prms.combs.val.sim$selected_prm_comb_z[1:numPrm$ml.model],
                        c(prms.combs.val.sim$perturbed_prm1, prms.combs.val.sim$perturbed_prm2),prm.ranges.z.selected$DF[,1:numPrm$ml.model], grid.lines =  prms.combs.val.sim$num_grids, zoom = 1, zlim = c(input$plot_range_val[1],input$plot_range_val[2]), gap = 0 , three.dim = F)
      }

      if(1){
        image2D(matrix( prms.combs.val.sim$pred, nrow = prms.combs.val.sim$num_grids), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1]), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2]), contour = T, zlim = c(input$plot_range_val[1],input$plot_range_val[2]),xlab = prms.combs.val.sim$perturbed_prm1, ylab =prms.combs.val.sim$perturbed_prm2)
        points(prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm2], pch =  19 )
      }
    }})


  })

  output$val_plot_sim_2d <- renderPlot({
    input$gen_val_plots
    isolate({if(!is.null( ml.model.selected$ml.model)){
      image2D(matrix(prms.combs.val.sim$simulated, nrow =prms.combs.val.sim$num_grids), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1]), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2]), contour = T, zlim = c(input$plot_range_val[1],input$plot_range_val[2]),xlab = prms.combs.val.sim$perturbed_prm1, ylab =prms.combs.val.sim$perturbed_prm2)
      points(prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm2], pch =  19 )
    }})
  })


  output$val_plot_pred_3d <- rgl::renderRglwidget({
    input$gen_val_plots
    #since rf models were trained without kYp, dYp, n, K
    isolate({if(!is.null( ml.model.selected$ml.model)){
      try(rgl.close(), silent = TRUE)

      if(0){
        vec.plot.bc.mod( ml.model.selected$ml.model, ml.model.selected$ml.model.res,prms.combs.val.sim$selected_prm_comb_z[1:numPrm$ml.model],
                        c(prms.combs.val.sim$perturbed_prm1, prms.combs.val.sim$perturbed_prm2),prm.ranges.z.selected$DF[,1:numPrm$ml.model], grid.lines =  prms.combs.val.sim$num_grids, zoom = 1, zlim = c(input$plot_range_val[1],input$plot_range_val[2]), gap = 0 , three.dim = T)
      }

      if(1){
        color.gradient <- function(x, colors=c("blue", "green","yellow","red"), colsteps=100) {
          return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(input$plot_range_val[1],input$plot_range_val[2], length.out=colsteps)) ] )
        }
        plot3d(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2],
               z = prms.combs.val.sim$pred, zlim = c(input$plot_range_val[1],input$plot_range_val[2]), xlab = prms.combs.val.sim$perturbed_prm1, ylab =prms.combs.val.sim$perturbed_prm2, zlab = prms.combs.val.sim$phenotype)
        plot3d(prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm2], z =   prms.combs.val.sim$selected_prm_comb_z_pred, xlab = prms.combs.val.sim$perturbed_prm1, ylab = prms.combs.val.sim$perturbed_prm2,
               main = "Prediction", col = "red", size = 7, add = TRUE, zlim = c(input$plot_range_val[1],input$plot_range_val[2]))
        surface3d(unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1]), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2]),
                  z = prms.combs.val.sim$pred, col = color.gradient( prms.combs.val.sim$pred), size = 4, alpha = 0.4, zlim = c(input$plot_range_val[1],input$plot_range_val[2]))

      }
      scene2<- scene3d()
      rglwidget(scene2)

    }})


  })

  output$val_plot_sim_3d <- rgl::renderRglwidget({
    input$gen_val_plots
    isolate({if(!is.null( ml.model.selected$ml.model)){
      try(rgl.close(), silent = TRUE)
      color.gradient <- function(x, colors=c("blue", "green","yellow","red"), colsteps=100) {
        return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(input$plot_range_val[1],input$plot_range_val[2], length.out=colsteps)) ] )
      }
      plot3d(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2],
             z = prms.combs.val.sim$simulated, zlim =c(input$plot_range_val[1],input$plot_range_val[2]), xlab = prms.combs.val.sim$perturbed_prm1, ylab =prms.combs.val.sim$perturbed_prm2, zlab = prms.combs.val.sim$phenotype)
      plot3d(prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm1], prms.combs.val.sim$selected_prm_comb_z[prms.combs.val.sim$perturbed_prm2], z = prms.combs.val.sim$selected_prm_comb_z_simulated, xlab = prms.combs.val.sim$perturbed_prm1, ylab = prms.combs.val.sim$perturbed_prm2,
             main = "Simulation", col = "red", size = 7, add = TRUE, zlim = c(input$plot_range_val[1],input$plot_range_val[2]))
      surface3d(unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm1]), unique(prms.combs.val.sim$DF_z[,prms.combs.val.sim$perturbed_prm2]),
                z = prms.combs.val.sim$simulated, col = color.gradient(prms.combs.val.sim$simulated), size = 4, alpha = 0.4,  zlim = c(input$plot_range_val[1],input$plot_range_val[2]))

      scene3<- scene3d()
      rglwidget(scene3)
    }})

  })


  output$val_plot_corr <- renderPlot({
    input$gen_val_plots
    isolate({if(!is.null( ml.model.selected$ml.model)){
      df <- data.frame(x = prms.combs.val.sim$simulated, y = prms.combs.val.sim$pred )
      g = ggplot2::ggplot(df,ggplot2::aes(x=x, y=y)) + ggplot2::geom_point(alpha=1, size = 1, color = "black") +# stat_density2d(aes( alpha = ..level..),geom='polygon',colour='yellow', size = 0.05)+
        ggplot2::geom_abline(slope = 1,intercept = 0) + ggplot2::xlim(input$plot_range_val[1],input$plot_range_val[2]) +ggplot2::ylim(input$plot_range_val[1],input$plot_range_val[2]) + ggplot2::ggtitle(paste0("Pred VS Sim for ", prms.combs.val.sim$perturbed_prm1," and ", prms.combs.val.sim$perturbed_prm2, ": corr = ", signif(cor( prms.combs.val.sim$simulated, prms.combs.val.sim$pred,use = "complete.obs"), 3)))+ ggplot2::theme_bw() + ggplot2::xlab("Simulated      ") +ggplot2:: ylab("Predicted     ") + #+ geom_smooth(method=lm,linetype=2,colour="red",se=F)
        ggplot2::theme(axis.text=ggplot2::element_text(size=10), axis.title=ggplot2::element_text(size=10))
      g
    }})
  })



  #h-clustering for prm.z and localVarImp
  output$gen_hclust_ui <- renderUI({
    list(
      column(3,actionButton("gen_hclust","Generate hierachical clustering plots!")),
      column(2, checkboxInput("hclust_prms_col_dendr", h5("Column dendrogram for parameters"))),
      column(2, checkboxInput("hclust_locImp_col_dendr", h5("Column dendrogram for local importance")))

    )
  })


  brush.points <- reactiveValues()
  hclust <- reactiveValues()

  myfun <- function(x) hclust(x, method = "ward.D")

  observeEvent(input$gen_hclust,{
    if(!is.null(input$tsne_brush)){
      brush.points$rtsne = brushedPoints(ggdata, input$tsne_brush, xvar = "tSNE1", yvar = "tSNE2")
      if(!is.null(ml.model.selected$ml.model)){
        brush.points$loc.imp = local.importance$DF[local.importance$DF$pkey %in% brush.points$rtsne$pkey,c("pkey", parameters$ml.model)]
        brush.points$loc.imp <- brush.points$loc.imp[order(brush.points$loc.imp$pkey),]
        row.names(brush.points$loc.imp) = brush.points$loc.imp$pkey
        brush.points$loc.imp = brush.points$loc.imp[,-1]
      }else{
        numPrm$ml.model <- numPrm$prmset
      }

      temp.prms.custom <- setdiff( parameters$ml.model, parameters$prmset)
      if(length(temp.prms.custom) != 0){
        brush.points$prm.z <- prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey %in% brush.points$rtsne$pkey,c("pkey", setdiff(parameters$ml.model,temp.prms.custom ))]
        temp.prms.custom.z <- get.custom.scale(PPM$object, paste0(ml.model.selected$custom.scale$name,".z"))
        temp.prm.values <- temp.prms.custom.z$func.obj(prm.sets.selected$original[prm.sets.selected$original$pkey %in% brush.points$rtsne$pkey,c("pkey", parameters$prmset)],
                                    temp.prms.custom.z$other.vals,
                                    F)
        brush.points$prm.z <- cbind(brush.points$prm.z, temp.prm.values[,-1])
        temp.prm.values <- NULL
      }else{
        brush.points$prm.z = prm.sets.selected$rescaled[prm.sets.selected$rescaled$pkey %in% brush.points$rtsne$pkey,c("pkey", parameters$ml.model)]
      }
      row.names( brush.points$prm.z) = brush.points$prm.z$pkey
      brush.points$prm.z = brush.points$prm.z[,-1]

      hclust$prms.row <- as.dendrogram(myfun(dist(as.matrix(brush.points$prm.z))))
      hclust$prms.col <- as.dendrogram(myfun(dist(t(as.matrix(brush.points$prm.z)))))
      hclust$locImp.row <- as.dendrogram(myfun(dist(as.matrix(brush.points$loc.imp))))
      hclust$locImp.col <- as.dendrogram(myfun(dist(t(as.matrix(brush.points$loc.imp)))))
      hclust$locImp.row.cut <- NULL
      hclust$locImp.row.cut.color <- NULL


    }else{
      brush.points$rtsne <- NULL
      brush.points$prm.z <- NULL
      brush.points$loc.imp <- NULL
      hclust$prms.row <- NULL
      hclust$prms.col <- NULL
      hclust$locImp.row <- NULL
      hclust$locImp.col <- NULL
      hclust$locImp.row.cut <- NULL
      hclust$locImp.row.cut.color <- NULL
    }

  })



  output$hclust_prms <- renderPlot({
    if(!is.null(brush.points$prm.z)){

        temp.range = signif(range(as.numeric(as.matrix(brush.points$prm.z))),2 )
        colors <- c(seq(temp.range[1],temp.range[2],length=100))
        if(!is.null(input$hclust_prms_selected_pt)){
          if(input$hclust_prms_selected_pt){
            ##denote selected point

            temp.hclust.prms.row.col <- rep(NA, nrow(brush.points$prm.z) )
            temp.hclust.prms.row.col[row.names(brush.points$prm.z) == selected$pkey] <- "black"

            if(!input$hclust_prms_col_dendr){
              isolate({
                #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun, Rowv = TRUE, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
                gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations"),  RowSideColors=temp.hclust.prms.row.col )
              })
            }else{
              isolate({
                #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun,  trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
                gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = hclust$prms.col, trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations"), RowSideColors=temp.hclust.prms.row.col )
              })
            }


          }else{
            if(!input$hclust_prms_col_dendr){
              isolate({
                #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun, Rowv = TRUE, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
                gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
              })
            }else{
              isolate({
                #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun,  trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
                gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = hclust$prms.col, trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
              })
            }
          }
        }else{
          if(!input$hclust_prms_col_dendr){
            isolate({
              #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun, Rowv = TRUE, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
              gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
            })
          }else{
            isolate({
              #heatmap.2(as.matrix(brush.points$prm.z), hclustfun = myfun,  trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
              gplots::heatmap.2(as.matrix(brush.points$prm.z),  Rowv = hclust$prms.row, Colv = hclust$prms.col, trace = "none",breaks = colors, col = colorRampPalette(c("blue", "white", "red"))(n = 99), main = paste0("Parameter combinations") )
            })
          }
        }



    }
  })

  output$hclust_prms_selected_ui <- renderUI({
    if(!is.null(brush.points$prm.z) & !is.null(selected$pkey)){
      if(selected$pkey %in% row.names(brush.points$prm.z)){
        checkboxInput("hclust_prms_selected_pt", h5("Denote selected point"))
      }
    }
  })


  output$hclust_locImp_spec_ui <- renderUI({
    if(!is.null(brush.points$loc.imp)){
      isolate({
      temp.range = signif(range(as.numeric(as.matrix(local.importance$DF[,-1]))),1 )
      list(
        column(4,sliderInput(inputId = "hclust_locImp_color_scale",
                    label = h5("Color scale"),
                    min = 0,
                    max = temp.range[2]/2,
                    step = (temp.range[2])/500,
                    round = -1,
                    dragRange = TRUE,
                    value =  temp.range[2])),
        column(4,selectInput(inputId = "num_clust",label = h5("Number of cluster"), choices = c(1:20))),
        column(3,actionButton("hclust_local_varimp_refresh", h5("Refresh")))
      )
      })
    }
  })


  observeEvent(input$hclust_local_varimp_refresh,{
    if(!is.null(input$num_clust)){
      if(input$num_clust > 1){
        hclust$locImp.row.cut = cutree(as.hclust(hclust$locImp.row), input$num_clust)
        color.gradient = colorRampPalette(c("blue", "green","yellow","red"))
        temp.clust.color <- color.gradient( input$num_clust)
        hclust$locImp.row.cut.color <-  temp.clust.color[ hclust$locImp.row.cut ]
      }else{
        hclust$locImp.row.cut <- NULL
        hclust$locImp.row.cut.color <- NULL
      }
    }
  })

  output$hclust_local_varimp <- renderPlot({
    if(!is.null(brush.points$loc.imp)){
      input$hclust_local_varimp_refresh
      input$hclust_locImp_col_dendr
      isolate({
        if(nrow(brush.points$loc.imp) != 0  & !is.null(input$hclust_locImp_color_scale)){

          colors <- c(seq(-input$hclust_locImp_color_scale,input$hclust_locImp_color_scale,length=100))
          if(!is.null(hclust$locImp.row.cut)){##with clusters
            if(!input$hclust_locImp_col_dendr){
              #heatmap.2(as.matrix(brush.points$loc.imp), hclustfun = myfun, Rowv = TRUE, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
              gplots::heatmap.2(as.matrix(brush.points$loc.imp),  Rowv = hclust$locImp.row, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") ,  RowSideColors=hclust$locImp.row.cut.color)
            }else{
              #heatmap.2(as.matrix(brush.points$loc.imp), hclustfun = myfun, trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
              gplots::heatmap.2(as.matrix(brush.points$loc.imp),  Rowv = hclust$locImp.row, Colv = hclust$locImp.col, trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance"), RowSideColors=hclust$locImp.row.cut.color )
            }
          }else{
            if(!input$hclust_locImp_col_dendr){
              #heatmap.2(as.matrix(brush.points$loc.imp), hclustfun = myfun, Rowv = TRUE, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
              gplots::heatmap.2(as.matrix(brush.points$loc.imp),  Rowv = hclust$locImp.row, Colv = FALSE,  dendrogram = "row", trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
            }else{
              #heatmap.2(as.matrix(brush.points$loc.imp), hclustfun = myfun, trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
              gplots::heatmap.2(as.matrix(brush.points$loc.imp),  Rowv = hclust$locImp.row, Colv = hclust$locImp.col, trace = "none",breaks = colors, col = colorRampPalette(c("green", "black", "red"))(n = 99), main = paste0("Local variable importance") )
            }

          }


        }
      })
    }
  })
}

