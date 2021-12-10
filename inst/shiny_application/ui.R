ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "MAPPA",
                                  shinydashboard::dropdownMenu()),

  # title = "MAPPA",
  # header = tags$header(class = "main-header",
  #                      span(class = "logo",
  #                           "MAPPA"),
  #
  #                      tags$nav(class = "navbar navbar-static-top", role = "navigation",
  #                               # Embed hidden icon so that we get the font-awesome dependency
  #                               span(shiny::icon("fa fa-bars"), style = "display:none;"),
  #                               # Sidebar toggle button
  #                               a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
  #                                 role="button",
  #                                 span(class="sr-only", "Toggle navigation")
  #                               ),
  #                               tags$div(class = "navbar-custom-menu",
  #                                        )
  #                    )
  #
  # ),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = "side_menu_tab",
                                shinydashboard::menuItem("Overview", tabName = "overview_tab"),
                                shinydashboard::menuItem("Workspace", tabName = "workspace_tab"),
                                shinydashboard::menuItem("Parameter combination sampling", tabName = "prm_smpl_tab",
                                                         shinydashboard::menuSubItem("Inital sampling", tabName = "init_smpl_tab"),
                                                         shinydashboard::menuSubItem("Additional sampling", tabName = "add_smpl_tab")),
                                shinydashboard::menuItem("ML model training", tabName = "ml_model_tab"),
                                shinydashboard::menuItem("Explore PPM", tabName = "exp_phase_tab")
    )

  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(type="text/css", "#conf_mat tr td:first-child {font-weight:bold;} #conf_stats tr td:first-child {font-weight:bold;}")
    ),

    shinydashboard::tabItems(
      shinydashboard::tabItem("overview_tab",
                tags$div(img(src = "Figure1.png", width = "800"))
                    ),
      shinydashboard::tabItem("workspace_tab",
              fluidRow(

                column(3,h4("Create a new PPM."),
                       textInput("PPM_name", "PPM name."),
                       actionButton("new_PPM","Create!")),
                column(3, h4("Open an existing PPM."),

                       shinyFiles::shinyFilesButton("file_PPM",
                                           "Browse...","Please select a PPM.",
                                           FALSE)
                       #fileInput("file_PPM",
                        #           h4("Open an existing PPM."))),
                       ),
                column(3,h4("Save the current PPM (.Rds)"),
                       #textInput("file_PPM_name", "File name.", value=".Rds"),
                       #downloadButton("save_PPM","Save!")),
                       shinyFiles::shinySaveButton("save_PPM", "Save!", "Save as ...", filetype=list(Rds="Rds"))
                )
              ),
              br(),
              hr(),
              fluidPage(
                column(4,uiOutput("current_PPM_name"))
              ),

              fluidRow(
                column(4, uiOutput("list_prm_ranges")),
                column(4, uiOutput("list_init_prm_combs")),
                column(4, uiOutput("list_addit_prm_combs"))
                ),

              fluidRow(
                column(4, uiOutput("list_phenotypes")),
                column(4, uiOutput("list_ML.models"))
              )


              ),


      shinydashboard::tabItem("init_smpl_tab",

              #h1("Parameter combination generation"),
              sidebarLayout(
                sidebarPanel(selectInput("sampling_meth", label = h5("Select a sampling method."),
                                         choices = list("Uniform grid" = "unif_grid", "Pseudorandom" = "pseudorandom", "Sobol'" = "sobol'", "Latin hypercube" = "latin_hyp")),
                             numericInput("prm_comb_num", label = h5("Number of parameter combinations"), value = 1000, min = 1, max = 10000000),
                             h5("Parameter keys"),
                             fluidRow(
                               column(6,dateInput("pkey_date", label = h6("Current date"), format = "mmddyyyy")),
                               column(6,textInput("pkey_digits", label = h6("Starting digit")))
                             ),
                             checkboxInput("continue", label = h5("Continue"), value = FALSE),
                             actionButton("gen_prm_combs", "Generate!"),
                             #uiOutput("save_prm_combs_ui")
                             shinyFiles::shinySaveButton("save_prm_combs", "Save as a file", "Save parameter combinations as ...", filetype=list(text="txt", csv = "csv")),
                             br(),
                             textInput("init_prm_combs_name",label = h5("Enter a name for this parameter set")),
                             actionButton("init_prm_combs_save2ps", "Add to PPM"),
                             uiOutput("list_init_prm_combs_ui"),
                             verbatimTextOutput("test")
                ),



                mainPanel(h4("Load parameter ranges"),

                          fluidRow(
                             column(4, uiOutput("file_ui")),
                             column(4, uiOutput("prm_ranges_select_ui")),
                             column(2,actionButton("reset", "Reset"),
                                    actionButton("load", "Load"),
                                    actionButton("delete", "Delete"))
                          ),

                          fluidRow(
                            column(3, uiOutput("prm_num_ui")),
                            column(3,h5("Log scale"),
                                   actionButton("sel_desel_all", label = "Select/Deselect All"))
                          ),
                          fluidRow(
                            column(6,rhandsontable::rHandsontableOutput("parameter_ranges")
                            )

                          ),
                          br(),
                          fluidRow(
                            column(3, textInput("prm_ranges_name", h5("Name of parameter ranges")),
                                   actionButton("prm_ranges_save2ps",label = "Add to PPM"),
                                   shinyFiles::shinySaveButton("save", "Save as a file", "Save parameter ranges as ...", filetype=list(text="txt", csv = "csv"))),
                            column(3, uiOutput("prm_grids_gen_ui"))
                          ),
                          hr(),
                          fluidRow(
                            column(3, h4("Parameter grids")),
                            column(3, checkboxInput("check_prm_grids_mod", label = h5("Modify parameter grids")))
                          ),

                          fluidRow(
                            uiOutput("prm_grids_ui")
                          ),


                          uiOutput("prm_comb_ui")
                          #tableOutput("prm.ranges.dup")


                          )

                )


              ),
      shinydashboard::tabItem("add_smpl_tab",
             # h1("Parameter combination zoom-in"),
             sidebarLayout(
               sidebarPanel(selectInput("add_sampling_meth", label = h5("Select a sampling method."),
                                         choices = list("Uniform grid" = "unif_grid", "Pseudorandom" = "pseudorandom", "Sobol'" = "sobol'", "Latin hypercube" = "latin_hyp")),
                             numericInput("add_prm_comb_num", label = h5("Number of sampling per each selected combination"), value = 100, min = 1, max = 10000000),

                             h5("Parameter keys"),
                             fluidRow(
                               column(6,dateInput("add_pkey_date", label = h6("Current date"), format = "mmddyyyy")),
                               column(6,textInput("add_pkey_digits", label = h6("Starting digit")))
                             ),


                             actionButton("gen_prm_combs_zoomin", "Generate!"),
                             #uiOutput("save_prm_combs_ui")
                             shinyFiles::shinySaveButton("save_prm_combs_zoomin", "Save as a file", "Save parameter combinations as ...", filetype=list(text="txt", csv = "csv")),
                             br(),
                             textInput("addit_prm_combs_name",label = h5("Enter a name for this parameter set")),
                             actionButton("addit_prm_combs_save2ps", "Add to PPM"),
                             uiOutput("list_addit_prm_combs_ui"),

                             verbatimTextOutput("add_test")
                ),



               mainPanel(h4("Load an existing parameter space"),
                          fluidRow(
                            column(4,uiOutput("laad_prm_ranges_ui")),
                            column(4,uiOutput("load_init_prm_combs_ui")),
                            column(2,actionButton("add_reset", "Reset"))
                          ),

                          h4("Selected parameter combinations"),
                          uiOutput("file_prm_selected_ui"),


                          fluidRow(
                            uiOutput("prm_space_selected_tab_ui")
                          ),

                          uiOutput("prm_comb_zoomin_ui")

                          )

                )

      ),
      shinydashboard::tabItem("ml_model_tab",
              fluidRow(
                column(3, uiOutput("list_phenotypes_ml_tab_ui"),
                       checkboxInput("with_ml.models_ml", label = h5("With ML models"))),
                column(3, uiOutput("list_prm_sets_ml_tab_ui"),
                       checkboxInput("with_tsne_ml", label = h5("With tSNE coordinates"))),
                column(3, uiOutput("list_ml.models_ml_tab_ui"))
              ),

              fluidRow(
                column(3,uiOutput("list_parameters_ml_tab_ui")),
                column(3,sliderInput("ratio_train_test",  h4("Fraction of training set(%)"),
                                      min = 0, max = 100, step = 0.1, value = 80),
                       uiOutput("text_ratio_ui"),
                       textInput("ml_model_name", h4("Assign a name for ML model."))),
                column(4,
                       fluidRow(column(6,radioButtons("ml_model_mode", h4("Training mode"), choices = c(Classification = "class", Regression = "reg"))),
                                column(6,uiOutput("bias_correct_ui"))),
                       uiOutput("filter_ml_ui"),
                       uiOutput("class_def_ml_ui")
                       )


              ),
              fluidRow(
                column(2,uiOutput("manual_curation_button_ml_ui")),
                column(2,actionButton("train_ml_model", h5("Train ML model!")))
              ),

              fluidRow(uiOutput("manual_curation_hist_ml_ui")
              ),

              br(),
              fluidRow(column(3,uiOutput("new_ml_models_ml_ui")),
                       column(2,br(),br(),actionButton("register_ml", h5("Register to PPM")),
                              actionButton("remove_ml", h5("Remove")))),
              br(),
              fluidRow(
                shinydashboard::tabBox(id = "selection_ml", selected = NULL, width = 12, #type = "pills",
                       tabPanel("Info of trained ML model", value = "tab_info_ml",
                                column(4,
                                       plotOutput("performance")
                                       ),
                                column(4,
                                       plotOutput("varImp")
                                       )

                                ),
                       tabPanel("Prediction performance", value = "tab_pred_perform",
                                column(4,
                                       uiOutput("options_bias_pred_ui")
                                       ),
                                column(8,
                                       uiOutput("OOB_test_pred_ui")
                                       )
                                )
                       )
                )
              ),


      shinydashboard::tabItem("exp_phase_tab",
              fluidRow(
                column(3, uiOutput("list_phenotypes_tab_ui"),
                       checkboxInput("with_ml.models", label = h5("With ML models"))),
                column(3, uiOutput("list_prm_sets_tab_ui"),
                       checkboxInput("with_tsne", label = h5("With tSNE coordinates"))),
                column(3, uiOutput("list_ml.models_tab_ui")),
                column(3, actionButton("launch_exp_phase",label = "Launch!"),
                       br(),
                       h4("Selected ML model"),
                       verbatimTextOutput("test_exp_phase_tab"))
              ),

              fluidRow(
                column(9,
                       h4("Parameter space in t-SNE"),
                       sidebarLayout(

                         sidebarPanel(
                           uiOutput("exp_phase_side_ui"), width = 3
                         ),

                         mainPanel(
                           fluidRow(
                             radioButtons(inputId = "within_ml.model", label = NULL, choices = c("All", "Training and test sets", "Training set" ),inline = TRUE),
                             uiOutput("tsne_ml_ui")
                           ),
                           # fluidRow(column(6,checkboxInput("within_ml.model", label = h5("Training and test sets"))),
                           #          column(6,checkboxInput("within_ml.model", label = h5("Training and test sets")))
                           plotOutput("t_SNE",
                                      dblclick = "tsne_dblclick",
                                      click = "tsne_click",
                                      brush = brushOpts(
                                        id = "tsne_brush",
                                        resetOnNew = TRUE
                                      ),
                                      height = "600px"),
                           width = 9)
                       )
                ),
                column(3,h5("Global Variable Importance (GVI)"),
                       plotOutput("global_varImp",height = "300px"),
                       textInput("tsne_manual_pt_select", h5("Input parameter key for manual selection"))
                       )

                # column(2,
                #        br(),
                #        br(),
                #        tableOutput("parmeter_values")
                # )
              ),
              br(),
              fluidRow(
                uiOutput("selected_point_ui")
              ),
              fluidRow(
                uiOutput("further_info_ml_model_button_ui"),
                uiOutput("further_info_ml_model_ui")
              ),

              br(),
              fluidRow(
                shinydashboard::tabBox(id = "selection", selected = NULL, width = 12, #type = "pills",
                            tabPanel("Heatmaps for selected points", value = "tab_hclust",
                                     fluidRow(
                                       uiOutput("gen_hclust_ui")
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,
                                              plotOutput("hclust_prms",height = "600px"),
                                              uiOutput("hclust_prms_selected_ui")),

                                       column(6,
                                              plotOutput("hclust_local_varimp",height = "600px"),
                                              uiOutput("hclust_locImp_spec_ui")
                                              )
                                       )
                                     ),
                            tabPanel("LVI and parameter perturbation", value = "tab_varImp_perturb",
                                     fluidRow(
                                       column(4,
                                              "Local Variable Importance (LVI)",
                                              #plotOutput("global_varImp",height = "300px"),
                                              plotOutput("local_varImp", height = "300px")
                                       ),
                                       column(8,
                                              "Perturbation",
                                              #choice of parameters to perturb and the type. actionbutton for generation
                                              fluidRow(
                                                column(3,
                                                       uiOutput("parameter_choice1"),
                                                       uiOutput("parameter_choice2"),
                                                       selectInput(inputId = "plot_type",
                                                                   label = "Plot type:",
                                                                   choices = c("2D", "3D")),
                                                       numericInput("num_grids_val", label = h6("Number of grids"),value = 30, min = 2, max = 100),
                                                       uiOutput("plot_range_ui"),
                                                       actionButton(inputId = "plot_gen",
                                                                    label = "Generate a plot!")
                                                ),
                                                column(9,
                                                       uiOutput("perturb_plot")
                                                )
                                              ),
                                              fluidRow(
                                                uiOutput("gen_prm_combs_val_ui")
                                              )
                                       )
                                     )

                                     ),
                            tabPanel("Validation", value = "tab_val",

                                     h5("Load a validation simulation result (.csv)."),
                                     shinyFiles::shinyFilesButton("file_validation",
                                                                  "Browse...","Please select a file with validation simulation results",
                                                                  FALSE),
                                     #fileInput("file_validation", "Load a validation simulation result (.csv)."),
                                     fluidRow(uiOutput("gen_validation_ui")),
                                     fluidRow(uiOutput("validation_plots"))

                                     ),
                            tabPanel("Navigation (under development)", value = "tab_nav",

                                     fluidRow(
                                       ##parameter sliders
                                       column(6, wellPanel(id = "tPanel",
                                                           style = "overflow-y:scroll; max-height: 300px",
                                                           uiOutput("parameters")))


                                       )


                                     )


                            )


                )

              )
    )
  )
)
