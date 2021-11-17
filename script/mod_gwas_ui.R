#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_gwas_ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      sidebarMenu(
        menuItem(strong("Introduction"),
          tabName = "intro", icon = icon("user"),
          menuSubItem(strong("Introduction"), tabName = "intro1"),
          menuSubItem(strong("Analysis Workflow"), tabName = "aw"),
          menuSubItem(strong("Q&A"), tabName = "qa")
        ),
        menuItem(strong("Run GWAS"), tabName = "gwas", icon = icon("galactic-senate")),
        menuItem(strong("Visualization"), tabName = "vis", icon = icon("braille")),
        menuItem(strong("Extraction"), tabName = "extraction", icon = icon("accusoft")),
        menuItem(strong("Annotation"), tabName = "anno", icon = icon("list")),
        menuItem(strong("Feedback"), tabName = "feedback", icon = icon("comment-alt"))
      )
      # absolutePanel(
      #   bottom = 95,
      #   left = 10,
      #   draggable = F,
      #   width = "100%",
      #   height = "auto",
      #   p(a(icon("github fa-2x"), href = "https://github.com/YTLogos/Bna_GWAS_Cloud", target = "_blank"))
      # ),
      # absolutePanel(
      #   bottom = 95,
      #   left = 55,
      #   draggable = F,
      #   width = "100%",
      #   height = "auto",
      #   p(a(icon("paper-plane fa-2x"), href = "mailto:tyan@zju.edu.cn", target = "_blank"))
      # ),
      # absolutePanel(
      #   bottom = 75,
      #   left = 100,
      #   draggable = F,
      #   width = "100%",
      #   height = "auto",
      #   p(a(icon("link fa-2x"), href = "https://taoyan.netlify.app", target = "_blank"))
      # ),
      #
      # absolutePanel(
      #   bottom = 50,
      #   left = 5,
      #   draggable = F,
      #   width = "100%",
      #   height = "auto",
      #   div(span("Developed by", style = "color:grey"), a("JiangLX Lab", href = "https://jianglab.netlify.app", target = "_blank"), span(a(", College of", href = "http://www.cab.zju.edu.cn/en/", target = "_blank"), style = "color:grey")),
      #   div(a("Agriculture and Biotechnology (CAB),", href = "http://www.cab.zju.edu.cn/en/", target = "_blank"), style = "color:grey"),
      #   div(a("Zhejiang University,", href = "http://www.zju.edu.cn/english/", target = "_blank"), style = "color:grey")
      # )
    ),
    dashboardBody(
      tabItems(

        # -------------------tabItem: Introduction of GWAS------------------------------------
        tabItem(
          tabName = "intro1",
          includeMarkdown("www/md/introduction.md")
        ),
        tabItem(
          tabName = "aw",
          includeMarkdown("www/md/workflow.md")
        ),
        tabItem(
          tabName = "qa",
          includeMarkdown("www/md/qa.md")
        ),


        #----------------------tabitem: feedback----------------
        tabItem(
          tabName = "feedback",
          includeMarkdown("www/md/feedback.md")
        ),


        # -----------------------tabitem: run gwas --------------------------------------
        tabItem(
          tabName = "gwas",
          # upload phenotype file
          fluidRow(
            shinydashboard::box(
              title = strong("Step1: Upload Your Phenotype File (.txt format)"),
              status = "warning",
              solidHeader = TRUE,
              fileInput(ns("phenotype"),
                label = "Upload Your Phenotype File (Support txt, csv and xlsx type) (NO Header!) (Leave blank for example run).",
                accept = c(
                  "text/plain",
                  "text/comma-separated-values,text/plain",
                  ".txt",
                  ".csv",
                  ".xlsx"
                ),
                placeholder = "data/rapeseed_hair.txt"
              ),height = 400
            ),

            #-----------------add trait name and select model ----------
            shinydashboard::box(
              title = strong("Step2: Select Model and Enter Your Trait Name"),
              status = "warning",
              solidHeader = TRUE,

              #--------------------Enter trait name-------------------------
              selectInput(ns("ref"),
                          label = "Select The Reference Genome:",
                          choices = list("Darmor-bzh","ZS11"),
                          selected = "Darmor-bzh"
                          ),
              textInput(inputId = ns("trait"), "Enter Your Trait Name: ", value = "Bna_trait"),
              selectInput(ns("model"),
                label = "Select the GWAS Model (Now just support EMMAX model): ",
                choices = list("GLM", "MLM", "CMLM", "EMMAX", "Farm-CPU"),
                selected = "EMMAX"
              ),
              actionButton(ns("run_gwas"),
                "Run Analysis",
                icon("paper-plane"),
                style = "color:#fff; background-color:#20293c; border-color:#20293c; font-weight:700"
              ),height = 400
            )
          ),
          fluidRow(
            shinydashboard::box(
              status = "warning",
              solidHeader = TRUE,
              title = strong("Distribution Of Your Phenotype"),
              withSpinner(plotOutput(ns("phenotype_vis")), image = "img/loading-dog.gif", image.width = 256, image.height = 256)
            ),
            shinydashboard::box(
              status = "warning",
              title = strong("Result of GWAS"),
              solidHeader = TRUE,
              withSpinner(DT::dataTableOutput(ns("gwas_res")), image = "img/loading-dog.gif", image.width = 256, image.height = 256),
              br(),
              br(),
              downloadButton(ns("download_gwas_res"), "Download GWAS Results")
            )
          ),
          br(),
          tags$strong("If you use EMMAX to publish research, please cite: "),
          tags$p("Kang HM, Sul JH, Service SK, Zaitlen NA, Kong SY, Freimer NB, Sabatti C, Eskin E. (2010) Variance component model to account for sample structure in genome-wide association studies. Nat. Genet. 42:348-54.", style = "font-family: Times, serif; font-weight: 500;font-size:15px;")
        ),
        # -----------------------------tabitam: vis-------------------------------------------
        tabItem(
          tabName = "vis",
          fluidRow(
            #------param for manhattan and qq plot------------
            shinydashboard::box(
              status = "warning",
              title = strong("Customized the visualization of Manhattan Plot and QQ Plot"),
              solidHeader = TRUE,
              width = 12,
              column(
                3,
                colourInput(ns("col1"), "Select Color1: ", "#FF8C00")
              ),
              column(
                3,
                colourInput(ns("col2"), "Select Color2: ", "#556B2F")
              ),
              column(4,
                offset = 1,
                sliderInput(ns("logpvalue"), "Choose -log 10 p-value: ",
                  min = -log10(0.01), max = -log10(0.0000000001), value = -log10(0.00001), step = 0.5
                )
              ),
              column(6,
                offset = 5,
                actionButton(ns("run_vis"),
                  "Run Visualization",
                  icon("magic"),
                  style = "color:#fff; background-color:#20293c; border-color:#20293c; font-weight:700"
                )
              )
            )
          ),
          fluidRow(
            #-----manhattan plot and QQ plot------
            shinydashboard::box(
              title = strong("Manhattan Plot"),
              status = "warning",
              width = 12,
              solidHeader = TRUE,
              withSpinner(plotOutput(ns("manhattanplot")), image = "img/loading-dog.gif", image.width = 256, image.height = 256),
              downloadButton(ns("dm"), "Manhattan Plot Download")
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = strong("QQ plot"),
              status = "warning",
              width = 5,
              solidHeader = TRUE,
              withSpinner(plotOutput(ns("qq_plot")), image = "img/loading-dog.gif", image.width = 256, image.height = 256),
              downloadButton(ns("download_qqplot"), "QQ Plot Download")
            )
          )
        ),
        #---------------------------tabitem: extraction-------------------------
        tabItem(
          tabName = "extraction",
          fluidRow(
            #-----------select params for extraction------------
            shinydashboard::box(
              title = strong("Extract genes based on significant SNPs"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              column(
                6,
                sliderInput(ns("sig_p"), "Choose significant -log 10 p-value: ",
                  min =
                    -log10(0.01), max = -log10(0.00000000001), value =
                    -log10(0.000001), step = 0.5
                )
              ),
              column(
                6,
                sliderInput(ns("distance"), "Choose the distance (kb): ",
                  min = 0,
                  max = 150,
                  value = 75,
                  step = 5
                )
              ),
              column(6,
                offset = 5,
                actionButton(ns("run_extraction"),
                  "Run Extraction",
                  icon("magic"),
                  style = "color:#fff; background-color:#20293c; border-color:#20293c; font-weight:700"
                )
              )
            )
          ),
          fluidRow(
            #----------the extracted genes-------------
            shinydashboard::box(
              title = strong("Genes extracted based on significant SNPs"),
              status = "warning",
              width = 12,
              solidHeader = TRUE,
              withSpinner(DT::dataTableOutput(ns("related_genes")), image = "img/loading-dog.gif", image.width = 256, image.height = 256),
              br(),
              downloadButton(ns("download_genes"), "Download Significant Genes")
            )
          )
        ),
        #-----------------------------tabitem: gene annotation------------------
        tabItem(
          tabName = "anno",
          #---------gene annotation-------
          fluidRow(
            shinydashboard::box(
              title = strong("Gene annotation based on different databases"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              tags$p("If you are sure your previous analysis are right, then clink the button: Run Annotation"),
              br(),
              column(6,
                offset = 5,
                actionButton(ns("run_annotation"),
                  "Run Annotation",
                  icon("magic"),
                  style = "color:#fff; background-color:#20293c; border-color:#20293c; font-weight:700"
                )
              )
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = strong("Gene annotation"),
              status = "warning",
              width = 12,
              solidHeader = TRUE,
              withSpinner(DT::dataTableOutput(ns("gene_annotation")), image = "img/loading-dog.gif", image.width = 256, image.height = 256),
              downloadButton(ns("gene_anno_download"), "Download Genes with Annotation")
            )
          )
        )
      )
    )
  )
}