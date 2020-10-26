mod_gwas_server <- function(input, output, session) {
  ns <- session$ns
  options(shiny.usecairo = TRUE)
  global_value <- reactiveValues(
    p_data = NULL,
    samples = NULL,
    trait = NULL,
    res = NULL,
    col1 = NULL,
    col2 = NULL,
    logpvalue = NULL,
    sig_p = NULL,
    distance = NULL,
    gwas_res_emmax_vis = NULL,
    gene_extracted = NULL,
    gene_sig_select = NULL,
    manhattan_plot = NULL,
    QQ_plot = NULL
  )
  
  ## ==========设定表型文件名用以后续写入及读取===============
  trait_name <- eventReactive(input$run_gwas,{
    name <- paste0("./tmp/", input$trait,".txt")
    return(name)
  })
  
  ## ============上传表型数据======================
  pheno <- reactive({
    df <- readNewData(input$phenotype)
    df <- left_join(core, df, by = c("core" = "V1"))
    df <- df[match(tfam$V1, df$core), ]
    df[df == -999] <- NA
    df <- cbind(df[, 1], df)
    return(df)
  })
  ## ===============可视化表型数据分布==================
  p_his <- reactive({
    ggplot(pheno(), aes(x=V2,y=..density..))+
      geom_histogram(fill="lightblue", color="grey60",size=0.2)+
      geom_density(color="red")+
      labs(
        title = paste0("Distribution of Phenotype ", "(", input$trait, ")"),
        x = paste0("Value of Phenotype ", "(", input$trait, ")"),
        y = "Density",
        caption = "\n Be carful of your phenotype data, \n it will affect the results of GWAS greatly!"
      ) +
      global_theme
  })
  output$phenotype_vis <- renderPlot({
    shiny::req(pheno())
    print(p_his())
  })
  ## =============运行GWAS并展示结果==================
  output$gwas_res <- renderDataTable({
    write.table(pheno(), trait_name(), col.names = FALSE, row.names = FALSE, quote = FALSE)
    out <- output(trait = input$trait)
    withProgress(
      message = "GWAS RUN in progress",
      detail = "This may take 1 min! Please Wait! ...",
      value = 0,
      {
        for (i in 1:15) {
          incProgress(1 / 15)
          Sys.sleep(0.01)
        }
        gwas_emmax(phenotype = trait_name(), out = out)
        res <- data.table::fread(paste0("./tmp/", Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.ps"), data.table = FALSE)
        global_value$res <- res
        colnames(res) <- c("SNPID", "beta", "SE(beta)", "p-value")
        DT::datatable(global_value$res,
                      rownames = FALSE,
                      filter = "top",
                      selection = "single",
                      options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        columnDefs = list(list(className = "dt-right", target = "_all"))
                      )
        )
      }
    )
  })
  
## =============下载GWAS结果================  
  output$download_gwas_res <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.result.txt")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }
          write.table(global_value$res, file, row.names = FALSE, col.names = TRUE, quote = FALSE)
        }
      )
    }
  )
  ## ============Visualization of GWAS results: manhattan plot and qq plot=============
  # gwas_res_pre <- eventReactive(input$run_vis,{
  #   gwas_data_vis <- manhattan_data_prepare(gwas_res_emmax = res)
  #   return(gwas_data_vis)
  # })
  # 
  
  observeEvent(input$run_vis, {
    global_value$col1 <- input$col1
    global_value$col2 <- input$col2
    global_value$logpvalue <- input$logpvalue
  })
  
  Bna_manhattan <- eventReactive(input$run_vis,{
        gwas_data_vis <- manhattan_data_prepare(gwas_res_emmax = global_value$res)
        global_value$gwas_res_emmax_vis <- gwas_data_vis
        Bna_manhattan <- ggmanhattan(gwasres = global_value$gwas_res_emmax_vis, color = c(input$col1, input$col2), p_select = input$logpvalue, title = paste0("Manhattan Plot of Phenotype ", "(", input$trait, ")"), vlinesize = 0.5)
        return(Bna_manhattan)
  })
  
  
  output$manhattanplot <- renderPlot({
    withProgress(
      message = "Visualization in progress",
      detail = "This may take a while ...",
      value = 0,
      {
        for (i in 1:15) {
          incProgress(1 / 15)
          Sys.sleep(0.01)
        }
        print(Bna_manhattan())
      }
    )
  })
  
  ## ===============download manhattan plot==================
  output$dm <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.manhattan.png")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }
          png(file, width = 15 * 300, height = 7 * 300, res = 300)
          print(Bna_manhattan())
          dev.off()
        }
      )
    }
  )
  ## ================qqplot=================
  output$qq_plot <- renderPlot({
    validate(
      need(!is.null(global_value$col1), "Select the Colors"),
      need(!is.null(global_value$logpvalue), "Select the logpvalue")
    )
    qqman::qq(global_value$gwas_res_emmax_vis$P)
    })
  
  ## ====================download QQ plot===============
  output$download_qqplot <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.QQ_plot.png")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }
          png(file, width = 7 * 300, height = 7 * 300, res = 300)
          qqman::qq(global_value$gwas_res_emmax_vis$P)
          dev.off()
        }
      )
    }
  )
  
  # ==========================Extraction====================================
  # ----------------output of gene select based on p-value
  gene_select <- eventReactive(input$run_extraction,{
    p <- 10^-(input$sig_p)
    snp_select <- global_value$gwas_res_emmax_vis %>% dplyr::filter(P <= p)
    chr_select <- as.character(unique(snp_select$CHR))
    Bna_geneid_select <- Bna_geneid %>% dplyr::filter(chr %in% chr_select)
    gene_sig_select <- get_gene_from_snp(gff = Bna_geneid_select, sig.snp = snp_select, distance = input$distance)
    return(gene_sig_select)
  })
  output$related_genes <- renderDT({
    shiny::req(gene_select())
    DT::datatable(gene_select(),
                  rownames = FALSE,
                  filter = "bottom",
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    columnDefs = list(list(className = "dt-right", target = "_all"))
                  )
    )
  })
  # --------------download significant genes---------
  
  output$download_genes <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.txt")
    },
    content = function(file) {
      write.table(gene_select(), file, row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  )
  
  # =========================gene annotation===================
  gene_anno_data <- eventReactive(input$run_annotation, {
    gene_id <- unique(gene_select()$gene)
    gene_anno_select <- Bna_anno %>% dplyr::filter(geneid %in% gene_id)
    return(gene_anno_select)
  })
  
  output$gene_annotation <- renderDT({
    DT::datatable(gene_anno_data(),
                  rownames = FALSE,
                  filter = "bottom",
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    columnDefs = list(list(className = "dt-right", target = "_all"))
                  ), class = "white-space: nowrap"
    )
  })
  
  # ---------------gene annotation download---------------
  output$gene_anno_download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.anno.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(gene_anno_data(), file)
    }
  )
}

