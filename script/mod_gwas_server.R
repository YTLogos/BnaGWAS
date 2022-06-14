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
    gwas_data_value = NULL,
    gene_extracted = NULL,
    gene_sig_select = NULL,
    manhattan_plot = NULL,
    QQ_plot = NULL
  )
  

  # 1. Create an InputValidator object
  iv <- InputValidator$new()
  
  # 2. Add validation rules
  iv$add_rule("trait", sv_required())
  iv$add_rule("trait",char_detect)
  # 3. Start displaying errors in the UI
  iv$enable()

  
  ## ============上传表型数据======================
  pheno <- reactive({
    df <- readNewData(input$phenotype)
    
    if(input$ref=="Darmor-bzh") {
      core <- core.darmor
    }else{
      core <- core.zs11
    }
    tfam <- tfam(ref=input$ref)
    df <- left_join(core, df, by = c("core" = "X1"))
    df <- df[match(tfam$V1, df$core), ]
    df[df == -999] <- NA
    df <- cbind(df[, 1], df)
    return(df)
  })
  ## ==========设定表型文件名用以后续写入及读取===============
  trait_name <- eventReactive(input$run_gwas, {
    name <- paste0("./tmp/", input$trait, ".txt")
    return(name)
  })


  ## ===============可视化表型数据分布==================
  p_his <- reactive({
    ggplot(pheno(), aes(x = X2, y = ..density..)) +
      geom_histogram(fill = "lightblue", color = "grey60", size = 0.2) +
      geom_density(color = "red") +
      labs(
        title = paste0("Distribution of Phenotype ", "(", input$trait, ")"),
        x = paste0("Value of Phenotype ", "(", input$trait, ")"),
        y = "Density",
        caption = if (sum(is.na(pheno()>=10))) paste0("WARNING!!! You phenotype dataset has ",sum(is.na(pheno()))," missing values\nthis may affect GWAS results! Just Be Careful")
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
    out <- output(trait = input$trait, ref=input$ref)
    withProgress(
      message = "GWAS RUN in progress",
      detail = "This may take 2 mins! Please Wait! ...",
      value = 0,
      {
        for (i in 1:15) {
          incProgress(1 / 15)
          Sys.sleep(0.01)
        }
        gwas_emmax(phenotype = trait_name(), out = out, ref=input$ref)
        res <- data.table::fread(paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.ps"), data.table = FALSE)
        gwas_data <- manhattan_data_prepare(gwas_res_emmax = res)
        fwrite(gwas_data,file = paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.txt"), sep = "\t", quote = FALSE)
        cmd <- sprintf("python3 ./script/rename_chr.py  %s %s", paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.txt"), paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.chr.txt"))
        system(cmd)
        zip(zipfile = paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.chr.txt.zip"), files = paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.chr.txt"))
        global_value$res <- res
        gwas_data_dis <- fread(paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.chr.txt"), data.table = FALSE, col.names = c("SNP","CHR","BP","P"))
        global_value$gwas_data_value <- gwas_data_dis
        gwas_data_dis <- gwas_data_dis%>%filter(P<=0.01)
        DT::datatable(gwas_data_dis,
          rownames = FALSE,
          filter = "top",
          selection = "single",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            columnDefs = list(list(className = "dt-right", targets = "_all"))
          )
        )
      }
    )
  })

  ## =============下载GWAS结果================
  output$download_gwas_res <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.result.txt.zip")
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
          # write.table(global_value$res, file, row.names = FALSE, col.names = c("SNPID", "beta", "SE(beta)", "p-value"), quote = FALSE)
          file.copy(paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.chr.txt.zip"), file)
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

  Bna_manhattan <- eventReactive(input$run_vis, {
    # gwas_data_vis <- manhattan_data_prepare(gwas_res_emmax = global_value$res)
    # global_value$gwas_res_emmax_vis <- gwas_data_vis
    gwas_data_vis <- fread(paste0("./tmp/", Sys.Date(), ".", input$trait, ".", input$ref, ".GWAS.EMMAX.cov.txt"), data.table = FALSE)
    global_value$gwas_data_vis <- gwas_data_vis
    gwas_res_emmax_m <- gwas_data_vis%>%filter(P<=0.1)
    Bna_manhattan <- ggmanhattan(gwasres = gwas_res_emmax_m, color = c(input$col1, input$col2), p_select = input$logpvalue, title = paste0("Manhattan Plot of Phenotype ", "(", input$trait, ")"), vlinesize = 0.5, ref = input$ref)
    manhattan_name <<- paste0("./tmp/",system("date +%Y%m%d%H%M%S", intern = TRUE),".", input$trait, ".", input$ref, ".manhattan.png")
    ggsave(manhattan_name, Bna_manhattan, width = 15, height = 7,dpi = 300)
    return(Bna_manhattan)
  })
  Bna_qqplot <- eventReactive(input$run_vis, {
    qqplot_name <<- paste0("./tmp/",system("date +%Y%m%d%H%M%S", intern = TRUE),".", input$trait, ".", input$ref, ".QQplot.png")
    png(filename = qqplot_name,width = 8*300, height = 8*300, res = 300)
    gwas_res_emmax_qq <-global_value$gwas_data_vis
    gwas_res_emmax_qq <- gwas_res_emmax_qq%>%filter(P<=0.1)
    myqq(gwas_res_emmax_qq$P, main = "Q-Q plot of GWAS p-values", col="blue4")
    dev.off()
    Bna_qqplot <- myqq(gwas_res_emmax_qq$P, main = "Q-Q plot of GWAS p-values", col="blue4")
    return(Bna_qqplot)
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
      paste0(Sys.Date(), ".", input$ref, ".", input$trait, ".GWAS.EMMAX.cov.manhattan.png")
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
          file.copy(manhattan_name, file)
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
    print(Bna_qqplot())
  })

  ## ====================download QQ plot===============
  output$download_qqplot <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), ".", input$ref, ".", input$trait, ".GWAS.EMMAX.cov.QQ_plot.png")
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
          file.copy(qqplot_name, file)
        }
      )
    }
  )

  # ==========================Extraction====================================
  # ----------------output of gene select based on p-value
  gene_select <- eventReactive(input$run_extraction, {
    p <- 10^-(input$sig_p)
    snp_select <- global_value$gwas_data_value %>% dplyr::filter(P <= p)
    chr_select <- as.character(unique(snp_select$CHR))
    if(input$ref=="Darmor-bzh"){
      Bna_geneid <- Bna_geneid.darmor
    }else{
      Bna_geneid <- Bna_geneid.zs11
    }
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
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })
  # --------------download significant genes---------

  output$download_genes <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$ref, ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.txt")
    },
    content = function(file) {
      write.table(gene_select(), file, row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  )

  # =========================gene annotation===================
  gene_anno_data <- eventReactive(input$run_annotation, {
    gene_id <- unique(gene_select()$gene)
    if(input$ref=="Darmor-bzh"){
      Bna_anno <- Bna_anno.darmor
    }else{
      Bna_anno <- Bna_anno.zs11
    }
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
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      ), class = "white-space: nowrap"
    )
  })

  # ---------------gene annotation download---------------
  output$gene_anno_download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$ref, ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.anno.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(gene_anno_data(), file)
    }
  )
}
