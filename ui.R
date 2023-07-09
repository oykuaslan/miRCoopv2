fluidPage(
  
  tags$head(tags$script(src="js/index.js")),
  
  tags$head( # must include css
    tags$style(HTML("
        .img-local {
        }
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 5px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }"
    ))
  ),
  
  #includeCSS("www/styles.css"),
  
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #179E93 !important;}')),
  tags$style(HTML(".shiny-notification {position:fixed;top: 30%;left: 30%;right: 30%;}")),
  
  setSliderColor(c("#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F"), c(1, 2, 3, 4, 5, 6)), #depends on the number of sliders, change if you add or remove sliders.
  
  navbarPage(title =a(div(class ="logo",img(src="logo.png")), href = "http://mircoop.sabanciuniv.edu")
             ,id = "mirCoop",
             #theme = bslib_sabanci20_theme, 
             tabPanel('Home',
                      includeCSS("www/ui_styles.css"),
                      # div(class = "container",
                      #     div(id = "st-box"),
                      #     div(id = "nd-box"),
                      #     div(id = "rd-box"),
                      #     div(id = "fourth-box")
                      # ),
                      div(class = "intro_large1",
                      div(class = "container_circ",
                          div(id = "first_circ",
                              img(class = "icon_1",src = "icon_1.png", onclick="customHref('CancerSpecificTriplets')"),
                              p(class = "intro_header1", 
                                a(style = "color: #1A1A1A;","Cancer Specific Triplets", onclick="customHref('CancerSpecificTriplets')")
                                )
                              ),
                          div(id = "second_circ",
                              img(class = "icon_2",src = "icon_2.png", onclick="customHref('Pan-cancerTriplets')"),
                              p(class = "intro_header2", 
                                a(style = "color: #1A1A1A;","Pan-cancer Triplets", onclick="customHref('Pan-cancerTriplets')")
                                )
                              ),
                          div(id = "third_circ",
                              img(class = "icon_3",src = "icon_3.png",onclick="customHref('Pan-cancermiRNAPairs')",
                                  p(class = "intro_header3", 
                                    a(style = "color: #1A1A1A;","Pan-cancer miRNA Pairs", onclick="customHref('Pan-cancermiRNAPairs')")
                                    )
                                  )
                              ),
                          div(id = "fourth_circ",
                              img(class = "icon_4",src = "icon_4.png",  onclick="customHref('Statistics')",
                                  p(class = "intro_header4",
                                    a(style = "color: #1A1A1A;","Statistics", onclick="customHref('Statistics')")
                                    )
                                  )
                              )
                      
                      ),
                      div(class = "intro_exp_div",
                          p(class = "intr_exp","miRCoop uses kernel-based statistical interaction tests, together with miRNA and mRNA target information to identify synergistic miRNA pairs that have weak or no repression on the target mRNA individually, but when act together, induce strong repression. miRCoop web-based user interface allows users to examine the potential triplet interactions." ,
                            span(
                                 a(style ="color:#F1595A"," › Learn more", onclick="customHref('About')")
                                 )
                            )
                      )
                      ),
                      div(class = "footer_box_general",
                        div(class = "footer_box_intro",
                            a(img(class = "footer_sabanci",
                                src = "sabanci.png"), href = "https://www.sabanciuniv.edu/en"
                              ),
                            a(img(class = "footer_bilkent", src = "bilkent.png"), href = "https://w3.bilkent.edu.tr/bilkent/"
                              ),
                            p(class = "footer_text",
                                "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu"
                              )
                            )
                      )
             ),
             navbarMenu(title = "Triplet Interactions",
               tabPanel("Cancer Specific Triplets",value="CancerSpecificTriplets",fluid = TRUE,
                        div(class = "cancer_specific_general",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       selectInput(inputId = "dataset",
                                                   label = p("Cancer Type ",infoBtn('workingPop') %>% 
                                                               spsComps::bsTooltip(
                                                                 title = "TCGA Abbreviations of the cancer types",
                                                                 placement = "right",
                                                                 trigger = "hover"
                                                                 
                                                               )),
                                                   choices = cancerNames,
                                                   selected = "50 Free",
                                                   width = "220px"
                                       ),
                                       uiOutput("dataset"),
                                       
                                       hr(),
                                       selectizeInput("mrnaFilter", "mRNA Filter", choices=NULL, multiple=TRUE),
                                       br(),
                                       selectizeInput("mirnaFilter", "miRNA Filter", choices=NULL, multiple=TRUE),
                                       hr(),
                                       
                                       
                                       sliderInput("Lancaster_XY_Z_range",
                                                   label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                   value = c(0,0.01),
                                                   min = 0,
                                                   max = 0.01),
                                       
                                       hr(),
                                       
                                       conditionalPanel(condition = "input.dataset == 'ACC'",
                                                        sliderInput("BH_pvalue_adjusted_ACC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(ACC_BH_pvalues_adjusted_min,4),
                                                                    max=round(ACC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(ACC_BH_pvalues_adjusted_min,4),round(ACC_BH_pvalues_adjusted_max,4))
                                                        )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'BLCA'",
                                                        sliderInput("BH_pvalue_adjusted_BLCA",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(BLCA_BH_pvalues_adjusted_min,4),
                                                                    max=round(BLCA_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(BLCA_BH_pvalues_adjusted_min,4),round(BLCA_BH_pvalues_adjusted_max,4))
                                                        )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'BRCA'",
                                                        sliderInput("BH_pvalue_adjusted_BRCA",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(BRCA_BH_pvalues_adjusted_min,4),
                                                                    max=round(BRCA_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(BRCA_BH_pvalues_adjusted_min,4),round(BRCA_BH_pvalues_adjusted_max,4))
                                                        )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'CESC'",
                                                        sliderInput("BH_pvalue_adjusted_CESC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(CESC_BH_pvalues_adjusted_min,4),
                                                                    max=round(CESC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(CESC_BH_pvalues_adjusted_min,4),round(CESC_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'CHOL'",
                                                        sliderInput("BH_pvalue_adjusted_CHOL",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(CHOL_BH_pvalues_adjusted_min,4),
                                                                    max=round(CHOL_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(CHOL_BH_pvalues_adjusted_min,4),round(CHOL_BH_pvalues_adjusted_max,4))
                                                      )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'COAD'",
                                                        sliderInput("BH_pvalue_adjusted_COAD",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(COAD_BH_pvalues_adjusted_min,4),
                                                                    max=round(COAD_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(COAD_BH_pvalues_adjusted_min,4),round(COAD_BH_pvalues_adjusted_max,4))
                                                        )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'DLBC'",
                                                        sliderInput("BH_pvalue_adjusted_DLBC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(DLBC_BH_pvalues_adjusted_min,4),
                                                                    max=round(DLBC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(DLBC_BH_pvalues_adjusted_min,4),round(DLBC_BH_pvalues_adjusted_max,4))
                                                         )),
                                       
                                       conditionalPanel(condition = "input.dataset == 'ESCA'",
                                                        sliderInput("BH_pvalue_adjusted_ESCA",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(ESCA_BH_pvalues_adjusted_min,4),
                                                                    max=round(ESCA_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(ESCA_BH_pvalues_adjusted_min,4),round(ESCA_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'HNSC'",
                                                        sliderInput("BH_pvalue_adjusted_HNSC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(HNSC_BH_pvalues_adjusted_min,4),
                                                                    max=round(HNSC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(HNSC_BH_pvalues_adjusted_min,4),round(HNSC_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'KICH'",
                                                        sliderInput("BH_pvalue_adjusted_KICH",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(KICH_BH_pvalues_adjusted_min,4),
                                                                    max=round(KICH_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(KICH_BH_pvalues_adjusted_min,4),round(KICH_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'KIRC'",
                                                        sliderInput("BH_pvalue_adjusted_KIRC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(KIRC_BH_pvalues_adjusted_min,4),
                                                                    max=round(KIRC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(KIRC_BH_pvalues_adjusted_min,4),round(KIRC_BH_pvalues_adjusted_max,4))
                                                      )),
                                       conditionalPanel(condition = "input.dataset == 'KIRP'",
                                                        sliderInput("BH_pvalue_adjusted_KIRP",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(KIRP_BH_pvalues_adjusted_min,4),
                                                                    max=round(KIRP_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(KIRP_BH_pvalues_adjusted_min,4),round(KIRP_BH_pvalues_adjusted_max,4))
                                                        )),
                                       conditionalPanel(condition = "input.dataset == 'LGG'",
                                                        sliderInput("BH_pvalue_adjusted_LGG",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(LGG_BH_pvalues_adjusted_min,4),
                                                                    max=round(LGG_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(LGG_BH_pvalues_adjusted_min,4),round(LGG_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'LIHC'",
                                                        sliderInput("BH_pvalue_adjusted_LIHC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(LIHC_BH_pvalues_adjusted_min,4),
                                                                    max=round(LIHC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(LIHC_BH_pvalues_adjusted_min,4),round(LIHC_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'LUAD'",
                                                        sliderInput("BH_pvalue_adjusted_LUAD",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(LUAD_BH_pvalues_adjusted_min,4),
                                                                    max=round(LUAD_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(LUAD_BH_pvalues_adjusted_min,4),round(LUAD_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'LUSC'",
                                                        sliderInput("BH_pvalue_adjusted_LUSC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(LUSC_BH_pvalues_adjusted_min,4),
                                                                    max=round(LUSC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(LUSC_BH_pvalues_adjusted_min,4),round(LUSC_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'MESO'",
                                                        sliderInput("BH_pvalue_adjusted_MESO",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(MESO_BH_pvalues_adjusted_min,4),
                                                                    max=round(MESO_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(MESO_BH_pvalues_adjusted_min,4),round(MESO_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'OV'",
                                                        sliderInput("BH_pvalue_adjusted_OV",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(OV_BH_pvalues_adjusted_min,4),
                                                                    max=round(OV_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(OV_BH_pvalues_adjusted_min,4),round(OV_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'PAAD'",
                                                        sliderInput("BH_pvalue_adjusted_PAAD",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(PAAD_BH_pvalues_adjusted_min,4),
                                                                    max=round(PAAD_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(PAAD_BH_pvalues_adjusted_min,4),round(PAAD_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'PCPG'",
                                                        sliderInput("BH_pvalue_adjusted_PCPG",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(PCPG_BH_pvalues_adjusted_min,4),
                                                                    max=round(PCPG_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(PCPG_BH_pvalues_adjusted_min,4),round(PCPG_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'PRAD'",
                                                        sliderInput("BH_pvalue_adjusted_PRAD",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(PRAD_BH_pvalues_adjusted_min,4),
                                                                    max=round(PRAD_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(PRAD_BH_pvalues_adjusted_min,4),round(PRAD_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'READ'",
                                                        sliderInput("BH_pvalue_adjusted_READ",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(READ_BH_pvalues_adjusted_min,4),
                                                                    max=round(READ_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(READ_BH_pvalues_adjusted_min,4),round(READ_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'SARC'",
                                                        sliderInput("BH_pvalue_adjusted_SARC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(SARC_BH_pvalues_adjusted_min,4),
                                                                    max=round(SARC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(SARC_BH_pvalues_adjusted_min,4),round(SARC_BH_pvalues_adjusted_max,4))
                                                                            )),
                                       conditionalPanel(condition = "input.dataset == 'SKCM'",
                                                        sliderInput("BH_pvalue_adjusted_SKCM",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(SKCM_BH_pvalues_adjusted_min,4),
                                                                    max=round(SKCM_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(SKCM_BH_pvalues_adjusted_min,4),round(SKCM_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'STAD'",
                                                        sliderInput("BH_pvalue_adjusted_STAD",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(STAD_BH_pvalues_adjusted_min,4),
                                                                    max=round(STAD_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(STAD_BH_pvalues_adjusted_min,4),round(STAD_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'TGCT'",
                                                        sliderInput("BH_pvalue_adjusted_TGCT",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(TGCT_BH_pvalues_adjusted_min,4),
                                                                    max=round(TGCT_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(TGCT_BH_pvalues_adjusted_min,4),round(TGCT_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'THCA'",
                                                        sliderInput("BH_pvalue_adjusted_THCA",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(THCA_BH_pvalues_adjusted_min,4),
                                                                    max=round(THCA_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(THCA_BH_pvalues_adjusted_min,4),round(THCA_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'THYM'",
                                                        sliderInput("BH_pvalue_adjusted_THYM",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(THYM_BH_pvalues_adjusted_min,4),
                                                                    max=round(THYM_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(THYM_BH_pvalues_adjusted_min,4),round(THYM_BH_pvalues_adjusted_max,4))
                                                                    )),
                                       conditionalPanel(condition = "input.dataset == 'UCEC'",
                                                        sliderInput("BH_pvalue_adjusted_UCEC",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(UCEC_BH_pvalues_adjusted_min,4),
                                                                    max=round(UCEC_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(UCEC_BH_pvalues_adjusted_min,4),round(UCEC_BH_pvalues_adjusted_max,4))
                                                        )),
                                       conditionalPanel(condition = "input.dataset == 'UCS'",
                                                        sliderInput("BH_pvalue_adjusted_UCS",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(UCS_BH_pvalues_adjusted_min,4),
                                                                    max=round(UCS_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(UCS_BH_pvalues_adjusted_min,4),round(UCS_BH_pvalues_adjusted_max,4))
                                                        )),
                                       conditionalPanel(condition = "input.dataset == 'UVM'",
                                                        sliderInput("BH_pvalue_adjusted_UVM",
                                                                    label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                    min=round(UVM_BH_pvalues_adjusted_min,4),
                                                                    max=round(UVM_BH_pvalues_adjusted_max,4),
                                                                    value=c(round(UVM_BH_pvalues_adjusted_min,4),round(UVM_BH_pvalues_adjusted_max,4))
                                                        )),
                                       
                                       hr(),
                                       checkboxGroupInput(inputId = "is_mrna_tf",
                                                          label = "Filter out:",
                                                          choiceNames = c("mRNAs that are not TF"),
                                                          choiceValues = c("True"),
                                                          selected = NULL)

                          ),
                          mainPanel(
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Table", br(),
                                                 DT::dataTableOutput("table", height = "800px"),
                                                 div(style = "margin-bottom:15px",
                                                   downloadButton("downloadData", "Download")
                                                   )),
                                        
                                        tabPanel("Network",
                                                 conditionalPanel(condition = "input.dataset == 'ACC' || input.dataset == 'DLBC' ||
                                                                  input.dataset == 'LGG' || input.dataset == 'MESO' || input.dataset == 'OV' || input.dataset == 'TGCT'  ||
                                                                  input.dataset == 'UCS' || input.dataset == 'UVM'",
                                                                  selectInput("colorGroup1", "Color Nodes Based on :",
                                                                              c("miRNA Family", "miRNA Cluster"),selected = NULL)
                                                                  ),
                                                 
                                                 conditionalPanel(condition = "input.dataset == 'BLCA' || input.dataset == 'BRCA' || input.dataset == 'CESC' ||
                                                                  input.dataset == 'CHOL' ||  input.dataset == 'COAD' ||  input.dataset == 'ESCA' || input.dataset == 'HNSC' ||
                                                                  input.dataset == 'KICH' || input.dataset == 'KIRC' || input.dataset == 'KIRP' || input.dataset == 'LIHC' ||
                                                                  input.dataset == 'LUAD' || input.dataset == 'LUSC' || input.dataset == 'PAAD' || input.dataset == 'PCPG' ||
                                                                  input.dataset == 'PRAD' || input.dataset == 'READ' || input.dataset == 'SARC' || input.dataset == 'SKCM' ||
                                                                  input.dataset == 'STAD' || input.dataset == 'THCA' || input.dataset == 'THYM' || input.dataset == 'UCEC'
                                                                  ",
                                                                  selectInput("colorGroup2", "Color Nodes Based on :",
                                                                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"),selected = NULL)
                                                 ),
  
                                                 shinycustomloader::withLoader(visNetworkOutput("vNetwork", height = "100vh"),type = "html",loader="loader3"),
                                                 div(align = "right", downloadButton("downloadNodeTable", "Download Node Table"),
                                                     downloadButton("downloadEdgeTable", "Download Edge Table")),
                                                 br(),
                                                 br())
                            )
                          )
                        )
                        )
                ),
               
               
               tabPanel("Pan-cancer Triplets", value="Pan-cancerTriplets",fluid = TRUE,
                        div(class = "pancancer_triplets_general",
                          sidebarLayout(
                            sidebarPanel(
                              
                              shinyWidgets::pickerInput(inputId = "CommonTripletCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE,options = pickerOptions(list(`actions-box` = TRUE))),
                              hr(),
                              selectizeInput("mrnaCommonTriplet", "mRNA Filter", choices=unique(mrnaFilterCommonTriplets$name), multiple=TRUE),
                              br(),
                              selectizeInput("mirnaCommonTriplet", "miRNA Filter", choices=unique(mirnaFilterCommonTriplets$name), multiple=TRUE),
                              hr(),
                              br(),
                              downloadButton("downloadCommonTripletsData", "Download")
                              
                            ),
                            
                            mainPanel(
                              useShinyalert(),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Table", br(),
                                                   DT::dataTableOutput("tableCommonTriplet")),
                                          tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonTripletNetwork", height = "100vh"),type = "html",loader="loader3"),
                                                   br(),
                                                   div(align = "right",downloadButton("downloadNodeTableCommonTriplet", "Download Node Table"),
                                                       downloadButton("downloadEdgeTableCommonTriplet", "Download Edge Table")),
                                                   br(),
                                                   br()
                                          )
                              )
                            )
                            
                          )
                        )
               ),
               
               tabPanel("Pan-cancer miRNA Pairs", value="Pan-cancermiRNAPairs",fluid = TRUE,
                        div(class = "pancancer_mirna_pairs_general",
                            sidebarLayout(
                              sidebarPanel(
                                
                                # checkboxGroupInput(inputId = "CommonCancer", 
                                #                    label = "Select Cancer Names",
                                #                    choices = cancerNames,
                                #                    selected=cancerNames),
                               
                                pickerInput(inputId = "CommonMirnaPairCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE),
                                hr(),
                                selectizeInput("mirnaCommonMirnaPair", "miRNA Filter", choices=mirnaListCommonMirnaPairs, multiple=TRUE),
                                hr(),
                                selectInput(inputId = "CommonMirnaPairCancerCount", label = "Select with Count Above:",choices = c(1,2,3,4), selected=c(1), multiple = FALSE),
                                # checkboxGroupInput(inputId = "CommonMirnaPairCancerCount",
                                #                    label = "Filter with Count",
                                #                    choiceNames = c("Above 3","Above 4","Above 5","Above 6", "Above 7","Above 8"),
                                #                    choiceValues = c("Above3","Above4","Above5","Above6", "Above7","Above8"),
                                #                    selected = c("Above3","Above4","Above5","Above6", "Above7","Above8")),
                                #numericInput("CommonMirnaPairCancerCount2", "Select with Count Above:", 2, min = 2, max = 8, step = 1),
                                br(),
                                br(),
                                downloadButton("downloadCommonMirnaPairsData", "Download")
                                
                              ),
                              
                              mainPanel(
                                useShinyalert(),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPair")
                                                     ),
                                            tabPanel("Network", selectInput("colorCommonMirna", "Color Nodes Based on :",
                                                                            c("miRNA Family", "miRNA Cluster"),selected = NULL),
                                                     shinycustomloader::withLoader(visNetworkOutput("commonMirnaNetwork", height = "100vh"),type = "html",loader="loader3"),
                                                     br(),
                                                     div(align = "right", downloadButton("downloadNodeTableCommonMirnaPair", "Download Node Table"),
                                                         downloadButton("downloadEdgeTableCommonMirnaPair", "Download Edge Table")),
                                                     br(),
                                                     br())
                                            
                                            
                                )
                              )
                              
                            )
                        )
               )
       
             ),
             navbarMenu(title = "Synergy Modules",
                        tabPanel("Cancer Specific Synergy Modules",value="CancerSpecificSynergyModules",fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                selectInput(inputId = "SynergyModulesType", label = "Select Interaction Type",choices = c("Type 1","Type 2","Type 3"),selected = "Type 1",multiple = FALSE),
                                                conditionalPanel("input.SynergyModulesType == 'Type 3'",
                                                                 selectInput(inputId = "datasetTF_Type3",
                                                                             label = p("Cancer Type ",infoBtn('workingPop') %>% 
                                                                                         spsComps::bsTooltip(
                                                                                           title = "TCGA Abbreviations of the cancer types",
                                                                                           placement = "right",
                                                                                           trigger = "hover"
                                                                                           
                                                                                         )),
                                                                             choices = cancerNames_Type3,
                                                                             selected = "50 Free",
                                                                             width = "220px"
                                                                 ),
                                                                 hr(),
                                                                 selectizeInput("mirnaFilterTF_Type3", "miRNA Filter", choices=NULL, multiple=TRUE),
                                                                 hr(),
                                                                 br(),
                                                                 selectizeInput("tfFilterTF_Type3", "TF Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 selectizeInput("targetFilterTF_Type3", "Target mRNA Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 sliderInput("Lancaster_XY_Z_rangeTF_Type3",
                                                                             label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                             value = c(0,0.01),
                                                                             min = 0,
                                                                             max = 0.01),
                                                                 hr(),
                                                                 #checkboxInput("tfActivator_Type3", "Show Activator TFs", value = TRUE, width = NULL),
                                                                 #checkboxInput("tfRepressor_Type3", "Show Repressor TFs", value = TRUE, width = NULL),
                                                                 #hr(),
                                                                 downloadButton("downloadDataTF_Type3", "Download")
                                                                 
                                                ),
                                                conditionalPanel("input.SynergyModulesType == 'Type 1'",
                                                                 selectInput(inputId = "datasetTF_Type1",
                                                                             label = p("Cancer Type ",infoBtn('workingPop') %>%
                                                                                         spsComps::bsTooltip(
                                                                                           title = "TCGA Abbreviations of the cancer types",
                                                                                           placement = "right",
                                                                                           trigger = "hover"
                                                                                           
                                                                                         )),
                                                                             choices = cancerNames_Type1,
                                                                             selected = "50 Free",
                                                                             width = "220px"
                                                                 ),
                                                                 hr(),
                                                                 selectizeInput("mirnaFilterTF_Type1", "miRNA Filter", choices=NULL, multiple=TRUE),
                                                                 hr(),
                                                                 br(),
                                                                 selectizeInput("tfFilterTF_Type1", "TF Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 selectizeInput("targetFilterTF_Type1", "Target mRNA Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 sliderInput("Lancaster_XY_Z_rangeTF_Type1",
                                                                             label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                             value = c(0,0.01),
                                                                             min = 0,
                                                                             max = 0.01),
                                                                 hr(),
                                                                 #checkboxInput("tfActivator_Type1", "Show Activator TFs", value = TRUE, width = NULL),
                                                                 #checkboxInput("tfRepressor_Type1", "Show Repressor TFs", value = TRUE, width = NULL),
                                                                 #hr(),
                                                                 downloadButton("downloadDataTF_Type1", "Download")
                                                                 
                                                ),
                                                conditionalPanel("input.SynergyModulesType == 'Type 2'",
                                                                 selectInput(inputId = "datasetTF_Type2",
                                                                             label = p("Cancer Type ",infoBtn('workingPop') %>%
                                                                                         spsComps::bsTooltip(
                                                                                           title = "TCGA Abbreviations of the cancer types",
                                                                                           placement = "right",
                                                                                           trigger = "hover"
                                                                                           
                                                                                         )),
                                                                             choices = cancerNames_Type2,
                                                                             selected = "50 Free",
                                                                             width = "220px"
                                                                 ),
                                                                 hr(),
                                                                 selectizeInput("mirnaFilterTF_Type2", "miRNA Filter", choices=NULL, multiple=TRUE),
                                                                 hr(),
                                                                 br(),
                                                                 selectizeInput("tfFilterTF_Type2", "TF Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 selectizeInput("targetFilterTF_Type2", "Target mRNA Filter", choices=NULL, multiple=TRUE),
                                                                 br(),
                                                                 sliderInput("Lancaster_XY_Z_rangeTF_Type2",
                                                                             label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                             value = c(0,0.01),
                                                                             min = 0,
                                                                             max = 0.01),
                                                                 hr(),
                                                                 #checkboxInput("tfActivator_Type2", "Show Activator TFs", value = TRUE, width = NULL),
                                                                 #checkboxInput("tfRepressor_Type2", "Show Repressor TFs", value = TRUE, width = NULL),
                                                                 #hr(),
                                                                 downloadButton("downloadDataTF_Type2", "Download")
                                                                 
                                                )
                                                
                                                
                                   ),
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Table", br(),
                                                          DT::dataTableOutput("table_TF", height = "800px"),
                                                          div(style = "margin-bottom:15px",
                                                              downloadButton("downloadData_TF", "Download")
                                                          )),
                                                 
                                                 tabPanel("Network",
                                                          shinycustomloader::withLoader(visNetworkOutput("vNetwork_TF", height = "100vh"),type = "html",loader="loader3"),
                                                          div(align = "right", downloadButton("downloadNodeTable_TF", "Download Node Table"),
                                                              downloadButton("downloadEdgeTable_TF", "Download Edge Table")),
                                                          br(),
                                                          br())
                                     )
                                   )
                                 )
                        ),
                        
                        
                        tabPanel("Pan-cancer Synergy Modules", value="Pan-cancerSynergyModules",fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "PanCancerSynergyModulesType", label = "Select Interaction Type",choices = c("Type 1","Type 2","Type 3"),selected = "Type 3",multiple = FALSE),
                                     conditionalPanel("input.PanCancerSynergyModulesType == 'Type 3'",
                                                      
                                                      shinyWidgets::pickerInput(inputId = "CommonSynergyModuleCancer_Type3", label = "Select Cancer Names",choices = cancerNames_Type3, selected=cancerNames_Type3, multiple = TRUE,options = pickerOptions(list(`actions-box` = TRUE))),
                                                      hr(),
                                                      #selectizeInput("mrnaSynergyModule_Type3", "mRNA Filter", choices=targetFilterCommonSynergyModules_Type3, multiple=TRUE),
                                                      br(),
                                                      selectizeInput("mirnaSynergyModule_Type3", "miRNA Filter", choices=mirnaFilterCommonSynergyModules_Type3, multiple=TRUE),
                                                      hr(),
                                                      br(),
                                                      downloadButton("downloadCommonTripletsData_TF", "Download")
                                     )
                                     
                                   ),
                                   
                                   mainPanel(
                                     useShinyalert(),
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Table", br(),
                                                          DT::dataTableOutput("tableCommonSynergyModules")),
                                                 tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonSynergyNetwork_TF", height = "100vh"),type = "html",loader="loader3"),
                                                          br(),
                                                          div(align = "right",downloadButton("downloadNodeTableCommonSynergyNetwork_TF", "Download Node Table"),
                                                              downloadButton("downloadEdgeTableCommonSynergyNetwork_TF", "Download Edge Table")),
                                                          br(),
                                                          br()
                                                 )
                                     )
                                   )
                                   
                                 )
                        ),
                        
                        tabPanel("Pan-cancer miRNA Pairs in Synergy Modules", value="Pan-cancermiRNASynergyModules",fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "PanCancerMirnaPairsSynergyModulesType", label = "Select Interaction Type",choices = c("Type 1","Type 2","Type 3"),selected = "Type 3",multiple = FALSE),
                                     conditionalPanel("input.PanCancerMirnaPairsSynergyModulesType == 'Type 3'",
                                                      
                                                      pickerInput(inputId = "CommonMirnaPairSynergyModuleCancer_Type3", label = "Select Cancer Names",choices = cancerNames_Type3, selected=cancerNames_Type3, multiple = TRUE),
                                                      hr(),
                                                      selectizeInput("mirnaCommonMirnaSynergyModulePair_Type3", "miRNA Filter", choices=mirnaListCommonMirnaPairs_Type3, multiple=TRUE),
                                                      hr(),
                                                      selectInput(inputId = "CommonMirnaPairSynergyModuleCancerCount_Type3", label = "Select with Count Above:",choices = c(1,2,3,4,5,6,7,8), selected=c(1), multiple = FALSE),
                                                      br(),
                                                      br(),
                                                      downloadButton("downloadCommonMirnaPairsData_TF", "Download")
                                     ) 
                                   ),
                                   
                                   mainPanel(
                                     useShinyalert(),
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPairSynergyModules")
                                                 ),
                                                 tabPanel("Network",
                                                          shinycustomloader::withLoader(visNetworkOutput("commonMirnaNetwork_TF", height = "100vh"),type = "html",loader="loader3"),
                                                          br(),
                                                          div(align = "right", downloadButton("downloadNodeTableCommonMirnaPair_TF", "Download Node Table"),
                                                              downloadButton("downloadEdgeTableCommonMirnaPair_TF", "Download Edge Table")),
                                                          br(),
                                                          br())
                                                 
                                                 
                                     )
                                   )
                                   
                                 )
                        )
                        
             ),
             
             tabPanel("Statistics", value = "Statistics", fluid= TRUE,
                      div(class = "statistics_general",
                        div(class = "total_counts_plot_exp_div",
                              div(class = "total_counts_plot_title", "Counts Across All Cancer Types"),
                              div(class = "total_counts_plot_exp", "Each bar represents statics for one cancer, from left to right: number of triplets found in the cancer, the number of unique miRNA pairs that participate in these triplets, number of miRNAs unique in the triplets, number of unique mRNAs.")
                            ),
                        div(class = "total_counts_plot",
                            shinycustomloader::withLoader(plotlyOutput("totalCountsPlot"),type = "html",loader="loader3")
                            ),
                        div(class = "mrna_heatmap_div",
                            div(class = "mrna_heatmap_title", "Most Frequent mRNAs Across All Cancers"),
                            div(class = "mrna_heatmap_exp","The heatmap shows the normalized number of triplets the mRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. mRNAs that have more than 20 total participation are shown.")
                            ),
                      div(class = "mrna_heatmap_plot",
                          shinycustomloader::withLoader(plotlyOutput("commonMrnaHeatmap",height=687),type = "html",loader="loader3")
                          ),
                      div(class = "mirna_heatmap_div",
                          div(class = "mirna_heatmap_title", "Most Frequent miRNAs Across All Cancers"),
                          div(class = "mirna_heatmap_exp","The heatmap shows the normalized number of triplets the miRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. miRNAs that have more than 50 total participation are shown.")
                          ),
                      div(class = "mirna_heatmap_plot",
                          shinycustomloader::withLoader(plotlyOutput("commonMirnaHeatmap",height=627),type = "html",loader="loader3")
                          ),
                      div(class = "scatter_exp_div",
                        div(class = "mrnaScatter_exp","The number of mRNAs the miRNA targets plotted against the number of triplets the mRNA is found in. The Pearson correlation is found 0.3429"),
                        div(class = "mirnaScatter_exp", "The number of miRNAs that target the mRNA plotted against the number of triplets the miRNA is found in. The Pearson correlation is found 0.63")
                        ),
                      div(class = "scatter_div",
                          div(class="mrna_scatter",shinycustomloader::withLoader(plotlyOutput("MrnaScatterPlot"),type = "html",loader="loader3")),
                          div(class="mirna_scatter",shinycustomloader::withLoader(plotlyOutput("MirnaScatterPlot"),type = "html",loader="loader3"))
                          )
                      ),
                      div(class = "footer_box_general_stats",
                        div(class = "footer_box_stats",
                            img(class = "footer_sabanci",
                                src = "sabanci.png"
                            ),
                            img(class = "footer_bilkent", src = "bilkent.png"),
                            div(class = "footer_text",
                                "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu")
                            )
                        )
             ),
             
             tabPanel("About",value="About",fluid = TRUE,
                      fluidRow(
                        column (12,
                                # h5(p(align = "justify;",style="font-size:16px;","miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their target's. To achieve this, a three-step method was proposed. First, miRNA pairs targeting the common mRNA were identified. For this, experimentally validated databases(miRTarBase, TarBase v7.0, miRecords ) and a prediction algorithm (TargetScan) were resorted. A miRNA-mRNA target catalogue was generated by intersecting miRNA-mRNA pairs from 3 experimentally validated databases with those from TargetScan. The triplets where miRNA pairs have an overlapping binding site on the target mRNA were filtered. miRNA pairs targeting common mRNA composed a potential triplet. Secondly, potential triplets obtained from Step 1 were eliminated according to the expression profiles of miRNAs and mRNAs. The rationale for the exclusion of the potential triplets is based on this assumption: The mRNA expression level is expected to be lower when both miRNAs are upregulated compared to when both miRNAs are downregulated. In the thid step, statistical interaction tests were performed on miRNA and mRNA expression data for each potential triplet candidate that passes through Step2. We are interested in cases where miRNAs are pairwise independent with mRNAs but form a mutually dependent triplet. ")
                                # ),
                                div(class = "about_general",
                                div(class = "about_1",
                                    p(class = "about_1_text",
                                      "miRCoop uses kernel-based statistical interaction tests, together with miRNA and mRNA target information to identify synergistic miRNA pairs that have weak or no repression on the target mRNA individually, but when act together, induce strong repression. We applied our approach to patient data of various The Cancer Genome Atlas Projects(TCGA) cancer types. miRCoop web-based user interface allows users to examine the potential triplet interactions.")
                                ),
                                br(),
                                div(class = "about_2",
                                    p(class = "about_2_text",
                                      "We believe miRCoop can aid our understanding of the complex regulatory interactions in different health and disease states of the cell and can help in designing miRNA-based therapies."),
                                    ),
                                div(
                                  img(class = "steps_image",src ="miRCoopSteps.png")
                                ),
                                div(class = "about_3",
                                    p(class = "about_3_text",
                                      "miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their targets. To achieve this, a three-step method was proposed. First, miRNA pairs targeting the common mRNA were identified. For this, experimentally validated databases(miRTarBase, TarBase v7.0, miRecords ) and a prediction algorithm (TargetScan) were resorted. A miRNA-mRNA target catalogue was generated by intersecting miRNA-mRNA pairs from 3 experimentally validated databases with those from TargetScan. The triplets where miRNA pairs have an overlapping binding site on the target mRNA were filtered. miRNA pairs targeting common mRNA composed a potential triplet. Secondly, potential triplets obtained from Step 1 were eliminated according to the expression profiles of miRNAs and mRNAs. The rationale for the exclusion of the potential triplets is based on this assumption: The mRNA expression level is expected to be lower when both miRNAs are upregulated compared to when both miRNAs are downregulated. In the thid step, statistical interaction tests were performed on miRNA and mRNA expression data for each potential triplet candidate that passes through Step2. We are interested in cases where miRNAs are pairwise independent with mRNAs but form a mutually dependent triplet. ")
                                ),
                                br(),
                                div(
                                    img(class = "web_image", src = "mircoop_web.png")
                                ),
                                div(class = "about_4",
                                    p(class = "about_4_text",
                                      "The triplets detected for 31 different cancers can be examined in the ‘Cancer Specific Triplets’ screen. It takes pre-computed results as data source and presents themit to the user with a datatable in a structured manner. This section essentially builds on the identified triplets, miRNA pairs and their target mRNA, which is represented with both HGNC symbol and Entrez Gene ID, and triplet p-values. The data has been enriched with the following additional information:", br(),br(),"› Experimental data source of miRNA and mRNA relationships",br(),  "› Differential expression analysis results: Differential expression analysis was performed by comparing the expression of each miRNAs and mRNAs between Primary Tumor samples and Solid Tissue Normal samples. Positive logFC values indicate upregulation in the primary tumor samples compared to solid tissue normal samples. In contrast, a negative logFC value shows downregulation in the primary tumor samples compared to solid tissue normal. A p-value < 0.05 is selected as cut-off criteria to define statistically significant difference for the logFC",br(),"› Literature support of cancer-miRNA and cancer-mRNA relationship",br(), "› mRNA expressions of patients grouped by miRNA expression levels", br(),"› Transcription factor information")
                                ),
                                div(class = "about_cite_box",
                                    div(class = "about_cite_inside",
                                      p(class = "about_cite_line1", "If you find miRCoop useful for your research, please cite the following papers:"),
                                      p(class = "about_cite_line2",
                                        a(href="https://ieeexplore.ieee.org/document/9311836","miRCoop: Identifying Cooperating miRNAs via Kernel Based Interaction Tests")
                                        ),
                                      p(class = "about_cite_line3",
                                        "G. Olgun and O. Tastan, IEEE/ACM Transactions on Computational Biology and Bioinformatics, doi: 10.1109/TCBB.2020.3047901."
                                        )
                                    )
                                    )
                                ),
                                div(class = "footer_box_general_about",
                                  div(class = "footer_box",
                                      img(class = "footer_sabanci",
                                          src = "sabanci.png"
                                          ),
                                      img(class = "footer_bilkent", src = "bilkent.png"),
                                      div(class = "footer_text",
                                        "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu")
                                      )
                                )
                                
                        ),
                        
                      ),
             ),
             tabPanel("Glossary", value="Glossary",fluid = TRUE,
                      
                      div(class = "glossary_general",
                        fluidRow(
                          column(6,
                                 h4(p("Glossary")),
                                 tags$div(style="font-size:14px;",tableOutput("Glossary"))
                          ),
                          column(6, 
                                 h4(p("Abbreviations and Full Names of TCGA Projects")),
                                 tags$div(style="font-size:14px;",tableOutput("TCGAAbbrv"))
                          )
                        )
                      ),
                      div(class = "footer_box_general_glossary",
                            div(class = "footer_box_glossary",
                                img(class = "footer_sabanci",
                                    src = "sabanci.png"
                                ),
                                img(class = "footer_bilkent", src = "bilkent.png"),
                                p(class = "footer_text",
                                  "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu"
                                )
                            )
                        )
             )
             
  )
  
  
)


