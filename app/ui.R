library(shiny)
library(semantic.dashboard)
library(shiny.semantic)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(sourcetools)
library(fs)
library(shinyWidgets)
library(shinyjs)

shinyUI(dashboardPage(skin = "purple",
    dashboardHeader(title = "Cálculadora de Probabilidades", titleWidth = 350),
    dashboardSidebar(sidebarMenu(
        menuItem(text = "Binomial", tabName = "binomial", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Poisson", tabName = "poisson", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Geométrica", tabName = "geometrica", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Hipergeométrica", tabName = "hipergeometrica", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Binomial Negativa", tabName = "negative_binom", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Uniforme", tabName = "uniforme", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Exponencial", tabName = "exponencial", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Gama", tabName = "gamma", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Beta", tabName = "beta", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Normal", tabName = "normal", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "t de Student", tabName = "t_student", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "Qui Quadrado", tabName = "qui_quadrado", icon = icon(name = "stats", class = "fas fa-chart-bar")),
        menuItem(text = "F de Snedecor", tabName = "f_snedecor", icon = icon(name = "stats", class = "fas fa-chart-bar"))
    )),
    
    dashboardBody(
        tags$style(HTML("


                .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#222d32
                }

                ")),
        tabItems(
            ## binomial -------------------------------------------------------
            tabItem(tabName = "binomial", h2("Função massa de probabilidade Binomial"),
                br(),
                withMathJax(),
                tags$strong(h4("$$\\mathbb{P}[X=x]=\\binom{n}{x} p^{x}(1-p)^{n-x}, 0 \\leq x \\leq n$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "n_value",
                            label = tags$strong("n", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        ),
                        numericInput(
                            inputId = "p_value",
                            label = tags$strong("p", style = "color:white"),
                            min = 0,
                            max = 1,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_binom",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            min = 0,
                            step = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = n \\cdot p = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("binom_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = n \\cdot p \\cdot (1 - p) = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("binom_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("binom_prob"),  style = "color:white"))
                    )
                )
            ),
            ## poisson --------------------------------------------------------
            tabItem(tabName = "poisson", h2("Função massa de probabilidade Poisson"),
                br(),
                withMathJax(),
                tags$strong(h4("$$\\mathbb{P}[X=x]=\\frac{e^{-\\lambda} \\cdot \\lambda^x}{x!}, \\forall x \\in \\mathbb{Z}_{+}$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "lam_value_poiss",
                            label = tags$strong(withMathJax("\\( \\lambda \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_poiss",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            min = 0,
                            step = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\lambda = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("poiss_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\lambda = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("poiss_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("poiss_prob"),  style = "color:white"))
                    )
                )
            ),
            ## geometrica ------------------------------------------------------
            tabItem(tabName = "geometrica", h2("Função massa de probabilidade Geométrica"),
                br(),
                withMathJax(),
                tags$strong(h4("$$\\mathbb{P}[X=x]= p \\cdot (1 - p)^x, \\forall x \\in \\mathbb{Z}_{+}$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "p_value_geom",
                            label = tags$strong("p", style = "color:white"),
                            min = 0,
                            max = 1,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_geom",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            min = 0,
                            step = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{1-p}{p} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("geom_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{1-p}{p^2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("geom_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("geom_prob"),  style = "color:white"))
                    )
                )
            ),
            ## hipergeometrica -------------------------------------------------
            tabItem(tabName = "hipergeometrica", h2("Função massa de probabilidade Hipergeométrica"),
                br(),
                withMathJax(),
                tags$strong(h4("$$\\mathbb{P}[X=x]=\\frac{\\binom{M}{x} \\binom{N-M}{n-x}}{\\binom{N}{n}}, \\max \\{0, n-(N-M)\\} \\leq x \\leq \\min \\{M, n\\}$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "N_value_hgeom",
                            label = tags$strong("N", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        ),
                        numericInput(
                            inputId = "n_value_hgeom",
                            label = tags$strong("n", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        ),
                        numericInput(
                            inputId = "M_value_hgeom",
                            label = tags$strong("M", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_hgeom",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            min = 0,
                            step = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = n \\cdot \\frac{M}{N} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("hgeom_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = n \\frac{M}{N} \\frac{(N-M)}{N}\\left(1-\\frac{n-1}{N-1}\\right) = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("hgeom_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("hgeom_prob"),  style = "color:white"))
                    )
                )
            ),
            ## binom negativa --------------------------------------------------
            tabItem(tabName = "negative_binom", h2("Função massa de probabilidade Binomial Negativa"),
                br(),
                withMathJax(),
                tags$strong(h4("$$\\mathbb{P}[X=x]= \\binom{x - 1}{k - 1} p^{k}(1-p)^{x-k}; x = k, k + 1, k + 2, \\cdots$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "k_value_neg_binom",
                            label = tags$strong("k", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        ),
                        numericInput(
                            inputId = "p_value_neg_binom",
                            label = tags$strong("p", style = "color:white"),
                            min = 0,
                            max = 1,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_neg_binom",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            min = 0,
                            step = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{k}{p} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("neg_binom_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{k(1-p)}{p^2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("neg_binom_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("neg_binom_prob"),  style = "color:white"))
                    )
                )
            ),
            ## Uniforme --------------------------------------------------------
            tabItem(tabName = "uniforme", h2("Função densidade de probabilidade Uniforme"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\left\\{\\begin{array}{l}
                         \\frac{1}{b-a}, \\text { se } a \\leq x \\leq b \\\\
                         0, \\text { caso contrário }
                         \\end{array}\\right.$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "a_uniform_value",
                            label = tags$strong("a", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 1
                        ),
                        numericInput(
                            inputId = "b_uniform_value",
                            label = tags$strong("b", style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_uniform",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{a + b}{2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("uniform_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{(b-a)^2}{12} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("uniform_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("uniform_prob"),  style = "color:white"))
                    )
                )
            ),
            ## exponencial -----------------------------------------------------
            tabItem(tabName = "exponencial", h2("Função densidade de probabilidade Exponencial"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\left\\{\\begin{array}{l}
                        \\lambda e^{-\\lambda x} \\text {, se } x \\geq 0 \\\\
                        0 \\text {, caso contrário }
                        \\end{array}\\right.$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "lambda_exp_value",
                            label = tags$strong(withMathJax("\\( \\lambda \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_exp",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{1}{\\lambda} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("exp_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{1}{\\lambda^2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("exp_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("exp_prob"),  style = "color:white"))
                    )
                )
            ),
            ## gama ------------------------------------------------------------
            tabItem(tabName = "gamma", h2("Função densidade de probabilidade Gama"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\left\\{\\begin{array}{l}
                    \\frac{\\beta^{\\alpha} x^{\\alpha-1} e^{-\\beta x}}{\\Gamma(\\alpha)} 
                    \\text {, se } x \\geq 0 \\\\
                    0, \\text { caso contrário }
                    \\end{array}\\right.$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "alpha_gama_value",
                            label = tags$strong(withMathJax("\\( \\alpha \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        ),
                        numericInput(
                            inputId = "beta_gama_value",
                            label = tags$strong(withMathJax("\\( \\beta \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_gama",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{\\alpha}{\\beta} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("gamma_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{\\alpha}{\\beta^2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("gamma_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("gamma_prob"),  style = "color:white"))
                    )
                )
            ),
            ## beta ------------------------------------------------------------
            tabItem(tabName = "beta", h2("Função densidade de probabilidade Beta"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\left\\{\\begin{array}{l}
                    \\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha) \\Gamma(\\beta)} x^{\\alpha-1}(1-x)^{\\beta-1}
                    \\text {, se } x \\in (0,1) \\\\
                    0, \\text { caso contrário }
                    \\end{array}\\right.$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "alpha_beta_value",
                            label = tags$strong(withMathJax("\\( \\alpha \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        ),
                        numericInput(
                            inputId = "beta_beta_value",
                            label = tags$strong(withMathJax("\\( \\beta \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_beta",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{\\alpha}{\\beta} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("beta_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{\\alpha}{\\beta^2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("beta_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("beta_prob"),  style = "color:white"))
                    )
                )
            ),
            ## normal ----------------------------------------------------------
            tabItem(tabName = "normal", h2("Função densidade de probabilidade Normal"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\frac{1}{\\sqrt{2 \\pi \\sigma^{2}}} \\exp
                      \\left[-\\frac{1}{2}\\left(\\frac{x-\\mu}{\\sigma}\\right)^{2}\\right],
                      \\forall x \\in \\mathbb{R}$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "mu_norm_value",
                            label = tags$strong(withMathJax("\\( \\mu \\)"), style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        ),
                        numericInput(
                            inputId = "sigma2_norm_value",
                            label = tags$strong(withMathJax("\\( \\sigma^2 \\)"), style = "color:white"),
                            min = 0,
                            value = 1,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_norm",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\mu = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("norm_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\sigma^2 = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("norm_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("norm_prob"),  style = "color:white"))
                    )
                )
            ),
            ## t de student ----------------------------------------------------
            tabItem(tabName = "t_student", h2("Função densidade de probabilidade t de Student"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\frac{\\Gamma\\left(\\frac{\\nu+1}{2}\\right)}{
                     \\sqrt{\\nu \\pi \\Gamma\\left(\\frac{\\nu}{2}\\right)}}
                     \\left(1+\\frac{x^{2}}{\\nu}\\right)^{-\\left(\\frac{\\nu+1}{2}\\right)}
                     , \\forall x \\in \\mathbb{R}$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "nu_student_value",
                            label = tags$strong(withMathJax("\\(\\nu\\text{ (Número de graus de liberdade)}\\)"),
                                                style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_student",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\mu = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("student_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\sigma^2 = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("student_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("student_prob"),  style = "color:white"))
                    )
                )
            ),
            ## qui quadrado ----------------------------------------------------
            tabItem(tabName = "qui_quadrado", h2("Função densidade de probabilidade Qui Quadrado"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\frac{1}{2^{\\nu / 2} \\Gamma(\\nu / 2)} x^{(v / 2)-1} e^ \\left(-\\frac{x}{2}\\right) ; x>0$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "nu_chisq_value",
                            label = tags$strong(withMathJax("\\(\\nu\\text{ (Número de graus de liberdade)}\\)"),
                                                style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_chisq",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\nu = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("chisq_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = 2 \\cdot \\nu = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("chisq_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("chisq_prob"),  style = "color:white"))
                    )
                )
            ),
            ## F de Snedor -----------------------------------------------------
            tabItem(tabName = "f_snedecor", h2("Função densidade de probabilidade F de Snedecor"),
                br(),
                withMathJax(),
                tags$strong(h4("$$f(x)=\\frac{\\Gamma\\left[\\frac{m+n}{2}\\right]\\left(\\frac{m}{n}\\right)^{\\frac{m}{2}} x^{\\frac{m}{2}-1}}{\\Gamma\\left[\\frac{m}{2}\\right] \\Gamma\\left[\\frac{n}{2}\\right]\\left[\\left(\\frac{m}{n}\\right) x+1\\right]^{\\frac{m+n}{2}}}; x > 0$$")),
                br(),
                fluidRow(
                    box(
                        width = 6, title = "Parâmetros", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "m_snedecor_value",
                            label = tags$strong(withMathJax("\\(m \\text{ (Graus de liberdade numerador)}\\)"),
                                                style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        ),
                        numericInput(
                            inputId = "n_snedecor_value",
                            label = tags$strong(withMathJax("\\(n \\text{ (Graus de liberdade denominador)}\\)"),
                                                style = "color:white"),
                            min = 0,
                            value = 0,
                            step = 0.01
                        )
                    ),
                    box(
                        width = 6, title = "Probabilidade", status = "primary", solidHeader = TRUE,
                        numericInput(
                            inputId = "quantil_snedecor",
                            label = tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\): Informe x"), style = "color:white"),
                            value = 0,
                            step = 0.01
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 6, title = "Esperança", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{E}[X] = \\frac{n}{n-2} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("snedecor_esp"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Variância", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{Var}[X] = \\frac{2 n^{2}(m+n-2)}{m(n-2)^{2}(n-4)} = \\)"), style = "color:white"),
                        h4(tags$strong(textOutput("snedecor_var"),  style = "color:white"))
                    ),
                    box(
                        width = 6, title = "Resultado", status = "primary", solidHeader = TRUE,
                        tags$strong(withMathJax("\\(\\mathbb{P}[X \\leq x]\\) ="), style = "color:white"),
                        h4(tags$strong(textOutput("snedecor_prob"),  style = "color:white"))
                    )
                )
            )
            ## fim da F --------------------------------------------------------
        )
    )
))
