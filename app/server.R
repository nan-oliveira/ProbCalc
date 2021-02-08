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

shinyServer(function(input, output) {

    ## binom -------------------------------------------------------------------
    esp_binom <- reactive({
        return(input$n_value * input$p_value)
    })
    var_binom <- reactive({
        return(input$n_value * input$p_value * (1 - input$p_value))
    })
    prob_binom <- reactive({
        return(pbinom(input$quantil_binom, input$n_value, input$p_value, TRUE))
    })
    output[["binom_esp"]] <- renderText(esp_binom())
    output[["binom_var"]] <- renderText(var_binom())
    output[["binom_prob"]] <- renderText(prob_binom())
    
    ## poiss -------------------------------------------------------------------
    esp_poiss <- reactive({
        return(input$lam_value_poiss)
    })
    prob_poiss <- reactive({
        return(ppois(input$quantil_poiss, input$lam_value_poiss, TRUE))
    })
    output[["poiss_esp"]] <- renderText(esp_poiss())
    output[["poiss_var"]] <- renderText(esp_poiss())
    output[["poiss_prob"]] <- renderText(prob_poiss())
    
    ## geom --------------------------------------------------------------------
    esp_geom <- reactive({
        return((1-input$p_value_geom)/input$p_value_geom)
    })
    var_geom <- reactive({
        return((1-input$p_value_geom)/input$p_value_geom^2)
    })
    prob_geom <- reactive({
        return(pgeom(input$quantil_geom, input$p_value_geom, TRUE))
    })
    output[["geom_esp"]] <- renderText(esp_geom())
    output[["geom_var"]] <- renderText(var_geom())
    output[["geom_prob"]] <- renderText(prob_geom())
    
    ## hiper geom --------------------------------------------------------------
    esp_hgeom <- reactive({
        return((input$n_value_hgeom * input$M_value_hgeom) / input$N_value_hgeom)
    })
    var_hgeom <- reactive({
        part_1 <- (input$n_value_hgeom * input$M_value_hgeom * 
                       (input$N_value_hgeom - input$M_value_hgeom)) / input$N_value_hgeom^2
        part_2 <- 1 - (input$n_value_hgeom - 1)/(input$N_value_hgeom - 1)
        return(part_1 * part_2)
    })
    prob_hgeom <- reactive({
        return(phyper(input$quantil_hgeom, input$M_value_hgeom, 
                      input$N_value_hgeom - input$M_value_hgeom, 
                      input$n_value_hgeom, TRUE))
    })
    output[["hgeom_esp"]] <- renderText(esp_hgeom())
    output[["hgeom_var"]] <- renderText(var_hgeom())
    output[["hgeom_prob"]] <- renderText(prob_hgeom())
    
    ## Binomial Negativa--------------------------------------------------------
    esp_neg_binom <- reactive({
        return(input$k_value_neg_binom / input$p_value_neg_binom)
    })
    var_neg_binom <- reactive({
        return((input$k_value_neg_binom * (1-input$p_value_neg_binom)) /
                   input$p_value_neg_binom^2)
    })
    prob_neg_binom <- reactive({
        # função massa de probabilidade da dist binom negativa
        fmp <- function(x, k, p){
            return(choose(x-1, k-1) * p^k * (1-p)^(x-k))
        }
        
        # P[X <= x] da binom negativa
        pnegbinom <- function(q, k, p){
            sum(fmp(k:q, k, p))
        }
        
        return(pnegbinom(input$quantil_neg_binom, input$k_value_neg_binom,
                         input$p_value_neg_binom))
    })
    output[["neg_binom_esp"]] <- renderText(esp_neg_binom())
    output[["neg_binom_var"]] <- renderText(var_neg_binom())
    output[["neg_binom_prob"]] <- renderText(prob_neg_binom())
    
    ## uniforme ----------------------------------------------------------------
    esp_uniform <- reactive({
        return((input$a_uniform_value + input$b_uniform_value)/2)
    })
    var_uniform <- reactive({
        return((input$b_uniform_value - input$a_uniform_value)^2 /12)
    })
    prob_uniform <- reactive({
        return(punif(input$quantil_uniform, input$a_uniform_value,
                     input$b_uniform_value))
    })
    output[["uniform_esp"]] <- renderText(esp_uniform())
    output[["uniform_var"]] <- renderText(var_uniform())
    output[["uniform_prob"]] <- renderText(prob_uniform())
    
    ## exponencial -------------------------------------------------------------
    esp_exp <- reactive({
        return(1/input$lambda_exp_value)
    })
    var_exp <- reactive({
        return(1/(input$lambda_exp_value)^2)
    })
    prob_exp <- reactive({
        return(pexp(input$quantil_exp, input$lambda_exp_value))
    })
    output[["exp_esp"]] <- renderText(esp_exp())
    output[["exp_var"]] <- renderText(var_exp())
    output[["exp_prob"]] <- renderText(prob_exp())
    
    ## gama --------------------------------------------------------------------
    esp_gamma <- reactive({
        return(input$alpha_gama_value / input$beta_gama_value)
    })
    var_gamma <- reactive({
        return(input$alpha_gama_value / input$beta_gama_value^2)
    })
    prob_gamma <- reactive({
        return(pgamma(input$quantil_gama, input$alpha_gama_value,
                      1/input$beta_gama_value))
    })
    output[["gamma_esp"]] <- renderText(esp_gamma())
    output[["gamma_var"]] <- renderText(var_gamma())
    output[["gamma_prob"]] <- renderText(prob_gamma())
    
    ## beta --------------------------------------------------------------------
    esp_beta <- reactive({
        return(input$alpha_beta_value / 
                   (input$alpha_beta_value + input$beta_beta_value))
    })
    var_beta <- reactive({
        return((input$alpha_beta_value * input$beta_beta_value)/
                   ((input$alpha_beta_value + input$beta_beta_value + 1) *
                        (input$alpha_beta_value + input$beta_beta_value)^2))
    })
    prob_beta <- reactive({
        return(pbeta(input$quantil_beta, input$alpha_beta_value,
                     input$beta_beta_value))
    })
    output[["beta_esp"]] <- renderText(esp_beta())
    output[["beta_var"]] <- renderText(var_beta())
    output[["beta_prob"]] <- renderText(prob_beta())
    
    ## normal ------------------------------------------------------------------
    esp_norm <- reactive({
        return(input$mu_norm_value)
    })
    var_norm <- reactive({
        return(input$sigma2_norm_value)
    })
    prob_norm <- reactive({
        return(pnorm(input$quantil_norm, input$mu_norm_value,
                     sqrt(input$sigma2_norm_value)))
    })
    output[["norm_esp"]] <- renderText(esp_norm())
    output[["norm_var"]] <- renderText(var_norm())
    output[["norm_prob"]] <- renderText(prob_norm())

    ## t de Student ------------------------------------------------------------
    esp_student <- reactive({
        ifelse(input$nu_student_value > 1, return(0), return(NaN))
    })
    var_student <- reactive({
        ifelse(input$nu_student_value > 2, 
               return(
                   input$nu_student_value/(input$nu_student_value - 2)
               ),
               return(NaN))
    })
    prob_student <- reactive({
        return(pt(input$quantil_student, input$nu_student_value))
    })
    output[["student_esp"]] <- renderText(esp_student())
    output[["student_var"]] <- renderText(var_student())
    output[["student_prob"]] <- renderText(prob_student())
    
    ## Qui Quadrado ------------------------------------------------------------
    esp_chisq <- reactive({
        return(input$nu_chisq_value)
    })
    var_chisq <- reactive({
        return(2 * input$nu_chisq_value)
    })
    prob_chisq <- reactive({
        return(pchisq(input$quantil_chisq, input$nu_chisq_value))
    })
    output[["chisq_esp"]] <- renderText(esp_chisq())
    output[["chisq_var"]] <- renderText(var_chisq())
    output[["chisq_prob"]] <- renderText(prob_chisq())
    
    ## F de Snedecor ------------------------------------------------------------
    esp_snedecor <- reactive({
        ifelse(input$n_snedecor_value > 2,
               return(input$n_snedecor_value/(input$n_snedecor_value-2)),
               return(NaN))
    })
    var_snedecor <- reactive({
        num <- 2 * input$n_snedecor_value^2 * 
            (input$m_snedecor_value + input$n_snedecor_value - 2)
        den <- input$m_snedecor_value * (input$n_snedecor_value - 2)^2 *
            (input$n_snedecor_value - 4)
        ifelse(input$n_snedecor_value > 4, return(num/den), return(NaN))
    })
    prob_snedecor <- reactive({
        return(pf(input$quantil_snedecor, input$n_snedecor_value,
                  input$m_snedecor_value))
    })
    output[["snedecor_esp"]] <- renderText(esp_snedecor())
    output[["snedecor_var"]] <- renderText(var_snedecor())
    output[["snedecor_prob"]] <- renderText(prob_snedecor())
})
