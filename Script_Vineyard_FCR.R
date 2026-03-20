rm(list = ls())

install.packages("devtools")
install.packages("nlme")
install.packages("emmeans")
install.packages("lme4")
install.packages("drc")
install.packages("MASS")
install.packages("reshape")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("multcomp")
install.packages("multcompView")
install.packages("gtacle")
install.packages("vctrs")
install.packages("ggrepel")
install.packages("rlang")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("PerformanceAnalytics")
install.packages("permute")
install.packages("lattice")
install.packages("RColorBrewer")
install.packages ("gam")
install.packages('mgcv')
install.packages('agricolae')
install.packages('printr')
install.packages('relaimpo')
install.packages('ggforce')
install.packages("svglite")
install.packages("carData")
library(svglite)
library(nlme)
library(lme4)
#library(tidyverse)
library(readxl)
library(ggplot2)
library(multcomp) 
library(emmeans)
library(scales)
library(ggpubr)
library("xlsx")
library("openxlsx")
#remove.packages(rlang)
#library(dplyr)
library(factoextra)
library(FactoMineR)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)
library(vegan)
library(agricolae)
library(mgcv)
library(ggplot2)
library(gam)
library(print)


  
  #____________________________Bulk Density___________________#

 #  MIXED MODEL WITH HIERARCHICAL STRUCTURE
  {
   
    # =========================
    # BULK DENSITY - VINEYARD
    # Position × Layer
    # Random: SamplingPoint nested within VineRow
    # =========================
    
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    library(openxlsx)
    
    DF <- read_excel(
      "...../Data_All_Vin_FCR.xlsx",
      sheet = "Risultats_OK_Vineyard"
    )
    
    
    
    # Check names
    str(DF)
    head(DF)
    
    # Convert variables to factors
    DF$Posizione     <- factor(DF$Posizione)   # row / inter-row
    DF$Layer         <- factor(DF$Layer, levels = c("First", "Second", "Third"))
    DF$VineRow       <- factor(DF$VineRow)
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # Mixed model
    mod_vine <- lmer(
      Bulk_Density_Mgha ~ Posizione * Layer + (1 | VineRow/SamplingPoint),
      data = DF,
      REML = TRUE
    )
    
    summary(mod_vine)
    
    # Type III ANOVA
    anova_vine <- Anova(mod_vine, type = 3)
    print(anova_vine)
    
    # Estimated marginal means
    # If interaction is of interest, use the interaction
    emm_vine <- emmeans(mod_vine, ~ Layer)
    print(emm_vine)
    
    # Pairwise comparisons with Sidak adjustment
    pairs_vine <- pairs(emm_vine, adjust = "sidak")
    print(pairs_vine)
    
    # Compact letter display
    cld_vine <- cld(emm_vine, Letters = letters, reverse = TRUE, adjust = "sidak")
    results_vine <- as.data.frame(cld_vine)
    results_vine
    
    
    # =========================
    # BULK DENSITY - FCR
    # Layer
    # Random: SamplingPoint nested within Transect
    # =========================
    
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    library(openxlsx)
    
    DF <- read_excel(
      "C:/Users/giorg/Desktop/R_Directory/Paper_Vigneto/Dati_All_Vigneto.xlsx",
      sheet = "Results_OK_Concentr_Wheat"
    )
    
    str(DF)
    head(DF)
    
    # Convert variables to factors
    DF$Layer         <- factor(DF$Layer, levels = c("Second", "Third"))
    DF$Transect      <- factor(DF$Transect)
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # Mixed model
    mod_fcr <- lmer(
      Bulk_Density_Mgha ~ Layer + (1 | Transect/SamplingPoint),
      data = DF,
      REML = TRUE
    )
    
    summary(mod_fcr)
    
    # Type III ANOVA
    anova_fcr <- Anova(mod_fcr, type = 3)
    print(anova_fcr)
    
    # Estimated marginal means
    emm_fcr <- emmeans(mod_fcr, ~ Layer)
    print(emm_fcr)
    
    # Pairwise comparisons with Sidak adjustment
    pairs_fcr <- pairs(emm_fcr, adjust = "sidak")
    print(pairs_fcr)
    
    # Compact letter display
    cld_fcr <- cld(emm_fcr, Letters = letters, reverse = TRUE, adjust = "sidak")
    results_fcr <- as.data.frame(cld_fcr)
    results_fcr
    
    # Save
    write.xlsx(
      results_fcr,
      "C:/Users/Giorgia/Desktop/R_Directory/Paper_Vigneto/Risultati/BD_Wheat.xlsx",
      rowNames = FALSE
    )

  }
  
#_________________________Elements Concentrations______#
{
                 #  Vineyard - MIXED MODEL WITH HIERARCHICAL STRUCTURE
                  {
    
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    library(ggplot2)
    
    # =========================
    # LOAD DATA
    # =========================
    DF <- read_excel(
      "...../Data_All_Vin_FCR.xlsx",
      sheet = "Risultats_OK_Vineyard"
    )
    
    head(DF)
    str(DF)
    
    # =========================
    # FACTORS
    # =========================
    DF$Posizione <- factor(DF$Posizione)
    DF$Layer     <- factor(DF$Layer, levels = c("First", "Second", "Third"),
                           labels = c("First", "Second", "Third"))
    DF$VineRow <- factor(DF$VineRow)
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Unique sampled-location ID
    DF$PointID <- interaction(DF$VineRow, DF$SamplingPoint, drop = TRUE)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # =========================
    # VARIABLES TO ANALYZE
    # =========================
    vars <- c("Corg_PercSS", "Cu_mgkgSS", "IC_Solfati_mgkgSS", "Fe_mgkgSS",
              "TKN_PercSS", "IC_Nitrati_mgkgSS", "IC_Potassio_mgkgSS",
              "IC_Fosfati_mgkgSS", "ICPCa_mgkgSS", "ICPK_mgkgSS", "ICPMg_mgkgSS")
    
    # =========================
    # OBJECTS TO STORE RESULTS
    # =========================
    all_models <- list()
    all_anova  <- list()
    all_emm    <- list()
    all_cld    <- list()
    
    # =========================
    # LOOP OVER VARIABLES
    # =========================
    for (v in vars) {
      
      cat("\n\n====================================================\n")
      cat("VARIABLE:", v, "\n")
      cat("====================================================\n")
      
      # Full model
      form_full <- as.formula(
        paste(v, "~ Posizione * Layer + (1 | VineRow/PointID)")
      )
      
      mod_full <- lmer(form_full, data = DF, REML = TRUE)
      
      sing <- isSingular(mod_full, tol = 1e-4)
      cat("Singular fit:", sing, "\n")
      
      # Simplify if singular
      if (sing) {
        form_use <- as.formula(
          paste(v, "~ Posizione * Layer + (1 | PointID)")
        )
        mod_use <- lmer(form_use, data = DF, REML = TRUE)
        cat("Model used: ", deparse(form_use), "\n")
      } else {
        mod_use <- mod_full
        cat("Model used: ", deparse(form_full), "\n")
      }
      
      # Store model
      all_models[[v]] <- mod_use
      
      # Print model summary and variance components
      print(summary(mod_use))
      print(VarCorr(mod_use))
      
      # Type III ANOVA
      anova_res <- Anova(mod_use, type = 3)
      all_anova[[v]] <- anova_res
      
      cat("\n--- Type III ANOVA ---\n")
      print(anova_res)
      
      # Extract p-values safely
      p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
      
      p_int <- if ("Posizione:Layer" %in% rownames(anova_res)) anova_res["Posizione:Layer", p_col] else NA
      p_pos <- if ("Posizione" %in% rownames(anova_res)) anova_res["Posizione", p_col] else NA
      p_lay <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
      
      # =========================
      # EMMs + CLD
      # =========================
      if (!is.na(p_int) && p_int < 0.05) {
        
        cat("\nInteraction significant: post hoc on Posizione × Layer\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
      } else if (!is.na(p_pos) && p_pos < 0.05 && !is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nBoth main effects significant, interaction not significant.\n")
        cat("Post hoc on Posizione:\n")
        emm_pos <- emmeans(mod_use, ~ Posizione)
        cld_pos <- cld(emm_pos, Letters = letters, reverse = TRUE, adjust = "sidak")
        print(cld_pos)
        
        cat("\nPost hoc on Layer:\n")
        emm_lay <- emmeans(mod_use, ~ Layer)
        cld_lay <- cld(emm_lay, Letters = letters, reverse = TRUE, adjust = "sidak")
        print(cld_lay)
        
        emm_res <- list(Posizione = emm_pos, Layer = emm_lay)
        cld_res <- list(Posizione = cld_pos, Layer = cld_lay)
        
      } else if (!is.na(p_pos) && p_pos < 0.05) {
        
        cat("\nMain effect significant: Posizione\n")
        emm_res <- emmeans(mod_use, ~ Posizione)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
      } else if (!is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nMain effect significant: Layer\n")
        emm_res <- emmeans(mod_use, ~ Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
      } else {
        
        cat("\nNo significant fixed effects at p < 0.05\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- NULL
      }
      
      # Store results
      all_emm[[v]] <- emm_res
      all_cld[[v]] <- cld_res
      
      # Print EMMs and letters
      cat("\n--- Estimated marginal means ---\n")
      print(emm_res)
      
      cat("\n--- Compact letter display ---\n")
      print(cld_res)
    }
    
    # =========================
    # EXAMPLES OF HOW TO VIEW RESULTS AFTER THE LOOP
    # =========================
    
    #"Corg_PercSS", "Cu_mgkgSS", "IC_Solfati_mgkgSS", "Fe_mgkgSS",
    #"TKN_PercSS", "IC_Nitrati_mgkgSS", "IC_Potassio_mgkgSS",
    #"IC_Fosfati_mgkgSS", "ICPCa_mgkgSS", "ICPK_mgkgSS", "ICPMg_mgkgSS" 
    
    # View ANOVA for one variable
    all_anova$Cu_mgkgSS
    all_anova$Corg_PercSS
    
    # View post hoc letters for one variable
    all_cld$Cu_mgkgSS
    all_cld$Corg_PercSS
    
    # Example if both main effects were tested separately:
    # all_cld$SomeVariable$Posizione
    # all_cld$SomeVariable$Layer
    
    
    # PLOT PAPER
    
    DF <- read_excel("C:/Users/Giorgia/Desktop/R_Directory/Paper_Vigneto/Dati_All_Vigneto.xlsx",
                     sheet = "Risultati_OK_Concentr_Vineyard")
    # Load package
    library(ggplot2)
    
    # Relabel Layer factor
    DF$Layer <- factor(DF$Layer, 
                       levels = c("First", "Second", "Third"),
                       labels = c("0-10", "10-20", "20-40"))
    
    # Plot
    ggplot(DF, aes(x = Layer, y = Cu_mgkgSS, fill = Posizione)) +
      geom_boxplot(position = position_dodge(width = 0.8),
                   color = "black") +
      geom_jitter(aes(shape = Posizione),
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                  alpha = 0.6, size = 2, color = "black") +
      labs(x = "Soil depth (cm)", 
           y = expression(Cu~mg~kg^{-1})) +
      scale_fill_grey(start = 0.3, end = 0.8, name = "Position") +
      theme_classic(base_size = 14) +
      theme(legend.position = c(0.80, 0.85),  # explicit position (x, y)
            axis.line = element_line(size = 0.5, colour = "black"))
    
    #==================================
    # New Plot con solo Layer come variabile significativa e non anche posizione
    
    # Boxplot per Layer (no Posizione)
    
    library(readxl)
    library(ggplot2)
    
    DF <- read_excel("C:/Users/giorg/Desktop/R_Directory/Paper_Vigneto/Dati_All_Vigneto.xlsx",
                     sheet = "Risultati_OK_Concentr_Vineyard")
    head(DF)
    View(DF)
    
    
    # Relabel Layer factor
    DF$Layer <- factor(DF$Layer, 
                       levels = c("First", "Second", "Third"),
                       labels = c("0-10", "10-20", "20-40"))
    # Plot
    ggplot(DF, aes(x = Layer, y = IC_Solfati_mgkgSS)) +
      geom_boxplot(color = "black", fill = "grey70") +
      #     geom_jitter(position = position_jitter(width = 0.2),
      #            alpha = 0.6, size = 2, color = "black") +
      #  stat_summary(fun = mean,
      #              geom = "point",
      #             shape = 18, size = 3, color = "black") +
      stat_summary(fun.data = mean_sdl,
                   fun.args = list(mult = 1),
                   geom = "errorbar",
                   width = 0.2) +
      scale_y_continuous(limits = c(200, 1000)) +
      labs(x = "Soil depth (cm)", 
           y = expression("Sulphate concentration (mg kg"^{-1}*")")) +
      theme_classic(base_size = 14) +
      theme(axis.line = element_line(size = 0.5, colour = "black"))
    
    
    
    
    
    
    
  }
  
                # FCR - MIXED MODEL WITH HIERARCHICAL STRUCTURE
                {
                  rm(list = ls())
                  
                  library(readxl)
                  library(lme4)
                  library(lmerTest)
                  library(car)
                  library(emmeans)
                  library(multcomp)
                  library(multcompView)
                  
                  # Load dataset
                  DF <- read_excel(
                    "...../Data_All_Vin_FCR.xlsx",
                    sheet = "Risults_OK_FCR"
                  )
                  
                  head(DF)
                  str(DF)
                  
                  # =========================
                  # FACTORS
                  # =========================
                  DF$Layer <- factor(DF$Layer,
                                     levels = c("Second", "Third"),
                                     labels = c("Second", "Third"))
                  
                  DF$Transect <- factor(DF$Transect)
                  DF$SamplingPoint <- factor(DF$SamplingPoint)
                  
                  # Unique sampled-location ID within transect
                  DF$PointID <- interaction(DF$Transect, DF$SamplingPoint, drop = TRUE)
                  
                  # Type III contrasts
                  options(contrasts = c("contr.sum", "contr.poly"))
                  
                  # =========================
                  # VARIABLES TO ANALYZE
                  # =========================
                  vars <- c("Corg_PercSS", "Cu_mgkgSS", "IC_Solfati_mgkgSS", "Fe_mgkgSS",
                            "TKN_PercSS", "IC_Nitrati_mgkgSS", "IC_Potassio_mgkgSS",
                            "IC_Fosfati_mgkgSS", "ICPCa_mgkgSS", "ICPK_mgkgSS", "ICPMg_mgkgSS")
                  
                  # =========================
                  # STORE RESULTS IN R
                  # =========================
                  all_models <- list()
                  all_anova  <- list()
                  all_emm    <- list()
                  all_cld    <- list()
                  
                  for (v in vars) {
                    
                    cat("\n\n====================================================\n")
                    cat("VARIABLE:", v, "\n")
                    cat("====================================================\n")
                    
                    # Full model matching Methods:
                    # fixed = Layer
                    # random = SamplingPoint nested within Transect
                    form_full <- as.formula(
                      paste(v, "~ Layer + (1 | Transect/PointID)")
                    )
                    
                    mod_full <- lmer(form_full, data = DF, REML = TRUE)
                    
                    # Check singularity
                    sing <- isSingular(mod_full, tol = 1e-4)
                    cat("Singular fit:", sing, "\n")
                    
                    # Simplify if singular
                    if (sing) {
                      form_use <- as.formula(
                        paste(v, "~ Layer + (1 | PointID)")
                      )
                      mod_use <- lmer(form_use, data = DF, REML = TRUE)
                      cat("Using simplified random structure: (1 | PointID)\n")
                    } else {
                      mod_use <- mod_full
                      cat("Using full random structure: (1 | Transect/PointID)\n")
                    }
                    
                    all_models[[v]] <- mod_use
                    
                    cat("\n--- Model summary ---\n")
                    print(summary(mod_use))
                    
                    cat("\n--- Random effects ---\n")
                    print(VarCorr(mod_use))
                    
                    # Type III ANOVA
                    anova_res <- Anova(mod_use, type = 3)
                    all_anova[[v]] <- anova_res
                    
                    cat("\n--- Type III ANOVA ---\n")
                    print(anova_res)
                    
                    # Detect p-value column safely
                    p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
                    p_lay <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
                    
                    # EMMs and post hoc
                    if (!is.na(p_lay) && p_lay < 0.05) {
                      
                      cat("\nMain effect significant: Layer\n")
                      emm_res <- emmeans(mod_use, ~ Layer)
                      cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
                      
                      cat("\n--- EMMs ---\n")
                      print(emm_res)
                      
                      cat("\n--- Significant letters ---\n")
                      print(cld_res)
                      
                    } else {
                      
                      cat("\nNo significant fixed effects at p < 0.05\n")
                      emm_res <- emmeans(mod_use, ~ Layer)
                      cld_res <- NULL
                      
                      cat("\n--- EMMs ---\n")
                      print(emm_res)
                    }
                    
                    all_emm[[v]] <- emm_res
                    all_cld[[v]] <- cld_res
                  }
                  
                  # Example: inspect one variable
                  all_anova$Cu_mgkgSS
                  all_cld$Cu_mgkgSS
  }
                
                # Vineyard VS FCR 
 
                # MIXED MODEL 
                {
      
      rm(list = ls())
      
      library(readxl)
      library(lme4)
      library(lmerTest)
      library(car)
      library(emmeans)
      library(multcomp)
      library(multcompView)
      
      # Load dataset
      DF <- read_excel(
        "...../Data_All_Vin_FCR.xlsx",
        sheet = "Risults_OK_Vineyard"
      )
      
      head(DF)
      str(DF)
      
      # =========================
      # FACTORS
      # =========================
      DF$Crop <- factor(DF$Crop,
                        levels = c("Vineyard", "Wheat"),
                        labels = c("Vineyard", "FCR"))
      
      DF$Layer <- factor(DF$Layer,
                         levels = c("ZeroTwenty", "TwentyFourthy"),
                         labels = c("ZeroTwenty", "TwentyFourthy"))
      
      DF$SamplingPoint <- factor(DF$SamplingPoint)
      
      # Common grouping variable
      DF$SpatialGroup <- ifelse(DF$Crop == "Vineyard",
                                paste0("Row_", DF$VineRow),
                                paste0("Transect_", DF$Transect))
      DF$SpatialGroup <- factor(DF$SpatialGroup)
      
      # Unique point within grouping unit
      DF$PointID <- interaction(DF$SpatialGroup, DF$SamplingPoint, drop = TRUE)
      
      # Type III contrasts
      options(contrasts = c("contr.sum", "contr.poly"))
      
      # Variables to analyze
      vars <- c("Corg_PercSS", "Cu_mgkgSS", "IC_Solfati_mgkgSS", "Fe_mgkgSS",
                "TKN_PercSS", "IC_Nitrati_mgkgSS", "IC_Potassio_mgkgSS",
                "IC_Fosfati_mgkgSS", "ICPCa_mgkgSS", "ICPK_mgkgSS", "ICPMg_mgkgSS")
      
      # Store results
      all_models <- list()
      all_anova  <- list()
      all_emm    <- list()
      all_cld    <- list()
      
      for (v in vars) {
        
        cat("\n\n====================================================\n")
        cat("VARIABLE:", v, "\n")
        cat("====================================================\n")
        
        # Full mixed model
        form_full <- as.formula(
          paste(v, "~ Crop * Layer + (1 | SpatialGroup/PointID)")
        )
        
        mod_full <- lmer(form_full, data = DF, REML = TRUE)
        
        sing <- isSingular(mod_full, tol = 1e-4)
        cat("Singular fit:", sing, "\n")
        
        # Simplify if needed
        if (sing) {
          form_use <- as.formula(
            paste(v, "~ Crop * Layer + (1 | PointID)")
          )
          mod_use <- lmer(form_use, data = DF, REML = TRUE)
          cat("Using simplified random structure: (1 | PointID)\n")
        } else {
          mod_use <- mod_full
          cat("Using full random structure: (1 | SpatialGroup/PointID)\n")
        }
        
        all_models[[v]] <- mod_use
        
        cat("\n--- Model summary ---\n")
        print(summary(mod_use))
        
        cat("\n--- Random effects ---\n")
        print(VarCorr(mod_use))
        
        # Type III ANOVA
        anova_res <- Anova(mod_use, type = 3)
        all_anova[[v]] <- anova_res
        
        cat("\n--- Type III ANOVA ---\n")
        print(anova_res)
        
        # Detect p-value column safely
        p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
        
        p_int  <- if ("Crop:Layer" %in% rownames(anova_res)) anova_res["Crop:Layer", p_col] else NA
        p_crop <- if ("Crop" %in% rownames(anova_res)) anova_res["Crop", p_col] else NA
        p_lay  <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
        
        # Post hoc logic
        if (!is.na(p_int) && p_int < 0.05) {
          
          cat("\nInteraction significant: post hoc on Crop × Layer\n")
          emm_res <- emmeans(mod_use, ~ Crop * Layer)
          cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
          
          cat("\n--- EMMs ---\n")
          print(emm_res)
          
          cat("\n--- Significant letters ---\n")
          print(cld_res)
          
        } else if (!is.na(p_crop) && p_crop < 0.05 && !is.na(p_lay) && p_lay < 0.05) {
          
          cat("\nInteraction not significant; both main effects significant.\n")
          
          emm_crop <- emmeans(mod_use, ~ Crop)
          cld_crop <- cld(emm_crop, Letters = letters, reverse = TRUE, adjust = "sidak")
          
          emm_lay <- emmeans(mod_use, ~ Layer)
          cld_lay <- cld(emm_lay, Letters = letters, reverse = TRUE, adjust = "sidak")
          
          emm_res <- list(Crop = emm_crop, Layer = emm_lay)
          cld_res <- list(Crop = cld_crop, Layer = cld_lay)
          
          cat("\n--- EMMs: Crop ---\n")
          print(emm_crop)
          cat("\n--- Significant letters: Crop ---\n")
          print(cld_crop)
          
          cat("\n--- EMMs: Layer ---\n")
          print(emm_lay)
          cat("\n--- Significant letters: Layer ---\n")
          print(cld_lay)
          
        } else if (!is.na(p_crop) && p_crop < 0.05) {
          
          cat("\nMain effect significant: Crop\n")
          emm_res <- emmeans(mod_use, ~ Crop)
          cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
          
          cat("\n--- EMMs ---\n")
          print(emm_res)
          
          cat("\n--- Significant letters ---\n")
          print(cld_res)
          
        } else if (!is.na(p_lay) && p_lay < 0.05) {
          
          cat("\nMain effect significant: Layer\n")
          emm_res <- emmeans(mod_use, ~ Layer)
          cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
          
          cat("\n--- EMMs ---\n")
          print(emm_res)
          
          cat("\n--- Significant letters ---\n")
          print(cld_res)
          
        } else {
          
          cat("\nNo significant fixed effects at p < 0.05\n")
          emm_res <- emmeans(mod_use, ~ Crop * Layer)
          cld_res <- NULL
          
          cat("\n--- EMMs ---\n")
          print(emm_res)
        }
        
        all_emm[[v]] <- emm_res
        all_cld[[v]] <- cld_res
      }
      
      # Example: inspect one variable
      all_anova$Cu_mgkgSS
      all_cld$Cu_mgkgSS
      
    }

}

 
  #_____________________Elements Content____________________#
  
  {
    
    ##########################
    # ANALISI Vineyard
    #########################
  
  #  MIXED MODELS - 020_2040_RowInterRow_Vineyard
  {
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    library(ggplot2)
    
    # =========================
    # LOAD DATA
    # =========================
    DF <- read_excel(
      "......Data_VinVsFCR.xlsx",
      sheet = "020_2040_RowInterRow_Vigneto"
    )
    
    
    head(DF)
    str(DF)
    
    # =========================
    # FACTORS
    # =========================
    DF$Posizione <- factor(DF$Posizione)   # row / inter-row
    
    DF$Layer <- factor(
      DF$Layer,
      levels = c("ZeroTwenty", "TwentyFourthy"),
      labels = c("0-20", "20-40")
    )
    
    DF$VineRow <- factor(DF$VineRow)
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Unique sampled-location ID within vine row
    DF$PointID <- interaction(DF$VineRow, DF$SamplingPoint, drop = TRUE)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # =========================
    # VARIABLES TO ANALYZE
    # =========================
    vars <- c("Corg_Mgha", "Cu_kgha", "ICSolfati_kgha", "Fe_kgha",
              "TKN_kgha", "ICNitrati_kgha", "ICPotassio_kgha",
              "ICFosfati_kgha", "ICCa_kgha", "ICK_kgha", "ICMg_kgha", "ICNa_kgha")
    
    # =========================
    # STORE RESULTS
    # =========================
    all_models <- list()
    all_anova  <- list()
    all_emm    <- list()
    all_cld    <- list()
    
    # =========================
    # LOOP OVER VARIABLES
    # =========================
    for (v in vars) {
      
      cat("\n\n====================================================\n")
      cat("VARIABLE:", v, "\n")
      cat("====================================================\n")
      
      # Full model matching the Methods section
      # Fixed: Posizione * Layer
      # Random: SamplingPoint nested within VineRow
      form_full <- as.formula(
        paste(v, "~ Posizione * Layer + (1 | VineRow/PointID)")
      )
      
      mod_full <- lmer(form_full, data = DF, REML = TRUE)
      
      # Check singularity
      sing <- isSingular(mod_full, tol = 1e-4)
      cat("Singular fit:", sing, "\n")
      
      # Simplify only if necessary
      if (sing) {
        form_use <- as.formula(
          paste(v, "~ Posizione * Layer + (1 | PointID)")
        )
        mod_use <- lmer(form_use, data = DF, REML = TRUE)
        cat("Using simplified random structure: (1 | PointID)\n")
      } else {
        mod_use <- mod_full
        cat("Using full random structure: (1 | VineRow/PointID)\n")
      }
      
      all_models[[v]] <- mod_use
      
      cat("\n--- Model summary ---\n")
      print(summary(mod_use))
      
      cat("\n--- Random effects ---\n")
      print(VarCorr(mod_use))
      
      # Type III ANOVA
      anova_res <- Anova(mod_use, type = 3)
      all_anova[[v]] <- anova_res
      
      cat("\n--- Type III ANOVA ---\n")
      print(anova_res)
      
      # Detect p-value column safely
      p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
      
      p_int <- if ("Posizione:Layer" %in% rownames(anova_res)) anova_res["Posizione:Layer", p_col] else NA
      p_pos <- if ("Posizione" %in% rownames(anova_res)) anova_res["Posizione", p_col] else NA
      p_lay <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
      
      # =========================
      # EMMs + LETTERS
      # =========================
      if (!is.na(p_int) && p_int < 0.05) {
        
        cat("\nInteraction significant: post hoc on Posizione × Layer\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_pos) && p_pos < 0.05 && !is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nInteraction not significant; both main effects significant.\n")
        
        emm_pos <- emmeans(mod_use, ~ Posizione)
        cld_pos <- cld(emm_pos, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_lay <- emmeans(mod_use, ~ Layer)
        cld_lay <- cld(emm_lay, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_res <- list(Posizione = emm_pos, Layer = emm_lay)
        cld_res <- list(Posizione = cld_pos, Layer = cld_lay)
        
        cat("\n--- EMMs: Posizione ---\n")
        print(emm_pos)
        cat("\n--- Significant letters: Posizione ---\n")
        print(cld_pos)
        
        cat("\n--- EMMs: Layer ---\n")
        print(emm_lay)
        cat("\n--- Significant letters: Layer ---\n")
        print(cld_lay)
        
      } else if (!is.na(p_pos) && p_pos < 0.05) {
        
        cat("\nMain effect significant: Posizione\n")
        emm_res <- emmeans(mod_use, ~ Posizione)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nMain effect significant: Layer\n")
        emm_res <- emmeans(mod_use, ~ Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else {
        
        cat("\nNo significant fixed effects at p < 0.05\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- NULL
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
      }
      
      all_emm[[v]] <- emm_res
      all_cld[[v]] <- cld_res
    }
    
    # =========================
    # EXAMPLES TO RECALL RESULTS
    # =========================
    
    # ANOVA table for one variable
    all_anova$Cu_kgha
    
    # Significant letters for one variable
    all_cld$Cu_kgha
    all_cld$ICSolfati_kgha
  }
  
  #====================================================
  
  # NMIXED MODELS - ANALISI DEL PRIMO STRATO 0-10 VS 10-20
  {
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    library(ggplot2)
    
    # =========================
    # LOAD DATA
    # =========================
    DF <- read_excel(
      "......Data_VinVsFCR.xlsx",
      sheet = "010_1020_RowInterw_Vigneto"
    )
    
    
    head(DF)
    str(DF)
    #View(DF)
    
    # =========================
    # FACTORS
    # =========================
    DF$Posizione <- factor(DF$Posizione)   # row / inter-row
    
    DF$Layer <- factor(
      DF$Layer,
      levels = c("ZeroTen", "TenTwenty"),
      labels = c("0-10", "10-20")
    )
    
    DF$VineRow <- factor(DF$VineRow)
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Unique sampled-location ID within vine row
    DF$PointID <- interaction(DF$VineRow, DF$SamplingPoint, drop = TRUE)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # =========================
    # VARIABLES TO ANALYZE
    # =========================
    vars <- c("Corg_Mgha", "Cu_kgha", "ICSolfati_kgha", "Fe_kgha", 
              "TKN_kgha", "ICNitrati_kgha", "K_kgha", 
              "ICFosfati_kgha", "ICCa_kgha", "ICK_kgha", "ICMg_kgha", "ICNa_kgha")
    # =========================
    # STORE RESULTS
    # =========================
    all_models <- list()
    all_anova  <- list()
    all_emm    <- list()
    all_cld    <- list()
    
    # =========================
    # LOOP OVER VARIABLES
    # =========================
    for (v in vars) {
      
      cat("\n\n====================================================\n")
      cat("VARIABLE:", v, "\n")
      cat("====================================================\n")
      
      # Full model matching the Methods section
      # Fixed: Posizione * Layer
      # Random: SamplingPoint nested within VineRow
      form_full <- as.formula(
        paste(v, "~ Posizione * Layer + (1 | VineRow/PointID)")
      )
      
      mod_full <- lmer(form_full, data = DF, REML = TRUE)
      
      # Check singularity
      sing <- isSingular(mod_full, tol = 1e-4)
      cat("Singular fit:", sing, "\n")
      
      # Simplify only if necessary
      if (sing) {
        form_use <- as.formula(
          paste(v, "~ Posizione * Layer + (1 | PointID)")
        )
        mod_use <- lmer(form_use, data = DF, REML = TRUE)
        cat("Using simplified random structure: (1 | PointID)\n")
      } else {
        mod_use <- mod_full
        cat("Using full random structure: (1 | VineRow/PointID)\n")
      }
      
      all_models[[v]] <- mod_use
      
      cat("\n--- Model summary ---\n")
      print(summary(mod_use))
      
      cat("\n--- Random effects ---\n")
      print(VarCorr(mod_use))
      
      # Type III ANOVA
      anova_res <- Anova(mod_use, type = 3)
      all_anova[[v]] <- anova_res
      
      cat("\n--- Type III ANOVA ---\n")
      print(anova_res)
      
      # Detect p-value column safely
      p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
      
      p_int <- if ("Posizione:Layer" %in% rownames(anova_res)) anova_res["Posizione:Layer", p_col] else NA
      p_pos <- if ("Posizione" %in% rownames(anova_res)) anova_res["Posizione", p_col] else NA
      p_lay <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
      
      # =========================
      # EMMs + LETTERS
      # =========================
      if (!is.na(p_int) && p_int < 0.05) {
        
        cat("\nInteraction significant: post hoc on Posizione × Layer\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_pos) && p_pos < 0.05 && !is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nInteraction not significant; both main effects significant.\n")
        
        emm_pos <- emmeans(mod_use, ~ Posizione)
        cld_pos <- cld(emm_pos, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_lay <- emmeans(mod_use, ~ Layer)
        cld_lay <- cld(emm_lay, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_res <- list(Posizione = emm_pos, Layer = emm_lay)
        cld_res <- list(Posizione = cld_pos, Layer = cld_lay)
        
        cat("\n--- EMMs: Posizione ---\n")
        print(emm_pos)
        cat("\n--- Significant letters: Posizione ---\n")
        print(cld_pos)
        
        cat("\n--- EMMs: Layer ---\n")
        print(emm_lay)
        cat("\n--- Significant letters: Layer ---\n")
        print(cld_lay)
        
      } else if (!is.na(p_pos) && p_pos < 0.05) {
        
        cat("\nMain effect significant: Posizione\n")
        emm_res <- emmeans(mod_use, ~ Posizione)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nMain effect significant: Layer\n")
        emm_res <- emmeans(mod_use, ~ Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else {
        
        cat("\nNo significant fixed effects at p < 0.05\n")
        emm_res <- emmeans(mod_use, ~ Posizione * Layer)
        cld_res <- NULL
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
      }
      
      all_emm[[v]] <- emm_res
      all_cld[[v]] <- cld_res
    }
    
    # =========================
    # EXAMPLES TO RECALL RESULTS
    # =========================
    
    # ANOVA table for one variable
    all_anova$Cu_kgha
    
    # Significant letters for one variable
    all_cld$Cu_kgha
    all_cld$ICSolfati_kgha
  }
  
  ##########################
  # ANALISI FCR 
  #########################
  

  # MIXED MODEL 
      {
        rm(list = ls())
        
        library(readxl)
        library(lme4)
        library(lmerTest)
        library(car)
        library(emmeans)
        library(multcomp)
        library(multcompView)
        
        # Load dataset
        DF <- read_excel(
          "...../Data_All_Vin_FCR.xlsx",
          sheet = "Risultats_OK_Vineyard"
        )
        
        head(DF)
        str(DF)
        
        # =========================
        # FACTORS
        # =========================
        DF$Layer <- factor(DF$Layer,
                           levels = c("Second", "Third"),
                           labels = c("Second", "Third"))
        
        DF$Transect <- factor(DF$Transect)
        DF$SamplingPoint <- factor(DF$SamplingPoint)
        
        # Unique sampled-location ID within transect
        DF$PointID <- interaction(DF$Transect, DF$SamplingPoint, drop = TRUE)
        
        # Type III contrasts
        options(contrasts = c("contr.sum", "contr.poly"))
        
        # =========================
        # VARIABLES TO ANALYZE
        # =========================
        vars <- c("Corg_Mgha", "Cu_kgha", "IC_Solfati_kgha", "Fe_kgha", 
                  "TKN_kgha", "IC_Nitrati_kgha", "IC_Potassio_kgha", 
                  "IC_Fosfati_kgha", "ICPCa_kgha", "ICPK_kgha", "ICPMg_kgha")
        
        # =========================
        # STORE RESULTS IN R
        # =========================
        all_models <- list()
        all_anova  <- list()
        all_emm    <- list()
        all_cld    <- list()
        
        for (v in vars) {
          
          cat("\n\n====================================================\n")
          cat("VARIABLE:", v, "\n")
          cat("====================================================\n")
          
          # Full model matching Methods:
          # fixed = Layer
          # random = SamplingPoint nested within Transect
          form_full <- as.formula(
            paste(v, "~ Layer + (1 | Transect/PointID)")
          )
          
          mod_full <- lmer(form_full, data = DF, REML = TRUE)
          
          # Check singularity
          sing <- isSingular(mod_full, tol = 1e-4)
          cat("Singular fit:", sing, "\n")
          
          # Simplify if singular
          if (sing) {
            form_use <- as.formula(
              paste(v, "~ Layer + (1 | PointID)")
            )
            mod_use <- lmer(form_use, data = DF, REML = TRUE)
            cat("Using simplified random structure: (1 | PointID)\n")
          } else {
            mod_use <- mod_full
            cat("Using full random structure: (1 | Transect/PointID)\n")
          }
          
          all_models[[v]] <- mod_use
          
          cat("\n--- Model summary ---\n")
          print(summary(mod_use))
          
          cat("\n--- Random effects ---\n")
          print(VarCorr(mod_use))
          
          # Type III ANOVA
          anova_res <- Anova(mod_use, type = 3)
          all_anova[[v]] <- anova_res
          
          cat("\n--- Type III ANOVA ---\n")
          print(anova_res)
          
          # Detect p-value column safely
          p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
          p_lay <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
          
          # EMMs and post hoc
          if (!is.na(p_lay) && p_lay < 0.05) {
            
            cat("\nMain effect significant: Layer\n")
            emm_res <- emmeans(mod_use, ~ Layer)
            cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
            
            cat("\n--- EMMs ---\n")
            print(emm_res)
            
            cat("\n--- Significant letters ---\n")
            print(cld_res)
            
          } else {
            
            cat("\nNo significant fixed effects at p < 0.05\n")
            emm_res <- emmeans(mod_use, ~ Layer)
            cld_res <- NULL
            
            cat("\n--- EMMs ---\n")
            print(emm_res)
          }
          
          all_emm[[v]] <- emm_res
          all_cld[[v]] <- cld_res
        }
        
        # Example: inspect one variable
        all_anova$Cu_mgkgSS
        all_cld$Cu_mgkgSS
      }
  
    ##########################
    # Vineyard VS FCR 
    #########################
  
  #NUOVO CONFRONTO FATTO CON MIXED MODEL 
  {
    rm(list = ls())
    
    library(readxl)
    library(lme4)
    library(lmerTest)
    library(car)
    library(emmeans)
    library(multcomp)
    library(multcompView)
    
    # Load dataset
    DF <- read_excel(
      "......Data_VinVsFCR.xlsx",
      sheet = "020_2040_RowInterRow"
    )
    
    
    head(DF)
    str(DF)
    
    # =========================
    # FACTORS
    # =========================
    DF$Crop <- factor(DF$Crop,
                      levels = c("Vineyard", "Wheat"),
                      labels = c("Vineyard", "FCR"))
    
    DF$Layer <- factor(DF$Layer,
                       levels = c("ZeroTwenty", "TwentyFourthy"),
                       labels = c("ZeroTwenty", "TwentyFourthy"))
    
    DF$SamplingPoint <- factor(DF$SamplingPoint)
    
    # Common grouping variable
    DF$SpatialGroup <- ifelse(DF$Crop == "Vineyard",
                              paste0("Row_", DF$VineRow),
                              paste0("Transect_", DF$Transect))
    DF$SpatialGroup <- factor(DF$SpatialGroup)
    
    # Unique point within grouping unit
    DF$PointID <- interaction(DF$SpatialGroup, DF$SamplingPoint, drop = TRUE)
    
    # Type III contrasts
    options(contrasts = c("contr.sum", "contr.poly"))
    
    # Variables to analyze
    vars <- c("Corg_kgha", "Cu_kgha", "ICSolfati_kgha", "Fe_kgha", 
              "TKN_kgha", "ICNitrati_kgha", "ICPotassio_kgha", 
              "ICFosfati_kgha", "ICPCa_kgha", "ICPK_kgha", "ICPMg_kgha")
    # Store results
    all_models <- list()
    all_anova  <- list()
    all_emm    <- list()
    all_cld    <- list()
    
    for (v in vars) {
      
      cat("\n\n====================================================\n")
      cat("VARIABLE:", v, "\n")
      cat("====================================================\n")
      
      # Full mixed model
      form_full <- as.formula(
        paste(v, "~ Crop * Layer + (1 | SpatialGroup/PointID)")
      )
      
      mod_full <- lmer(form_full, data = DF, REML = TRUE)
      
      sing <- isSingular(mod_full, tol = 1e-4)
      cat("Singular fit:", sing, "\n")
      
      # Simplify if needed
      if (sing) {
        form_use <- as.formula(
          paste(v, "~ Crop * Layer + (1 | PointID)")
        )
        mod_use <- lmer(form_use, data = DF, REML = TRUE)
        cat("Using simplified random structure: (1 | PointID)\n")
      } else {
        mod_use <- mod_full
        cat("Using full random structure: (1 | SpatialGroup/PointID)\n")
      }
      
      all_models[[v]] <- mod_use
      
      cat("\n--- Model summary ---\n")
      print(summary(mod_use))
      
      cat("\n--- Random effects ---\n")
      print(VarCorr(mod_use))
      
      # Type III ANOVA
      anova_res <- Anova(mod_use, type = 3)
      all_anova[[v]] <- anova_res
      
      cat("\n--- Type III ANOVA ---\n")
      print(anova_res)
      
      # Detect p-value column safely
      p_col <- grep("^Pr\\(", colnames(anova_res), value = TRUE)[1]
      
      p_int  <- if ("Crop:Layer" %in% rownames(anova_res)) anova_res["Crop:Layer", p_col] else NA
      p_crop <- if ("Crop" %in% rownames(anova_res)) anova_res["Crop", p_col] else NA
      p_lay  <- if ("Layer" %in% rownames(anova_res)) anova_res["Layer", p_col] else NA
      
      # Post hoc logic
      if (!is.na(p_int) && p_int < 0.05) {
        
        cat("\nInteraction significant: post hoc on Crop × Layer\n")
        emm_res <- emmeans(mod_use, ~ Crop * Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_crop) && p_crop < 0.05 && !is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nInteraction not significant; both main effects significant.\n")
        
        emm_crop <- emmeans(mod_use, ~ Crop)
        cld_crop <- cld(emm_crop, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_lay <- emmeans(mod_use, ~ Layer)
        cld_lay <- cld(emm_lay, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        emm_res <- list(Crop = emm_crop, Layer = emm_lay)
        cld_res <- list(Crop = cld_crop, Layer = cld_lay)
        
        cat("\n--- EMMs: Crop ---\n")
        print(emm_crop)
        cat("\n--- Significant letters: Crop ---\n")
        print(cld_crop)
        
        cat("\n--- EMMs: Layer ---\n")
        print(emm_lay)
        cat("\n--- Significant letters: Layer ---\n")
        print(cld_lay)
        
      } else if (!is.na(p_crop) && p_crop < 0.05) {
        
        cat("\nMain effect significant: Crop\n")
        emm_res <- emmeans(mod_use, ~ Crop)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else if (!is.na(p_lay) && p_lay < 0.05) {
        
        cat("\nMain effect significant: Layer\n")
        emm_res <- emmeans(mod_use, ~ Layer)
        cld_res <- cld(emm_res, Letters = letters, reverse = TRUE, adjust = "sidak")
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
        
        cat("\n--- Significant letters ---\n")
        print(cld_res)
        
      } else {
        
        cat("\nNo significant fixed effects at p < 0.05\n")
        emm_res <- emmeans(mod_use, ~ Crop * Layer)
        cld_res <- NULL
        
        cat("\n--- EMMs ---\n")
        print(emm_res)
      }
      
      all_emm[[v]] <- emm_res
      all_cld[[v]] <- cld_res
    }
    
    # Example: inspect one variable
    all_anova$Fe_kgha 
    all_cld$Fe_kgha 
    
    
}

  
  }
  

   

  
  
  