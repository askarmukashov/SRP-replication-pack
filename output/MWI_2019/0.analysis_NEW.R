packages <- c("tidyverse", "gridExtra", "readxl", "limSolve", "Matrix", "EnvStats","MVN","pracma",
              "Hmisc",
              "cowplot", 
              "rJava", "xlsx","dplyr","mvtnorm", 'spatstat', 'scales','GGally','relaimpo',
              'randomForest','tidyr','data.table','rpart',"rpart.plot",'extrafont')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
# options(java.parameters = "-Xmx8000m")
lapply(packages, require, character.only = TRUE)
# # clear the environment and the console
# rm(list=ls(all=TRUE))
# cat("\014")

shocks <- as.data.frame(read_excel(path = 
                                     "C:/Work/AI/historical_uncertainty/data/mwi_2019/2sampled_scenarios_mwi_2019.xlsx", 
                                   sheet = "scenarios"))
shock_vars <- setdiff(names(shocks), c("X", "T"))
shocks <- subset(shocks, X != 'base')
shocks <- subset(shocks, X != 0)
shocks$X <- as.numeric(shocks$X)
shocks$T <- as.numeric(shocks$T)
shocks <- shocks[order(shocks$X, shocks$T), ]
row.names(shocks) <- NULL

shocks_repr <- shocks
colnames(shocks_repr)[colnames(shocks_repr) == "pw_mine"] <- "pw_ener"

base_sh <- data.frame(X = 0, T = 2019, lapply(shocks_repr[, -c(which(names(shocks_repr) %in% c("X", "T")))], median))
shocks_repr <- rbind(base_sh,shocks_repr)
shocks_repr$X <- as.numeric(shocks_repr$X)
shocks_repr$T <- as.numeric(shocks_repr$T)
shocks_repr <- shocks_repr[order(shocks_repr$X, shocks_repr$T), ]
row.names(shocks_repr)<- NULL
write.csv(shocks_repr,'Risks.csv')

shocks_repr <- subset(shocks_repr, X != 0)
row.names(shocks_repr) <- NULL

shocks_repr$T <- NULL
shocks_repr <- shocks_repr[shocks_repr$X != 0, ]
shocks_repr$X <- NULL

variable_order <- colnames(shocks_repr)[!(colnames(shocks_repr) %in% c("T", "X"))]
shocks_repr <- melt(shocks_repr)
shocks_repr$variable <- factor(shocks_repr$variable, levels = rev(variable_order))

map <- as.data.frame(read_excel(path = "../orders_for_reporting.xlsx", sheet = "map_shocks"))
colors <- setNames(map$color, map$Variable)
names <- setNames(map$Label, map$Variable)

selected_variables <- c("pw_cere", "pd_cere", "FSAV")
filtered_shocks_repr <- shocks_repr[shocks_repr$variable %in% selected_variables, ]

filtered_shocks_repr$variable <- as.character(filtered_shocks_repr$variable)
filtered_shocks_repr$variable[filtered_shocks_repr$variable == "pw_cere"] <- "World_Prices"
filtered_shocks_repr$variable[filtered_shocks_repr$variable == "pd_cere"] <- "Productivity"
filtered_shocks_repr$variable[filtered_shocks_repr$variable == "FSAV"] <- "FX_flows"
filtered_shocks_repr$variable <- factor(filtered_shocks_repr$variable, levels = c("World_Prices", "FX_flows", "Productivity"))

updated_colors <- colors
names(updated_colors)[names(updated_colors) == "pw_cere"] <- "World_Prices"
names(updated_colors)[names(updated_colors) == "pd_cere"] <- "Productivity"
names(updated_colors)[names(updated_colors) == "FSAV"] <- "FX_flows"
updated_colors <- updated_colors[names(updated_colors) %in% c("World_Prices", "Productivity", "FX_flows")]

# Update the names for the legend
updated_names <- c(
  "World_Prices" = "World Prices (deviation from base, %)",
  "FX_flows" = "FX Flows (deviation from base, % of GDP)",
  "Productivity" = "Productivity (deviation from base, %)"
)

# Create a plot for extracting the legend
temp_plot <- ggplot(filtered_shocks_repr, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = updated_colors, labels = updated_names) +
  scale_x_discrete(labels = updated_names) +  
  coord_flip() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  ylab(NULL) +
  xlab(NULL) +
  guides(color = guide_legend(nrow = 3, title = NULL))
get_legend <- function(my_plot) {
  g <- ggplotGrob(my_plot)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  if (length(legend) > 0) return(legend[[1]]) else return(NULL)
}

legend <- get_legend(temp_plot)

# Create the main plot with updated labels for consistency
plot <- ggplot(shocks_repr, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  ylab(NULL) +
  xlab(NULL)
combined_plot <- plot_grid(
  legend, plot, 
  ncol = 1, 
  rel_heights = c(0.15, 1)
)
ggsave("shocks_boxplots.pdf", plot = combined_plot, width = 11, height = 10)



MACROTAB <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                                     sheet = "MACROTABX"))
names(MACROTAB)[1:2] <- c("X", "T")
MACROTAB <- subset(MACROTAB, X != 'base')
MACROTAB <- subset(MACROTAB, X != 0)
MACROTAB$X <- as.numeric(MACROTAB$X)
MACROTAB$T <- as.numeric(MACROTAB$T)
MACROTAB <- MACROTAB[order(MACROTAB$X, MACROTAB$T), ]
row.names(MACROTAB) <- NULL

PH <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                               sheet = "PH"))
names(PH)[1:2] <- c("X", "T")
PH <- subset(PH, X != 'base')
PH <- subset(PH, X != 0)
PH$X <- as.numeric(PH$X)
PH$T <- as.numeric(PH$T)
PH <- PH[order(PH$X, PH$T), ]
row.names(PH) <- NULL



gdp_AFS <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                                    sheet = "gdp_AFS"))
names(gdp_AFS)[1:2] <- c("X", "T")
gdp_AFS <- subset(gdp_AFS, X != 'base')
gdp_AFS <- subset(gdp_AFS, X != 0)
gdp_AFS$X <- as.numeric(gdp_AFS$X)
gdp_AFS$T <- as.numeric(gdp_AFS$T)
gdp_AFS <- gdp_AFS[order(gdp_AFS$X, gdp_AFS$T), ]
row.names(gdp_AFS) <- NULL

gdp_sect <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                                     sheet = "gdp_nonag"))
names(gdp_sect)[1:2] <- c("X", "T")
gdp_sect <- subset(gdp_sect, X != 'base')
gdp_sect <- subset(gdp_sect, X != 0)
gdp_sect$X <- as.numeric(gdp_sect$X)
gdp_sect$T <- as.numeric(gdp_sect$T)
gdp_sect <- gdp_sect[order(gdp_sect$X, gdp_sect$T), ]
row.names(gdp_sect) <- NULL
gdp_sect <- data.frame(X=gdp_AFS$X,
                       T=gdp_AFS$T,
                       Total = MACROTAB$GDPFC,
                       Agriculture=gdp_AFS$`AGR+`,
                       Industry=gdp_sect$aa_IND,
                       Services=gdp_sect$aa_SRV)

HHD_CONS <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                                    sheet = "HHD_CONS_X"))
names(HHD_CONS)[1:2] <- c("X", "T")
HHD_CONS <- subset(HHD_CONS, X != 'base')
HHD_CONS <- subset(HHD_CONS, X != 0)
HHD_CONS$X <- as.numeric(HHD_CONS$X)
HHD_CONS$T <- as.numeric(HHD_CONS$T)
HHD_CONS <- HHD_CONS[order(HHD_CONS$X, HHD_CONS$T), ]
row.names(HHD_CONS) <- NULL

outcomes <- data.frame(X=gdp_AFS$X,
                       T=gdp_AFS$T,
                       GDP.TOT=MACROTAB$GDPFC,
                       GDP.AGR=gdp_sect$Agriculture,
                       GDP.IND=gdp_sect$Industry,
                       GDP.SRV=gdp_sect$Services,
                       ABSORPTION.TOT=MACROTAB$ABSORP,
                       PRVCON=MACROTAB$PRVCON,
                       HHD.URB=HHD_CONS$urban,
                       HHD.URB.P = HHD_CONS$urban_poor,
                       HHD.URB.M = HHD_CONS$urban_midd,
                       HHD.URB.R = HHD_CONS$urban_rich,
                       HHD.RUR  = HHD_CONS$rural,
                       HHD.RUR.P= HHD_CONS$rural_poor,
                       HHD.RUR.M= HHD_CONS$rural_midd,
                       HHD.RUR.R= HHD_CONS$rural_rich,
                       NET.TRD = MACROTAB$EXPORTS - MACROTAB$IMPORTS,
                       EXPORTS=MACROTAB$EXPORTS,
                       IMPORTS=MACROTAB$IMPORTS,
                       POV.NAT=PH$POVR_N_NAT_L,
                       HUN.NAT=PH$HUNR_N,
                       POV.URB=PH$POVR_U_NAT_L,
                       HUN.URB=PH$HUNR_U,
                       POV.RUR=PH$POVR_R_NAT_L,
                       HUN.RUR=PH$HUNR_R)


################# analysis of unsolved - start
model_stat <- as.data.frame(read_excel(path =  "mwi_2019_STATIC.xlsx", 
                                       sheet = "model_stat"))
names(model_stat)[1:2] <- c("X", "T")
model_stat <- subset(model_stat, X != 'base')
model_stat <- subset(model_stat, X != 0)
model_stat$X <- as.numeric(model_stat$X)
model_stat$T <- as.numeric(model_stat$T)
model_stat <- model_stat[order(model_stat$X, model_stat$T), ]
row.names(model_stat) <- NULL

unsolved <- merge(shocks, model_stat, by = c("X", "T"), all = TRUE)
solved <- unsolved[unsolved$solvestat == 1, ]
unsolved <- na.omit(unsolved[unsolved$solvestat != 1, ])

if (nrow(unsolved) > 0) {
  together <- list()
  for (col in setdiff(names(unsolved), names(model_stat))) 
    local({
      p <- ggplot() + 
        geom_density(aes(x = unsolved[, col], color = "Unsolved"), size = 1) + 
        geom_density(aes(x = shocks[, col], color = "Shocks"), size = 1) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        xlab('Variation from trend (%)') +
        ylab("Density") +
        scale_color_manual(values = c("Unsolved" = "red", "Shocks" = "blue")) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank())   
      together[[col]] <<- grid.arrange(
        p,
        ncol = 1,
        top = paste0(col, " (share of total unsolved = ", 100 * nrow(unsolved) / nrow(shocks), " %)" )
      )
    })
  dev.off()
  ggsave(filename = "1. Density_of_unsolved_shocks.pdf", 
         plot = marrangeGrob(together, nrow = 1, ncol = 1),
         width = 14, height = 7)
  
  merged_solved <- merge(solved, outcomes, by = c("X", "T"), all = FALSE)
  merged_solved$solvestat <- NULL
  merged_solved$modelstat <- NULL
  merged_solved$WALRASSQR <- NULL
  
  unsolved_imputed <- unsolved
  unsolved_imputed$solvestat <- NULL
  unsolved_imputed$modelstat <- NULL
  unsolved_imputed$WALRASSQR <- NULL
  unsolved_imputed[setdiff(names(outcomes), c("X", "T"))] <- NA
  
  shock_vars <- setdiff(names(shocks), c("X", "T"))
  for (t_value in unique(merged_solved$T)) {
    # Filter data for each T
    data_for_t <- subset(merged_solved, T == t_value)
    for (outcome_var in names(outcomes)) {
      # Skip the first two columns if they're not outcome variables
      if (outcome_var %in% c("X", "T")) next
      # Output to R console for tracking progress
      cat("Calculating for outcome:", outcome_var, "and Year:", t_value, "\n")
      
      # Create the formula string dynamically
      formula_str <- paste(outcome_var, "~", paste(shock_vars, collapse = " + "))
      # Convert the string to a formula
      formula <- as.formula(formula_str)
      
      # Fitting the linear model on the filtered data
      model <- lm(formula, data = data_for_t)
      
      # Output R-squared and number of observations to R console
      model_summary <- summary(model)
      cat("Model R-squared:", model_summary$r.squared, "- Number of observations:", length(model$residuals), "\n")
      
      unsolved_imputed[unsolved_imputed$T == t_value, outcome_var] <- 
        predict(model, newdata = unsolved_imputed[unsolved_imputed$T == t_value, shock_vars, drop = FALSE])
    }
  }
  
  unsolved_CGE <- merge(unsolved, outcomes, by = c("X", "T"), all = FALSE)
  unsolved_CGE$solvestat <- NULL
  unsolved_CGE$modelstat <- NULL
  unsolved_CGE$WALRASSQR <- NULL
  
  together <- list()
  for (outcome_var in setdiff(names(outcomes), c("X", "T"))) 
    local({
      data_x <- unsolved_CGE[, outcome_var]
      data_y = unsolved_imputed[, outcome_var]
      common_limits <- range(c(data_x, data_y), na.rm = TRUE)
      
      p <- ggplot() + 
        geom_point(aes(x = data_x, y = data_y), color = "blue", size = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.5) +
        scale_x_continuous(limits = common_limits, breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(limits = common_limits, breaks = scales::pretty_breaks(n = 10)) +
        xlab("CGE (min Walrassq. before crash)") +
        ylab("Imputed (based on LM)") +
        theme_minimal() +
        theme(legend.position = "none")   
      
      together[[outcome_var]] <<- grid.arrange(
        p,
        ncol = 1,
        top = paste0(outcome_var, " (share of total unsolved = ", 100 * nrow(unsolved) / nrow(shocks), " %)" )
      )
    })
  dev.off()
  ggsave(filename = "2. Comparison_CGE_vs_imputed.pdf", 
         plot = marrangeGrob(together, nrow = 1, ncol = 1),
         width = 7, height = 7)
  
  # replaced unsolved with imputed
  outcomes <- outcomes[!(outcomes$X %in% unsolved_imputed$X & outcomes$T %in% unsolved_imputed$T), ]
  outcomes <- rbind(outcomes, unsolved_imputed[,names(outcomes)])
  # end of if check
}

base <- data.frame(X = 0, T = 2019, lapply(outcomes[, -c(which(names(outcomes) %in% c("X", "T")))], median))
outcomes <- rbind(base,outcomes)
outcomes$X <- as.numeric(outcomes$X)
outcomes$T <- as.numeric(outcomes$T)
outcomes <- outcomes[order(outcomes$X, outcomes$T), ]
row.names(outcomes)<- NULL
write.csv(outcomes,'q1_outcomes.csv')

outcomes <- subset(outcomes, X != 0)
row.names(outcomes) <- NULL

unique_cols <- setdiff(names(MACROTAB), names(outcomes))
outcomes_summ <- merge(outcomes, MACROTAB[,c("X", "T", unique_cols)], by = c("X", "T"), all = FALSE)

MACROTAB_summary <- outcomes_summ %>%
  dplyr::select(-c("X", "T")) %>%
  summarise(across(everything(), list(base=median, min = min, max = max)))

MACROTAB_summary <- MACROTAB_summary %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value", values_fn = list(value = list)) %>%
  unnest(cols = c(base, min, max))

MACROTAB_order <- as.data.frame(read_excel(path = "../orders_for_reporting.xlsx", sheet = "MACROTAB_order"))

MACROTAB_summary <- MACROTAB_summary %>%
  filter(variable %in% MACROTAB_order$variable) %>%
  arrange(factor(variable, levels = MACROTAB_order$variable))

write.csv(MACROTAB_summary, 'MACROTAB_summary.csv', row.names = FALSE)


###### boxplots outcomes

ALL_comp <- data.frame(X=outcomes$X,
                       T=outcomes$T,
                       GDP.Total=outcomes$GDP.TOT,
                       GDP.Agriculture=outcomes$GDP.AGR,
                       GDP.Industry=outcomes$GDP.IND,
                       GDP.Services=outcomes$GDP.SRV,
                       Cons.Total= outcomes$PRVCON,
                       Cons.Urban = outcomes$HHD.URB,
                       Cons.UrbanP=outcomes$HHD.URB.P,
                       Cons.UrbanM=outcomes$HHD.URB.M,
                       Cons.UrbanR=outcomes$HHD.URB.R,
                       Cons.Rural=outcomes$HHD.RUR,
                       Cons.RuralP=outcomes$HHD.RUR.P,
                       Cons.RuralM=outcomes$HHD.RUR.M,
                       Cons.RuralR=outcomes$HHD.RUR.R,
                       Poverty.Nat=outcomes$POV.NAT,
                       Poverty.Urb=outcomes$POV.URB,
                       Poverty.Rur=outcomes$POV.RUR,
                       Hunger.Nat=outcomes$HUN.NAT,
                       Hunger.Urb=outcomes$HUN.URB,
                       Hunger.Rur=outcomes$HUN.RUR
)
ALL_comp$X <- as.numeric(ALL_comp$X)
ALL_comp$T <- as.numeric(ALL_comp$T)
ALL_comp <- ALL_comp[order(ALL_comp$X, ALL_comp$T), ]

gdp_sect <- data.frame(X=outcomes$X,
                       T=outcomes$T,
                       Total=outcomes$GDP.TOT,
                       Agriculture=outcomes$GDP.AGR,
                       Industry=outcomes$GDP.IND,
                       Services=outcomes$GDP.SRV)
                       
gdp_sect <- gdp_sect %>%
  mutate(across(
    .cols = -c(T, X),  # Exclude columns "T" and "X"
    .fns = ~ ((. - median(.)) / median(.)) * 100  # Calculate % deviation with regard to median
  ))

gdp_sect$T <- NULL
gdp_sect$X <- NULL
variable_order <- colnames(gdp_sect)[!(colnames(gdp_sect) %in% c("T", "X"))]

gdp_sect <- melt(gdp_sect)

gdp_sect$variable <- factor(gdp_sect$variable, 
                            levels = variable_order)
p <- ggplot(gdp_sect, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = names,position = "top") +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')
ggsave("GDPs_boxplots_vertical.pdf", plot = p, width = 5, height = 5)

gdp_sect$variable <- factor(gdp_sect$variable, 
                            levels = rev(variable_order) )
p <- ggplot(gdp_sect, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')

ggsave("GDPs_boxplots_horizontal.pdf", plot = p, width = 5, height = 4)


TRD_comp <- data.frame(X=outcomes$X,
                       T=outcomes$T,
                       Export = outcomes$EXPORTS,
                       Import=outcomes$IMPORTS)
TRD_comp <- TRD_comp %>%
  mutate(across(
    .cols = -c(T, X),  # Exclude columns "T" and "X"
    .fns = ~ ((. - median(.)) / median(.)) * 100  # Calculate % deviation with regard to median
  ))

TRD_comp$T <- NULL
TRD_comp$X <- NULL
variable_order <- colnames(TRD_comp)[!(colnames(TRD_comp) %in% c("T", "X"))]

TRD_comp <- melt(TRD_comp)

TRD_comp$variable <- factor(TRD_comp$variable, 
                            levels = variable_order)
p <- ggplot(TRD_comp, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')
ggsave("TRD_boxplots_vertical.pdf", plot = p, width = 3, height = 5)

TRD_comp$variable <- factor(TRD_comp$variable, 
                            levels = rev(variable_order) )
p <- ggplot(TRD_comp, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')

ggsave("TRD_boxplots_horizontal.pdf", plot = p, width = 5, height = 2)

CON_comp <- data.frame(X=outcomes$X,
                       T=outcomes$T,
                       Total= outcomes$PRVCON,
                       Urban = outcomes$HHD.URB,
                       UrbanP=outcomes$HHD.URB.P,
                       UrbanM=outcomes$HHD.URB.M,
                       UrbanR=outcomes$HHD.URB.R,
                       Rural=outcomes$HHD.RUR,
                       RuralP=outcomes$HHD.RUR.P,
                       RuralM=outcomes$HHD.RUR.M,
                       RuralR=outcomes$HHD.RUR.R)
CON_comp <- CON_comp %>%
  mutate(across(
    .cols = -c(T, X),  # Exclude columns "T" and "X"
    .fns = ~ ((. - median(.)) / median(.)) * 100  # Calculate % deviation with regard to median
  ))

CON_comp$T <- NULL
CON_comp$X <- NULL
variable_order <- colnames(CON_comp)[!(colnames(CON_comp) %in% c("T", "X"))]

CON_comp <- melt(CON_comp)

CON_comp$variable <- factor(CON_comp$variable, 
                            levels = variable_order)
p <- ggplot(CON_comp, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')
ggsave("CON_boxplots_vertical.pdf", plot = p, width = 5, height = 5)

CON_comp$variable <- factor(CON_comp$variable, 
                            levels = rev(variable_order) )
p <- ggplot(CON_comp, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none"
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')
ggsave("CON_boxplots_horizontal.pdf", plot = p, width = 5, height = 3)

ALL_comp <- ALL_comp %>%
  mutate(across(
    .cols = -c(T, X),  # Exclude columns "T" and "X"
    .fns = ~ case_when(
      # Apply this formula for the specified columns
      cur_column() %in% c("Hunger.Nat", "Hunger.Urb", "Hunger.Rur", "Poverty.Nat", "Poverty.Urb", "Poverty.Rur") ~ (. - median(.)),
      # Apply this formula for other columns
      TRUE ~ ((. / median(.)) - 1) * 100
    )
  ))

ALL_comp$T <- NULL
ALL_comp$X <- NULL
variable_order <- colnames(ALL_comp)[!(colnames(ALL_comp) %in% c("T", "X"))]

ALL_comp <- melt(ALL_comp)

ALL_comp$variable <- factor(ALL_comp$variable, 
                            levels = rev(variable_order) )
p <- ggplot(ALL_comp, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = variable), size = 0.75) + 
  scale_color_manual(values = colors, labels = updated_names) +
  scale_x_discrete(labels = names) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits = c(-15, 15)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 15),   
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15),   
    legend.position = "none",
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal grid lines
    panel.grid.major.x = element_line(color = "grey80", size = 0.5),  # Make major vertical grid lines more visible
  ) +
  xlab(NULL) +
  ylab('Deviation from baseline, %')
ggsave("ALL_boxplots_horizontal.pdf", plot = p, width = 6, height = 8)

outcomes_order <- as.data.frame(read_excel(path = "../orders_for_reporting.xlsx", sheet = "outcomes_order"))
outcomes <- outcomes %>%
  dplyr::select(all_of(outcomes_order$variable))
merged_data <- merge(shocks, outcomes, by = c("X", "T"), all = TRUE)
merged_data <- merged_data[order(merged_data$X,merged_data$T), ]
row.names(merged_data) <- NULL

################# computationally intensive part - start
if (!exists("all_rel_importance")) {

all_rel_importance <- data.frame()  # Initialize an empty data frame to store results

for (t_value in unique(merged_data$T)) {
  data_for_t <- subset(merged_data, T == t_value)
  
  for (outcome_var in names(outcomes)) {
    if (outcome_var %in% c("X", "T")) next
    
    # Output to R console for tracking progress
    cat("Calculating for outcome:", outcome_var, "and Year:", t_value, "\n")
    
    # Create the formula string dynamically
    formula_str <- paste(outcome_var, "~", paste(shock_vars, collapse = " + "))
    # Convert the string to a formula
    formula <- as.formula(formula_str)
    
    # Fitting the linear model on the filtered data
    model <- lm(formula, data = data_for_t)
    
    # Output R-squared and number of observations to R console
    model_summary <- summary(model)
    cat("Model R-squared:", model_summary$r.squared, "- Number of observations:", length(model$residuals), "\n")
    
    # Calculating relative importance
    set.seed(123)
    rel_importance <- calc.relimp(model, type = "lmg", rela = TRUE)

    # Convert the relative importance to a data frame and add columns for the outcome variable, T, and R-squared
    temp_df <- as.data.frame(rel_importance$lmg)
    temp_df$outcome_var <- outcome_var
    temp_df$T <- t_value
    temp_df$R_squared <- model_summary$r.squared  # Add R-squared value here
    temp_df$Metric <- rownames(temp_df)
    rownames(temp_df) <- NULL  # Clean up row names for binding

    # Bind this to the main data frame
    all_rel_importance <- rbind(all_rel_importance, temp_df)
    write.csv(temp_df, file = paste0(outcome_var, ".csv"), row.names = FALSE)
  }
}

names(all_rel_importance) <- c('Importance', 'Outcome', 'Year', 'R_squared', 'Variable')
all_rel_importance$Importance <- all_rel_importance$Importance*100
all_rel_importance$R_squared <- all_rel_importance$R_squared*100
all_rel_importance_lmg <- all_rel_importance
all_rel_importance_lmg$Variable <- ifelse(all_rel_importance_lmg$Variable == "pw_mine", "pw_ener", all_rel_importance_lmg$Variable)


############################################ RF
importance_RF_1 <- data.frame()  # Initialize for %IncMSE importance
importance_RF_2 <- data.frame()  # Initialize for IncNodePurity importance

for (t_value in unique(merged_data$T)) {
  data_for_t <- subset(merged_data, T == t_value)
  
  for (outcome_var in names(outcomes)) {
    if (outcome_var %in% c("X", "T")) next
    # outcome_var <- 'PRVCON'
    cat("Calculating for outcome:", outcome_var, "and Year:", t_value, "\n")
    
    formula_str <- paste(outcome_var, "~", paste(shock_vars, collapse = " + "))
    formula <- as.formula(formula_str)
    
    # Fit the random forest model
    rf_model <- randomForest(formula, data = data_for_t, ntree = 100, importance = TRUE)
    
    # # Fit a simpler decision tree using the rpart package
    # rpart_control <- rpart.control(maxdepth = 3) # Limit tree depth for simplicity
    # simple_tree_model <- rpart(formula, data = data_for_t, method = "anova", control = rpart_control)
    # pdf("simple_tree_plot.pdf", width = 6, height = 2.5)
    # # Set graphical parameters to use Times New Roman
    # par(family = "Times")
    # rpart.plot(simple_tree_model, type = 4, extra = 101, digits = 4)
    # dev.off() # Close the PDF device
      
    # Calculate R-squared value
    rsq_value <- round(100 * rf_model$rsq[length(rf_model$rsq)], 2)  # Assuming rsq is available and contains values for each tree
    
    # %IncMSE importance
    importance_vals_1 <- importance(rf_model, type = 1)
    temp_df_1 <- as.data.frame(t(importance_vals_1))
    colnames(temp_df_1) <- shock_vars
    temp_df_1$Year <- t_value
    temp_df_1$Outcome <- outcome_var
    temp_df_1$R_squared <- rsq_value  # Add R-squared value
    temp_df_1 <- temp_df_1[, c(ncol(temp_df_1)-2, ncol(temp_df_1)-1, ncol(temp_df_1), 1:(ncol(temp_df_1)-3))]
    
    # IncNodePurity importance
    importance_vals_2 <- importance(rf_model, type = 2)
    temp_df_2 <- as.data.frame(t(importance_vals_2))
    colnames(temp_df_2) <- shock_vars
    temp_df_2$Year <- t_value
    temp_df_2$Outcome <- outcome_var
    temp_df_2$R_squared <- rsq_value  # Add R-squared value
    temp_df_2 <- temp_df_2[, c(ncol(temp_df_2)-2, ncol(temp_df_2)-1, ncol(temp_df_2), 1:(ncol(temp_df_2)-3))]
    
    cat("Random Forest model for", outcome_var, "in year", t_value, "- Number of trees: 1000 \n",
        "Total variance explained = ", rsq_value, "\n")
    
    importance_RF_1 <- rbind(importance_RF_1, temp_df_1)
    importance_RF_2 <- rbind(importance_RF_2, temp_df_2)
  }
}

# Adjust the column names if needed
names(importance_RF_1)[1:3] <- c('Year', 'Outcome', 'R_squared')
names(importance_RF_2)[1:3] <- c('Year', 'Outcome', 'R_squared')

all_rel_importance_RF_IncMSE <-
  reshape2::melt(importance_RF_1, id.vars = c("Year", "Outcome", "R_squared"), variable.name = "Variable", value.name = "Importance")

all_rel_importance_RF_IncMSE <- all_rel_importance_RF_IncMSE[names(all_rel_importance)]
all_rel_importance_RF_IncMSE$Outcome <- factor(all_rel_importance_RF_IncMSE$Outcome,
                                               levels = unique(all_rel_importance$Outcome))

all_rel_importance_RF_IncMSE$Variable <- factor(all_rel_importance_RF_IncMSE$Variable,
                                                levels = unique(all_rel_importance$Variable))

all_rel_importance_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
  arrange(Outcome, Variable)
all_rel_importance_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
  group_by(Outcome, Year) %>%
  mutate(Importance = 100*Importance / sum(Importance))
all_rel_importance_RF_IncMSE$Variable <- ifelse(all_rel_importance_RF_IncMSE$Variable == "pw_mine", "pw_ener", 
                                                as.character(all_rel_importance_RF_IncMSE$Variable))


all_rel_importance_RF_IncNodePurity <-
  reshape2::melt(importance_RF_2, id.vars = c("Year", "Outcome", "R_squared"), variable.name = "Variable", value.name = "Importance")
all_rel_importance_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity[names(all_rel_importance)]
all_rel_importance_RF_IncNodePurity$Outcome <- factor(all_rel_importance_RF_IncNodePurity$Outcome,
                                                      levels = unique(all_rel_importance$Outcome))
all_rel_importance_RF_IncNodePurity$Variable <- factor(all_rel_importance_RF_IncNodePurity$Variable,
                                                       levels = unique(all_rel_importance$Variable))
all_rel_importance_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
  arrange(Outcome, Variable)
all_rel_importance_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
  group_by(Outcome, Year) %>%
  mutate(Importance = 100*Importance / sum(Importance))
all_rel_importance_RF_IncNodePurity$Variable <- ifelse(all_rel_importance_RF_IncNodePurity$Variable == "pw_mine", "pw_ener", 
                                                       as.character(all_rel_importance_RF_IncNodePurity$Variable))
} 
################################## end of comp intentsive
outcomes_order_filtered <- setdiff(outcomes_order$variable, c("X", "T"))
all_rel_importance_lmg <- all_rel_importance_lmg %>%
  filter(Outcome %in% outcomes_order_filtered)
all_rel_importance_lmg <- all_rel_importance_lmg %>%
  mutate(Outcome = factor(Outcome, levels = outcomes_order_filtered)) %>%
  arrange(Outcome)
all_rel_importance_lmg <- na.omit(all_rel_importance_lmg)
all_rel_importance_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
  filter(Outcome %in% outcomes_order_filtered)
all_rel_importance_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
  mutate(Outcome = factor(Outcome, levels = outcomes_order_filtered)) %>%
  arrange(Outcome)
all_rel_importance_RF_IncMSE <- na.omit(all_rel_importance_RF_IncMSE)
all_rel_importance_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
  filter(Outcome %in% outcomes_order_filtered)
all_rel_importance_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
  mutate(Outcome = factor(Outcome, levels = outcomes_order_filtered)) %>%
  arrange(Outcome)
all_rel_importance_RF_IncNodePurity <- na.omit(all_rel_importance_RF_IncNodePurity)

# Load the data
desc <- as.data.frame(read_excel(path = "C:/Work/AI/historical_uncertainty/data/mwi_2019/2sampled_scenarios_mwi_2019.xlsx", 
                                 sheet = "desc"))

# Define colors manually (needed later for pie chart)
colors_manual <- c("world markets" = "red1", 
                   "current account" = "deeppink4",
                   "domestic" = "green3") 
pdf("Analysis_of_importance_Lindeman_Merenda_Gold.pdf", width = 9, height = 4)
for (outcome in unique(all_rel_importance_lmg$Outcome)) {
  data_subset <- outcomes[, c("X", "T", outcome)]
  rsq_value <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    summarise(total_rsq = mean(R_squared)) %>%
    pull(total_rsq)
  # Determine y-axis title based on the outcome variable
  y_title <- ifelse(outcome %in% tail(names(outcomes), 6), "Share of respective population (%)","Billions of 2019 USD")
  # Box Plot
  box_plot <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    # geom_hline(yintercept = base[[outcome]], color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - levels", y = y_title)
  # Box Plot2
  if (outcome %in% tail(names(outcomes), 6)  ) {
    # data_subset[,outcome] <- (data_subset[,outcome] - quantile(data_subset[,3], 0.5)  )
    data_subset[,outcome] <- (data_subset[,outcome] - base[[outcome]])
  } else {
    # data_subset[,outcome] <- (data_subset[,outcome]/ quantile(data_subset[,3], 0.5) -1)*100    
    data_subset[,outcome] <- (data_subset[,outcome]/base[[outcome]]-1)*100
  }
  box_plot2 <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - deviation", y = 'diff. vs base')
  
  # Importance Plot for different methods
  importance_data <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable")
  imp_plot <- ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +  # Use colors as they are specified in the data
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Individual shares")
  # Pie Charts
  pie_data <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable") %>%
    group_by(category) %>%
    summarise(total_Importance = sum(Importance, na.rm = TRUE)) %>%
    mutate(label = sprintf("%.2f%%", total_Importance / sum(total_Importance) * 100))
  # Ensure 'category' is a factor and set the levels in the desired order
  pie_data$category <- factor(pie_data$category, levels = c("domestic", "current account","world markets"))
  
  pie_chart <- ggplot(pie_data, aes(x = "", y = total_Importance, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "black"),
          legend.key = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = colors_manual, labels = c(
      "domestic" = "domestic (pd)",
      "world markets" = "world markets (pw)", 
      "current account" = "current account (FSAV)")
    ) +
    labs(subtitle = paste("LM R sq. =", sprintf("%.2f%%",  rsq_value),
                          "\nAggregate categories of risk \n(% of R sq.)"))+
    guides(fill = guide_legend(nrow = 3))
  
  combined_plot <- grid.arrange(box_plot, box_plot2, pie_chart, imp_plot, 
                                ncol = 4,
                                nrow=1,
                                widths = c(0.85, 0.85, 0.85, 1),
                                top = outcome)
}
dev.off()

pdf("Analysis_of_importance_ALL.pdf", width = 8.27, height = 12)
for (outcome in unique(all_rel_importance_lmg$Outcome)) {
  data_subset <- outcomes[, c("X", "T", outcome)]
  
  # Calculate R-squared value for page title
  rsq_value <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    summarise(total_rsq = mean(R_squared)) %>%
    pull(total_rsq)
  
  rsq_value_RF <- all_rel_importance_RF_IncMSE %>%
    filter(Outcome == outcome) %>%
    summarise(total_rsq = mean(R_squared)) %>%
    pull(total_rsq)
  
  # Determine y-axis title based on the outcome variable
  y_title <- ifelse(outcome %in% tail(names(outcomes), 6), "Share of respective population (%)","Billions of 2019 USD")
  
  # Box Plot
  box_plot <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    # geom_hline(yintercept = base[[outcome]], color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - levels", y = y_title)
  # Box Plot2
  if (outcome %in% tail(names(outcomes), 6)  ) {
    # data_subset[,outcome] <- (data_subset[,outcome] - quantile(data_subset[,3], 0.5)  )
    data_subset[,outcome] <- (data_subset[,outcome] - base[[outcome]])
  } else {
    # data_subset[,outcome] <- (data_subset[,outcome]/ quantile(data_subset[,3], 0.5) -1)*100    
    data_subset[,outcome] <- (data_subset[,outcome]/base[[outcome]]-1)*100
  }
  box_plot2 <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - deviation", y = 'diff. vs base')
  
    # Importance Plot for different methods
  importance_data <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable")
  
  importance_data_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable")
  importance_data_RF_IncMSE$importance_lmg <- importance_data$Importance
  
  importance_data_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable")
  importance_data_RF_IncNodePurity$importance_lmg <- importance_data$Importance
  
  
  imp_plot <- ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +  # Use colors as they are specified in the data
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Individual shares")


  imp_plotRF1 <- ggplot(importance_data_RF_IncMSE, aes(x = reorder(Variable, importance_lmg), y = Importance, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Individual shares")
  
  imp_plotRF2 <- ggplot(importance_data_RF_IncNodePurity, aes(x = reorder(Variable, importance_lmg), y = Importance, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Individual shares")
  
  # Pie Charts
  pie_data <- all_rel_importance_lmg %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable") %>%
    group_by(category) %>%
    summarise(total_Importance = sum(Importance, na.rm = TRUE)) %>%
    mutate(label = sprintf("%.2f%%", total_Importance / sum(total_Importance) * 100))
  # Ensure 'category' is a factor and set the levels in the desired order
  pie_data$category <- factor(pie_data$category, levels = c("domestic", "current account","world markets"))
  
  pie_chart <- ggplot(pie_data, aes(x = "", y = total_Importance, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "black"),
          legend.key = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = colors_manual, labels = c("domestic" = "domestic (pd)", "world market" = "world market (pw and FSAV)", "other" = "Other")) +
    labs(subtitle = paste("LM R sq. =", sprintf("%.2f%%",  rsq_value),
                       "\nAggregate categories of risk \n(% of R sq.)"))+
    guides(fill = guide_legend(nrow = 3))
  
  pie_data <- all_rel_importance_RF_IncMSE %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable") %>%
    group_by(category) %>%
    summarise(total_Importance = sum(Importance, na.rm = TRUE)) %>%
    mutate(label = sprintf("%.2f%%", total_Importance / sum(total_Importance) * 100))
  # Ensure 'category' is a factor and set the levels in the desired order
  pie_data$category <- factor(pie_data$category, levels = c("domestic", "current account","world markets"))
  
  pie_chartRF1 <- ggplot(pie_data, aes(x = "", y = total_Importance, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "black"),
          legend.key = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = colors_manual, 
                      labels = c("domestic" = "domestic (pd)", "world market" = "world market (pw and FSAV)", "other" = "Other")) +
    labs(subtitle = paste("RF R sq. =", sprintf("%.2f%%", rsq_value_RF),
                          "\nAggregate categories of risk\n (% of IncMSE)"))+
    guides(fill = guide_legend(nrow = 3))
  
  pie_data <- all_rel_importance_RF_IncNodePurity %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable") %>%
    group_by(category) %>%
    summarise(total_Importance = sum(Importance, na.rm = TRUE)) %>%
    mutate(label = sprintf("%.2f%%", total_Importance / sum(total_Importance) * 100))
  # Ensure 'category' is a factor and set the levels in the desired order
  pie_data$category <- factor(pie_data$category, levels = c("domestic", "current account","world markets"))
  
  pie_chartRF2 <- ggplot(pie_data, aes(x = "", y = total_Importance, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "black"),
          legend.key = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = colors_manual, 
                      labels = c("domestic" = "domestic (pd)", "world market" = "world market (pw and FSAV)", "other" = "Other")) +
    labs(subtitle = paste("RF R sq. =", sprintf("%.2f%%", rsq_value_RF),
                          "\nAggregate categories of risk \n(% of IncNodePurity)"))+
    guides(fill = guide_legend(nrow = 3))
  combined_plot <- grid.arrange(box_plot, pie_chart, imp_plot, 
                                box_plot2, pie_chartRF2, imp_plotRF2, 
                                box_plot2, pie_chartRF1, imp_plotRF1, 
                                ncol = 3,
                                nrow=3,
                                widths = c(1, 1, 1),
                                top = outcome)
}
dev.off()


final_lmg <- all_rel_importance_lmg %>%
  dplyr::select(Variable, Outcome, Importance)
# Pivot the data to have unique Variables and Outcomes as columns
final_lmg <- final_lmg %>%
  pivot_wider(names_from = Outcome, values_from = Importance)
final_lmg <- final_lmg %>%
  dplyr::select(all_of(c('Variable', setdiff(outcomes_order$variable, c("X", "T"))) ))

R_sq_lin <- all_rel_importance_lmg %>%
  distinct(Outcome, .keep_all = TRUE)
R_sq_lin$Importance <- NULL
R_sq_lin$Variable <- NULL
R_sq_lin$Year <- NULL
R_sq_lin <- as.data.frame(t(R_sq_lin))
colnames(R_sq_lin) <- R_sq_lin[1, ]
R_sq_lin$Variable <- row.names(R_sq_lin)
R_sq_lin <- R_sq_lin[-1, ]
R_sq_lin <- R_sq_lin[,c(ncol(R_sq_lin), 2:ncol(R_sq_lin)-1)]
row.names(R_sq_lin) <- NULL
write.csv(rbind(R_sq_lin, final_lmg), 'lmg.csv')


final_RF_IncMSE <- all_rel_importance_RF_IncMSE %>%
  dplyr::select(Variable, Outcome, Importance)
final_RF_IncMSE$Year <-NULL
# Pivot the data to have unique Variables and Outcomes as columns
final_RF_IncMSE <- final_RF_IncMSE %>%
  pivot_wider(names_from = Outcome, values_from = Importance)
final_RF_IncMSE <- final_RF_IncMSE %>%
  dplyr::select(all_of(c('Variable', setdiff(outcomes_order$variable, c("X", "T"))) ))
R_sq_RF <- all_rel_importance_RF_IncMSE %>%
  distinct(Outcome, .keep_all = TRUE)
R_sq_RF$Importance <- NULL
R_sq_RF$Variable <- NULL
R_sq_RF$Year <- NULL
R_sq_RF <- as.data.frame(t(R_sq_RF))
colnames(R_sq_RF) <- R_sq_RF[1, ]
R_sq_RF$Variable <- row.names(R_sq_RF)
R_sq_RF <- R_sq_RF[-1, ]
R_sq_RF <- R_sq_RF[,c(ncol(R_sq_RF), 2:ncol(R_sq_RF)-1)]
row.names(R_sq_RF) <- NULL
write.csv(rbind(R_sq_RF, final_RF_IncMSE), 'RF_IncMSE.csv')


final_RF_IncNodePurity <- all_rel_importance_RF_IncNodePurity %>%
  dplyr::select(Variable, Outcome, Importance)
final_RF_IncNodePurity$Year <-NULL
# Pivot the data to have unique Variables and Outcomes as columns
final_RF_IncNodePurity <- final_RF_IncNodePurity %>%
  pivot_wider(names_from = Outcome, values_from = Importance)
final_RF_IncNodePurity <- final_RF_IncNodePurity %>%
  dplyr::select(all_of(c('Variable', setdiff(outcomes_order$variable, c("X", "T"))) ))
write.csv(rbind(R_sq_RF, final_RF_IncNodePurity), 'RF_IncNodePurity.csv')



all_rel_importance_compared <- do.call(rbind, lapply(unique(all_rel_importance_lmg$Outcome), function(outcome) {
  lmg_rows <- all_rel_importance_lmg[all_rel_importance_lmg$Outcome == outcome, ]
  rf_rows <- all_rel_importance_RF_IncNodePurity[all_rel_importance_RF_IncNodePurity$Outcome == outcome, ]
  if (max(lmg_rows$R_squared) > max(rf_rows$R_squared)) {
    cbind(lmg_rows, Method = "Linear")
  } else {
    cbind(rf_rows, Method = "RF")
  }
}))


R_sq_final <- all_rel_importance_compared %>%
  distinct(Outcome, .keep_all = TRUE)
R_sq_final$Importance <- NULL
R_sq_final$Variable <- NULL
R_sq_final$Year <- NULL
R_sq_final <- as.data.frame(t(R_sq_final))
colnames(R_sq_final) <- R_sq_final[1, ]
R_sq_final$Variable <- row.names(R_sq_final)
R_sq_final <- R_sq_final[-1, ]
R_sq_final <- R_sq_final[,c(ncol(R_sq_final), 2:ncol(R_sq_final)-1)]
row.names(R_sq_final) <- NULL

pdf("Analysis_of_importance_final.pdf", width = 9, height = 4)
for (outcome in unique(all_rel_importance_compared$Outcome)) {
  data_subset <- outcomes[, c("X", "T", outcome)]
  rsq_value <- all_rel_importance_compared %>%
    filter(Outcome == outcome) %>%
    summarise(total_rsq = mean(R_squared)) %>%
    pull(total_rsq)
  # Determine y-axis title based on the outcome variable
  y_title <- ifelse(outcome %in% tail(names(outcomes), 6), "Share of respective population (%)","Billions of 2019 USD")
  # Box Plot
  box_plot <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    # geom_hline(yintercept = base[[outcome]], color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - levels", y = y_title)
  # Box Plot2
  if (outcome %in% tail(names(outcomes), 6)  ) {
    # data_subset[,outcome] <- (data_subset[,outcome] - quantile(data_subset[,3], 0.5)  )
    data_subset[,outcome] <- (data_subset[,outcome] - base[[outcome]])
  } else {
    # data_subset[,outcome] <- (data_subset[,outcome]/ quantile(data_subset[,3], 0.5) -1)*100    
    data_subset[,outcome] <- (data_subset[,outcome]/base[[outcome]]-1)*100
  }
  box_plot2 <- ggplot(data_subset, aes(x = factor(1), y = .data[[outcome]])) +
    geom_boxplot(alpha = 0.5) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.025) , color = "blue", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = quantile(data_subset[,3], 0.975) , color = "blue", linetype = "dashed", size = 0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype = "dashed"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Uncertainty - deviation", y = 'diff. vs base')
  
  # Importance Plot for different methods
  importance_data <- all_rel_importance_compared %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable")
  imp_plot <- ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +  # Use colors as they are specified in the data
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Individual shares")
  # Pie Charts
  pie_data <- all_rel_importance_compared %>%
    filter(Outcome == outcome) %>%
    left_join(desc, by = "Variable") %>%
    group_by(category) %>%
    summarise(total_Importance = sum(Importance, na.rm = TRUE)) %>%
    mutate(label = sprintf("%.2f%%", total_Importance / sum(total_Importance) * 100))
  # Ensure 'category' is a factor and set the levels in the desired order
  pie_data$category <- factor(pie_data$category, levels = c("domestic", "current account","world markets"))
  
  pie_chart <- ggplot(pie_data, aes(x = "", y = total_Importance, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "black"),
          legend.key = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = colors_manual, labels = c(
      "domestic" = "domestic (pd)",
      "world markets" = "world markets (pw)", 
      "current account" = "current account (FSAV)")
    ) 
  type <- all_rel_importance_compared %>%  filter(Outcome == outcome)
  type <- type$Method[1]
  if (type=="Linear"   ) {
    pie_chart <- pie_chart  +
      labs(subtitle = paste("LM R sq. =", sprintf("%.2f%%",  rsq_value),
                            "\nAggregate categories of risk \n(% of R sq.)"))+
      guides(fill = guide_legend(nrow = 3))
  } else {
    pie_chart <- pie_chart  +
      labs(subtitle = paste("RF R sq. =", sprintf("%.2f%%",  rsq_value),
                            "\nAggregate categories of risk \n(% of R sq.)"))+
      guides(fill = guide_legend(nrow = 3))}
  
  
  combined_plot <- grid.arrange(box_plot, box_plot2, pie_chart, imp_plot, 
                                ncol = 4,
                                nrow=1,
                                widths = c(0.85, 0.85, 0.85, 1),
                                top = outcome)
}
dev.off()

final_importance <- all_rel_importance_compared %>%
  dplyr::select(Variable, Outcome, Importance)
final_importance$Year <-NULL
# Pivot the data to have unique Variables and Outcomes as columns
final_importance <- final_importance %>%
  pivot_wider(names_from = Outcome, values_from = Importance)
final_importance <- final_importance %>%
  dplyr::select(all_of(c('Variable', setdiff(outcomes_order$variable, c("X", "T"))) ))

final_importance <- rbind(R_sq_final, final_importance)
write.csv(final_importance, 'final_importance.csv')