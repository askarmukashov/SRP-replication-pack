# actually needed
packages <- c("tidyverse", "gridExtra", "readxl", "limSolve", "Matrix", 
              "EnvStats","MVN","pracma",'KernSmooth','scales',
              'EnvStats','reshape2','KernSmooth','zoo','purrr','tidyr','Hmisc','mFilter','corrplot')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)
# clear the environment and the console
rm(list=ls(all=TRUE)) 
cat("\014")

set.seed(123)

indices <- as.data.frame(read_excel(path = "1. Historical_data_for_sampling.xlsx", 
                                    sheet = "historical" ))
descr <- as.data.frame(read_excel("1. Historical_data_for_sampling.xlsx", 
                                  sheet = "desc"))
# detrend
deviation_abs <- indices
deviation_rel <- indices
lt_trend <- indices
year_col_index <- which(colnames(indices) == "year")

for(i in 2:ncol(indices)) { # Loop starts from 2 to skip the 'year' column
  if(!is.na(tail(indices[[i]], 1))) {
    # If the last observation is not NA, apply the filter to the entire column
    data_to_filter <- indices[[i]]
  } else {
    # If the last observation is NA, subset the data to exclude 2023
    data_to_filter <- indices[indices$year != 2023, i]
  }
  
  # Apply HP filter with lambda = 100 for annual data to the appropriate subset
  hp_fit <- hpfilter(data_to_filter, freq = 100)
  
  # Initialize columns if they don't exist yet
  if (!is.vector(lt_trend[[i]])) {
    lt_trend[[i]] <- rep(NA, nrow(indices))
    deviation_abs[[i]] <- rep(NA, nrow(indices))
    deviation_rel[[i]] <- rep(NA, nrow(indices))
  }
  
  # Store the trend component
  lt_trend[indices$year != 2023 | !is.na(indices[[i]]), i] <- hp_fit$trend
  
  # Calculate absolute deviation from the trend
  deviation_abs[indices$year != 2023 | !is.na(indices[[i]]), i] <- indices[[i]][indices$year != 2023 | !is.na(indices[[i]])] - hp_fit$trend
  
  # Calculate relative deviation factor from the trend
  deviation_rel[indices$year != 2023 | !is.na(indices[[i]]), i] <- 100 * (indices[[i]][indices$year != 2023 | !is.na(indices[[i]])] / hp_fit$trend - 1)
}

# Fill in missing values for 2023 if there was no data to filter
lt_trend[is.na(lt_trend)] <- NA
deviation_abs[is.na(deviation_abs)] <- NA
deviation_rel[is.na(deviation_rel)] <- NA

deviation <- deviation_rel
deviation$FSAV<- deviation_abs$FSAV
write.csv(lt_trend,'lt_trend.csv')
together <- list()
for(i in 1:ncol(indices[,-1]))
  local({
    selected <- data.frame(year = indices$year,
                           yield = indices[, i+1])
    p1 <- ggplot(data=selected) +
      geom_line(aes(x = year, y = yield, color='actual')) +
      geom_line(data=lt_trend[lt_trend$year <= 2023, ], aes(x = year, y =  lt_trend[lt_trend$year <= 2023, i+1], color='trend'))  +
      ylab(descr$Units[i]) +
      xlab(NULL) +
      scale_x_continuous(breaks=seq(1975, 2050, 1), expand = c(0.01, 0.01)) +
      theme_minimal() +
      scale_y_continuous(breaks = pretty_breaks(n = 10))+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position='bottom',
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 8), # Rotate x-axis labels
            axis.text.y = element_text(size = 10),
            panel.grid.minor.x = element_blank())
    together[[i]] <<- grid.arrange(p1, ncol=1, 
                                   top= descr$Long[i])
  })
dev.off()
ggsave("3a.Dynamics_historical_RWA.pdf",
       marrangeGrob(together, nrow = 2, ncol = 1))

together <- list()
for(i in 1:ncol(deviation[,-1]))
  local({
    selected <- data.frame(year = deviation$year,
                           yield = deviation[, i+1])
    p1 <- ggplot(data=selected) +
      geom_line(aes(x = year, y = yield, color='actual')) +
      ylab('deviation from trend (%)') +
      xlab(NULL) +
      scale_x_continuous(breaks=seq(1975, 2050, 1), expand = c(0.01, 0.01)) +
      theme_minimal() +
      geom_hline(yintercept = 0, color = "gray") +
      scale_y_continuous(breaks = pretty_breaks(n = 10))+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position='bottom',
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 8), # Rotate x-axis labels
            axis.text.y = element_text(size = 10),
            panel.grid.minor.x = element_blank())
    together[[i]] <<- grid.arrange(p1, ncol=1, 
                                   top= descr$Long[i])
  })
dev.off()
ggsave("3b.Dynamics_historical_deviation_from_trend_RWA.pdf",
       marrangeGrob(together, nrow = 2, ncol = 1))

sigmas <- apply(deviation[,-1], 2, function(x) sd(x, na.rm = TRUE))
# means <- apply(deviation[,-1], 2, mean)
means <- rep(0, length(sigmas))

corr <- as.matrix(cor(na.omit(deviation[,-1]), method = "spearman"))
corr <- nearPD(corr, corr = TRUE, keepDiag = TRUE, 
               ensureSymmetry=TRUE, trace = TRUE, maxit = 10000)
corr <- as.matrix(corr$mat)

corr_long <- melt(corr)

# Rename columns for clarity
colnames(corr_long) <- c("Variable1", "Variable2", "Correlation")

# Convert factors to characters to avoid comparison issues
corr_long$Variable1 <- as.character(corr_long$Variable1)
corr_long$Variable2 <- as.character(corr_long$Variable2)

# Filter out redundant pairs and the diagonal (where Variable1 == Variable2)
corr_filtered <- corr_long %>%
  filter(Variable1 != Variable2) %>%
  distinct(Variable1, Variable2, .keep_all = TRUE) %>%
  # Ensure we don't have duplicate pairs in reverse order, now safely using character types
  mutate(pair_id = pmin(Variable1, Variable2), pair_id2 = pmax(Variable1, Variable2)) %>%
  group_by(pair_id, pair_id2) %>%
  slice(1) %>%
  ungroup() %>%
  select(-pair_id, -pair_id2)

# Sort the dataframe by the absolute values of the correlations, in descending order
corr_sorted <- corr_filtered %>%
  arrange(desc(abs(Correlation)))

write.csv(corr_sorted,'corr_sorted.csv')

distributions <- rep("norm", ncol(corr))
names(distributions) <- row.names(corr)
param <- list()
for(i in 1:ncol(corr)) {
  local({
    i <- i
    p <-   list(mean = means[i], sd = sigmas[i])
    param[[i]] <<- p
  })
}

shocks <- data.frame(simulateMvMatrix(10000,
                                      distributions = distributions, 
                                      param.list = param, 
                                      cor.mat = corr, 
                                      sample.method = "LHS",
                                      seed = 123,
                                      left.tail.cutoff =descr$left,
                                      right.tail.cutoff=descr$right
))

historical <- deviation[,-1]
together <- list()
for(i in 1: ncol(historical) )
  local({
    i <- i
    # i <- 1
    p_historical_i <- ggplot() +
      geom_density(aes(x = na.omit(historical[,i]))) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.05)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.5)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.95)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      xlab('Variation from trend (%)') +
      ylab("Density") +
      ggtitle("Historical") +
      scale_x_continuous(breaks = pretty_breaks(n = 10)) +
      coord_cartesian(xlim = c(min(na.omit(historical[,i]), shocks[,i]), 
                               max(na.omit(historical[,i]), shocks[,i]))) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position='none', text = element_text(size = 14))
    
    p_sampled_i <- ggplot() +
      geom_density(aes(x = shocks[,i])) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.05)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.5)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      geom_vline(aes(xintercept = quantile(na.omit(historical[,i]), probs = 0.95)),
                 linetype ="dashed",
                 linewidth = 0.5) +
      xlab('Variation from trend (%)') +
      ylab("Density") +
      ggtitle("Sampled") +
      scale_x_continuous(breaks = pretty_breaks(n = 10)) +
      coord_cartesian(xlim = c(min(na.omit(historical[,i]), shocks[,i]), 
                               max(na.omit(historical[,i]), shocks[,i]))) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position='none', text = element_text(size = 14))
    
    max_y <-      max(ggplot_build(p_sampled_i)$layout$panel_scales_y[[1]]$range$range[2],
                      ggplot_build(p_historical_i)$layout$panel_scales_y[[1]]$range$range[2])
    p_sampled_i <- p_sampled_i +       scale_y_continuous(limits = c(0, max_y),
                                                          breaks = pretty_breaks(n = 10))
    
    p_historical_i <- p_historical_i +      scale_y_continuous(limits = c(0, max_y),
                                                               breaks = pretty_breaks(n = 10))
    
    together[[i]] <<- grid.arrange(
      p_historical_i,
      p_sampled_i,
      ncol=2,
      top = descr$Long[i])
  })
dev.off()
ggsave(filename="3c. Density_of_sampled_shocks_RWA.pdf", 
       plot = marrangeGrob(together, nrow = 1, ncol = 1),
       width = 14, 
       height = 7)

lt_trend$year <- as.numeric(as.character(lt_trend$year))
lt_trend_filtered <- subset(lt_trend, year >= 2019)

shocks_yearly <- do.call(rbind, lapply(split(lt_trend_filtered, lt_trend_filtered$year), 
                                       function(df) {
  replicated_df <- df[rep(1, 10000), ]
  replicated_df$X <- 1:10000
  return(replicated_df)
}))

shocks_yearly <- shocks_yearly[, c("year", "X", 
                                   setdiff(names(shocks_yearly), c("year", "X")))]
rownames(shocks_yearly) <- NULL
shocks$X <- 1:10000

shocks_yearly_aligned <- merge(shocks_yearly, shocks, by = "X", suffixes = c("", ".shock"))

variable_names <- setdiff(names(shocks_yearly), c("year", "X"))
for(variable_name in variable_names) {
  if (variable_name == "FSAV") {
  shocks_yearly_aligned[[variable_name]] <- 
    shocks_yearly_aligned[[variable_name]] + shocks_yearly_aligned[[paste(variable_name, ".shock", sep = "")]]
  } else {
    shocks_yearly_aligned[[variable_name]] <- 
    shocks_yearly_aligned[[variable_name]] * (1+shocks_yearly_aligned[[paste(variable_name, ".shock", sep = "")]]/100)
  }
  }

# Now remove the additional shock columns
shocks_yearly_const_dist <- 
  shocks_yearly_aligned[, !(names(shocks_yearly_aligned) %in% paste(variable_names, ".shock", sep = ""))]

# Order the dataframe by 'year' and 'X', both in ascending order
shocks_yearly_const_dist <- shocks_yearly_const_dist[order(shocks_yearly_const_dist$year, shocks_yearly_const_dist$X), ]

variable_order <- c("year", "X", setdiff(names(shocks_yearly_const_dist), c("year", "X")))

# Rearrange the columns based on the desired order
shocks_yearly_const_dist <- shocks_yearly_const_dist[, variable_order]
rownames(shocks_yearly_const_dist) <- NULL

conf_intervals_list <- list()
for (var_name in names(shocks_yearly_const_dist)[-c(1, 2)]) {
  ci_data <- shocks_yearly_const_dist %>%
    group_by(year) %>%
    summarise(mean =  mean(!!sym(var_name), na.rm = TRUE),
              lwr = min(!!sym(var_name),  na.rm = TRUE),
              upr = max(!!sym(var_name), na.rm = TRUE)) %>%
    ungroup() %>%
    # Add a column to identify the variable this CI data belongs to
    mutate(variable = var_name)
  
  # Add the CI data for this variable to the list
  conf_intervals_list[[var_name]] <- ci_data
}

# Combine all confidence interval dataframes into one
conf_intervals_df <- bind_rows(conf_intervals_list, .id = "variable")

# Remove all rows with year > 2022
conf_intervals_df <- conf_intervals_df[conf_intervals_df$year <= 2023, ]
conf_intervals_df[is.nan(conf_intervals_df$mean), c("mean", "lwr", "upr")] <- NA

together <- list()
for(i in 1:ncol(indices[,-1]))
  local({
    # i <- 27
    # Extract the current variable's name
    current_var_name <- names(shocks_yearly_const_dist)[-c(1, 2)][i]
    # Filter the confidence intervals for the current variable
    ci_data_current_var <- conf_intervals_df %>% filter(variable == current_var_name)
    selected <- data.frame(year = indices$year,
                           yield = indices[, i+1])
    p1 <- ggplot(data=selected) +
      geom_line(aes(x = year, y = yield, color='actual')) +
      geom_line(data=lt_trend[lt_trend$year <= 2023, ], aes(x = year, y =  lt_trend[lt_trend$year <= 2023, i+1], color='trend'))  +
      geom_ribbon(data = ci_data_current_var,
                  aes(x = year, ymin = lwr, ymax = upr), fill = "lightblue", alpha = 0.5) +
      ylab(descr$Units[i]) +
      xlab(NULL) +
      scale_x_continuous(breaks=seq(1975, 2023, 1), expand = c(0.01, 0.01)) + 
      theme_minimal() +
      scale_y_continuous(breaks = pretty_breaks(n = 10))+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position='bottom',
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 8), # Rotate x-axis labels
            axis.text.y = element_text(size = 10),
            panel.grid.minor.x = element_blank())+
      guides(fill = "none")
    if (current_var_name == "FSAV") {
      p1 <- p1 + geom_hline(yintercept = 0, color = "gray") 
    } else {
      p1 <- p1 + geom_hline(yintercept = 100, color = "gray") 
    }
    together[[i]] <<- grid.arrange(p1, ncol=1, 
                                   top= descr$Long[i])
  })
dev.off()
ggsave("3d.Dynamics_historical_with_sampled_intervals_RWA.pdf",
       marrangeGrob(together, nrow = 2, ncol = 1))

# for presentation
write.csv(deviation,'deviation.csv')
write.csv(corr,'corr.csv')
write.csv(sigmas,'sigmas.csv')

shocks$NEXR <- NULL
shocks$T <- 2019
# Reorder columns: 'X', 'T', followed by remaining columns except 'X' and 'T'
shocks <- shocks[c("X", "T", setdiff(names(shocks), c("X", "T")))]

rebase <- as.data.frame(
  c(X = 0, T = 2019, -deviation[deviation$year == 2019, -which(names(deviation) == "year")]))
rebase$NEXR <- NULL

shocks <- rbind(rebase, shocks)

write.csv(shocks,'4.Sampled_scenarios_RWA.csv')


cov <- as.matrix(cov(na.omit(deviation[,-1])))
cov <- cov[!(rownames(cov) == "NEXR"), !(colnames(cov) == "NEXR")]

pdf("cov_RWA.pdf")
corrplot(cov, 
         method = 'square',
         order = "original",  # Keep the original order of variables
         tl.col = "black",    # Text label color,
         type = c("upper"),
         is.corr = FALSE)
dev.off()

write.csv(cov,'cov_RWA.csv')