Replication of Tables (Thesis)
================

Packages to Load

``` r
library(dplyr) # For Data Manipulation
library(lme4)  # For Linear Mixed-effects Modeling
library(lmerTest) # For Type III Anova
library(texreg) # Last three packages for Tables
library(stargazer) # Table Creation
library(xtable) # Table Creation
```

Encoding and Data

``` r
encoding <- "UTF-8" 
csv_file <- "sen_youtube_data.csv" 
df <- read.csv(csv_file, fileEncoding = encoding) # Load in the necessary file. Make sure its in the working directory.
```

Table 5.1: Descriptive Statistics Video Level

``` r
vars_video <- df[, c("log_viewCount", "durationMin", "pos_prob",
                     "neu_prob", "neg_prob", "compound_score", "log_likeCount")]
stargazer(
  vars_video,
  title = "Descriptive Statistics: Video Level Variables",
  type = "text",
  summary.stat = c("mean", "median", "sd", "min", "max"),
  align = TRUE,
  covariate.labels = c("View Count (Log)", "Duration (Minutes)", 
                       "Positive Sentiment", "Neutral Sentiment",
                       "Negative Sentiment", "Compound Score", "Like Count (Log)*"),
  digits = 2, 
  omit = "Constant",
  notes = c("N = 44,100 (videos). Stargazer: February 16th, 2024.", 
            "*N = 39,100."),
  label = "tab:video_level"
)
```

    ## 
    ## Descriptive Statistics: Video Level Variables
    ## =========================================================
    ## Statistic             Mean  Median St. Dev.  Min    Max  
    ## ---------------------------------------------------------
    ## View Count (Log)      5.63   5.35    1.80    0.00  14.85 
    ## Duration (Minutes)    6.36   5.32    5.98    0.12  59.87 
    ## Positive Sentiment    0.28   0.15    0.27   0.002   0.99 
    ## Neutral Sentiment     0.47   0.51    0.18   0.005   0.94 
    ## Negative Sentiment    0.25   0.21    0.22   0.001   0.98 
    ## Compound Score        0.02  -0.06    0.46   -0.97   0.99 
    ## Like Count (Log)*     2.28   1.95    1.79    0.00  11.28 
    ## ---------------------------------------------------------
    ## N = 44,100 (videos). Stargazer: February 16th, 2024.     
    ## *N = 39,100.

``` r
# Output is currently in Text Format. The thesis used type=  "Latex".
```

Table 5.2: Descriptive Statistics Senator Level

``` r
vars_sens <- df[, c("abs_nom", "age", "seniority", "log_subscriber")]
unique_vars_sens <- vars_sens[!duplicated(vars_sens), ]
stargazer(
  unique_vars_sens,
  title = "Descriptive Statistics: Senator Level Variables",
  type = "text",
  summary.stat = c("mean", "median", "sd", "min", "max"),
  align = TRUE,
  covariate.labels = c("*Ideology", "Age",
                       "Seniority", "Subscriber Count (Logged)"),
  digits = 2, 
  omit = "Constant",
  notes = c("N = 99 (senators). Stargazer: February 16th, 2024.", 
            "*Ideology is the absolute value of the DW-NOMINATE score."), 
  label = "tab:senator_level"
)
```

    ## 
    ## Descriptive Statistics: Senator Level Variables
    ## ==============================================================
    ## Statistic                    Mean  Median St. Dev.  Min   Max 
    ## --------------------------------------------------------------
    ## *Ideology                    0.44   0.41    0.19   0.06  0.94 
    ## Age                         64.47    66    10.79    36    90  
    ## Seniority                   12.28    11     8.51     1    43  
    ## Subscriber Count (Logged)    7.60   7.31    1.74   2.89  12.48
    ## --------------------------------------------------------------
    ## N = 99 (senators). Stargazer: February 16th, 2024.            
    ## *Ideology is the absolute value of the DW-NOMINATE score.

Table 6.1: Intercept Models (1-4)

``` r
null_model_int <- lmer(log_viewCount ~ 1 + (1 | channelId ), data = df)
intercept_model1 <- lmer(log_viewCount ~ neg_prob  + abs_nom +
                           (1 | channelId ), data = df)
intercept_model2 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                           seniority + gender + (1 | channelId ), data = df)
intercept_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                           seniority + gender +  log_subscriber +(1 | channelId ), data = df)
model_list_6_1 <- list(null_model_int, intercept_model1, intercept_model2,intercept_model3)

latex_code_6_1 <- texreg(
  model_list_6_1,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Negative Sentiment",
                        "Ideology", "Duration (Minutes)", "Seniority", 
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Random-Intercept Models)", 
  label = "table:models1",
  custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)",
                       "Var: Residual")
)

cat(latex_code_6_1)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Random-Intercept Models)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 1 & Model 2 & Model 3 & Model 4 \\
    ## \midrule
    ## Intercept                & $5.51^{***}$ & $4.70^{***}$ & $3.83^{***}$ & $1.59^{***}$ \\
    ##                          & $(0.11)$     & $(0.27)$     & $(0.38)$     & $(0.32)$     \\
    ## Negative Sentiment       &              & $0.91^{***}$ & $0.90^{***}$ & $0.90^{***}$ \\
    ##                          &              & $(0.03)$     & $(0.03)$     & $(0.03)$     \\
    ## Ideology                 &              & $1.31^{*}$   & $1.87^{**}$  & $-0.08$      \\
    ##                          &              & $(0.56)$     & $(0.59)$     & $(0.42)$     \\
    ## Duration (Minutes)       &              &              & $0.03^{***}$ & $0.03^{***}$ \\
    ##                          &              &              & $(0.00)$     & $(0.00)$     \\
    ## Seniority                &              &              & $0.03^{*}$   & $-0.01$      \\
    ##                          &              &              & $(0.01)$     & $(0.01)$     \\
    ## Gender (M)               &              &              & $0.03$       & $-0.09$      \\
    ##                          &              &              & $(0.23)$     & $(0.15)$     \\
    ## Subscribers (Log)        &              &              &              & $0.48^{***}$ \\
    ##                          &              &              &              & $(0.04)$     \\
    ## \midrule
    ## AIC                      & $151508.25$  & $150620.80$  & $149832.88$  & $149762.89$  \\
    ## BIC                      & $151534.34$  & $150664.27$  & $149902.44$  & $149841.14$  \\
    ## Log Likelihood           & $-75751.13$  & $-75305.40$  & $-74908.44$  & $-74872.45$  \\
    ## Num. obs.                & $44100$      & $44100$      & $44100$      & $44100$      \\
    ## Num. groups: Channels    & $99$         & $99$         & $99$         & $99$         \\
    ## Var: Channel (Intercept) & $1.16$       & $1.05$       & $0.98$       & $0.42$       \\
    ## Var: Residual            & $1.80$       & $1.76$       & $1.73$       & $1.73$       \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models1}
    ## \end{center}
    ## \end{table}

Table 6.2: Random Slope Models (5-8)

``` r
base_model_slope <- lmer(log_viewCount ~  1 + (1 + neg_prob| channelId ), data = df)
negative_model1 <- lmer(log_viewCount ~ neg_prob + abs_nom +
                          (1 + neg_prob| channelId ), data = df)
negative_model2 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                          seniority  + gender  +
                          (1 + neg_prob| channelId ), data = df)
negative_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                          seniority  + gender  + log_subscriber +
                          (1 + neg_prob| channelId ), data = df)

model_list_6_2 <- list(base_model_slope, negative_model1, negative_model2, negative_model3)

latex_code_6_2 <- texreg(
  model_list_6_2,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Negative Sentiment", "Ideology", 
                        "Duration (Minutes)", "Seniority",
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Random Slope)", 
  label = "table:models2",
  custom.model.names = c("Model 5", "Model 6", "Model 7", "Model 8"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Negativity", 
                       "Cov: Channel (Int.) Neg.", "Var: Residual")
)
cat(latex_code_6_2)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Random Slope)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 5 & Model 6 & Model 7 & Model 8 \\
    ## \midrule
    ## Intercept                & $5.20^{***}$ & $4.81^{***}$ & $3.84^{***}$ & $1.64^{***}$ \\
    ##                          & $(0.10)$     & $(0.26)$     & $(0.36)$     & $(0.33)$     \\
    ## Negative Sentiment       &              & $0.80^{***}$ & $0.81^{***}$ & $0.79^{***}$ \\
    ##                          &              & $(0.08)$     & $(0.08)$     & $(0.08)$     \\
    ## Ideology                 &              & $1.13^{*}$   & $1.75^{**}$  & $-0.01$      \\
    ##                          &              & $(0.53)$     & $(0.55)$     & $(0.43)$     \\
    ## Duration (Minutes)       &              &              & $0.03^{***}$ & $0.03^{***}$ \\
    ##                          &              &              & $(0.00)$     & $(0.00)$     \\
    ## Seniority                &              &              & $0.04^{**}$  & $-0.01$      \\
    ##                          &              &              & $(0.01)$     & $(0.01)$     \\
    ## Gender (M)               &              &              & $0.03$       & $-0.07$      \\
    ##                          &              &              & $(0.22)$     & $(0.16)$     \\
    ## Subscribers (Log)        &              &              &              & $0.47^{***}$ \\
    ##                          &              &              &              & $(0.04)$     \\
    ## \midrule
    ## AIC                      & $150217.27$  & $150156.85$  & $149376.96$  & $149323.36$  \\
    ## BIC                      & $150260.74$  & $150217.71$  & $149463.90$  & $149419.00$  \\
    ## Log Likelihood           & $-75103.63$  & $-75071.43$  & $-74678.48$  & $-74650.68$  \\
    ## Num. obs.                & $44100$      & $44100$      & $44100$      & $44100$      \\
    ## Num. groups: Channels    & $99$         & $99$         & $99$         & $99$         \\
    ## Var: Channel (Intercept) & $1.00$       & $0.93$       & $0.86$       & $0.45$       \\
    ## Var: Channel Negativity  & $1.14$       & $0.53$       & $0.49$       & $0.48$       \\
    ## Cov: Channel (Int.) Neg. & $0.24$       & $0.15$       & $0.17$       & $-0.12$      \\
    ## Var: Residual            & $1.74$       & $1.74$       & $1.71$       & $1.71$       \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models2}
    ## \end{center}
    ## \end{table}

Table 6.3: Composite Score Models (9-12)

``` r
compound_model1 <- lmer(log_viewCount ~ 1  + 
                          (1 + compound_score| channelId ), data = df)
compound_model2 <- lmer(log_viewCount ~  compound_score + abs_nom  +
                          (1 + compound_score| channelId ), data = df)
compound_model3 <- lmer(log_viewCount ~ compound_score + abs_nom + durationMin +
                          seniority  + gender  + 
                          (1 + compound_score| channelId ), data = df)
compound_model4 <- lmer(log_viewCount ~ compound_score + abs_nom + durationMin +
                          seniority  + gender  + log_subscriber +
                          (1 + compound_score| channelId ), data = df)

model_list_6_3 <- list(compound_model1, compound_model2, compound_model3,compound_model4)

latex_code_6_3 <- texreg(
  model_list_6_3,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Composite Score",
                        "Ideology", "Duration (Minutes)", "Seniority", "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Composite Score)", 
  label = "table:models3",
  custom.model.names = c("Model 9", "Model 10", "Model 11", "Model 12"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Sentiment", 
                       "Cov: Channel (Int.) Sent.", "Var: Residual")
)

cat(latex_code_6_3)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Composite Score)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 9 & Model 10 & Model 11 & Model 12 \\
    ## \midrule
    ## Intercept                 & $5.11^{***}$ & $5.11^{***}$  & $4.24^{***}$  & $1.91^{***}$  \\
    ##                           & $(0.10)$     & $(0.25)$      & $(0.35)$      & $(0.33)$      \\
    ## Composite Score           &              & $-0.44^{***}$ & $-0.41^{***}$ & $-0.40^{***}$ \\
    ##                           &              & $(0.04)$      & $(0.04)$      & $(0.04)$      \\
    ## Ideology                  &              & $0.91$        & $1.52^{**}$   & $-0.00$       \\
    ##                           &              & $(0.51)$      & $(0.53)$      & $(0.43)$      \\
    ## Duration (Minutes)        &              &               & $0.03^{***}$  & $0.03^{***}$  \\
    ##                           &              &               & $(0.00)$      & $(0.00)$      \\
    ## Seniority                 &              &               & $0.03^{**}$   & $-0.00$       \\
    ##                           &              &               & $(0.01)$      & $(0.01)$      \\
    ## Gender (M)                &              &               & $0.00$        & $-0.07$       \\
    ##                           &              &               & $(0.21)$      & $(0.16)$      \\
    ## Subscribers (Log)         &              &               &               & $0.46^{***}$  \\
    ##                           &              &               &               & $(0.04)$      \\
    ## \midrule
    ## AIC                       & $149836.30$  & $149775.71$   & $149140.71$   & $149092.88$   \\
    ## BIC                       & $149879.78$  & $149836.57$   & $149227.65$   & $149188.52$   \\
    ## Log Likelihood            & $-74913.15$  & $-74880.85$   & $-74560.35$   & $-74535.44$   \\
    ## Num. obs.                 & $44100$      & $44100$       & $44100$       & $44100$       \\
    ## Num. groups: Channels     & $99$         & $99$          & $99$          & $99$          \\
    ## Var: Channel (Intercept)  & $1.25$       & $1.04$        & $0.97$        & $0.43$        \\
    ## Var: Channel Sentiment    & $0.34$       & $0.16$        & $0.15$        & $0.15$        \\
    ## Cov: Channel (Int.) Sent. & $-0.37$      & $-0.19$       & $-0.18$       & $-0.01$       \\
    ## Var: Residual             & $1.72$       & $1.72$        & $1.70$        & $1.70$        \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models3}
    ## \end{center}
    ## \end{table}

Table 7.1: Random-effect Anova

``` r
negative_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                          seniority  + gender  + log_subscriber +
                          (1 + neg_prob| channelId ), data = df)
intercept_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                           seniority + gender +  log_subscriber + 
                           (1 | channelId ), data = df)

ranova_negative <- ranova(negative_model3)
ranova_intercept <- ranova(intercept_model3)
combined_results <- rbind(negative_model3 = ranova_negative, intercept_model3 = ranova_intercept)

rownames(combined_results) <- c("Negative Slope Model", "Removing Random Slope", 
                                "Negative Intercept Model", "Removing Random Intercept")
combined_results <- combined_results[, -which(names(combined_results) == "AIC")]
last_col <- ncol(combined_results)
combined_results[, last_col] <- sprintf("%.2e", combined_results[, last_col])

print(xtable(combined_results, caption = "Random-effects ANOVA"), 
      caption.placement = "top", include.rownames = TRUE)
```

    ## % latex table generated in R 4.3.3 by xtable 1.8-4 package
    ## % Thu May 16 17:02:39 2024
    ## \begin{table}[ht]
    ## \centering
    ## \caption{Random-effects ANOVA} 
    ## \begin{tabular}{lrrrrr}
    ##   \hline
    ##  & npar & logLik & LRT & Df & Pr($>$Chisq) \\ 
    ##   \hline
    ## Negative Slope Model & 11.00 & -74650.68 &  &  & NA \\ 
    ##   Removing Random Slope & 9.00 & -74872.45 & 443.53 & 2 & 4.89e-97 \\ 
    ##   Negative Intercept Model & 9.00 & -74872.45 &  &  & NA \\ 
    ##   Removing Random Intercept & 8.00 & -77447.87 & 5150.84 & 1 & 0.00e+00 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

Table 7.2: Likelihood Ratio Test

``` r
anova_results <- anova(negative_model3, intercept_model3)
```

    ## refitting model(s) with ML (instead of REML)

``` r
anova_df <- as.data.frame(anova_results)
anova_df$`Pr(>Chisq)` <- sprintf("%.2e", anova_df$`Pr(>Chisq)`)

print(xtable(anova_df, caption = "Likelihood Ratio Test Comparing Model 4 and Model 8"),
      caption.placement = "top")
```

    ## % latex table generated in R 4.3.3 by xtable 1.8-4 package
    ## % Thu May 16 17:02:40 2024
    ## \begin{table}[ht]
    ## \centering
    ## \caption{Likelihood Ratio Test Comparing Model 4 and Model 8} 
    ## \begin{tabular}{rrrrrrrrl}
    ##   \hline
    ##  & npar & AIC & BIC & logLik & deviance & Chisq & Df & Pr($>$Chisq) \\ 
    ##   \hline
    ## intercept\_model3 & 9.00 & 149727.94 & 149806.19 & -74854.97 & 149709.94 &  &  & NA \\ 
    ##   negative\_model3 & 11.00 & 149290.54 & 149386.17 & -74634.27 & 149268.54 & 441.40 & 2.00 & 1.41e-96 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

Table A.1: Like Count Models (13-16)

``` r
base_like_model <- lmer(log_likeCount ~  1 + (1 + neg_prob| channelId ), data = df)
like_model1 <- lmer(log_likeCount ~ neg_prob + abs_nom +
                      (1 + neg_prob| channelId ), data = df)
like_model2 <- lmer(log_likeCount ~ neg_prob + abs_nom + durationMin +
                      seniority  + gender  +
                      (1 + neg_prob| channelId ), data = df)
like_model3 <- lmer(log_likeCount ~ neg_prob + abs_nom + durationMin +
                      seniority  + gender  + log_subscriber +
                      (1 + neg_prob| channelId ), data = df)

model_list_A_1 <- list(base_like_model, like_model1, like_model2, like_model3)

latex_code_A_1 <- texreg(
  model_list_A_1,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Negative Sentiment", "Ideology", 
                        "Duration (Minutes)", "Seniority",
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Like Count)", 
  label = "table:models4",
  custom.model.names = c("Model 13", "Model 14", "Model 15", "Model 16"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Negativity", 
                       "Cov: Channel (Int.) Neg.", "Var: Residual")
)
cat(latex_code_A_1)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Like Count)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 13 & Model 14 & Model 15 & Model 16 \\
    ## \midrule
    ## Intercept                & $1.51^{***}$ & $1.18^{***}$ & $0.39$       & $-2.16^{***}$ \\
    ##                          & $(0.10)$     & $(0.26)$     & $(0.37)$     & $(0.32)$      \\
    ## Negative Sentiment       &              & $0.88^{***}$ & $0.89^{***}$ & $0.89^{***}$  \\
    ##                          &              & $(0.09)$     & $(0.09)$     & $(0.09)$      \\
    ## Ideology                 &              & $1.35^{*}$   & $1.81^{**}$  & $0.49$        \\
    ##                          &              & $(0.55)$     & $(0.57)$     & $(0.42)$      \\
    ## Duration (Minutes)       &              &              & $0.02^{***}$ & $0.02^{***}$  \\
    ##                          &              &              & $(0.00)$     & $(0.00)$      \\
    ## Seniority                &              &              & $0.03^{**}$  & $-0.01$       \\
    ##                          &              &              & $(0.01)$     & $(0.01)$      \\
    ## Gender (M)               &              &              & $0.05$       & $-0.06$       \\
    ##                          &              &              & $(0.22)$     & $(0.15)$      \\
    ## Subscribers (Log)        &              &              &              & $0.49^{***}$  \\
    ##                          &              &              &              & $(0.04)$      \\
    ## \midrule
    ## AIC                      & $122032.09$  & $121964.60$  & $121502.52$  & $121441.44$   \\
    ## BIC                      & $122074.96$  & $122024.62$  & $121588.26$  & $121535.75$   \\
    ## Log Likelihood           & $-61011.05$  & $-60975.30$  & $-60741.26$  & $-60709.72$   \\
    ## Num. obs.                & $39100$      & $39100$      & $39100$      & $39100$       \\
    ## Num. groups: Channels    & $96$         & $96$         & $96$         & $96$          \\
    ## Var: Channel (Intercept) & $1.11$       & $0.96$       & $0.90$       & $0.40$        \\
    ## Var: Channel Negativity  & $1.29$       & $0.55$       & $0.52$       & $0.52$        \\
    ## Cov: Channel (Int.) Neg. & $0.51$       & $0.22$       & $0.22$       & $-0.07$       \\
    ## Var: Residual            & $1.30$       & $1.30$       & $1.29$       & $1.29$        \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models4}
    ## \end{center}
    ## \end{table}

Table A.2: Channels with more than 50 videos Models (17-20)

``` r
channel_counts <- table(df$channelId)
filtered_channel_df <- df[df$channelId %in% names(channel_counts[channel_counts >= 50]), ]

base_channel_model <- lmer(log_viewCount ~  1 + (1 + neg_prob| channelId ),
                           data = filtered_channel_df)
channel_model1 <- lmer(log_viewCount ~ neg_prob + abs_nom +
                         (1 + neg_prob| channelId ), data = filtered_channel_df)
channel_model2 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                         seniority  + gender  +
                         (1 + neg_prob| channelId ), data = filtered_channel_df)
channel_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                         seniority  + gender  + log_subscriber +
                         (1 + neg_prob| channelId ), data = filtered_channel_df)

model_list_A_2 <- list(base_channel_model, channel_model1, channel_model2,channel_model3)

latex_code_A_2 <- texreg(
  model_list_A_2,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Negative Sentiment", "Ideology", 
                        "Duration (Minutes)", "Seniority",
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Without channels w/ under 50 videos)", 
  label = "table:models5",
  custom.model.names = c("Model 17", "Model 18", "Model 19", "Model 20"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Negativity", 
                       "Cov: Channel (Int.) Neg.", "Var: Residual")
)
cat(latex_code_A_2)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Without channels w/ under 50 videos)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 17 & Model 18 & Model 19 & Model 20 \\
    ## \midrule
    ## Intercept                & $5.13^{***}$ & $4.56^{***}$ & $3.58^{***}$ & $0.99^{***}$ \\
    ##                          & $(0.10)$     & $(0.26)$     & $(0.36)$     & $(0.29)$     \\
    ## Negative Sentiment       &              & $0.80^{***}$ & $0.81^{***}$ & $0.80^{***}$ \\
    ##                          &              & $(0.09)$     & $(0.08)$     & $(0.08)$     \\
    ## Ideology                 &              & $1.57^{**}$  & $2.09^{***}$ & $-0.04$      \\
    ##                          &              & $(0.54)$     & $(0.54)$     & $(0.37)$     \\
    ## Duration (Minutes)       &              &              & $0.03^{***}$ & $0.03^{***}$ \\
    ##                          &              &              & $(0.00)$     & $(0.00)$     \\
    ## Seniority                &              &              & $0.03^{**}$  & $-0.01$      \\
    ##                          &              &              & $(0.01)$     & $(0.01)$     \\
    ## Gender (M)               &              &              & $0.16$       & $0.12$       \\
    ##                          &              &              & $(0.22)$     & $(0.13)$     \\
    ## Subscribers (Log)        &              &              &              & $0.53^{***}$ \\
    ##                          &              &              &              & $(0.04)$     \\
    ## \midrule
    ## AIC                      & $149460.15$  & $149398.23$  & $148619.21$  & $148539.69$  \\
    ## BIC                      & $149503.59$  & $149459.05$  & $148706.10$  & $148635.26$  \\
    ## Log Likelihood           & $-74725.08$  & $-74692.11$  & $-74299.60$  & $-74258.84$  \\
    ## Num. obs.                & $43864$      & $43864$      & $43864$      & $43864$      \\
    ## Num. groups: Channels    & $90$         & $90$         & $90$         & $90$         \\
    ## Var: Channel (Intercept) & $0.95$       & $0.84$       & $0.77$       & $0.32$       \\
    ## Var: Channel Negativity  & $1.15$       & $0.53$       & $0.49$       & $0.49$       \\
    ## Cov: Channel (Int.) Neg. & $0.27$       & $0.12$       & $0.14$       & $-0.15$      \\
    ## Var: Residual            & $1.74$       & $1.74$       & $1.71$       & $1.71$       \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models5}
    ## \end{center}
    ## \end{table}

Table A.3: Removed videos with extremely large view counts Model (21-24)

``` r
z_scores_log <- (df$log_viewCount - mean(df$log_viewCount)) / sd(df$log_viewCount)
outliers_log <- df[abs(z_scores_log) > 3, ]
outlier_indices <- which(abs(z_scores_log) > 3)
df_outliers <- df[-outlier_indices, ]


base_outliers_model <- lmer(log_viewCount ~  1 + (1 + neg_prob| channelId ), data = df_outliers)
outlier_model1 <- lmer(log_viewCount ~ neg_prob + abs_nom +
                         (1 + neg_prob| channelId ), data = df_outliers)
outlier_model2 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                         seniority  + gender +
                         (1 + neg_prob| channelId ), data = df_outliers)
outlier_model3 <- lmer(log_viewCount ~ neg_prob + abs_nom + durationMin +
                         seniority  + gender  + log_subscriber +
                         (1 + neg_prob| channelId ), data = df_outliers)

model_list_A_3 <- list(base_outliers_model, outlier_model1, outlier_model2,outlier_model3)

latex_code_A_3 <- texreg(
  model_list_A_3,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Negative Sentiment", "Ideology", 
                        "Duration (Minutes)", "Seniority",
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Outliers Removed)", 
  label = "table:models6",
  custom.model.names = c("Model 21", "Model 22", "Model 23", "Model 24"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Negativity", 
                       "Cov: Channel (Int.) Neg.", "Var: Residual")
)
cat(latex_code_A_3)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Outliers Removed)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 21 & Model 22 & Model 23 & Model 24 \\
    ## \midrule
    ## Intercept                & $5.29^{***}$ & $4.79^{***}$ & $3.85^{***}$ & $1.79^{***}$ \\
    ##                          & $(0.10)$     & $(0.25)$     & $(0.35)$     & $(0.32)$     \\
    ## Negative Sentiment       &              & $0.72^{***}$ & $0.73^{***}$ & $0.71^{***}$ \\
    ##                          &              & $(0.08)$     & $(0.08)$     & $(0.08)$     \\
    ## Ideology                 &              & $1.15^{*}$   & $1.76^{**}$  & $-0.04$      \\
    ##                          &              & $(0.52)$     & $(0.54)$     & $(0.42)$     \\
    ## Duration (Minutes)       &              &              & $0.03^{***}$ & $0.03^{***}$ \\
    ##                          &              &              & $(0.00)$     & $(0.00)$     \\
    ## Seniority                &              &              & $0.04^{**}$  & $-0.01$      \\
    ##                          &              &              & $(0.01)$     & $(0.01)$     \\
    ## Gender (M)               &              &              & $0.04$       & $-0.06$      \\
    ##                          &              &              & $(0.21)$     & $(0.15)$     \\
    ## Subscribers (Log)        &              &              &              & $0.45^{***}$ \\
    ##                          &              &              &              & $(0.04)$     \\
    ## \midrule
    ## AIC                      & $144233.77$  & $144177.95$  & $143420.71$  & $143364.52$  \\
    ## BIC                      & $144277.20$  & $144238.75$  & $143507.56$  & $143460.06$  \\
    ## Log Likelihood           & $-72111.88$  & $-72081.98$  & $-71700.35$  & $-71671.26$  \\
    ## Num. obs.                & $43710$      & $43710$      & $43710$      & $43710$      \\
    ## Num. groups: Channels    & $99$         & $99$         & $99$         & $99$         \\
    ## Var: Channel (Intercept) & $0.95$       & $0.89$       & $0.82$       & $0.44$       \\
    ## Var: Channel Negativity  & $0.98$       & $0.47$       & $0.44$       & $0.44$       \\
    ## Cov: Channel (Int.) Neg. & $0.07$       & $0.07$       & $0.09$       & $-0.12$      \\
    ## Var: Residual            & $1.56$       & $1.56$       & $1.53$       & $1.53$       \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models6}
    ## \end{center}
    ## \end{table}

Table A.4: Positive Sentiment Models (25-28)

``` r
base_pos_model <- lmer(log_viewCount ~  1 + (1 + pos_prob| channelId ), data = df)
positive_model1 <- lmer(log_viewCount ~ pos_prob + abs_nom +
                          (1 + pos_prob| channelId ), data = df)
positive_model2 <- lmer(log_viewCount ~ pos_prob + abs_nom + durationMin +
                          seniority  + gender  +
                          (1 + pos_prob| channelId ), data = df)
positive_model3 <- lmer(log_viewCount ~ pos_prob + abs_nom + durationMin +
                          seniority  + gender  + log_subscriber +
                          (1 + pos_prob| channelId ), data = df)

model_list_A_4 <- list(base_pos_model, positive_model1, positive_model2, positive_model3)

latex_code_A_4 <- texreg(
  model_list_A_4,
  include.rsquared = TRUE,
  custom.coef.names = c("Intercept", "Positive Sentiment", "Ideology", 
                        "Duration (Minutes)", "Seniority",
                        "Gender (M)", "Subscribers (Log)"),
  use.viewer = FALSE, dcolumn = FALSE, booktabs = TRUE,
  center = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Multilevel Linear Regression (Positive Sentiment)", 
  label = "table:models7",
  custom.model.names = c("Model 25", "Model 26", "Model 27", "Model 28"),
  custom.gof.names = c("AIC", "BIC", "Log Likelihood", "Num. obs.", 
                       "Num. groups: Channels", "Var: Channel (Intercept)", 
                       "Var: Channel Positivity", 
                       "Cov: Channel (Int.) Pos.", "Var: Residual")
)
cat(latex_code_A_4)
```

    ## 
    ## \begin{table}
    ## \caption{Multilevel Linear Regression (Positive Sentiment)}
    ## \begin{center}
    ## \begin{tabular}{l c c c c}
    ## \toprule
    ##  & Model 25 & Model 26 & Model 27 & Model 28 \\
    ## \midrule
    ## Intercept                & $5.07^{***}$ & $5.37^{***}$  & $4.54^{***}$  & $2.15^{***}$  \\
    ##                          & $(0.09)$     & $(0.25)$      & $(0.35)$      & $(0.33)$      \\
    ## Positive Sentiment       &              & $-0.76^{***}$ & $-0.68^{***}$ & $-0.66^{***}$ \\
    ##                          &              & $(0.08)$      & $(0.08)$      & $(0.08)$      \\
    ## Ideology                 &              & $0.76$        & $1.38^{**}$   & $0.01$        \\
    ##                          &              & $(0.50)$      & $(0.53)$      & $(0.43)$      \\
    ## Duration (Minutes)       &              &               & $0.03^{***}$  & $0.03^{***}$  \\
    ##                          &              &               & $(0.00)$      & $(0.00)$      \\
    ## Seniority                &              &               & $0.03^{**}$   & $-0.00$       \\
    ##                          &              &               & $(0.01)$      & $(0.01)$      \\
    ## Gender (M)               &              &               & $-0.02$       & $-0.07$       \\
    ##                          &              &               & $(0.21)$      & $(0.16)$      \\
    ## Subscribers (Log)        &              &               &               & $0.45^{***}$  \\
    ##                          &              &               &               & $(0.04)$      \\
    ## \midrule
    ## AIC                      & $149876.82$  & $149818.95$   & $149275.26$   & $149229.49$   \\
    ## BIC                      & $149920.29$  & $149879.80$   & $149362.20$   & $149325.12$   \\
    ## Log Likelihood           & $-74933.41$  & $-74902.47$   & $-74627.63$   & $-74603.74$   \\
    ## Num. obs.                & $44100$      & $44100$       & $44100$       & $44100$       \\
    ## Num. groups: Channels    & $99$         & $99$          & $99$          & $99$          \\
    ## Var: Channel (Intercept) & $1.72$       & $1.28$        & $1.18$        & $0.48$        \\
    ## Var: Channel Positivity  & $1.06$       & $0.51$        & $0.49$        & $0.49$        \\
    ## Cov: Channel (Int.) Pos. & $-1.00$      & $-0.52$       & $-0.46$       & $-0.16$       \\
    ## Var: Residual            & $1.72$       & $1.72$        & $1.70$        & $1.70$        \\
    ## \bottomrule
    ## \multicolumn{5}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \label{table:models7}
    ## \end{center}
    ## \end{table}
