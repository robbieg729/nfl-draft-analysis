# Helper function to plot year-over-year differences in Madden Ratings
plot_yoy_diff <- function(df, pos){
  overalls_by_year <- df %>%
    filter(season <= 2024) %>%
    group_by(player_id) %>%
    mutate(year_diff=overall - lag(overall)) %>%
    group_by(playing_year) %>%
    summarize(mean_diff=mean(year_diff, na.rm=TRUE),
              median_diff=median(year_diff, na.rm=TRUE),
              sd_diff=sd(year_diff, na.rm=TRUE)) %>%
    filter(playing_year != 1)
  ggplot(data=overalls_by_year, aes(x=playing_year, y=mean_diff)) +
    geom_bar(stat="identity", fill="darkblue") +
    scale_x_continuous(name="Playing Year", breaks=2:12) + 
    ylab("Mean Difference in Madden Rating") +
    labs(title="Mean Difference in Madden Ratings from Previous Year",
         subtitle=paste(pos, "s Drafted from 2012-2023", sep="")) +
    theme(axis.title=element_text(size=15), axis.text=element_text(size=12),
          plot.title=element_text(size=18), plot.subtitle=element_text(size=14))
  ggsave(paste("../Charts/Madden Ratings/YoY Diff/", pos, ".pdf", sep=""),
         width=7, height=7)
}

# Helper function to plot linear regression line of Madden Rating vs. Proportion of time as roster type
plot_roster_type_lm <- function(df, pos){
  third_to_fifth <- df %>%
    filter(season <= 2023, !is.na(overall))
  
  df1 <- select(third_to_fifth, overall, active_prop)
  colnames(df1)[3] <- "proportion"
  df2 <- select(third_to_fifth, overall, practice_squad_prop)
  colnames(df2)[3] <- "proportion"
  g <- ggplot(data=df1, aes(y=overall, x=proportion)) +
    geom_smooth(data=df1, aes(x=proportion, color="Active"), method="lm", formula=y~x) +
    geom_smooth(data=df2, aes(x=proportion, color="Practice Squad/Non-Rostered"), method="lm") +
    xlab("Proportion of games in each season") + ylab("Madden Rating") + 
    labs(title="Madden Rating vs. Proportion of Games on Roster Type",
         subtitle=paste(pos, "s drafted from 2012-2023", sep=""),
         color="Roster Status")
  ggsave(paste("../Charts/Madden Ratings/Roster Type/", pos, ".pdf", sep=""),
         width=7, height=7)
}

# Helper function to plot histogram of Madden Ratings faceted by primary roster type
plot_roster_type_faceted <- function(df, pos){
  ggplot(data=df %>% filter(!is.na(overall)), aes(x=overall)) +
    geom_histogram(aes(y=after_stat(density)), fill="darkblue") +
    facet_wrap(vars(status), nrow=2, scales="free") +
    xlab("Madden Rating") + ylab("Density") +
    labs(title="Madden Ratings Histogram by Season Roster Type",
         subtitle=paste(pos, "s Drafted 2012-2023", sep=""))  +
    theme(axis.title=element_text(size=15), axis.text=element_text(size=12),
          plot.title=element_text(size=18), plot.subtitle=element_text(size=14),
          strip.text=element_text(size=15))
  ggsave(paste("../Charts/Madden Ratings/Roster Type Histograms/", pos, ".pdf", sep=""),
         width=7, height=10)
}

# Helper function to fit an exponential model
fit_exp_model <- function(df, xvar, start_=NULL){
  if (!grepl("score", xvar) & xvar !="rating"){
    if (is.null(start_)){
      theta.0 <- min(df$mean_overall) * 0.5  
      
      # Estimate the rest parameters using a linear model
      model.0 <- lm(log(mean_overall - theta.0) ~ x, data=df)  
      alpha.0 <- exp(coef(model.0)[1])
      beta.0 <- coef(model.0)[2]
    }
    else{
      alpha.0 <- start_[1]
      beta.0 <- start_[2]
      theta.0 <- start_[3]
    }
    
    # Starting parameters
    start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
    
    model <- nls(mean_overall ~ alpha * exp(beta * x) + theta, data=df,
                 start=start)
  }
  else{
    model <- nls(mean_overall ~ alpha * exp(beta * x), data=df, start=list(alpha=40, beta=0.1))
  }
  
  
  p <- predict.nls(model, newdata=data.frame(x = df$x),
                   interval="confidence", se.fit=TRUE, level=0.95)
  
  ss_res <- sum((df$mean_overall - p$fit$fit)^2)
  ss_tot <- sum((df$mean_overall - mean(df$mean_overall))^2)
  model$r2 <- 1 - ss_res/ss_tot
  model$fit <- p$fit$fit
  model$lwr <- p$fit$lwr
  model$upr <- p$fit$upr
  
  return (model)
}

# Helper function to get position correlations for given variable
get_model_correlations <- function(df, xvar, xvar_lab){
  if (xvar=="position_rank"){
    result <- data.frame()
    for (pos in unique(df$position)){
      ranks <- df %>%
        filter(position==pos) %>%
        mutate(x=n() + 1 - rank(mean_overall, ties.method="min"),
               y=position_rank) %>%
        arrange(y) %>%
        select(x, y)
      pos_result <- data.frame(position=pos, draft_year="All", kendalls=cor(ranks$x, ranks$y,
                                                                           method="kendall"))
      for (year in 2012:2021){
        ranks <- df %>%
          filter(position==pos, draft_year==year) %>%
          mutate(x=n() + 1 - rank(mean_overall, ties.method="min"),
                 y=position_rank) %>%
          arrange(y) %>%
          select(x, y)
        pos_result <- rbind(pos_result, data.frame(position=pos, draft_year=year,
                            kendalls=cor(ranks$x, ranks$y, method="kendall")))
      }
      result <- rbind(result, pos_result)
    }
    return (result)
  }
  else{
    lin_r2 <- c(0)
    exp_r2 <- c(0)
    loess_r2 <- c(0)
    correlations <- c(0)
    positions <- append(unique(df$position), "All")
    for (pos in positions){
      eligible_players <- df
      if (pos != "All"){
        eligible_players <- eligible_players %>% filter(position==pos)
      }
      lin <- fit_linear_model(eligible_players %>% 
                                mutate(x=!!sym(xvar),
                                       y=mean_overall))
      m <- fit_exp_model(eligible_players %>%
                           mutate(x=!!sym(xvar),
                                  y=mean_overall),
                         xvar)
      l <- fit_loess_model(eligible_players %>%
                             mutate(x=!!sym(xvar),
                                    y=mean_overall))
      lin_r2 <- append(lin_r2, lin$r2)
      exp_r2 <- append(exp_r2, m$r2)
      loess_r2 <- append(loess_r2, l$r2)
      
      
      correlations <- append(correlations, cor(eligible_players$mean_overall,
                                               log(as.vector(unlist(eligible_players %>%
                                                                  mutate(x=!!sym(xvar)) %>%
                                                                  filter(!is.na(x)) %>%
                                                                  select(x))))))
      expn_coefs <- as.vector(coef(m))
      plot_draft_curve(eligible_players %>% 
                         mutate(x=!!sym(xvar)), xvar, "mean_overall",
                       xvar_lab, "Average Madden Rating", 
                       title=paste("Draft Curve for Average Madden Rating in Years 3-5 vs.", xvar_lab),
                       subtitle=paste(ifelse(pos=="All", "All Offensive/Defensive Player", pos), "s Drafted ", ifelse(xvar=="ovr_rank", "2016-21", "2012-21"), sep=""),
                       path=paste("../Charts/Draft Curves/Average Madden Ratings/", 
                                  xvar_lab, "/Position/Curves/", pos, " Exp.pdf", sep=""),
                       method="lm", method_str="Exponential", r2=m$r2, expn=TRUE,
                       expn_params=list(alpha=expn_coefs[1], beta=expn_coefs[2],
                                        theta=ifelse(xvar!="final_score", expn_coefs[3], 0)))
      
    }
    corrs <- data.frame(position=append(NA,positions),
                        lin_r2=lin_r2,
                        exp_r2=exp_r2,
                        loess_r2=loess_r2,
                        correlations=correlations)
    corrs$xvar <- rep(xvar, nrow(corrs))
    return (corrs=corrs %>% filter(lin_r2 != 0))
  }
  
}

# Helper function to get model R-Squared for each year and position
get_yearly_r2s <- function(df, xvar, xvar_label, each=TRUE){
  results <- data.frame()
  for (pos in rev(unique(df$position))){
    pos_results <- data.frame()
    m <- fit_exp_model(df %>%
                         filter(position==pos) %>%
                         mutate(x=!!sym(xvar)), xvar)
    coefs <- as.vector(coef(m))
    pos_fits <- df %>%
      filter(position==pos) %>%
      mutate(fit=m$fit)
    pos_results <- rbind(pos_results, data.frame(position=pos, draft_year="All", r2=m$r2))
    if (each){
      for (year in 2012:2021){
        pos_fits_yr <- pos_fits %>% filter(draft_year==year)
        ss_res <- sum((pos_fits_yr$mean_overall - pos_fits_yr$fit)^2)
        ss_tot <- sum((pos_fits_yr$mean_overall - mean(pos_fits$mean_overall))^2)
        year_r2 <- 1 - ss_res/ss_tot
        pos_results <- rbind(pos_results, data.frame(position=pos, draft_year=year, r2=year_r2))
      }
    }
    results <- rbind(results, pos_results)
  }
  return (results)
}

# Helper function to plot model correlations of different correlation types
plot_model_correlations <- function(corrs, xvar_lab){
  if (xvar_lab=="Draft Positional Rank"){
    for (pos in unique(corrs$position)){
      pos_corrs <- corrs %>% filter(position==pos)
      ggplot(data=pos_corrs, aes(x=draft_year, y=kendalls)) +
        geom_bar(stat="identity", fill="darkblue") +
        xlab("Draft Year") + ylab("Kendall Rank Coefficient") +
        labs(title="Kendall Rank Coefficient between Average Madden Rating in Years 3-5 and\nDraft Positional Rank",
             subtitle=paste(pos, "s Drafted 2012-21", sep="")) +
        scale_x_continuous(breaks=min(corrs$draft_year):max(corrs$draft_year))
      ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Positional Rank/Kendall/",
                   pos, ".pdf", sep=""), width=7, height=7)
    }
    corrs <- corrs %>%
      filter(draft_year != "All") %>%
      mutate(draft_year=as.numeric(draft_year)) %>%
      group_by(position) %>%
      mutate(label=ifelse(!(rank(kendalls, ties.method="min") %in% 2:9), draft_year, "")) %>%
      group_by(draft_year) %>%
      mutate(label_yr=ifelse(!(rank(kendalls, ties.method="min") %in% 2:9), draft_year, ""))
        
    ggplot(data=corrs, aes(x=kendalls, y=reorder(position, kendalls, median))) +
      geom_boxplot() + geom_point() + geom_text_repel(aes(label=label), size=5) +
      xlab("Kendall's Tau") + ylab("Position") +
      labs(title="Kendall's Tau between Average Madden Rating in Years 3-5 and Draft\nPositional Rank",
           subtitle="Draft Years 2012-21") + 
      theme(axis.text=element_text(size=12), axis.title=element_text(size=15), 
            plot.title=element_text(size=20), plot.subtitle=element_text(size=15))
    ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Positional Rank/Kendall/Boxplot.pdf",
           width=12, height=7)
    
    ggplot(data=corrs, aes(x=kendalls, y=reorder(draft_year, -draft_year, mean))) +
      geom_boxplot() + geom_point() + geom_text_repel(aes(label=label_yr), size=5) +
      xlab("Kendall's Tau") + ylab("Draft Year") +
      labs(title="Kendall's Tau between Average Madden Rating in Years 3-5 and Draft\nPositional Rank",
           subtitle="Draft Years 2012-21") +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=15), 
            plot.title=element_text(size=20), plot.subtitle=element_text(size=15))
    ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Positional Rank/Kendall/Boxplot Year.pdf",
           width=12, height=7)
  }
  else{
    ggplot(data=corrs, aes(x=reorder(position, -exp_r2))) +
      geom_point(aes(y=lin_r2, color="Linear"), size=2) +
      geom_point(aes(y=exp_r2, color="Exp."), size=2) +
      geom_point(aes(y=loess_r2, color="Loess"), size=2) +
      scale_color_discrete(breaks=c("Loess", "Exp.", "Linear"),
                           type=c("red", "lightgreen", "darkblue")) +
      xlab("Position") + ylab("R-Squared") +
      labs(title="R-Squared for varying model types",
           subtitle=paste(xvar_lab, " Model", sep=""),
           color="Model") 
    ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Model Comparisons/", xvar_lab, ".pdf", sep=""), 
           width=7, height=7)
  }
}

# Helper function to plot a draft curve
plot_draft_curve <- function(df, x, df_metric, x_label, df_metric_label, title, subtitle,
                             path, method="loess", method_str="Loess",
                             expn=FALSE, expn_params=list(alpha=0, beta=0, theta=0),
                             facet=FALSE, facet_order=NULL, r2=NULL, add_points=TRUE, save=TRUE){
  if (expn == FALSE){
    if (df_metric=="draft_number"){
      g <- ggplot(data=df %>% mutate(x=!!sym(x), y=!!sym(df_metric)), aes(x=x, y=y)) +
        geom_smooth(method=method, span=0.5, formula=(y~log(x))) + ylim(c(0, 262))
    }
    else{
      g <- ggplot(data=df %>% mutate(x=!!sym(x), y=!!sym(df_metric)), aes(x=x, y=y)) +
        geom_smooth(method=method, span=0.5)
    }
    
  }
  else{
    alpha <- expn_params$alpha
    beta <- expn_params$beta
    theta <- expn_params$theta
    df <- df %>%
      mutate(x=(!!sym(x)), y=(!!sym(df_metric)))
    g <- ggplot(data=df, aes(x=x, y=y))
    if (x=="final_score" | x=="rating"){
      g <- g + stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x)),
                           method.args=list(start=list(alpha=alpha, beta=beta)))
    }
    else{
      g <- g + stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x) + theta),
                           method.args=list(start=list(alpha=alpha, beta=beta, theta=theta)))
    }
  }
  g <- g +
    xlab(x_label) + ylab(df_metric_label) +
    labs(title=title, subtitle=subtitle)
  if (facet){
    df_r2s <- data.frame(position=facet_order, r2=r2)
    x_ <- ifelse(x=="draft_number", 200,
                 ifelse(x=="ovr_rank", 200, 60))
    g <- g + facet_wrap(~factor(position, facet_order), nrow=4, ncol=3) +
      geom_label(data=df_r2s, aes(x=x_, y=80, label=paste("R" %p% supsc("2"), " = ", round(r2, 3), sep="")))
  }
  else{
    if (add_points){
      x_ <- ifelse(x=="draft_number", 200,
                   ifelse(x=="ovr_rank", 200, 
                          ifelse(x=="rating", 0.8, 60)))
      y_ <- 90
      g <- g + geom_point() + annotate("label", x=x_, y=y_, label=paste("R" %p% supsc("2"), " = ", round(r2, 3), sep=""),
                                       size=7)
    }
  }
  g <- g + theme(plot.title=element_text(size=20), plot.subtitle = element_text(size=15),
                 axis.title=element_text(size=15),
                 axis.text=element_text(size=12))
  if (save){
    ggsave(path, plot=g, width=12, height=7)
  }
  return (g)
}

# Helper function to plot a faceted spaghetti plot of draft curves
plot_faceted_spaghetti <- function(draft_picks_for_facet, fct_order, fct_row, fct_col,
                                   model_params, grp_label, corr_data, add_label=""){
  ggplot(data=draft_picks_for_facet, aes(x=draft_number, y=mean_overall, group=grp,
                                      color=is_fct)) +
    stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x) + theta),
                method.args=list(start=list(alpha=model_params[1], beta=model_params[2],
                                            theta=model_params[3]),
                                 control=nls.control(maxiter=200)), se=FALSE) +
    xlab("Draft Pick") + ylab("Average Madden Rating") +
    labs(title=paste("Draft Curves for Average Madden Rating in Years 3-5 vs. Draft Pick by",
                     ifelse(add_label=="", grp_label, paste(add_label, grp_label))),
         subtitle="All Offensive/Defensive Players Drafted 2012-21") +
    gghighlight(is_fct) +
    facet_wrap(~factor(fct, fct_order), nrow=fct_row, ncol=fct_col) +
    scale_color_discrete(type=c("darkblue")) +
    geom_label(data=corr_data,
               aes(x=200, y=80, label=paste("R" %p% supsc("2"), " = ", round(r2, 3), sep="")),
               color="black") + 
    theme(legend.position="None") + theme(plot.title=element_text(size=20), plot.subtitle = element_text(size=15),
                                          axis.title=element_text(size=15),
                                          strip.text.x=element_text(size=14), axis.text=element_text(size=12))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", grp_label, "/Spaghetti ", 
               ifelse(add_label=="", grp_label, paste(add_label, grp_label)), ".pdf", sep=""),
         width=12, height=7)
}

# Helper function to plot and return "best available" information
plot_best_available_plots <- function(draft_picks_for_fct, fct_row, fct_col,
                                             fct_label){
  
  fct_info <- draft_picks_for_fct %>%
    group_by(fct) %>%
    summarize(best_available_prop=100*sum(madden_pos_rank_available==1)/n(),
              top_3_available_prop=100*sum(madden_pos_rank_available<=3)/n(),
              median_rank=median(madden_pos_rank_available),
              median_diff=median(abs(diff_pos_best_available)),
              n_first_rounders=sum(draft_round == 1),
              n_first_rounders_best=sum(draft_round ==1 & madden_pos_rank_available==1),
              first_rounders_best_prop=n_first_rounders_best/n_first_rounders,
              n_first_rounders_top_3=sum(draft_round == 1 & madden_pos_rank_available<=3),
              first_rounders_top_3_prop=n_first_rounders_top_3/n_first_rounders) %>%
    arrange(desc(best_available_prop))
  
  if (fct_label == "Draft Round"){
    fct_info <- fct_info %>%
      arrange(fct)
  }
  
  folder <- ifelse(grepl("class", fct_label), "Classman", fct_label)
  title_label <- ifelse(grepl("Position", fct_label), "",
                        ifelse(grepl("class", fct_label), fct_label, paste(" by", fct_label)))

  
  ggplot(data=draft_picks_for_fct, aes(x=madden_pos_rank_available)) +
    geom_histogram(aes(y=after_stat(density)), binwidth=1, fill="darkblue") + xlab("Madden Rank") + 
    ylab("Density") +
    labs(title=paste("Madden Ranking of Drafted Player out of Available Players at Position", title_label, sep=""),
         subtitle="All Offensive/Defensive Players Drafted 2012-21") +
    facet_wrap(~factor(fct, fct_info$fct), nrow=fct_row, ncol=fct_col) +
    geom_text(data=fct_info,
               aes(x=20, y=max(best_available_prop/100) - 0.075, label=paste("% Best Available = ", round(best_available_prop, 1), "%", sep="")),
               color="black", size=ifelse(fct_col * fct_row >= 30, 3, 5)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=15), 
          plot.title=element_text(size=20), plot.subtitle=element_text(size=15),
          strip.text = element_text(size=14))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", folder, "/Rank Available Faceted ", fct_label, ".pdf", sep=""),
         width=12, height=7)
  
  if (fct_label != "Draft Round"){
    fct_info <- fct_info %>%
      arrange(median_diff)
  }
  
  ggplot(data=draft_picks_for_fct, aes(x=abs(diff_pos_best_available))) +
    geom_histogram(aes(y=after_stat(density)), fill="darkblue", binwidth=1) + xlab("Difference in Madden Rating") + 
    ylab("Density") +
    labs(title=paste("Difference in Madden Rating of Drafted Player to Best Available Player at Position", title_label, sep=""),
         subtitle="All Offensive/Defensive Players Drafted 2012-21") +
    facet_wrap(~factor(fct, fct_info$fct), nrow=fct_row, ncol=fct_col) +
    geom_text(data=fct_info,
               aes(x=20, y=max(best_available_prop) - 0.075, label=paste("Median =", round(median_diff, 2))),
               color="black", size=ifelse(fct_col * fct_row >= 30, 3, 4))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", folder, "/Diff Best Available Faceted ", fct_label, ".pdf", sep=""),
         width=12, height=7)
  
  ggplot(data=fct_info, aes(x=reorder(fct, -n_first_rounders))) +
    geom_bar(aes(y=n_first_rounders, color="Not Best Available Pick"), stat="identity", fill="darkblue", width=1) +
    geom_col(aes(y=n_first_rounders_best, color="Best Available Pick"), position="identity", fill="lightblue",
             width=1) +
    xlab(ifelse(grepl("class", fct_label), "Position", fct_label)) + ylab("Number of Picks") + 
    labs(title=paste("Conversion of First-Round Draft Picks into Best Available Player at Position", title_label, sep=""),
         subtitle="All Offensive/Defensive Players Drafted 2012-21", color="Legend") +
    theme(legend.position="inside", legend.position.inside=c(0.8, 0.8),
          axis.text.x=element_text(angle=ifelse(fct_label=="GM", 90, 0))) 
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", folder, "/First Round Conversion ", fct_label, ".pdf", sep=""),
         width=12, height=7)
  
  ggplot(data=fct_info, aes(x=reorder(fct, -n_first_rounders))) +
    geom_bar(aes(y=n_first_rounders, color="Not Top 3 Available Pick"), stat="identity", fill="darkblue", width=1) +
    geom_col(aes(y=n_first_rounders_top_3, color="Top 3 Available Pick"), position="identity", fill="lightblue",
             width=1) +
    xlab(ifelse(grepl("class", fct_label), "Position", fct_label)) + ylab("Number of Picks") + 
    labs(title=paste("Conversion of First-Round Draft Picks into Top 3 Available Player at Position", title_label, sep=""),
         subtitle="All Offensive/Defensive Players Drafted 2012-21", color="Legend") +
    theme(legend.position="inside", legend.position.inside=c(0.8, 0.8),
          axis.text.x=element_text(angle=ifelse(fct_label=="GM", 90, 0))) 
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", folder, "/First Round Conversion Top 3 ", fct_label, ".pdf", sep=""),
         width=12, height=7)
  
  ggplot(data=fct_info, aes(x=reorder(fct, -top_3_available_prop))) +
    geom_bar(aes(y=top_3_available_prop), stat="identity", fill="darkblue") +
    xlab(ifelse(grepl("class", fct_label), "Position", fct_label)) + ylab("%") +
    labs(title=paste("How Often a Top 3 Available Player is Selected at the Position", title_label, sep=""),
         subtitle="All Offensive/Defensive Players Drafted 2012-21") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=15),
          plot.title=element_text(size=20), plot.subtitle = element_text(size=15))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/", folder, "/Top 3 Available ", fct_label, ".pdf", sep=""),
         width=12, height=7)
  
  return (fct_info)
}