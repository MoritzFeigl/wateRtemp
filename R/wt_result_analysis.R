#' wt_result_analysis
#'
#' Plots diagnostic plots for analysing the differen model results.
#' @param catchment
#' @param model
#' @param cv_mode
#' @param plot
#'
#' @return None
#' @export
#'
#' @examples
wt_result_analysis <- function(catchment, data_inputs, model, cv_mode, plot){

  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
  }
  rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0
  # in case of radiation or all data_input, load radiation data
  if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
    data_prefix <- "radiation_"
  } else {
    data_prefix <- ""
  }

  train <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
  test <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))

  model_folder <- paste0(catchment, "/", model, "/", data_inputs, "Model_", cv_mode, "/")
  library(dygraphs, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  # library(xts, quietly = TRUE)
  # library(tidyverse, quietly = TRUE)
  # library(caret, quietly = TRUE)
  # library(doParallel, quietly = TRUE)
#
#   sub_train <- train[, -c(1, 3, 4, 11, 14:56)] # fuzzy
#   sub_val <- val[, -c(1, 3, 4, 11, 14:56)] # fuzzy
#   # remove NA rows resulting from Qdiff, Tmean_diff
#   na_train <- which(is.na(sub_train), arr.ind = TRUE)
#   sub_train <- sub_train[-na_train[,1],]
  na_rows <- which(is.na(test), arr.ind = TRUE)[,1]
  test <- test[-na_rows, ]
  prediction <- feather::read_feather(paste0(model_folder, "predicted_values.feather"))
  names(prediction) <- "predictions"





  if(plot == "dygraph"){
  pred_xts <- xts::xts(data.frame(test, prediction, Q2 = test$Q/50, RR2 = test$RR/50),
                         order.by = as.POSIXct(paste0(test$year, "-", test$mon, "-", test$day)))
  print(dygraphs::dygraph(pred_xts[, c("wt", "predictions", "Q2", "RR2", "Tmean")]) %>%  dygraphs::dyRangeSelector())
  }

  if(plot == "errors"){
    error_df <- dplyr::bind_cols(test, "predictions" = prediction)
    # define prediction error
    error_df$error <- error_df$predictions - error_df$wt

    # NSE/RMSE
    RMSE <- round(sqrt(mean(error_df$error^2, na.rm = TRUE)), 3)
    NSE <- round(1 - (sum((error_df$predictions- error_df$wt)^2, na.rm = TRUE) / sum( (error_df$wt - mean(error_df$wt, na.rm = TRUE))^2, na.rm = TRUE ) ), 3)


    # 1. Plot predictions vs. observations
    ggplot(error_df, aes(x = wt, y = predictions)) + geom_point(col = "black", shape = 1) +
      xlab("Observed water temp in °C") +
      ylab("Predicted water temp in °C") +
      geom_smooth(method = 'lm', formula =y~x) +
      annotate("text", x = 2, y = max(error_df$predictions) , label = paste0("  RMSE = ", RMSE, "\nNSE = ", NSE))


    # 2. Plot Zeitreihe
    pred_xts <- xts::xts(data.frame(test, prediction),
                         order.by = as.POSIXct(paste0(test$year, "-", test$mon, "-", test$day)))

    years <- as.character(unique(error_df$year)[5])
    ts_plot <- broom::tidy(pred_xts[, c("wt", "predictions")][years])
    ts_plot$value <- as.numeric(ts_plot$value)
    p1 <- ggplot(ts_plot, aes(x = index, y = value, color = series)) + geom_line()

    # 3. Plot Wt + andere infos
    ts_plot2 <- broom::tidy(pred_xts[, c("RR", "Q")][years])
    ts_plot2$value <- as.numeric(ts_plot2$value)
    p2 <- ggplot(ts_plot2, aes(x = index, y = value, color = series)) + geom_line()

    figure <- ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2)

    ggpubr::annotate_figure(figure,
                    top = ggpubr::text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                    bottom = ggpubr::text_grob("Data source: \n mtcars data set", color = "blue",
                                       hjust = 1, x = 1, face = "italic", size = 10),
                    left = ggpubr::text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                    right = "I'm done, thanks :-)!",
                    fig.lab = "Figure 1", fig.lab.face = "bold"
    )
    # AIR TEMPERATUR FOR PLOTTING --------------------------------------------------------
    # Tmean max value for cut
    if(max(ceiling(error_df$Tmean)) %% 2 == 0){
      tmean_max_bin <- max(ceiling(error_df$Tmean))
    } else {
      tmean_max_bin <-  max(ceiling(error_df$Tmean)) + 1
    }
    # Tmean min value for cut
    if(min(floor(error_df$Tmean)) %% 2 == 0){
      tmean_min_bin <- min(floor(error_df$Tmean))
    } else {
      tmean_min_bin <-  min(floor(error_df$Tmean)) - 1
    }
    # Tmean bins 1 °C range
    error_df$tmean_bins <- cut(error_df$Tmean, breaks = seq(tmean_min_bin, tmean_max_bin, 1))
    # Tmean bins 2 °C range
    error_df$tmean_bins2 <- cut(error_df$Tmean, breaks = seq(tmean_min_bin, tmean_max_bin, 2))


    # Tmean bins 5 °C range
    if(max(ceiling(error_df$Tmean)) %% 5 == 0){
      tmean_max_bin5 <- max(ceiling(error_df$Tmean))
    } else {
      tmean_max_bin5 <-  ceiling(max(ceiling(error_df$Tmean))/5) * 5
    }
    # Tmean min value for cut
    if(min(floor(error_df$Tmean)) %% 5 == 0){
      tmean_min_bin5 <- min(floor(error_df$Tmean))
    } else {
      tmean_min_bin5 <-  floor(min(floor(error_df$Tmean))/5) * 5
    }
    error_df$tmean_bins5 <- cut(error_df$Tmean, breaks = seq(tmean_min_bin5, tmean_max_bin5, 5))

    # get cut mean values
    agg_tmean1 <- aggregate(Tmean ~ tmean_bins, error_df, mean)
    names(agg_tmean1)[2] <- "mean_Tmean"
    error_df <- merge(error_df, agg_tmean1, by = "tmean_bins", all.x = TRUE)

    agg_tmean2 <- aggregate(Tmean ~ tmean_bins2, error_df, mean)
    names(agg_tmean2)[2] <- "mean_Tmean2"
    error_df <- merge(error_df, agg_tmean2, by = "tmean_bins2", all.x = TRUE)

    agg_tmean5 <- aggregate(Tmean ~ tmean_bins5, error_df, mean)
    names(agg_tmean5)[2] <- "mean_Tmean5"
    error_df <- merge(error_df, agg_tmean5, by = "tmean_bins5", all.x = TRUE)

    # WATER TEMPERATUR FOR PLOTTING ------------------------------------------------------
    if(max(ceiling(error_df$Tmean)) %% 2 == 0){
      max_wt_bins <-max(ceiling(error_df$wt))
    } else {
      max_wt_bins <- max(ceiling(error_df$wt)) + 1
    }
    error_df$wt_bins <- cut(error_df$wt, seq(0, max_wt_bins, 1))
    error_df$wt_bins2 <- cut(error_df$wt, seq(0, max_wt_bins, 2))

    agg_wt1 <- aggregate(wt ~ wt_bins, error_df, mean)
    names(agg_wt1)[2] <- "mean_wt"
    error_df <- merge(error_df, agg_wt1, by = "wt_bins", all.x = TRUE)

    agg_wt2 <- aggregate(wt ~ wt_bins2, error_df, mean)
    names(agg_wt2)[2] <- "mean_wt2"
    error_df <- merge(error_df, agg_wt2, by = "wt_bins2", all.x = TRUE)

    # wt bins 5 °C range
    if(max(ceiling(error_df$wt)) %% 5 == 0){
      max_wt_bins5 <- max(ceiling(error_df$wt))
    } else {
      max_wt_bins5 <-  ceiling(max(ceiling(error_df$wt))/5) * 5
    }
    error_df$wt_bins5 <- cut(error_df$wt, breaks = seq(0, max_wt_bins5, 5))

    agg_wt5 <- aggregate(wt ~ wt_bins5, error_df, mean)
    names(agg_wt5)[2] <- "mean_wt5"
    error_df <- merge(error_df, agg_wt5, by = "wt_bins5", all.x = TRUE)

    # Q FOR PLOTTING ---------------------------------------------------------------------

    # Q bins 100
    # get max 100 Q
    max_q <- ceiling(max(error_df$Q)/100)*100
    error_df$Q_bins <- cut(error_df$Q, seq(0, max_q, 100))

    # Q bins 10
    # get max 100 Q
    error_df$Q_bins10_low <- cut(error_df$Q, seq(0, max_q, 10))

    # Air Temperature influence on prediction --------------------------------------------
    ggplot(error_df, aes(tmean_bins, error, fill = mean_Tmean)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black") +
      xlab("daily mean air temperature °C") + ylab("prediction error in water temp. °C (prediction - observation)") +
      labs(fill = "daily mean \nair temp. °C")

    ggplot(error_df, aes(factor(tmean_bins2), error, fill = mean_Tmean2)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black") +
      xlab("daily mean air temperature °C") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nair temp. °C")

    ggplot(error_df, aes(factor(tmean_bins5), error, fill = mean_Tmean5)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black") +
      xlab("daily mean air temperature °C") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nair temp. °C")

    # To Anzahl Datenpunkte zu Boxplots schreiben

    # water Temperature influence on prediction --------------------------------------------
    ggplot(error_df, aes(wt_bins, error, fill = mean_wt)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300",
                           midpoint = mean(error_df$wt),
                           na.value = "black", limits = c(0, NA)) +
      xlab("daily mean water temperature °C") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nwater temp. °C")

    ggplot(error_df, aes(wt_bins2, error, fill = mean_wt2)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300",
                           midpoint = mean(error_df$wt),
                           na.value = "black", limits = c(0, NA)) +
      xlab("daily mean water temperature °C") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nwater temp. °C")

    ggplot(error_df, aes(wt_bins5, error, fill = mean_wt5)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300",
                           midpoint = mean(error_df$wt),
                           na.value = "black", limits = c(0, NA)) +
      xlab("daily mean water temperature °C") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nwater temp. °C")


    # Runoff vs error vs water temp - only Q<100
    ggplot(error_df, aes(wt, error, col = Tmean)) + geom_point()  +
      scale_color_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black",
                            midpoint = mean(error_df$wt)) +
      labs(color = "daily mean \nair temp. °C") +
      ylab("absolut prediction error in °C") + xlab("daily mean \nwater temp. °C")


    # runoff influence on prediction --------------------------------------------

    # Runoff 100 bins vs error
    ggplot(error_df, aes(Q_bins, error)) +
      geom_boxplot() + coord_flip() +
      #scale_fill_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black") +
      xlab("daily mean runoff m³/s") + ylab("prediction error in °C (prediction - observation)") +
      geom_hline(yintercept = 0)

     # Runoff 10 bins vs error
    ggplot(error_df, aes(Q_bins10_low, error)) +
      geom_boxplot() + coord_flip() +
      #scale_fill_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black") +
      xlab("daily mean runoff m³/s") + ylab("prediction error in °C (prediction - observation)") +
      geom_hline(yintercept = 0)

    # Runoff 10 bins vs error - only low flows
    ggplot(error_df, aes(Q_bins10_low, error)) +
      geom_boxplot() + coord_flip() +
      xlab("daily mean runoff m³/s") + ylab("prediction error in °C (prediction - observation)") +
      geom_hline(yintercept = 0) + xlim(levels(error_df$Q_bins10_low)[1:20])


    # Runoff vs error vs water temp - only Q<100
    ggplot(error_df, aes(Q, error, col = wt)) +
      geom_hline(yintercept = 0) + geom_point() + xlim(0, 100) +
      scale_color_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black",
                            midpoint = mean(error_df$wt)) +
      labs(color = "daily mean \nwater temp. °C") + ylab("prediction error in °C (prediction - observation)") +
      xlab("daily mean runoff m³/s")


    # Runoff vs error vs air temp - only Q<100
    ggplot(error_df, aes(Q, error, col = Tmean)) +
      geom_hline(yintercept = 0) + geom_point() + xlim(0, 100) +
      scale_color_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black",
                            midpoint = mean(error_df$Tmean)) +
      labs(color = "daily mean \nair temp. °C") + ylab("prediction error in °C (prediction - observation)") +
      xlab("daily mean runoff m³/s")

    # Q, WT, Tmean -----------------------------
    # Runoff vs error vs water temp - only Q<100
    ggplot(error_df, aes(wt, Q, color = error)) + geom_point()  + ylim(0, 100) +
      scale_color_gradient2(low = "#0099FF", high = "#FF3300", na.value = "black",
                            midpoint = mean(error_df$error)) +
      labs(color = "prediction \nerror °C") + xlab("water temperature in °C") +
      ylab("daily mean runoff m³/s")



  }

}
