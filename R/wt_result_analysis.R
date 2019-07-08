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
wt_result_analysis <- function(catchment, model, folder_name, epochs, bs, plot){

  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
  }
  input_init <- substr(as.character(folder_name), 1, 3)
  if(input_init == "all") data_inputs <- "all"
  if(input_init == "pre") data_inputs <- "precip"
  if(input_init == "rad") data_inputs <- "radiation"
  if(input_init == "sim") data_inputs <- "simple"
  if(input_init == "lm_") data_inputs <- "simple"
  rad_data <- length(list.files(path = as.character(catchment), pattern = "radiation_")) > 0
  # in case of radiation or all data_input, load radiation data
  if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
    data_prefix <- "radiation_"
  } else {
    data_prefix <- ""
  }

  train <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
  test <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))
  if(model %in% c("LM", "RF", "XGBoost")){
    if(sum(is.na(test)) > 0){
      test <- test[which(!is.na(test$Qdiff)), ]
      test <- test[which(!is.na(test$Tmean_diff)), ]
    }
  }
  # RF & XGBoost
  if( model %in% c("RF", "XGBoost", "LM")){
  model_folder <- paste0(catchment, "/", model, "/", folder_name, "/")
  }

  if( model == "LSTM"){
  model_folder <- paste0(catchment, "/", model, "/", folder_name, "/",
                         paste0(epochs, "epochs_", bs, "batchsize/"))
  }
  if( model == "ANN"){
    model_folder <- paste0(catchment, "/", model, "/", folder_name, "/",
                           paste0(epochs, "epochs_", bs, "batchsize_100ensembleRuns/"))
  }
  if(model == "LSTM"){
    tss <- unlist(strsplit(as.character(folder_name), "_"))[3]
    ts <- as.integer(substr(tss, 1, nchar(tss)-2))
    test <- test[(ts + 1):nrow(test),]
  }
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
  #na_rows <- which(is.na(test), arr.ind = TRUE)[,1]
  #test <- test[-na_rows, ]
  prediction <- feather::read_feather(paste0(model_folder, "predicted_values.feather"))
  names(prediction) <- "predictions"

  # # Fix LSTM problem ######################
  # data <- test[, c("date", "wt")]
  # full_ts <- data.frame(
  #   date = as.character(format.POSIXct(
  #     seq(data$date[1], data$date[nrow(data)], by = "day"),
  #     format = "%Y-%m-%d"), stringsAsFactors = FALSE))
  # data$date <- as.character(data$date)
  # # merge with data
  # full_train <- merge(full_ts, data, by = "date", all = TRUE)
  # # split data in sub time series without gaps
  # missing_days <- unique(which(is.na(full_train), arr.ind = TRUE)[, 1])
  # if(length(missing_days) != 0){
  #
  #   # if only one split is necessary
  #   if(length(missing_days) == 1){
  #     cut_points <- missing_days
  #     data_list <- vector(mode = "list")
  #     data_list[[1]] <- data[1:(cut_points[1] - 1), ]
  #   } else {
  #
  #     # remove consecutive missing days
  #     rm_ind <- integer()
  #     if(length(missing_days) == 2){
  #       if(missing_days[2] - missing_days[1] <= 20) rm_ind <- c(rm_ind, 2)
  #     }
  #
  #
  #     if(length(missing_days) > 2){
  #       for(i in 2:(length(missing_days)-1)){
  #         if(missing_days[i] - missing_days[i-1] <= 20 &
  #            missing_days[i+1] - missing_days[i] <= 20) rm_ind <- c(rm_ind, i)
  #       }
  #     }
  #
  #     # for all cut points create list entry with data until that point
  #     cut_points <- missing_days[-rm_ind]
  #     data_list <- vector(mode = "list")
  #     for(i in seq_along(cut_points)){
  #       if(i == 1){
  #         data_list[[i]] <- data[1:(cut_points[i] - 1), ]
  #       } else {
  #         data_list[[i]] <- data[(cut_points[i-1]+1):(cut_points[i]-1), ]
  #       }
  #     }
  #     # for the last cut point also add the time from the last cutpoint to the end of the data
  #     data_list[[length(data_list) + 1]] <- data[(cut_points[length(cut_points)] + 1):nrow(data), ]
  #   }
  # } # produces all complete snips of data
  # data_list <- data_list[!(unlist(lapply(data_list, nrow)) < ts + 1)]
  # data_list <- do.call(rbind, data_list)
  # #saveRDS(y_test_arr, "test_dates_best_ybbs_LSTM.rds")
  # y_dates <- readRDS("test_dates_best_ybbs_LSTM.rds")
  # #saveRDS(y_test_arr, "test_wt_best_ybbs_LSTM.rds")
  # y_values <- readRDS("test_wt_best_ybbs_LSTM.rds")
  # y_test <- data.frame(date = y_dates, obs = y_values, stringsAsFactors = FALSE)
  # head(y_test[-c(1:90), ])
  # prediction

#################
  if(plot == "dygraph"){
    pred_xts <- xts::xts(data.frame(test, prediction),
                         order.by = as.POSIXct(paste0(test$year, "-", test$mon, "-", test$day)))
    print(dygraphs::dygraph(pred_xts[, c("wt", "predictions", "Tmean")]) %>%  dygraphs::dyRangeSelector())
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
      xlab("observed water temp in °C") +
      ylab("predicted water temp in °C") +
      geom_smooth(method = 'lm', formula =y~x) +
      annotate("text", x = min(error_df$wt) + 1, y = max(error_df$predictions) , label = paste0("  RMSE = ", RMSE, "\nNSE = ", NSE))
    ggsave(paste0(model_folder, "correlation.png"))


    # 2. Plot Zeitreihe
    pred_xts <- xts::xts(data.frame(test, prediction),
                         order.by = as.POSIXct(paste0(test$year, "-", test$mon, "-", test$day)))

    #years <- as.character(unique(error_df$year)[5])
    years <- "2015"
    ts_plot <- broom::tidy(pred_xts[, c("wt", "predictions")][years])
    ts_plot$value <- as.numeric(ts_plot$value)
    ts_plot$series[ts_plot$series == "wt"] <- "observed"
    ts_plot$series[ts_plot$series == "predictions"] <- model
    names(ts_plot)[2] <- "legend"
    p1 <- ggplot(ts_plot, aes(x = index, y = value, color = legend)) + geom_line() +
      xlab("") + ylab("daily mean water temp. °C") +
      scale_color_manual("", values = c("darkorchid2", "mediumseagreen"), labels = c(paste0(model, "  "),
                                    "observed  ")) +
      ggtitle(catchment) +
      theme( legend.box.spacing = unit(0, 'cm'),
             axis.title=element_text(size=9),
             axis.text = element_text(size = 9),
             plot.title = element_text(hjust = 0.5))



    # 3. Plot Wt + andere infos
    ts_plot2 <- as.data.frame(pred_xts[, c("RR", "Q")][years], stringsAsFactors = FALSE)
    ts_plot2$time <- as.POSIXct(row.names(ts_plot2))
    ts_plot2$Q <- as.numeric(ts_plot2$Q)

    if(mean(ts_plot2$Q) > 1000){
      ts_plot2$RR <- as.numeric(ts_plot2$RR)*200
      p2 <- ggplot(ts_plot2, aes(x = time)) +
        geom_col(aes( y = RR, fill = 'redfill')) +
        geom_line(aes(y = Q, group = 1, color = 'blackline')) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . /200, name = "daily precipitation mm")) +
        scale_fill_manual('', labels = 'P', values = "dodgerblue2") +
        scale_color_manual('', labels = 'Q', values = 'black') +
        xlab("") + ylab("daily mean runoff m³/s") +
        theme(legend.margin = margin(-0.95,0,0,0, unit="cm"),
              axis.title=element_text(size=9),
              axis.text = element_text(size = 9))
    } else if(mean(ts_plot2$Q) > 100) {
      ts_plot2$RR <- as.numeric(ts_plot2$RR)*5
      p2 <- ggplot(ts_plot2, aes(x = time)) +
        geom_col(aes( y = RR, fill = 'redfill')) +
        geom_line(aes(y = Q, group = 1, color = 'blackline')) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . /5, name = "daily precipitation mm")) +
        scale_fill_manual('', labels = 'P', values = "dodgerblue2") +
        scale_color_manual('', labels = 'Q', values = 'black') +
        xlab("") + ylab("daily mean runoff m³/s") +
        theme(legend.margin = margin(-0.95,0,0,0, unit="cm"))+
        theme( legend.box.spacing = unit(0.5, 'cm'),
               axis.title=element_text(size=9),
               axis.text = element_text(size = 9))

    } else {
      ts_plot2$RR <- as.numeric(ts_plot2$RR)*2
      p2 <- ggplot(ts_plot2, aes(x = time)) +
        geom_col(aes( y = RR, fill = 'redfill')) +
        geom_line(aes(y = Q, group = 1, color = 'blackline')) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . /2, name = "daily precipitation mm")) +
        scale_fill_manual('', labels = 'P', values = "dodgerblue2") +
        scale_color_manual('', labels = 'Q', values = 'black') +
        xlab("") + ylab("daily mean runoff m³/s") +
        theme(legend.margin = margin(-0.95,0,0,0, unit="cm"),
              axis.title=element_text(size=9),
              axis.text = element_text(size = 9))
    }


    # figure <- ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2)
    # ggpubr::annotate_figure(figure,
    #                         top = ggpubr::text_grob(catchment, color = "black", face = "bold", size = 14, hjust = 1.9)
    #                         #bottom = ggpubr::text_grob("Data source: \n mtcars data set", color = "blue",
    #                         #                           hjust = 1, x = 1, face = "italic", size = 10),
    #                         #left = ggpubr::text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
    #                         #right = "I'm done, thanks :-)!",
    #                         #fig.lab = "Figure 1", fig.lab.face = "bold"
    # )
    # ggsave(paste0(model_folder, "ts_2015.png"))






    years <- "2015"
    ts_plot3 <- broom::tidy(pred_xts[, c("GL")][years])
    ts_plot3$value <- as.numeric(ts_plot3$value)

    #ts_plot$series[ts_plot$series == "wt"] <- "observed"
    #ts_plot$series[ts_plot$series == "predictions"] <- model
    #names(ts_plot)[2] <- "legend"
     ts_plot3 <- as.data.frame(pred_xts[, c("GL", "Tmean")][years], stringsAsFactors = FALSE)
     ts_plot3$time <- as.POSIXct(row.names(ts_plot3))
     ts_plot3$GL <- as.numeric(ts_plot3$GL)
     ts_plot3$Tmean <- as.numeric(ts_plot3$Tmean) * 30


     temp_scale <- seq(floor(min(ts_plot3$Tmean/30)/5)*5, to = ceiling(max(ts_plot3$Tmean/30)/5)*5, by = 5)

       p3 <- ggplot(ts_plot3, aes(x = time)) +
       geom_line(aes(y = GL, color = "chocolate1")) +
       geom_line(aes(y = Tmean, color = "brown1")) +
       scale_y_continuous(sec.axis = sec_axis(trans = ~ . /30 ,
                                              breaks = temp_scale,
                                              name = "daily mean air temperature °C")) +
       scale_color_manual("", labels = c("T", "GL"),
                          values = c("brown1", "olivedrab3")) +
       xlab("") + ylab("daily mean global radiation W/m²") +
       theme(legend.margin = margin(-0.95,0,0,0, unit="cm"),
             axis.title=element_text(size=9),
             axis.text = element_text(size = 9))



     figure2 <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
     #ggpubr::annotate_figure(figure2,
                             #top = ggpubr::text_grob(catchment, color = "black", face = "bold", size = 14, hjust = 1.9)
                             #bottom = ggpubr::text_grob("Data source: \n mtcars data set", color = "blue",
                             #                           hjust = 1, x = 1, face = "italic", size = 10),
                             #left = ggpubr::text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                             #right = "I'm done, thanks :-)!",
                             #fig.lab = "Figure 1", fig.lab.face = "bold"
     #)
     ggsave(paste0(model_folder, "ts_radiation_2015.png"), height = 20, width = 25, units = "cm")



    # AIR TEMPERATUR FOR PLOTTING --------------- -----------------------------------------
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
    error_df$Q_bins <- cut(error_df$Q, seq(0, max_q, 100), dig.lab=10)

    # Q bins 10
    # get max 100 Q
    min_q <- floor(min(error_df$Q)/10)*10
    error_df$Q_bins10_low <- cut(error_df$Q, seq(min_q, max_q, 10), dig.lab=10)


    # # RR for PLotting --------------------------------------------------------------------
    # # RR max value for cut
    #   RR_max_bin <- ceiling(max(error_df$RR) * 5)/5
    #   RR_min_bin <-  floor(min(error_df$RR) * 5)/5
    # error_df$RR_bins <- cut(error_df$RR, breaks = seq(RR_min_bin, RR_max_bin, 5))
    #
    # # RR bins 5 mm range
    #   agg_wt5_RR <- aggregate(wt ~ RR_bins, error_df, function(x) mean(x, na.rm = TRUE))
    #   names(agg_wt5_RR)[2] <- "mean_wt5_RR"
    #   error_df <- merge(error_df, agg_wt5_RR, by = "RR_bins", all.x = TRUE)
    #


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
    ggsave(paste0(model_folder, "Tmean_vs_error.png"))
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
    ggsave(paste0(model_folder, "wt_vs_error.png"))

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
    ggsave(paste0(model_folder, "Q100_vs_error.png"))
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
    ggsave(paste0(model_folder, "Q10_vs_error.png"))


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

    # RR influence on prediction

    ggplot(error_df, aes(RR_bins, error, fill = mean_wt5_RR)) +
      geom_hline(yintercept = 0) + geom_boxplot() + coord_flip() +
      scale_fill_gradient2(low = "#0099FF", high = "#FF3300",
                           midpoint = mean(error_df$wt),
                           na.value = "black", limits = c(0, NA)) +
      xlab("RR") + ylab("prediction error in °C (prediction - observation)") +
      labs(fill = "daily mean \nwater temp. °C")
    ggsave(paste0(model_folder, "wt_vs_error.png"))

  }

}
