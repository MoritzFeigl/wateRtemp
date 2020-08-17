#setwd("D:/Dropbox/WT_Project/Modelling/data_final")

wt_air2stream <- function(catchent){
  for(catchment in c("Ybbs", "Inn", "Enns", #"Donau",
                 "Erlauf", "Kleine_Muehl", "Saalach", "Salzach", "Traisen")){
  train <- feather::read_feather(paste0(catchment, "/train_data.feather"))
  test <- feather::read_feather(paste0(catchment, "/test_data.feather"))
  air2sream_variables <- c("year", "mon", "day", "Tmean", "wt", "Q")
  train <- train[, air2sream_variables]
  test <- test[, air2sream_variables]

  # if test does not start at 1st Jan than take that part from train data
  if(test$mon[1] != 1 | test$day[1] != 1){
    year_part <- train[train$year == max(train$year), ]
    train <- train[train$year != max(train$year), ]
    test <- rbind(year_part, test)
  }

  if(!dir.exists(paste0(catchment, "/air2stream"))){
    dir.create(paste0(catchment, "/air2stream"))
  }
  if(!dir.exists(paste0(catchment, "/air2stream/", catchment))){
    dir.create(paste0(catchment, "/air2stream/", catchment))
  }

  write.table(train, file = paste0(catchment, "/air2stream/", catchment, "/air_water_cc.txt"),
              row.names = FALSE, col.names = FALSE, sep = "	      ")

  write.table(test, file = paste0(catchment, "/air2stream/", catchment, "/air_water_cv.txt"),
              row.names = FALSE, col.names = FALSE, sep = "	      ")

  head(train)
  #parameters
  num_particles <- 500
  c12 <- c(2, 2) #c1, c2: constants known as cognitive and social learning factors
  minmax <- c(0.9, 0.4) # inertia max and min
  pso_paras <- c("! PSO parameters",
                 as.character(num_particles),
                 paste0(c12, collapse = " "),
                 paste0(minmax, collapse = " "))
  writeLines(pso_paras,
             paste0(catchment, "/air2stream/PSO.txt"))

  # input txt
  writeLines(c(paste0(catchment, " \t"), "air \t",
               "water \t", "c \t", "1d \t", "8 \t", "0 \t", "RMS \t",
               "CRN \t", "PSO \t", "0.60 \t", "500 \t", "-999 \t"),
             paste0(catchment, "/air2stream/input.txt"))


  writeLines(c("! Main input",
               paste0(catchment, "		! name of the river/basin/region"),
               "air      		! name/ID of the air temperature station",
               "water			! name/ID of the water temperature station",
               "c				! type of series: c=continuous series, m=mean year",
               "1d        		! time resolution: 1d=daily, nw=n weeks (n=1,2,...), 1m=monthly",
               "8           	! version: 3,4,5,7,8 parameters",
               "0				! Threshold temperature for ice formation",
               "RMS				! objective function: RMS (Root Mean Square Error), NSE (Nash-Sutcliffe Efficiency Index), KGE (Kling-Gupta Efficiency Index)",
               "CRN          	! mod_num : numerical method used to solve the model equation EUL (Euler Explicit), RK2 (Runge-Kutta 2), RK4 (Runge-Kutta 4), CRN (Crank Nicolson). CRN is the suggested choice.",
               "PSO            	! optimization algorithm: PSO (Particle Swarm Optimization) or RANSAM (Random Sampling)",
               "0.60			! minimum percentage of data: 0...1. E.g., when using 1m time resolution, the monthly average is set to NaN when available data during one month are less than this percentage (60% in this case)",
               "500				! n_run: number of iterations",
               "-999			! minimum efficiency index (i.e., RMS, NSE, KGE). All model realization with efficiency index greater than this threshold are saved in file 0_..."),
             paste0(catchment, "/air2stream/input.txt")
  )






  # parameters
  parameters <- c("-5    -5    -5   -1  0   0    0   -1	 !first",
                  "15   1.5  5   1   20  10   1    5	!second")
  writeLines(parameters,
             paste0(catchment, "/air2stream/", catchment, "/parameters.txt"))
  # file.copy("D:/Dropbox/WT_Project/Modelling/air2stream/air2stream-master/Switzerland/parameters.txt",
  #           paste0(catchment, "/air2stream/", catchment, "/parameters.txt"))



  if(!dir.exists(paste0(catchment, "/air2stream/", catchment, "/output_8"))){
    dir.create(paste0(catchment, "/air2stream/", catchment, "/output_8"))
  }


  # command <- paste0(catchment, "/air2stream/air2stream_1.0.0.exe")
  # system(command)

  oldwd <- getwd()
  setwd(paste0(catchment, "/air2stream"))
  file.copy("D:/Dropbox/WT_Project/Modelling/air2stream/air2stream-master/air2stream_1.0.0.exe",
            "air2stream_1.0.0.exe")
  system("air2stream_1.0.0.exe")
  setwd(oldwd)

  # test results
  test_results <- read.table(paste0(catchment, "/air2stream/", catchment,
                                    "/output_8/3_PSO_RMS_air_water_cv_1d.out"))
  test_results <- test_results[, 1:6]
  colnames(test_results) <- c("year", "mon", "day", "Tmean", "wt", "wt_predicted")
  test_results <- test_results[test_results$year != -999, ]

  predicted_values <- data.frame(predicted_values = test_results$wt_predicted)
  row.names(predicted_values) <- as.POSIXct(paste0(test_results$year, "-",
                                                   test_results$mon, "-",
                                                   test_results$day))
  feather::write_feather(predicted_values, paste0(catchment, "/air2stream/predicted_values.feather"))


  write.csv(data.frame(user_name = "Mo_small",
                       model = "air2stream",
                       RMSE_test = round(RMSE(test_results$wt_predicted, test_results$wt), 3),
                       NSE_test = NSE(test_results$wt_predicted, test_results$wt)),
            paste0(catchment, "/air2stream/model_scores.csv"), row.names = FALSE)

}
}
