## -----------------------------------------------------------------------------
## Clear RStudio
rm(list = ls())  # Clear the global environment --> over there
graphics.off()  # Clear all plots
cat("\014")  # Clear the console - the window below this one
## -----------------------------------------------------------------------------

## Load all relevant packages
library(dplyr)
library(writexl)
library(ggplot2)
library(reshape2)
library(naniar)
library(imputeTS)
library(openxlsx)

##------------------------------------------------------------------------------
# Set the path to your folder - this is in order to find your files
setwd("E:/Research Assistant - SU/Wallenberg Pilot Psychotherapy Data/FaceReader - Overall")

folder_path <- ("E:/Research Assistant - SU/Wallenberg Pilot Psychotherapy Data/FaceReader - Overall")

# List all .txt files in the folder - will read all.txt files in the folder
txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Read the files into a list, skipping the first 12 rows containing identifying data
data_list <- lapply(txt_files, function(file) read.table(file, header = TRUE, skip = 12, sep = '\t'))

# Check the structure of the first file in the list (i.e. is it read in correctly)
str(data_list[[1]])

# Create a list to store the subset data frames - subsetting for 'Valence',
# Arousal' and 'Video.Time'
subset_data_list <- list()

# Loop through each data frame in data_list, i.e. all 22 files
for (i in seq_along(data_list)) {
  # Extract Valence and Arousal columns
  subset_data <- data_list[[i]][, c("Video.Time", "Valence", "Arousal")]
  
  # Add the subset data frame to the list
  subset_data_list[[i]] <- subset_data
}

# Check the structure of the first subset data frame to see if it has subset correctly
str(subset_data_list[[1]])

# Now to subset based upon the two dyads
# We make two data frames, one for each therapeutic dyad
# Extract the first 14 data frames, i.e. the 7 sessions for T1 & P1
D1 <- subset_data_list[1:14]

# Extract the last 8 data frames, i.e. the 4 sessions for T2 & P2
D2 <- subset_data_list[15:length(subset_data_list)]

# Create new data frames for each session by merging the subset data frames for 
# therapist and client

# Create a function to merge and convert Video.Time to POSIXct (a time format)
merge_and_convert <- function(df1, df2) {
  merged_df <- merge(df1, df2, by = "Video.Time", all = TRUE)
  merged_df$Video.Time <- as.POSIXct(merged_df$Video.Time, format = "%H:%M:%OS")
  return(merged_df)
}

# Merge and convert for D1 sessions
# We're merging the data frames for T1 with T2 here for each session
D1_S1 <- merge_and_convert(D1[[1]], D1[[8]])
D1_S2 <- merge_and_convert(D1[[2]], D1[[9]])
D1_S3 <- merge_and_convert(D1[[3]], D1[[10]])
D1_S4 <- merge_and_convert(D1[[4]], D1[[11]])
D1_S5 <- merge_and_convert(D1[[5]], D1[[12]])
D1_S6 <- merge_and_convert(D1[[6]], D1[[13]])
D1_S7 <- merge_and_convert(D1[[7]], D1[[14]])

# Merge and convert for D2 sessions, doing the same for T2 and P2
D2_S1 <- merge_and_convert(D2[[1]], D2[[5]])
D2_S2 <- merge_and_convert(D2[[2]], D2[[6]])
D2_S3 <- merge_and_convert(D2[[3]], D2[[7]])
D2_S4 <- merge_and_convert(D2[[4]], D2[[8]])

# Now create a list to store all of the session data frames
session_frames <- list(D1_S1, D1_S2, D1_S3, D1_S4, D1_S5, D1_S6, D1_S7,
                       D2_S1, D2_S2, D2_S3, D2_S4)

##------------------------------------------------------------------------------
## Now to trim the data frames by their actual start and end times.
# In order to do this we need to convert 'Video.Time' 
# Assuming 'session_frames' is a list containing all your data frames
for (i in seq_along(session_frames)) {
  session_frames[[i]]$Video.Time <- as.POSIXct(session_frames[[i]]$Video.Time, format = "%H:%M:%OS")
}

# Set the correct start and stop times for all sessions
start_time_D1_S1 <- as.POSIXct("00:00:00.000", format = "%H:%M:%OS")
stop_time_D1_S1 <- as.POSIXct("00:46:29.000", format = "%H:%M:%OS")

start_time_D1_S2 <- as.POSIXct("00:00:22.000", format = "%H:%M:%OS")
stop_time_D1_S2 <- as.POSIXct("00:41:50.000", format = "%H:%M:%OS")

start_time_D1_S3 <- as.POSIXct("00:00:53.000", format = "%H:%M:%OS")
stop_time_D1_S3 <- as.POSIXct("00:44:20.000", format = "%H:%M:%OS")

start_time_D1_S4 <- as.POSIXct("00:00:30.000", format = "%H:%M:%OS")
stop_time_D1_S4 <- as.POSIXct("00:41:38.000", format = "%H:%M:%OS")

start_time_D1_S5 <- as.POSIXct("00:00:30.000", format = "%H:%M:%OS")
stop_time_D1_S5 <- as.POSIXct("00:38:10.000", format = "%H:%M:%OS")

start_time_D1_S6 <- as.POSIXct("00:01:00.000", format = "%H:%M:%OS")
stop_time_D1_S6 <- as.POSIXct("00:30:19.000", format = "%H:%M:%OS")

start_time_D1_S7 <- as.POSIXct("00:00:05.000", format = "%H:%M:%OS")
stop_time_D1_S7 <- as.POSIXct("00:38:15.000", format = "%H:%M:%OS")

start_time_D2_S1 <- as.POSIXct("00:01:20.000", format = "%H:%M:%OS")
stop_time_D2_S1 <- as.POSIXct("00:48:45.000", format = "%H:%M:%OS")

start_time_D2_S2 <- as.POSIXct("00:00:40.000", format = "%H:%M:%OS")
stop_time_D2_S2 <- as.POSIXct("00:45:00.000", format = "%H:%M:%OS")

start_time_D2_S3 <- as.POSIXct("00:01:30.000", format = "%H:%M:%OS")
stop_time_D2_S3 <- as.POSIXct("00:53:05.000", format = "%H:%M:%OS")

start_time_D2_S4 <- as.POSIXct("00:00:50.000", format = "%H:%M:%OS")
stop_time_D2_S4 <- as.POSIXct("00:57:35.000", format = "%H:%M:%OS")

# Create a list to group all start times togetger
start_times <- c(start_time_D1_S1, start_time_D1_S2, start_time_D1_S3, start_time_D1_S4, 
                 start_time_D1_S5, start_time_D1_S6, start_time_D1_S7,start_time_D2_S1, 
                 start_time_D2_S2, start_time_D2_S3, start_time_D2_S4)

# Now to do the same for stop times
stop_times <- c(stop_time_D1_S1, stop_time_D1_S2, stop_time_D1_S3, stop_time_D1_S4, 
                stop_time_D1_S5, stop_time_D1_S6, stop_time_D1_S7, stop_time_D2_S1, 
                stop_time_D2_S2, stop_time_D2_S3, stop_time_D2_S4)

for (i in seq_along(session_frames)) {
  
  # Convert 'Video.Time' to POSIXct format
  session_frames[[i]]$Video.Time <- as.POSIXct(session_frames[[i]]$Video.Time, format = "%Y-%m-%d %H:%M:%OS")
  
  # Now to trim each data frame based on the corresponding start and stop times
  session_frames[[i]] <- session_frames[[i]] %>%
    filter(Video.Time >= start_times[i] & Video.Time <= stop_times[i])
}

## For some reason data frames are trimmed within the sessions_frames list but 
# the inidividual data frames retain their original 'Video.Time' structure. 
# Additional code required for the individual data frames 

# Trim D1_S1
D1_S1 <- D1_S1[D1_S1$Video.Time >= start_time_D1_S1 & D1_S1$Video.Time <= stop_time_D1_S1, ]

# Trim D1_S2
D1_S2 <- D1_S2[D1_S2$Video.Time >= start_time_D1_S2 & D1_S2$Video.Time <= stop_time_D1_S2, ]

# Trim D1_S3
D1_S3 <- D1_S3[D1_S3$Video.Time >= start_time_D1_S3 & D1_S3$Video.Time <= stop_time_D1_S3, ]

# Trim D1_S4
D1_S4 <- D1_S4[D1_S4$Video.Time >= start_time_D1_S4 & D1_S4$Video.Time <= stop_time_D1_S4, ]

# Trim D1_S5
D1_S5 <- D1_S5[D1_S5$Video.Time >= start_time_D1_S5 & D1_S5$Video.Time <= stop_time_D1_S5, ]

# Trim D1_S6
D1_S6 <- D1_S6[D1_S6$Video.Time >= start_time_D1_S6 & D1_S6$Video.Time <= stop_time_D1_S6, ]

# Trim D1_S7
D1_S7 <- D1_S7[D1_S7$Video.Time >= start_time_D1_S7 & D1_S7$Video.Time <= stop_time_D1_S7, ]

# Trim D2_S1
D2_S1 <- D2_S1[D2_S1$Video.Time >= start_time_D2_S1 & D2_S1$Video.Time <= stop_time_D2_S1, ]

# Trim D2_S2
D2_S2 <- D2_S2[D2_S2$Video.Time >= start_time_D2_S2 & D2_S2$Video.Time <= stop_time_D2_S2, ]

# Trim D2_S3
D2_S3 <- D2_S3[D2_S3$Video.Time >= start_time_D2_S3 & D2_S3$Video.Time <= stop_time_D2_S3, ]

# Trim D2_S4
D2_S4 <- D2_S4[D2_S4$Video.Time >= start_time_D2_S4 & D2_S4$Video.Time <= stop_time_D2_S4, ]


# Now to check the head (start) and tail (stop) of session four for dyad 2
head(D2_S4)
tail(D2_S4)

##------------------------------------------------------------------------------
## Now to make the columns for 'arousal' and 'valence' numeric for the ccf

# Convert all columns in chr format (except 'Video.Time') to numeric for 'D1' sessions
D1_S1[, -1] <- mutate_all(D1_S1[, -1], as.numeric)
D1_S2[, -1] <- mutate_all(D1_S2[, -1], as.numeric)
D1_S3[, -1] <- mutate_all(D1_S3[, -1], as.numeric)
D1_S4[, -1] <- mutate_all(D1_S4[, -1], as.numeric)
D1_S5[, -1] <- mutate_all(D1_S5[, -1], as.numeric)
D1_S6[, -1] <- mutate_all(D1_S6[, -1], as.numeric)
D1_S7[, -1] <- mutate_all(D1_S7[, -1], as.numeric)

# Do the same for all 'D2' sessions
D2_S1[, -1] <- mutate_all(D2_S1[, -1], as.numeric)
D2_S2[, -1] <- mutate_all(D2_S2[, -1], as.numeric)
D2_S3[, -1] <- mutate_all(D2_S3[, -1], as.numeric)
D2_S4[, -1] <- mutate_all(D2_S4[, -1], as.numeric)

# Now check the structure of a few data frames to see if the conversion was 
# successful. You should see 'num' instead of 'chr' for the variable columns
str(D1_S3)
str(D1_S7)
str(D2_S1)
str(D2_S3)

##------------------------------------------------------------------------------
## It appears that all data frames contain missing values.
# We need to calculate the number of missing values as a percentage for each column
# in each data frame

# For example, in order to obtain this for 'D1_S1', we do...
missing_percentage_D1_S1 <- colMeans(is.na(D1_S1)) * 100

# Print the results to see the percentage
print(missing_percentage_D1_S1)

# Now to create a loop so that percentages can be calculated for all data frames
# First, create an empty list to store missing percentage data frames
missing_percentage_list <- list()

# Loop through each session data frame
for (i in seq_along(session_frames)) {
  # Now calculate missing value percentages
  missing_percentages <- colMeans(is.na(session_frames[[i]])) * 100
  
  # Create a new data frame with missing percentages
  missing_percentage_df <- data.frame(
    Session = paste0("session_", i),
    Column = names(missing_percentages),
    MissingPercentage = missing_percentages
  )
  
  # Add to the pre-established list
  missing_percentage_list[[i]] <- missing_percentage_df
}

# Combine all data frames into one using r.bind
all_missing_percentages <- do.call(rbind, missing_percentage_list)

# Define the file name
file_name <- "all_sessions_missing_percentages_try.xlsx"

# And finally save the percentages to Excel
write_xlsx(all_missing_percentages, file_name)

##------------------------------------------------------------------------------

# From the above we can see that a number of data frames, especially from D1 sessions, 
# contain missing data. 
# We can calculate whether data is missing completely at random (MCAR) using 
# Little's test.

# Fr example we'll see if 'D1_S1' is missing values
# We create a matrix indicating these missing values (TRUE for missing, FALSE for observed)
missing_matrix <- is.na(D1_S1)

# The we plot this missing data matrix to have a visual illustration 
ggplot(data = melt(missing_matrix), aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("#FFFFFF", "#FF0000"), name = "Missing") +
  labs(title = "Missing Data Matrix-Dyad 1 (Session 1)", x = "Variables", y = "Time Points") +
  theme_minimal()

## Plotting has enabled me to gain a better sense of the 'missingness' but I still
# want to estimate 'missingness' using a Little's MCAR...

# Create a function to perform Little's MCAR test on a session data frame using 
# the 'naniar' package
perform_mcar_test <- function(session_df) {
  result <- mcar_test(session_df)
  return(result)
}

# Get a list of all session data frames
session_dfs <- list(D1_S1, D1_S2, D1_S3, D1_S4, D1_S5, D1_S6, D1_S7,
                    D2_S1, D2_S2, D2_S3, D2_S4)

# Perform MCAR test for each session data frame
results_list <- lapply(session_dfs, perform_mcar_test)

# Print the results
for (i in seq_along(results_list)) {
  cat("Session", i, ":", "\n")
  print(results_list[[i]])
  cat("\n")
}

## Little's MCAR suggests data not missing at random for each of the
# sessions. This is understandable given the nature of dropped frames 
# represented in time series (which generate clusters of lost data as seen in
# in the previous plot)

##------------------------------------------------------------------------------
## Imputation deemed to be a potentially beneficial way of combatting the issue 
# of missing values. Package 'ImputeTS' to be used as it specifically addresses
# missing data in time series

# Creat a function to impute missing values in a 'session' data frame
impute_session <- function(session_df, name) {
  imputed_df <- na_interpolation(session_df, option = "spline")
  assign(name, imputed_df, envir = .GlobalEnv)
  return(imputed_df)
}

# The list all of the session data frames, including their desired names ""
sessions_and_names <- list(
  list(session_df = D1_S1, name = "D1_S1_imputed"),
  list(session_df = D1_S2, name = "D1_S2_imputed"),
  list(session_df = D1_S3, name = "D1_S3_imputed"),
  list(session_df = D1_S4, name = "D1_S4_imputed"),
  list(session_df = D1_S5, name = "D1_S5_imputed"),
  list(session_df = D1_S6, name = "D1_S6_imputed"),
  list(session_df = D1_S7, name = "D1_S7_imputed"),
  list(session_df = D2_S1, name = "D2_S1_imputed"),
  list(session_df = D2_S2, name = "D2_S2_imputed"),
  list(session_df = D2_S3, name = "D2_S3_imputed"),
  list(session_df = D2_S4, name = "D2_S4_imputed")
)

# Apply the imputation function to the new list 'sessions_and_names'
imputed_session_dfs <- lapply(sessions_and_names, function(session) {
  impute_session(session_df = session$session_df, name = session$name)
})

# Assign the names to the imputed data frames
names(imputed_session_dfs) <- sapply(sessions_and_names, function(session) session$name)

## Now all available data frames has been imputed = no pesky NAs 
##------------------------------------------------------------------------------
## Now, finally, for the 'ccf' or cross-correlation function!

# Important to set the parameters for your windows and lag (in seconds)
lag_max <- 5
window_size <- 30

# Now to create the 'ccf' function, including the set parameters and plots
compute_acf_ccf_specific <- function(imputed_session_df) {
  ccf_valence <- ccf(imputed_session_df$Valence.x, imputed_session_df$Valence.y, lag.max = 5, plot = TRUE, na.action = na.pass)
  ccf_arousal <- ccf(imputed_session_df$Arousal.x, imputed_session_df$Arousal.y, lag.max = 5, plot = TRUE, na.action = na.pass)
}

# Apply the modified function to each session
results_list_specific <- lapply(imputed_session_dfs, compute_acf_ccf_specific)

resultsD1_S1 <- results_list_specific[[1]]
resultsD1_S2 <- results_list_specific[[2]]
resultsD1_S3 <- results_list_specific[[3]]
resultsD1_S4 <- results_list_specific[[4]]
resultsD1_S5 <- results_list_specific[[5]]
resultsD1_S6 <- results_list_specific[[6]]
resultsD1_S7 <- results_list_specific[[7]]
resultsD2_S1 <- results_list_specific[[8]]
resultsD2_S2 <- results_list_specific[[9]]
resultsD2_S3 <- results_list_specific[[10]]
resultsD2_S4 <- results_list_specific[[11]]


resultsD1_S1 
resultsD1_S2 
resultsD1_S3 
resultsD1_S4
resultsD1_S5
resultsD1_S6 
resultsD1_S7
resultsD2_S1
resultsD2_S2
resultsD2_S3
resultsD2_S4

# Now to save the results as an Excel file!
# Create a new Excel workbook (wb)
wb <- createWorkbook()

# Add each data frame as a new sheet in the workbook (wb)
addWorksheet(wb, sheetName = "resultsD1_S1")
writeData(wb, sheet = "resultsD1_S1", resultsD1_S1)

addWorksheet(wb, sheetName = "resultsD1_S2")
writeData(wb, sheet = "resultsD1_S2", resultsD1_S2)

addWorksheet(wb, sheetName = "resultsD1_S3")
writeData(wb, sheet = "resultsD1_S3", resultsD1_S3)

addWorksheet(wb, sheetName = "resultsD1_S4")
writeData(wb, sheet = "resultsD1_S4", resultsD1_S4)

addWorksheet(wb, sheetName = "resultsD1_S5")
writeData(wb, sheet = "resultsD1_S5", resultsD1_S5)

addWorksheet(wb, sheetName = "resultsD1_S6")
writeData(wb, sheet = "resultsD1_S6", resultsD1_S6)

addWorksheet(wb, sheetName = "resultsD1_S7")
writeData(wb, sheet = "resultsD1_S7", resultsD1_S7)

addWorksheet(wb, sheetName = "resultsD2_S1")
writeData(wb, sheet = "resultsD2_S1", resultsD2_S1)

addWorksheet(wb, sheetName = "resultsD2_S2")
writeData(wb, sheet = "resultsD2_S2", resultsD2_S2)

addWorksheet(wb, sheetName = "resultsD2_S3")
writeData(wb, sheet = "resultsD2_S3", resultsD2_S3)

addWorksheet(wb, sheetName = "resultsD2_S4")
writeData(wb, sheet = "resultsD2_S4", resultsD2_S4)

# Save the Excel workbook to a file
saveWorkbook(wb, file = "cross_correlation_results_10.xlsx")

##------------------------------------------------------------------------------
## Now it's time to examine the autocorrelation 
# Assuming session_dfs is a list of pre-imputed data frames
# Assuming imputed_session_dfs is a list of imputed data frames
# Set the lag.max value according to your preference
lag_max <- 15

# Function to compute and plot autocorrelation for each variable across lists
compute_and_plot_acf_across_lists <- function(session_df, imputed_session_df, variable_name) {
  par(mfrow=c(1,2)) 
  
  # Autocorrelation for pre-imputed data
  acf_result_pre_imputed <- acf(session_df[[variable_name]], lag.max = lag_max, plot = FALSE, na.action = na.pass)
  plot(acf_result_pre_imputed, main = paste("Autocorrelation for", variable_name, "(Pre-imputed)"))
  
  # Autocorrelation for imputed data
  acf_result_imputed <- acf(imputed_session_df[[variable_name]], lag.max = lag_max, plot = FALSE, na.action = na.pass)
  plot(acf_result_imputed, main = paste("Autocorrelation for", variable_name, "(Imputed)"))
  
  invisible(list(acf_result_pre_imputed = acf_result_pre_imputed, acf_result_imputed = acf_result_imputed))
}

# Create a list to store acf results for each variable
acf_results_list_across_lists <- list()

# Variable names for variavles of interest
variable_names_across_lists <- c("Valence.x", "Valence.y", "Arousal.x", "Arousal.y")

# Apply the function to each session and variable
for (i in seq_along(session_dfs)) {
  acf_results_session <- lapply(variable_names_across_lists, function(variable) {
    compute_and_plot_acf_across_lists(session_dfs[[i]], imputed_session_dfs[[i]], variable)
  })
  acf_results_list_across_lists[[length(acf_results_list_across_lists) + 1]] <- acf_results_session
}

## Okay, we're almost there! Now we want to say the results as an excel document

# Once again, we create a list to store data frames in
result_dfs <- lapply(acf_results_list_across_lists, function(acf_results) {
  if ("acf_result_pre_imputed" %in% names(acf_results[[1]]) && "acf_result_imputed" %in% names(acf_results[[1]])) {
    acf_data_pre_imputed <- acf_results[[1]]$acf_result_pre_imputed
    acf_data_imputed <- acf_results[[1]]$acf_result_imputed
    
    # Create a data frame
    result_df <- data.frame(acf_pre_imputed = acf_data_pre_imputed$acf, lag_pre_imputed = acf_data_pre_imputed$lag,
                            acf_imputed = acf_data_imputed$acf, lag_imputed = acf_data_imputed$lag)
    return(result_df)
  }
})

# We have to remove NULL entries from the list
result_dfs <- result_dfs[sapply(result_dfs, function(x) !is.null(x))]

# Then we write the list of data frames to an Excel file - change name accordingly
write_xlsx(result_dfs, "acf_output_15.xlsx")

## All done! Hooray!

# Just a few pointers on the acf and ccf. Firstly the acf results or autocorrelation...
# As you can see, we have values over the 0.8 mark aross a lag of 10 for most of
# variables across most of the sessions. This roughly means that at any time the
# current value of interest in highly correlated with the previous value and so on.
# This is important, especially in relation to imputation as we discussed, because
# indicates that there is some level of stationarity in the data when it is observed
# or analysed at 15Hz (that is 15 frames per second). This essentially means that
# the statistical properties of the data (in the form of the time series) 
# do not change so much over time. This in turn can be useful for imputation because
# it could hypothetically mean our estimate can be more robust based upon this 
# assumption that values local to one another are fairly similar (i.e there's not 
# much variation). The max.lag can be altered depending of what lag you wish to 
# investigate.

# On the ccf or cross-correlation, what you're presented with is a correlation 
# coefficient for each point of the lag. The array begins at '1' and ends at '11' 
# representing the lag from -5 to 0 and then through to 5. Interpreting this is 
# fairly simple, i.e. that positive correlation across the lag represents in-phase 
# a positive relationship shared by valence in person one and valence in person 2
# for example, also interpretable as a measure of synchrony. Indeed, the strength 
# of this relationship across the lag gives you an idea of when synchrony is liable 
# to occur, i.e. if the coefficient at +3 is the highest across the array, it indicates
# that one person in the dyad is most likely to synchronise their valence/arousal 3
# 3 seconds after the original exhibition of that strength of valence/arousal by 
# the other person. My understanding is that the values in the array represent averaged
# or summary coefficients if you want to think of it like that, so that each coefficient
# for each lag is the sum of many coefficient occuring across the time series. 

## Email or speak to me if you have further questions! Good luck!