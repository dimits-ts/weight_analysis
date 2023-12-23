# FUNCTIONS

RESOURCE_PATH = "output"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}

filepath <- function(filename) {
  return(file.path(RESOURCE_PATH, filename))
}


# Utility function to save a plot to the disk.
my_save_plot <- function(name, plot_func, ...) {
  # display plot
  plot_func(...)
  # save plot
  filepath = filepath_png(name)
  png(filepath)
  plot_func(...)
  dev.off() # COMMENT THIS LINE TO PREVENT PLOT SAVING
}


# generates bins for lm model homoscedascity  
quantcut <- function(x, digits=6) { 
  cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}
