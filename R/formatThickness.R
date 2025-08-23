#' Re-formats the results output from ImageJ to display measurements by Red, Green, Blue and total area.
#'
#' @description
#' Uses one supplied dataframe (thickness_results) from the Thickness Assay ImageJ analysis, and reorganizes the dataframe to display measurements by the separate colours (Red, Green, Blue) and total area.
#'
#' @param results A dataframe that stems from using the Epithelial_Barrier_Thickness_Tool in ImageJ for the Thickness Assay analysis.
#'
#' @example \dontrun{formatThickness(read.csv("Thickness_Assay_Measurements.csv"))}
#'
#' @returns A .csv file containing the file name broken down into Strain, Replicate and Image, as well as the measurements for the colours and total area.
#' @export

formatThickness <- function(results){

  # first remove the first column
  results_df <- results[-1]

  # split "Label" into different columns for Strain, Replicate, and Image
  namesdf <- data.frame(Label = results_df$Label) %>%
    separate(Label, into = c("Strain", "Replicate", "Image"), sep = '_') %>%
    separate(Image, into = c("Image", "JPG", "Colour"))

  # for NA values in Color, we changed it to "total", the total area measurements
  namesdf$Colour[is.na(namesdf$Colour)] <- "total"

  # split Replicate and Image from their respective numbers
  namesdf <- namesdf %>%
    extract(Replicate, into = c("Remove1", "Replicate"), regex = "([a-zA-Z]+)(\\d+)") %>%
    extract(Image, into = c("Remove2", "Image"), regex = "([a-zA-Z]+)(\\d+)")

  # combines the split names into original data frame, and then neatly reorganizes the columns
  combine_df <- results_df %>%
    cbind(namesdf) %>%
    separate(Label, into = c("Label", "Remove3"), sep = " ") %>%
    select(Label, Strain, Replicate, Image, everything()) %>%
    select(-JPG, -Remove1, -Remove2, -Remove3)

  # puts the measurements to the appropriate RGB and total area, and reorders columns neatly
  new_combine <- combine_df %>%
    pivot_wider(names_from = Colour, values_from = c("Area^2", "Area %", "Avg. Height", "Std. Dev. Height")) %>%
    select(Label, Strain, Replicate, Image, `Area^2_total`, `Area %_total`, `Avg. Height_total`, `Std. Dev. Height_total`,`Area^2_red`, `Area %_red`, `Avg. Height_red`, `Std. Dev. Height_red`, `Area^2_blue`, `Area %_blue`, `Avg. Height_blue`, `Std. Dev. Height_blue`, `Area^2_green`, `Area %_green`,`Avg. Height_green`, `Std. Dev. Height_green`)

  # outputs the new data frame back into a csv file
  fileName <- file.path(getwd(), "Thickness_Assay_Results.csv")
  write.csv(new_combine, fileName, row.names=FALSE)
}
