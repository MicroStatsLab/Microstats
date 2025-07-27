#' combine two ImageJ result csv into one csv file.
#'
#' @description
#' Uses two supplied dataframes (total area, and R/G/B measurements) of the thickness assay images and combines into one file.
#'
#' @param area_total A dataframe of the total area.
#' @param area_RGB A dataframe the red, green, blue area.
#'
#' @example \dontrun{growthRates(read.csv("totalArea.csv"), read.csv("separateArea.csv"))}
#'
#' @returns A dataframe containing the file name broken down into strain, replicate and images, as well as the measurements
#' @export

combineImageJ <- function(area_total, area_RGB){

  # first we split "label" into different columns for strain, replicate, image
  # split replicate and image to just keep the numbers
  # removes jpg and other columns we dont need
  namesdf_total <- data.frame(Label = area_total$Label)
  namesdf_total <- namesdf_total %>%
    separate(Label, into = c("Strain", "Replicate", "Image"), sep = "_") %>%
    separate(Image, into = c("Image", "JPG")) %>%
    separate(Replicate, into = c("Remove1", "Replicate"), sep = "p") %>%
    separate(Image, into = c("Remove2", "Image"), sep = "e") %>%
    select(-Remove1, -Remove2, -JPG)

  # combines the split names into og dataframe, and then neatly reorganizes the columns
  area_total <- area_total %>%
    cbind(namesdf_total) %>% select(Label, Strain, Replicate, Image, everything())


  #remove Count and Average Size from both files since we don't need it
  if("Count" %in% names(area_total)){
    area_total$Count <- NULL
  }

  if("Average Size" %in% names(area_total)){
    area_total$`Average Size` <- NULL
  }

  if("Count" %in% names(area_RGB)){
    area_RGB$Count <- NULL
  }

  if("Average Size" %in% names(area_RGB)){
    area_RGB$`Average Size` <- NULL
  }

  # create new column for the RGB values, then creates new columns for RGB measurements for total area and % area
  # this also reorganizes the columns for the colour measurements are next to each other
  new_area_RGB = area_RGB %>%
    separate(Label, into = c("Label", "Colour"), sep = " ") %>%
    pivot_wider(names_from = Colour, values_from = c("Total Area", "% Area")) %>%
    select(-Label, `Total Area_(blue)`, `% Area_(blue)`, `Total Area_(green)`,`% Area_(green)`,`Total Area_(red)`,`% Area_(red)`)

  # combines both dataframes into one
  thickness_measurements = area_total %>%
    cbind(new_area_RGB)

  returns(thickness_measurements)

}
