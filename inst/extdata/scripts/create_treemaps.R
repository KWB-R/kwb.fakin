################################################################################
##
## author: Hauke Sonnenberg
## created: 2018-09-20
## purpose: \
##   Create treemaps with treemap::treemap() for path trees occurring at KWB
## usage:
##   step-1: set file_info_dir to a valid path containing path-info* files
##   step-2: source the whole script to load the paths
##   step-3: \
##     go manually through the section of interest within an if (FALSE) section
##
################################################################################

#install.packages("treemap")
#install.packages("magick")

# Define path to directory containing "path-info" files
file_info_dir <- "/home/hauke/Desktop/Data/FAKIN/file-info_by-department"
#file_info_dir <- "~/Data/FAKIN"

# Load full file information from a text file
path_infos <- kwb.fakin:::read_path_information(file_info_dir)

# Define argument lists to png()
args_png_4_3 <- list(
  width = 0.8 * 25.4,
  height = 0.8 * 19.05,
  units = "cm",
  res = 200
)

args_png_4_3_half <- list(
  width = 0.8 * 0.5 * 25.4,
  height = 0.8 * 19.05,
  units = "cm",
  res = 200
)

args_png_1024_768 <- list(width = 1024, height = 768, units = "px")

# Plot all treemaps at once ----------------------------------------------------
if (FALSE)
{
  png_files <- kwb.fakin::plot_all_treemaps(path_infos, as_png = TRUE)
}

# Plot department overview -----------------------------------------------------

if (FALSE)
{
  # Define department folders
  department_strings <- c("SUW_Department", "GROUNDWATER", "WWT_Department")

  # Join department paths for very general overview
  path_data_kwb <- kwb.utils::resetRowNames(
    do.call(rbind, path_infos[department_strings])
  )

  png_files <- kwb.fakin::plot_treemaps_from_path_data(
    path_data = path_data_kwb,
    name = "KWB_half_width",
    as_png = TRUE,
    args_png = args_png_4_3_half,
    output_dir = kwb.utils::safePath(kwb.utils::desktop(), "tmp")
  )

  # Read the created images, merge them side-by-side and save as new image
  images <- magick::image_read(png_files)
  image_side_by_side <- magickx::image_matrix(images, nrow = 1)
  magick::image_write(image_side_by_side, file.path(tempdir(), "treemap_KWB_size.png"))
}

# Further plots ----------------------------------------------------------------
if (FALSE)
{
  png_files <- kwb.fakin::plot_treemaps_from_path_data(
    path_data = path_infos$GROUNDWATER, name = "GROUNDWATER_tmp",
    root_path = "Y:/GROUNDWATER/PROJECTS/",
    as_png = TRUE,
    args_png = args_png_4_3
  )

  #kwb.utils::assignPackageObjects("kwb.fakin")
  #kwb.utils::assignArgumentDefaults(kwb.fakin::plot_treemaps_from_path_data)

  png_files <- plot_treemaps_from_path_data(
    path_data = path_infos$WWT_Department,
    root_path = "Y:/WWT_Department/Projects",
    name = "wwt-projects",
    as_png = TRUE,
    n_levels = 1,
    max_depth = 4,
    n_biggest = 2,
    args_png = args_png_4_3
  )

  png_files <- kwb.fakin::plot_treemaps_from_path_data(
    path_data = path_infos$WWT_Department,
    root_path = "Y:/WWT_Department/Projects/POWERSTEP/Exchange/03 - Rabea/",
    name = "Rabea",
    as_png = FALSE,
    args_png = args_png_4_3
  )

  png_files <- kwb.fakin::plot_treemaps_from_path_data(
    path_data = path_infos$WWT_Department,
    root_path = "Y:/WWT_Department/Projects/AquaNES/",
    name = "AquaNES",
    as_png = TRUE,
    args_png = args_png_4_3
  )
}
