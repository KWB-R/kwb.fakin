# boxplot_main_extensions ------------------------------------------------------
boxplot_main_extensions <- function(data, top_n = 10)
{
  extensions <- kwb.utils::selectColumns(data, "extension")

  data$extension <- to_top_n(extensions, n = top_n)

  data$size[kwb.utils::selectColumns(data, "size") == 0] <- 1

  graphics::boxplot(size ~ extension, data = data, log = "y", las = 2)
}
