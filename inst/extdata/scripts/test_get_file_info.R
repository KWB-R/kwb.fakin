if (FALSE)
{
  root_dir <- "C:/Users/hsonne/Desktop"

  system.time(file_info <- get_file_info_powershell(root_dir, top_n = NULL))
  # User      System verstrichen
  #12.06       22.94       37.25

  system.time(file_info_2 <- kwb.fakin::get_recursive_file_info(root_dir))
  # User      System verstrichen
  # 0.94        3.72        8.05

  dim(file_info)
  dim(file_info_2)
}
