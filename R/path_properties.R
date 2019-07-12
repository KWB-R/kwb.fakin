# get_words_to_attribute_list --------------------------------------------------
get_words_to_attribute_list <- function(file)
{
  df <- read_csv_de(file)

  df <- df[df$attributes != "", ]

  df
}

# print_replacement_template ---------------------------------------------------
print_replacement_template <- function(x)
{
  kwb.utils::catLines(paste(x, x, sep = ";"))
}
