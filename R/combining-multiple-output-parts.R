# Combine multiple parts into one results file:

jh_output_files <- list.files(path = "output/jh",
                              pattern = "_\\d.xlsx$",
                              full.names = TRUE)

jh_output <- map_dfr(jh_output_files,
                     read_xlsx,
                     .id = "part")

jh_output %>% 
  count(part)


names(jh_output)


writexl::write_xlsx(jh_output,
                    path = "output/jh/reviews_output_machine_jh.xlsx")

write_csv(jh_output,
          file = "output/jh/reviews_output_machine_jh.csv")
