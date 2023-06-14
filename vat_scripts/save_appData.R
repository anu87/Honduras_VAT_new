
salmi_data <- readxl::read_xlsx("data/SALMI HON Inventario Biologicos_2022-01-13. Vaccine Inventory .xlsx")
salmi_data <- salmi_data[,1:7]
colnames(salmi_data) <- salmi_data[3,]
salmi_data <- salmi_data[-c(1:3),]

saveRDS(salmi_data, "appdata/salmi_app_data.rds")
