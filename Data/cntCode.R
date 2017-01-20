cnt <- c("SK", "GB", "IE", "EE", "FI", "SE", "DE", "LU", "NL", "DK", "BE", "FR", "ES", "PO",
         "CZ", "CH", "AT", "IS", "HU", "SI", "IT", "GR", "TR", "AU", "CA", "CL", "IL", "JP",
         "KR", "MX", "NZ", "US", "LV", "PT", "NO")
sort(cnt)
cntCode <- unique(Dat[, c("Country", "CountryCode")])
write.csv(cntCode, "Data/cntCode.csv", row.names = FALSE)