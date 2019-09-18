# ggmantel
```
## install
if(!require(ggcorrr))
  devtools::install_github("houyunhuang/ggcorrr")
devtools::install_github("houyunhuang/ggmantel")

## examples
library(ggcorrr)
library(ggmantel)
library(vegan)
data(varespec)
data(varechem)
spec <- list(spec01 = varespec[ , 22:25],
             spec02 = varespec[ , 1:4],
             spec03 = varespec[ , 38:43],
             spec04 = varespec[ , 15:20])
df <- fortify_mantel(spec, varechem, process = FALSE)
corr_df <- fortify_corr(varechem, type = "upper", show_diag = FALSE,
                        corr_test = TRUE, cluster = T)
mantel_df <- get_link_data(df, corr_df, type = "upper", grp_hjust = c(0, -1.5, -1, 0))
ggmantel(mantel_df, corr_df, type = "upper", corr_type = "square", curvature = 0.05)
```
