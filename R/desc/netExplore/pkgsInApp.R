pkgs = c(
	'dplyr', 'tidyr', 'magrittr',
	'network', 'igraph',
	'ggplot2', 'RColorBrewer', 'latex2exp',
	'countrycode', 'extrafont', 'Cairo',
  'shiny', 'shinyWidgets',
  'scales', 'visNetwork'
)

library(knitr)
pkgInfo = installed.packages()[pkgs,]
pkgInfo = pkgInfo[,c('Package', 'Version')]
rownames(pkgInfo) = NULL

R.version
kable(pkgInfo, format='markdown')

# ## output from sm
#                _
# platform       x86_64-w64-mingw32
# arch           x86_64
# os             mingw32
# system         x86_64, mingw32
# status
# major          4
# minor          0.0
# year           2020
# month          04
# day            24
# svn rev        78286
# language       R
# version.string R version 4.0.0 (2020-04-24)
# nickname       Arbor Day
#
# |Package      |Version |
# |:------------|:-------|
# |dplyr        |0.8.5   |
# |tidyr        |1.0.2   |
# |magrittr     |1.5     |
# |network      |1.16.0  |
# |igraph       |1.2.5   |
# |ggplot2      |3.3.0   |
# |RColorBrewer |1.1-2   |
# |latex2exp    |0.4.0   |
# |countrycode  |0.16    |
# |extrafont    |0.17    |
# |Cairo        |1.5-12  |
# |shiny        |1.4.0.2 |
# |shinyWidgets |0.5.3   |
# |scales       |1.1.0   |
# |visNetwork   |2.0.9   |
