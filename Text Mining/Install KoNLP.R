install.packages("multilinguer")
install.packages("remotes")
install.packages(c('stringr','hash','tau','Sejong','RSQLite','devtools'),type="binary")
install.packages(c("remotes","ggplot2", "data.table", "rlang"))

library(multilinguer)
library(rJava)
library(KoNLP)

install_jdk()

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes=TRUE)
buildDictionary(ext_dic = "woorimalsam") # "woorimalsam" dic을 불러옵니다

useNIADic()

s <- "오늘은 날씨가 맑습니다."
extractNoun(s)
