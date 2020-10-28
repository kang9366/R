if(!require(multilinguer)){install.packages("multilinguer")}
if(!require(remotes)){install.packages("remotes")}
if(!require(devtools)){install.packages("devtools")}
install.packages(c('stringr','hash','tau','Sejong','RSQLite','devtools'),type="binary")
install.packages(c("remotes","ggplot2", "data.table", "rlang"))
install_jdk()

library(multilinguer)
library(rJava)
library(KoNLP)

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes=TRUE)
buildDictionary(ext_dic = "woorimalsam")

useNIADic()

s <- "오늘은 날씨가 맑습니다."
extractNoun(s)
