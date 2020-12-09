if(!require(multilinguer)){install.packages("multilinguer")}
if(!require(remotes)){install.packages("remotes")}
if(!require(devtools)){install.packages("devtools")}
install.packages(c('stringr','hash','tau','Sejong','RSQLite','devtools'),type="binary")
install.packages(c("remotes","ggplot2", "data.table", "rlang"))
install_jdk()

library(Sejong)
library(multilinguer)
library(rJava)
library(KoNLP)
install.packages("rJava")
devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes=TRUE)
buildDictionary(ext_dic = "woorimalsam")
library()

useNIADic()
library(KoNLP)
library(devtools)
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos=NULL, type="source")
s <- "오늘은 날씨가 맑습니다."
extractNoun(s)
