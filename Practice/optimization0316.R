
#set working directory

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
# feature에 문자들어오면 이방법 사용 불가


wbcd <- wbcd[-1] # column 1번(id)를 빼라

wbcd