if(!require('googledrive')) install.packages('googledrive')

library(googledrive)
drive_auth()
drive_find(n_max = 10)

drive_find(pattern = "/2022 - 2/*.zip", n_max = 100)

drive_ls(path = '/2022 - 2/', pattern = '*.*')


  
  comilla simple <- alt + 39 <- '' E♠
  comillas invertidas <- alt + 96 <- ``