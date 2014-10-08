#' Syllabify
#'
#' @export
#' @import stringr
#' @examples
#' syllabify(c("S", "T", "R", "EH1", "NG", "TH", "AH0", "N"))
#' syllabify(c("M", "AE2", "S", "T", "ER0"))
#' syllabify(c("B","AE","N","T","ER"))
#' syllabify(c("P","R","AE","NG","K","S","T","ER","Z"))
#' syllabify("S AH0 L AE1 B IH0 F AH0 K EY2 SH AH0 N")
syllabify <- function(trans){

  segments <- unlist(str_split(trans, " "))
  nuclei_string <- "A|E|I|O|U|@"
  nucs <- grep(nuclei_string, segments)
  n <- length(nucs)

  r_colored <- nucs[grep("R", segments[nucs])]

  if(length(r_colored) > 0){
    for(r_idx in 1:length(r_colored)){
      r <- r_colored[r_idx]
      if(r == length(segments)){
        segments <- c(segments, "R")
      }else if(segments[r+1] != "R"){
        segments <- c(segments[1:r], "R", segments[(r+1):length(segments)])
        nucs <- grep(nuclei_string, segments)
        r_colored <- nucs[grep("R", segments[nucs])]
      }
    }
  }

  syls <- vector(mod = "list", length = n)
  nsyls <- vector(mod = "list", length = n)


  for(i in 1:n){
    if(segments[nucs[i]] == "@"){
      stress <- 0
    }else{
      stress <- as.numeric(gsub("[A-Z]*", "", segments[nucs[i]]))
    }
    if(segments[nucs[i]] == "AH0"){
      segments[nucs[i]] <- "@"
    }
    syls[[i]] <- list(onset = vector(), nucleus = gsub("[0-9]", "", segments[nucs[i]]), coda = vector(), stress = stress )
    nsyls[[i]] <- list(onset = vector(), nucleus = nucs[i], coda = vector())


    while(T){
      if(length(syls[[i]]$onset) < 1){
        ons <- nsyls[[i]]$nucleus[1]-1
      }else{
        ons <- nsyls[[i]]$onset[1] - 1
      }
      if(ons == 0){
        break
      }
      if(i > 1){
        if(ons == nsyls[[i-1]]$nucleus[1]){
          break
        }
      }

      okl = c("B","F","G","K","P","S","S")
      okr = c("B","D","F","G","K","P","T","V")
      oks = c("K","L","M","N","P","T","V","W")
      okw = c("T","D","K","P","S","HH")
      ## HH included for for occasional "HH W" transcriptions for <wh>
      oky = c("B","F","V","K","G")

      if(length(syls[[i]]$onset) == 0){
        syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
        nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
      }else if(syls[[i]]$onset[1] == "L"){
        if(segments[ons] %in% okl){
          syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
          nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
        }else{
          break
        }
      }else if(syls[[i]]$onset[1] == "R"){
        if(segments[ons] %in% okr){
          syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
          nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
        }else{
          break
        }
      }else if(syls[[i]]$onset[1] == "W"){
        if(segments[ons] %in% okw){
          syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
          nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
        }else{
          break
        }
      }else if(syls[[i]]$onset[1] == "Y"){
        if(segments[ons] %in% oky){
          syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
          nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
        }else{
          break
        }

      }else if(syls[[i]]$onset[1] %in% oks){
        if(segments[ons] == "S"){
          #print("adding s onset")
          syls[[i]]$onset <- c(segments[ons], syls[[i]]$onset)
          nsyls[[i]]$onset <- c(ons, nsyls[[i]]$onset)
        }else{
          break
        }
      }else{
        break
      }
    }
  }
  for(i in 1:n){
    if(length(syls[[i]]$onset) > 1 & syls[[i]]$onset[1] == "NG"){
      syls[[i]]$onset <- syls[[i]]$onset[-1]
      syls[[i-1]]$coda <- c(syls[[i-1]]$coda, "NG")
    }
    while(T){
      if(length(syls[[i]]$coda) == 0){
        cod <- nsyls[[i]]$nucleus[1] + 1
      }else{
        cod <- max(nsyls[[i]]$coda)+1
      }
      if(cod > length(segments)){
        break
      }else if(i == n){
        syls[[i]]$coda <- c(syls[[i]]$coda, segments[cod])
        nsyls[[i]]$coda <- c(nsyls[[i]]$coda, cod)
      }else if(length(nsyls[[i+1]]$onset) == 0){
        if(cod == nsyls[[i+1]]$nucleus[1]){
          break
        }
      }else if(cod == nsyls[[i+1]]$onset[1]){
        break
      }else{
        syls[[i]]$coda <- c(syls[[i]]$coda, segments[cod])
        nsyls[[i]]$coda <- c(nsyls[[i]]$coda, cod)
      }
    }
  }
  return(syls)
}