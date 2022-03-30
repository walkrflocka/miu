
# MIU encoder, takes string and turns into list representation using run-length encoding.
encodeMIU <- function(string){
  if(grepl('[^MIU]', string)){
    stop('String does not contain only "M", "I", or "U"')
  } 
  
  miu.split <- strsplit(string, '*')[[1]]
  
  # The longest possible vector we need to allocate using RLE is length(string),
  # with pattern IUIUIUIU... and so on, with 1-length runs. 
  miu.coded <- list(rep(NA, times = nchar(string)))
  
  # Now, implement the run length encoding - only encoding I's.
  i <- 1
  while(i <= length(miu.split)){
    if(miu.split[[i]] %in% c('M', 'U')){
      # If M or I, return self.
      miu.coded[[i]] <- miu.split[[i]]
      i <- i + 1
    } else if(miu.split[[i]] == "I"){
      run <- 1
      # Look ahead until you hit a non-I character, incrementing i every time
      while((i + run - 1) < length(miu.split)){
        if(miu.split[[i + run]] == "I"){
          run <- run + 1
        } else break
      }
      
      miu.coded[[i]] <- run
      i <-  i + run
      
    } else {
      # Catcher for weird edge cases. Should never fire. I'm still paranoid.
      stop(paste('Unexpected character in position', i))
    }
  }
  
  miu.coded <- na.omit(miu.coded)
  
  return(miu.coded)
}

# FIXY FIXY
# Sum sequential runs. Just a cleaner subroutine.
accumulateMIU <- function(miu.list){
  collapse <- i <- 1
  while(i < length(miu.list)){
    if(suppressWarnings(!is.na(as.numeric(miu.list[[i]])))){
      repeat{
        i <- i + 1
        if(suppressWarnings(is.na(as.numeric(miu.list[[i]])))) break
      }
      collapse <- collapse + 1
    } else {
      i <- i + 1
    }
  }
}

# Rule 1: If you possess a string whose last letter is I, you may add on a U at the end.
rule1 <- function(miu.list){
  if(miu.list[[length(miu.list)]] != "I") stop('Rule 1 violation: Last char not "I"')
  
  return(c(miu.list, 'U'))
}

# Rule 2: Given a string matches the pattern "Mx", where "x" is any string, recieve "Mxx"
rule2 <- function(miu.list){
  if(miu.list[[1]] != 'M') stop('Rule 2 violation: First char not "M"')
  
  dub.list <- c(miu.list, miu.list[2:length(miu.list)])
  out.list
  
}

# Rule 3: If "III" occurs in a string, you may replace it with "U"
# With our encoded string, this checks for a number at position i greater than 3,
# then splits it into c(x, "U", y), where x + y + 3 == the original number.
rule3 <- function(miu.list, i, x, y){
  if(!is.numeric(miu.list[[i]])){
    stop(paste('Rule 3 violation: Value at position', i, 'is not a run:', miu.list[[i]]))
  }
  
  if(miu.list[[i]] < 3){
    stop(paste('Rule 3 violation: Value at position', i, 'is less than 3:', miu.list[[i]]))
  }
  if(x + y + 3 != miu.list[[i]]){ 
    stop(paste0('Rule 3 violation: X + Y + 3 do not equal the number at position ', i, ': ', miu.list[[i]]))
  }
  
  return(c(miu.list[1:(i-1)], x, "U", y, miu.list[(i+1):length(miu.list)]))
}

# Rule 4: If "UU" occurs, you may drop it.
# With our encoding, if i and i+1 are "U", drop them.
rule4 <- function(miu.list, i){
  if(miu.list[[i]] != 'U' & miu.list[[i+1]] != 'U'){
    stop(paste('Rule 4 violation: No "UU" pattern at index:', i))
  }
  
  return(c(miu.list[1:(i-1)], miu.list[(i+2):length(miu.list)]))
}