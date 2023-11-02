
DoubleDigestProblem <- function(delta_XA, delta_XB, delta_XAB){
  all_A <- AllPermutations(delta_XA)
  all_B <- AllPermutations(delta_XB)

  for (A in all_A){
    for (B in all_B){

        A_possitions <- 0:length(A)
        for (i in 1:length(A)){ A_possitions[i+1] <- A_possitions[i] + A[i] }
        B_possitions <- 0:length(B)
        for (i in 1:length(B)){ B_possitions[i+1] <- B_possitions[i] + B[i] }

        combination <- unique(c(A_possitions, B_possitions))
        sorted_combination <- sort(combination)

        differences <- diff(sorted_combination)
        sorted_differences <- sort(differences)

        correct <- TRUE
        for (id in 1:length(sorted_differences)){
          if (delta_XAB[id] == sorted_differences[id]){ }else{correct <- FALSE} }

        if (correct == TRUE){
          print('Correct arrangement:')
          print(A)
          print(B)
          print('Correct arrangement (transverse):')
          print(sum(A)-A)
          print(sum(A)-B)
        }

    }}
}

AllPermutations <- function(vec) {
  if (length(vec) == 1) {
    return(list(vec))
  } else {
    perms <- list()
    for (i in 1:length(vec)) {
      sub_vec <- vec[-i]
      sub_perms <- AllPermutations(sub_vec)
      for (perm in sub_perms) {
        new_perm <- c(vec[i], perm)
        perms <- append(perms, list(new_perm))
      }
    }
    return(perms)
  }
}

# delta_XA <- c(2,3,5,10)
# delta_XB <- c(3,7,10)
# delta_XAB <- c(1,2,2,5,5,5)

DoubleDigestProblem(delta_XA=c(2,3,5,10), delta_XB=c(3,7,10), delta_XAB=c(1,2,2,5,5,5))


PartialDigestProblem <- function(deltaX){
  width <- max(deltaX)
  which_max<- which.max(deltaX)
  deltaX <- deltaX[-which_max]
  X <- c(0, width)
  Place(deltaX, X, width)
}

Place <- function(deltaX, X, width){
  if (length(deltaX)==0){ # ukončovací podminka
    print(X)
    return(X)
  }
  y <- max(deltaX)
  deltaX_podm <- abs(X-y)
  deltaX_podm_w <- abs(X-(width-y))

  if (all(deltaX_podm %in% deltaX)){
    for (num in deltaX_podm){ # remove lengths with y from deltaX
      i <- which(deltaX == num)[1]
      deltaX <- deltaX[-i] }
    X <- sort(c(X, y)) # add y to X

    Place(deltaX, X, width)

    X <- X[-(which(X == y)[1])] # remove y from X
    deltaX <- sort(c(deltaX, deltaX_podm))  # add lengths with y from deltaX
  }

  if (all(deltaX_podm_w %in% deltaX)){
    for (num in deltaX_podm_w){ # remove lengths with (width-y) from deltaX
      i <- which(deltaX == num)[1]
      deltaX <- deltaX[-i] }
    X <- sort(c(X, (width-y))) # add (width-y) to X

    Place(deltaX, X, width)

    X <- X[-(which(X == (width-y))[1])] # remove (width-y) from X
    deltaX <- sort(c(deltaX, deltaX_podm_w)) # add lengths with (width-y) from deltaX
  }
  return()
}

PartialDigestProblem(c(2,2,3,3,4,5,6,7,8,10))