sim4 <- function(n = 36){
  id <- 1:n
  dyads <- t(combn(id, 2))
  base_data <- data.frame(id1 = dyads[, 1],
                          id2 = dyads[, 2])
  rm(dyads)
  n_dyad <- nrow(base_data)
  n_high_pos <- round(n_dyad * 1/6)
  n_remain <- n_dyad - n_high_pos
  aux <- rep(1:4, length = n_remain)
  n_mid_pos <- sum(aux == 1)
  n_low_pos <- sum(aux == 2)
  n_amb <- sum(aux == 3)
  n_neg <- sum(aux == 4)
  rm(aux)
  base_data$role <- c(rep("high_pos", n_high_pos), rep("mid_pos", n_mid_pos), 
                      rep("low_pos", n_low_pos), rep("amb", n_amb), 
                      rep("neg", n_neg))
  base_data$role <- sample(base_data$role)
  ##
  result_id1 <- NULL
  result_id2 <- NULL
  result_role <- NULL
  result_interaction_count <- NULL
  result_interaction_type <- NULL
  ##
  for(i in 1:nrow(base_data)){
    if(base_data$role[i] == "high_pos"){
      interaction_count <- 200/2
      prob_pos_neg = c(0.8, 0.2)
    }
    if(base_data$role[i] == "mid_pos"){
      interaction_count <- 150/2
      prob_pos_neg = c(0.8, 0.2)
    }
    if(base_data$role[i] == "low_pos"){
      interaction_count <- 100/2
      prob_pos_neg = c(0.8, 0.2)
    }
    if(base_data$role[i] == "amb"){
      interaction_count <- 100/2
      prob_pos_neg = c(0.5, 0.5)
    }
    if(base_data$role[i] == "neg"){
      interaction_count <- 100/2
      prob_pos_neg = c(0.2, 0.8)
    }
    ##
    pos_neg <- sample(c("pos", "neg"), size = interaction_count, replace = T, prob = prob_pos_neg)
    pos <- sample(c("cs", "gr"), replace = T, size = sum(pos_neg == "pos"), prob = c(0.5, 0.5))
    neg <- sample(c("th", "ch", "at"), replace = T, size = sum(pos_neg == "neg"), prob = c(0.7, 0.2, 0.1))
    ##
    result_id1_here <- rep(base_data$id1[i], interaction_count)
    result_id2_here <- rep(base_data$id2[i], interaction_count)
    result_role_here <- rep(base_data$role[i], interaction_count)
    result_interaction_count_here <- 1:interaction_count
    result_interaction_type_here <- sample(c(pos, neg))
    ##  
    result_id1 <- c(result_id1, result_id1_here)
    result_id2 <- c(result_id2, result_id2_here)
    result_role <- c(result_role, result_role_here)
    result_interaction_count <- c(result_interaction_count, result_interaction_count_here)
    result_interaction_type <- c(result_interaction_type, result_interaction_type_here)
  }
  result <- data.frame(id1 = result_id1, id2 = result_id2, role = result_role, 
                       interaction_count = result_interaction_count, 
                       interaction_type = result_interaction_type)
  return(result)
}