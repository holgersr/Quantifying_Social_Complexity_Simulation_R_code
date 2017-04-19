compress_sim <- function(data){
  data$dyad <- paste(data$id1, data$id2, sep = "_")
  dat <- ddply(data, c("dyad"), summarize, 
               role = role[1],
               n = length(interaction_type),
               n_t = sum(interaction_type == "th"),
               n_ch = sum(interaction_type == "ch"),
               n_a = sum(interaction_type == "at"),
               n_bc = sum(interaction_type == "cs"),
               n_g = sum(interaction_type == "gr"))
  aux <- do.call(rbind, strsplit(dat$dyad, "_"))
  dat$id1 <- as.numeric(aux[, 1])
  dat$id2 <- as.numeric(aux[, 2])
  return(dat) 
}

bdi <- function(data){
  data$dyad <- paste(data$id1, data$id2, sep = "_")
  aux <- ddply(data, c("dyad"), summarize, 
               p_t = mean(interaction_type == "th"),
               p_ch = mean(interaction_type == "ch"),
               p_a = mean(interaction_type == "at"),
               p_bc = mean(interaction_type == "cs"),
               p_g = mean(interaction_type == "gr"))
  return(1/apply(aux[, c("p_t", "p_ch", "p_a", "p_bc", "p_g")]^2, MAR = 1, FUN = sum))
}

dcsi <- function(data){
  data$dyad <- paste(data$id1, data$id2, sep = "_")
  aux <- ddply(data, c("dyad"), summarize,
               R_bc = sum(interaction_type == "cs"),
               R_g = sum(interaction_type == "gr"))
  aux$R_bc <- aux$R_bc/mean(aux$R_bc)
  aux$R_g <- aux$R_g/mean(aux$R_g)
  return((aux$R_bc + aux$R_g)/2)
}

ifi <- function(data){
  data$dyad <- paste(data$id1, data$id2, sep = "_")
  aux <- ddply(data, c("dyad"), summarize,
               R_t = sum(interaction_type == "th"),
               R_c = sum(interaction_type == "ch"),
               R_a = sum(interaction_type == "at"),
               R_bc = sum(interaction_type == "cs"),
               R_g = sum(interaction_type == "gr"))
  aux$R_t <- aux$R_t/mean(aux$R_t)
  aux$R_c <- aux$R_c/mean(aux$R_c)
  aux$R_a <- aux$R_a/mean(aux$R_a)
  aux$R_bc <- aux$R_bc/mean(aux$R_bc)
  aux$R_g <- aux$R_g/mean(aux$R_g)
  return((aux$R_t + aux$R_c + aux$R_a + aux$R_bc + aux$R_g)/5)
}

tenor <- function(data){
  data$dyad <- paste(data$id1, data$id2, sep = "_")
  aux <- ddply(data, c("dyad"), summarize,
               R_t = sum(interaction_type == "th"),
               R_c = sum(interaction_type == "ch"),
               R_a = sum(interaction_type == "at"),
               R_bc = sum(interaction_type == "cs"),
               R_g = sum(interaction_type == "gr"))
  return((aux$R_bc + aux$R_g)/(aux$R_bc + aux$R_g + aux$R_t + aux$R_c + aux$R_a))
}

do_it_all1 <- function(seed){
  set.seed(seed)
  simulated_interactions <- sim1()
  simulated_interactions$dyad <- paste(simulated_interactions$id1, simulated_interactions$id2, sep = ".")
  dd <- ddply(simulated_interactions, c("dyad"), summarize, sum_obs = length(interaction_count))
  keep_index <- which(!(simulated_interactions$dyad %in% sample(dd$dyad)[1:189]))
  simulated_interactions <- simulated_interactions[keep_index, ]
  dat <- compress_sim(data = simulated_interactions)
  ## BDI:
  dat$bdi <- bdi(simulated_interactions)
  ## DCSI:
  dat$dcsi <- dcsi(data = simulated_interactions)
  ## IFI:
  dat$ifi <- ifi(data = simulated_interactions)
  ## Tenor:
  dat$tenor <- tenor(data = simulated_interactions)
  return(list(dat = dat, simulated_interactions = simulated_interactions))
}

do_it_all2 <- function(seed){
  set.seed(seed)
  simulated_interactions <- sim2()
  simulated_interactions$dyad <- paste(simulated_interactions$id1, simulated_interactions$id2, sep = ".")
  dd <- ddply(simulated_interactions, c("dyad"), summarize, sum_obs = length(interaction_count))
  keep_index <- which(!(simulated_interactions$dyad %in% sample(dd$dyad)[1:189]))
  simulated_interactions <- simulated_interactions[keep_index, ]
  dat <- compress_sim(data = simulated_interactions)
  ## BDI:
  dat$bdi <- bdi(simulated_interactions)
  ## DCSI:
  dat$dcsi <- dcsi(data = simulated_interactions)
  ## IFI:
  dat$ifi <- ifi(data = simulated_interactions)
  ## Tenor:
  dat$tenor <- tenor(data = simulated_interactions)
  return(list(dat = dat, simulated_interactions = simulated_interactions))
}

do_it_all3 <- function(seed){
  set.seed(seed)
  simulated_interactions <- sim3()
  simulated_interactions$dyad <- paste(simulated_interactions$id1, simulated_interactions$id2, sep = ".")
  dd <- ddply(simulated_interactions, c("dyad"), summarize, sum_obs = length(interaction_count))
  keep_index <- which(!(simulated_interactions$dyad %in% sample(dd$dyad)[1:189]))
  simulated_interactions <- simulated_interactions[keep_index, ]
  dat <- compress_sim(data = simulated_interactions)
  ## BDI:
  dat$bdi <- bdi(simulated_interactions)
  ## DCSI:
  dat$dcsi <- dcsi(data = simulated_interactions)
  ## IFI:
  dat$ifi <- ifi(data = simulated_interactions)
  ## Tenor:
  dat$tenor <- tenor(data = simulated_interactions)
  return(list(dat = dat, simulated_interactions = simulated_interactions))
}

do_it_all4 <- function(seed){
  set.seed(seed)
  simulated_interactions <- sim4()
  simulated_interactions$dyad <- paste(simulated_interactions$id1, simulated_interactions$id2, sep = ".")
  dd <- ddply(simulated_interactions, c("dyad"), summarize, sum_obs = length(interaction_count))
  keep_index <- which(!(simulated_interactions$dyad %in% sample(dd$dyad)[1:189]))
  simulated_interactions <- simulated_interactions[keep_index, ]
  dat <- compress_sim(data = simulated_interactions)
  ## BDI:
  dat$bdi <- bdi(simulated_interactions)
  ## DCSI:
  dat$dcsi <- dcsi(data = simulated_interactions)
  ## IFI:
  dat$ifi <- ifi(data = simulated_interactions)
  ## Tenor:
  dat$tenor <- tenor(data = simulated_interactions)
  return(list(dat = dat, simulated_interactions = simulated_interactions))
}