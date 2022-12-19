read.packages <- function(){
  Packages <- c("plyr","dplyr", "ggplot2", "tidyverse", "cluster", "zoo", "gtools", 
                "nloptr", "lubridate", "adagio", "ncdf4", "glmtools", 
                "likelihood", "hydroGOF", "akima", "pracma", "signal","RColorBrewer",
                "gridExtra","purrr","rLakeAnalyzer","sfsmisc","viridis","patchwork")
  lapply(Packages, library, character.only = TRUE)
}

stringwstring <- function(textarg = "** Default text **" ){ 
  text <- (noquote(textarg)) 
  return(text) 
}

get_glm_description <- function(iter, perm, id, description){
  permutations_id_matrix <-read.table(perm,header = TRUE, sep =",")
  id_matrix <- read_csv(id, 
                        col_names = TRUE)
  id_description <- read_csv(description, col_names = TRUE)
  
  val1 <- c()
  for (ii in 2:ncol(permutations_id_matrix)){
    val1 <- append(val1, id_matrix[permutations_id_matrix[iter, ii], ii-1])
  }
  
  val2 <- c();val3 <- c()
  for (ii in 1:length(val1)){
    val2 <- append(val2, id_description$description[id_description$mode == val1[ii]])
    val3 <- append(val3, id_description$value[id_description$mode == val1[ii]])
  }
  val <- cbind(val2, val3)
  return(val)
}

plot_contour <- function(mod_nc, reference = "surface", h, var, unit, tlevels){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)
  
  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),0.1))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  if ((median(apply(sim.watdep,2,max,na.rm=TRUE))+median(apply(sim.watdep,2,sd,na.rm=TRUE)))<
      max(sim.watdep[,1],na.rm=TRUE)){
    max.plot.dep <- ceiling(median(apply(sim.watdep,2,max,na.rm=TRUE)))
  } else {
    max.plot.dep <-ceiling(max(sim.watdep[,1],na.rm=TRUE))
  }
  
  if (max.plot.dep<=20){iter.plot.dep = 1} else if (max.plot.dep<=50){iter.plot.dep = 5} else (iter.plot.dep = 10)
  
  spectral.colors <-  colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral') )
  
  inMeter <- function(x) {paste0(x, " m")}
  inCelsius <- function(x) {paste0(x, paste(" ",unit,sep=''))}
  filled.contour(x=as.numeric(datetime), y=(int.dep)*(-1), z = t(sim.wattemp),
                 levels=tlevels,
                 col=rev(spectral.colors(length(tlevels))), main=h, cex = 1.5, cex.main = 3., 
                 plot.axes = {axis(1, labels=format(pretty(datetime,20), "%Y-%b"), at = as.numeric(pretty(datetime,20)), cex.axis=2,las=1.5);
                   axis(2, labels=inMeter(rev(seq(0,max.plot.dep,iter.plot.dep))*(-1)), at = rev(seq(0,max(max.plot.dep),iter.plot.dep))*(-1), cex.axis=1.5)},
                 key.axes = {axis(4,at=unique(tlevels),labels=(inCelsius(unique(tlevels))))})
}

get_wattemp <- function(mod_nc, reference = "surface", h, var, int_step){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)
  
  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),int_step))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  return(list('sim' = sim.wattemp, 'time' = datetime, 'depth' = int.dep))
}

save_ncdf <- function(name, time, z, wtr, oxy){
  long = 50
  lat = 50
  
  dimX = ncdim_def("lon", "degrees", long)
  dimY = ncdim_def("lat", "degrees", lat)
  dimT = ncdim_def("time", "hours since", as.double(time))
  dimZ = ncdim_def("depth", "meters", z)
  
  fillvalue <- 1e32
  dlname <- 'water temperature'
  tmp_def <- ncvar_def("wtr", "deg_C", list(dimZ, dimT), fillvalue, dlname, prec="single")
  dlname <- 'dissolved oxygen'
  oxy_def <- ncvar_def("do", "mgperLitre", list(dimZ, dimT), fillvalue, dlname, prec="single")
  
  ncfname <- paste(name,".nc",sep="")
  ncout <- nc_create(ncfname, list(tmp_def, oxy_def), force_v4 = TRUE)
  
  ncvar_put(ncout, tmp_def, wtr)
  ncvar_put(ncout, oxy_def, oxy)
  
  nc_close(ncout)
}

# tadgh's function
mod2obs <- function(mod_nc, obs, reference = 'surface', var){
  deps = unique(obs[,2])
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var,reference = reference, z_out = deps)
  mod <- match.tstep(obs, mod) #From gotm_functions.R
  mod <- reshape2::melt(mod, id.vars = 1)
  mod[,2] <- as.character(mod[,2])
  mod[,2] <- as.numeric(gsub(paste(var,"_",sep=''),'',mod[,2]))
  colnames(mod) <- c('DateTime', 'Depth', var)
  mod <- mod[order(mod$DateTime, mod$Depth),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1,2), all.x = T)
    #mod <- merge(obs, mod, by = c(1,2), all = T)
    mod <- mod[order(mod$DateTime, mod$Depth),]
    mod <- mod[,c(1,2,4)]
    colnames(mod) <- c('DateTime', 'Depth', var)
  }
  return(mod)
}

mod2obs_phy <- function(mod_nc, obs, reference = 'surface', var){
  deps = seq(0,2,by = 0.5)
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var,reference = reference, z_out = deps)
  mod <- data.frame('DateTime' = mod$DateTime, 'Var' = apply(mod[,2:ncol(mod)],1,mean))
  obs$datetime <- as.Date(obs$datetime,format='%Y-%m-%d')
  obs <- as.data.frame(obs)
  mod$DateTime <- as.Date(mod$DateTime,format='%Y-%m-%d')
  mod <- match.tstep(obs, mod) #From gotm_functions.R
  # mod <- reshape2::melt(mod, id.vars = 1)
  # mod[,2] <- as.character(mod[,2])
  #  mod[,2] <- as.numeric(gsub(paste(var,"_",sep=''),'',mod[,2]))
  colnames(mod) <- c('DateTime', var)
  mod <- mod[order(mod$DateTime),]
  # mod <- mod[order(mod$DateTime),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1), all.x = T)
    #mod <- merge(obs, mod, by = c(1,2), all = T)
    mod <- mod[order(mod$datetime),]
    # mod <- mod[order(mod$DateTime),]
    mod <- mod[,c(1,3)]
    colnames(mod) <- c('DateTime', var)
  }
  return(mod)
}


run_glm <- function(os){
  if (os == "Windows"){
    system('run_glm3.bat',ignore.stdout=TRUE)
  } else if (os == "Unix"){
    system("glm",ignore.stdout=TRUE)
  } else if (os == "Original"){
    file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
    file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
    #file.copy('aed2/aed2_phyto_twogroups.nml', 'aed2/aed2_phyto_pars_NEW_30Jan19.nml', overwrite = TRUE)
    system("/Users/robertladwig/Documents/AquaticEcoDynamics_gfort/GLM/glm",ignore.stdout=TRUE)
    #system("source /opt/intel/bin/compilervars.sh intel64; /Users/robertladwig/Documents/AquaticEcoDynamics/GLM/glm",ignore.stdout=TRUE)
  } else if (os == "Compiled"){
    system("source /opt/intel/bin/compilervars.sh intel64; /Users/robertladwig/Documents/AquaticEcoDynamics/GLM/glm",ignore.stdout=TRUE)
} else if (os == "Cayelan"){
  system("source /opt/intel/bin/compilervars.sh intel64;/Users/robertladwig/Documents/AquaticEcoDynamics_Cayelan/GLM/glm",ignore.stdout=TRUE)
} else if (os == 'gfort'){
  system("/Users/robertladwig/Documents/AquaticEcoDynamics_gfort/GLM/glm",ignore.stdout=TRUE)
}
}

# gotm_functions.R
match.tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
    tim1 = df1[,1]
    tim2 = df2[,1]
    ind = c()
    pb = txtProgressBar(min = 0, max = length(tim1), style = 3)
    for(i in 1:length(tim1)){
      ind = append(ind, which(tim2 == tim1[i]))
      setTxtProgressBar(pb, i)
    }
    close(pb)
    df2.mat = df2[ind,]
    return(df2.mat)
  }
}

get_nse <- function(x, y){
  id1 <- !is.na(obs[,3]) 
  obs <- y[id1,3]
  mods <- x[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  sum_bottom <- sum((obs-mean(obs))^2)
  nse <- 1- sum_up/sum_bottom
  return(nse)
}

get_rmse <- function(mods, obs){
  id1 <- !is.na(obs[,3]) 
  obs <- obs[id1,3]
  mods <- mods[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  rmse <- sqrt(sum_up/length(obs))
  return(rmse)
}

get_kge <- function(mods,obs){
  id1 <- !is.na(obs[,3]) 
  obs <- obs[id1,3]
  mods <- mods[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  kge <- KGE(obs, mods)
  return(kge)
}

get_kge_phy <- function(mods,obs){
  id1 <- !is.na(obs[,2]) 
  obs <- obs[id1,2]
  mods <- mods[id1,2]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
 kge <- KGE(obs, mods)
  return(kge)
}


get_nse_phy <- function(x, y){
  id1 <- !is.na(obs[,2]) 
  obs <- y[id1,2]
  mods <- x[id1,2]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2,na.rm = TRUE)
  sum_bottom <- sum((obs-mean(obs,na.rm = TRUE))^2,na.rm = TRUE)
  nse <- 1- sum_up/sum_bottom
  return(nse)
}
# mods <- modeled
# obs <- field
get_rmse_phy <- function(mods, obs){
  id1 <- !is.na(obs[,2]) 
  obs <- obs[id1,2]
  mods <- mods[id1,2]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2,na.rm = TRUE)
  rmse <- sqrt(sum_up/length(obs))
  return(rmse)
}

create_timeseries <- function(mods, obs, var, var_range, var_unit){
  for (ii in 1:length(unique(obs$DateTime))){
    obs_f <- obs %>%
      dplyr::filter(DateTime == unique(obs$DateTime)[ii]) 
    mods_f <- mods %>%
      dplyr::filter(DateTime == unique(obs$DateTime)[ii]) 
    nse <- get_nse(mods_f,obs_f)
    png(paste0('results/',var,'/',ii,'.png'), width = 300, height = 400)
    plot(obs_f[,3], obs_f$Depth, xlim = var_range, ylim = rev(range(obs_f$Depth)),  xlab = var_unit,
         ylab = 'Depth m',
         main =paste(unique(obs_f$DateTime),"NSE:",round(nse,2)))
    lines(mods_f[,3], mods_f$Depth)
    dev.off()
  }
  return()
}

# gotm_functions.R
diag.plots <- function(mod, obs, ggplot = T){
  stats = sum_stat(mod, obs, depth = T)
  if(max(mod[,2]) >= 0){ #Makes depths negative
    mod[,2] <- -mod[,2]
  }
  if(ggplot == F){
    dif = mod[,3] - obs[,3]
    par(mfrow=c(2,3))
    
    xfit <- seq(min(dif, na.rm = T), max(dif, na.rm = T), length=40)
    yfit_density <- dnorm(xfit, mean=mean(0, na.rm = T), sd=sd(dif, na.rm = T))
    
    # frequency
    h_freq <- hist(dif, breaks=50, col="blue", xlab="Model - Obs (C)", main='Histogram of residuals',probability = F, xlim = c(min(na.rm =T,  dif),max(na.rm =T,  dif)))
    yfit_freq <- yfit_density*diff(h_freq$mids[1:2])*length(dif)
    lines(xfit, yfit_freq, col="red",lty =2, lwd=2)
    mn <- round(mean(dif, na.rm =T),2)
    abline(v = mn,lty =2,lwd =2, col = 'green')
    std.dev <- round(sd(dif, na.rm =T),2)
    eqn <- bquote(Mean == .(mn) * "," ~~ S.D. == .(std.dev))
    Corner_text(eqn)
    
    plot(mod[,3], dif, cex = 0.5, pch ='.', main = 'Residuals vs. Modelled',
         ylab = 'Residuals', xlab = 'Modelled values')
    abline( h =0, col =2, lty =2)
    
    plot(mod[,1], dif, ylab = 'Time', xlab = 'Residuals', main = 'Residuals vs. Time', pch = '.')
    abline(h =0, lty =2, col =2)
    
    if(min(mod[,2]) >= 0){
      mod[,2] = -mod[,2]
    }
    plot(dif, mod[,2], ylim = range(mod[,2]), ylab = 'Depth (m)', xlab = 'Residuals', main = 'Residuals vs. Depth', pch = '.')
    abline(v =0, lty =2, col =2)
    
    plot(mod[,3], obs[,3], pch ='.', main = 'Obs vs. Mod', ylab = 'Obs',
         xlab ='Mod', ylim = range(mod[,3], obs[,3], na.rm =T), xlim = range(mod[,3], obs[,3], na.rm =T))
    abline(0,1, col =2, lty =2)
    eqn <- bquote(Pear_R == .(round(stats$Pearson_r,2)) * "," ~~ var.obs == .(round(stats$Variance_obs,2)) *
                    "," ~~ var.mod == .(round(stats$Variance_mod,2)) *  "," ~~ NSE == .(round(stats$NSE,2)))
    eqn2 <- bquote(cov == .(round(stats$Covariance,2)) * "," ~~ bias == .(round(stats$Bias,2)) *
                     "," ~~ MAE == .(round(stats$MAE,2)) * "," ~~ RMSE == .(round(stats$RMSE,2)))
    Corner_text(eqn)
    Corner_text(eqn2,location = 'bottomright')
    
    qqnorm(dif)
    abline(0,1, lty =2, col =2)
  }else{
    #ggplot2 version - put all variables in one dataframe
    mod$res <- mod[,3] - obs[,3]
    deps <- unique(mod[,2])
    deps <- deps[order(deps)]
    if(length(deps) < 10){
      lgd.sz = 4
    }else{
      lgd.sz =2
    }
    mod$fdepth <- factor(mod[,2], levels = as.character(deps))
    mod$obs <- obs[,3]
    
    mean.res = round(mean(mod$res, na.rm =T),2)
    med.res = round(median(mod$res, na.rm = T),2)
    std.dev = round(sd(mod$res, na.rm =T), 2)
    n = nrow(mod[!is.na(mod$res),])
    bw = 0.2
    min.res = min(mod$res, na.rm =T)
    max.res = max(mod$res, na.rm =T)
    
    # Create text to be added to plots
    grob1 <- grid::grobTree(grid::textGrob(paste0("Mean = ", mean.res,'; S.D = ', std.dev), x=0.5,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob2 <- grid::grobTree(grid::textGrob(paste0("Pear_R = ", round(stats$Pearson_r,2),'; v.obs = ', round(stats$Variance_obs,2),'; v.mod = ', round(stats$Variance_mod,2),'; NSE = ',round(stats$NSE,2)), x=0.05,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob3 <- grid::grobTree(grid::textGrob(paste0("cov = ", round(stats$Covariance,2),'; bias = ', round(stats$Bias,2),'; MAE = ', round(stats$MAE,2),'; RMSE = ',round(stats$RMSE,2)), x=0.05,  y=0.05, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    
    
    #Plots
    p1 <-ggplot(mod, aes(x = res)) + 
      geom_histogram(fill = "blue", colour = 'black', breaks = seq(min.res, max.res, bw)) + 
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        }, 
        args = c(mean = 0, sd = std.dev, n = n, bw = bw), colour = 'red', linetype = 'dashed', size = 1.2) + 
      scale_x_continuous("Model - Obs (C)")+
      scale_y_continuous("Frequency")+
      ggtitle('Histogram of residuals')+
      coord_cartesian(xlim = c(min(mod$res, na.rm = T),max(mod$res,na.rm =T)))+
      geom_vline(xintercept = med.res, colour = 'green', linetype = 'dashed', size = 1.2)+
      theme_bw()
    p1 <- p1 + annotation_custom(grob1)
    
    p2 <- ggplot(mod, aes_string(names(mod)[3], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Modelled values')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Modelled')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()
    
    p3 <- ggplot(mod, aes_string(names(mod)[1], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Time')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Time')+
      #scale_color_gradientn(colors = rev(my.cols), name = 'Depths')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=lgd.sz)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()#+
    #theme(legend.text=element_text(size= (lgd.sz*2.5)))
    
    p4 <- ggplot(mod, aes_string('res', names(mod)[2], colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Depth')+
      xlab('Residuals')+
      ggtitle('Residuals vs. Depth')+
      scale_color_discrete(name = 'Depths', guide = F)+
      geom_vline(xintercept = 0, linetype = 'dashed', size = 1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    
    
    p5 <- ggplot(mod,aes_string(names(mod)[3], 'obs', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Obs')+
      xlab('Modelled')+
      ggtitle('Obs vs. Mod')+
      scale_color_discrete(name = 'Depths', guide = F)+
      coord_cartesian(xlim = range(mod[,3], obs[,3], na.rm =T), ylim = range(mod[,3], obs[,3], na.rm =T))+
      geom_abline(slope = 1, intercept = 0, colour = 'black', linetype = 'dashed', size =1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    p5 <- p5 + annotation_custom(grob2) + annotation_custom(grob3)   
    
    p6 <- ggplot(mod, aes(sample = res))+
      stat_qq()+
      geom_abline(slope = 1, intercept = 0, size =1, linetype = 'dashed')+
      xlab('Sample Quantiles')+
      ylab('Theoretical Quantiles')+
      ggtitle('Normal Q-Q Plot')+
      theme_bw()
    
    g <- gridExtra::arrangeGrob(p1,p2,p3,p4,p5,p6, nrow = 2)
    gridExtra::grid.arrange(g)
    
    return(g)
  }
}


# gotmtools.R
sum_stat <- function(mod, obs, depth =F,na.rm =T, depth.range =NULL){
  if(depth == T){
    if(!is.null(depth.range)){
      obs = obs[(obs[,2] <= depth.range[1] & obs[,2] >= depth.range[2]),]
      mod = mod[(mod[,2] <= depth.range[1] & mod[,2] >= depth.range[2]),]
    }
    dif = mod[,3]- obs[,3]
    pear_r = cor.test(obs[,3], mod[,3], method = 'pearson')
    var_obs = mean(((obs[,3]-mean(obs[,3], na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod[,3]-mean(mod[,3], na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs[,3], na.rm = na.rm)
    SD_mod = sd(mod[,3], na.rm = na.rm)
    cov = mean((obs[,3]-mean(obs[,3], na.rm = na.rm))*(mod[,3]-mean(mod[,3], na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod[,3], obs[,3])
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }else{
    dif = mod- obs
    pear_r = cor.test(obs, mod, method = 'pearson')
    var_obs = mean(((obs-mean(obs, na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod-mean(mod, na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs, na.rm = na.rm)
    SD_mod = sd(mod, na.rm = na.rm)
    cov = mean((obs-mean(obs, na.rm = na.rm))*(mod-mean(mod, na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod, obs)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }
  
}

compare_depths <- function(obs, mods, dep, var,var_unit,mult){
  
  surftemp_obs <- obs %>%
    dplyr::filter(Depth == dep) %>%
    select(DateTime, var)
  
  surftemp_mods <- mods %>%
    dplyr::filter(Depth == dep) %>%
    select(DateTime, var)
  
  surftemp <- data.frame("DateTime" = surftemp_obs$DateTime, "obs" = surftemp_obs[,2], 
                         "mods" = surftemp_mods[,2] * 1/mult)
  surftemp <- reshape2::melt(surftemp, id.var = "DateTime")
  g <- ggplot(surftemp, aes(x = DateTime, y = value, col = variable)) +
    geom_line(aes(linetype = variable)) +
    ggtitle(paste0(var,' [',var_unit,'], ',dep," m", sep=''))+
    theme_bw() +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
  g <- ggplot()+
    geom_point(data=data.frame(cbind(surftemp$DateTime[surftemp$variable == 'mods'],surftemp$value[surftemp$variable == 'mods'])),aes(x=surftemp$DateTime[surftemp$variable == 'mods'], y=surftemp$value[surftemp$variable == 'mods'], col='mods')) +
    geom_point(data=data.frame(cbind(surftemp$DateTime[surftemp$variable == 'obs'],surftemp$value[surftemp$variable == 'obs'])),aes(x=surftemp$DateTime[surftemp$variable == 'obs'], y=surftemp$value[surftemp$variable == 'obs'], col='obs'))+
    xlab("Datetime") +
    ylab(paste0(var,' [',var_unit,']')) +
    ggtitle(paste0(var,' [',var_unit,'], ',dep," m", sep=''))+
    theme_bw() +
    theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
  return(g)
}


random_sampling <- function(x0,p,del,lb,ub){
  k <- length(x0)
  m <- k+1
  
  (B <- matrix(0, m, k))
  lower.tri(B)
  B[lower.tri(B)] <- 1
  
  D_star <- diag(plus_minus_one(k), k, k)
  J_mk <- matrix( 1, m, k)
  
  (1/2)*((2*B-J_mk)%*%D_star+J_mk)  
  
  x_star <- sample(random_value(p,del),k)
  
  P_star <- matrix(0,k,k)
  sample_P_star <- sample(k)
  for (j in 1:ncol(P_star)){
    P_star[sample_P_star[j],j] <- 1
  }
  
  J_mk_x_star <- matrix(rep(x_star, each = m),m,k)
  
  # J_mk %*% x_star
  
  B_star <- (J_mk_x_star+ (del/2) * ((2 * B - J_mk) %*% D_star + J_mk)) %*% P_star
  
  p = c(B_star[1,])#,lb,ub)
  
  #glmFUNsa(p)
  
  result_matrix <- matrix(0,nrow(B_star),1)
  for (j in 1:nrow(result_matrix)){
    result_matrix[j,]<- glmFUNsa(c(B_star[j,]))
  }
  
  ee <- matrix(0,ncol=k,nrow=1)
  for (i in 1:ncol(ee)){
    ind <- which(diff(B_star[,i])!=0)
    if (diff(B_star[,1])[ind] > 0){
      ee[i] <- (result_matrix[ind+1] - result_matrix[ind])/del}
    else {
      ee[i] <- (result_matrix[ind] - result_matrix[ind+1])/del
    }
  }
  return(ee)
}

glmFUNsa <- function(p){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  
  p <- wrapper_scales_sa(p, lb, ub)
  # test10 <- wrapper_scales(p, lb, ub)
  # test1 <- wrapper_scales_sa(p, lb, ub)
  # eval <- cbind(pars, lb, ub,p , test10, test1)
  # write.csv(eval, '/Users/robertladwig/Documents/lakemendota-glm/sens.csv',
  #           row.names = FALSE)
  
  eg_nml <- read_nml(nml_file = nml_file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm(os)
  
  mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  
  fit = sum((mod[,3] - obs[,3])^2,na.rm = T)
  
  print(paste('SAE', round(fit,1)))
  return(fit)
}

wrapper_scales <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(10)*(x)
  return(y)
}

wrapper_scales_sa <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(1)*(x)
  return(y)
}

plus_minus_one <- function(x){
  a <- runif(x,0,1)
  a[a<=1/2] <- -1
  a[a>1/2] <- 1
  return(a)}

random_value <- function(p,del){
  iter <- c()
  for (n in 1:((1-del)*(p-1))){
    iter <- cbind(iter,n/(p-1))  
  }
  return(c(0,iter))
}

glmFUN <- function(p){
  p <- wrapper_scales(p, lb, ub)
  eg_nml <- read_nml(nml_file)
  
  for(i in 1:nrow(calib)){
    eg_nml <- set_nml(eg_nml,pars[i],p[i])
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm(os)
  
  # uni.deps = unique(obs)
  # if(length(uni.deps) > 300){
  #   mod <- resample_to_field(out, 'temp.csv')
  # }else{
  mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  # }
  
  # if(metric == 'RMSE'){
  #   fit = rmse(mod[,3], obs[,3])
  # }else if(metric == 'lnlikelihood'){
  #   fit <- lnlike(mod[,3], obs[,3],sd = T)
  # }
  
  
  
  
  #print(paste(metric, fit))
  return(mod[,3])
}


glmFUNrmse <- function(p){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  
  p <- wrapper_scales(p, lb, ub)
  print(p)
  eg_nml <- read_nml(nml_file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  #print(paste(p))
  error <- try(run_glm(os))
  print(error)
  while (error != 0){
    error <- try(run_glm(os))
    print(error)
  }
  
  if (var == "PHY_TCHLA"){
    mod <- mod2obs_phy(mod_nc = out, obs = obs, reference = 'surface', var)
  } else {
    mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  }
  
  if (var == "PHY_TCHLA"){
   # fit = rmse(mod[,2], obs[,2])
    fit = KGE(mod[,2], obs[,2]) *(-1)
  } else{
    fit = rmse(mod[,3], obs[,3])}
  
  #Create a data frame to output each calibration attempt
  dat = data.frame(matrix(NA, ncol = (length(pars)+2), nrow = 1, dimnames = list(c(1), c('DateTime', pars, calib.metric))))
  dat[1,] = c(format(Sys.time()),p,fit)
  
  #Opens and writes a csv file with datetime, parameters,and fitness
  if (second.run == FALSE){
  if(!file.exists(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))){
    write.csv(dat,paste0('results/calib_results_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))
    df = rbind.data.frame(dat, df)
    write.csv(df,paste0('results/calib_results_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
  }
  } else {
    if(!file.exists(paste0('results/calib_results_2ndrun_',calib.metric,'_',var,'.csv'))){
      write.csv(dat,paste0('results/calib_results_2ndrun_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
    }else{
      df = read.csv(paste0('results/calib_results_2ndrun_',calib.metric,'_',var,'.csv'))
      df = rbind.data.frame(dat, df)
      write.csv(df,paste0('results/calib_results_2ndrun_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
    }
  }
  print(paste(calib.metric, fit))
  return(fit)
}

run_sensitivity <- function(var, max_r, x0, lb, ub, pars, obs, nml_file){
  calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
  
  all_ee <- matrix(0, nrow=max_r, ncol=length(x0))
  
  if (nrow(calib) %% 2 > 0){
    p <- nrow(calib) *2
  } else {
    p <- nrow(calib) *2
  }
  #p = nrow(calib) *2 # should be even
  del = p/(2*(p-1))
  
  # obs <- read_field_obs('field_data/field_mendota.csv', var)
  
  for (r in 1:max_r){
    ee <- random_sampling(x0,p,del,lb,ub)
    ee[is.na(ee)] <- ee[which.max(abs(ee))] #replace NA's with max value
    all_ee[r,] <- ee
  }
  
  ee_norm <- (abs(all_ee) - min(abs(all_ee)))/(max(abs(all_ee)) - min(abs(all_ee)))
  
  morris_res <- data.frame('pars'=c(pars), 'mean' = apply(abs(all_ee),2,mean), 'std' = apply(all_ee,2,sd))
  colnames(all_ee) <- pars
  write.csv(all_ee, paste0('results/SA_ee_results_',var,'.csv'), quote = F, row.names = F)
  morris_norm <- data.frame('pars'=c(pars), 'mean' = apply(abs(ee_norm),2,mean), 'std' = apply(ee_norm,2,sd))
  p6 <- ggplot(morris_norm, aes(pars,mean))+
    geom_bar(stat="identity", fill = 'blue')+
    geom_hline(yintercept = 0.1, colour = 'red', linetype = 'dashed')+
    ylab('Normalized Mean')+
    xlab('Parameters')+
    ggtitle('Normalised Sensitivity')+
    theme_bw()
  
  ggsave(file=paste0('results/SA_plot_',var,'.png'), p6, dpi = 300,width = 384,height = 216, units = 'mm') #saves g
  
  
  morris_cluster = morris_res[,c('mean','std')]
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:(nrow(morris_cluster)-1)
  mean_ss <- k.values*0
  for (k in k.values){
    km.res <- kmeans(morris_cluster, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(morris_cluster))
    mean_ss[k-1] <- mean(ss[, 3])
  }
  
  k_means <- kmeans(morris_cluster,k.values[index(max(mean_ss))])
  morris_res_clust <- data.frame(morris_res, "cluster" = k_means$cluster)
  
  p7 <- ggplot(morris_res_clust, aes(mean, std, col= factor(cluster), shape = pars))+
    geom_point()+
    scale_shape_manual(values = seq(0,19,1))+
    ylab('std EE')+
    xlab('mean EE')+
    ggtitle('Sensitivity')+
    theme_bw()
  ggsave(file=paste0('results/SA_plot_',var,'-clust.png'), p7, dpi = 300,width = 150,height = 150, units = 'mm') #saves g
  
  cal_pars = calib[c(which(morris_res_clust$cluster == row(k_means$centers)[k_means$centers==max(k_means$centers)])),]
  
  #Separate parameters to be calibrated and default parameters
  cal_pars = calib[c(which(morris_norm$mean >= 0.1)),]
  def_pars = calib[-c(which(morris_norm$mean >= 0.1)),]
  
  # cal_pars <- rbind(cal_pars, calib[17,])
  
  if (any(cal_pars$par %in% pars[duplicated(pars)])) {
    tt <- match(pars[duplicated(pars)],cal_pars$par,pars[duplicated(pars)])
    tt <- tt[!is.na(tt)]
    for (i in 1:length(tt)){
      id <- cal_pars[tt,]$par
      cal_pars <- cal_pars[-tt[i],]
      tt2 <-  which(id == calib$par)
      cal_pars<-rbind(cal_pars, calib[tt2,])
    }
  }
  
  # for(i in 1:length(pars[!duplicated(pars)])){
  #   if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
  #     eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
  #                       p[which(pars[!duplicated(pars)][i] == pars)])
  #   } else {
  #     eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
  #   }
  # }
  # 
  write.csv(cal_pars, paste0('calibration_file_',var,'.csv'), row.names = F, quote = F)
  
  return()
}

run_calibvalid <- function(var, var_unit, var_seq, cal_pars, pars, ub, lb, init.val, 
                           obs, method, calib.metric, os, target_fit, target_iter,nml_file, flag){
  
  
  #Reset original values OR calibrated values
  if (second.run == FALSE | var == 'temp'){
  for (p in flag){
    if (length(flag) == 0){
      file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
      file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
    } else {
      if (p == 'temp'){
        calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',p,'.csv'))
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('glm3.nml')
        # for(i in 2:(ncol(best_par)-1)){
        #   nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        # }
        
        check_duplicates <- c()
        for (i in 2:(ncol(best_par)-2)){
          string1 <- colnames(best_par)[i]
          for (j in (i+1):(ncol(best_par)-1)){
            string2 <- colnames(best_par)[j]
            if (substr(string1,1,floor(nchar(string1)/2)) == substr(string2,1,floor(nchar(string1)/2))){
              check_duplicates <- append(check_duplicates, i)
              check_duplicates <- append(check_duplicates, j)
            }
          }
        }
        checked <- 2:(ncol(best_par)-1)
        for (i in 1:length(check_duplicates)){
          checked <- checked[!checked == check_duplicates[i]]
        }
        
        for(i in checked){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        for (i in check_duplicates[1]){
          nml <- set_nml(nml,colnames(best_par)[i],as.numeric(best_par[1,check_duplicates]))
        }
        
        write_nml(nml, file = 'glm3.nml')
      } else {
        calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',p,'.csv'))
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('aed2/aed2.nml')
        for(i in 2:(ncol(best_par)-1)){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        write_nml(nml, file = 'aed2/aed2.nml')
      }
    }
  }
  } else if (second.run == TRUE & var != 'temp') {
    for (p in flag){
      if (length(flag) == 0){
        file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
        file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
      } else {
        if (p == 'temp'){
          calib <- read.csv(paste0('results/calib_results_2ndrun_',calib.metric,'_',p,'.csv'))
          eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
          nml <- read_nml('glm3.nml')
          # for(i in 2:(ncol(best_par)-1)){
          #   nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
          # }
          
          check_duplicates <- c()
          for (i in 2:(ncol(best_par)-2)){
            string1 <- colnames(best_par)[i]
            for (j in (i+1):(ncol(best_par)-1)){
              string2 <- colnames(best_par)[j]
              if (substr(string1,1,floor(nchar(string1)/2)) == substr(string2,1,floor(nchar(string1)/2))){
                check_duplicates <- append(check_duplicates, i)
                check_duplicates <- append(check_duplicates, j)
              }
            }
          }
          checked <- 2:(ncol(best_par)-1)
          for (i in 1:length(check_duplicates)){
            checked <- checked[!checked == check_duplicates[i]]
          }
          
          for(i in checked){
            nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
          }
          for (i in check_duplicates[1]){
            nml <- set_nml(nml,colnames(best_par)[i],as.numeric(best_par[1,check_duplicates]))
          }
          
          write_nml(nml, file = 'glm3.nml')
        } else {
          calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',p,'.csv'))
          eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
          nml <- read_nml('aed2/aed2.nml')
          for(i in 2:(ncol(best_par)-1)){
            nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
          }
          write_nml(nml, file = 'aed2/aed2.nml')
        }
      }
    }
  }
  # 
  # if (flag == 1){
  #   file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
  #   file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
  # } else if (flag == 2){
  #   file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
  # }
  
  calibration.list <- list("start" = '2005-01-01 12:00:00',
                           "stop" = '2015-12-30 12:00:00')
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = calibration.list)
  write_nml(nml, 'glm3.nml')
  
  glmOPT <- pureCMAES(init.val, glmFUNrmse, lower = rep(0,length(init.val)), 
                      upper = rep(10,length(init.val)), 
                      sigma = 0.5, 
                      stopfitness = target_fit, 
                      stopeval = target_iter)
  glmFUNrmse(glmOPT$xmin)
  
  #read in calibration data 
  if (second.run == FALSE){
  calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))
  eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
  write.csv(best_par, paste0('results/calib_par_',var,'.csv'), row.names = F, quote = F)
  
  best_par <- read.csv(paste0('results/calib_par_',var,'.csv'))}
  else {
    calib <- read.csv(paste0('results/calib_results_2ndrun_',calib.metric,'_',var,'.csv'))
    eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
    write.csv(best_par, paste0('results/calib_par_2ndrun_',var,'.csv'), row.names = F, quote = F)
    
    best_par <- read.csv(paste0('results/calib_par_2ndrun_',var,'.csv'))
  }
  
  #Input best parameter set
  nml <- read_nml(nml_file = nml_file)
  check_duplicates <- c()
  for (i in 2:(ncol(best_par)-2)){
    string1 <- colnames(best_par)[i]
    for (j in (i+1):(ncol(best_par)-1)){
      string2 <- colnames(best_par)[j]
      if (substr(string1,1,floor(nchar(string1)*9/10)) == substr(string2,1,floor(nchar(string1)*9/10))){
        check_duplicates <- append(check_duplicates, i)
        check_duplicates <- append(check_duplicates, j)
      }
    }
  }
  checked <- 2:(ncol(best_par)-1)
  for (i in 1:length(check_duplicates)){
    checked <- checked[!checked == check_duplicates[i]]
  }
  
  for(i in checked){
    nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  }
  
  if (!is.null(check_duplicates)){
  check_duplicates <- matrix(check_duplicates,ncol=2, byrow = TRUE)
  find_dupl_groups <- list()
  it <- 1
  for (ii in 1:nrow(check_duplicates)){
    if (ii == 1){
      find_dupl_groups[[it]] <- (check_duplicates[ii,])
    } else {
      if (ii > 1){
        if (any(check_duplicates[ii,] %in% find_dupl_groups[[it]])){
          place <- !(check_duplicates[ii,] %in% find_dupl_groups[[it]])
          find_dupl_groups[[it]]<-append(find_dupl_groups[[it]], check_duplicates[ii, which(place == TRUE)])
        } else {
          it <- it+1
          find_dupl_groups[[it]] <- (check_duplicates[ii,])
        } 
      }
    } }
  
  
  for (i in 1:length(find_dupl_groups)){
    nml <- set_nml(nml,
                   gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]],
                   as.numeric(best_par[find_dupl_groups[[i]]]))
    print(gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]])
    print(as.numeric(best_par[unlist(find_dupl_groups[[i]])]))
  }
  } else {
    for(i in 2:(ncol(best_par)-1)){
      nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
    }
  }
  
  # OLD CODE, still useful? or do the new lines totally replace it?
  # for (i in check_duplicates[1]){
  #   nml <- set_nml(nml,colnames(best_par)[i],as.numeric(best_par[1,check_duplicates]))
  # }
  # 
  
  # for(i in 2:(ncol(best_par)-1)){
  #   nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  # }
  
  
  write_nml(nml, file = nml_file)
  
  #Run GLM
  run_glm(os)
  if (var == "PHY_TCHLA"){
    h <- paste(filename,', RMSE',
               round(get_rmse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                              obs),2),var_unit,'NSE',
               round(get_nse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                             obs),2),sep=" ")
  } else{
  h <- paste(filename,', RMSE',
             round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                            obs),2),var_unit,'NSE',
             round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                           obs),2),sep=" ")}
  if (var == "PHY_TCHLA"){
    print(h)
  } else{
    png(paste0('results/',var,'_calibration',filename,'.png'), width = 1600, height = 900)
    plot_contour(mod_nc = out, reference = 'surface', h , var, var_unit,var_seq) 
    dev.off()}
  
  
  validation.list <- list("start" = '1995-05-09 12:00:00',
                          "stop" = '2004-12-31 12:00:00')
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = validation.list)
  write_nml(nml, 'glm3.nml')
  run_glm(os)
  if (var == "PHY_TCHLA"){
    h <- paste(filename,', RMSE',
               round(get_rmse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                  obs),2),var_unit,'NSE',
               round(get_nse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                 obs),2),sep=" ")
  } else{
    h <- paste(filename,', RMSE',
               round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                              obs),2),var_unit,'NSE',
               round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                             obs),2),sep=" ")}
  if (var == "PHY_TCHLA"){
    print(h)
  } else{
    png(paste0('results/',var,'wtemp_validation',filename,'.png'), width = 1600, height = 900)
    plot_contour(mod_nc = out, reference = 'surface', h , var,var_unit,var_seq) 
    dev.off()}
  
  
  total.list <- list("start" = '1979-01-02 12:00:00',
                     "stop" = '2015-12-3 12:00:00')
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = total.list)
  write_nml(nml, 'glm3.nml')
  run_glm(os)
  if (var == "PHY_TCHLA"){
    h <- paste(filename,', RMSE',
               round(get_rmse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                  obs),2),var_unit,'NSE',
               round(get_nse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                 obs),2),sep=" ")
  } else{
    h <- paste(filename,', RMSE',
               round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                              obs),2),var_unit,'NSE',
               round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                             obs),2),sep=" ")}
  if (var == "PHY_TCHLA"){
    print(h)
  } else{
    png(paste0('results/',var,'wtemp_total',filename,'.png'), width = 1600, height = 900)
    plot_contour(mod_nc = out, reference = 'surface', h , var, var_unit,var_seq) 
    dev.off()}
  
  g1 <- diag.plots(mod2obs(out, obs, reference = 'surface', var), obs)
  ggsave(file=paste0('results/mod_obs_',var,'totalperiod_',filename,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  
  
  
  
  return()
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

plot_contour_thermodep <- function(mod_nc, reference = "surface", h, var, unit, tlevels,
                                   td){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)
  
  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),0.1))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  if ((median(apply(sim.watdep,2,max,na.rm=TRUE))+median(apply(sim.watdep,2,sd,na.rm=TRUE)))<
      max(sim.watdep[,1],na.rm=TRUE)){
    max.plot.dep <- ceiling(median(apply(sim.watdep,2,max,na.rm=TRUE)))
  } else {
    max.plot.dep <-ceiling(max(sim.watdep[,1],na.rm=TRUE))
  }
  
  if (max.plot.dep<=20){iter.plot.dep = 1} else if (max.plot.dep<=50){iter.plot.dep = 5} else (iter.plot.dep = 10)
  
  spectral.colors <-  colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral') )
  
  inMeter <- function(x) {paste0(x, " m")}
  inCelsius <- function(x) {paste0(x, paste(" ",unit,sep=''))}
  plot(td$datetime, (-1)*td$thermo.depth, type = 'l', ylim = (range(int.dep*(-1))),ylab='Depth [m]',
       xlab = 'Datetime',cex.lab=1.5, cex.axis=1.5)
  .filled.contour(x=(td$datetime), 
                  y=(int.dep)*(-1), 
                  z = t(sim.wattemp),
                  levels=tlevels,
                  col=rev(spectral.colors(length(tlevels))))
  axis(1, labels=format(pretty(datetime,20), "%Y-%b"), at = as.numeric(pretty(datetime,20)), cex.axis=0.8,las=2)
  # axis(2, labels=inMeter(rev(seq(0,max.plot.dep,iter.plot.dep))*(-1)), at = rev(seq(0,max(max.plot.dep),iter.plot.dep))*(-1), cex.axis=1)
  lines(td$datetime, td$thermo.depth*(-1), col='black', lwd = 2, lty = 'dashed')
  #            key.axes = {axis(4,at=unique(tlevels),labels=(inCelsius(unique(tlevels))))})
}

duplicates <- function(data){
  dup_obs <- duplicated(data)
  which(dup_obs == TRUE)
  data[!duplicated(data),]
  
  how_many <- c()
  how_big <- c()
  # check multiple depths
  for (i in 2:nrow(data)){
    if (data$depth[i] == data$depth[i-1]){
      print(paste('PROBLEM! PROBLEM!',data$datetime[i-1],'is bad!'))
      how_many <- append(how_many, i)
      how_big <- append(how_big,
                        data$temp[i]-data$temp[i-1])
    } 
  }
  if (is.integer(how_many) == TRUE){
    data_cleaned <- data[-how_many, ] 
  } else {data_cleaned = data}
  
  return(data_cleaned)
}


get_best_sim <- function(flag, starttime, endtime, second.run){
  for (p in flag){
    if (length(flag) == 0){
      file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
      file.copy('../aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
    } else {
      if (p == 'temp'){
        if (second.run == TRUE){calib <- read.csv(paste0('../results/calib_results_2ndrun_',calib.metric,'_',p,'.csv'))
        } else {calib <- read.csv(paste0('../results/calib_results_',calib.metric,'_',p,'.csv'))}
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('glm3.nml')
        check_duplicates <- c()
        for (i in 2:(ncol(best_par)-2)){
          string1 <- colnames(best_par)[i]
          for (j in (i+1):(ncol(best_par)-1)){
            string2 <- colnames(best_par)[j]
            if (substr(string1,1,floor(nchar(string1)/2)) == substr(string2,1,floor(nchar(string1)/2))){
              check_duplicates <- append(check_duplicates, i)
              check_duplicates <- append(check_duplicates, j)
            }
          }
        }
        checked <- 2:(ncol(best_par)-1)
        for (i in 1:length(check_duplicates)){
          checked <- checked[!checked == check_duplicates[i]]
        }
        
        for(i in checked){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        for (i in check_duplicates[1]){
          nml <- set_nml(nml,colnames(best_par)[i],as.numeric(best_par[1,check_duplicates]))
        }
        write_nml(nml, file = 'glm3.nml')
      } else {
        if (second.run == TRUE && p == "OXY_oxy"){
          calib <- read.csv(paste0('../results/calib_results_2ndrun_',calib.metric,'_',p,'.csv'))
        } else {
          calib <- read.csv(paste0('../results/calib_results_',calib.metric,'_',p,'.csv'))
        }
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('aed2/aed2.nml')
        for(i in 2:(ncol(best_par)-1)){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        write_nml(nml, file = 'aed2/aed2.nml')
      }
    }
  }
  
  total.list <- list("start" = starttime,
                     "stop" = endtime,
                     "meteo_fl" = '../Inputs/NLDAS2_Mendota_1979_2016_cell_5_GLMReady_cut.csv',
                     "inflow_fl" = c("../Inputs/Mendota_yahara_30year2xP.csv",
                                     "../Inputs/Mendota_pheasant_branch_30year2xP.csv"),
                     "outflow_fl" = "../Inputs/Mendota_outflow_30year.csv",
                     "nsave" = 24)
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = total.list)
  write_nml(nml, 'glm3.nml')
}


#' Calculate water density from temperature
#'
#' Calculate water density from water temperature using the formula from (Millero & Poisson, 1981).
#'
#' @param wtemp vector or matrix; Water temperatures
#' @return vector or matrix; Water densities in kg/m3
#' @export
calc_dens <- function(wtemp){
  dens = 999.842594 + (6.793952 * 10^-2 * wtemp) - (9.095290 * 10^-3 *wtemp^2) + (1.001685 * 10^-4 * wtemp^3) - (1.120083 * 10^-6* wtemp^4) + (6.536336 * 10^-9 * wtemp^5)
  return(dens)
}


get_depth_spec_result <- function(var_dep, var, mult, var_unit, var_sim, var_name=NULL){
  if (var_dep == 0){var_dep = seq(0,4,0.5)}
  if (var == "OXY_oxy" || var == 'temp'){
    field <- read_field_obs('../field_data/field_mendota-timestamp.csv', var)
  }else { field <- read_field_obs('../field_data/field_chemistry-timestamp.csv', var)}
  field <- field %>%
    dplyr::filter(Depth == var_dep[1]) %>%
    dplyr::select(DateTime, var)
  if (length(var_sim)>1){
    modeled1 <- get_var(out, var_sim[1], reference = 'surface', z_out = var_dep)
    if (var_dep[1] == 0){modeled1 <- data.frame('DateTime' = modeled1$DateTime, 'Var' = apply(modeled1[,2:ncol(modeled1)],1,mean))}
    colnames(modeled1) <- c('DateTime', 'Var')
    modeled2 <- get_var(out, var_sim[2], reference = 'surface', z_out = var_dep)
    if (var_dep[1] == 0){modeled2 <- data.frame('DateTime' = modeled2$DateTime, 'Var' = apply(modeled2[,2:ncol(modeled2)],1,mean))}
    colnames(modeled2) <- c('DateTime', 'Var')
    modeled <- data.frame('DateTime' = modeled1$DateTime, 'Var' = modeled1$Var + modeled2$Var)
  } else {
    modeled <- get_var(out, var_sim, reference = 'surface', z_out = var_dep)
    if (var_dep[1] == 0){modeled <- data.frame('DateTime' = modeled$DateTime, 'Var' = apply(modeled[,2:ncol(modeled)],1,mean))}
  }
  colnames(field) <- c('DateTime', 'Var')
  colnames(modeled) <- c('DateTime', 'Var')
  lotsofids <- match(field$DateTime,modeled$DateTime)[!is.na(match(field$DateTime,modeled$DateTime))]
  mods <- data.frame('DateTime' = field$DateTime[1:length(lotsofids)], 'Var' = field$Var[1:length(lotsofids)])
  mods$Var <- modeled$Var[lotsofids] /mult
  obs <- field[1:length(lotsofids),]
  
  nse = 1- sum((mods$Var - obs$Var)^2,na.rm = TRUE)/(sum((obs$Var-mean(obs$Var,na.rm = TRUE))^2,na.rm=TRUE))
  print(nse)
  if (var == 'PHY_TCHLA'){
    h <- paste0('RMSE ',round(get_rmse_phy(mods,obs),2) ,var_unit,' NSE ',
                round(get_nse_phy(mods,obs),2),
                ' KGE ',round(get_kge_phy(mods,obs),2),sep=" ")
                }else {
  h <- paste0('RMSE ',
              round(get_rmse_phy(mods,obs),2) ,var_unit,' NSE ',
              round(get_nse_phy(mods,obs),2),
              ' KGE ',round(get_kge_phy(mods,obs),2),sep=" ")}
  print(h)
  print(round(get_nse_phy(mods,obs),2))
  if (!is.null(var_name)){
    name_string = var_name
  } else {
    name_string = var
  }
  g1 <- ggplot() +
    geom_line(data = modeled, aes(x = as.POSIXct(DateTime), y = Var * 1/mult), col = 'blue') +
    geom_point(data = field, aes(x = as.POSIXct(DateTime), y = Var), col = 'red') +
    xlab("Datetime") +
    ylab(paste0(name_string,' [',var_unit,']')) +
    ggtitle(paste0(name_string,' [',var_unit,'], ',var_dep," m: ",h, sep=''))+
    theme_bw() +
    theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
  g1
  ggsave(file=paste0('results/',var,'_',var_sim[1],'_',var_dep[1],'.png'), g1, dpi = 300,width = 484,height = 116, units = 'mm')
  
  return(g1)
}

check_calibvalid <- function(obs, var, var_unit, var_seq, var_mult){
  
  for (i in 1:3){
    
    if (i ==1){
      time.list <- list("start" = '2005-01-01 12:00:00',
                               "stop" = '2015-12-30 12:00:00')
      optperiod = 'Calibration'
  } else if (i == 2){
    time.list <- list("start" = '1995-05-09 12:00:00',
                            "stop" = '2004-12-31 12:00:00')
    optperiod = "Validation"
  } else if (i == 3){
    time.list <- list("start" = '1979-01-02 12:00:00',
                       "stop" = '2015-12-30 12:00:00')
    optperiod = "Total"
  }
  
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = time.list)
  write_nml(nml, 'glm3.nml')
  
  error <- try(run_glm(os))
  print(error)
  while (error != 0){
    error <- try(run_glm(os))
    print(error)
  }
  
  if (var == "PHY_TCHLA"){
    h <- paste0(optperiod,', RMSE ',
               round(get_rmse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                  obs),2) ,var_unit,' NSE ',
               round(get_nse_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                 obs),2),
               'KGE ',round(get_kge_phy(temp_mods <- mod2obs_phy(out, obs, reference = 'surface', var), 
                                    obs),2),sep=" ")
  } else{
    h <- paste0(optperiod,', RMSE ',
               round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                              obs),2) * var_mult,var_unit,' NSE ',
               round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                             obs),2),
               ' KGE ',round(get_kge(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                                        obs),2),sep=" ")}
  if (var == "PHY_TCHLA"){
    print(h)
    mod <- mod2obs_phy(out, obs, reference = 'surface', var)
    data <- data.frame('time' = mod$DateTime, 'obs' = obs$chla, 'mod' = mod$PHY_TCHLA)
    g <- ggplot(data, aes(obs, mod)) +
      geom_point() +
      theme_bw() +
      geom_abline(intercept = 0, slope = 1) +
      ggtitle(paste0(var,': ',h))
    ggsave(file=paste0('results/',optperiod,'_chla.png'), g, dpi = 300,width = 200,height = 200, units = 'mm')
    
    
  } else{
    png(paste0('results/contour_',var,'_',optperiod,'.png'), width = 1600, height = 900)
    plot_contour(mod_nc = out, reference = 'surface', h , var, var_unit,var_seq) 
    dev.off()}
  
  if (var != "PHY_TCHLA"){
    g1 <- diag.plots(mod2obs(out, obs, reference = 'surface', var), obs)
    ggsave(file=paste0('results/diag_',var,'_',optperiod,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  }
}
}

# stolen, psssst!
plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    #  geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density") +
    theme_bw() +
    theme(text = element_text(size = 10),
          legend.title=element_blank())
  plt + guides(fill=guide_legend(title=label_column))}

birgean.work.old<- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2

  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)

  A <- approx(bthD, bthA, dep)$y

  birg <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    birg <- append(birg, g/A[1] * trapz(dep[took],A[took] * (1000- data[took,ii]) * dep[took]))
  }
  return(data.frame('time' = x$time, 'B' = birg))
}
# 
# schmidt.work <- function(x, bthD, bthA){
#   rho = 998.2 # km/m3
#   g <- 9.81 # m/s2
#   
#   dep <- rev(x$depth)
#   data <- apply(x$sim,2,rev)
#   
#   A <- approx(bthD, bthA, dep)$y
#   
#   
#   
#   schmidt <- c()
#   for (ii in 1:length(x$time)){
#     took <- !is.na(data[,ii])
#     zv <- 1/500E6 * trapz(dep[took],A[took]* dep[took])
#     schmidt <- append(schmidt, g/A[1] * trapz(dep[took],A[took] *(1000- data[took,ii]) * (zv - dep[took])))
#   }
#   return(data.frame('time' = x$time, 'St' = schmidt))
# }
birgean.work <- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  birg <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), x$depth[took])
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    birg <- append(birg, g/A[1] * trapz(dep,A * (1000- inter.data$y) * dep))
  }
  return(data.frame('time' = x$time, 'B' = birg))
}
birgean.distribution <- function(x, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}
  
  birg <- matrix(NA,nrow = length(dep), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    #birg <- append(birg, g/A[1] * trapz(dep,A * (1000- inter.data$y) * dep))
    birg[,xi] <- g/A[1] * (A * (1000- inter.data$y) * dep)
    #g/A[1] * (A[6] * (1000- inter.data$y[6]) * dep[6])
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  if (doy != -1){
    colnames(birg) <- as.character(format(x$time[id.doy], '%Y'))
  } else {colnames(birg) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  dbirg <- data.frame(cbind('depths' = dep,birg))
  
  return(dbirg)
}

schmidt.work <- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  zv <- 1/479823720 * trapz(dep,A* dep) #  479823720 m3
  zv <- dep %*% A / sum(A)
  schmidt <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    
    schmidt <- append(schmidt, g/A[1] * trapz(dep,A *(1000- inter.data$y) * (zv - dep)))
  }
  return(data.frame('time' = x$time, 'St' = schmidt))
}


stability.index <- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  zv <- 1/479823720 * trapz(dep,A* dep) #  479823720 m3
  zv <- dep %*% A / sum(A)
  schmidt <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    
    schmidt <- append(schmidt, sum(inter.data$y * (dep - zv)))
  }
  return(data.frame('time' = x$time, 'St' = schmidt))
}

schmidt.work.zv <- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  zv <- 1/479823720 * trapz(dep,A* dep) #  479823720 m3
  schmidt <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    
    schmidt <- append(schmidt, g/A[1] * trapz(dep,A *(1000- inter.data$y) * (zv - dep)))
  }
  return(data.frame('time' = x$time, 'St' = schmidt))
}


schmidt.distribution <- function(x, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  zv <- 1/479823720 * trapz(dep,A* dep)
  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}
  
  schmidt <- matrix(NA,nrow = length(dep), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    
    #schmidt <- append(schmidt, g/A[1] * trapz(dep,A *(1000- inter.data$y) * (zv - dep)))
    schmidt[,xi] <- g/A[1] * (A * (1000- inter.data$y) * (zv - dep))
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  if (doy != -1){
    colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y'))
  } else {colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  dschmidt <- data.frame(cbind('depths' = dep,schmidt))

  # if (doy != -1){
  #   colnames(energy) <- as.character(format(x$time[id.doy], '%Y'))
  # } else {colnames(energy) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  # dschmidt <- list('depths' = dep,"time" = x$time,
  #                 'schmidt'=schmidt)
  return(dschmidt)
}

schmidt.distribution_input <- function(x, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  zv <- 1/479823720 * trapz(dep,A* dep)
  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}
  
  schmidt <- matrix(NA,nrow = length(dep), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    
    #schmidt <- append(schmidt, g/A[1] * trapz(dep,A *(1000- inter.data$y) * (zv - dep)))
    schmidt[,xi] <- g/A[1] * (A * (1000- inter.data$y) * (zv - dep))
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  # if (doy != -1){
  #   colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y'))
  # } else {colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  # dschmidt <- data.frame(cbind('depths' = dep,schmidt))
  
  # if (doy != -1){
  #   colnames(energy) <- as.character(format(x$time[id.doy], '%Y'))
  # } else {colnames(energy) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  dschmidt <- list('depths' = dep,"time" = x$time,
                  'schmidt'=schmidt)
  return(dschmidt)
}
energy.distribution <- function(x, temp, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  cw = 4186 #J kg-1 degK-1 
  
  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)
  
  temp =  apply(temp$sim,2,rev)
  
  numD = nrow(data)
  if(max(bthD) > dep[numD]){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < dep[numD]) {
    bthD = c(bthD, dep[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  
  #zv <- 1/sum(A)/mean(abs(diff(dep))) * trapz(dep,A* dep)
  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}
  
  energy <- matrix(NA,nrow = length(A[-length(A)]), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[took,ii])
      appr.temp <- c(temp[took,ii][1], temp[took,ii])
      appr.dep <- c(min(dep), rev(x$depth[took]))
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(appr.data)])
      appr.temp <- c(appr.temp, appr.temp[length(appr.temp)])
      appr.dep <- c(appr.dep, max(dep))
    }
    inter.data <- stats::approx(appr.dep, appr.data, dep)
    inter.temp <- stats::approx(appr.dep, appr.temp, dep)
    
    #schmidt <- append(schmidt, g/A[1] * trapz(dep,A *(1000- inter.data$y) * (zv - dep)))
    energy[,xi] <- 1/A[1] * ( ( (A[-length(A)]*diff(dep)) * inter.data$y[-length(A)] ) * cw * inter.temp$y[-length(A)])
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  # if (doy != -1){
  #   colnames(energy) <- as.character(format(x$time[id.doy], '%Y'))
  # } else {colnames(energy) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  denergy <- list('depths' = dep[-length(A)],"time" = x$time,
                              'energy'=energy)
  
  return(denergy)
}



birgean.distribution.old <- function(x, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2

  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)

  A <- approx(bthD, bthA, dep)$y

  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}

  birg <- matrix(NA,nrow = length(x$depth), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    birg[took,xi] <- g/A[1] * (A[took] * (1000- data[took,ii]) * dep[took])
    g/A[1] * (A[5] * (1000- data[5,ii]) * dep[5])
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  if (doy != -1){
    colnames(birg) <- as.character(format(x$time[id.doy], '%Y'))
  } else {colnames(birg) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  dbirg <- data.frame(cbind('depths' = dep,birg))

  return(dbirg)
}

schmidt.distribution.old <- function(x, bthD, bthA, doy){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2

  dep <- rev(x$depth)
  data <- apply(x$sim,2,rev)

  A <- approx(bthD, bthA, dep)$y

  if (doy != -1){
    all.doy <- yday(x$time)
    id.doy <- which(all.doy == doy)
  } else {id.doy <- c(1:length(x$time))}

  schmidt <- matrix(NA,nrow = length(x$depth), ncol = length(id.doy))
  for (ii in id.doy){
    if (ii == id.doy[1]){xi <- 1}
    took <- !is.na(data[,ii])
    zv <- 1/500E6 * trapz(dep[took],A[took]* dep[took])
    schmidt[took,xi] <- g/A[1] * (A[took] * (1000- data[took,ii]) * (zv - dep[took]))
    #birg <- append(birg, g/A[1] * trapz(A[took] * (1000- data[took,ii]) * dep[took]))
    xi <- xi+1
  }
  if (doy != -1){
    colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y'))
  } else {colnames(schmidt) <- as.character(format(x$time[id.doy], '%Y-%m-%d'))}
  dschmidt <- data.frame(cbind('depths' = dep,schmidt))

  return(dschmidt)
}


