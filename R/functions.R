
###Launch shiny
launch_MAPPA <- function(x, ...)
{
  shiny::runApp(appDir = system.file("shiny_application", package = "MAPPA"),
                ...)
}


###convert attributes from PhaseX to MAPPA
func_conv_phase_MAPPA <- function(obj){
  names(attributes(obj))[1] <- "PPM.name"
  attributes(obj)$class[1] <- "PPM"
  attributes(attributes(obj)$class )$package <- "MAPPA"
  return(obj)
}


###Create a PPM object
func_create_PPM <- function(PPM.name){
  return(methods::new(Class = "PPM", PPM.name))
}


##generate parameter grids
func_gen_prm_grids <- function(prm.ranges){
  max.num.grids = max(prm.ranges$"number of grids")
  prm.grids = data.frame(matrix(NA, nrow = nrow(prm.ranges), ncol = max.num.grids + 1))
  colnames(prm.grids) = c("names", "min", paste0("V",2:(max(prm.ranges$"number of grids")-1)), "max")
  prm.grids$names = prm.ranges$names

  for(i in 1:nrow(prm.ranges)){
    if(prm.ranges$log.scale[i] == TRUE){
      grids.temp = exp(log(prm.ranges$min[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))/(prm.ranges$"number of grids"[i]-1)*(1:prm.ranges$"number of grids"[i]-1))
      #print(grids.temp)
      prm.grids[i,c(1+1,max.num.grids+1)] = grids.temp[c(1,prm.ranges$"number of grids"[i])]
      if(prm.ranges$"number of grids"[i] >2){
        prm.grids[i,(2+1):(prm.ranges$"number of grids"[i])] = grids.temp[2:(prm.ranges$"number of grids"[i]-1)]
      }

    }else{
      grids.temp = prm.ranges$min[i] + (prm.ranges$max[i]-prm.ranges$min[i])/(prm.ranges$"number of grids"[i]-1)*(1:prm.ranges$"number of grids"[i]-1)
      prm.grids[i,c(1+1,max.num.grids+1)] = grids.temp[c(1,prm.ranges$"number of grids"[i])]
      if(prm.ranges$"number of grids"[i] >2){
        prm.grids[i,(2+1):(prm.ranges$"number of grids"[i])] = grids.temp[2:(prm.ranges$"number of grids"[i]-1)]
      }
    }

  }

  prm.grids[,2:ncol(prm.grids)] = signif( prm.grids[,2:ncol(prm.grids)],6)
  return(prm.grids)
}


##generate parameter keys
#sourceCpp("prm_gen_R.cpp")


##generate parameter combinations (pseudorandom, sobol', latin hypercube)
func_gen_prm_combs <- function(prm.ranges, prm.comb.num, sampling.meth, prm.grids = NULL , continue = FALSE, count = NULL, prm.ranges.org = NULL){
  if(sampling.meth == "unif_grid"){
    if(!is.null(prm.grids)){
      prm.combinations = data.frame(matrix(NA, nrow = prm.comb.num, ncol = nrow(prm.ranges)))
      colnames(prm.combinations) = prm.ranges$names
      set.seed(1)
      if(continue == TRUE && !is.null(count) && count != 0){
        for(i in 1:nrow(prm.ranges)){
          sample.int(n = prm.ranges$"number of grids"[i], replace = T, size = count)
        }
      }
      for(i in 1:nrow(prm.ranges)){
        prm.combinations[,i] = sample.int(n = prm.ranges$"number of grids"[i], replace = T, size = prm.comb.num)
        prm.combinations[prm.combinations[,i] == prm.ranges$"number of grids"[i] ,i] = max(prm.ranges$"number of grids")
        vec.temp = as.numeric(prm.combinations[,i])
        prm.combinations[,i] = t(prm.grids[i,vec.temp+1])
        prm.combinations[,i] = signif(prm.combinations[,i],digits = 6)
      }
      return(list(prm.combs =prm.combinations)) # from global.R
      #return(prm.combinations)
    }
  }else if(sampling.meth == "pseudorandom"){
    prm.combs.pseudorand = matrix(NA, nrow = prm.comb.num, ncol = nrow(prm.ranges))
    colnames(prm.combs.pseudorand) = prm.ranges$names
    prm.combinations = prm.combs.pseudorand
    prm.combinations.z = prm.combs.pseudorand
    set.seed(1)
    if(continue == TRUE && !is.null(count) && count != 0){

      stats::runif(count*nrow(prm.ranges))
    }

    for(i in 1:nrow(prm.ranges)){
      prm.combs.pseudorand[,i] = stats::runif(prm.comb.num)
      prm.combinations[,i] = prm.combs.pseudorand[,i]
      prm.combinations.z[,i] = prm.combs.pseudorand[,i]

      if(prm.ranges$log.scale[i] == TRUE){
        prm.combinations[,i] = exp(log(prm.ranges$min[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.combinations[,i])
      }else{
        prm.combinations[,i] = prm.ranges$min[i] + (prm.ranges$max[i]-prm.ranges$min[i])*prm.combinations[,i]
      }
      prm.combinations[,i] = signif(prm.combinations[,i],digits = 6)
      prm.combinations.z[,i] = signif(prm.combinations.z[,i],digits = 6)
    }


    if(continue == TRUE && !is.null(count) && count ==0){
      prm.combinations.z = scale(prm.combinations.z)
      return(list(raw.smpl = prm.combs.pseudorand, prm.combs = prm.combinations, prm.combs.z = prm.combinations.z))
    }else {
      return(list(prm.combs = prm.combinations))
    }


  }else if(sampling.meth == "sobol'"){
    set.seed(1)

    if(continue == TRUE && !is.null(count) && count != 0 ){
      prm.combs.sobol = randtoolbox::sobol(count + prm.comb.num, dim=nrow(prm.ranges), scrambling = 3)
      prm.combs.sobol <- prm.combs.sobol[(count+1):(count+prm.comb.num),]
    }else{
      prm.combs.sobol = randtoolbox::sobol(prm.comb.num, dim=nrow(prm.ranges), scrambling = 3)
    }


    colnames(prm.combs.sobol) = prm.ranges$names
    prm.combinations = prm.combs.sobol
    prm.combinations.z = scale(prm.combs.sobol)

    colnames(prm.combinations) = prm.ranges$names
    for(i in 1:nrow(prm.ranges)){
      if(prm.ranges$log.scale[i] == TRUE){
        prm.combinations[,i] = exp(log(prm.ranges$min[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.combinations[,i])
        prm.combinations.z[,1]
      }else{
        prm.combinations[,i] = prm.ranges$min[i] + (prm.ranges$max[i]-prm.ranges$min[i])*prm.combinations[,i]
      }
      prm.combinations[,i] = signif(prm.combinations[,i],digits = 6)
      prm.combinations.z[,i] = signif(prm.combinations.z[,i],digits = 6)
    }

    if(continue == TRUE && !is.null(count) && count ==0){
      return(list(raw.smpl = prm.combs.sobol, prm.combs = prm.combinations, prm.combs.z = prm.combinations.z))
      # }
      # else if(continue == FALSE){
      #   bff <- NULL
      #   for(i in 1:nrow(prm.combinations)){
      #
      #     temp.logic <- c(apply(cbind(prm.combinations[i,],prm.ranges.org$min),1, function(input){input[1] < input[2]}),
      #                     apply(cbind(prm.combinations[i,],prm.ranges.org$max),1, function(input){input[1] > input[2]}))
      #     if(any(temp.logic == TRUE)){
      #       print(table(temp.logic))
      #     }else{
      #       bff = rbind(bff,prm.combinations[i,])
      #     }
      #   }
      #   prm.combinations <- bff
      #   return(list(prm.combs = prm.combinations))
    }else{
      return(list(raw.smpl = prm.combs.sobol, prm.combs = prm.combinations, prm.combs.z = prm.combinations.z))
    }

  }else if(sampling.meth == "latin_hyp"){
    ##warning don't try optimumLHS for prm.comb.num > 100,
    set.seed(1)
    if(continue == TRUE && !is.null(count) && count != 0 ){
      prm.combs.lhs = lhs::randomLHS(count, nrow(prm.ranges))
      prm.combs.lhs = lhs::augmentLHS(prm.combs.lhs, prm.comb.num)
      prm.combs.lhs = prm.combs.lhs[(count+1):(count+prm.comb.num),]
    }else{
      prm.combs.lhs = lhs::randomLHS(prm.comb.num, nrow(prm.ranges))
    }
    #prm.combinations = data.frame(optimumLHS(prm.comb.num, nrow(prm.ranges), 10))

    colnames(prm.combs.lhs) = prm.ranges$names
    prm.combinations =  prm.combs.lhs
    prm.combinations.z = scale(prm.combs.lhs)

    for(i in 1:nrow(prm.ranges)){
      if(prm.ranges$log.scale[i] == TRUE){
        prm.combinations[,i] = exp(log(prm.ranges$min[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.combinations[,i])
      }else{
        prm.combinations[,i] = prm.ranges$min[i] + (prm.ranges$max[i]-prm.ranges$min[i])*prm.combinations[,i]
      }
      prm.combinations[,i] = signif(prm.combinations[,i],digits = 6)
      prm.combinations.z[,i] = signif(prm.combinations.z[,i],digits = 6)
    }
    if(continue == TRUE && !is.null(count) && count ==0){
      return(list(raw.smpl = prm.combs.lhs, prm.combs = prm.combinations, prm.combs.z = prm.combinations.z))
    }else{
      return(list( prm.combs = prm.combinations))
    }


  }

}


##09152017 subranges centered on each selected parameter combination for zoom-in sampling
#prm.comb: a selected parameter combination
#prm.ranges: whole parameter ranges
func_gen_prm_subranges <- function(prm.comb, prm.ranges, sampling.meth, prm.grids = NULL) {
  #fraction of whole ranges
  prm.ranges$frac.range

  subranges = data.frame(matrix(NA, nrow = nrow(prm.ranges), ncol = 4))
  names(subranges) = c("names", "min", "max", "log.scale")
  subranges$log.scale = prm.ranges$log.scale
  subranges$names = prm.ranges$names

  if(sampling.meth != "unif_grid"){
    for(i in 1:nrow(prm.ranges)){
      #print(prm.comb[1])
      if(prm.ranges$log.scale[i] == TRUE){
        #print(log(prm.comb[i]))
        #print((log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.ranges$frac.range[i]/2)
        subranges[i,"min"] = log(prm.comb[i]) - (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.ranges$frac.range[i]
        subranges[i,"max"] = log(prm.comb[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*prm.ranges$frac.range[i]
        subranges[i,"min"] = exp(subranges[i,"min"])
        subranges[i,"max"] = exp(subranges[i,"max"])
        if(subranges[i,"min"]< prm.ranges$min[i]){
          subranges[i,"min"] = prm.ranges$min[i]
        }
        if(subranges[i,"max"]> prm.ranges$max[i]){
          subranges[i,"max"] = prm.ranges$max[i]
        }
      }else{
        # print(prm.comb[i])
        # print(prm.ranges[i,])
        # print((prm.ranges$max[i]-prm.ranges$min[i]))
        # print(prm.ranges$frac.range[i]/2)
        subranges[i,"min"] = prm.comb[i] -  (prm.ranges$max[i]-prm.ranges$min[i])*prm.ranges$frac.range[i]
        subranges[i,"max"] = prm.comb[i] +  (prm.ranges$max[i]-prm.ranges$min[i])*prm.ranges$frac.range[i]
        if(subranges[i,"min"]< prm.ranges$min[i]){
          subranges[i,"min"] = prm.ranges$min[i]
        }
        if(subranges[i,"max"]> prm.ranges$max[i]){
          subranges[i,"max"] = prm.ranges$max[i]
        }
      }
    }
  }else{
    for(i in 1:nrow(prm.ranges)){
      temp.grids <- prm.grids[i,!is.na(prm.grids[i,])]
      idx.temp = which(temp.grids == prm.comb[,i])

      if(temp.grids$max != temp.grids$min){

        #min
        if(names(temp.grids)[idx.temp] != "min"){

          #unit interval
          temp.min = temp.grids[idx.temp-1]

          if(prm.ranges$log.scale[i] == TRUE){
            temp.min.delta = log(prm.comb[i]) - log(temp.min)
            subranges[i,"min"] = log(prm.comb[i]) - temp.min.delta*prm.ranges$frac.range[i]
            subranges[i,"min"] = exp(subranges[i,"min"])
          }else{
            temp.min.delta = prm.comb[i] - temp.min
            subranges[i,"min"] = prm.comb[i] - temp.min.delta*prm.ranges$frac.range[i]
          }
        }else{
          subranges[i,"min"] = prm.comb[i]
        }

        #max
        if(names(temp.grids)[idx.temp] != "max"){
          if(!is.na(temp.grids[idx.temp+1])){
            temp.max = temp.grids[idx.temp+1]

            if(prm.ranges$log.scale[i] == TRUE){
              temp.max.delta = log(prm.comb[i]) - log(temp.max)
              subranges[i,"max"] = log(prm.comb[i]) - temp.max.delta*prm.ranges$frac.range[i]
              subranges[i,"max"] = exp(subranges[i,"max"])
            }else{
              temp.max.delta = prm.comb[i] - temp.max
              subranges[i,"max"] = prm.comb[i] - temp.max.delta*prm.ranges$frac.range[i]
            }

          }else{
            subranges[i,"max"] = temp.grids["max"]
          }
        }else{
          subranges[i,"max"] = prm.comb[i]
        }
      }else{
        subranges[i,"min"] = prm.comb[i]
        subranges[i,"max"] = prm.comb[i]
      }

      if(subranges[i,"min"]< prm.ranges$min[i]){
        subranges[i,"min"] = prm.ranges$min[i]
      }
      if(subranges[i,"max"]> prm.ranges$max[i]){
        subranges[i,"max"] = prm.ranges$max[i]
      }
      subranges$"number of grids" = prm.ranges$"number of grids"
    }
  }


  subranges[,c("min", "max")] = signif(subranges[,c("min", "max")] ,digits = 6)

  return(subranges)


}




####phasespace

#### Modified from a code by  http://arxiv.org/abs/1605.09196 under license GLP-2
vec.plot.bc.mod = function (model1, model2 = NULL, X, i.var, prm.ranges ,grid.lines = 100,
                            zoom = 1, limitY = F, zlim, gap, three.dim = T, posit.class = NULL, pred.type = NULL, cut.off = NULL, moreArgs = list(), ...) {
  #library(scales)
  d = length(i.var)

  scales = lapply(i.var, function(i) {
    rXi = range(prm.ranges[,i])
    span = abs(rXi[2] - rXi[1]) * zoom/2
    center = mean(rXi)
    seq(center - span, center + span, length.out = grid.lines)
  })

  anchor.points = as.matrix(expand.grid(scales), dimnames = NULL)
  #colnames(X) = colnames(prm.ranges)
  names(X) = colnames(prm.ranges)
  Xgeneralized = as.numeric(X)
  Xtest.vec = data.frame(t(replicate(dim(anchor.points)[1],  Xgeneralized)))
  names(Xtest.vec) = colnames(prm.ranges)
  Xtest.vec[, i.var] = anchor.points

  if(model1$type == "regression" & !is.null(model2)){
    yhat.vec = stats::predict(model1, Xtest.vec) - stats::predict(model2, Xtest.vec)
    yhat.pt = stats::predict(model1,Xgeneralized) - stats::predict(model2, Xgeneralized)
  }else if(model1$type == "regression" & is.null(model2)){
    yhat.vec = stats::predict(model1, Xtest.vec)
    yhat.pt = stats::predict(model1,Xgeneralized)
  }else if(model1$type == "classification"){
    yhat.vec = stats::predict(model1, Xtest.vec, "prob")
    yhat.vec = yhat.vec[,posit.class]
    yhat.pt = stats::predict(model1,Xgeneralized, "prob")
    yhat.pt = yhat.pt[,posit.class]

    if(pred.type == "Binary"){
      yhat.vec[yhat.vec >= cut.off] = 1
      yhat.vec[yhat.vec < cut.off] = 0
      yhat.pt[yhat.pt >= cut.off] = 1
      yhat.pt[yhat.pt < cut.off] = 0
    }
  }


  values.to.plot =X[ i.var]

  if(is.null(zlim)){
    zlim <- c(0,1)
  }

  if (d == 2) {
    color.gradient <- function(x, colors=c("blue", "green","yellow","red"), colsteps=100) {
      return( grDevices::colorRampPalette(colors) (colsteps) [ findInterval(x, seq(zlim[1],zlim[2], length.out=colsteps)) ] )
    }

    if(three.dim == T){

      rgl::plot3d(x = anchor.points[,1], y = anchor.points[,2], z = yhat.vec, xlab =i.var[1], ylab =i.var[2],
             main = "Prediction", zlim =zlim, zlab = "Phenotype")
      rgl::plot3d(x = values.to.plot[1], y = values.to.plot[2], z = yhat.pt + abs(gap*yhat.pt), xlab = i.var[1], ylab = i.var[2],
             main = "Prediction", col = "red", size = 7, add = TRUE, zlim = zlim)
      rgl::surface3d(x = scales[[1]], y = scales[[2]],
                z = yhat.vec, col = color.gradient(yhat.vec), size = 4, alpha = 0.4, zlim = zlim)


    }else{

      plot3D::image2D( matrix(yhat.vec, nrow = grid.lines), x = scales[[1]], y = scales[[2]], contour = T, zlim = zlim, xlab = i.var[1], ylab =i.var[2]  )
      graphics::points(x = values.to.plot[1], y = values.to.plot[2], pch =  19 )


    }

  }
  # else {
  #   if (limitY) {
  #     ylim = range(model$y)
  #   }
  #   else {
  #     ylim = NULL
  #   }
  #   plotArgs.std = alist(x = scales[[1]], y = yhat.vec, type = "l",
  #                        xlab = names(X)[i.var][1], col = "red", ylim = ylim)
  #   plotArgs.all = append.overwrite.alists(list(...), plotArgs.std)
  #   do.call(plot, plotArgs.all)
  #   pointsArgs.std = alist(x = values.to.plot, y = yhat.obs,
  #                          col = "#DD202030")
  #   pointsArgs.all = append.overwrite.alists(moreArgs, pointsArgs.std)
  #   do.call(points, pointsArgs.all)
  # }

  return(Xtest.vec)
}


fun.scale.conv = function(sample_meth, prm.ranges, raw.smpl = NULL, prm.grids = NULL, prm.combs, z.to.org = TRUE){
  temp.num.prm = nrow(prm.ranges)

  temp.num.grids = prm.ranges$"number of grids"
  temp.prm.combs.val.z = data.frame(prm.combs)
  temp.prm.combs.val = data.frame(prm.combs)
  temp.prm.combs.val.raw = data.frame(prm.combs) # for pseudorandom, latinhypercube, sobol

  if(z.to.org == TRUE){
    if(sample_meth == "unif_grid"){
      for(i in 1:temp.num.prm){
        if(prm.ranges$log.scale[i] == FALSE){
          temp.mean = mean(as.numeric(prm.grids[i,c(names(prm.grids)[c(2:temp.num.grids[i])], "max")]),na.rm = T)
          temp.std = stats::sd(prm.grids[i,c(names(prm.grids)[c(2:temp.num.grids[i])], "max")],na.rm = T)
          temp.prm.combs.val[,i] =  temp.prm.combs.val.z[,i]*temp.std + temp.mean

        }else{
          temp.mean = mean(as.numeric(log(prm.grids[i,c(names(prm.grids)[c(2:temp.num.grids[i])], "max")])),na.rm = T)
          temp.std = stats::sd(log(prm.grids[i,c(names(prm.grids)[c(2:temp.num.grids[i])], "max")]),na.rm = T)
          temp.prm.combs.val[,i] =  exp(temp.prm.combs.val.z[,i]*temp.std + temp.mean)
        }
      }
      names(temp.prm.combs.val) <- prm.ranges$names
      return(temp.prm.combs.val)
    }else{
      ##for other sampling schemes
      for(i in 1:temp.num.prm){
        if(prm.ranges$log.scale[i] == FALSE){
          temp.mean = mean(raw.smpl[,i+1])
          temp.std = stats::sd(raw.smpl[,i+1])
          temp.prm.combs.val.raw[,i] <- temp.prm.combs.val.z[,i]*temp.std + temp.mean
          temp.prm.combs.val[,i] = prm.ranges$min[i] + (prm.ranges$max[i]-prm.ranges$min[i])*temp.prm.combs.val.raw[,i]

        }else{
          temp.mean = mean(raw.smpl[,i+1])
          temp.std = stats::sd(raw.smpl[,i+1])
          temp.prm.combs.val.raw[,i] <- temp.prm.combs.val.z[,i]*temp.std + temp.mean
          temp.prm.combs.val[,i] = exp(log(prm.ranges$min[i]) + (log(prm.ranges$max[i])-log(prm.ranges$min[i]))*temp.prm.combs.val.raw[,i])
        }
      }
      names(temp.prm.combs.val) <- prm.ranges$names
      temp.prm.combs.val <- signif(temp.prm.combs.val, digits = 6)
      return(temp.prm.combs.val)
    }

  }else if(z.to.org == FALSE){
    if(sample_meth == "unif_grid"){
      for(i in 1:temp.num.prm){
        if(prm.ranges$log.scale[i] == FALSE){
          temp.mean = mean(as.numeric(prm.grids[i,c(2:(temp.num.grids[i]),max(temp.num.grids+1))]),na.rm = T)
          temp.std = stats::sd(prm.grids[i,c(2:(temp.num.grids[i]),max(temp.num.grids+1))],na.rm = T)

          if(temp.std != 0){
            temp.prm.combs.val.z[,i] =  (temp.prm.combs.val[,i] - temp.mean)/temp.std
          }else{
            temp.prm.combs.val.z[,i] =  temp.prm.combs.val[,i] - temp.mean
          }

        }else{
          temp.mean = mean(as.numeric(log(prm.grids[i,c(2:(temp.num.grids[i]),max(temp.num.grids+1))])),na.rm = T)

          temp.std = stats::sd(log(prm.grids[i,c(2:(temp.num.grids[i]),max(temp.num.grids+1))]),na.rm = T)

          if(temp.std != 0){
            temp.prm.combs.val.z[,i] =  (log(temp.prm.combs.val[,i]) - temp.mean)/temp.std
          }else {
            temp.prm.combs.val.z[,i] =  log(temp.prm.combs.val[,i]) - temp.mean
          }
        }
      }
      names(temp.prm.combs.val.z) <- prm.ranges$names
      return(temp.prm.combs.val.z)

    }else{ #for other parameter scheme
      for(i in 1:temp.num.prm){
        if(prm.ranges$log.scale[i] == FALSE){
          temp.mean = mean(raw.smpl[,i+1])
          temp.std = stats::sd(raw.smpl[,i+1])

          temp.prm.combs.val.raw[,i] <- (temp.prm.combs.val[,i] - prm.ranges$min[i])/(prm.ranges$max[i]-prm.ranges$min[i])

          if(temp.std != 0){
            temp.prm.combs.val.z[,i] =  (temp.prm.combs.val.raw[,i] - temp.mean)/temp.std
          }else{
            temp.prm.combs.val.z[,i] =  temp.prm.combs.val.raw[,i] - temp.mean
          }

        }else{
          temp.mean = mean(raw.smpl[,i+1])
          temp.std = stats::sd(raw.smpl[,i+1])
          temp.prm.combs.val.raw[,i] <- (log(temp.prm.combs.val[,i]) - log(prm.ranges$min[i]))/ (log(prm.ranges$max[i])-log(prm.ranges$min[i]))

          if(temp.std != 0){
            temp.prm.combs.val.z[,i] =  (temp.prm.combs.val.raw[,i] - temp.mean)/temp.std
          }else {
            temp.prm.combs.val.z[,i] =  temp.prm.combs.val.raw[,i] - temp.mean
          }
        }
      }
      names(temp.prm.combs.val.z) <- prm.ranges$names
      temp.prm.combs.val.z <- signif(temp.prm.combs.val.z, digits = 6)

      return(temp.prm.combs.val.z)
    }


  }
}


rocpr = function(rf.obj,X.test = NULL, ref = NULL, positive){
  #library(randomForest)
  if(!is.null(X.test)){
    p.prob = stats::predict(rf.obj, X.test, type = "prob")
  }else{
    p.prob = rf.obj$votes
  }
  if(is.null(ref)){
    ref = as.character(rf.obj$y)
  }

  roc = matrix(rep(NA, 2*101), ncol = 2)
  prec.recall = matrix(rep(NA, 2*101), ncol = 2)
  bal.acc = rep(NA, 101)
  conf.mat = list()
  for(j in 0:100){
    cutoff = 0.01
    p.binary = rep(NA, nrow(p.prob))

    temp.idx = p.prob[,positive]>=cutoff*j
    p.binary[!temp.idx] = colnames(p.prob)[colnames(p.prob) != positive]
    p.binary[temp.idx] = positive

    temp.vec <- factor(append(p.binary,as.character(ref)))
    p.binary <- temp.vec[1:length(p.binary)]
    ref <- temp.vec[-(1:length(p.binary))]
    cfm = caret::confusionMatrix(p.binary, ref, positive)
    roc[j+1, 1] = 1 - cfm$byClass[2]
    roc[j+1, 2] = cfm$byClass[1]
    colnames(roc) <- c("False Positive", "True Positive")
    prec.recall[j+1,1] = cfm$byClass[1]
    prec.recall[j+1,2] = cfm$byClass[3]
    colnames(prec.recall) <- c("Recall", "Precision")
    bal.acc[j+1] = cfm$byClass[8]
    conf.mat[[j+1]] = cfm
  }


  return(list(roc = roc, prec.recall = prec.recall, bal.acc = bal.acc, conf.mat = conf.mat))
}

colfunc<-grDevices::colorRampPalette(c("royalblue","springgreen","yellow","red"))



##server 359

func_rhandsontable_prm_ranges <- function(df, option, ncol.temp = NULL){
  if(option == "unif_grid"){
    rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )  %>% rhandsontable::hot_col(col = "log.scale", type = "checkbox") %>% rhandsontable::hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))  %>% rhandsontable::hot_col(col = ncol.temp,renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits
  }else {
    rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )  %>% rhandsontable::hot_col(col = "log.scale", type = "checkbox") %>% rhandsontable::hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))

  }
}
## server 453
func_rhandsontable_prm_grids <- function(df, ncol.temp = NULL){
  rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )   %>% rhandsontable::hot_col(col = c(2:ncol.temp),renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits

}

## Server 960
func_rhandsontable_prm_ranges_add1 <- function(df){
  rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )   %>% rhandsontable::hot_col(col = c("min","max"),renderer=htmlwidgets::JS("safeHtmlRenderer"))

}

func_rhandsontable_prm_ranges_add2 <- function(df, ncol.temp = NULL){
  rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )  %>% rhandsontable::hot_col(col = "log.scale", type = "checkbox") %>% rhandsontable::hot_col(col = c("min","max","frac.range"),renderer=htmlwidgets::JS("safeHtmlRenderer"))  %>% rhandsontable::hot_col(col = ncol.temp,renderer=htmlwidgets::JS("safeHtmlRenderer")) ## to show all digits
}
func_rhandsontable_prm_ranges_add3 <- function(df){
  rhandsontable::rhandsontable(df, digit = 10, contextMenu = FALSE )  %>% rhandsontable::hot_col(col = "log.scale", type = "checkbox") %>% rhandsontable::hot_col(col = c("min","max","frac.range"),renderer=htmlwidgets::JS("safeHtmlRenderer"))
}

###help popup
###https://github.com/daattali/ddpcr/tree/master/inst/shiny/ui
helpPopup <- function(content, title = NULL) {
  shiny::a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    shiny::icon("question-circle")
  )
}
