###Define a intergrated workspace containing a model, parameter ranges, parameter combinations..
###by defining R S4 Class "PPM"

setClass(
  Class = "PPM",
  representation = representation(
    PPM.name = "character",
    model = "list",
    prm.space = "list",
    phenotypes = "list",
    ml.models = "list",
    analysis = "list",
    simulation = "list"
  ),
  validity = function(object){
  }
)

##constructor
setMethod(
  f="initialize",
  signature = "PPM",
  definition = function(.Object, name){
    #if(0){Stop("initialize PPM: error")}else{}
    .Object@PPM.name <- name
    .Object@prm.space$prm.ranges <- list()
    .Object@prm.space$initial.prm.combs <- list()
    .Object@prm.space$additional.prm.combs <- list()
    .Object@prm.space$counter <- list()
    .Object@prm.space$counter[["unif_grid"]] <- 0
    .Object@prm.space$counter[["pseudoranddom"]] <- 0
    .Object@prm.space$counter[["sobol'"]] <- 0
    .Object@prm.space$counter[["latin_hyp"]] <- 0
    .Object@ml.models
    .Object@analysis$tsne <- list()
    return(.Object)

  }

)

setGeneric("get.PPM.name",function(object){standardGeneric ("get.PPM.name")})
setMethod(
  f = "get.PPM.name",
  signature = "PPM",

  definition = function(object){
    return(object@PPM.name)
  }
)

setGeneric("get.prm.space",function(object){standardGeneric ("get.prm.space")})
setMethod(
  f = "get.prm.space",
  signature = "PPM",
  definition = function(object){
    return(object@prm.space)
  }
)

setGeneric("get.prm.ranges.name",function(object){standardGeneric ("get.prm.ranges.name")})
setMethod(
  f = "get.prm.ranges.name",
  signature = "PPM",
  definition = function(object){
    return(names(object@prm.space$prm.ranges))
  }
)

setGeneric("get.prm.ranges",function(object, name){standardGeneric ("get.prm.ranges")})
setMethod(
  f = "get.prm.ranges",
  signature = "PPM",
  definition = function(object, name){
    return(object@prm.space$prm.ranges[[name]])
  }
)

setGeneric("get.init.prm.combs.name",function(object){standardGeneric ("get.init.prm.combs.name")})
setMethod(
  f = "get.init.prm.combs.name",
  signature = "PPM",
  definition = function(object){
    if(!is.null(object)){
      temp.prm.ranges.name <- get.prm.ranges.name(object)
      if(!is.null(temp.prm.ranges.name)){
        if(length(temp.prm.ranges.name)==1){
          temp.init.prm.combs.name <- list()
          temp.init.prm.combs.name[[1]] <- apply(matrix(temp.prm.ranges.name),1,function(name, object){names(object@prm.space$initial.prm.combs[[name]])},object = object)
        }else{
          temp.init.prm.combs.name <- apply(matrix(temp.prm.ranges.name),1,function(name, object){names(object@prm.space$initial.prm.combs[[name]])},object = object)
        }

        if(!is.null(unlist(temp.init.prm.combs.name))){
          names(temp.init.prm.combs.name) <- temp.prm.ranges.name
          return(temp.init.prm.combs.name)
        }else{
          return(NULL)
        }

      }else{
        return(NULL)
      }

    }else{
      return(NULL)

    }
  }
)


setGeneric("get.init.prm.combs",function(object, name, prm.ranges.name){standardGeneric ("get.init.prm.combs")})
setMethod(
  f = "get.init.prm.combs",
  signature = "PPM",
  definition = function(object, name, prm.ranges.name){


    return(object@prm.space$initial.prm.combs[[prm.ranges.name]][[name]])
  }
)


setGeneric("get.addit.prm.combs.name",function(object){standardGeneric ("get.addit.prm.combs.name")})
setMethod(
  f = "get.addit.prm.combs.name",
  signature = "PPM",
  definition = function(object){
    if(!is.null(object)){
      temp.prm.ranges.name <- get.prm.ranges.name(object)

      if(!is.null(temp.prm.ranges.name)){
        if(length(temp.prm.ranges.name)==1){
          temp.init.prm.combs.name <- list()
          temp.init.prm.combs.name[[1]] <- apply(matrix(temp.prm.ranges.name),1,function(name, object){names(object@prm.space$initial.prm.combs[[name]])},object = object)
        }else{
          temp.init.prm.combs.name <- apply(matrix(temp.prm.ranges.name),1,function(name, object){names(object@prm.space$initial.prm.combs[[name]])},object = object)
        }
        if(!is.null(unlist(temp.init.prm.combs.name))){
          temp.addit.prm.combs.name <-list()
          for(i in 1:length( temp.prm.ranges.name)){
            temp.addit.prm.combs.name[[temp.prm.ranges.name[i]]] <- list()
            if(!is.null(temp.init.prm.combs.name[[i]])){
              temp.name<- unlist(apply(matrix(temp.init.prm.combs.name[[i]]), 1,
                                function(name,prm.ranges.name, object){
                                  temp.names <- names(object@prm.space$additional.prm.combs[[prm.ranges.name]][[name]])
                                  #print(name)
                                  # if(!is.null(temp.names)){
                                  #   names(temp.names) <- name
                                  # }
                                  #print(temp.names)
                                  },
                                prm.ranges.name = temp.prm.ranges.name[i],
                                object=object
              ))
              names(temp.name) <- NULL
              if(!is.null(temp.name)){
                temp.addit.prm.combs.name[[temp.prm.ranges.name[i]]] <- temp.name
                #names(temp.addit.prm.combs.name[[temp.prm.ranges.name[i]]])<- rep(temp.init.prm.combs.name[[i]], length(temp.name))
              }
            }
          }
          if(!is.null(unlist(temp.addit.prm.combs.name))){
            return(temp.addit.prm.combs.name)
          }else{
            return(NULL)
          }


        }else{
          return(NULL)
        }

      }else{
        return(NULL)
      }

    }else{
      return(NULL)

    }

  }
)


setGeneric("get.addit.prm.combs",function(object, name, prm.ranges.name, init.prm.combs.name ){standardGeneric ("get.addit.prm.combs")})
setMethod(
  f = "get.addit.prm.combs",
  signature = "PPM",
  definition = function(object, name, prm.ranges.name,init.prm.combs.name){
    return(object@prm.space$additional.prm.combs[[prm.ranges.name]][[init.prm.combs.name]][[name]])
  }
)



setGeneric("get.phenotypes",function(object, name){standardGeneric ("get.phenotypes")})
setMethod(
  f = "get.phenotypes",
  signature = "PPM",
  definition = function(object, name){
    return(object@phenotypes[[name]])
  }
)

setGeneric("get.phenotypes.for.selected.prm.sets",function(object, phenotypes, prm.sets){standardGeneric ("get.phenotypes.for.selected.prm.sets")})
setMethod(
  f = "get.phenotypes.for.selected.prm.sets",
  signature = "PPM",
  definition = function(object, phenotypes, prm.sets){
    temp.phenotypes <- lapply(X = phenotypes, FUN = get.phenotypes, object = object )
    names( temp.phenotypes) <- phenotypes
    phenotype.values.selected <- list()

    for(temp.phenotype.name in phenotypes){
      phenotype.values.selected[[temp.phenotype.name]] <- data.frame(stringsAsFactors = F)
      for(temp.prm.set.name in prm.sets){
        phenotype.values.selected[[temp.phenotype.name]] <- rbind(phenotype.values.selected[[temp.phenotype.name]], temp.phenotypes[[temp.phenotype.name]][[temp.prm.set.name]])
      }
      phenotype.values.selected[[temp.phenotype.name]] <- phenotype.values.selected[[temp.phenotype.name]][order(phenotype.values.selected[[temp.phenotype.name]]$pkey),]
    }
    return(phenotype.values.selected)
  }
)



setGeneric("get.phenotypes.name",function(object){standardGeneric ("get.phenotypes.name")})
setMethod(
  f = "get.phenotypes.name",
  signature = "PPM",
  definition = function(object){
    return(names(object@phenotypes))
  }
)


setGeneric("get.ml.models.name",function(object){standardGeneric ("get.ml.models.name")})
setMethod(
  f = "get.ml.models.name",
  signature = "PPM",
  definition = function(object){
    temp.phenotypes.name <- get.phenotypes.name(object)
    temp.ml.models.name <- apply(matrix( temp.phenotypes.name),1,function(name, object){names(object@ml.models[[name]])},object = object)
    return(temp.ml.models.name)
  }
)

setGeneric("get.phenotypes.with.ml.models",function(object){standardGeneric ("get.phenotypes.with.ml.models")})
setMethod(
  f = "get.phenotypes.with.ml.models",
  signature = "PPM",
  definition = function(object){
    return(names(object@ml.models))
  }
)


setGeneric("get.ml.models.path",function(object, phenotype.name, ml.model.name ){standardGeneric ("get.ml.models.path")})
setMethod(
  f = "get.ml.models.path",
  signature = "PPM",
  definition = function(object, phenotype.name, ml.model.name){
    temp.ml.model.paths <- c(object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model.path"]],
                             object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model.res.path"]])
    return(temp.ml.model.paths)
  }
)



setGeneric("get.ml.model",function(object, phenotype.name, ml.model.name ){standardGeneric ("get.ml.model")})
setMethod(
  f = "get.ml.model",
  signature = "PPM",
  definition = function(object, phenotype.name, ml.model.name){
    temp.ml.model <- list(ml.model = object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model"]],
                          ml.model.res =  object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model.res"]],
                          ml.model.path = object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model.path"]],
                          ml.model.res.path =  object@ml.models[[phenotype.name]][[ ml.model.name]][["ml.model.res.path"]],
                          train.data = object@ml.models[[phenotype.name]][[ ml.model.name]][["train.data"]],
                          test.data = object@ml.models[[phenotype.name]][[ ml.model.name]][["test.data"]],
                          custom.scale = object@ml.models[[phenotype.name]][[ ml.model.name]][["custom.scale"]],
                          mode = object@ml.models[[phenotype.name]][[ ml.model.name]][["mode"]],
                          prm.sets.used = object@ml.models[[phenotype.name]][[ ml.model.name]][["prm.sets.used"]],
                          class.def = object@ml.models[[phenotype.name]][[ ml.model.name]][["class.def"]],
                          note = object@ml.models[[phenotype.name]][[ ml.model.name]][["note"]],
                          seed.num = object@ml.models[[phenotype.name]][[ ml.model.name]][["seed.num"]]

                          )
    return(temp.ml.model)
  }
)


setGeneric("get.tsne.coord.name",function(object, name){standardGeneric ("get.tsne.coord.name")})
setMethod(
  f = "get.tsne.coord.name",
  signature = "PPM",
  definition = function(object){
    return(names(object@analysis$tsne))
  }
)





setGeneric("get.tsne.coord",function(object, name){standardGeneric ("get.tsne.coord")})
setMethod(
  f = "get.tsne.coord",
  signature = "PPM",
  definition = function(object,name){
    return(object@analysis$tsne[[name]])
  }
)
#list(ml.model.name = , name = )
setGeneric("get.tsne.coord.ml",function(object, ml.model.name , name){standardGeneric ("get.tsne.coord.ml")})
setMethod(
  f = "get.tsne.coord.ml",
  signature = "PPM",
  definition = function(object,ml.model.name , name){
    return(object@analysis$tsne[[ml.model.name]][[name]])
  }
)

setGeneric("get.tsne.coord.ml.exist",function(object, ml.model.name){standardGeneric ("get.tsne.coord.ml.exist")})
setMethod(
  f = "get.tsne.coord.ml.exist",
  signature = "PPM",
  definition = function(object,ml.model.name){
    if(!is.null(object@analysis$tsne[[ml.model.name]])){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
)


setGeneric("get.validation.info",function(object, name){standardGeneric ("get.validation.info")})
setMethod(
  f = "get.validation.info",
  signature = "PPM",
  definition = function(object){

  }
)






setGeneric("add.prm.ranges<-", function(object,value){standardGeneric("add.prm.ranges<-")})
setReplaceMethod(
  f = "add.prm.ranges",
  signature = "PPM",
  definition = function(object, value){
    object@prm.space$prm.ranges[[value[["name"]]]] <- value[["prm.ranges"]] #data.frame
    return(object)
  }
)

# ##wrapper
# add.prm.ranges <- function(object, prm.ranges,  name){
#   add.prm.ranges(object) <- list(prm.ranges = prm.ranges, name = name )
# }


#list( prm.ranges.name, name, method, log.scale, num.grids, prm.grids, raw.smpl, prm.combs, prm.combs.z)
setGeneric("add.init.prm.combs<-", function(object, value){standardGeneric("add.init.prm.combs<-")})
setReplaceMethod(
  f = "add.init.prm.combs",
  signature = "PPM",
  #definition = function(object, prm.ranges.name, name,  method, log.scale, num.grids =NULL, prm.grids = NULL, raw.smpl = NULL, prm.combs, prm.combs.z ){
  definition = function(object, value ){

    if(is.null(object@prm.space$initial.prm.combs[[value$prm.ranges.name]])){
      object@prm.space$initial.prm.combs[[value$prm.ranges.name]] <- list()
    }else{}

    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]] <- list()
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["method"]] <- value$method #string
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["log.scale"]] <- value$log.scale # vector
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["num.grids"]] <- value$num.grids #vector
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["prm.grids"]] <- value$prm.grids # data.frame
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["raw.smpl"]] <- value$raw.smpl # data.frame
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["prm.combs"]] <- value$prm.combs #data.frame
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][["prm.combs.z"]] <- value$prm.combs.z #data.frame
    object@prm.space$initial.prm.combs[[value$prm.ranges.name]][[value$name]][[value$name]][["rd_seed"]] <- value$rd_seed # vector
    return(object)

  }
)

# list(prm.ranges.name, init.prm.combs.name, name, method, log.scale, frac.range, num.grids = NULL, prm.combs.selected, prm.combs, prm.combs.z, raw.smpl = NULL)
setGeneric("add.additional.prm.combs<-", function(object, value ){standardGeneric("add.additional.prm.combs<-")})
setReplaceMethod(
  f = "add.additional.prm.combs",
  signature = "PPM",
  definition = function(object,value ){

    if(is.null(object@prm.space$additional.prm.combs[[value$prm.ranges.name]])){
      object@prm.space$additional.prm.combs[[value$prm.ranges.name]] <- list()
    }else{}

    if(is.null(object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]])){
      object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]] <- list()
    }else{}

    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]] <- list()
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["method"]] <- value$method #string
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["log.scale"]] <- value$log.scale # vector
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["frac.range"]] <- value$frac.range # vector
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["num.grids"]] <- value$num.grids # vector
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["prm.combs.selected"]] <- value$prm.combs.selected # dataframe
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["prm.combs"]] <- value$prm.combs # dataframe
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["prm.combs.z"]] <- value$prm.combs.z # dataframe
    object@prm.space$additional.prm.combs[[value$prm.ranges.name]][[value$init.prm.combs.name]][[value$name]][["rd_seed"]] <- value$rd_seed # vector
    return(object)

  }
)

#list(name, prm.combs.name, phenotype)
setGeneric("add.phenotypes<-", function(object, value ){standardGeneric("add.phenotypes<-")})
setReplaceMethod(
  f = "add.phenotypes",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@phenotypes[[value$name]])){
      object@phenotypes[[value$name]] <- list()
    }
    object@phenotypes[[value$name]][[value$prm.combs.name]] <- value$phenotype #data.frame
    return(object)
  }
)
##list(phenotype.name = ,name = , ml.model = NULL, ml.model.res = NULL ,ml.model.path = NULL , ml.model.res.path = NULL ,mode = ,prm.sets.used = c(prmset1.name, prmset2.name), train.data = , test.data = NULL, class.def = NULL, note = string, custom.scale = list ) for ml.model, input a relative path ("ml.models/ml.model.file")
setGeneric("add.ml.model<-", function(object, value ){standardGeneric("add.ml.model<-")})
setReplaceMethod(
  f = "add.ml.model",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@ml.models[[value$phenotype.name]])){
      object@ml.models[[value$phenotype.name]] <- list()
    }
    object@ml.models[[value$phenotype.name]][[value$name]] <- list()

    #if the size of the model is large
    object@ml.models[[value$phenotype.name]][[value$name]][["ml.model.path"]] <- value$ml.model.path # string of path for ml.model.object file
    object@ml.models[[value$phenotype.name]][[value$name]][["ml.model.res.path"]] <- value$ml.model.res.path # string of path for ml.model.res.object file for bias correction.

    #if the size of the model is small
    object@ml.models[[value$phenotype.name]][[value$name]][["ml.model"]] <- value$ml.model# string of path for ml.model.object file
    object@ml.models[[value$phenotype.name]][[value$name]][["ml.model.res"]] <- value$ml.model.res # string of path for ml.model.res.object file for bias correction.

    object@ml.models[[value$phenotype.name]][[value$name]][["mode"]] <- value$mode # string "regression" or "classification"
    object@ml.models[[value$phenotype.name]][[value$name]][["prm.sets.used"]] <- value$prm.sets.used #
    object@ml.models[[value$phenotype.name]][[value$name]][["train.data"]] <- value$train.data # whole, character vector
    object@ml.models[[value$phenotype.name]][[value$name]][["test.data"]] <- value$test.data # character vector
    object@ml.models[[value$phenotype.name]][[value$name]][["class.def"]] <- value$class.def # data.frame for class explaination.
    object@ml.models[[value$phenotype.name]][[value$name]][["note"]] <- value$note # string for data filtering information
    object@ml.models[[value$phenotype.name]][[value$name]][["custom.scale"]] <- value$custom.scale # list(name of scaling, scaled parameters (scaled = "original"), scaled values
    object@ml.models[[value$phenotype.name]][[value$name]][["seed.num"]] <- value$seed.num # integer

    return(object)
  }
)

#list( prm.combs.name =, tsne.coord =,   custom.scale = list)
setGeneric("add.tsne.coord<-", function(object, value ){standardGeneric("add.tsne.coord<-")})
setReplaceMethod(
  f = "add.tsne.coord",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@analysis$tsne)){
      object@analysis$tsne <- list()
    }
    object@analysis$tsne[[value$prm.combs.name]] <- value$tsne.coord # data.frame(pkey, tSNE1, tSNE2)
    if(is.null( object@analysis$tsne[["custom.scale"]]) ){
      object@analysis$tsne[["custom.scale"]] <- list()
    }
    if(!is.null(value$custom.scale)){
      object@analysis$tsne[["custom.scale"]][[value$custom.scale$name]]<- list()
      object@analysis$tsne[["custom.scale"]][[value$custom.scale$name]][["parameters"]] <- value$custom.scale$parameters # list(name of scaling, scaled parameters (scaled = "original")
    }
    return(object)
  }
)

#ml model specific tsne
#list( prm.combs.name =, tsne.coord =,  ml.model.name =  custom.scale = list)
setGeneric("add.tsne.coord.ml<-", function(object, value ){standardGeneric("add.tsne.coord.ml<-")})
setReplaceMethod(
  f = "add.tsne.coord.ml",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@analysis$tsne)){
      object@analysis$tsne <- list()
    }
    if(is.null(object@analysis$tsne[[value$ml.model.name]])){
      object@analysis$tsne[[value$ml.model.name]]<- list()
    }
    object@analysis$tsne[[value$ml.model.name]][[value$prm.combs.name]] <- value$tsne.coord # data.frame(pkey, tSNE1, tSNE2)
    if(is.null( object@analysis$tsne[[value$ml.model.name]][["custom.scale"]]) ){
      object@analysis$tsne[[value$ml.model.name]][["custom.scale"]] <- list()
    }
    if(!is.null(value$custom.scale)){
      object@analysis$tsne[[value$ml.model.name]][["custom.scale"]][[value$custom.scale$name]]<- list()
      object@analysis$tsne[[value$ml.model.name]][["custom.scale"]][[value$custom.scale$name]][["parameters"]] <- value$custom.scale$parameters # list(name of scaling, scaled parameters (scaled = "original")
    }
    return(object)
  }
)





#Validation not yet done
#list(prm.comb.selected = c(pkey, values), parent.prm.set.name, prms.perturbed = c(...), prm.combs.val, prm.combs.z.val, phenotype.name = , phen.val.pred, ml.model.name, phen.val.sim)
setGeneric("add.validation<-", function(object, value ){standardGeneric("add.validation<-")})
setReplaceMethod(
  f = "add.validation",
  signature = "PPM",
  definition = function(object, value ){

    if(is.null(object@analysis$validation)){
      object@analysis$validation <- list()
    }

    temp.val.name <- paste0(value$prm.comb.selected$pkey,"_", value$prms.tuned[1],"_", value$prms.tuned[2])
    if(is.null(object@analysis$validation[[value$temp.val.name]])){
      object@analysis$validation[[value$temp.val.name]] <- list()
    }
    object@analysis$validation[[value$temp.val.name]][["parent.prm.set"]] <- value$parent.prm.set.name
    object@analysis$validation[[value$temp.val.name]][["prm.comb.selected"]] <- value$prm.com.selected #data.frame
    object@analysis$validation[[value$temp.val.name]][["prms.perturbed"]] <- value$prms.perturbed # string vector
    object@analysis$validation[[value$temp.val.name]][["prm.combs.val"]] <- value$prm.combs.val # data.frame
    object@analysis$validation[[value$temp.val.name]][["prm.combs.z.val"]] <- value$prm.combs.z.val # data.frame
    if(is.null(object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]])){
      object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]] <- list()
    }

    if(is.null(object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]][["prediction"]])){
      object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]][["prediction"]] <- list()
    }
    object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]][["prediction"]][["ml.model.name"]]<- value$phen.val.pred #list(ml.model.name, data.frame(pkeys, values))
    object@analysis$validation[[value$temp.val.name]][[value$phenotype.name]][["simulation"]] <- value$phen.val.sim #data.frame(pkey, values)

    return(object)
  }
)

#counter for parameter generation
setGeneric("get.prm.combs.count",function(object, smpl_method, prm.ranges.name){standardGeneric ("get.prm.combs.count")})
setMethod(
  f = "get.prm.combs.count",
  signature = "PPM",
  definition = function(object, smpl_method, prm.ranges.name){
    if(is.null(object@prm.space$counter[[prm.ranges.name]])){
      return(NULL)
    }else if(is.null(object@prm.space$counter[[prm.ranges.name]][[smpl_method]])){
      return(NULL)
    }else{
      return(object@prm.space$counter[[prm.ranges.name]][[smpl_method]])
    }

  }
)

#list(prm.ranges.name, smpl_method, addit.count)
setGeneric("add.prm.combs.count<-", function(object, value ){standardGeneric("add.prm.combs.count<-")})
setReplaceMethod(
  f = "add.prm.combs.count",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@prm.space$counter[[value$prm.ranges.name]])){
     object@prm.space$counter[[value$prm.ranges.name]] <- list()
    }
    if(is.null(object@prm.space$counter[[value$prm.ranges.name]][[value$smpl_method]])){
      object@prm.space$counter[[value$prm.ranges.name]][[value$smpl_method]] <- 0
    }
    object@prm.space$counter[[value$prm.ranges.name]][[value$smpl_method]] <- object@prm.space$counter[[value$prm.ranges.name]][[value$smpl_method]] + value$addit.count
    return(object)
  }
)

#list(name, parameters, func.obj (forward, reverse), values(data.frame(pkey, values)))
setGeneric("add.custom.scale<-", function(object, value ){standardGeneric("add.custom.scale<-")})
setReplaceMethod(
  f = "add.custom.scale",
  signature = "PPM",
  definition = function(object, value ){
    if(is.null(object@analysis[["custom.scale"]])){
      object@analysis[["custom.scaling"]] <- list()
    }
    object@analysis[["custom.scale"]][[value$name]] <- list()
    object@analysis[["custom.scale"]][[value$name]][["parameters"]] <- value$parameters # string vector (scaled == "original")
    object@analysis[["custom.scale"]][[value$name]][["func.obj"]] <- value$func.obj # function object to convert original parameters to custom scaling or vice versa.
    object@analysis[["custom.scale"]][[value$name]][["other.vals"]] <- value$other.vals # data.frame( pkey, parameter values)

    return(object)
  }
)


setGeneric("get.custom.scale",function(object, name){standardGeneric ("get.custom.scale")})
setMethod(
  f = "get.custom.scale",
  signature = "PPM",
  definition = function(object, name){
    if(is.null(object@analysis$custom.scale)){
      return(NULL)
    }else if(is.null(object@analysis$custom.scale[[name]])){
      return(NULL)
    }else{
      return(object@analysis$custom.scale[[name]])
    }

  }
)


setGeneric("get.custom.scale.func.obj",function(object, name){standardGeneric ("get.custom.scale.func.obj")})
setMethod(
  f = "get.custom.scale.func.obj",
  signature = "PPM",
  definition = function(object, name){
    if(is.null(object@analysis$custom.scale)){
      return(NULL)
    }else if(is.null(object@analysis$custom.scale[[name]])){
      return(NULL)
    }else{
      return(object@analysis$custom.scale[[name]]$func.obj)
    }

  }
)




setGeneric("get.custom.scale.func.obj.other.vals",function(object, name){standardGeneric ("get.custom.scale.func.obj.other.vals")})
setMethod(
  f = "get.custom.scale.func.obj.other.vals",
  signature = "PPM",
  definition = function(object, name){
    if(is.null(object@analysis$custom.scale)){
      return(NULL)
    }else if(is.null(object@analysis$custom.scale[[name]])){
      return(NULL)
    }else{
      return(object@analysis$custom.scale[[name]]$other.vals)
    }

  }
)


setGeneric("get.custom.scale.prms",function(object ){standardGeneric ("get.custom.scale.prms")})
setMethod(
  f = "get.custom.scale.prms",
  signature = "PPM",
  definition = function(object){
    temp.prms<- NULL
    if(is.null(object@analysis$custom.scale)){
      return(NULL)
    }else{
      temp.prms<-list()
      temp.names <- names(object@analysis$custom.scale)
      for(temp.name in temp.names){
        temp.prms[[temp.name]]<-names(object@analysis$custom.scale[[temp.name]]$parameters)
      }

    }
    return(temp.prms)
  }
)


