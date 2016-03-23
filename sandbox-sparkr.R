# Set this to where Spark is installed
Sys.setenv(SPARK_HOME="/opt/spark/spark-1.6.1-bin-hadoop2.6")
Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.3.0" "sparkr-shell"')

# This line loads SparkR from the installed directory
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))

# Load the SparkR library
library(SparkR)
sc <- sparkR.init(master="local[*]")
sqlContext <- sparkRSQL.init(sc)
write.csv(trainsetImpute, file = 'trainsetImpute.csv', row.names = FALSE)

summary(trainsetDf)

types <- sapply(trainsetImpute, function(x) {
  class <- class(x)[1]
  if (class %in% c('integer')) {
    'integer'
  } else if (class %in% c('numeric')) {
    'float'
  } else if (class %in% c('factor', 'character')) {
    'string'
  } else if (class %in% c('POSIXct', 'POSIXt')) {
    'date'
  } else {
    'string'
  }
})

structFields <- lapply(names(types), function(x) {
  structField(x, types[x], TRUE)
})

schema <- do.call(structType, structFields)

trainsetDf <- read.df(sqlContext, path = 'trainsetImpute.csv', source="com.databricks.spark.csv", header="true", schema = schema)

sparkR.stop()
