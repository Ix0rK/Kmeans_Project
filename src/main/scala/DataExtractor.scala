package DataExtractor

import scala.collection.mutable.ArrayBuffer

object DataExtractor {

  def main(args: Array[String]): Unit = {
    print(getArrayOfNameIris)

  }
  def readIrisCSV: ArrayBuffer[ArrayBuffer[Double]] = {
    val matrice :ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]
    /*
        println("sepal length, sepal width, petal length, petal width")
    */
    val bufferedSource = scala.io.Source.fromFile("src/main/bezdekIris.data")
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      val colsWithoutName = cols.slice(0,4)
      val arrayOfDouble = new ArrayBuffer[Double]
      for(item <- colsWithoutName){
        arrayOfDouble.append(item.toDouble)
      }
      matrice.append(arrayOfDouble)
    }
    bufferedSource.close
    return matrice
  }
  def getArrayOfNameIris: ArrayBuffer[String] ={
    val matrice :ArrayBuffer[String] = new ArrayBuffer[String]
    /*
        println("sepal length, sepal width, petal length, petal width")
    */
    val bufferedSource = scala.io.Source.fromFile("src/main/bezdekIris.data")
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      val onlycolsOfName = cols.slice(4, 5)(0)
      matrice.append(onlycolsOfName)
    }
    bufferedSource.close
    return matrice
  }

}
