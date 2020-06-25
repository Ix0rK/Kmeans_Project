import DataExtractor.DataExtractor
import smile.plot.swing.PlotGroup

import scala.collection.mutable.ArrayBuffer
import scala.math.pow

object Kmeans {
  def main(args: Array[String]): Unit = {
    // Props To Kmeans
    val arrayOfIrisName = DataExtractor.getArrayOfNameIris.toArray
    val matriceFromCsv: ArrayBuffer[ArrayBuffer[Double]] = DataExtractor.readIrisCSV
    val matriceCSVArray : Array[Array[Double]] = matriceFromCsv.map(x => x.clone().toArray).toArray
    val arrayOfAxisLabel = Array("sepal.length","sepal.width","petal.length","petal.width")
    val nameOfIris: ArrayBuffer[String] = DataExtractor.getArrayOfNameIris
    // Kmeans by Ilan Keller
    val matriceOfAssignementForKmeans = kmeans_ik(matriceCSVArray,3,1000).toArray
    // Kmeans by Smile
    val kmeansVerify = smile.clustering.kmeans(matriceCSVArray,3,1000)
    // Kmeans From PPCA by Ilan Keller
    val matricePPCAProjection = Statistics.exportPPCAProjection()
    val matriceOfAssignementForKmeansFromPPCA = kmeans_ik(matricePPCAProjection,3,1000)
    // Kmeans From PPCA by Smile
    // Projections
    val plotGroupIrisDataset = createIrisDataPlotGroup(matriceCSVArray,arrayOfIrisName,arrayOfAxisLabel)
    val plotGroupKmeansIk = createKmeansPlotGroup(matriceCSVArray,matriceOfAssignementForKmeans,arrayOfAxisLabel)
    val plotGroupKmeansVerify = createKmeansPlotGroup(matriceCSVArray,kmeansVerify.y,arrayOfAxisLabel)

    smile.plot.desktop(plotGroupKmeansIk)
    smile.plot.desktop(plotGroupKmeansVerify)
    smile.plot.desktop(smile.plot.swing.plot(matricePPCAProjection,matriceOfAssignementForKmeansFromPPCA,'.'))
    smile.plot.desktop(plotGroupIrisDataset)
    /// Test number of individuals by groups
    printTestNumberOfIndividualInCluster(matriceOfAssignementForKmeans)
    printTestNumberOfIndividualInCluster(kmeansVerify.y)
    printTestNumberOfIndividualInCluster(matriceOfAssignementForKmeansFromPPCA)
  }
  def printTestNumberOfIndividualInCluster(matriceOfAssignement:Array[Int]): Unit ={
    var mapOfIndividuals: Map[Int,Int]= Map[Int,Int]()
    mapOfIndividuals += (0 -> 0)
    mapOfIndividuals += (1 -> 0)
    mapOfIndividuals += (2 -> 0)
    matriceOfAssignement.foreach((x)=> {
      mapOfIndividuals = mapOfIndividuals.updated(x,mapOfIndividuals(x) +1)
    })

    println(mapOfIndividuals.map(_.productIterator.mkString(":")).mkString("|"))
  }
  def createKmeansPlotGroup(matriceInput:Array[Array[Double]],matriceOfAssignement:Array[Int],arrayOfAxisLabel:Array[String]): PlotGroup ={
    val plotgroup = new smile.plot.swing.PlotGroup()
    for(i <- 0 until matriceInput(0).length){
      for(j<- 0 until matriceInput(0).length){
        val matriceCSVArray : Array[Array[Double]] = matriceInput.map(x => Array(x(i), x(j))).toArray
        val canvas : smile.plot.swing.Canvas= smile.plot.swing.plot(matriceCSVArray,matriceOfAssignement,'.')
        canvas.setAxisLabels(arrayOfAxisLabel(i),arrayOfAxisLabel(j))
        val panel: smile.plot.swing.PlotPanel= new smile.plot.swing.PlotPanel(canvas)
        plotgroup.add(panel)

      }
    }

    return plotgroup
  }
  def createIrisDataPlotGroup(matriceInput:Array[Array[Double]],matriceOfAssignement:Array[String],arrayOfAxisLabel:Array[String]): PlotGroup ={
    val plotgroup = new smile.plot.swing.PlotGroup()
    for(i <- 0 until matriceInput(0).length){
      for(j<- 0 until matriceInput(0).length){
        val matriceCSVArray : Array[Array[Double]] = matriceInput.map(x => Array(x(i), x(j))).toArray
        val canvas : smile.plot.swing.Canvas= smile.plot.swing.plot(matriceCSVArray,matriceOfAssignement,'.')
        canvas.setAxisLabels(arrayOfAxisLabel(i),arrayOfAxisLabel(j))
        val panel: smile.plot.swing.PlotPanel= new smile.plot.swing.PlotPanel(canvas)
        plotgroup.add(panel)
      }
    }

    return plotgroup
  }
  def kmeans_ik(matriceInput:Array[Array[Double]],numOfK:Int, numOfiteration:Int): Array[Int] ={
    var bufferedIterator:Int= numOfiteration
    var matriceOfK = createMatriceOfK(matriceInput,numOfK)
    var matrixOfAssignement = initialiserMatriceAffectationParIndex(matriceInput);
    do{
      matrixOfAssignement = affectMatricePointToKpoint(matriceInput,matriceOfK,matrixOfAssignement)
      matriceOfK = updateKpoint(matriceInput,matriceOfK,matrixOfAssignement)
      bufferedIterator = bufferedIterator - 1
    }while(bufferedIterator!=0)

    matriceOfK.foreach(x => println("Matrice of K",x.mkString("|")))
    return matrixOfAssignement
  }
  def initialiserMatriceAffectationParIndex(matriceInput:Array[Array[Double]]): Array[Int] = {
    return matriceInput.map(point => -1)
  }
  def createMatriceOfK(matriceInput:Array[Array[Double]],numOfK:Int): Array[Array[Double]] ={
    val matriceOfK : Array[Array[Double]] = new Array[Array[Double]](numOfK)
    for(i <- 0 until numOfK) {
      matriceOfK(i) = selectARandomPointFromMatrice(matriceInput)
    }
    return matriceOfK
  }
  def selectARandomPointFromMatrice(matrice:Array[Array[Double]]):Array[Double] ={
    val r = scala.util.Random
    val randomNumberInMatriceRange = r.nextInt(matrice.length)
    return matrice(randomNumberInMatriceRange)
  }
  def affectMatricePointToKpoint(matriceInput : Array[Array[Double]],matriceOfK:Array[Array[Double]],matrixOfAssignment:Array[Int]):Array[Int]= {
    matriceInput.zipWithIndex.foreach{case (point,i)=> {
      var matriceOfDistanceBetweenKAndPoint = matriceOfK.map(kpoint => {
        point.zip(kpoint).map(tuple =>pow((tuple._1 - tuple._2),2)).reduce((a,b)=> a+b)
      })
      var minDistance = matriceOfDistanceBetweenKAndPoint.min
      var indexOfKWithMinDistance = matriceOfDistanceBetweenKAndPoint.indexOf(minDistance)
      matrixOfAssignment(i) = indexOfKWithMinDistance
    }}
    return matrixOfAssignment
  }
  def updateKpoint(matriceInput:Array[Array[Double]],matriceOfK:Array[Array[Double]],matrixOfAssigement:Array[Int]):Array[Array[Double]]={
    try{
      matriceOfK.zipWithIndex.map{
        case (kPoint,indexOfK)=>{
          val pointsNearOfThisK : Array[Array[Double]] = matriceInput.zipWithIndex.map{case (point,i) =>{
            point.clone() :+ matrixOfAssigement(i).toDouble
          } }.filter(point =>{
            point(point.length-1).toInt == indexOfK
          })
          var buffMatriceForFold = pointsNearOfThisK(0).slice(0,pointsNearOfThisK(0).length -1).map(x => 0.toDouble)
          val reduceMatriceOfPoint:Array[Double] = pointsNearOfThisK.fold(buffMatriceForFold){
            (acc,num) => {
              val buffMatrice = new ArrayBuffer[Double]
              for((itemAcc,itemNum) <- acc.zip(num)){
                buffMatrice.append(itemAcc + itemNum)
              }
              buffMatrice.toArray
            };
          }
          reduceMatriceOfPoint.map(x => x/pointsNearOfThisK.length)
        }
      }
    }catch{
      case e:ArrayIndexOutOfBoundsException => println("Error uploading Kpoint - Randomize Kpoint gived the same")
        matriceOfK.foreach(x => println(x.mkString("|")))
        System.exit(1)
        new Array[Array[Double]](1)
    }
  }
}
