import DataExtractor.DataExtractor

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

object Statistics {

  def main(args: Array[String]): Unit = {
    val arrayOfIrisName = DataExtractor.getArrayOfNameIris.toArray
    val matriceFromCsv: ArrayBuffer[ArrayBuffer[Double]] = DataExtractor.readIrisCSV
    val matriceCSVarray = matriceFromCsv.map(x=> x.clone().toArray).toArray
    val matriceMoyenne: ArrayBuffer[Double] = moyenneMatrice(matriceFromCsv);
    val matriceVariance: ArrayBuffer[Double] = varianceMatrice(matriceFromCsv, matriceMoyenne)
    val matriceEcartType: ArrayBuffer[Double] = ecartTypeMatrice(matriceVariance)
    val matriceStandardiseParAttribut:ArrayBuffer[ArrayBuffer[Double]] = standardisationMatrice(matriceMoyenne,matriceEcartType,matriceFromCsv)
    val matriceCorrelations: ArrayBuffer[ArrayBuffer[Double]] = matriceDesCorrelations(matriceStandardiseParAttribut)
    val matricePPCA = matriceDesProjectionParPCA(matriceFromCsv)
    val MapMoyenneParClasse = baricentrePourChaqueClasse(matriceFromCsv,arrayOfIrisName)
    println(MapMoyenneParClasse.mkString("\n"))
    println("Moyenne",matriceMoyenne)
    println("variance",matriceVariance)
    println("EcartType",matriceEcartType)
    println("matriceCorrelations",matriceCorrelations.mkString("\n"))

    smile.plot.desktop(smile.plot.swing.plot(matricePPCA,'*'))
    smile.plot.desktop(smile.plot.swing.plot(matricePPCA,arrayOfIrisName,'*'))


  }
  def baricentrePourChaqueClasse(matriceInput:ArrayBuffer[ArrayBuffer[Double]],classNames:Array[String]): Map[String,ArrayBuffer[Double]] ={
    var mapOfdifferentIris:Map[String,ArrayBuffer[Double]] = Map[String,ArrayBuffer[Double]]()
    classNames.distinct.foreach(distinctClasseName =>  {
        mapOfdifferentIris += (distinctClasseName -> moyenneMatrice(matriceInput.zip(classNames).filter((tuple) => tuple._2 == distinctClasseName).map(tuple => tuple._1)))
    })
    mapOfdifferentIris
  }
  def exportPPCAProjection(): Array[Array[Double]] ={
    val matriceFromCsv: ArrayBuffer[ArrayBuffer[Double]] = DataExtractor.readIrisCSV
    val matricePPCA = matriceDesProjectionParPCA(matriceFromCsv)
    matricePPCA
  }
  def moyenneMatrice(matriceInput: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[Double] = {
    var matriceBuffer: ArrayBuffer[Double] = new ArrayBuffer[Double]
    for ((line, i) <- matriceInput.view.zipWithIndex) {
      if (i == 0) {
        matriceBuffer = line.clone()
      } else {
        for ((item, i) <- line.view.zipWithIndex) {
          matriceBuffer(i) += item
        }
      }
    }
    val matriceResult: ArrayBuffer[Double] = matriceBuffer.map(x => x / matriceInput.length)
    return matriceResult
  }

  def varianceMatrice(matriceInput: ArrayBuffer[ArrayBuffer[Double]], matriceMoyenne: ArrayBuffer[Double]): ArrayBuffer[Double] = {
    var matriceBuffer: ArrayBuffer[Double] = new ArrayBuffer[Double];
    for ((line, i) <- matriceInput.view.zipWithIndex) {
      if (i == 0) {
        matriceBuffer = line.zipWithIndex.map { case (x, i) => pow(x - matriceMoyenne(i), 2) }
      } else {
        for ((item, i) <- line.view.zipWithIndex) {
          matriceBuffer(i) = matriceBuffer(i) + pow(item - matriceMoyenne(i), 2)
        }
      }
    }
    val matriceResult: ArrayBuffer[Double] = matriceBuffer.map(x => x / matriceInput.length)
    return matriceResult
  }

  def ecartTypeMatrice(matriceVariance: ArrayBuffer[Double]): ArrayBuffer[Double] = {
    return matriceVariance.map(x => sqrt(x))
  }
  def standardisationMatrice(matriceMoyenne: ArrayBuffer[Double],matriceEcartType: ArrayBuffer[Double],matriceInput: ArrayBuffer[ArrayBuffer[Double]]):ArrayBuffer[ArrayBuffer[Double]] = {
    var matriceOutput : ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]
    var matriceAvecVecteurParAttribut = creationDeVecteurParAttribut(matriceInput);
    for((line,i) <- matriceAvecVecteurParAttribut.view.zipWithIndex){
      matriceOutput += line.map(element => (element - matriceMoyenne(i))/matriceEcartType(i))
    }


    return matriceOutput
  }
  def creationDeVecteurParAttribut(matriceInput: ArrayBuffer[ArrayBuffer[Double]]):ArrayBuffer[ArrayBuffer[Double]] = {
    val matriceOutput: ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]
    for( i <- matriceInput(0).indices){
      matriceOutput+= new ArrayBuffer[Double]
      for((line,j) <- matriceInput.view.zipWithIndex){
        matriceOutput(i)+=line(i)
      }
    }
    return matriceOutput
  }
  def coefficientDeCorrelationPourDeuxVecteur(x:ArrayBuffer[Double],y:ArrayBuffer[Double]): Double ={
    var coefficientDeCorrelation :Double= 0
    for(i <- x.indices) {
      coefficientDeCorrelation =coefficientDeCorrelation + ( (x(i)* y(i)) / x.size)
    }
    return coefficientDeCorrelation
  }
  def matriceDesCorrelations(matriceStandardiseParAttribut:ArrayBuffer[ArrayBuffer[Double]]):ArrayBuffer[ArrayBuffer[Double]]={
    val matriceDesCorrelations:ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]
    for((attribut,i )<- matriceStandardiseParAttribut.view.zipWithIndex){
      val arrayDeCorrelationPourlattribut = new ArrayBuffer[Double]
      for((correlateur,j) <- matriceStandardiseParAttribut.view.zipWithIndex){
        arrayDeCorrelationPourlattribut+=coefficientDeCorrelationPourDeuxVecteur(attribut,correlateur)
      }
      matriceDesCorrelations+=arrayDeCorrelationPourlattribut
    }
    return matriceDesCorrelations
  }
  def matriceDesProjectionParPCA(matriceInput:ArrayBuffer[ArrayBuffer[Double]]):Array[Array[Double]]={
    val matriceArray =  matriceInput.map(x => x.clone().toArray).toArray
    val matricePPCA =  smile.projection.ppca(matriceArray,2).getProjection().toArray()
    println("noisvariance",smile.projection.ppca(matriceArray,2).getNoiseVariance())
    val projectionMatrice = matriceArray.map(point => {
      matricePPCA.map{
        (current) =>{
          current.zip(point).foldLeft(0.toDouble){
            (y,z)=> {y + (z._1 * z._2)}}
        }}
    })
    return projectionMatrice
  }
}
