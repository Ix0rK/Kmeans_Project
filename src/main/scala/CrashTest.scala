object CrashTest {
  // Avant de lancer le crashtest je vous conseille de commenter les execution de projection kmeans:l28 à l30
  def main(args: Array[String]): Unit = {
    for(i <- 0 until 100){
      Kmeans.main(new Array[String](2))
    }
    println("succeed - no bug throw")
  }
}
