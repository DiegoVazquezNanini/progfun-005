object pairs {

  def isPrime(n:Int):Boolean = (2 until n ) forall (c => n % c != 0 )

  val n = 7
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i,j))) filter (pair => isPrime(pair._1 + pair._2))

}