import scala.util.Random
import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._

package object riego {

  /*          DEFINICION DE TIPOS         */

  // Un tablón es una tripleta con el tiempo de supervivencia,
  // el tiempo de riego y la prioridad del tablón
  type Tablon = (Int, Int, Int)


  // Una finca es un vector de tablones
  type Finca = Vector[Tablon]
  // si f: Finca, f(i) = (tsi, tri, pi)


  // La distancia entre dos tablones se representa por
  // una matriz
  type Distancia = Vector[Vector[Int]]


  // Una programación de riego es un vector que asocia
  // cada tablón i con su turno de riego (0 es el primer turno,
  // n−1 es el último turno)
  type ProgRiego = Vector[Int]
  // si v: ProgRiego, y v.length == n, v es una permutación
  // de {0, ..., n−1} v(i) es el turno de riego del tablón i
  // para 0 <= i < n


  // El tiempo de inicio de riego es un vector que asocia
  // cada tablón i con el momento del tiempo en que se riega
  type TiempoInicioRiego = Vector[Int]
  // si t: TiempoInicioRiego y t.length == n, t(i) es la hora a
  // la que inicia a regarse el tablón i


  /*                 FUNCIONES           */

  /*          Entradas aleatorias         */
  val random = new Random()

  def fincaAlAzar(long: Int): Finca = {
    // Crea una finca de long tablones,
    // con valores aleatorios entre 1 y long*2 para el tiempo
    // de supervivencia, entre 1 y long para el tiempo
    // de riego, y entre 1 y 4 para la prioridad
    val v = Vector.fill(long) {
      (random.nextInt(long * 2) + 1,
        random.nextInt(long) + 1,
        random.nextInt(4) + 1)
    }
    v
  }

  def distanciaAlAzar(long: Int): Distancia = {
    // Crea una matriz de distancias para una finca
    // de long tablones, con valores aleatorios entre
    // 1 y long*3
    val v = Vector.fill(long, long) {
      random.nextInt(long * 3) + 1
    }
    Vector.tabulate(long, long)(
      (i, j) => if (i < j) v(i)(j)
      else if (i == j) 0
      else v(j)(i))
  }


  /*          Acceso a las entradas         */
  // f: Finca, i: Tablon
  def tsup(f: Finca, i: Int): Int = { // Tiempo de supervivencia
    f(i)._1
  }

  def treg(f: Finca, i: Int): Int = { // Tiempo de regado
    f(i)._2
  }

  def prio(f: Finca, i: Int): Int = { // Prioridad
    f(i)._3
  }


  ///////////////////////////////////////////////////////////////////
  ////////////////////////////////RESOLVER//////////////////////////


  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {

    def calcularTiempoInicio(actual: Int): Int = {
      val indiceActual = pi.indexOf(actual) // El indice de mi tablon actual en la programacion de riego

      if (actual == pi(0))
        0
      else
        calcularTiempoInicio(pi(indiceActual - 1)) + treg(f, pi(indiceActual - 1))
    }

    val tiemposInicioRiego = for (i <- 0 until f.length) yield calcularTiempoInicio(i)

    tiemposInicioRiego.toVector
  }

  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val costoRiego = (ti: Int) => {
      if (tsup(f, i) - treg(f, i) >= ti)//tsup = tiempo de supervivencia
        tsup(f, i) - (ti + treg(f, i))//treg =tiempo de regado
      else
        prio(f, i) * ((ti + treg(f, i)) - tsup(f, i))//prio=prioridad
    }

    costoRiego(tIR(f, pi)(i))
  }

  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    val costos = for (i <- 0 until f.length) yield costoRiegoTablon(i, f, pi)

    def sumarElementos(vector: Vector[Int]): Int = vector match {
      case Vector() => 0
      case head +: tail => head + sumarElementos(tail)
    }

    sumarElementos(costos.toVector)
  }


  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    val n = pi.length // La llamamos como se llama en la formalizacion
    val costos = for (j <- 0 until n - 1) yield d(pi(j))(pi(j + 1))

    def sumarElementos(vector: Vector[Int]): Int = vector match {
      case Vector() => 0
      case head +: tail => head + sumarElementos(tail)
    }

    sumarElementos(costos.toVector)
  }


  def generarProgramacionesRiego(f: Finca):Vector[ProgRiego] = {
    // Dada una f i n c a de n t a b l o n e s , d e v u e l v e t o d a s l a s
    // p o s i b l e s p ro g rama c ion es de r i e g o de l a f i n c a

  }

  def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una f i n c a d e v u e l v e l a p rog ramac ion
    // de r i e g o opt ima

  }


  ////////////////////////PARALELIZACION DE DATOS///////////////////////////////



  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    val costosPar = (0 until f.length).par.map(i => costoRiegoTablon(i, f, pi))
    costosPar.sum
  }

  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    val n = pi.length
    val costosPar = (0 until n - 1).par.map(j => d(pi(j))(pi(j + 1)))//se toma el indice del siguiente tablon
    costosPar.sum
  }


}

