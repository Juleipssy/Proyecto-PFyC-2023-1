import scala.util.Random

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
    val v = Vector.fill(long){
      (random.nextInt(long * 2) + 1,
        random.nextInt(long) + 1,
        random.nextInt(4) + 1) }
    v
  }

  def distanciaAlAzar(long: Int): Distancia = {
    // Crea una matriz de distancias para una finca
    // de long tablones, con valores aleatorios entre
    // 1 y long*3
    val v = Vector.fill(long, long){
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

  /*
  //          POR RESOLVER

  /*         Funcion base          */
  // 2.3.1 - Calculando el tiempo de inicio de Riego
  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    // Dada una finca f y una programación de riego pi,
    // y f.length == n, tIR(f, pi) devuelve t: TiempoInicioRiego
    // tal que t(i) es el tiempo en que inicia el riego del
    // tablón i de la finca f según pi
    // ...
  }

  /*         Funciones secuenciales          */
  // 2.3.2 Calculando costos - costoRiegoTablon ---- (* Viernes 16 - Junio)
  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    // devuelve el costo de regar el tablon i de la finca f
    // con la programación pi
  }

  // 2.3.2 Calculando costos - costoRiegoFinca ---- (* Viernes 16 - Junio)
  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    // devuelve el costo total de regar una finca dada
    // una programación de riego dada
    // ...
  }

  // 2.3.2 Calculando costos - costoMovilidad ---- (* Viernes 16 - Junio)
  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    // ...
  }

  // 2.3.3 Generando programaciones de Riego ---- (* Viernes 16 - Junio)
  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
    // Dada una finca de tablones, devuelve todas las posibles programaciones de riego de la finca
    // ...
  }

  // 2.3.3 Generando una programación de Riego óptimo ---- (* Viernes 23 - Junio)
  def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una finca devuelve la programación de riego óptima
    // ...
  }

  /*         Versiones paralelas          */
  // 2.4.1 Paralelizando el cálculo de los costos - Costo de Riego ---- (* Viernes 23 - Junio)
  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    // Devuelve el costo total de regar una finca dada una programación de riego pi
    // ...
  }

  // 2.4.1 Paralelizando el cálculo de los costos - Costo de movilidad ---- (* Viernes 23 - Junio)
  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    // ...
  }

  // 2.4.2 Paralelizando la programación de riego óptimo ---- (* Viernes 23 - Junio)
  def generarProgramacionesRiegoPar(f: Finca): Vector[ProgRiego] = {
    // Paraleliza el calculo de la versión secuencial
    // Devuelve el Vector de ProgRiego generado
  }

  // 2.4.3 Paralelizando la programación de riego óptimo ---- (* Viernes 23 - Junio)
  def ProgramacionRiegoOptimoPar(f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una finca devuelve la programación de riego óptima
    // ...
  }
  */
}
