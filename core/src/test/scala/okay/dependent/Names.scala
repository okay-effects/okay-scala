package okay.dependent

// A DEPENDENT'S OWN NAMES, in a file of its package that imports nothing: the names of the library's evidence and
// machinery, which nobody writes. `NamesSuite`, beside it, says `import okay.scala3.*`

final case class Known(name: String)

object Known:
  val all: List[Known] = List(Known("one"), Known("two"))

trait Fits:
  def fits: Boolean

object Natural:
  val first: Int = 1

type Unplaced = Option[String]
type Unsaid = String

object Branches:
  def of(n: Int): List[Int] = List.range(0, n)

object SplitFound
object FitsInto
object NaturalFirst
object NaturalInit
object StateWords
object ReaderWords
object AbortWords
object ChooseWords
