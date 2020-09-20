import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Stack
import scala.io.Source
import scala.collection.immutable.LazyList.cons

object Main extends App {

  import NumberManipulation._
  import StackUtils.mergeBytesToInt

  // check for provided file paths
  if (args.length == 0) {
    println("usage: scalajvm [classfiles] ...\n")
    System.exit(1)
  }

  // transform to stack structure
  val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(args(0)))
  val loader: Loader = Loader(Stack(byteArray: _*))

  // parse cafebabe magic signature
  val cafeBabe: Seq[Byte] = loader.loadBytes(4)

  // parse versions
  val majorVersion = loader.loadBytes(2)
  val minorVersion = loader.loadBytes(2)

  val cp: ConstantPool = loader.parseConstantPool()

  val loadedClass: Class = Class(
    ConstantPool = cp,
    Flags = loader.loadBytes(2),
    Name = cp.resolve(loader.loadBytes(2)),
    Super = cp.resolve(loader.loadBytes(2)),
    Interfaces = loader.parseInterfaces(cp),
    Fields = loader.parseFields(cp),
    Methods = loader.parseFields(cp),
    Attributes = loader.parseAttrs(cp)
  )

  val frame: Frame = loadedClass
    .MakeFrame("add", 2, 3)
    .getOrElse(throw new IllegalArgumentException("method not found"))

  val result = Class.Exec(frame)

  println(s"Result: $result")
}
