import scala.collection.mutable.Stack

case class Constant(
    val tag: Byte = 0,
    val nameIndex: Int = 0,
    val classIndex: Int = 0,
    val nameAndTypeIndex: Int = 0,
    val stringIndex: Int = 0,
    val descIndex: Int = 0,
    val string: String = ""
)

case class ConstantPool(cp: Seq[Constant]) {
  def resolve(idx: Int): String = {
    if (cp(idx - 1).tag == 0x01) { // nasty off-by-one gotcha here!
      return cp(idx - 1).string
    }
    return ""
  }
}

case class Field(
    flags: Int,
    name: String,
    descriptor: String,
    attributes: Seq[Attribute]
)

// Attributes contain addition information about fields and classes
// The most useful is "Code" attribute, which contains actual byte code
case class Attribute(
    name: String,
    data: Seq[Byte]
)

case class Loader(data: Stack[Byte]) {

  import StackUtils._

  def loadBytes(numBytes: Int): Seq[Byte] =
    data.popManyBytes(numBytes)

  def parseConstantPool(): ConstantPool = {
    // get constant pool size
    val constPoolCount: Int = loadBytes(2)

    // parse constant pool
    // Valid constant pool indices start from 1
    ConstantPool {
      (1 to (constPoolCount - 1)).map { _ =>
        val tag: Int = loadBytes(1)

        tag match {
          case 0x01 => {
            // UTF8 string literal, 2 bytes length + data
            val stringData: Seq[Byte] = loadBytes(loadBytes(2))
            Constant(
              tag = tag.toByte,
              string = stringData.map(_.toChar).mkString("")
            )
          }
          case 0x07 => {
            // Class index
            val nameIndex: Int = loadBytes(2)
            Constant(tag = tag.toByte, nameIndex = nameIndex)
          }
          case 0x08 => {
            // String reference index
            val stringIndex: Int = loadBytes(2)
            Constant(tag = tag.toByte, stringIndex = stringIndex)
          }
          case 0x09 | 0x0a => {
            // Field and method: class index + NaT index
            val classIndex: Int = loadBytes(2)
            val nameAndTypeIndex: Int = loadBytes(2)
            Constant(
              tag = tag.toByte,
              classIndex = classIndex,
              nameAndTypeIndex = nameAndTypeIndex
            )
          }
          case 0x0c => {
            // Name-and-type
            val nameIndex: Int = loadBytes(2)
            val descIndex: Int = loadBytes(2)
            Constant(
              tag = tag.toByte,
              nameIndex = nameIndex,
              descIndex = descIndex
            )
          }
          case _ => {
            println(s"unsupported tag: $tag")
            Constant(tag = tag.toByte)
          }
        }
      }
    }
  }

  def parseInterfaces(cp: ConstantPool): Seq[String] = {
    val interfaceCount: Int = loadBytes(2)

    (0 to (interfaceCount - 1)).map(_ => cp.resolve(loadBytes(2)))
  }

  def parseFields(cp: ConstantPool): Seq[Field] = {
    val fieldsCount: Int = loadBytes(2)
    (0 to (fieldsCount - 1)).map { _ =>
      Field(
        flags = loadBytes(2),
        name = cp.resolve(loadBytes(2)),
        descriptor = cp.resolve(loadBytes(2)),
        attributes = parseAttrs(cp)
      )
    }
  }

  def parseAttrs(cp: ConstantPool): Seq[Attribute] = {
    val attributesCount: Int = loadBytes(2)
    (0 to (attributesCount - 1)).map { _ =>
      Attribute(
        name = cp.resolve(loadBytes(2)),
        data = loadBytes(loadBytes(4))
      )
    }
  }

}

case class Frame(
    Class: Class,
    var IP: Int = 0,
    Code: Seq[Byte],
    Locals: Seq[Any],
    Stack: Stack[Any]
)

case class Class(
    ConstantPool: ConstantPool,
    Name: String,
    Super: String,
    Flags: Int,
    Interfaces: Seq[String],
    Fields: Seq[Field],
    Methods: Seq[Field],
    Attributes: Seq[Attribute]
) {

  def MakeFrame(method: String, args: Any*): Option[Frame] = {
    val f = for {
      m <- Methods
      if (m.name == method)

      attr <- m.attributes
      if (attr.name == "Code" && attr.data.size > 8)

      code = attr.data.slice(8, attr.data.size - 1)
      maxLocals = Seq(attr.data(2), attr.data(3))
    } yield (
      Frame(
        Class = this,
        Code = code,
        Locals = args,
        Stack = Stack()
      )
    )

    if (f.size == 0) {
      println("method not found")
      None
    }

    Some(f(0)) // get found frame
  }
}

object Class {

  import NumberManipulation._

  def Exec(f: Frame): Any = {
    while (true) {
      val op = f.Code(f.IP)
      println(s"OP: $op STACK: ${f.Stack.mkString(" ")}")

      op match {
        case 26 => // iload_0
          f.Stack.push(f.Locals(0))
        case 27 => // iload_1
          f.Stack.push(f.Locals(1))
        case _ if op == 96.toByte => {
          val a: Int = toInt(f.Stack.pop).getOrElse(0)
          val b: Int = toInt(f.Stack.pop).getOrElse(0)

          f.Stack.push(a + b)
        }
        case _ if op == 172.toByte => { // ireturn
          return f.Stack.pop
        }
      }

      f.IP = f.IP + 1
    }
  }
}
