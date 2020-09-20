import scala.collection.mutable.Stack

object NumberManipulation {

  implicit def toWord16(byteTup2: Tuple2[Byte, Byte]): Int = {
    // make sure only get lowest 8 bits, and not the padded / signed 1's
    val firstByte = byteTup2._1 & 0xff
    val secondByte = byteTup2._2 & 0xff
    (firstByte << 8) + secondByte
  }

  def toInt(any: Any): Option[Int] = {
    any match {
      case i: Int => Some(i)
      case _      => None
    }
  }

}

object StackUtils {

  implicit class Manipulation[T](s: Stack[T]) {
    def popManyBytes(numBytes: Int): Seq[T] = {
      if (numBytes != 0) {
        if (s.size != 0) {
          return Seq(s.pop).concat(popManyBytes(numBytes - 1))
        }
      }

      return Seq()
    }
  }

  implicit def mergeBytesToInt(s: Seq[Byte]): Int = {
    s.size match {
      case 4 =>
        ((s(0) & 0xff) << (8 * 3)) +
          ((s(1) & 0xff) << (8 * 2)) +
          ((s(2) & 0xff) << (8 * 1)) +
          (s(3) & 0xff)
      case 2 => ((s(0) & 0xff) << 8) + (s(1) & 0xff)
      case 1 => s(0) & 0xff
    }
  }

}
