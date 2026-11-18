package hearth.kindlings.pureconfigderivation

import java.io.DataInputStream

/** Reads the bytecode size of every method of compiled classes (the length of each method's `Code` attribute), so tests
  * can check that derived instances stay below HotSpot's 8000-byte JIT limit (`-XX:-DontCompileHugeMethods`).
  *
  * JVM only: it reads the `.class` files from the classpath.
  */
object BytecodeSizes {

  /** HotSpot does not JIT-compile a method whose bytecode is larger than this. */
  val HugeMethodLimit: Int = 8000

  /** `(class name, method name, bytecode length)` of every method of `cls` and of its nested/anonymous classes. */
  def methodSizes(cls: Class[?]): List[(String, String, Int)] =
    classFileNames(cls).flatMap { name =>
      val in = cls.getClassLoader.getResourceAsStream(name)
      try parse(new DataInputStream(in)).map { case (method, size) => (name, method, size) }
      finally in.close()
    }

  /** Class files of `cls` and of the classes nested in it (`Outer$...`), found next to `cls`'s class file. */
  private def classFileNames(cls: Class[?]): List[String] = {
    val path = cls.getName.replace('.', '/')
    val dir = path.substring(0, path.lastIndexOf('/') + 1)
    val prefix = path.substring(dir.length).stripSuffix("$")
    val url = cls.getClassLoader.getResource(path + ".class")
    val siblings: List[String] = url.getProtocol match {
      case "file" =>
        Option(new java.io.File(url.toURI).getParentFile.list()).toList.flatten
      case "jar" =>
        val jar = url.openConnection().asInstanceOf[java.net.JarURLConnection].getJarFile
        val entries = jar.entries()
        val names = List.newBuilder[String]
        while (entries.hasMoreElements) {
          val entry = entries.nextElement().getName
          if (entry.startsWith(dir) && entry.indexOf('/', dir.length) < 0) names += entry.substring(dir.length)
        }
        names.result()
      case other => throw new IllegalStateException(s"Cannot list class files from a $other URL: $url")
    }
    siblings
      .filter(f => f.endsWith(".class") && (f == prefix + ".class" || f.startsWith(prefix + "$")))
      .map(dir + _)
      .sorted
  }

  /** `(method name, bytecode length)` of every method with code in a class file. */
  private def parse(in: DataInputStream): List[(String, Int)] = {
    def skip(n: Int): Unit = in.readFully(new Array[Byte](n))
    skip(8) // magic, minor and major version
    val constantPoolCount = in.readUnsignedShort()
    val utf8 = new Array[String](constantPoolCount)
    var i = 1
    while (i < constantPoolCount) {
      in.readUnsignedByte() match {
        case 1                          => utf8(i) = in.readUTF()
        case 3 | 4                      => skip(4)
        case 5 | 6                      => skip(8); i += 1 // long and double take two entries
        case 7 | 8 | 16 | 19 | 20       => skip(2)
        case 9 | 10 | 11 | 12 | 17 | 18 => skip(4)
        case 15                         => skip(3)
        case tag                        => throw new IllegalStateException(s"Unknown constant pool tag $tag")
      }
      i += 1
    }
    skip(6) // access flags, this class, super class
    skip(2 * in.readUnsignedShort()) // interfaces
    def members(): List[(String, Int)] =
      List.fill(in.readUnsignedShort()) {
        skip(2) // access flags
        val name = utf8(in.readUnsignedShort())
        skip(2) // descriptor
        var codeLength = -1
        (0 until in.readUnsignedShort()).foreach { _ =>
          val attributeName = utf8(in.readUnsignedShort())
          val length = in.readInt()
          if (attributeName == "Code") {
            skip(4) // max stack, max locals
            codeLength = in.readInt()
            skip(length - 8)
          } else skip(length)
        }
        (name, codeLength)
      }
    val _ = members() // fields
    members().filter(_._2 >= 0)
  }
}
