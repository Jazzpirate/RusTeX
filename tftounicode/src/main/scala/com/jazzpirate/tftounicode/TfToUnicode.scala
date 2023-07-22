package com.jazzpirate.tftounicode

import java.io.File
import scala.collection.mutable
import scala.sys.process._

// https://www.compart.com/en/unicode

// https://www.cdnfonts.com/
// https://fonts.google.com/
// https://www.onlinewebfonts.com/
// https://fonts.adobe.com/
// Download
// https://www.fontsquirrel.com/
// https://fontesk.com/

object Utils {
  def kpsewhich(s: String*) = {
    val pb = Process("kpsewhich" +: s)
    pb.lazyLines_!.toList.mkString("\n")
  }
  def write(f: File, ls: List[String]): Unit = {
    if (!f.exists()) f.createNewFile()
    val pw = new java.io.PrintWriter(f)
    ls.foreach(pw.println)
    pw.close()
  }

  def fileToStrings(f: File) = {
    val in = scala.io.Source.fromFile(f)
    val lines = in.getLines().toList
    in.close()
    lines
  }

  def openPdf(fname: String) = {
    tempdir.listFiles().foreach(_.delete())
    val tmpfile = new File(tempdir, "font.tex")
    write(tmpfile, texstring.replace("%%FONTNAME%%", fname).split('\n').toList)
    val pb = Process(Seq("pdflatex","-interaction","nonstopmode","-halt-on-error", "font"), tempdir)
    pb.lazyLines_!.toList
    val pdffile = new File(tempdir, "font.pdf")
    if (pdffile.exists()) {
      java.awt.Desktop.getDesktop.open(pdffile)
      true
    } else false
  }

  lazy val tempdir = {
    new File(java.nio.file.Files.createTempDirectory("tftounicode").toAbsolutePath.toString)
  }
  lazy val texstring = {
    val res = scala.io.Source.fromResource("fonts.tex")
    val ret = res.mkString
    res.close()
    ret
  }

  def romanToInt(s: String): Int = {
    val values = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
    var i = s.length - 1
    var result = values(s(i))
    while (i > 0) {
      val curr = values(s(i))
      val prev = values(s(i - 1))
      if (prev < curr) result -= prev
      else result += prev
      i -= 1
    }
    result
  }

  def numToChar(s:String) = {
    val ls = if (s.forall(isHexDigit) && s.length == 12) {
      s.take(4) :: s.slice(4, 8) :: s.drop(8) :: Nil
    } else if (s.forall(isHexDigit) && s.length == 8) {
      s.take(4) :: s.drop(4) :: Nil
    } else s.split(' ').toList
    ls.map(s => Integer.parseInt(s,16)).map(i => new String(Character.toChars(i))).mkString
  }

  private val encfiles = mutable.HashMap.empty[String,Option[File]]
  private val pfbfiles = mutable.HashMap.empty[String,Option[File]]
  private val encodings = mutable.Set.empty[(String,Map[Int,String])]

  def load(s:String,outfile:File) = {
    encodings.find(_._1 == s) match {
      case Some(_) =>
      case None =>
        val out = new File(outfile, s + ".txt")
        if (out.exists()) {
          val filemap = mutable.Map.empty[Int, String]
          fileToStrings(out).zipWithIndex.foreach {
            case (str, i) =>
              filemap(i) = str
              encodings += ((s,filemap.toMap))
          }
        }
    }
  }

  def parseunicode(s:String) : String = {
    val name = if (s.startsWith(".")) s else s.takeWhile(_ != '.')
    glyphmap.get(name) match {
      case Some(char) =>
        char
      case None if name.startsWith("uni") && name.drop(3).forall(isHexDigit) =>
        numToChar(name.drop(3))
      case _ if name.startsWith("u") && name.drop(1).forall(isHexDigit) =>
        numToChar(name.drop(1))
      case _ if name.endsWith("_sub") || name.endsWith("_sup") ||
        name.endsWith("_SUB") || name.endsWith("_SUP") =>
        parseunicode(name.dropRight(4))
      case _ if name.endsWith("_os") =>
        parseunicode(name.dropRight(3))
      case _ if name.endsWith("_SC") =>
        parseunicode(name.dropRight(3))
      case _ if name.endsWith("_swash") || name.endsWith("_short") =>
        parseunicode(name.dropRight(6))
      case _ if name.endsWith("_swash1") =>
        parseunicode(name.dropRight(7))
      case _ if name.startsWith("_") =>
        "???"
      case _ if name.contains("_") =>
        val ret = name.split('_').map(parseunicode)
          if (!ret.contains("???")) ret.mkString else "???"
      case _ if name.startsWith("LinearA") =>
        val r = name.drop(7)
        val i = romanToInt(r) + Integer.parseInt("10600",16) - 1
        new String(Character.toChars(i))
      case _ if name.startsWith("sym") =>
        "???"
      case _ =>
        "???"
    }
  }

  def getEncFromEncFile(f:File,outfile:File) = {
    encodings.find(p => p._1 == f.getName.replace(".enc","")) match {
      case Some((s,_)) => s
      case _ =>
        val lines = fileToStrings(f).filterNot(l => l.startsWith("%") || l.endsWith("[") || l.endsWith("[%") || l.startsWith("]") || l.startsWith("/ECEncoding"))
          .flatMap(s => s.takeWhile(_ != '%').split("\\s")).filter(_.startsWith("/"))
        val filemap = mutable.Map.empty[Int, String]
        var i = 0
        lines.foreach { l =>
          filemap(i) = parseunicode(l.drop(1).trim)
          i += 1
        }
        encodings.find(p => p._2 == filemap) match {
          case Some((s,_)) => s
          case _ =>
            val s = f.getName.replace(".enc", "")
            if (filemap.nonEmpty) {
              val out = new File(outfile, s + ".txt")
              val pw = new java.io.PrintWriter(out)
              val max = filemap.keys.max
              (0 to max).foreach { i =>
                pw.println(filemap.getOrElse(i, ""))
              }
              pw.close()
              encodings += ((s, filemap.toMap))
              s
            } else "ASCII"
        }
    }
  }

  def isHexDigit(c:Char) = c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  def getEncFromPfbFile(f:File,outfile:File) = {
    encodings.find(p => p._1 == f.getName.replace(".pfb","")) match {
      case Some((s,_)) => s
      case _ =>
        val pb = Process(Seq("t1disasm", f.getName), f.getParentFile)
        val lines = pb.lazyLines_!.toList
        val filemap = mutable.Map.empty[Int, String]
        lines.foreach { l =>
          if (l.startsWith("dup") && l.endsWith("put")) {
            val pair = l.drop(3).dropRight(3).split('/').map(_.trim)
            if (pair.length == 2) {
              val index = pair(0).toInt
              filemap(index) = parseunicode(pair(1).trim)
            }
          }
        }
        encodings.find(p => p._2 == filemap) match {
          case Some((s,_)) => s
          case _ =>
            val s = f.getName.replace(".pfb","")
            if (filemap.nonEmpty) {
              val out = new File(outfile, s + ".txt")
              val pw = new java.io.PrintWriter(out)
              val max = filemap.keys.max
              (0 to max).foreach { i =>
                pw.println(filemap.getOrElse(i, ""))
              }
              pw.close()
              encodings += ((s, filemap.toMap))
              s
            } else "ASCII"
        }
    }
  }

  def getEncFile(s:String) = encfiles.getOrElseUpdate(s,{
    val f = new File(kpsewhich(s))
    if (f.exists()) Some(f) else None
  })
  def getPfbFile(s:String) = pfbfiles.getOrElseUpdate(s,{
    val f = new File(kpsewhich(s))
    if (f.exists()) Some(f) else None
  })

  lazy val glyphmap = {
    val src = scala.io.Source.fromResource("glyphlist.txt")
    val txt = src.mkString.split('\n')
    src.close()
    val map = mutable.HashMap.empty[String, String]
    txt.foreach { l =>
      if (!l.startsWith("#") && l.nonEmpty) {
        l.split(';').toList match {
          case name :: code :: _ if code.startsWith("STRING ") =>
            map(name) = code.drop(7)
          case name :: code :: _ if code == "STRING" =>
            map(name) = ""
          case name :: code :: Nil =>
            map(name) = numToChar(code)
          case code :: name :: _ :: Nil =>
            map(name) = numToChar(code)
          case _ =>
            ???
        }
      }
    }

    map
  }

  var skip = false

}
import Utils._

class Font(name: String, encfilename:Option[String], pfbfilename: Option[String]) {
  lazy val tfm = {
    val f = new File(kpsewhich(name+".tfm"))
    if (f.exists()) Some(f) else None
  }
  lazy val encfile = encfilename.flatMap(getEncFile)
  lazy val pfbfile = pfbfilename.flatMap(getPfbFile)
  def get(outfile:File,oldchars:Option[String]) = tfm match {
    case Some(_) if openPdf(name) => encfile match {
      case Some(e) =>
        val chars = if (oldchars.isDefined) oldchars.get else if (skip) "l" else {
          print("\n(s)ans, (i)talic, (b)old, (S)cript, (C)apital, (m)onospace, (B)lackboard, (f)raktur, (x)none: ")
          val c = scala.io.StdIn.readLine()
          if (c == "q") scala.sys.exit()
          if (c == "l") skip = true
          c
        }
        Some((name,getEncFromEncFile(e,outfile),chars))
      case _ => pfbfile match {
        case None => None
        case Some(e) =>
          val chars = if (oldchars.isDefined) oldchars.get else if (skip) "l" else {
            print("\n(s)ans, (i)talic, (b)old, (S)cript, (C)apital, (m)onospace, (B)lackboard, (f)raktur, (x)none: ")
            val c = scala.io.StdIn.readLine()
            if (c == "q") scala.sys.exit()
            if (c == "l") skip = true
            c
          }
          Some((name,getEncFromPfbFile(e,outfile),chars))
      }
    }
    case _ => None
  }
}

object TfToUnicode {
  def main(args: Array[String]): Unit = {

    val outfile = new File(args(0))
    outfile.mkdirs()

    var allfonts : List[(String,String,String)] = Nil
    val allfiles = new File(outfile,"00index.txt")
    if (allfiles.exists()) {
      allfonts = fileToStrings(allfiles).map{ l =>
        val entries = l.split(' ')
        val name = entries(0)
        val map = entries(1)
        load(map,outfile)
        val chars = entries(2)
        (name,map,chars)
      }.sortBy(_._1)
    }

    val pdftexmap = fileToStrings(new File(kpsewhich("pdftex.map")))
      .filterNot(_.startsWith("%")).sorted
    pdftexmap.zipWithIndex.foreach {
      case (l, i) =>
        val entries = l.split(' ')
        val name = entries(0)
        val af = allfonts.find(_._1 == name)
        if (af.isEmpty || (!skip && af.get._3 == "l") || !(new File(outfile,af.get._2 + ".txt").exists())) {
          af match {
            case Some((a,b,c)) =>
              val j = allfonts.indexOf((a,b,c))
              allfonts = allfonts.take(j) ::: allfonts.drop(j + 1)
            case _ =>
          }
          val encfilename = entries.find(_.endsWith(".enc")).map(_.drop(1)) match {
            case Some(s) if s.startsWith("[") => Some(s.drop(1))
            case o => o
          }
          val pfbfilename = entries.find(_.endsWith(".pfb")).map(_.drop(1))
          print(s"\rProcessing font ${i + 1} of ${pdftexmap.size}: $name ($encfilename,$pfbfilename)                         ")
          val font = new Font(name, encfilename, pfbfilename)
          font.get(outfile,if (af.exists(_._3 == "l")) None else af.map(_._3)).foreach { t =>
            allfonts = (t :: allfonts).sortBy(_._1)
            write(allfiles, allfonts.map { case (n, m, c) => s"$n $m $c" })
          }
        }
    }

    /*
    val map = glyphmap
    val (pfbfiles,tfmmap) = findFilesInMap(outfile)

    println(s"Found ${pfbfiles.size} .pfb files")

    pfbfiles.toList.zipWithIndex.foreach {
      case (f, i) =>
        println(s"Processing file ${i + 1} of ${pfbfiles.size}: ${f.getName}")
        dopfbfile(f, map, tfmmap, outfile)
    }


    //val topfile = getTopFile


    //println(s"Finding files in $topfile")
    //val allfiles = findFiles(topfile)
    //val pfbfiles = allfiles.filter(f => f.getName.endsWith(".pfb") &&
    //  allfiles.exists(g => g.getName.endsWith(".tfm") && g.getName.replace(".tfm","") == f.getName.replace(".pfb","")))
    */
  }




  def findFilesInMap(outfile: File): (Set[File],mutable.HashMap[String,String]) = {
    val pfbfiles = new File(outfile, "00index.txt")
    val tfmfiles = new File(outfile,"00tfmindex.txt")
    val tfmmap = mutable.HashMap.empty[String,String]
    if (pfbfiles.exists() && tfmfiles.exists()) {
      val pfbs = fileToStrings(pfbfiles).map(new File(_)).toSet
      fileToStrings(tfmfiles).foreach{
        l =>
          val pair = l.split(" ")
          tfmmap(pair(0)) = pair(1)
      }
      return (pfbs,tfmmap)
    }
    val f = new File(kpsewhich("pdftex.map"))
    val lines = fileToStrings(f)
    val donepfbs = mutable.Map.empty[String,Boolean]
    val donetfms = mutable.Set.empty[String]
    val files = lines.zipWithIndex.flatMap { case (l, i) =>
      print(s"\rChecking font map entry ${i + 1}/${lines.length + 1}...   ")
      if (l.startsWith("%")) Nil
      else {
        val entries = l.split(' ')
        val tfm = entries(0)
        if (!donetfms.contains(tfm)) {
          donetfms += tfm
          val file = new File(kpsewhich(tfm + ".tfm"))
          if (file.exists()) {
            val pfb = entries.find(e => e.startsWith("<") && e.endsWith(".pfb")).map(_.drop(1))
            pfb.flatMap(donepfbs.get) match {
              case Some(true) =>
                tfmmap(tfm) = pfb.get
                Nil
              case Some(false) =>
                Nil
              case None if pfb.isEmpty =>
                Nil
              case None =>
                val pfbfile = new File(kpsewhich(pfb.get))
                if (pfbfile.exists()) {
                  tfmmap(tfm) = pfb.get
                  donepfbs(pfb.get) = true
                  List(pfbfile)
                }
                else {
                  donepfbs(pfb.get) = false
                  Nil
                }
            }
          } else Nil
        } else Nil
      }
    }

    write(pfbfiles,files.map(_.getAbsolutePath))
    write(tfmfiles,tfmmap.toList.map{case (k,v) => s"$k $v"})

    (files.toSet,tfmmap)


/*
        val fn = l.split('<').last
        if (donepfbs.contains(fn)) Nil else {
          donepfbs += fn
          val file = new File(kpsewhich(fn))
          if (file.exists()) {
            val tfm = new File(kpsewhich(fn.replace(".pfb", ".tfm")))
            if (tfm.exists()) List(file) else Nil
          }
          else Nil
        }
      }
    }

 */
  }

  def dopfbfile(f: File, charmap: mutable.HashMap[String, String],tfmmap:mutable.HashMap[String,String], outdir: File): Unit = {
    val fname = tfmmap.find(p => p._2 == f.getName) match {
      case None => return
      case Some((s,_)) =>
        println(s" - $s")
        s
    }
    if (!openPdf(fname)) {
      return
    }
    val pb = Process(Seq("t1disasm", f.getName), f.getParentFile)
    val lines = pb.lazyLines_!.toList
    val filemap = mutable.Map.empty[Int, String]
    lines.foreach { l =>
      if (l.startsWith("dup") && l.endsWith("put")) {
        val pair = l.drop(3).dropRight(3).split('/').map(_.trim)
        if (pair.length != 2) {
          ???
        }
        val name = pair(1)
        val index = pair(0).toInt
        filemap(index) = charmap.get(name) match {
          case Some(char) =>
            char
          case _ =>
            "???"
        }
      }
    }
    if (filemap.nonEmpty) {
      val out = new File(outdir, f.getName.replace(".pfb", ".txt"))
      val pw = new java.io.PrintWriter(out)
      val max = filemap.keys.max
      (0 to max).foreach { i =>
        pw.println(filemap.getOrElse(i, ""))
      }
      pw.close()
    }
  }


/*
  def findFiles(f: File): List[File] = if (f.isDirectory) {
    f.listFiles().flatMap(findFiles).toList
  } else if (f.getName.endsWith(".pfb") || f.getName.endsWith(".tfm")) {
    List(f)
  } else Nil

  def getTopFile = {
    val initdir = new File(kpsewhich("cmr10.tfm"))
    initdir.getParentFile.getParentFile.getParentFile.getParentFile
  }
 */



}
