import java.awt.image.BufferedImage
import java.io.File
import java.io.FileInputStream
import java.util.PropertyResourceBundle

import scala.io.Source

import javax.imageio.ImageIO

object Util {
  
  val bundle = new PropertyResourceBundle(new FileInputStream(new File("./compare.config")))
  
  def getProperty(key: String): String = bundle.getString(key)
  
  def rgbError():Int = getProperty("error.rgb").toInt
  
  def pixelError(): Int = getProperty("error.pixel").toInt
  
  def origDir(): String = getProperty("image.original.dir")
  
  def editDir(): String = getProperty("image.edited.dir")
  
  def readFile(fileName: String): List[String] =
    Source.fromFile(fileName).getLines().toList
    
  def formatInput(input: List[String]): List[File] =
    for {
      s <- input filter (entry => ("0" != entry) && !entry.isEmpty())
    } yield new File(editDir + s.trim)

  def writeFile(fileName: String, lines: List[String]) = {
    val out = new java.io.PrintWriter(fileName)
    lines.foreach(line => out.println(line))
    out.close
  }

  def formatOutput(list: List[String]): List[String] =
    ( for {
      i <- 0 until list.length
    } yield "Case " + (i+1) + ": " + list(i) ).toList
  
  //def getFiles(f: File): Array[File] = f.listFiles.filterNot(_.isDirectory).filter(_.getName.endsWith(".bmp"))
  
  def getOrigFiles(): List[File] = 
    new File(origDir).listFiles.filter(_.getName.endsWith(".bmp")).toList
  
  def getImage(file: File): BufferedImage = ImageIO.read(file)

}