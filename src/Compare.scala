import Util._
import java.awt.image.BufferedImage
import java.io.File
import java.awt.image.Raster

object Compare {
  type Pixel = (Int, Int, Int)

  def main(args: Array[String]): Unit = {
    val files = formatInput(readFile(args(0)))
    val matches = solution(files)
    matches.foreach(println)
    writeFile(args(1), formatOutput(matches))
  }

  def solution(files: List[File]): List[String] = for ( edit <- files ) yield compareWith(edit)

  def compareWith(edited: File): String =
    getOrigFiles.find(f => process(f, edited)).map(_.getName).getOrElse("NONE")
  
  def process(o: File, e: File): Boolean = try {
    println("[Edited Image: " + e + "] ==>> [Original Image: " + o + "]")
    compareVariants(getImage(o), getImage(e))
  } catch {
    case _ => false
  }
  
  def compareVariants(orig: BufferedImage, edit: BufferedImage): Boolean =
    if (! equalSize(orig, edit)) false
    else if (compare(id)(orig, edit)) true
    else if (compare(invert)(orig, edit)) true
    else if (compare(grayscale)(orig, edit)) true
    else false
  
  def equalSize(orig: BufferedImage, edit: BufferedImage): Boolean = 
    (orig.getWidth == edit.getWidth) && (orig.getHeight == edit.getHeight)
  
  def id(p: Pixel): Pixel = p
  
  def invert(p: Pixel): Pixel = (255 - p._1, 255 - p._2, 255 - p._3)
  
  def grayscale(p: Pixel): Pixel = {
    val avg: Int = (0.3 * p._1 + 0.59 * p._2 + 0.11 * p._3).toInt
    (avg, avg, avg)
  }

  def compare(trans: Pixel => Pixel)(orig: BufferedImage, edited: BufferedImage): Boolean = {
    val pixelPair = toPixels(orig.getData).zip(toPixels(edited.getData))
    val diff = for ( (orig, edit) <- pixelPair ) yield pixelDiff(trans(orig), edit)
    stats(diff, pixelPair)
  }
  
  def toPixels(data: Raster): List[(Int, Int, Int)] =
    (for {
      y <- data.getMinY until data.getHeight
      x <- data.getMinX until data.getWidth
    } yield pixelTuple(data.getPixel(x, y, new Array[Int](3)))).toList

  def pixelTuple(arr: Array[Int]): Pixel = arr.toList match {
    case r::g::b::Nil => (r, g, b)
    case _ => (-1, -1, -1)		// error case, not sure what to return
  }
  
  def pixelDiff(a: Pixel, b: Pixel): Int = {
    abs(a._1 - b._1) + abs(a._2 - b._2) + abs(a._3 - b._3)
  }
  
  def abs(a: Int): Int = if (a < 0) -a else a
  
  def stats(diff: List[Int], pp: List[(Pixel, Pixel)]): Boolean = {
    val sumOrigPixel: Float = ( for ( (o, e) <- pp ) yield o._1 + o._2 + o._3 ).sum
    val numPixel: Float = diff.length
    val pixelsInDiff: Float = diff.filter(_ != 0).length
    val sumDiff: Float = diff.sum
    val rgbDeltaPixel: Float = if (0 == pixelsInDiff) 0 else (sumDiff / 3) / (pixelsInDiff)
    val rgbDelta: Float = (sumDiff * 100) / (sumOrigPixel)
    val pixelsInError: Float = (pixelsInDiff * 100) / (numPixel)
    
    println("\t[RGB delta: " + rgbDelta + "%][Faulty pixels: " + pixelsInError + "%][RGB delta / pixel: " + rgbDeltaPixel + "]")
    
    if (rgbDelta < rgbError) true else false
  }
}