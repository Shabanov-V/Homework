import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Main {
  def main(args: Array[String]): Unit = {
    test()
  }

  val r = 2 // Blur radius
  val NumOfThreads = 10
  var w = 1
  var h = 1
  var photo1 = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
  var out = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)


  class MyRunnable(start: Int, finish: Int) extends Runnable {
    override def run() {
      blur(photo1, true, start, finish)
    }
  }


  def blur_pixel(img: BufferedImage, x: Int, y: Int, r: Int) : Unit = {
    var tmp       = 1
    var cur_red   = (img.getRGB(x, y) & 0xff0000) / 65536
    var cur_green = (img.getRGB(x, y) & 0xff00) / 256
    var cur_blue  = (img.getRGB(x, y) & 0xff)
    for (t1 <- math.max((x - r), 0) until math.min((x + r), w))
      for (t2 <- math.max((y - r), 0) until math.min((y + r), h)) {
        cur_red   += (img.getRGB(t1, t2) & 0xff0000) / 65536
        cur_green += (img.getRGB(t1, t2) & 0xff00) / 256
        cur_blue  += img.getRGB(t1, t2) & 0xff
        tmp       += 1
      }
    out.setRGB(x, y, ((cur_red / tmp * 65536) + (cur_green / tmp * 256) + cur_blue / tmp))
  }
  def blur(img: BufferedImage, isHoriz: Boolean, from: Int, to: Int): BufferedImage = {
    var f1, f2, t1, t2 = 0
    if (isHoriz) {
      f1 = 0
      t1 = w
      f2 = from
      t2 = to
    }

    for (x <- f1 until t1)
      for (y <- f2 until t2) {
        blur_pixel(img, x, y, r);
      }
    out
  }

  def test() {
    photo1 = ImageIO.read(new File("photo.jpg"))

    w = photo1.getWidth
    h = photo1.getHeight

    out = photo1

    //blur(photo1, true, 0, h)
    val s = 0
    val threads =
      for (i <- 1 to NumOfThreads)
        yield new Thread(new MyRunnable(s + (i - 1) * (h / NumOfThreads), s + i * (h / NumOfThreads)))
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    /*
    for (i <- 0 until h) {
      start = finish
      finish = finish + 1
      val thread = new Thread (new MyRunnable)
      thread.start
    }*/

    ImageIO.write(out, "jpg", new File("result.jpg"))
  }
}
