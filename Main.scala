import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Main {
  def main(args: Array[String]): Unit = {
    test()
  }

  def phototest(img: BufferedImage): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val r = 2 // Blur radius
    for (x <- 0 until w)
      for (y <- 0 until h) {
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
    out
  }

  def test() {
    val photo1 = ImageIO.read(new File("photo.jpg"))

    val photo2 = phototest(photo1)

    ImageIO.write(photo2, "jpg", new File("result.jpg"))
  }
}