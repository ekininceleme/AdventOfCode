package utils

import cats.effect.IO
import fs2.io.file.{Files, Path}

object Utils {
  def readFile(filePath: String): fs2.Stream[IO, String] =
    Files[IO].readUtf8Lines(Path(filePath))
}
