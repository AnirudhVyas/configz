package me.free.config
import cats.data.{Kleisli, Reader}
import cats.effect.IO
import cats.kernel.Monoid
import com.typesafe.config.{Config, ConfigFactory}
import scala.reflect.ClassTag
import scala.util.Try
/**
 * Basic typeclasses for cleaner, better and safer [[Config]].
 *
 * @author ricky.nj@gmail.com
 */
object ConfigTypeclasses {
  import scala.collection.JavaConverters._
  /**
   * Configuration monoid.
   */
  implicit object ConfigMonoid extends Monoid[Config] {
    override def empty: Config = ConfigFactory.empty()
    override def combine(x: Config, y: Config): Config = {
      val z = x.entrySet().asScala.map { e => e.getKey -> e.getValue }.toMap ++
        y.entrySet().asScala.map { e => e.getKey -> e.getValue }.toMap
      ConfigFactory.parseMap(z.asJava)
    }
  }
  /**
   * Generic catch all failure for reads.
   *
   * @param msg   message provided.
   * @param cause [[Throwable]] cause for [[Config]] reading.
   */
  final case class ConfigReadFailure(msg: String, cause: Throwable) extends Throwable
  /**
   * Read from a path.
   *
   * @param path
   */
  implicit class RichConfigString(path: String) {
    def load: IO[Either[ConfigReadFailure, Config]] = IO[Either[ConfigReadFailure, Config]] {
      Try(ConfigFactory.load(path)).toEither match {
        case Left(value) => Left(ConfigReadFailure(s"failed to read config at $path", value))
        case Right(t) => Right(t)
      }
    }
  }
  object Select {
    type Select[A] = Kleisli[IO, Config, Either[ConfigReadFailure, A]]
    def apply[A](run: Config => IO[Either[ConfigReadFailure, A]]): Select[A] = Kleisli[IO, Config, Either[ConfigReadFailure, A]](run)
    def lift[A](r: Reader[Config, IO[Either[ConfigReadFailure, A]]]): Select[A] = Select[A](r.run)
  }
  implicit class RichConfig(x: Config) {
    def <+>(other: Config)(implicit m: Monoid[Config]): Config = m.combine(x, other)
    def identityReader: Reader[Config, Config] = Reader[Config, Config](identity[Config])
    /**
     * Selects a unique path.
     *
     * @param path path provided
     * @tparam A type of result value derived out of configuration.
     * @return instance of  [[Select.Select]] which can then be combined to do for yields.
     */
    def selectUnique[A: ClassTag](path: String): Select.Select[A] = Select[A] { cfg =>
      IO {
        Try(cfg.getAnyRef(path)).toEither match {
          case Left(t) => Left(ConfigReadFailure(s"failed at $path -- doesn't exist", t))
          case Right(v) => Right(v.asInstanceOf[A])
        }
      }
    }
  }
}