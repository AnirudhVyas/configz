package free.me.config
import cats.kernel.Monoid
import com.typesafe.config.{Config, ConfigFactory}
import me.free.config.ConfigTypeclasses._
import org.scalacheck.{Gen, Prop, Properties}
/**
 * Config monoidal laws
 */
object ConfigMonoidLaws extends Properties("ConfigMonoidLaws") {
  val configGen: Gen[Config] = Gen.oneOf(List(ConfigFactory.parseString(
    """
      |configz {
      | k1 = "v1"
      | k2 = 0L
      |}
      |""".stripMargin)))
  val configTwoGen: Gen[Config] = Gen.oneOf(List(ConfigFactory.parseString(
    """
      |configz {
      | k3 = "v3"
      | k4 = 1000L
      |}
      |""".stripMargin)))
  val configThreeGen: Gen[Config] = Gen.oneOf(List(ConfigFactory.parseString(
    """
      |configz {
      | k5 = "v6"
      | k6 = 1001L
      |}
      |""".stripMargin)))
  val identityCfg: Config = ConfigFactory.empty()
  property("identity") = Prop.forAll(configGen) { cfg1 =>
    Monoid[Config].combine(cfg1, identityCfg) == cfg1
  }
  property("associative") = Prop.forAll(configGen, configTwoGen, configThreeGen) { (cfg1, cfg2, cfg3) =>
    val z = Monoid[Config].combine(cfg1, cfg2)
    val y = Monoid[Config].combine(cfg2, cfg3)
    cfg3 <+> z == cfg1 <+> y
  }
}
