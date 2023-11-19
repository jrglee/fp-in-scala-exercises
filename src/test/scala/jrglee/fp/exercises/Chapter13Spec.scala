package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter12.Monad
import jrglee.fp.exercises.Chapter13._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class Chapter13Spec extends AnyFreeSpec with Matchers {

  "13.1" - {
    "should create a monad" in {
      val m = freeMonad[Function0]
      m.unit(1) shouldBe Return(1)
    }
  }

  "13.2" - {
    "should run trampoline with monad" in {
      val m = freeMonad[Function0]
      runTrampoline(m.unit(0)) shouldBe 0
      runTrampoline(m.map(m.unit(0))(_ + 1)) shouldBe 1
    }

    "should run trampoline with suspend" in {
      runTrampoline(Suspend(() => 10)) shouldBe 10
    }
  }

  "13.3" - {
    "should run with a monad" in {
      implicit val m: Monad[Option] = Monad.optionMonad

      Chapter13.run(Return[Option, Int](10)) shouldBe Some(10)
      Chapter13.run(Suspend(Option(10))) shouldBe Some(10)
      Chapter13.run(Suspend(Option(10)).flatMap(a => Return[Option, Int](a * 2))) shouldBe Some(20)
      Chapter13.run(Return[Option, Int](10).flatMap(a => Return[Option, Int](a * 2)).map(_ + 1)) shouldBe Some(21)
    }
  }

  "13.4" - {
    "should run free console" in {
      val stream = new ByteArrayOutputStream()

      scala.Console.withOut(stream) {
        runConsole(Suspend(PrintLine("hello world")))
      }

      new String(stream.toByteArray, StandardCharsets.UTF_8) shouldBe "hello world\n"
    }
  }
}
