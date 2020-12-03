import cats.effect.{Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/*
* We are going to create a process which sleeps an ammount of time
* given in the main method (we will update this to get a console imput)
* and once the process is done sleeping it will print the number and a
* character that we have assigned to it previously and then add 1 to the
* number and sleeping it again till the number reaches 10 then the program
* will stop.
* */

class SleepingProcesses[F[_]] (implicit timer: Timer[F], concurrent: Concurrent[F]) {
  def sleeperLauncher(firstSleepingTime: FiniteDuration, secondSleepingTime: FiniteDuration): F[Unit] = {
    def sleeperMethod(sleepTime: FiniteDuration,selectedCharacter: String): F[Unit] =
      timer.sleep(sleepTime).flatMap(_ =>
        if(sleepTime === 10.seconds)
          concurrent.delay(println(s"I've been sleeping for $sleepTime $selectedCharacter"))
        else {
          println(s"I've been sleeping for $sleepTime $selectedCharacter")
          sleeperMethod(sleepTime.plus(1.second), selectedCharacter)
        }
      )

    for {
      _ <- concurrent.start(sleeperMethod(secondSleepingTime,"A"))
      _ <- concurrent.start(sleeperMethod(firstSleepingTime,"B"))
    } yield ()
  }

}

object SleeperExcersise extends IOApp{
  val sleepingTime1: FiniteDuration = 1.seconds
  val sleepingTime2: FiniteDuration = 2.seconds
  val sleeperProcess: SleepingProcesses[IO] = new SleepingProcesses[IO]

  def run(args: List[String]): IO[ExitCode] = sleeperProcess.sleeperLauncher(sleepingTime1,sleepingTime2).as(ExitCode.Success)
}