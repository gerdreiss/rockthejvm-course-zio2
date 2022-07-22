package lessons

import utils.*
import zio.*
import zio.stm.*

// STM = Software Transactional Memory
object TransactionEffects extends ZIOAppDefault:

  // STM = "atomic effects"
  val success: USTM[Int]               = STM.succeed(42)
  val failure: STM[Exception, Nothing] = STM.fail(new Exception("error"))
  val attempt: TaskSTM[Int]            = STM.attempt(42 / 0)

  // STM vs ZIO
  // compose STMs to obtain other STMs
  // evaluation is fully atomic
  // evaluate = commit
  val anAtomicEffect: Task[Int] = attempt.commit

  // example
  def buggyTransferMoney(
      sender: Ref[Long],
      receiver: Ref[Long],
      amount: Long
  ): ZIO[Any, String, Long] =
    for
      senderBalance <- sender.get.debugThread("sender")
      _             <- if senderBalance < amount then ZIO.fail("Insufficient funds")
                       else sender.update(_ - amount) *> receiver.update(_ + amount)
      newBalance    <- receiver.get.debugThread("receiver")
    yield newBalance

  def exploitBuggyBank: IO[String, Long] =
    for
      sender   <- Ref.make(1000L)
      receiver <- Ref.make(0L)
      fib1     <- buggyTransferMoney(sender, receiver, 1000L).fork
      fib2     <- buggyTransferMoney(sender, receiver, 1000L).fork
      _        <- (fib1 <*> fib2).join
      result   <- receiver.get.debugThread
    yield result

  def transactionalTransferMoney(
      sender: TRef[Long],
      receiver: TRef[Long],
      amount: Long
  ): STM[String, Long] =
    sender.get.filterOrFail(_ >= amount)("Insufficient funds") *>
      sender.update(_ - amount) *>
      receiver.updateAndGet(_ + amount)
    // for
    //   senderBalance <- sender.get
    //   newBalance    <- if senderBalance < amount then STM.fail("Insufficient funds")
    //                    else sender.update(_ - amount) *> receiver.updateAndGet(_ + amount)
    // yield newBalance

  def runTransactionalBank: IO[String, Long] =
    for
      sender   <- TRef.make(1000L).commit.debugThread("sender")
      receiver <- TRef.make(0L).commit.debugThread("receiver")
      fib1     <- transactionalTransferMoney(sender, receiver, 1000L).commit.fork
      fib2     <- transactionalTransferMoney(sender, receiver, 1000L).commit.fork
      _        <- (fib1 <*> fib2).join
      result   <- receiver.get.commit.debugThread("result")
    yield result

  def runTransactionalBank2 =
    (TRef.make(1000L) <*> TRef.make(0L))
      .flatMap { (sender, receiver) =>
        transactionalTransferMoney(sender, receiver, 1000L)
          // this always fails
          .flatMap { _ =>
            transactionalTransferMoney(sender, receiver, 1000L)
          }
      }
      .commit
      .debugThread

  override def run: ZIO[Any, Any, Any] =
    runTransactionalBank2
      .debugThread("Should NOT be > 1000, but it is what? -> ")
    // .repeat(Schedule.spaced(100.milliseconds) && Schedule.recurs(10000))
