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
          // this always fails because the funds are exhausted at this point
          .flatMap { _ =>
            transactionalTransferMoney(sender, receiver, 1000L)
          }
      }
      .commit
      .debugThread

  /**
   * STM data structures
   */

  // atomic variable: TRef
  val v: USTM[TRef[Int]] = TRef.make(42)

  // TArray
  val a: USTM[TArray[Int]] = TArray.make(42, 43, 44)
  val i: USTM[TArray[Int]] = TArray.fromIterable(List(42, 43, 44))

  // get/apply
  val elm: USTM[Int] =
    for
      arr <- i
      elm <- arr(1)
    yield elm

  // set/update
  val updated: USTM[TArray[Int]] =
    for
      arr <- i
      _   <- arr.update(1, _ * 10)
    yield arr

  // transform
  val transformed: USTM[TArray[Int]] =
    for
      arr <- i
      _   <- arr.transform(_ + 1)
    yield arr

  // fold/foldSTM/foreach
  // TSet, TMap, TQueue, TStack, TPriorityQueue

  /**
   * concurrent coordination
   */
  // TRef, TArray, TMap, TSet, TQueue, TStack, TPriorityQueue
  // TPromise
  val tpromise: USTM[TPromise[String, Int]] = TPromise.make[String, Int]

  val tawait: STM[String, Int] =
    for
      tp  <- tpromise
      res <- tp.await
    yield res

  val demoSucceed: USTM[Unit] =
    for
      tp <- tpromise
      _  <- tp.succeed(42)
    yield ()

  // TSemaphore
  val tsemaphore: USTM[TSemaphore] = TSemaphore.make(10)

  val tsemAcquire: STM[String, Unit] =
    for
      tsem <- tsemaphore
      _    <- tsem.acquireN(1)
    yield ()

  val tsemRelease: STM[String, Unit] =
    for
      tsem <- tsemaphore
      _    <- tsem.releaseN(1)
    yield ()

  val tsemWithPermit: UIO[Int] =
    tsemaphore.commit
      .flatMap { tsem =>
        tsem.withPermit {
          ZIO.succeed(42)
        }
      }

  // TReentrantLock - can acquire the same lock multiple times without deadlocking
  // readers-writers problem
  // has two locks: read lock (lower prio) and write lock (higher prio)
  val tlock: USTM[TReentrantLock] = TReentrantLock.make

  val demoReentrantLock: USTM[Int] =
    for
      lock          <- tlock
      _             <- lock.acquireRead // acquires the read lock
      res           <- STM.succeed(200) // critical section
      isReadLocked  <- lock.readLocked  // status of the lock
      isWriteLocked <- lock.writeLocked
    yield res

  def demoReadersWriters: UIO[Unit] =
    def read(lock: TReentrantLock)(i: Int): UIO[Unit] =
      for
        _      <- lock.acquireRead.commit                          // acquires the read lock
        // critical section start
        _      <- ZIO.succeed(s"[task $i] taken the read lock, reading...").debugThread
        delay  <- Random.nextIntBounded(1000)
        result <- Random.nextIntBounded(10000).delay(delay.millis) // actual computation
        _      <- ZIO.succeed(s"[task $i] read value: $result").debugThread
        // critical section end
        _      <- lock.releaseRead.commit                          // releases the read lock
      yield ()

    def write(lock: TReentrantLock): UIO[Unit] =
      for
        _ <- ZIO.succeed(s"[writer] trying to write...").debugThread.delay(200.millis)
        _ <- lock.acquireWrite.commit
        // critical region start
        _ <- ZIO.succeed(s"[writer] writing...").debugThread
        // critical region end
        _ <- lock.releaseWrite.commit
      yield ()

    for
      lock       <- TReentrantLock.make.commit
      readersFib <- ZIO.collectAllParDiscard((1 to 10).map(read(lock))).fork
      writersFib <- write(lock).fork
      _          <- readersFib.join
      _          <- writersFib.join
    yield ()

  override def run: ZIO[Any, Any, Any] = demoReadersWriters
