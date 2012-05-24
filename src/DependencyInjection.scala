import java.lang.reflect.Modifier

object DependencyInjection {

  trait FooService {def serve: String}
  trait ProdFooService extends FooService {def serve = "Production!"}
  trait DevFooService extends FooService {def serve = "Development!"}
  trait BarService {def serve = "Barrrr!!"}

  trait DependencyInjectionContainer {
    def resolveService(serviceType: Class[_]): AnyRef
    def resolveService[T: Manifest]: T
    def create[T] = new Construct[T]()(this)
    def registerSingleton[T: Manifest](instance: T): Unit
  }

  class DependencyInjectionContainerImpl(
    val parentContainer: Option[DependencyInjectionContainer]) extends DependencyInjectionContainer {

    def this() = this(None)

    private var typesToSingletons = Map[Class[_], AnyRef](classOf[DependencyInjectionContainer] -> this)

    def registerSingleton[T: Manifest](instance: T): Unit = {
      typesToSingletons = typesToSingletons + (implicitly[Manifest[T]].erasure -> instance.asInstanceOf[AnyRef])
    }

    def resolveService[T: Manifest]: T =
      resolveService(implicitly[Manifest[T]].erasure).asInstanceOf[T]

    def resolveService(serviceType: Class[_]): AnyRef = {
      typesToSingletons.getOrElse(
        serviceType,
        parentContainer
          .getOrElse(throw new Exception("Can't resolve service for \"%s\"" format serviceType))
          .resolveService(serviceType))
    }
  }

  trait Constructor0[T] {
    def construct(implicit container: DependencyInjectionContainer): T
  }
  trait Constructor1[T, A] {
    def construct(arg1: A)(implicit container: DependencyInjectionContainer): T
  }
  trait Constructor2[T, A, B] {
    def construct(arg1: A, arg2: B)(implicit container: DependencyInjectionContainer): T
  }
  trait Constructor3[T, A, B, C] {
    def construct(arg1: A, arg2: B, arg3: C)(implicit container: DependencyInjectionContainer): T
  }
  trait Constructor4[T, A, B, C, D] {
    def construct(arg1: A, arg2: B, arg3: C, arg4: D)(implicit container: DependencyInjectionContainer): T
  }
  trait Constructor5[T, A, B, C, D, E] {
    def construct(arg1: A, arg2: B, arg3: C, arg4: D, arg5: E)(implicit container: DependencyInjectionContainer): T
  }

  class Construct[T](implicit val container: DependencyInjectionContainer) {
    def apply()(implicit ctor: Constructor0[T]): T =
      ctor.construct
    def apply[A](arg1: A)(implicit ctor: Constructor1[T, A]): T =
      ctor.construct(arg1)
    def apply[A, B](arg1: A, arg2: B)(implicit ctor: Constructor2[T, A, B]): T =
      ctor.construct(arg1, arg2)
    def apply[A, B, C](arg1: A, arg2: B, arg3: C)(implicit ctor: Constructor3[T, A, B, C]): T =
      ctor.construct(arg1, arg2, arg3)
    def apply[A, B, C, D](arg1: A, arg2: B, arg3: C, arg4: D)(implicit ctor: Constructor4[T, A, B, C, D]): T =
      ctor.construct(arg1, arg2, arg3, arg4)
    def apply[A, B, C, D, E](arg1: A, arg2: B, arg3: C, arg4: D, arg5: E)(implicit ctor: Constructor5[T, A, B, C, D, E]): T =
      ctor.construct(arg1, arg2, arg3, arg4, arg5)
  }

  class ConstructImplicit[T] {
    def apply()(implicit ctor: Constructor0[T], c: DependencyInjectionContainer): T =
      ctor.construct
    def apply[A](arg1: A)(implicit ctor: Constructor1[T, A], c: DependencyInjectionContainer): T =
      ctor.construct(arg1)
    def apply[A, B](arg1: A, arg2: B)(implicit ctor: Constructor2[T, A, B], c: DependencyInjectionContainer): T =
      ctor.construct(arg1, arg2)
    def apply[A, B, C](arg1: A, arg2: B, arg3: C)(implicit ctor: Constructor3[T, A, B, C], c: DependencyInjectionContainer): T =
      ctor.construct(arg1, arg2, arg3)
    def apply[A, B, C, D](arg1: A, arg2: B, arg3: C, arg4: D)(implicit ctor: Constructor4[T, A, B, C, D], c: DependencyInjectionContainer): T =
      ctor.construct(arg1, arg2, arg3, arg4)
    def apply[A, B, C, D, E](arg1: A, arg2: B, arg3: C, arg4: D, arg5: E)(implicit ctor: Constructor5[T, A, B, C, D, E], c: DependencyInjectionContainer): T =
      ctor.construct(arg1, arg2, arg3, arg4, arg5)
  }

  implicit object WhimWhamConstructor extends Constructor0[Whimwham] {
    def construct(implicit container: DependencyInjectionContainer): Whimwham =
      new Whimwham(container.resolveService[FooService])
  }

  implicit object DooJammerConstructor extends Constructor2[DooJammer, String, Int] {
    def construct(arg1: String, arg2: Int)(implicit container: DependencyInjectionContainer): DooJammer =
      new DooJammer(arg1, arg2, container.resolveService[FooService], container.resolveService[BarService], container)
  }

  class DooJammer(
    val x: String,
    val y: Int,
    val fooService: FooService,
    val barService: BarService,
    val container: DependencyInjectionContainer) {
    def frob(): Unit = println("%s, %s, %s, %s" format (x, y, fooService.serve, barService.serve))

    def wobble(): Unit = container.create[Whimwham]().frob()
  }

  class Whimwham(val fooService: FooService) {
    def frob(): Unit = println("Hello, %s" format fooService.serve)
  }

  def create[T] = new ConstructImplicit[T]

  def main(args: Array[String]): Unit = {
    val container1 = new DependencyInjectionContainerImpl
    container1.registerSingleton[FooService](new DevFooService {})
    container1.registerSingleton[BarService](new BarService {})
    container1.create[DooJammer]("asdasd", 12).frob()

    val container2 = new DependencyInjectionContainerImpl(Some(container1))
    container2.registerSingleton[FooService](new ProdFooService {})
    val dooJammer2 = container2.create[DooJammer]("asdasd", 12)
    dooJammer2.frob()
    dooJammer2.wobble()

    implicit def c = container2

    val dj = create[DooJammer]("fgh", 45)
    dj.wobble()
    dj.wobble()
    dj.frob()
  }
}
