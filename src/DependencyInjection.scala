import java.lang.reflect.Modifier

object DependencyInjection {

  trait FooService {
    def serve: String
  }

  trait ProdFooService extends FooService {
    def serve = "Production!"
  }

  trait DevFooService extends FooService {
    def serve = "Development!"
  }

  trait BarService {
    def serve = "Barrrr!!"
  }

  trait Constructable { }
  trait Constructable1[-A] {}
  trait Constructable2[-A, -B] {}
  trait Constructable3[-A, -B, -C] {}
  trait Constructable4[-A, -B, -C, -D] {}

  // note: HLists would make this much prettier
  // and KList[Manifest]
  // and/or, we could use typeclasses instead of marker interfaces!
  // great idea.
  trait DependencyInjectionContainer {
    def resolveService(serviceType: Class[_]): AnyRef
    def create[T <: Constructable](implicit constructableManifest: Manifest[T]): T
    def create[T <: Constructable1[A], A](arg1: A)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A]): T
    def create[T <: Constructable2[A, B], A, B]
          (arg1: A, arg2: B)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A],
           arg2Manifest: Manifest[B]): T
    def create[T <: Constructable3[A, B, C], A, B, C]
          (arg1: A, arg2: B, arg3: C)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A],
           arg2Manifest: Manifest[B],
           arg3Manifest: Manifest[C]): T
    def create[T <: Constructable4[A, B, C, D], A, B, C, D]
          (arg1: A, arg2: B, arg3:       C, arg4: D)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A],
           arg2Manifest: Manifest[B],
           arg3Manifest: Manifest[C],
           arg4Manifest: Manifest[D]): T
  }

  class DependencyInjectionContainerImpl(
    val parentContainer: Option[DependencyInjectionContainer]) extends DependencyInjectionContainer {

    private var typesToSingletons = Map[Class[_], AnyRef](
      classOf[DependencyInjectionContainer] -> this)

    def registerSingleton[T](instance: T)(implicit instanceManifest: Manifest[T]): Unit = {
      typesToSingletons = typesToSingletons + (instanceManifest.erasure -> instance.asInstanceOf[AnyRef])
    }

    def create[T <: Constructable](implicit constructableManifest: Manifest[T]): T = {
      construct(constructableManifest.erasure, Seq(), Seq()).asInstanceOf[T]
    }

    def create[T <: Constructable1[A], A](arg1: A)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A]): T = {

      construct(constructableManifest.erasure,
        Seq(arg1Manifest.erasure),
        Seq(arg1.asInstanceOf[AnyRef]))
        .asInstanceOf[T]
    }

    def create[T <: Constructable2[A, B], A, B]
          (arg1: A, arg2: B)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A],
           arg2Manifest: Manifest[B]): T = {

      construct(constructableManifest.erasure,
        Seq(arg1Manifest, arg2Manifest).map(_.erasure),
        Seq(arg1, arg2).map(_.asInstanceOf[AnyRef]))
        .asInstanceOf[T]
    }

    def create[T <: Constructable3[A, B, C], A, B, C]
          (arg1: A, arg2: B, arg3: C)
          (implicit constructableManifest: Manifest[T],
           arg1Manifest: Manifest[A],
           arg2Manifest: Manifest[B],
           arg3Manifest: Manifest[C]): T = {

      construct(constructableManifest.erasure,
        Seq(arg1Manifest, arg2Manifest, arg3Manifest).map(_.erasure),
        Seq(arg1, arg2, arg3).map(_.asInstanceOf[AnyRef]))
        .asInstanceOf[T]
    }

    def create[T <: Constructable4[A, B, C, D], A, B, C, D]
          (arg1: A, arg2: B, arg3:       C, arg4: D)
                (implicit constructableManifest: Manifest[T],
                 arg1Manifest: Manifest[A],
                 arg2Manifest: Manifest[B],
                 arg3Manifest: Manifest[C],
                 arg4Manifest: Manifest[D]): T = {

      construct(constructableManifest.erasure,
        Seq(arg1Manifest, arg2Manifest, arg3Manifest, arg4Manifest).map(_.erasure),
        Seq(arg1, arg2, arg3, arg4).map(_.asInstanceOf[AnyRef]))
        .asInstanceOf[T]
    }

    def resolveService[T](implicit serviceManifest: Manifest[T]): T =
      resolveService(serviceManifest.erasure).asInstanceOf[T]

    def resolveService(serviceType: Class[_]): AnyRef = {
      typesToSingletons.getOrElse(
        serviceType,
        parentContainer
          .getOrElse(throw new Exception("Can't resolve service for \"%s\"" format serviceType))
          .resolveService(serviceType))
    }

    private def construct(constructableType: Class[_], providedArgTypes: Seq[Class[_]], providedArgs: Seq[AnyRef]): AnyRef =  {
      val constructor = constructableType.getConstructors
        .filter(ctor => Modifier.isPublic(ctor.getModifiers))
        .head // fixme: come back and make this throw exception if >1 public ctor

      val ctorArgs = constructor.getParameterTypes.toList

      if (!ctorArgs.startsWith(providedArgTypes)) // fixme: make this work for subtypes
        throw new Exception("Incompatible argument types: \"%s\" provided vs. \"%s\" required." format (providedArgTypes, ctorArgs))

      val serviceTypes = ctorArgs.drop(providedArgTypes.length)
      val allArgs = providedArgs ++ serviceTypes.map(resolveService(_))

      constructor.newInstance(allArgs: _*).asInstanceOf[AnyRef]
    }
  }

  class DooJammer(
    val x: String,
    val y: Int,
    val fooService: FooService,
    val barService: BarService,
    val container: DependencyInjectionContainer)
    extends
      Constructable2[String, Int] {
    def frob(): Unit = println("%s, %s, %s, %s" format (x, y, fooService.serve, barService.serve))
    def wobble(): Unit = container.create[Whimwham].frob()
  }

  class Whimwham(val fooService: FooService) extends Constructable {
    def frob(): Unit = println("Hello, %s" format fooService.serve)
  }

  def main(args: Array[String]): Unit = {

    // todo: use HLists to make this construction syntax less ugly?
    // or could just use implicits to register little ConstructableManifests?
    val container1 = new DependencyInjectionContainerImpl(None)
    container1.registerSingleton[FooService](new DevFooService { })
    container1.registerSingleton[BarService](new BarService { })
    container1.create[DooJammer, String, Int]("asdasd", 12).frob()

    val container2 = new DependencyInjectionContainerImpl(Some(container1))
    container2.registerSingleton[FooService](new ProdFooService { })
    val dooJammer2 = container2.create[DooJammer, String, Int]("asdasd", 12)
    dooJammer2.frob()
    dooJammer2.wobble()

  }
}
