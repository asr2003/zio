"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[93761],{15680:(e,t,n)=>{n.d(t,{xA:()=>c,yg:()=>f});var a=n(96540);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function r(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):r(r({},t),e)),n},c=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},h=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,i=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),u=p(n),h=o,f=u["".concat(s,".").concat(h)]||u[h]||d[h]||i;return n?a.createElement(f,r(r({ref:t},c),{},{components:n})):a.createElement(f,r({ref:t},c))}));function f(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var i=n.length,r=new Array(i);r[0]=h;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[u]="string"==typeof e?e:o,r[1]=l;for(var p=2;p<i;p++)r[p]=n[p];return a.createElement.apply(null,r)}return a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},64320:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>r,default:()=>d,frontMatter:()=>i,metadata:()=>l,toc:()=>p});var a=n(58168),o=(n(96540),n(15680));const i={id:"associativeflatten",title:"AssociativeFlatten"},r=void 0,l={unversionedId:"zio-prelude/functional-abstractions/parameterized-types/associativeflatten",id:"zio-prelude/functional-abstractions/parameterized-types/associativeflatten",title:"AssociativeFlatten",description:"AssociativeFlatten[F] describes a way of combining two layers of a value of type F[F[A]] into a F[A] in a way that is associative.",source:"@site/docs/zio-prelude/functional-abstractions/parameterized-types/associativeflatten.md",sourceDirName:"zio-prelude/functional-abstractions/parameterized-types",slug:"/zio-prelude/functional-abstractions/parameterized-types/associativeflatten",permalink:"/zio-prelude/functional-abstractions/parameterized-types/associativeflatten",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-prelude/functional-abstractions/parameterized-types/associativeflatten.md",tags:[],version:"current",frontMatter:{id:"associativeflatten",title:"AssociativeFlatten"},sidebar:"ecosystem-sidebar",previous:{title:"AssociativeEither",permalink:"/zio-prelude/functional-abstractions/parameterized-types/associativeeither"},next:{title:"CommutativeBoth",permalink:"/zio-prelude/functional-abstractions/parameterized-types/commutativeboth"}},s={},p=[],c={toc:p},u="wrapper";function d(e){let{components:t,...n}=e;return(0,o.yg)(u,(0,a.A)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,o.yg)("p",null,(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten[F]")," describes a way of combining two layers of a value of type ",(0,o.yg)("inlineCode",{parentName:"p"},"F[F[A]]")," into a ",(0,o.yg)("inlineCode",{parentName:"p"},"F[A]")," in a way that is associative."),(0,o.yg)("p",null,"Its signature is:"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"trait AssociativeFlatten[F[+_]] {\n  def flatten[A](ffa: F[F[A]]): F[A]\n}\n")),(0,o.yg)("p",null,"If we import ",(0,o.yg)("inlineCode",{parentName:"p"},"zio.prelude._")," we can use the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator on any data type for which an ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," instance is defined to flatten a nested value ",(0,o.yg)("inlineCode",{parentName:"p"},"F[F[A]]")," down to a single value."),(0,o.yg)("p",null,"This may appear to be a unary operator but it is actually a binary operator because there are two layers of ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," values here, the outer ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," value and the inner ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," value. The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator combines these two layers of ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," values into a single value."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator must be associative, so if we have three layers of nested values flattening the outer and the middle layers and then flattening the resulting layer with the inner layer must be the same as flattening the middle and the inner layers and flattening the outer layer with that."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"fffa.flatten.flatten == fffa.map(_.flatten).flatten\n")),(0,o.yg)("p",null,"This looks slightly different than the signatures of the other associative laws we have seen because of our need to map over the value to flatten it but it is conceptually the same as the associative laws for the ",(0,o.yg)("inlineCode",{parentName:"p"},"Associative"),", ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeBoth"),", and ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeEither")," functional abstractions."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator corresponds to running the outer value to produce a new value and then running that value."),(0,o.yg)("p",null,"We can see this by recognizing that at this point we know that ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," is covariant so it produces values of type ",(0,o.yg)("inlineCode",{parentName:"p"},"A"),". We start with a value ",(0,o.yg)("inlineCode",{parentName:"p"},"F[F[A]]"),", which is essentially a producer of producers of values of type ",(0,o.yg)("inlineCode",{parentName:"p"},"A"),"."),(0,o.yg)("p",null,"So to get a final value ",(0,o.yg)("inlineCode",{parentName:"p"},"F[A]")," that just produces ",(0,o.yg)("inlineCode",{parentName:"p"},"A")," values we are going to need to run the outer ",(0,o.yg)("inlineCode",{parentName:"p"},"F")," value to get those producers of ",(0,o.yg)("inlineCode",{parentName:"p"},"A")," values, then run those producers of ",(0,o.yg)("inlineCode",{parentName:"p"},"A")," values to get actual ",(0,o.yg)("inlineCode",{parentName:"p"},"A")," values, and finally put the whole thing back together in the structure of ",(0,o.yg)("inlineCode",{parentName:"p"},"F"),"."),(0,o.yg)("p",null,"This turns out to be extremely powerful."),(0,o.yg)("p",null,"To see what this looks like let's start as we have before with ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO"),"."),(0,o.yg)("p",null,"A common mistake we might make when getting started with ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," is accidentally creating a nested ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow like this:"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},'import zio._\n\nimport java.io.IOException\n\nval greet: ZIO[Random, Nothing, ZIO[Console, IOException, Unit]] =\n  Random.nextIntBounded(100).map { n =>\n    Console.printLine(s"The number is $n")\n  }\n// greet: ZIO[Random, Nothing, ZIO[Console, IOException, Unit]] = OnSuccess(\n//   trace = "repl.MdocSession.MdocApp.greet(associativeflatten.md:23)",\n//   first = Stateful(\n//     trace = "repl.MdocSession.MdocApp.greet(associativeflatten.md:23)",\n//     onState = zio.FiberRef$unsafe$$anon$2$$Lambda$17730/0x00007f359e73ad78@50a08d16\n//   ),\n//   successK = zio.ZIO$$Lambda$17789/0x00007f359e2243d0@5e15984b\n// )\n')),(0,o.yg)("p",null,"This is very understandable."),(0,o.yg)("p",null,"We want to perform a second ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow, in this case printing to the console, based on the result of the first workflow. So we use the ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," operator, which is described by the ",(0,o.yg)("inlineCode",{parentName:"p"},"Covariant")," abstraction, to get the success value of the first workflow and transform it with a function."),(0,o.yg)("p",null,"It is worth pointing out here that ",(0,o.yg)("inlineCode",{parentName:"p"},"Covariant")," is the only abstraction in ZIO Prelude that describes the ability to transform the output of a covariant type with a function. So based on the structure described by the other functional abstractions in ZIO Prelude there is nothing we could do other than use ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," to make progress towards implementing this."),(0,o.yg)("p",null,"However, we are now faced with a problem. We have a ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow that produces another ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow instead of producing a value. This is normally not what we want."),(0,o.yg)("p",null,"This workflow is going to be difficult to compose with other workflows. If we want to get the result of the second workflow we are now going to have to use the ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," operator twice just to access the inner value."),(0,o.yg)("p",null,"In addition, running this workflow will probably not do what we intend, because it will just generate a random number and then return another workflow. We will not see anything printed to the console."),(0,o.yg)("p",null,"Conceptually what we would like to do is run this workflow to produce its result, which is another workflow that will actually print something to the console, and then run that workflow. However, we don't want to have to run these nested workflows ourselves and we want to be able to use this workflow in an ergonomic way without going through these multiple layers of nesting."),(0,o.yg)("p",null,"This is exactly what the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator does for us. It takes a ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow that produces another workflow and flattens it down to a single workflow that conceptually runs the first workflow and then runs the resulting workflow."),(0,o.yg)("p",null,"Let's see how we can use the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator to fix our code from above."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},'val greet: ZIO[Random with Console, IOException, Unit] =\n  Random.nextIntBounded(100).map { n =>\n    Console.printLine(s"The number is $n")\n  }.flatten\n// greet: ZIO[Random with Console, IOException, Unit] = OnSuccess(\n//   trace = "repl.MdocSession.MdocApp.<local MdocApp>.greet(associativeflatten.md:35)",\n//   first = OnSuccess(\n//     trace = "repl.MdocSession.MdocApp.<local MdocApp>.greet(associativeflatten.md:33)",\n//     first = Stateful(\n//       trace = "repl.MdocSession.MdocApp.<local MdocApp>.greet(associativeflatten.md:33)",\n//       onState = zio.FiberRef$unsafe$$anon$2$$Lambda$17730/0x00007f359e73ad78@461d1a7c\n//     ),\n//     successK = zio.ZIO$$Lambda$17789/0x00007f359e2243d0@5fa92b1c\n//   ),\n//   successK = zio.ZIO$$Lambda$17790/0x00007f359e225520@b35d5c2\n// )\n')),(0,o.yg)("p",null,"Now we have eliminated the nested workflows. This workflow will do exactly what we want, first generating a random number and then printing it to the console."),(0,o.yg)("p",null,"Of course if we are working with ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," we quickly learn to use the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator, which just combines ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten"),"."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},'val greet: ZIO[Random with Console, IOException, Unit] =\n  Random.nextIntBounded(100).flatMap { n =>\n    Console.printLine(s"The number is $n")\n  }\n// greet: ZIO[Random with Console, IOException, Unit] = OnSuccess(\n//   trace = "repl.MdocSession.MdocApp.<local MdocApp>.greet(associativeflatten.md:43)",\n//   first = Stateful(\n//     trace = "repl.MdocSession.MdocApp.<local MdocApp>.greet(associativeflatten.md:43)",\n//     onState = zio.FiberRef$unsafe$$anon$2$$Lambda$17730/0x00007f359e73ad78@7990c32f\n//   ),\n//   successK = <function1>\n// )\n')),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator is extremely convenient, especially in combination with Scala's syntax for for comprehensions. However, we are now in a position to see that there isn't really anything special about the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator itself."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator is just a combination of ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"map")," is a very basic operator defined on all covariant data types. The real work here is in the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator has to run one value to produce its output then run that output to produce a result."),(0,o.yg)("p",null,"In the case of ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," this is at least conceptually relatively simple because a ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflow always succeeds with exactly one value. So the interpretation of ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," is just to run the first workflow to produce its result and then if it is successful run that workflow."),(0,o.yg)("p",null,"In other cases the interpretation can be more complex."),(0,o.yg)("p",null,"For example, consider ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream"),", which can succeed with zero or more values. In that case the interpretation of flattening a stream of streams is to concatenate the inner streams into a single stream."),(0,o.yg)("p",null,"Many other data types also support ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operators, all of which have a similar interpretation of running the outer value to produce an inner value and then running the inner value."),(0,o.yg)("p",null,"Data types that model failure, such as ",(0,o.yg)("inlineCode",{parentName:"p"},"Either")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"Option"),", are one example of this. Flattening a nested ",(0,o.yg)("inlineCode",{parentName:"p"},"Either")," or ",(0,o.yg)("inlineCode",{parentName:"p"},"Option")," corresponds to running the outer value and if it is a success running the inner value but otherwise failing immediately."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def flatten[E, A](eea: Either[E, Either[E, A]]): Either[E, A] =\n  eea match {\n    case Left(e)   => Left(e)\n    case Right(ea) => ea\n  }\n")),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def flatten[A](ooa: Option[Option[A]]): Option[A] =\n  ooa match {\n    case None     => None\n    case Some(oa) => oa\n  }\n")),(0,o.yg)("p",null,"Collection types also have a ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator with a natural interpretation. If we think of running a collection as producing a set of values then flattening a collection corresponds to running a collection of collections and then running each of those collections to produce its values."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def flatten[A](aas: Chunk[Chunk[A]]): Chunk[A] =\n  aas.foldLeft[Chunk[A]](Chunk.empty)(_ ++ _)\n")),(0,o.yg)("p",null,"Notice the similarity here to the interpretation of ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," for ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream"),", although ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream")," can be potentially effectual and resourceful."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator is so powerful because it allows the next value to be produced by running the first value."),(0,o.yg)("p",null,"To see this, consider the following two ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," workflows."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},'val helloAssociativeBoth: ZIO[Console, IOException, Unit] =\n  console.putStrLn("What\'s your name?") *>\n    console.readLine *>\n    console.putStrLn(s"Hello! Welcome to ZIO!")\n\nval helloAssociativeFlatten: ZIO[Console, IOException, Unit] =\n  for {\n    _    <- console.putStrLn("What\'s your name?") *>\n    name <- console.readLine *>\n    _    <- console.putStrLn(s"Hello $name! Welcome to ZIO!")\n  } yield ()\n')),(0,o.yg)("p",null,"The first workflow is written using only the structure described by the ",(0,o.yg)("inlineCode",{parentName:"p"},"Covariant")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeBoth")," abstractions."),(0,o.yg)("p",null,"We can use these abstractions to describe running one workflow and then running another workflow. So the first workflow will ask us for our name, then read our name, and then say hello."),(0,o.yg)("p",null,"However, notice that in the first workflow we can't greet the user by name. Although we can describe printing to the console after reading from the console, we can't actually use the value we read from the console in printing to the console."),(0,o.yg)("p",null,"Workflows described solely in terms of the ",(0,o.yg)("inlineCode",{parentName:"p"},"Covariant")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeBoth")," abstractions are static like this. We can compose them together but later workflows can't use information produced by prior ones."),(0,o.yg)("p",null,"This can have significant advantages in terms of opportunities for optimization. The first workflow can potentially be analyzed before being run, allowing us to determine what kinds of things it will do or a more efficient way of running it."),(0,o.yg)("p",null,"The second workflow is written using the structure described by the ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," abstraction."),(0,o.yg)("p",null,"Now we can describe a more powerful notion of running one workflow that produces a workflow and then running that workflow. Now we are able not only to print a greeting after reading from the console but to use the value we read from the console in the greeting."),(0,o.yg)("p",null,"Workflows written with the structure of ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," are much more flexible and allow building workflows of arbitrary complexity. This is why the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator is so useful when working with ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO"),"."),(0,o.yg)("p",null,"At the same time, this power limits our ability to introspect on workflows written using the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator."),(0,o.yg)("p",null,"As we saw from our discussion at the beginning, we have to run the outer value to produce the inner value. So we don't know what the next workflow will be until we actually run the current workflow."),(0,o.yg)("p",null,"There are some ways we can increase the flexibility of workflows that don't use the structure provided by ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten"),". "),(0,o.yg)("p",null,"For example the ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeEither")," abstraction allows us to describe a concept of failure."),(0,o.yg)("p",null,"So we could express some limited degree of conditionality, for example reading from the console again if reading the first time failed. However that would still not allow us to actually use the result from the previous workflow in the current one."),(0,o.yg)("p",null,"In some cases this more limited functionality can be acceptable. For example in parsing it can get us quite far, and as discussed above there are benefits to this approach."),(0,o.yg)("p",null,"But for general purposes programming the flexibility provided by the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten")," operator is necessary to support the variety of things we may want to describe with our workflows."),(0,o.yg)("p",null,"As this discussion has hopefully made clear, ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," is a very important functional abstraction that gives us the ability to run a value that produces another value and then run that value, allowing us to describe workflows in a way that is extremely flexible."),(0,o.yg)("p",null,"If you are working with existing data types in ZIO or the Scala standard library they probably already define ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"flatten"),"."),(0,o.yg)("p",null,"There are some operators defined on the ",(0,o.yg)("inlineCode",{parentName:"p"},"ForEach")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"NonEmptyForEach")," abstractions that work with data types that have instances of ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," or ",(0,o.yg)("inlineCode",{parentName:"p"},"IdentityFlatten"),". These can be helpful to do things like fold over a collection with the possibility of failure."),(0,o.yg)("p",null,"When defining your own parameterized data types the ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," operator is so useful that you should be thinking about what an implementation of this abstraction would look like for your own data type. Or you may intentionally not want to expose it to allow for more static analysis, but that is definitely something worth thinking about."),(0,o.yg)("p",null,"For all the reasons discussed above the ",(0,o.yg)("inlineCode",{parentName:"p"},"AssociativeFlatten")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"IdentityFlatten")," abstractions are also very important in writing generic code to express values that depend on other values in a parameterized context."))}d.isMDXComponent=!0}}]);