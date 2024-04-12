"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[77900],{15680:(e,n,r)=>{r.d(n,{xA:()=>c,yg:()=>g});var a=r(96540);function o(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}function t(e,n){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),r.push.apply(r,a)}return r}function i(e){for(var n=1;n<arguments.length;n++){var r=null!=arguments[n]?arguments[n]:{};n%2?t(Object(r),!0).forEach((function(n){o(e,n,r[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):t(Object(r)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(r,n))}))}return e}function l(e,n){if(null==e)return{};var r,a,o=function(e,n){if(null==e)return{};var r,a,o={},t=Object.keys(e);for(a=0;a<t.length;a++)r=t[a],n.indexOf(r)>=0||(o[r]=e[r]);return o}(e,n);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);for(a=0;a<t.length;a++)r=t[a],n.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var s=a.createContext({}),p=function(e){var n=a.useContext(s),r=n;return e&&(r="function"==typeof e?e(n):i(i({},n),e)),r},c=function(e){var n=p(e.components);return a.createElement(s.Provider,{value:n},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},h=a.forwardRef((function(e,n){var r=e.components,o=e.mdxType,t=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),d=p(r),h=o,g=d["".concat(s,".").concat(h)]||d[h]||u[h]||t;return r?a.createElement(g,i(i({ref:n},c),{},{components:r})):a.createElement(g,i({ref:n},c))}));function g(e,n){var r=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var t=r.length,i=new Array(t);i[0]=h;var l={};for(var s in n)hasOwnProperty.call(n,s)&&(l[s]=n[s]);l.originalType=e,l[d]="string"==typeof e?e:o,i[1]=l;for(var p=2;p<t;p++)i[p]=r[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,r)}h.displayName="MDXCreateElement"},68073:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>t,metadata:()=>l,toc:()=>p});var a=r(58168),o=(r(96540),r(15680));const t={id:"scope",title:"Scope"},i=void 0,l={unversionedId:"reference/resource/scope",id:"reference/resource/scope",title:"Scope",description:"The Scope data type is the foundation of safe and composable resources handling in ZIO.",source:"@site/docs/reference/resource/scope.md",sourceDirName:"reference/resource",slug:"/reference/resource/scope",permalink:"/reference/resource/scope",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/reference/resource/scope.md",tags:[],version:"current",frontMatter:{id:"scope",title:"Scope"},sidebar:"reference-sidebar",previous:{title:"Introduction to Resource Management in ZIO",permalink:"/reference/resource/"},next:{title:"ZPool",permalink:"/reference/resource/zpool"}},s={},p=[{value:"Defining Resources",id:"defining-resources",level:2},{value:"Converting Resources Into Other ZIO Data Types",id:"converting-resources-into-other-zio-data-types",level:2},{value:"Controlling Finalizer Ordering",id:"controlling-finalizer-ordering",level:2},{value:"Advanced Scope Operators",id:"advanced-scope-operators",level:2}],c={toc:p},d="wrapper";function u(e){let{components:n,...r}=e;return(0,o.yg)(d,(0,a.A)({},c,r,{components:n,mdxType:"MDXLayout"}),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," data type is the foundation of safe and composable resources handling in ZIO."),(0,o.yg)("p",null,"Conceptually, a scope represents the lifetime of one or more resources. The resources can be used in the scope and are guaranteed to be released when the scope is closed."),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," data type takes this idea and represents it as a first class value."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\n\ntrait Scope {\n  def addFinalizerExit(finalizer: Exit[Any, Any] => UIO[Any]): UIO[Unit]\n  def close(exit: => Exit[Any, Any]): UIO[Unit]\n}\n")),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"addFinalizerExit")," operator lets us add a finalizer to the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),". The ",(0,o.yg)("inlineCode",{parentName:"p"},"close")," operator closes the scope, running all the finalizers that have been added to the scope."),(0,o.yg)("p",null,"In combination with the ZIO environment, ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," gives us an extremely powerful way to manage resources."),(0,o.yg)("p",null,"We can define a resource using operators such as ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO.acquireRelease"),", which lets us construct a scoped value from an ",(0,o.yg)("inlineCode",{parentName:"p"},"acquire")," and ",(0,o.yg)("inlineCode",{parentName:"p"},"release")," workflow. For example, here is how we might define a simple resource:"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\n\nimport java.io.IOException\nimport scala.io._\n\ndef acquire(name: => String): ZIO[Any, IOException, Source] =\n  ZIO.attemptBlockingIO(Source.fromFile(name))\n\ndef release(source: => Source): ZIO[Any, Nothing, Unit] =\n  ZIO.succeedBlocking(source.close())\n\ndef source(name: => String): ZIO[Scope, IOException, Source] =\n  ZIO.acquireRelease(acquire(name))(release(_))\n")),(0,o.yg)("p",null,"Notice that the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator added a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," to the environment required by the workflow. This indicates that this workflow needs a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," to be run and will add a finalizer that will close the resource when the scope is closed."),(0,o.yg)("p",null,"We can continue working with the resource as long as we want by using ",(0,o.yg)("inlineCode",{parentName:"p"},"flatMap")," or other ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," operators. For example, here is how we might read the contents of a file:"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},'source("cool.txt").flatMap { source =>\n  ZIO.attemptBlockingIO(source.getLines())\n}\n// res1: ZIO[Scope, IOException, Iterator[String]] = FlatMap(\n//   trace = "repl.MdocSession.MdocApp.res1(scope.md:50)",\n//   first = FlatMap(\n//     trace = "repl.MdocSession.MdocApp.source(scope.md:44)",\n//     first = Sync(\n//       trace = "repl.MdocSession.MdocApp.source(scope.md:44)",\n//       eval = zio.ZIO$$$Lambda$18472/0x00007f0155d35578@1969c4b1\n//     ),\n//     successK = zio.ZIO$$$Lambda$17906/0x00007f0155c448e8@13ba7de5\n//   ),\n//   successK = <function1>\n// )\n')),(0,o.yg)("p",null,"When we are done working with the file we can close the scope using the ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO.scoped")," operator, which creates a new ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),", provides it to the workflow, and closes the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," when the workflow is done."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def contents(name: => String): ZIO[Any, IOException, Chunk[String]] =\n  ZIO.scoped {\n    source(name).flatMap { source =>\n      ZIO.attemptBlockingIO(Chunk.fromIterator(source.getLines()))\n    }\n  }\n")),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," operator removes the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," from the environment, indicating that there are no longer any resources used by this workflow which require a scope. We now have a workflow that is ready to run."),(0,o.yg)("p",null,"In some cases ZIO applications may provide a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," for us for resources that we don't specify a scope for. For example ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIOApp")," provides a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," for our entire application and ZIO Test provides a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," for each test."),(0,o.yg)("h2",{id:"defining-resources"},"Defining Resources"),(0,o.yg)("p",null,"We have already seen the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator, which is one of the most fundamental operators for creating scoped resources."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"object ZIO {\n  def acquireRelease[R, E, A](acquire: => ZIO[R, E, A])(release: A => ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] =\n    ???\n}\n")),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator performs the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquire")," workflow uninterruptibly. This is important because if we allowed interruption during resource acquisition we could be interrupted when the resource was partially acquired."),(0,o.yg)("p",null,"The guarantee of the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator is that if the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquire")," workflow successfully completes execution then the ",(0,o.yg)("inlineCode",{parentName:"p"},"release")," workflow is guaranteed to be run when the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," is closed."),(0,o.yg)("p",null,"In addition to the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator, there is a more powerful variant called ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireReleaseExit")," that lets the finalizer depend on the ",(0,o.yg)("inlineCode",{parentName:"p"},"Exit")," value that the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," is closed with. This can be useful if we want to run a different finalizer depending on whether the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," was closed with a success or a failure."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"object ZIO {\n  def acquireReleaseExit[R, E, A](acquire: => ZIO[R, E, A])(release: (A, Exit[Any, Any]) => ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] =\n    ???\n}\n")),(0,o.yg)("p",null,"There is also another family of operators to be aware of that allow the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquire")," workflow to be interrupted."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"object ZIO {\n  def acquireReleaseInterruptible[R, E, A](acquire: => ZIO[R, E, A])(release: ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] =\n    ???\n  def acquireReleaseInterruptibleExit[R, E, A](acquire: => ZIO[R, E, A])(release: Exit[Any, Any] => ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] =\n    ???\n}\n")),(0,o.yg)("p",null,"In this case the ",(0,o.yg)("inlineCode",{parentName:"p"},"release")," workflow is not allowed to depend on the resource, since the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquire")," workflow might be interrupted after partially acquiring the resource. The ",(0,o.yg)("inlineCode",{parentName:"p"},"release")," workflow is responsible for independently determining what finalization is required, for example by inspecting in-memory state."),(0,o.yg)("p",null,"This is a more advanced variant so we should generally use the standard ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," operator. However, the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireReleaseInterruptible")," operator can be very useful to describe more advanced resource acquisition scenarios where part of the acquisition can be interruptible."),(0,o.yg)("h2",{id:"converting-resources-into-other-zio-data-types"},"Converting Resources Into Other ZIO Data Types"),(0,o.yg)("p",null,"We will commonly want to convert scoped resources into other ZIO data types, particularly ",(0,o.yg)("inlineCode",{parentName:"p"},"ZLayer")," for dependency injection and ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream"),", ",(0,o.yg)("inlineCode",{parentName:"p"},"ZSink"),", and ",(0,o.yg)("inlineCode",{parentName:"p"},"ZChannel")," for streaming."),(0,o.yg)("p",null,"We can easily do this using the ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," constructor on each of these data types. For example, here is how we might convert the ",(0,o.yg)("inlineCode",{parentName:"p"},"source")," resource above into a ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream")," of the contents:"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"import zio.stream._\n\ndef lines(name: => String): ZStream[Any, IOException, String] =\n  ZStream.scoped(source(name)).flatMap { source =>\n    ZStream.fromIteratorSucceed(source.getLines())\n  }\n")),(0,o.yg)("p",null,"Just like the ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," operator on ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO"),", the ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," operator on ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream")," removes the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," from the environment, indicating that there are no longer any resources used by this workflow which require a scope."),(0,o.yg)("p",null,"The lifetime of these resources will now be governed by the lifetime of the stream, which generally means that the resources will be released as soon as we are done pulling from the stream. This lets the lifetime of these resources be managed by various stream operators to release those resources as efficiently as possible, for example releasing resources associated with each stream as soon as we are done with that stream when we merge two streams."),(0,o.yg)("p",null,"Similarly, we can convert a scoped resource into a ",(0,o.yg)("inlineCode",{parentName:"p"},"ZLayer")," by using the ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," constructor on ",(0,o.yg)("inlineCode",{parentName:"p"},"ZLayer"),":"),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def sourceLayer(name: => String): ZLayer[Any, IOException, Source] =\n  ZLayer.scoped(source(name))\n")),(0,o.yg)("p",null,"Again, the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," has been removed from the environment, indicating that the lifetime of this resource will no longer be governed by the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," but by the lifetime of the layer. In this case, that means the resource will be released as soon as the workflow that the layer is provided to completes execution, whether by success, failure, or interruption."),(0,o.yg)("p",null,"We should generally use the ",(0,o.yg)("inlineCode",{parentName:"p"},"scoped")," operators on other ZIO data types to convert a scoped resource into a value of that data type. Having the lifetime of resources governed by the lifetime of those data types makes our code simpler and easier to reason about."),(0,o.yg)("h2",{id:"controlling-finalizer-ordering"},"Controlling Finalizer Ordering"),(0,o.yg)("p",null,"By default, when a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," is closed all finalizers added to that ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," will be closed in the reverse of the order in which those finalizers were added to the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),"."),(0,o.yg)("p",null,"Releasing resources in the reverse order in which they were acquired makes sense because a resource that was acquired first may be necessary for a later acquired resource to be closed."),(0,o.yg)("p",null,"For example, if we open a network connection and then open a file on a remote server we need to close the file before closing the network connection. Otherwise we would no longer be able to interact with the remote server to close the file!"),(0,o.yg)("p",null,"Therefore, in most cases we don't have to do anything with regard to order of finalizers. However, in some cases we may want to run finalizers in parallel instead of sequentially, for example when the resources were also acquired in parallel."),(0,o.yg)("p",null,"For this we can use the ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO.parallelFinalizers")," operator to indicate that finalizers should be run in parallel instead of sequentially when a scope is closed. Here is how we could use it to implement an operator that acquires and releases two resources in parallel."),(0,o.yg)("pre",null,(0,o.yg)("code",{parentName:"pre",className:"language-scala"},"def zipScoped[R <: Scope, E, A, B](\n  left: ZIO[R, E, A],\n  right: ZIO[R, E, B]\n): ZIO[R, E, (A, B)] =\n  ZIO.parallelFinalizers(left.zipPar(right))\n")),(0,o.yg)("p",null,"The ",(0,o.yg)("inlineCode",{parentName:"p"},"zipPar")," operator on ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO")," takes care of acquiring the resources in parallel and the ",(0,o.yg)("inlineCode",{parentName:"p"},"parallelFinalizers")," operator handles releasing them in parallel. This makes it easy for us to do parallel resource acquisition by leveraging the powerful concurrency operators that already exist on ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO"),"."),(0,o.yg)("h2",{id:"advanced-scope-operators"},"Advanced Scope Operators"),(0,o.yg)("p",null,"So far we have seen that while ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," is the foundation of safe and composable resource handling in ZIO, we don't actually need to work with the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," data type directly other than being able to inpect the type signature to see if a workflow is scoped."),(0,o.yg)("p",null,"In most cases we just use the ",(0,o.yg)("inlineCode",{parentName:"p"},"acquireRelease")," constructor or one of its variants to construct our resource and either work with the resource and close its scope using ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO.scoped")," or convert the resource into another ZIO data type using an operator such as ",(0,o.yg)("inlineCode",{parentName:"p"},"ZStream.scoped")," or ",(0,o.yg)("inlineCode",{parentName:"p"},"ZLayer.scoped"),". However, for more advanced use cases we may need to work with scopes directly and ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," has several useful operators for helping us do so."),(0,o.yg)("p",null,"First, we can ",(0,o.yg)("inlineCode",{parentName:"p"},"use")," a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," by providing it to a workflow that needs a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," and closing the ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," immediately after. This is analogous to the ",(0,o.yg)("inlineCode",{parentName:"p"},"ZIO.scoped")," operator."),(0,o.yg)("p",null,"Second, we can use the ",(0,o.yg)("inlineCode",{parentName:"p"},"extend")," operator on ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),' to provide a workflow with a scope without closing it afterwards. This allows us to extend the lifetime of a scoped resource to the lifetime of a scope, effectively allowing us to "extend" the lifetime of that resource.'),(0,o.yg)("p",null,"Third, we can ",(0,o.yg)("inlineCode",{parentName:"p"},"close")," a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),". One thing to note here is that by default only the creator of a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," can close it."),(0,o.yg)("p",null,"Creating a new ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," returns a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope.Closeable")," which can be closed. Normally users of a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," will only be provided with a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope")," which does not expose a ",(0,o.yg)("inlineCode",{parentName:"p"},"close")," operator."),(0,o.yg)("p",null,"This way the creator of a ",(0,o.yg)("inlineCode",{parentName:"p"},"Scope"),' can be sure that someone else will not "pull the rug out from under them" by closing the scope prematurely.'))}u.isMDXComponent=!0}}]);