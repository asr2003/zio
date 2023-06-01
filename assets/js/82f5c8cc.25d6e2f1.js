"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[70086],{3905:(e,t,n)=>{n.d(t,{Zo:()=>l,kt:()=>g});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function p(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),c=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},l=function(e){var t=c(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,s=e.parentName,l=p(e,["components","mdxType","originalType","parentName"]),u=c(n),m=a,g=u["".concat(s,".").concat(m)]||u[m]||d[m]||i;return n?r.createElement(g,o(o({ref:t},l),{},{components:n})):r.createElement(g,o({ref:t},l))}));function g(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=m;var p={};for(var s in t)hasOwnProperty.call(t,s)&&(p[s]=t[s]);p.originalType=e,p[u]="string"==typeof e?e:a,o[1]=p;for(var c=2;c<i;c++)o[c]=n[c];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},33988:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>o,default:()=>d,frontMatter:()=>i,metadata:()=>p,toc:()=>c});var r=n(87462),a=(n(67294),n(3905));const i={id:"with-javascript",title:"How to Interop with JavaScript?",sidebar_label:"JavaScript"},o=void 0,p={unversionedId:"guides/interop/with-javascript",id:"guides/interop/with-javascript",title:"How to Interop with JavaScript?",description:"Include ZIO in your Scala.js project by adding the following to your build.sbt:",source:"@site/docs/guides/interop/with-javascript.md",sourceDirName:"guides/interop",slug:"/guides/interop/with-javascript",permalink:"/guides/interop/with-javascript",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/guides/interop/with-javascript.md",tags:[],version:"current",frontMatter:{id:"with-javascript",title:"How to Interop with JavaScript?",sidebar_label:"JavaScript"},sidebar:"guides-sidebar",previous:{title:"Java",permalink:"/guides/interop/with-java"},next:{title:"Monix",permalink:"/guides/interop/with-monix"}},s={},c=[{value:"Example",id:"example",level:2}],l={toc:c},u="wrapper";function d(e){let{components:t,...n}=e;return(0,a.kt)(u,(0,r.Z)({},l,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"Include ZIO in your Scala.js project by adding the following to your ",(0,a.kt)("inlineCode",{parentName:"p"},"build.sbt"),":"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},'scalaJSUseMainModuleInitializer := true\nresolvers += Resolver.sonatypeRepo("snapshots")\nlibraryDependencies += "dev.zio" %%% "zio" % "2.0.14+1-a6ce7326-SNAPSHOT"\n')),(0,a.kt)("h2",{id:"example"},"Example"),(0,a.kt)("p",null,"Your main function can extend ",(0,a.kt)("inlineCode",{parentName:"p"},"App")," as follows.\nThis example uses ",(0,a.kt)("a",{parentName:"p",href:"https://github.com/scala-js/scala-js-dom"},"scala-js-dom")," to access the DOM; to run the example you\nwill need to add that library as a dependency to your ",(0,a.kt)("inlineCode",{parentName:"p"},"build.sbt"),"."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'import org.scalajs.dom.{document, raw}\nimport zio._\nimport zio.Clock._\n\nobject Main extends App {\n\n  def run(args: List[String]) = {\n    for {\n      _      <- Console.printLine("Starting progress bar demo.")  // Outputs on browser console log.\n      target <- IO.succeed(document.createElement("pre"))\n      _      <- update(target).repeat(Schedule.spaced(1.seconds))\n      _      <- IO.succeed(node.appendChild(target)) // "node" is provided in this page by mdoc.\n    } yield ExitCode.success\n  }\n\n  def update(target: raw.Element) = {\n      for {\n        time   <- currentTime(TimeUnit.SECONDS)\n        output <- ZIO.succeed(progress((time % 11).toInt, 10))\n        _      <- ZIO.succeed(target.innerHTML = output)\n      } yield ()\n  }\n\n  def progress(tick: Int, size: Int) = {\n      val bar_length = tick\n      val empty_length = size - tick\n      val bar = "#" * bar_length + " " * empty_length\n      s"$bar $bar_length%"\n  }\n\n}\n')))}d.isMDXComponent=!0}}]);