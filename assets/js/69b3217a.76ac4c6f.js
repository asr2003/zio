"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[23053],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>h});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var p=n.createContext({}),s=function(e){var t=n.useContext(p),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},c=function(e){var t=s(e.components);return n.createElement(p.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,p=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),m=s(a),d=r,h=m["".concat(p,".").concat(d)]||m[d]||u[d]||o;return a?n.createElement(h,i(i({ref:t},c),{},{components:a})):n.createElement(h,i({ref:t},c))}));function h(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,i=new Array(o);i[0]=d;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l[m]="string"==typeof e?e:r,i[1]=l;for(var s=2;s<o;s++)i[s]=a[s];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}d.displayName="MDXCreateElement"},72463:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>m,frontMatter:()=>o,metadata:()=>l,toc:()=>s});var n=a(87462),r=(a(67294),a(3905));const o={sidebar_position:1,title:"Introduction to ZIO Http",sidebar_label:"Introduction"},i=void 0,l={unversionedId:"zio-http/index",id:"zio-http/index",title:"Introduction to ZIO Http",description:"Table of Contents",source:"@site/docs/zio-http/index.md",sourceDirName:"zio-http",slug:"/zio-http/",permalink:"/zio-http/",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-http/index.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1,title:"Introduction to ZIO Http",sidebar_label:"Introduction"}},p={},s=[{value:"Creating a &quot;<em>Hello World</em>&quot; app",id:"creating-a-hello-world-app",level:2},{value:"Routing",id:"routing",level:2},{value:"Composition",id:"composition",level:2},{value:"ZIO Integration",id:"zio-integration",level:2},{value:"Accessing the Request",id:"accessing-the-request",level:2},{value:"Testing",id:"testing",level:2},{value:"Creating a socket app",id:"creating-a-socket-app",level:2},{value:"Starting an Http App",id:"starting-an-http-app",level:2}],c={toc:s};function m(e){let{components:t,...a}=e;return(0,r.kt)("wrapper",(0,n.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Table of Contents")),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#http"},"Http"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#creating-a-hello-world-app"},'Creating a "',(0,r.kt)("em",{parentName:"a"},"Hello World"),'" app')),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#routing"},"Routing")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#composition"},"Composition")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#zio-integration"},"ZIO Integration")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#accessing-the-request"},"Requests")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#testing"},"Testing")))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#socket"},"Socket"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#creating-a-socket-app"},"Creating a socket app")))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#server"},"Server"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#starting-an-http-app"},"Starting an Http App")))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#examples"},"Examples"))),(0,r.kt)("h1",{id:"http"},"Http"),(0,r.kt)("h2",{id:"creating-a-hello-world-app"},'Creating a "',(0,r.kt)("em",{parentName:"h2"},"Hello World"),'" app'),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import zio.http._\n\nval app = Http.text("Hello World!")\n')),(0,r.kt)("p",null,"An application can be made using any of the available operators on ",(0,r.kt)("inlineCode",{parentName:"p"},"zio.Http"),". In the above program for any Http request, the response is always ",(0,r.kt)("inlineCode",{parentName:"p"},'"Hello World!"'),"."),(0,r.kt)("h2",{id:"routing"},"Routing"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import zio.http._\n\nval app = Http.collect[Request] {\n  case Method.GET -> Root / "fruits" / "a"  => Response.text("Apple")\n  case Method.GET -> Root / "fruits" / "b"  => Response.text("Banana")\n}\n')),(0,r.kt)("p",null,"Pattern matching on route is supported by the framework"),(0,r.kt)("h2",{id:"composition"},"Composition"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import zio.http._\n\nval a = Http.collect[Request] { case Method.GET -> Root / "a"  => Response.ok }\nval b = Http.collect[Request] { case Method.GET -> Root / "b"  => Response.ok }\n\nval app = a <> b\n')),(0,r.kt)("p",null,"Apps can be composed using the ",(0,r.kt)("inlineCode",{parentName:"p"},"<>")," operator. The way it works is, if none of the routes match in ",(0,r.kt)("inlineCode",{parentName:"p"},"a")," , or a ",(0,r.kt)("inlineCode",{parentName:"p"},"NotFound")," error is thrown from ",(0,r.kt)("inlineCode",{parentName:"p"},"a"),", and then the control is passed on to the ",(0,r.kt)("inlineCode",{parentName:"p"},"b")," app."),(0,r.kt)("h2",{id:"zio-integration"},"ZIO Integration"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'val app = Http.collectM[Request] {\n  case Method.GET -> Root / "hello" => ZIO.succeed(Response.text("Hello World"))\n}\n')),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"Http.collectM")," allow routes to return a ZIO effect value."),(0,r.kt)("h2",{id:"accessing-the-request"},"Accessing the Request"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import zio.http._\n\nval app = Http.collect[Request] {\n  case req @ Method.GET -> Root / "fruits" / "a"  =>\n    Response.text("URL:" + req.url.path.asString + " Headers: " + r.headers)\n  case req @ Method.POST -> Root / "fruits" / "a" =>\n    Response.text(req.bodyAsString.getOrElse("No body!"))\n}\n')),(0,r.kt)("h2",{id:"testing"},"Testing"),(0,r.kt)("p",null,"Tests suites could be implemented using ",(0,r.kt)("inlineCode",{parentName:"p"},"zio-test")," library, as following:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'\nimport zio.test._\nimport zio.http._\nimport zio.test.Assertion.equalTo\n\nobject Spec extends DefaultRunnableSpec {\n  val app = Http.collect[Request] { case Method.GET -> !! / "text" =>\n    Response.text("Hello World!")\n  }\n\n  def spec = suite("http")(\n    test("should be ok") {\n      val req         = ???\n      val expectedRes = app(req).map(_.status)\n      assertZIO(expectedRes)(equalTo(Status.Ok))\n    },\n  )\n}\n')),(0,r.kt)("h1",{id:"socket"},"Socket"),(0,r.kt)("h2",{id:"creating-a-socket-app"},"Creating a socket app"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import zio.socket._\n\nprivate val socket = Socket.collect[WebSocketFrame] {\n  case WebSocketFrame.Text("FOO")  => ZStream.succeed(WebSocketFrame.text("BAR"))\n}\n\nprivate val app = Http.collect[Request] {\n  case Method.GET -> Root / "greet" / name  => Response.text(s"Greetings {$name}!")\n  case Method.GET -> Root / "ws" => Response.socket(socket)\n}\n')),(0,r.kt)("h1",{id:"server"},"Server"),(0,r.kt)("h2",{id:"starting-an-http-app"},"Starting an Http App"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"import zio.http._\nimport zio.http.Server\nimport zio._\n\nobject HelloWorld extends App {\n  val app = Http.ok\n\n  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =\n    Server.start(8090, app).exitCode\n}\n")),(0,r.kt)("p",null,"A simple Http app that responds with empty content and a ",(0,r.kt)("inlineCode",{parentName:"p"},"200")," status code is deployed on port ",(0,r.kt)("inlineCode",{parentName:"p"},"8090")," using ",(0,r.kt)("inlineCode",{parentName:"p"},"Server.start"),"."),(0,r.kt)("h1",{id:"examples"},"Examples"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/HelloWorld.scala"},"Simple Server")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/HelloWorldAdvanced.scala"},"Advanced Server")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/SocketEchoServer.scala"},"WebSocket Server")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/StreamingResponse.scala"},"Streaming Response")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/SimpleClient.scala"},"Simple Client")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/FileStreaming.scala"},"File Streaming")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://github.com/zio/zio-http/blob/main/zio-http-example/src/main/scala/example/Authentication.scala"},"Authentication"))))}m.isMDXComponent=!0}}]);