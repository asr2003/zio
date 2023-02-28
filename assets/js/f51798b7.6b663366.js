"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[50305],{3905:(e,t,n)=>{n.d(t,{Zo:()=>l,kt:()=>y});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),s=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},l=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,p=e.parentName,l=c(e,["components","mdxType","originalType","parentName"]),d=s(n),m=a,y=d["".concat(p,".").concat(m)]||d[m]||u[m]||o;return n?r.createElement(y,i(i({ref:t},l),{},{components:n})):r.createElement(y,i({ref:t},l))}));function y(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=m;var c={};for(var p in t)hasOwnProperty.call(t,p)&&(c[p]=t[p]);c.originalType=e,c[d]="string"==typeof e?e:a,i[1]=c;for(var s=2;s<o;s++)i[s]=n[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},67696:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>c,toc:()=>s});var r=n(87462),a=(n(67294),n(3905));const o={id:"concrete-entity",title:"Concrete Entity Example",sidebar_label:"Concrete Entity"},i=void 0,c={unversionedId:"zio-http/examples/advanced/concrete-entity",id:"zio-http/examples/advanced/concrete-entity",title:"Concrete Entity Example",description:"",source:"@site/docs/zio-http/examples/advanced/concrete-entity.md",sourceDirName:"zio-http/examples/advanced",slug:"/zio-http/examples/advanced/concrete-entity",permalink:"/zio-http/examples/advanced/concrete-entity",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-http/examples/advanced/concrete-entity.md",tags:[],version:"current",frontMatter:{id:"concrete-entity",title:"Concrete Entity Example",sidebar_label:"Concrete Entity"},sidebar:"ecosystem-sidebar",previous:{title:"Authentication Server",permalink:"/zio-http/examples/advanced/authentication-server"},next:{title:"Middleware Basic Authentication",permalink:"/zio-http/examples/advanced/middleware-basic-authentication"}},p={},s=[],l={toc:s},d="wrapper";function u(e){let{components:t,...n}=e;return(0,a.kt)(d,(0,r.Z)({},l,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},"import zio.http._\nimport zio.http.Server\nimport zio._\n\n/**\n * Example to build app on concrete entity\n */\nobject ConcreteEntity extends App {\n  // Request\n  case class CreateUser(name: String)\n\n  // Response\n  case class UserCreated(id: Long)\n\n  val user: Http[Any, Nothing, CreateUser, UserCreated] =\n    Http.collect[CreateUser] { case CreateUser(_) =>\n      UserCreated(2)\n    }\n\n  val app: HttpApp[Any, Nothing] =\n    user\n      .contramap[Request](req => CreateUser(req.path.toString))   // Http[Any, Nothing, Request, UserCreated]\n      .map(userCreated => Response.text(userCreated.id.toString)) // Http[Any, Nothing, Request, Response]\n\n  // Run it like any simple app\n  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =\n    Server.start(8090, app).exitCode\n}\n")))}u.isMDXComponent=!0}}]);