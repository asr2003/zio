"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[93974],{15680:(e,t,n)=>{n.d(t,{xA:()=>u,yg:()=>y});var r=n(96540);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function s(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?s(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):s(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},s=Object.keys(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var l=r.createContext({}),p=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=p(e.components);return r.createElement(l.Provider,{value:t},e.children)},d="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},c=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,s=e.originalType,l=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),d=p(n),c=a,y=d["".concat(l,".").concat(c)]||d[c]||m[c]||s;return n?r.createElement(y,i(i({ref:t},u),{},{components:n})):r.createElement(y,i({ref:t},u))}));function y(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var s=n.length,i=new Array(s);i[0]=c;var o={};for(var l in t)hasOwnProperty.call(t,l)&&(o[l]=t[l]);o.originalType=e,o[d]="string"==typeof e?e:a,i[1]=o;for(var p=2;p<s;p++)i[p]=n[p];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}c.displayName="MDXCreateElement"},56231:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>m,frontMatter:()=>s,metadata:()=>o,toc:()=>p});var r=n(58168),a=(n(96540),n(15680));const s={id:"request",title:"Request"},i=void 0,o={unversionedId:"zio-http/dsl/request",id:"zio-http/dsl/request",title:"Request",description:"ZIO HTTP Request is designed in the simplest way possible to decode HTTP Request into a ZIO HTTP request.",source:"@site/docs/zio-http/dsl/request.md",sourceDirName:"zio-http/dsl",slug:"/zio-http/dsl/request",permalink:"/zio-http/dsl/request",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-http/dsl/request.md",tags:[],version:"current",frontMatter:{id:"request",title:"Request"},sidebar:"ecosystem-sidebar",previous:{title:"Routes",permalink:"/zio-http/dsl/routes"},next:{title:"Response",permalink:"/zio-http/dsl/response"}},l={},p=[{value:"Creating a Request",id:"creating-a-request",level:2},{value:"Accessing the Request",id:"accessing-the-request",level:2},{value:"Creating and reading a Request with query params",id:"creating-and-reading-a-request-with-query-params",level:2}],u={toc:p},d="wrapper";function m(e){let{components:t,...n}=e;return(0,a.yg)(d,(0,r.A)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,a.yg)("p",null,(0,a.yg)("strong",{parentName:"p"},"ZIO HTTP")," ",(0,a.yg)("inlineCode",{parentName:"p"},"Request")," is designed in the simplest way possible to decode HTTP Request into a ZIO HTTP request.\nIt supports all HTTP request methods (as defined in ",(0,a.yg)("a",{parentName:"p",href:"https://datatracker.ietf.org/doc/html/rfc2616"},"RFC2616")," ) and headers along with custom methods and headers."),(0,a.yg)("h2",{id:"creating-a-request"},"Creating a Request"),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"Request")," can be created with ",(0,a.yg)("inlineCode",{parentName:"p"},"method"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"url"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"headers"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"remoteAddress")," and ",(0,a.yg)("inlineCode",{parentName:"p"},"data"),". "),(0,a.yg)("p",null,"Creating requests using ",(0,a.yg)("inlineCode",{parentName:"p"},"Request")," is useful while writing unit tests."),(0,a.yg)("p",null,"The below snippet creates a request with default params, ",(0,a.yg)("inlineCode",{parentName:"p"},"headers")," as ",(0,a.yg)("inlineCode",{parentName:"p"},"Headers.empty"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"data")," as ",(0,a.yg)("inlineCode",{parentName:"p"},"Body.Empty"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"remoteAddress")," as ",(0,a.yg)("inlineCode",{parentName:"p"},"None"),":"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},"import zio.http._\nimport zio._\n\nRequest(method = Method.GET, url = URL(Root))\n// res0: Request = Request(\n//   version = Default,\n//   method = GET,\n//   url = URL(\n//     path = Path(flags = 3, segments = IndexedSeq()),\n//     kind = Relative,\n//     queryParams = QueryParams(map = Map()),\n//     fragment = None\n//   ),\n//   headers = Iterable(),\n//   body = Body.empty,\n//   remoteAddress = None\n// )\n")),(0,a.yg)("h2",{id:"accessing-the-request"},"Accessing the Request"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"body")," to access the content of request as a ",(0,a.yg)("inlineCode",{parentName:"li"},"Body")),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"headers")," to get all the headers in the Request"),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"method")," to access request method"),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"url")," to access request URL"),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"remoteAddress")," to access request's remote address if available"),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("inlineCode",{parentName:"li"},"version")," to access the HTTP version")),(0,a.yg)("h2",{id:"creating-and-reading-a-request-with-query-params"},"Creating and reading a Request with query params"),(0,a.yg)("p",null,"Query params can be added in the request using ",(0,a.yg)("inlineCode",{parentName:"p"},"url")," in ",(0,a.yg)("inlineCode",{parentName:"p"},"Request"),", ",(0,a.yg)("inlineCode",{parentName:"p"},"URL")," stores query params as ",(0,a.yg)("inlineCode",{parentName:"p"},"Map[String, List[String]]"),"."),(0,a.yg)("p",null,"The below snippet creates a request with query params: ",(0,a.yg)("inlineCode",{parentName:"p"},"?q=a&q=b&q=c")," "),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},'Request.get(url = URL(Root, queryParams = QueryParams("q" -> Chunk("a","b","c"))))\n// res1: Request = Request(\n//   version = Default,\n//   method = GET,\n//   url = URL(\n//     path = Path(flags = 3, segments = IndexedSeq()),\n//     kind = Relative,\n//     queryParams = QueryParams(map = HashMap("q" -> IndexedSeq("a", "b", "c"))),\n//     fragment = None\n//   ),\n//   headers = Iterable(),\n//   body = Body.empty,\n//   remoteAddress = None\n// )\n')),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"url.queryParams")," can be used to read query params from the request"))}m.isMDXComponent=!0}}]);