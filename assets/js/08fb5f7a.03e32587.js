"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[49034],{15680:(e,t,r)=>{r.d(t,{xA:()=>p,yg:()=>m});var n=r(96540);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function i(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function s(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?i(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):i(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function o(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},i=Object.keys(e);for(n=0;n<i.length;n++)r=i[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(n=0;n<i.length;n++)r=i[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var l=n.createContext({}),u=function(e){var t=n.useContext(l),r=t;return e&&(r="function"==typeof e?e(t):s(s({},t),e)),r},p=function(e){var t=u(e.components);return n.createElement(l.Provider,{value:t},e.children)},c="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},y=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,i=e.originalType,l=e.parentName,p=o(e,["components","mdxType","originalType","parentName"]),c=u(r),y=a,m=c["".concat(l,".").concat(y)]||c[y]||d[y]||i;return r?n.createElement(m,s(s({ref:t},p),{},{components:r})):n.createElement(m,s({ref:t},p))}));function m(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=r.length,s=new Array(i);s[0]=y;var o={};for(var l in t)hasOwnProperty.call(t,l)&&(o[l]=t[l]);o.originalType=e,o[c]="string"==typeof e?e:a,s[1]=o;for(var u=2;u<i;u++)s[u]=r[u];return n.createElement.apply(null,s)}return n.createElement.apply(null,r)}y.displayName="MDXCreateElement"},96378:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>d,frontMatter:()=>i,metadata:()=>o,toc:()=>u});var n=r(58168),a=(r(96540),r(15680));const i={id:"index",title:"Introduction to ZIO Query",sidebar_label:"ZIO Query"},s=void 0,o={unversionedId:"zio-query/index",id:"zio-query/index",title:"Introduction to ZIO Query",description:"ZIO Query is a library for writing optimized queries to data sources in a high-level compositional style. It can add efficient pipelining, batching, and caching to any data source. ZIO Query helps us dramatically reduce load on data sources and improve performance.",source:"@site/docs/zio-query/index.md",sourceDirName:"zio-query",slug:"/zio-query/",permalink:"/zio-query/",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-query/index.md",tags:[],version:"current",frontMatter:{id:"index",title:"Introduction to ZIO Query",sidebar_label:"ZIO Query"},sidebar:"ecosystem-sidebar",previous:{title:"Resources",permalink:"/zio-profiling/resources"},next:{title:"ZIO Query",permalink:"/zio-query/"}},l={},u=[{value:"Introduction",id:"introduction",level:2},{value:"Installation",id:"installation",level:2},{value:"Example",id:"example",level:2},{value:"Resources",id:"resources",level:2}],p={toc:u},c="wrapper";function d(e){let{components:t,...r}=e;return(0,a.yg)(c,(0,n.A)({},p,r,{components:t,mdxType:"MDXLayout"}),(0,a.yg)("p",null,(0,a.yg)("a",{parentName:"p",href:"https://github.com/zio/zio-query"},"ZIO Query")," is a library for writing optimized queries to data sources in a high-level compositional style. It can add efficient pipelining, batching, and caching to any data source. ZIO Query helps us dramatically reduce load on data sources and improve performance."),(0,a.yg)("p",null,(0,a.yg)("a",{parentName:"p",href:"https://github.com/zio/zio/wiki/Project-Stages"},(0,a.yg)("img",{parentName:"a",src:"https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg",alt:"Production Ready"}))," ",(0,a.yg)("img",{parentName:"p",src:"https://github.com/zio/zio-query/workflows/CI/badge.svg",alt:"CI Badge"})," ",(0,a.yg)("a",{parentName:"p",href:"https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-query_2.13/"},(0,a.yg)("img",{parentName:"a",src:"https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-query_2.13.svg?label=Sonatype%20Release",alt:"Sonatype Releases"}))," ",(0,a.yg)("a",{parentName:"p",href:"https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-query_2.13/"},(0,a.yg)("img",{parentName:"a",src:"https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-query_2.13.svg?label=Sonatype%20Snapshot",alt:"Sonatype Snapshots"}))," ",(0,a.yg)("a",{parentName:"p",href:"https://javadoc.io/doc/dev.zio/zio-query-docs_2.13"},(0,a.yg)("img",{parentName:"a",src:"https://javadoc.io/badge2/dev.zio/zio-query-docs_2.13/javadoc.svg",alt:"javadoc"}))," ",(0,a.yg)("a",{parentName:"p",href:"https://github.com/zio/zio-query"},(0,a.yg)("img",{parentName:"a",src:"https://img.shields.io/github/stars/zio/zio-query?style=social",alt:"ZIO Query"}))),(0,a.yg)("h2",{id:"introduction"},"Introduction"),(0,a.yg)("p",null,"Some key features of ZIO Query:"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("strong",{parentName:"li"},"Batching")," \u2014 ZIO Query detects parts of composite queries that can be executed in parallel without changing the semantics of the query."),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("strong",{parentName:"li"},"Pipelining")," \u2014 ZIO Query detects parts of composite queries that can be combined together for fewer individual requests to the data source."),(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("strong",{parentName:"li"},"Caching")," \u2014 ZIO Query can transparently cache read queries to minimize the cost of fetching the same item repeatedly in the scope of a query.")),(0,a.yg)("p",null,"Compared with Fetch, ZIO Query supports response types that depend on request types, does not require higher-kinded types and implicits, supports ZIO environment and statically typed errors, and has no dependencies except for ZIO."),(0,a.yg)("p",null,"A ",(0,a.yg)("inlineCode",{parentName:"p"},"ZQuery[R, E, A]")," is a purely functional description of an effectual query that may contain requests from one or more data sources, requires an environment ",(0,a.yg)("inlineCode",{parentName:"p"},"R"),", and may fail with an ",(0,a.yg)("inlineCode",{parentName:"p"},"E")," or succeed with an ",(0,a.yg)("inlineCode",{parentName:"p"},"A"),"."),(0,a.yg)("p",null,"Requests that can be performed in parallel, as expressed by ",(0,a.yg)("inlineCode",{parentName:"p"},"zipWithPar")," and combinators derived from it, will automatically be batched. Requests that must be performed sequentially, as expressed by ",(0,a.yg)("inlineCode",{parentName:"p"},"zipWith")," and combinators derived from it, will automatically be pipelined. This allows for aggressive data source specific optimizations. Requests can also be deduplicated and cached."),(0,a.yg)("p",null,"This allows for writing queries in a high level, compositional style, with confidence that they will automatically be optimized. For example, consider the following query from a user service."),(0,a.yg)("p",null,"Assume we have the following database access layer APIs:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},"def getAllUserIds: ZIO[Any, Nothing, List[Int]] = {\n  // Get all user IDs e.g. SELECT id FROM users\n  ZIO.succeed(???)\n}\n\ndef getUserNameById(id: Int): ZIO[Any, Nothing, String] = {\n  // Get user by ID e.g. SELECT name FROM users WHERE id = $id\n  ZIO.succeed(???)\n}\n")),(0,a.yg)("p",null,"We can get their corresponding usernames from the database by the following code snippet:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},"val userNames = for {\n  ids   <- getAllUserIds\n  names <- ZIO.foreachPar(ids)(getUserNameById)\n} yield names\n")),(0,a.yg)("p",null,"It works, but this is not performant. It is going to query the underlying database ",(0,a.yg)("em",{parentName:"p"},"N + 1")," times, one for ",(0,a.yg)("inlineCode",{parentName:"p"},"getAllUserIds")," and one for each call to ",(0,a.yg)("inlineCode",{parentName:"p"},"getUserNameById"),"."),(0,a.yg)("p",null,"In contrast, ",(0,a.yg)("inlineCode",{parentName:"p"},"ZQuery")," will automatically optimize this to two queries, one for ",(0,a.yg)("inlineCode",{parentName:"p"},"userIds")," and one for ",(0,a.yg)("inlineCode",{parentName:"p"},"userNames"),":"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},"lazy val getAllUserIds: ZQuery[Any, Nothing, List[Int]]    = ???\ndef getUserNameById(id: Int): ZQuery[Any, Nothing, String] = ???\n\nlazy val userQuery: ZQuery[Any, Nothing, List[String]] = for {\n  userIds   <- getAllUserIds\n  userNames <- ZQuery.foreachPar(userIds)(getUserNameById)\n} yield userNames\n")),(0,a.yg)("h2",{id:"installation"},"Installation"),(0,a.yg)("p",null,"In order to use this library, we need to add the following line in our ",(0,a.yg)("inlineCode",{parentName:"p"},"build.sbt")," file:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},'libraryDependencies += "dev.zio" %% "zio-query" % "0.6.0"\n')),(0,a.yg)("h2",{id:"example"},"Example"),(0,a.yg)("p",null,"Here is an example of using ZIO Query, which optimizes multiple database queries by batching all of them in one query:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-scala"},'import zio._\nimport zio.query._\n\nobject ZQueryExample extends ZIOAppDefault {\n  case class GetUserName(id: Int) extends Request[Throwable, String]\n\n  lazy val UserDataSource: DataSource.Batched[Any, GetUserName] =\n    new DataSource.Batched[Any, GetUserName] {\n      val identifier: String = "UserDataSource"\n\n      def run(requests: Chunk[GetUserName])(implicit trace: Trace): ZIO[Any, Nothing, CompletedRequestMap] = {\n        val resultMap = CompletedRequestMap.empty\n        requests.toList match {\n          case request :: Nil =>\n            val result: Task[String] = {\n              // get user by ID e.g. SELECT name FROM users WHERE id = $id\n              ZIO.succeed(???)\n            }\n\n            result.exit.map(resultMap.insert(request, _))\n\n          case batch: Seq[GetUserName] =>\n            val result: Task[List[(Int, String)]] = {\n              // get multiple users at once e.g. SELECT id, name FROM users WHERE id IN ($ids)\n              ZIO.succeed(???)\n            }\n\n            result.fold(\n              err =>\n                requests.foldLeft(resultMap) { case (map, req) =>\n                  map.insert(req, Exit.fail(err))\n                },\n              _.foldLeft(resultMap) { case (map, (id, name)) =>\n                map.insert(GetUserName(id), Exit.succeed(name))\n              }\n            )\n        }\n      }\n\n    }\n\n  def getUserNameById(id: Int): ZQuery[Any, Throwable, String] =\n    ZQuery.fromRequest(GetUserName(id))(UserDataSource)\n\n  val query: ZQuery[Any, Throwable, List[String]] =\n    for {\n      ids <- ZQuery.succeed(1 to 10)\n      names <- ZQuery.foreachPar(ids)(id => getUserNameById(id)).map(_.toList)\n    } yield (names)\n\n  def run = query.run.tap(usernames => Console.printLine(s"Usernames: $usernames"))\n}\n')),(0,a.yg)("h2",{id:"resources"},"Resources"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},(0,a.yg)("a",{parentName:"li",href:"https://www.youtube.com/watch?v=rUUxDXJMzJo"},"Wicked Fast API Calls with ZIO Query")," by Adam Fraser (July 2020) (",(0,a.yg)("a",{parentName:"li",href:"https://www.youtube.com/watch?v=rUUxDXJMzJo"},"https://www.youtube.com/watch?v=rUUxDXJMzJo"),")")))}d.isMDXComponent=!0}}]);