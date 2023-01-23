"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[87110],{3905:(e,o,n)=>{n.d(o,{Zo:()=>p,kt:()=>u});var r=n(67294);function t(e,o,n){return o in e?Object.defineProperty(e,o,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[o]=n,e}function l(e,o){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);o&&(r=r.filter((function(o){return Object.getOwnPropertyDescriptor(e,o).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var o=1;o<arguments.length;o++){var n=null!=arguments[o]?arguments[o]:{};o%2?l(Object(n),!0).forEach((function(o){t(e,o,n[o])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(o){Object.defineProperty(e,o,Object.getOwnPropertyDescriptor(n,o))}))}return e}function g(e,o){if(null==e)return{};var n,r,t=function(e,o){if(null==e)return{};var n,r,t={},l=Object.keys(e);for(r=0;r<l.length;r++)n=l[r],o.indexOf(n)>=0||(t[n]=e[n]);return t}(e,o);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)n=l[r],o.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(t[n]=e[n])}return t}var a=r.createContext({}),s=function(e){var o=r.useContext(a),n=o;return e&&(n="function"==typeof e?e(o):i(i({},o),e)),n},p=function(e){var o=s(e.components);return r.createElement(a.Provider,{value:o},e.children)},c="mdxType",m={inlineCode:"code",wrapper:function(e){var o=e.children;return r.createElement(r.Fragment,{},o)}},d=r.forwardRef((function(e,o){var n=e.components,t=e.mdxType,l=e.originalType,a=e.parentName,p=g(e,["components","mdxType","originalType","parentName"]),c=s(n),d=t,u=c["".concat(a,".").concat(d)]||c[d]||m[d]||l;return n?r.createElement(u,i(i({ref:o},p),{},{components:n})):r.createElement(u,i({ref:o},p))}));function u(e,o){var n=arguments,t=o&&o.mdxType;if("string"==typeof e||t){var l=n.length,i=new Array(l);i[0]=d;var g={};for(var a in o)hasOwnProperty.call(o,a)&&(g[a]=o[a]);g.originalType=e,g[c]="string"==typeof e?e:t,i[1]=g;for(var s=2;s<l;s++)i[s]=n[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},42663:(e,o,n)=>{n.r(o),n.d(o,{assets:()=>a,contentTitle:()=>i,default:()=>c,frontMatter:()=>l,metadata:()=>g,toc:()=>s});var r=n(87462),t=(n(67294),n(3905));const l={id:"slf4j-bridge",title:"SLF4J bridge"},i=void 0,g={unversionedId:"zio-logging/slf4j-bridge",id:"zio-logging/slf4j-bridge",title:"SLF4J bridge",description:"It is possible to use zio-logging for SLF4J loggers, usually third-party non-ZIO libraries. To do so, import",source:"@site/docs/zio-logging/slf4j-bridge.md",sourceDirName:"zio-logging",slug:"/zio-logging/slf4j-bridge",permalink:"/zio-logging/slf4j-bridge",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-logging/slf4j-bridge.md",tags:[],version:"current",frontMatter:{id:"slf4j-bridge",title:"SLF4J bridge"},sidebar:"ecosystem-sidebar",previous:{title:"SLF4J",permalink:"/zio-logging/slf4j"},next:{title:"Log Metrics",permalink:"/zio-logging/metrics"}},a={},s=[{value:"Examples",id:"examples",level:2},{value:"SLF4J bridge with JSON console logger",id:"slf4j-bridge-with-json-console-logger",level:3}],p={toc:s};function c(e){let{components:o,...n}=e;return(0,t.kt)("wrapper",(0,r.Z)({},p,n,{components:o,mdxType:"MDXLayout"}),(0,t.kt)("p",null,"It is possible to use ",(0,t.kt)("inlineCode",{parentName:"p"},"zio-logging")," for SLF4J loggers, usually third-party non-ZIO libraries. To do so, import\nthe ",(0,t.kt)("inlineCode",{parentName:"p"},"zio-logging-slf4j-bridge")," module:"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},'libraryDependencies += "dev.zio" %% "zio-logging-slf4j-bridge" % "2.1.8"\n')),(0,t.kt)("p",null,"and use the ",(0,t.kt)("inlineCode",{parentName:"p"},"Slf4jBridge.initialize")," layer when setting up logging:"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},"import zio.logging.slf4j.Slf4jBridge\n\nprogram.provideCustom(Slf4jBridge.initialize)\n")),(0,t.kt)("br",null),(0,t.kt)("p",null,"SLF4J logger name is stored in log annotation with key ",(0,t.kt)("inlineCode",{parentName:"p"},"slf4j_logger_name")," (",(0,t.kt)("inlineCode",{parentName:"p"},"Slf4jBridge.loggerNameAnnotationKey"),"), following log format"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},"import zio.logging.slf4j.Slf4jBridge\nimport zio.logging.LoggerNameExtractor\n\nval loggerName = LoggerNameExtractor.annotationOrTrace(Slf4jBridge.loggerNameAnnotationKey)\nval loggerNameFormat = loggerName.toLogFormat()\n")),(0,t.kt)("p",null,"may be used to get logger name from log annotation or ZIO Trace. "),(0,t.kt)("p",null,"This logger name extractor can be used also in log filter, which applying log filtering by defined logger name and level:"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},'val logFilter: LogFilter[String] = LogFilter.logLevelByGroup(\n  LogLevel.Info,\n  loggerName.toLogGroup(),\n  "zio.logging.slf4j" -> LogLevel.Debug,\n  "SLF4J-LOGGER"      -> LogLevel.Warning\n)\n')),(0,t.kt)("br",null),(0,t.kt)("p",null,"SLF4J bridge with custom logger can be setup:"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},"import zio.logging.slf4j.Slf4jBridge\n\nval logger = Runtime.removeDefaultLoggers >>> zio.logging.consoleJson(LogFormat.default, LogLevel.Debug) >+> Slf4jBridge.initialize\n")),(0,t.kt)("br",null),(0,t.kt)("p",null,(0,t.kt)("strong",{parentName:"p"},"NOTE")," You should either use ",(0,t.kt)("inlineCode",{parentName:"p"},"zio-logging-slf4j")," to send all ZIO logs to an SLF4j provider (such as logback, log4j etc) OR ",(0,t.kt)("inlineCode",{parentName:"p"},"zio-logging-slf4j-bridge")," to send all SLF4j logs to\nZIO logging. Enabling both causes circular logging and makes no sense."),(0,t.kt)("h2",{id:"examples"},"Examples"),(0,t.kt)("h3",{id:"slf4j-bridge-with-json-console-logger"},"SLF4J bridge with JSON console logger"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre",className:"language-scala"},'package zio.logging.slf4j.bridge\n\nimport org.slf4j.{ Logger, LoggerFactory }\nimport zio.logging.{ LogFilter, LogFormat, LoggerNameExtractor, consoleJson }\nimport zio.{ ExitCode, LogLevel, Runtime, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer }\n\nobject Slf4jBridgeExampleApp extends ZIOAppDefault {\n\n  private val slf4jLogger: Logger = LoggerFactory.getLogger("SLF4J-LOGGER")\n\n  private val loggerName = LoggerNameExtractor.annotationOrTrace(Slf4jBridge.loggerNameAnnotationKey)\n\n  private val logFilter: LogFilter[String] = LogFilter.logLevelByGroup(\n    LogLevel.Info,\n    loggerName.toLogGroup(),\n    "zio.logging.slf4j" -> LogLevel.Debug,\n    "SLF4J-LOGGER"      -> LogLevel.Warning\n  )\n\n  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =\n    Runtime.removeDefaultLoggers >>> consoleJson(\n      LogFormat.label("name", loggerName.toLogFormat()) + LogFormat.default,\n      logFilter\n    ) >+> Slf4jBridge.initialize\n\n  override def run: ZIO[Scope, Any, ExitCode] =\n    for {\n      _ <- ZIO.logDebug("Start")\n      _ <- ZIO.succeed(slf4jLogger.debug("Test {}!", "DEBUG"))\n      _ <- ZIO.succeed(slf4jLogger.warn("Test {}!", "WARNING"))\n      _ <- ZIO.logInfo("Done")\n    } yield ExitCode.success\n\n}\n')),(0,t.kt)("p",null,"Expected Console Output:"),(0,t.kt)("pre",null,(0,t.kt)("code",{parentName:"pre"},'{"name":"zio.logging.slf4j.bridge.Slf4jBridgeExampleApp","timestamp":"2023-01-07T18:25:40.397593+01:00","level":"DEBUG","thread":"zio-fiber-4","message":"Start"}\n{"name":"SLF4J-LOGGER","timestamp":"2023-01-07T18:25:40.416612+01:00","level":"WARN","thread":"zio-fiber-6","message":"Test WARNING!"}\n{"name":"zio.logging.slf4j.bridge.Slf4jBridgeExampleApp","timestamp":"2023-01-07T18:25:40.42043+01:00 ","level":"INFO","thread":"zio-fiber-4","message":"Done"}\n')))}c.isMDXComponent=!0}}]);