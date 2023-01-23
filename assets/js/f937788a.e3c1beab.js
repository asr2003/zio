"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[47667],{3905:(e,t,r)=>{r.d(t,{Zo:()=>l,kt:()=>f});var a=r(67294);function n(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,a)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){n(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function s(e,t){if(null==e)return{};var r,a,n=function(e,t){if(null==e)return{};var r,a,n={},o=Object.keys(e);for(a=0;a<o.length;a++)r=o[a],t.indexOf(r)>=0||(n[r]=e[r]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)r=o[a],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(n[r]=e[r])}return n}var p=a.createContext({}),c=function(e){var t=a.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},l=function(e){var t=c(e.components);return a.createElement(p.Provider,{value:t},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},m=a.forwardRef((function(e,t){var r=e.components,n=e.mdxType,o=e.originalType,p=e.parentName,l=s(e,["components","mdxType","originalType","parentName"]),d=c(r),m=n,f=d["".concat(p,".").concat(m)]||d[m]||u[m]||o;return r?a.createElement(f,i(i({ref:t},l),{},{components:r})):a.createElement(f,i({ref:t},l))}));function f(e,t){var r=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=r.length,i=new Array(o);i[0]=m;var s={};for(var p in t)hasOwnProperty.call(t,p)&&(s[p]=t[p]);s.originalType=e,s[d]="string"==typeof e?e:n,i[1]=s;for(var c=2;c<o;c++)i[c]=r[c];return a.createElement.apply(null,i)}return a.createElement.apply(null,r)}m.displayName="MDXCreateElement"},76717:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>d,frontMatter:()=>o,metadata:()=>s,toc:()=>c});var a=r(87462),n=(r(67294),r(3905));const o={id:"index",title:"Introduction to ZIO Parser",sidebar_label:"ZIO Parser"},i=void 0,s={unversionedId:"zio-parser/index",id:"zio-parser/index",title:"Introduction to ZIO Parser",description:"Library for constructing parsers and pretty printers based on invertible syntax descriptions",source:"@site/docs/zio-parser/index.md",sourceDirName:"zio-parser",slug:"/zio-parser/",permalink:"/zio-parser/",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-parser/index.md",tags:[],version:"current",frontMatter:{id:"index",title:"Introduction to ZIO Parser",sidebar_label:"ZIO Parser"},sidebar:"ecosystem-sidebar",previous:{title:"Effectual Optics",permalink:"/zio-optics/effectual-optics"},next:{title:"ZIO Parser",permalink:"/zio-parser/"}},p={},c=[{value:"Introduction",id:"introduction",level:2},{value:"Installation",id:"installation",level:2}],l={toc:c};function d(e){let{components:t,...r}=e;return(0,n.kt)("wrapper",(0,a.Z)({},l,r,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("p",null,"Library for constructing parsers and pretty printers based on invertible syntax descriptions"),(0,n.kt)("p",null,(0,n.kt)("a",{parentName:"p",href:"https://github.com/zio/zio/wiki/Project-Stages"},(0,n.kt)("img",{parentName:"a",src:"https://img.shields.io/badge/Project%20Stage-Development-green.svg",alt:"Development"}))," ",(0,n.kt)("img",{parentName:"p",src:"https://github.com/zio/zio-parser/workflows/CI/badge.svg",alt:"CI Badge"})," ",(0,n.kt)("a",{parentName:"p",href:"https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-parser_2.13/"},(0,n.kt)("img",{parentName:"a",src:"https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-parser_2.13.svg?label=Sonatype%20Release",alt:"Sonatype Releases"}))," ",(0,n.kt)("a",{parentName:"p",href:"https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-parser_2.13/"},(0,n.kt)("img",{parentName:"a",src:"https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-parser_2.13.svg?label=Sonatype%20Snapshot",alt:"Sonatype Snapshots"}))," ",(0,n.kt)("a",{parentName:"p",href:"https://javadoc.io/doc/dev.zio/zio-parser-docs_2.13"},(0,n.kt)("img",{parentName:"a",src:"https://javadoc.io/badge2/dev.zio/zio-parser-docs_2.13/javadoc.svg",alt:"javadoc"}))," ",(0,n.kt)("a",{parentName:"p",href:"https://github.com/zio/zio-parser"},(0,n.kt)("img",{parentName:"a",src:"https://img.shields.io/github/stars/zio/zio-parser?style=social",alt:"ZIO Parser"}))),(0,n.kt)("h2",{id:"introduction"},"Introduction"),(0,n.kt)("p",null,(0,n.kt)("a",{parentName:"p",href:"https://www.youtube.com/watch?v=DEPpL9LBiyA"},(0,n.kt)("img",{parentName:"a",src:"https://i.ytimg.com/vi/DEPpL9LBiyA/maxresdefault.jpg",alt:"Zymposium - ZIO Parser"}))),(0,n.kt)("h2",{id:"installation"},"Installation"),(0,n.kt)("p",null,"Start by adding ",(0,n.kt)("inlineCode",{parentName:"p"},"zio-parser")," as a dependency to your project:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},'libraryDependencies += "dev.zio" %% "zio-parser" % "0.1.8"\n')))}d.isMDXComponent=!0}}]);