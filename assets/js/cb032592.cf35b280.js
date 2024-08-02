"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[86277],{15680:(e,t,n)=>{n.d(t,{xA:()=>p,yg:()=>g});var a=n(96540);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),m=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},p=function(e){var t=m(e.components);return a.createElement(s.Provider,{value:t},e.children)},c="mdxType",f={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),c=m(n),d=r,g=c["".concat(s,".").concat(d)]||c[d]||f[d]||i;return n?a.createElement(g,o(o({ref:t},p),{},{components:n})):a.createElement(g,o({ref:t},p))}));function g(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,o=new Array(i);o[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[c]="string"==typeof e?e:r,o[1]=l;for(var m=2;m<i;m++)o[m]=n[m];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},43029:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>o,default:()=>f,frontMatter:()=>i,metadata:()=>l,toc:()=>m});var a=n(58168),r=(n(96540),n(15680));const i={id:"tset",title:"TSet"},o=void 0,l={unversionedId:"reference/stm/tset",id:"reference/stm/tset",title:"TSet",description:"A TSet[A] is a mutable set that can participate in transactions in STM.",source:"@site/docs/reference/stm/tset.md",sourceDirName:"reference/stm",slug:"/reference/stm/tset",permalink:"/reference/stm/tset",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/reference/stm/tset.md",tags:[],version:"current",frontMatter:{id:"tset",title:"TSet"},sidebar:"reference-sidebar",previous:{title:"TRandom",permalink:"/reference/stm/trandom"},next:{title:"TMap",permalink:"/reference/stm/tmap"}},s={},m=[{value:"Create a TSet",id:"create-a-tset",level:2},{value:"Put an element to a TSet",id:"put-an-element-to-a-tset",level:2},{value:"Remove an element from a TSet",id:"remove-an-element-from-a-tset",level:2},{value:"Union of a TSet",id:"union-of-a-tset",level:2},{value:"Intersection of a TSet",id:"intersection-of-a-tset",level:2},{value:"Difference of a TSet",id:"difference-of-a-tset",level:2},{value:"Transform elements of a TSet",id:"transform-elements-of-a-tset",level:2},{value:"Perform effects for TSet elements",id:"perform-effects-for-tset-elements",level:2},{value:"Check TSet membership",id:"check-tset-membership",level:2},{value:"Convert TSet to a List",id:"convert-tset-to-a-list",level:2},{value:"Size of a TSet",id:"size-of-a-tset",level:2}],p={toc:m},c="wrapper";function f(e){let{components:t,...n}=e;return(0,r.yg)(c,(0,a.A)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,r.yg)("p",null,"A ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet[A]")," is a mutable set that can participate in transactions in STM."),(0,r.yg)("h2",{id:"create-a-tset"},"Create a TSet"),(0,r.yg)("p",null,"Creating an empty ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet"),":"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval emptyTSet: STM[Nothing, TSet[Int]] = TSet.empty[Int]\n")),(0,r.yg)("p",null,"Or creating a ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet")," with specified values:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval specifiedValuesTSet: STM[Nothing, TSet[Int]] = TSet.make(1, 2, 3)\n")),(0,r.yg)("p",null,"Alternatively, you can create a ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet")," by providing a collection of values:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval iterableTSet: STM[Nothing, TSet[Int]] = TSet.fromIterable(List(1, 2, 3))\n")),(0,r.yg)("p",null,"In case there are duplicates provided, the last one is taken."),(0,r.yg)("h2",{id:"put-an-element-to-a-tset"},"Put an element to a TSet"),(0,r.yg)("p",null,"The new element can be added to the set in the following way:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval putElem: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2)\n  _    <- tSet.put(3)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"In case the set already contains the element, no modification will happen."),(0,r.yg)("h2",{id:"remove-an-element-from-a-tset"},"Remove an element from a TSet"),(0,r.yg)("p",null,"The simplest way to remove an element from ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet")," is using ",(0,r.yg)("inlineCode",{parentName:"p"},"delete")," method:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval deleteElem: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3)\n  _    <- tSet.delete(1)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Also, it is possible to remove every element that satisfies provided predicate:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval removedEvenElems: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  _    <- tSet.removeIf(_ % 2 == 0)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Or you can keep all the elements that match predicate function:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval retainedEvenElems: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  _    <- tSet.retainIf(_ % 2 == 0)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Note that ",(0,r.yg)("inlineCode",{parentName:"p"},"retainIf")," and ",(0,r.yg)("inlineCode",{parentName:"p"},"removeIf")," serve the same purpose as ",(0,r.yg)("inlineCode",{parentName:"p"},"filter")," and ",(0,r.yg)("inlineCode",{parentName:"p"},"filterNot"),". The reason for naming them differently was to emphasize a distinction in their nature. Namely, both ",(0,r.yg)("inlineCode",{parentName:"p"},"retainIf")," and ",(0,r.yg)("inlineCode",{parentName:"p"},"removeIf")," are destructive - calling them can modify the collection."),(0,r.yg)("h2",{id:"union-of-a-tset"},"Union of a TSet"),(0,r.yg)("p",null,"Union of the sets A and B represents the set of elements belonging to set A or set B, or both.\nUsing ",(0,r.yg)("inlineCode",{parentName:"p"},"A union B")," method modifies set ",(0,r.yg)("inlineCode",{parentName:"p"},"A"),"."),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\n// unionTSet = {1, 2, 3, 4, 5, 6}\nval unionTSet: UIO[TSet[Int]] = (for {\n  tSetA <- TSet.make(1, 2, 3, 4)\n  tSetB <- TSet.make(3, 4, 5, 6)\n  _     <- tSetA.union(tSetB)\n} yield tSetA).commit\n")),(0,r.yg)("h2",{id:"intersection-of-a-tset"},"Intersection of a TSet"),(0,r.yg)("p",null,"The intersection of the sets A and B is the set of elements belonging to both A and B.\nUsing ",(0,r.yg)("inlineCode",{parentName:"p"},"A intersect B")," method modifies set ",(0,r.yg)("inlineCode",{parentName:"p"},"A"),"."),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\n// intersectionTSet = {3, 4}\nval intersectionTSet: UIO[TSet[Int]] = (for {\n  tSetA <- TSet.make(1, 2, 3, 4)\n  tSetB <- TSet.make(3, 4, 5, 6)\n  _     <- tSetA.intersect(tSetB)\n} yield tSetA).commit\n")),(0,r.yg)("h2",{id:"difference-of-a-tset"},"Difference of a TSet"),(0,r.yg)("p",null,"The difference between sets A and B is the set containing elements of set A but not in B.\nUsing ",(0,r.yg)("inlineCode",{parentName:"p"},"A diff B")," method modifies set ",(0,r.yg)("inlineCode",{parentName:"p"},"A"),"."),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\n// diffTSet = {1, 2}\nval diffTSet: UIO[TSet[Int]] = (for {\n  tSetA <- TSet.make(1, 2, 3, 4)\n  tSetB <- TSet.make(3, 4, 5, 6)\n  _     <- tSetA.diff(tSetB)\n} yield tSetA).commit\n")),(0,r.yg)("h2",{id:"transform-elements-of-a-tset"},"Transform elements of a TSet"),(0,r.yg)("p",null,"The transform function ",(0,r.yg)("inlineCode",{parentName:"p"},"A => A")," allows computing a new value for every element in the set: "),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval transformTSet: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  _    <- tSet.transform(a => a * a)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Note that it is possible to shrink a ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet"),":"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval shrinkTSet: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  _    <- tSet.transform(_ => 1)\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Resulting set in example above has only one element."),(0,r.yg)("p",null,"Note that ",(0,r.yg)("inlineCode",{parentName:"p"},"transform")," serves the same purpose as ",(0,r.yg)("inlineCode",{parentName:"p"},"map"),". The reason for naming it differently was to emphasize a distinction in its nature. Namely, ",(0,r.yg)("inlineCode",{parentName:"p"},"transform")," is destructive - calling it can modify the collection."),(0,r.yg)("p",null,"The elements can be mapped effectfully via ",(0,r.yg)("inlineCode",{parentName:"p"},"transformSTM"),":"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval transformSTMTSet: UIO[TSet[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  _    <- tSet.transformSTM(a => STM.succeed(a * a))\n} yield tSet).commit\n")),(0,r.yg)("p",null,"Folds the elements of a ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet")," using the specified associative binary operator:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval foldTSet: UIO[Int] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  sum  <- tSet.fold(0)(_ + _)\n} yield sum).commit\n")),(0,r.yg)("p",null,"The elements can be folded effectfully via ",(0,r.yg)("inlineCode",{parentName:"p"},"foldSTM"),":"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval foldSTMTSet: UIO[Int] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  sum  <- tSet.foldSTM(0)((acc, el) => STM.succeed(acc + el))\n} yield sum).commit\n")),(0,r.yg)("h2",{id:"perform-effects-for-tset-elements"},"Perform effects for TSet elements"),(0,r.yg)("p",null,(0,r.yg)("inlineCode",{parentName:"p"},"foreach")," is used for performing an STM effect for each element in set:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval foreachTSet = (for {\n  tSet   <- TSet.make(1, 2, 3, 4)\n  tQueue <- TQueue.unbounded[Int]\n  _      <- tSet.foreach(a => tQueue.offer(a).unit)\n} yield tSet).commit\n")),(0,r.yg)("h2",{id:"check-tset-membership"},"Check TSet membership"),(0,r.yg)("p",null,"Checking whether the element is present in a ",(0,r.yg)("inlineCode",{parentName:"p"},"TSet"),":"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval tSetContainsElem: UIO[Boolean] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  res  <- tSet.contains(3)\n} yield res).commit\n")),(0,r.yg)("h2",{id:"convert-tset-to-a-list"},"Convert TSet to a List"),(0,r.yg)("p",null,"List of set elements can be obtained as follows:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval tSetToList: UIO[List[Int]] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  list <- tSet.toList\n} yield list).commit\n")),(0,r.yg)("h2",{id:"size-of-a-tset"},"Size of a TSet"),(0,r.yg)("p",null,"Set's size can be obtained as follows:"),(0,r.yg)("pre",null,(0,r.yg)("code",{parentName:"pre",className:"language-scala"},"import zio._\nimport zio.stm._\n\nval tSetSize: UIO[Int] = (for {\n  tSet <- TSet.make(1, 2, 3, 4)\n  size <- tSet.size\n} yield size).commit\n")))}f.isMDXComponent=!0}}]);