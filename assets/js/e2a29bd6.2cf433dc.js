"use strict";(self.webpackChunkzio_site=self.webpackChunkzio_site||[]).push([[42364],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>k});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var d=r.createContext({}),l=function(e){var t=r.useContext(d),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},c=function(e){var t=l(e.components);return r.createElement(d.Provider,{value:t},e.children)},u="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},f=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,d=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),u=l(n),f=a,k=u["".concat(d,".").concat(f)]||u[f]||p[f]||o;return n?r.createElement(k,i(i({ref:t},c),{},{components:n})):r.createElement(k,i({ref:t},c))}));function k(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=f;var s={};for(var d in t)hasOwnProperty.call(t,d)&&(s[d]=t[d]);s.originalType=e,s[u]="string"==typeof e?e:a,i[1]=s;for(var l=2;l<o;l++)i[l]=n[l];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}f.displayName="MDXCreateElement"},93738:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>d,contentTitle:()=>i,default:()=>p,frontMatter:()=>o,metadata:()=>s,toc:()=>l});var r=n(87462),a=(n(67294),n(3905));const o={id:"annotated-tests",title:"Annotated Tests"},i=void 0,s={unversionedId:"zio-kafka/annotated-tests",id:"zio-kafka/annotated-tests",title:"Annotated Tests",description:"By way of examples, we present some annotated tests from the test suite.",source:"@site/docs/zio-kafka/annotated-tests.md",sourceDirName:"zio-kafka",slug:"/zio-kafka/annotated-tests",permalink:"/zio-kafka/annotated-tests",draft:!1,editUrl:"https://github.com/zio/zio/edit/series/2.x/docs/zio-kafka/annotated-tests.md",tags:[],version:"current",frontMatter:{id:"annotated-tests",title:"Annotated Tests"},sidebar:"ecosystem-sidebar",previous:{title:"Serialization And Deserialization",permalink:"/zio-kafka/serialization-and-deserialization"},next:{title:"Introduction",permalink:"/zio-keeper/"}},d={},l=[{value:"General Notes",id:"general-notes",level:2},{value:"First Producer Test",id:"first-producer-test",level:2},{value:"Kafka service",id:"kafka-service",level:3},{value:"Kafka Test Environment",id:"kafka-test-environment",level:3},{value:"Back to the producer",id:"back-to-the-producer",level:4}],c={toc:l},u="wrapper";function p(e){let{components:t,...n}=e;return(0,a.kt)(u,(0,r.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"By way of examples, we present some annotated tests from the test suite."),(0,a.kt)("p",null,"Tests are written using zio-test."),(0,a.kt)("h2",{id:"general-notes"},"General Notes"),(0,a.kt)("p",null,"The tests make use of KafkaTestUtils.scala which comprises a number of helper methods\nfor testing zio-kafka. You may find it useful to copy this file into your own test\nfolder for writing your kafka-based tests (there is no zio-test-utils project\nat present). Relevant portions of the KafkaTestUtils will be introduced as we work\nthrough the tests."),(0,a.kt)("h2",{id:"first-producer-test"},"First Producer Test"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'object ProducerSpec extends DefaultRunnableSpec {\n  override def spec =\n    suite("producer test suite")(\n      test("one record") {\n        for {\n          _ <- Producer.produce(new ProducerRecord("topic", "boo", "baa"), Serde.string, Serde.string)\n        } yield assertCompletes\n      },\n      // ...\n    ).provideSomeLayerShared[TestEnvironment](\n      (Kafka.embedded >+> (producer ++ transactionalProducer))\n        .mapError(TestFailure.fail) ++ Clock.live\n    )\n}\n')),(0,a.kt)("p",null,"First note the ",(0,a.kt)("inlineCode",{parentName:"p"},".provideSomeLayerShared"),". This gives the tests a ",(0,a.kt)("inlineCode",{parentName:"p"},"Kafka")," service\nadded to a full ",(0,a.kt)("inlineCode",{parentName:"p"},"TestEnvironment")," (this is needed because we want to provide both\nLive clock and the Kafka service)"),(0,a.kt)("h3",{id:"kafka-service"},"Kafka service"),(0,a.kt)("p",null,"This follows the module pattern (see main ZIO docs)"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'trait Kafka {\n  def bootstrapServers: List[String]\n  def stop(): UIO[Unit]\n}\n\nobject Kafka {\n  trait Service {\n    def bootstrapServers: List[String]\n    def stop(): UIO[Unit]\n  }\n  final case class EmbeddedKafkaService(embeddedK: EmbeddedK) extends Kafka.Service {\n    override def bootstrapServers: List[String] = List(s"localhost:${embeddedK.config.kafkaPort}")\n    override def stop(): UIO[Unit]              = ZIO.succeed(embeddedK.stop(true))\n  }\n\n  case object DefaultLocal extends Kafka.Service {\n    override def bootstrapServers: List[String] = List(s"localhost:9092")\n    override def stop(): UIO[Unit]              = UIO.unit\n  }\n\n  val embedded: ZLayer[Any, Throwable, Kafka] = ZLayer.fromScoped {\n    implicit val embeddedKafkaConfig: EmbeddedKafkaConfig = EmbeddedKafkaConfig(\n      customBrokerProperties = Map("group.min.session.timeout.ms" -> "500", "group.initial.rebalance.delay.ms" -> "0")\n    )\n    ZIO.acquireRelease(EmbeddedKafkaService(EmbeddedKafka.start()))\n  }(_.stop())\n\n  val local: ZLayer[Any, Nothing, Kafka] = ZLayer.succeed(DefaultLocal)\n')),(0,a.kt)("p",null,"In fact there are 2 provided implementations of service. The first is for the unit\ntests and makes use of ",(0,a.kt)("a",{parentName:"p",href:"https://github.com/embeddedkafka/embedded-kafka"},"EmbeddedKafka")),(0,a.kt)("p",null,"The second uses the default local port and is suitable for a stand-alone kafka\n(I used docker installation). You could create your own ",(0,a.kt)("inlineCode",{parentName:"p"},"Kafka")," service for testing\nagainst remote servers (but security would need to be added)."),(0,a.kt)("p",null,"Note the use of ",(0,a.kt)("inlineCode",{parentName:"p"},"ZLayer.fromAcquireRelease")," to ensure the service is also stopped."),(0,a.kt)("h3",{id:"kafka-test-environment"},"Kafka Test Environment"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'object KafkaTestUtils {\n\n  val producerSettings: ZIO[Kafka, Nothing, ProducerSettings] =\n    ZIO.serviceWith[Kafka](_.bootstrapServers).map(ProducerSettings(_))\n\n  val producer: ZLayer[Kafka, Throwable, Producer] =\n    (ZLayer(producerSettings) ++ ZLayer.succeed(Serde.string: Serializer[Any, String])) >>>\n      Producer.live\n\n  val transactionalProducerSettings: ZIO[Kafka, Nothing, TransactionalProducerSettings] =\n    ZIO.serviceWith[Kafka](_.bootstrapServers).map(TransactionalProducerSettings(_, "test-transaction"))\n\n  val transactionalProducer: ZLayer[Kafka, Throwable, TransactionalProducer] =\n    (ZLayer.fromZIO(transactionalProducerSettings) ++ ZLayer.succeed(\n      Serde.string: Serializer[Any, String]\n    )) >>>\n      TransactionalProducer.live\n\n  // ...\n')),(0,a.kt)("h4",{id:"back-to-the-producer"},"Back to the producer"),(0,a.kt)("p",null,"These ",(0,a.kt)("inlineCode",{parentName:"p"},"ZLayer"),"s are provided to the ZIO environment for the ",(0,a.kt)("inlineCode",{parentName:"p"},"ProducerSpec")," shown\nabove, along with the Live clock (which is essential when running code that\nuses scheduling or other timing features - as does much of zio-kafka)"),(0,a.kt)("p",null,"With the resource management encapsulated in the layers, the actual producer\noperation function is simply used from the environment"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'        for {\n          _ <- Producer.produce(new ProducerRecord("topic", "boo", "baa"), Serde.string, Serde.string)\n        } yield // ...\n')),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"Producer.produce")," takes a ",(0,a.kt)("inlineCode",{parentName:"p"},"ProducerRecord"),' (defined in the Java Kafka client on which\nthis library is based). In this case the topic is "topic" and the key and value\n"boo" and "baa", corresponding to the given string de/serializers.'))}p.isMDXComponent=!0}}]);