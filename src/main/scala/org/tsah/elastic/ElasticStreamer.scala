package org.tsah.elastic

import java.text.SimpleDateFormat
import java.util.Date

import akka.Done
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.alpakka.elasticsearch.IncomingMessage
import akka.stream.alpakka.elasticsearch.scaladsl.ElasticsearchSink
import akka.stream.scaladsl.Source
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder
import org.elasticsearch.client.{RestClient, RestClientBuilder}
import org.tsah.app.FilesFlow
import org.tsah.model.Transaction
import spray.json.{DefaultJsonProtocol, JsString, JsValue, JsonFormat}

import scala.concurrent.{Await, Future}
import scala.util.Try

object ElasticStreamer extends DefaultJsonProtocol {
  implicit val client: RestClient = RestClient
    .builder(new HttpHost("localhost",9200))
    .setHttpClientConfigCallback(new RestClientBuilder.HttpClientConfigCallback {
      val credentialsProvider = new BasicCredentialsProvider()
      credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials("elastic", "changeme"))
      override def customizeHttpClient(httpClientBuilder: HttpAsyncClientBuilder): HttpAsyncClientBuilder = {
        httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider)
      }
    })
    .build()
  implicit object DateFormat extends JsonFormat[Date] {
    def write(date: Date) = JsString(dateToIsoString(date))
    def read(json: JsValue): Date = json match {
      case JsString(rawDate) =>
        parseIsoDateString(rawDate)
          .getOrElse(throw new RuntimeException(s"Expected ISO Date format, got $rawDate"))
      case error =>
        throw new RuntimeException(s"Expected JsString, got $error")
    }
  }

  private val localIsoDateFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue() = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  }

  private def dateToIsoString(date: Date) =
    localIsoDateFormatter.get().format(date)

  implicit val format: JsonFormat[Transaction] = jsonFormat8(Transaction.apply)

  private def parseIsoDateString(date: String): Option[Date] =
    Try{ localIsoDateFormatter.get().parse(date) }.toOption
  def streamToElastic(transactions: List[Transaction])(implicit actorSystem: ActorSystem, materializer: Materializer): Future[Done] = {
    Source.fromIterator(() => transactions.iterator)
      .map { transaction =>
        IncomingMessage(None, transaction)
      }.runWith(
        ElasticsearchSink.create[Transaction](
          "transactions",
          "transaction"))
  }
}

object ElasticMain extends App {
  import scala.concurrent.duration._
  val transactions = FilesFlow.loadMainFileLines("files")

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: Materializer = ActorMaterializer()

  println(s"Streaming ${transactions.size} transactions.")
  Await.ready(ElasticStreamer.streamToElastic(transactions), 1.hour)
  println("Finished streaming")
  actorSystem.terminate()
}