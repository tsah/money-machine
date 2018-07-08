package org.tsah.elastic

import java.text.SimpleDateFormat
import java.util.Date

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.alpakka.elasticsearch.IncomingMessage
import akka.stream.alpakka.elasticsearch.scaladsl.ElasticsearchSink
import akka.stream.scaladsl.Source
import org.apache.http.HttpHost
import org.elasticsearch.client.RestClient
import org.tsah.model.Transaction
import spray.json.{DefaultJsonProtocol, JsString, JsValue, JsonFormat}

import scala.util.Try

object ElasticStreamer extends DefaultJsonProtocol {
  implicit val client: RestClient = RestClient.builder(new HttpHost(implicit object DateFormat extends JsonFormat[Date] {
    def write(date: Date) = JsString(dateToIsoString(date))
    def read(json: JsValue) = json match {
      case JsString(rawDate) =>
        parseIsoDateString(rawDate)
          .fold(JsErr(s"Expected ISO Date format, got $rawDate"))(identity)
      case error => deserializationError(s"Expected JsString, got $error")
    }
  }

  private val localIsoDateFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue() = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  }

  private def dateToIsoString(date: Date) =
    localIsoDateFormatter.get().format(date)

  private def parseIsoDateString(date: String): Option[Date] =
    Try{ localIsoDateFormatter.get().parse(date) }.toOption
  implicit val format: JsonFormat[Transaction] = jsonFormat8(Transaction.apply)
  implicit val materializer: Materializer = ActorMaterializer()
  implicit val actorSystem: ActorSystem = ActorSystem()

  def streamToElastic(transactions: List[Transaction]): Unit = {
    Source.fromIterator(() => transactions.iterator)
      .map { transaction =>
        IncomingMessage(None, transaction)
      }.runWith(
        ElasticsearchSink.create[Transaction](
          "transactions",
          "transaction"))
  }
}
