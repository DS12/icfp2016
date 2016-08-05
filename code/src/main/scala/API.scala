import scalaj.http.{Http, HttpRequest}
import play.api.libs.json._

object API {
  val KEY = "52-1024824841c110210d78a869bd112dd9"
  val keyHeader = ("X-API-Key", KEY)
  val encHeader = ("Accept-Encoding", "gzip")
  val helloWorldURL = "http://2016sv.icfpcontest.org/api/hello"
  def blobLookupURL(hash: String): String = s"http://2016sv.icfpcontest.org/api/blob/$hash"
  val snapshotURL = "http://2016sv.icfpcontest.org/api/snapshot/list"
  val problemSubmitURL = "http://2016sv.icfpcontest.org/api/problem/submit"
  val solutionSubmitURL = "http://2016sv.icfpcontest.org/api/solution/submit"


  def basicGET(url: String): JsValue = Json.parse(
    Http(url).headers(keyHeader, encHeader).asString.body
  )

  def helloWorld: JsValue = basicGET(helloWorldURL)

  case class Snapshot(snapshot_hash: String, snapshot_time: Long)
  implicit val snapEnc = Json.format[Snapshot] 

  def snapshots: List[Snapshot] = (basicGET(snapshotURL) \ "snapshots").get.as[List[Snapshot]]
  def latestSnapshotHash: String = snapshots.sortBy(-_.snapshot_time).head.snapshot_hash

  def blob(snapHash: String) = basicGET(blobLookupURL(snapHash))
}
