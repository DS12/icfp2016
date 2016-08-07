package icfp

import scalaj.http.{Http, HttpRequest, HttpOptions}
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

  case class JSSnapshot(snapshot_hash: String, snapshot_time: Long)
  implicit val snapEnc = Json.format[JSSnapshot]

  case class JSRanking(resemblance: Double, solution_size: Int)
  implicit val rankEnc = Json.format[JSRanking]

  case class JSProblem(
                        ranking: Seq[JSRanking],
                        publish_time: Long,
                        solution_size: Int,
                        problem_id: Int,
                        owner: String,
                        problem_size: Int,
                        problem_spec_hash: String)
  implicit val probEnc = Json.format[JSProblem]

  case class JSProbSubResponse(
                                ok: Boolean,
                                problem_id: Int,
                                publish_time: Long,
                                solution_spec_hash: String,
                                solution_size: Int,
                                problem_spec_hash: String,
                                problem_size: Int)
  implicit val jspsrEnc = Json.format[JSProbSubResponse]

  val waitTime = 3000

  def basicGET(url: String): String = {
    //println("called basicGET")
    Thread.sleep(waitTime)

    Http(url).
      headers(keyHeader, encHeader).
      option(HttpOptions.followRedirects(true)).
      asString.body
  }

  def JsonGET(url: String): JsValue = Json.parse(basicGET(url))

  def basicPOST(url: String, vals: Seq[(String, String)]): String = {
    //println("called basicPOST")
    Thread.sleep(waitTime)

    Http(url).
      headers(keyHeader, encHeader).
      option(HttpOptions.followRedirects(true)).
      postForm(vals).
      asString.body
  }

  def JsonPOST(url: String, vals: Seq[(String, String)]): JsValue = Json.parse(basicPOST(url, vals))

  def helloWorld: JsValue = JsonGET(helloWorldURL)

  def snapshots: List[JSSnapshot] = {
    val tmp = JsonGET(snapshotURL)
    (tmp \ "snapshots").get.as[List[JSSnapshot]]
  }

  def latestSnapshotHash: String =
    snapshots.sortBy(-_.snapshot_time).head.snapshot_hash

  def blob(snapHash: String): JsValue = JsonGET(blobLookupURL(snapHash))

  def getProblems: Seq[JSProblem] = {
    ( blob(latestSnapshotHash) \ "problems").as[List[JSProblem]]
  }

  def getProblemText(prob: JSProblem): (Int, String) = {
    val psh = prob.problem_spec_hash
    (prob.problem_id, basicGET(blobLookupURL(psh)))
  }

  def submitProblemSoln(probId: Int, soln: String) =
    basicPOST(
      solutionSubmitURL,
      List("problem_id" -> probId.toString, "solution_spec" -> soln.toString)
    )//.as[JSProbSubResponse]

}

object submit  {
  import API._
  val problems = getProblems
  val responses =
    for { p <- problems } yield {
    val (id, probText) = getProblemText(p)
    val solStr = SimpleFold(probText)().solution
    val out = submitProblemSoln(id, solStr)
    println(out)
    out
  }


}