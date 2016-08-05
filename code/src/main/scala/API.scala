import scalaj.http.Http

object API {
  val KEY = "52-1024824841c110210d78a869bd112dd9"
  val keyHeader = ("X-API-Key", KEY)
  val helloWorldURL = "http://2016sv.icfpcontest.org/api/hello"
  def blobLookupURL(hash: String): String = s"http://2016sv.icfpcontest.org/api/blob/$hash"
  val snapshotURL = "http://2016sv.icfpcontest.org/api/snapshot/list"
  val problemSubmitURL = "http://2016sv.icfpcontest.org/api/problem/submit"
  val solutionSubmitURL = "http://2016sv.icfpcontest.org/api/solution/submit"

  def helloWorld() = {
    val body = Http(helloWorldURL).headers(keyHeader).asString.body
  }

}
