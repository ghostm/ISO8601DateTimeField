import net.liftweb.record.field.DateTimeField
import net.liftweb.http.js.JE.{Str, JsNull}
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.TimeHelpers._
import java.text.SimpleDateFormat
import java.util.{TimeZone, Locale, Date, Calendar}
import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.record.Record

class ISO8601DateTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends DateTimeField[OwnerType](rec: OwnerType) {
  val utc = TimeZone.getTimeZone("UTC")

  private final def dateToCal(d: Date): Calendar = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal
  }

  def jsonDateFormatter = {
    //val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z", Locale.US)
    val ret = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssz", Locale.US)
    ret.setTimeZone(utc)
    ret
  }

  def toDate(in: Any): Box[Date] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(d)
        case lng: Long => Full(new Date(lng))
        case lng: Number => Full(new Date(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s: String => tryo(jsonDateFormatter.parse(s)) or tryo(dateFormatter.parse(s))
        case o => toDate(o.toString)
      }
    } catch {
      case e => Failure("Bad date: " + in, Full(e), Empty)
    }
  }

  def boxParseJsonDate(dateString: String): Box[Date] = tryo {
    jsonDateFormatter.parse(dateString)
  }

  def parseJsonDate(dateString: String): Date = tryo {
    jsonDateFormatter.parse(dateString)
  } openOr new Date(0L)

  /**@return a date formatted with the internet format */
  def toJsonDate(in: Date): String = jsonDateFormatter.format(in)

  /**@return a date formatted with the internet format (from a number of millis) */
  def toJsonDate(in: Long): String = jsonDateFormatter.format(new Date(in))

  override def setFromAny(in: Any): Box[Calendar] = toDate(in).flatMap(d => setBox(Full(dateToCal(d)))) or genericSetFromAny(in)

  override def setFromString(s: String): Box[Calendar] = s match {
    case "" if optional_? => setBox(Empty)
    case other => setBox(tryo(dateToCal(parseJsonDate(s))))
  }

  override def asJs = valueBox.map(v => Str(toJsonDate(v.getTime))) openOr JsNull

  override def asJValue = asJString(v => toJsonDate(v.getTime))

  override def setFromJValue(jvalue: JValue) = setFromJString(jvalue) {
    v => boxParseJsonDate(v).map(d => {
      val cal = Calendar.getInstance
      cal.setTime(d)
      cal
    })
  }
}