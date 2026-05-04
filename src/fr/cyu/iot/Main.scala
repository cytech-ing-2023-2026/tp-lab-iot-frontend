package fr.cyu.iot

import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.*
import tyrian.*
import tyrian.Html.*
import tyrian.SVG.{pattern as svgPattern, *}
import tyrian.http.Http
import tyrian.http.Request
import tyrian.websocket.WebSocket
import zio.*
import zio.interop.catz.*
import zio.json.*

@JSExportTopLevel("TyrianApp")
object Main extends TyrianZIOApp[Msg, Model]:

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[Task, Msg]) =
    (Model.default, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[Task, Msg]) =
    case Msg.SetAddress(value)          => (model.copy(address = value), Cmd.None)
    case Msg.Connected(socket)          => (model.copy(socket = Some(socket), status = Status.Success(s"Connected to ${model.address}")), Cmd.None)
    case Msg.Connect                    => (model, WebSocket.connect(model.socketEndpoint)(Msg.decodeConnect))
    case Msg.NetworkError(reason)       => (model.copy(status = Status.Error(reason)), Cmd.None)
    case Msg.Receive(data)              => (model.copy(sensors = Some(data)), Cmd.None)
    case Msg.Disconnected(1000, _)      => (model.copy(socket = None, status = Status.Neutral("Disconnected")), Cmd.None)
    case Msg.Disconnected(code, reason) => (model.copy(socket = None, status = Status.Error(s"Disconnected: $reason")), Cmd.None)
    case Msg.NoOp                       => (model, Cmd.None)

  def subscriptions(model: Model): Sub[Task, Msg] =
    model.socket.fold(Sub.None)(_.subscribe(Msg.decodeEvent))

  def formatTime(uptime: Long): String =
    val hours = uptime / 3_600_000
    val minutes = uptime % 3_600_000 / 60_000
    val seconds = uptime % 60_000 / 1_000

    String.format("%02d:%02d:%02d", hours, minutes, seconds)

  def viewJoystick(joystick: Joystick): List[Html[Msg]] = List(
    tr(
      td("X"),
      td(joystick.x.toString)
    ),
    tr(
      td("Y"),
      td(joystick.y.toString)
    ),
    tr(
      td("Pressed"),
      td(joystick.pressed.toString)
    )
  )

  def viewTMG(tmg: TMG): List[Html[Msg]] = List(
    tr(
      td("Proximity"),
      td(tmg.proximity.toString)
    ),
    tr(
      td("Color"),
      td(
        table(cls := "table table-xs")(
          tr(
            td("Red"),
            td(tmg.color.red.toString)
          ),
          tr(
            td("Blue"),
            td(tmg.color.blue.toString)
          ),
          tr(
            td("Green"),
            td(tmg.color.green.toString)
          ),
          tr(
            td("Clear"),
            td(tmg.color.clear.toString)
          ),
          tr(
            td("Lux"),
            td(tmg.color.lux.toString)
          ),
          tr(
            td("CCT"),
            td(tmg.color.cct.toString)
          )
        )
      )
    )
  )

  def viewBME(bme: BME): List[Html[Msg]] = List(
    tr(
      td("Temperature (°C)"),
      td(bme.temperature.toString)
    ),
    tr(
      td("Humidity"),
      td(bme.humidity.toString)
    ),
    tr(
      td("Pressure"),
      td(bme.pressure.toString)
    ),
    tr(
      td("Gas"),
      td(bme.gas.toString)
    )
  )

  def section(name: String): Html[Msg] =
    thead(
      tr(
        th(name)
      )
    )

  def viewSensors(sensors: Sensors): Html[Msg] =
    table(cls := "table table-zebra")(
      List(
        section("General"),
        tr(
          td("Uptime"),
          td(formatTime(sensors.uptime))
        ),
        tr(
          td("Heartbeat (mv)"),
          td(sensors.heartbeat.toString)
        ),
        section("Joystick")
      )
        ++ viewJoystick(sensors.joystick)
        ++ List(section("BME"))
        ++ sensors.bme.fold(Nil)(viewBME)
        ++ List(section("TMG"))
        ++ sensors.tmg.fold(Nil)(viewTMG)
    )

  def status(message: String): Html[Msg] =
    div(cls := "flex flex-col")(
      label(cls := "font-bold")("Status:"),
      p(message)
    )

  def view(model: Model): Html[Msg] =
    div(cls := "w-full h-full flex flex-col justify-start items-center gap-10 py-10")(
      h1(cls := "text-6xl font-bold text-cyan-400")("Sensors monitor"),
      div(cls := "w-5xl h-full flex flex-col justify-start items-center gap-10")(
        div(cls := "w-full flex flex-col items-center gap-2")(
          div(cls := "join")(
            div(cls := "flex flex-col")(
              label(cls := "input validator")(
                svg(cls := "h-[2em] opacity-50", xmlns := "http://www.w3.org/2000/svg", viewBox := "0 0 24 24")(
                  g(
                    Attribute("stroke-linejoin", "round"),
                    Attribute("stroke-linecap", "round"),
                    Attribute("stroke-width", "2.5"),
                    fill := "none",
                    stroke := "currentColor"
                  )(
                    path(d := "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"),
                    path(d := "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71")
                  )
                ),
                label(cls := "opacity-50")("ws://"),
                input(
                  `type` := "text",
                  required,
                  placeholder := "127.0.0.1",
                  onInput(Msg.SetAddress.apply)
                )
              )
            ),
            button(
              cls := "btn btn-info join-item",
              onClick(Msg.Connect)
            )(
              if model.socket.isDefined then
                if model.sensors.isDefined then span("Disconnect")
                else span(cls := "swap-off loading loading-spinner")("")
              else span("Connect")
            )
          ),
          model.status match
            case Status.Neutral(message) => div(cls := "alert alert-info alert-outline min-w-md")(status(message))
            case Status.Success(message) => div(cls := "alert alert-success alert-outline min-w-md")(status(message))
            case Status.Error(message)   => div(cls := "alert alert-error alert-outline min-w-md")(status(message))
        ),
        model.sensors.fold(div())(viewSensors)
      )
    )
