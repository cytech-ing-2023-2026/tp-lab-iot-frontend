package fr.cyu.iot

import tyrian.websocket.WebSocket
import zio.Task
import zio.json.*

case class Color(red: Double, green: Double, blue: Double, clear: Double, lux: Int, cct: Int) derives JsonDecoder
case class TMG(proximity: Double, color: Color) derives JsonDecoder
case class BME(temperature: Double, humidity: Double, pressure: Double, gas: Double) derives JsonDecoder
case class Joystick(x: Double, y: Double, pressed: Boolean) derives JsonDecoder
case class Sensors(uptime: Long, heartbeat: Int, joystick: Joystick, bme: Option[BME], tmg: Option[TMG]) derives JsonDecoder

enum Status:
  case Neutral(message: String)
  case Success(message: String)
  case Error(reason: String)

case class Model(address: String, socket: Option[WebSocket[Task]], lastMessage: Option[Long], status: Status, sensors: Option[Sensors]):
  def socketEndpoint: String = s"ws://$address"

object Model:
  val default: Model = Model("", None, None, Status.Neutral("Idle"), None)
