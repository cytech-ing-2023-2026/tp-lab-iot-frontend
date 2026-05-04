package fr.cyu.iot

import tyrian.websocket.WebSocket
import tyrian.websocket.WebSocketConnect
import tyrian.websocket.WebSocketEvent
import zio.Task
import zio.json.*

enum Msg:
  case SetAddress(value: String)
  case Connected(socket: WebSocket[Task])
  case Connect
  case NetworkError(reason: String)
  case Receive(data: Sensors)
  case Disconnected(code: Int, reason: String)
  case CheckTimeout
  case NoOp

object Msg:
  def decodingFailed(reason: String): Msg =
    Msg.NetworkError(s"Decoding failure: $reason")

  def decodeConnect(connect: WebSocketConnect[Task]): Msg = connect match
    case WebSocketConnect.Error(msg)        => Msg.NetworkError(msg)
    case WebSocketConnect.Socket(webSocket) => Msg.Connected(webSocket)

  def decodeEvent(event: WebSocketEvent): Msg = event match
    case WebSocketEvent.Close(code, reason) => Msg.Disconnected(code, reason)
    case WebSocketEvent.Error(reason)       => Msg.NetworkError(reason)
    case WebSocketEvent.Heartbeat           => Msg.NoOp
    case WebSocketEvent.Open                => Msg.NoOp
    case WebSocketEvent.Receive(message)    => message.fromJson[Sensors].fold(decodingFailed, Msg.Receive.apply)
