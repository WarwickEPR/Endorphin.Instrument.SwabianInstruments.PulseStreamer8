namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open System
open FSharp.Data
open Newtonsoft.Json
open FifteenBelow.Json
open Endorphin.Core.Choice
open System.Collections.Generic
open Endorphin.Instrument.SwabianInstruments.PulseStreamer8.Parse

module Interface = 
    [<AutoOpen>]
    module JsonRpc = 
        let private randomId = System.Random()

        type Notification<'T> = 
            { JsonRpc   : string
              Method    : string
              Params    : 'T }
   
        type Request<'T> = 
            { JsonRpc   : string
              Method    : string
              Params    : 'T
              Id        : int }

        type Error<'T> = 
            { Code      : int
              Message   : string
              Data      : option<'T> }

        type Response<'T1,'T2> = 
            { JsonRpc   : string
              Result    : option<'T1>
              Error     : option<Error<'T2>>
              Id        : int }

        let createRequest requestMethod (requestParams : 'T) : Request<'T>  = 
            { JsonRpc = "2.0"; Method = requestMethod; Params = requestParams; Id = randomId.Next(0, 100) }

        let createNotification notificationMethod notificationParams : Notification<'T> = 
            { JsonRpc = "2.0"; Method = notificationMethod; Params = notificationParams }

        let lowercaseSerialiser = Newtonsoft.Json.Serializer.LowercaseJsonSerializer()

        let postToAddress address postBody = 
            printfn "%A" (lowercaseSerialiser.SerializeObject(postBody))
            Http.RequestString(url=address, body=TextRequest(lowercaseSerialiser.SerializeObject(postBody)))

        let deserialiseResponse<'T1,'T2> response : Response<'T1,'T2> = 
            lowercaseSerialiser.DeserializeObject<Response<'T1,'T2>>(response)

    let (|Ok|HasError|) (response : JsonRpc.Response<'T1,'T2>) = 
        match (response.Error, response.Result) with
        | (Some error, None)       -> HasError (error.Message)
        | (None, Some result)      -> Ok
        | _                        -> failwithf "Unexpected response - received neither a success or error"

    let internal checkJsonRequestStatus = function
        | Ok                -> Choice.succeed ()
        | HasError message  -> Choice.fail (Exception message)

    let connectDevice address =  
        createRequest "version" []
        |> postToAddress address
        |> deserialiseResponse<string, string>
        |> checkJsonRequestStatus

    let disconnect address = 
        createRequest "stop" []
        |> postToAddress address
        |> deserialiseResponse<string, string>

    let writeSequence sequence (iterations : uint32) finalState errorState (triggerMode : TriggerMode) address = 
        let encodedSequence = Pulse.Encode.encode sequence

        createRequest "stream" ( encodedSequence, 
                                   iterations, 
                                   (0 , 0x00, 0u, 0u),
                                   (finalState.Length, ((Parse.channelMask finalState.Sample.Channels) |> byte), finalState.Sample.Analogue0, finalState.Sample.Analogue1), 
                                   (errorState.Length, ((Parse.channelMask errorState.Sample.Channels) |> byte), errorState.Sample.Analogue0, errorState.Sample.Analogue1), 
                                   (triggerEnum triggerMode) )
        |> postToAddress address
        |> deserialiseResponse<string, string>

    let setState state address = 
        createRequest "constant" [(0x00, Pulse.Encode.encodeChannels (Pulse.sample state), 0u, 0u)]
        |> postToAddress address
        |> deserialiseResponse<string, string>