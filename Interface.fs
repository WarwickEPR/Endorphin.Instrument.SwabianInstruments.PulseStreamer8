namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open System
open FSharp.Data
open Newtonsoft.Json
open FifteenBelow.Json
open Endorphin.Core.Choice
open System.Collections.Generic

module Interface = 
    [<AutoOpen>]
    module JsonRpc = 
        let private randomId = System.Random()

        type Notification<'T> = 
            { JsonRpc   : decimal
              Method    : string
              Params    : 'T }
   
        type Request<'T> = 
            { JsonRpc   : decimal
              Method    : string
              Params    : 'T
              Id        : int }

        type Error<'T> = 
            { Code      : int
              Message   : string
              Data      : option<'T> }

        type Response<'T1,'T2> = 
            { JsonRpc   : decimal
              Result    : option<'T1>
              Error     : option<Error<'T2>>
              Id        : int }

        let createRequest requestMethod (requestParams : 'T) : Request<'T>  = 
            { JsonRpc = 2.0m; Method = requestMethod; Params = requestParams; Id = randomId.Next(0, 100) }

        let createNotification notificationMethod notificationParams : Notification<'T> = 
            { JsonRpc = 2.0m; Method = notificationMethod; Params = notificationParams }

        let lowercaseSerialiser = Newtonsoft.Json.Serializer.LowercaseJsonSerializer()

        let postToAddress address postBody = 
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

    let writeSequence sequence (iterations : uint32) address = 
        let encodedSequence = Pulse.Encode.encode sequence
        createRequest "sequence" (sequence, 0u, 0.0f, 0.0f, iterations)
        |> postToAddress address
        |> deserialiseResponse<string, string>