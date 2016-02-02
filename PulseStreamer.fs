namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open Endorphin.Core
open Interface 
open System

[<RequireQualifiedAccess>]
module PulseStreamer = 
    let private httpAddress device = device.Address

    let internal checkStatus = function
        | Ok                -> Choice.succeed ()
        | HasError message  -> Choice.fail (Exception message)

    let openDevice address = 
        CommandRequestAgent.create (httpAddress >> sprintf "PulseStreamer8/2 %s") (fun () -> async {
            let initialisationResponse = Interface.connectDevice address
            match initialisationResponse with 
                | Success     -> return Choice.succeed {Address = address}
                | Failure exn -> return Choice.fail exn })
        |> Async.map PulseStreamer8

    let close (PulseStreamer8 pulseStreamer) = 
        pulseStreamer |> CommandRequestAgent.close
            (fun device -> Interface.disconnect (httpAddress device) |> checkStatus)

    module PulseSequence = 
        let writeSequence sequence iterations (PulseStreamer8 pulseStreamer) =
            pulseStreamer |> CommandRequestAgent.performCommand ("Write sequence to device")
                (fun device -> Interface.writeSequence sequence iterations (httpAddress device) |> checkStatus)