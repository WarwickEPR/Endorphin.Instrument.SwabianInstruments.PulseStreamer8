// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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
        CommandRequestAgent.create (httpAddress >> sprintf "PulseStreamer8/2 (%s)") (fun () -> async {
            return Choice.succeed {Address = address}
            (*let initialisationResponse = Interface.connectDevice address
            match initialisationResponse with
                | Success     -> return Choice.succeed {Address = address}
                | Failure exn -> return Choice.fail exn }*)})
        |> Async.map PulseStreamer8

    let close (PulseStreamer8 pulseStreamer) =
        pulseStreamer |> CommandRequestAgent.close
            (fun device -> Interface.disconnect (httpAddress device) |> checkStatus)

    module PulseSequence =
        let writeSequence sequence iterations finalState errorState triggerMode (PulseStreamer8 pulseStreamer) =
            pulseStreamer |> CommandRequestAgent.performCommand (sprintf "Write sequence of length %d to device" <| Seq.length sequence)
                (fun device -> Interface.writeSequence sequence iterations finalState errorState triggerMode (httpAddress device) |> checkStatus)

        let setState channels (PulseStreamer8 pulseStreamer) =
            pulseStreamer |> CommandRequestAgent.performCommand (sprintf "Setting state of pulseStreamer")
               ( let state = Pulse.create channels 1u
                 fun device -> Interface.setState state (httpAddress device) |> checkStatus )