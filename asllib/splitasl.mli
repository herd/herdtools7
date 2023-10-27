(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

(** Split (ASL) lexbuffer at "// =======..." limits *)

val split : Lexing.lexbuf -> (int * string) Seq.t
