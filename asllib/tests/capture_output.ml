open Asllib

let stdout_buf = Buffer.create 256
let stderr_buf = Buffer.create 256

let args =
  Runner.
    {
      default_args with
      exec = true;
      capture_output = Some (stdout_buf, stderr_buf);
      files = [ (NormalV1, "capture_output.asl") ];
    }

let code = Runner.safe_run args

let () =
  Printf.printf "exit code: %d\n\nstdout:\n%s\n\nstderr:\n%s\n" code
    (Buffer.contents stdout_buf)
    (Buffer.contents stderr_buf)
