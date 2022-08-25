use std::io::{Read, Write};
use std::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio};

use assert_cmd::prelude::CommandCargoExt;
use paste::paste;

// TODO: time out on reading after some reasonable time

struct Lox {
    child: Option<Child>,
    stdin: Option<ChildStdin>,
    stdout: ChildStdout,
    stderr: ChildStderr,
}

impl Lox {
    fn new() -> Self {
        let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let mut child = cmd.spawn().unwrap();
        Lox {
            stdin: Some(child.stdin.take().unwrap()),
            stdout: child.stdout.take().unwrap(),
            stderr: child.stderr.take().unwrap(),
            child: Some(child),
        }
    }

    fn assert_stdout<S: AsRef<str>>(&mut self, expected: S) {
        let expected = expected.as_ref();
        let mut buf = vec![b'\0'; expected.len()];
        self.stdout.read_exact(&mut buf).unwrap();
        assert_eq!(expected, std::str::from_utf8(&buf).unwrap());
    }

    fn assert_stderr<S: AsRef<str>>(&mut self, expected: S) {
        let expected = expected.as_ref();
        let mut buf = vec![b'\0'; expected.len()];
        self.stderr.read_exact(&mut buf).unwrap();
        assert_eq!(expected, std::str::from_utf8(&buf).unwrap());
    }

    fn writeln<S: AsRef<str>>(&mut self, s: S) {
        let stdin = self.stdin.as_mut().unwrap();
        stdin.write_all(s.as_ref().as_bytes()).unwrap();
        stdin.write_all(b"\n").unwrap();
        stdin.flush().unwrap();
    }
}

impl Drop for Lox {
    fn drop(&mut self) {
        let mut child = std::mem::take(&mut self.child).unwrap();
        drop(std::mem::take(&mut self.stdin).unwrap());
        child.wait().unwrap();

        let mut buf = vec![];
        self.stderr.read_to_end(&mut buf).unwrap();
        assert_eq!(
            "",
            std::str::from_utf8(buf.as_slice()).unwrap(),
            "Unconsumed STDERR"
        );
    }
}

macro_rules! lox_test {
    (out $lox:ident, E $o:literal) => {
        $lox.assert_stderr($o); $lox.assert_stderr("\n");
    };

    (out $lox:ident, $o:literal) => {
        $lox.assert_stdout($o); $lox.assert_stdout("\n");
    };

    ($n:ident, { $(
        > $i:literal
        $(
            $($p:ident)? $o:literal
        )*
    )* }) => { paste! {
        #[test]
        fn [<test_ $n>]() {
            let mut lox = Lox::new();
            $(
                lox.assert_stdout("> ");
                lox.writeln($i);
                $(lox_test!(out lox, $($p)? $o);)*
            )*
        }
    }};
}

lox_test!(smoke, {
    > "print 3;"
    "3"
    > "print 1 + 2 != 3 ? \"bad\" : \"good\";"
    "good"
});

lox_test!(variable_definition, {
    > "var x = 3;"
    > "print x == 3;"
    "true"
});

lox_test!(undefined_variable, {
    > "var x = 4;"
    > "print y;"
    E "Undefined variable y at 0:6"
});

lox_test!(assignment, {
    > "var x = true;"
    > "print x = 2;"
    "2"
    > "print x;"
    "2"
});

lox_test!(assignment_undefined_variable, {
    > "x = 2;"
    E "Undefined variable x at 0:0"
});
