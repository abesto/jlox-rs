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
        if self.stdout.read_exact(&mut buf).is_ok() {
            if let Ok(s) = std::str::from_utf8(&buf) {
                assert_eq!(expected, s);
            }
        }
    }

    fn assert_stderr<S: AsRef<str>>(&mut self, expected: S) {
        let expected = expected.as_ref();
        let mut buf = vec![b'\0'; expected.len()];
        if self.stderr.read_exact(&mut buf).is_ok() {
            if let Ok(s) = std::str::from_utf8(&buf) {
                assert_eq!(expected, s);
            }
        }
    }

    fn writeln<S: AsRef<str>>(&mut self, s: S) {
        if let Some(stdin) = self.stdin.as_mut() {
            stdin.write_all(s.as_ref().as_bytes()).unwrap();
            stdin.write_all(b"\n").unwrap();
            stdin.flush().unwrap();
        }
    }

    fn assert_stderr_consumed(&mut self) {
        let mut buf = vec![];
        if self.stderr.read_to_end(&mut buf).is_ok() {
            if let Ok(s) = std::str::from_utf8(buf.as_slice()) {
                assert_eq!("", s, "Unconsumed STDERR");
            }
        }
    }

    fn assert_stdout_consumed(&mut self) {
        let mut buf = vec![];
        if self.stdout.read_to_end(&mut buf).is_ok() {
            assert_eq!(
                "",
                std::str::from_utf8(buf.as_slice()).unwrap(),
                "Unconsumed STDOUT"
            );
        }
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
            let mut child = std::mem::take(&mut lox.child).unwrap();
            if let Some(stdin) = std::mem::take(&mut lox.stdin) {
                drop(stdin);
            }
            let _ = child.wait();
            lox.assert_stderr_consumed();
            lox.assert_stdout("> ");
            lox.assert_stdout_consumed();
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
    > "x = 3;"
    "3"
    > "print x;"
    "3"
});

lox_test!(assignment_undefined_variable, {
    > "x = 2;"
    E "Undefined variable x at 0:0"
});

lox_test!(lexical_scope_shadow, {
    > "var x = \"outer\"; { var x = \"inner\"; print x; } print x;"
    "inner"
    "outer"
});

lox_test!(lexical_scope_assign, {
    > "var x = \"outer\"; { x = \"inner\"; } print x; "
    "inner"
});

lox_test!(lexical_scope_complex, {
    > "var x = \"outer\"; { var x = \"inner \" + x; print x; } print x;"
    "inner outer"
    "outer"
});

lox_test!(interpreter_prints_expression_result, {
    > "var x = 2; x = x + 1; x;"
    "3"
});

lox_test!(uninitialized_variable, {
    > "var x; var y; x = 3; print x; print y;"
    "3"
    E "Uninitialized variable y at 0:36"
});

lox_test!(conditional_if, {
    > "var x = 1;"
    > "if (x == 1) { print x; var y = 2; } print y;"
    "1"
    E "Undefined variable y at 0:42"
});

lox_test!(conditional_if_else, {
    > "if (42) print \"yes\"; else print \"no\";"
    "yes"
    > "if (nil) print \"yes\"; else print \"no\";"
    "no"
});

lox_test!(conditional_chain, {
    > "if (false) print 1; else if (false) print 2; else print 3;"
    "3"
});

lox_test!(parsing_error_report, {
    > "+ 3;"
    E "LHS missing for `+` at 0:0"
    E "Expected expression, found: `;` at 0:3"
    E "Parsing failed, see errors above."
});

lox_test!(short_circuit_logical, {
    > "1 == 2 and 3;"
    "false"
    > "1 == 1 or 3;"
    "true"
    > "1 == 2 and 3 or 4;"
    "4"
});

lox_test!(while_loop, {
    > "var i = 0;"
    > "while (i < 3) { print i; i = i + 1; } print \"done\";"
    "0"
    "1"
    "2"
    "done"
});

lox_test!(for_loop, {
    > "for (var i = 0; i < 5; i = i + 2) print i;"
    "0"
    "2"
    "4"
});

lox_test!(break_statement, {
    > "var x = 0; while (true) { print x; while (true) break; x = x + 1; if (x >= 3) break; }"
    "0"
    "1"
    "2"
    > "while (false) {} break;"
    E "`break` outside loop at 0:7"
    E "Parsing failed, see errors above."
});

lox_test!(clock, {
    > "type(clock());"
    "Number"
});

lox_test!(call_non_callable, {
    > "13();"
    E "Can only call functions and classes, tried to call: `13` of type `Number` at 0:3"
});
