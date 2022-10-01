const examples = {
  greeter: `class Greeter {
    init(name) {
        this.name = name;
    }

    greet(you) {
        print "Hi " + you + ", I'm " + this.name;
    }
}

var g = Greeter("jlox-rs");
g.greet("esteemed user");
`,

  closures: `fun makeCounter() {
    // \`i\` will be captured in the closure of \`count\`
    var i = 0;
    fun count() {
        i = i + 1;
        print i;
    }

    // Yes we can return a function from a function!
    return count;
}

var counter = makeCounter();
counter();
counter();`,

  advanced_classes: `class Math {
    // This is a static method
    class squared(n) {
        return n * n;
    }
}

class Rectangle {
    init(a, b) {
        this.a = a;
        this.b = b;
    }

    // Note the lack of parentheses; this is a getter
    area {
        return this.a * this.b;
    }

    // Not a special method
    to_string() {
        return "Rectangle(" + this.a + ", " + this.b + ")";
    }
}

// Inheritance
class Square < Rectangle {
    init(a) {
        this.a = a;
    }

    b {
        return this.a;
    }

    to_string() {
        // Note the \`super\` call
        return super.to_string() + "/Rectangle";
    }
}

print Rectangle(1, 2).area;
print Square(4).area == Math.squared(4);
print Square(5).to_string();`,
};

export default examples;
