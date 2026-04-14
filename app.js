(function () {
  "use strict";

  const FUNCTION_ALIASES = new Map(
    [
      ["exp", "exp"],
      ["log", "log"],
      ["ln", "log"],
      ["sin", "sin"],
      ["cos", "cos"],
      ["tan", "tan"],
      ["cot", "cot"],
      ["sec", "sec"],
      ["csc", "csc"],
      ["asin", "asin"],
      ["acos", "acos"],
      ["atan", "atan"],
      ["arcsin", "asin"],
      ["arccos", "acos"],
      ["arctan", "atan"],
      ["sinh", "sinh"],
      ["cosh", "cosh"],
      ["tanh", "tanh"],
      ["asinh", "asinh"],
      ["acosh", "acosh"],
      ["atanh", "atanh"],
      ["arcsinh", "asinh"],
      ["arccosh", "acosh"],
      ["arctanh", "atanh"],
      ["asec", "asec"],
      ["acsc", "acsc"],
      ["acot", "acot"],
      ["arcsec", "asec"],
      ["arccsc", "acsc"],
      ["arccot", "acot"],
      ["sqrt", "sqrt"],
      ["sqr", "sqr"],
      ["half", "half"],
      ["minus", "minus"],
      ["inv", "inv"],
      ["abs", "abs"],
      ["avg", "avg"],
      ["hypot", "hypot"],
      ["logistic", "logistic"],
      ["sigmoid", "logistic"],
      ["logisticsigmoid", "logistic"],
      ["plus", "+"],
      ["times", "*"],
      ["subtract", "-"],
      ["divide", "/"],
      ["power", "^"],
      ["eml", "eml"],
    ].flatMap(([a, b]) => [
      [a, b],
      [capitalize(a), b],
    ])
  );

  function capitalize(value) {
    return value ? value[0].toUpperCase() + value.slice(1) : value;
  }

  function normalizeFunctionName(name) {
    const direct = FUNCTION_ALIASES.get(name);
    if (direct) return direct;
    return FUNCTION_ALIASES.get(name.toLowerCase()) || null;
  }

  function normalizeConstantName(name) {
    const key = name.toLowerCase();
    if (key === "e") return "e";
    if (key === "pi") return "pi";
    if (key === "i") return "i";
    if (key === "goldenratio" || key === "phi") return "goldenratio";
    return null;
  }

  function tokenize(source) {
    const tokens = [];
    let i = 0;

    while (i < source.length) {
      const ch = source[i];
      if (/\s/.test(ch)) {
        i += 1;
        continue;
      }

      if (isDigit(ch) || (ch === "." && isDigit(source[i + 1]))) {
        const start = i;
        while (i < source.length && isDigit(source[i])) i += 1;
        if (source[i] === ".") {
          i += 1;
          while (i < source.length && isDigit(source[i])) i += 1;
        }
        if ((source[i] === "e" || source[i] === "E") && isExponentStart(source, i + 1)) {
          i += 1;
          if (source[i] === "+" || source[i] === "-") i += 1;
          while (i < source.length && isDigit(source[i])) i += 1;
        }
        tokens.push({ type: "number", value: source.slice(start, i) });
        continue;
      }

      if (/[A-Za-z_]/.test(ch)) {
        const start = i;
        i += 1;
        while (i < source.length && /[A-Za-z0-9_]/.test(source[i])) i += 1;
        tokens.push({ type: "id", value: source.slice(start, i) });
        continue;
      }

      if ("+-*/^,()[]".includes(ch)) {
        tokens.push({ type: "op", value: ch });
        i += 1;
        continue;
      }

      throw new SyntaxError(`Unexpected character "${ch}" at position ${i + 1}.`);
    }

    tokens.push({ type: "eof", value: "" });
    return tokens;
  }

  function isDigit(ch) {
    return ch >= "0" && ch <= "9";
  }

  function isExponentStart(source, index) {
    const ch = source[index];
    const next = source[index + 1];
    return isDigit(ch) || ((ch === "+" || ch === "-") && isDigit(next));
  }

  class Parser {
    constructor(tokens) {
      this.tokens = tokens;
      this.pos = 0;
    }

    parse() {
      const expr = this.parseExpression();
      if (this.peek().type !== "eof") {
        throw new SyntaxError(`Unexpected token "${this.peek().value}".`);
      }
      return expr;
    }

    parseExpression() {
      return this.parseAdd();
    }

    parseAdd() {
      let node = this.parseMul();
      while (this.match("+") || this.match("-")) {
        const op = this.previous().value;
        node = binary(op, node, this.parseMul());
      }
      return node;
    }

    parseMul() {
      let node = this.parseUnary();
      while (true) {
        if (this.match("*") || this.match("/")) {
          const op = this.previous().value;
          node = binary(op, node, this.parseUnary());
          continue;
        }
        if (this.startsPrimary(this.peek())) {
          node = binary("*", node, this.parseUnary());
          continue;
        }
        break;
      }
      return node;
    }

    parseUnary() {
      if (this.match("+")) return this.parseUnary();
      if (this.match("-")) return unary("neg", this.parseUnary());
      return this.parsePower();
    }

    parsePower() {
      let node = this.parsePrimary();
      if (this.match("^")) {
        node = binary("^", node, this.parseUnary());
      }
      return node;
    }

    parsePrimary() {
      if (this.matchType("number")) {
        return numberNode(parseNumberLiteral(this.previous().value));
      }

      if (this.matchType("id")) {
        const name = this.previous().value;
        const fn = normalizeFunctionName(name);
        const next = this.peek().value;

        if (fn && (next === "(" || next === "[")) {
          return this.parseCall(fn);
        }

        if (!fn && name.length > 1 && (next === "(" || next === "[")) {
          throw new SyntaxError(`Unknown function "${name}".`);
        }

        const constant = normalizeConstantName(name);
        return constant ? constNode(constant) : symbolNode(name);
      }

      if (this.match("(") || this.match("[")) {
        const close = this.previous().value === "(" ? ")" : "]";
        const node = this.parseExpression();
        this.consume(close, `Expected "${close}".`);
        return node;
      }

      throw new SyntaxError("Expected a number, variable, function, or group.");
    }

    parseCall(name) {
      const open = this.advance().value;
      const close = open === "(" ? ")" : "]";
      const args = [];
      if (!this.check(close)) {
        do {
          args.push(this.parseExpression());
        } while (this.match(","));
      }
      this.consume(close, `Expected "${close}" after function arguments.`);

      if (["+", "*", "-", "/", "^"].includes(name)) {
        const expected = name === "-" ? [1, 2] : [2];
        if (!expected.includes(args.length)) {
          throw new SyntaxError(`Operator function needs ${expected.join(" or ")} argument(s).`);
        }
        return args.length === 1 ? unary("neg", args[0]) : binary(name, args[0], args[1]);
      }

      return callNode(name, args);
    }

    startsPrimary(token) {
      if (token.type === "number") return true;
      if (token.type === "id") return true;
      return token.type === "op" && (token.value === "(" || token.value === "[");
    }

    peek() {
      return this.tokens[this.pos];
    }

    previous() {
      return this.tokens[this.pos - 1];
    }

    advance() {
      if (this.peek().type !== "eof") this.pos += 1;
      return this.previous();
    }

    check(value) {
      return this.peek().value === value;
    }

    match(value) {
      if (this.check(value)) {
        this.advance();
        return true;
      }
      return false;
    }

    matchType(type) {
      if (this.peek().type === type) {
        this.advance();
        return true;
      }
      return false;
    }

    consume(value, message) {
      if (this.match(value)) return this.previous();
      throw new SyntaxError(message);
    }
  }

  function parseFormula(source) {
    return new Parser(tokenize(source)).parse();
  }

  function numberNode(value) {
    return { type: "number", value };
  }

  function symbolNode(name) {
    return { type: "symbol", name };
  }

  function constNode(name) {
    return { type: "const", name };
  }

  function unary(op, arg) {
    return { type: "unary", op, arg };
  }

  function binary(op, left, right) {
    return { type: "binary", op, left, right };
  }

  function callNode(name, args) {
    return { type: "call", name, args };
  }

  function rat(n, d = 1n) {
    if (d === 0n) throw new Error("Division by zero.");
    let num = BigInt(n);
    let den = BigInt(d);
    if (den < 0n) {
      num = -num;
      den = -den;
    }
    const g = gcd(absBig(num), den);
    return { n: num / g, d: den / g };
  }

  function absBig(value) {
    return value < 0n ? -value : value;
  }

  function gcd(a, b) {
    let x = a;
    let y = b;
    while (y !== 0n) {
      const t = x % y;
      x = y;
      y = t;
    }
    return x || 1n;
  }

  function parseNumberLiteral(raw) {
    let source = raw.toLowerCase();
    let sign = 1n;
    if (source.startsWith("+")) source = source.slice(1);
    if (source.startsWith("-")) {
      sign = -1n;
      source = source.slice(1);
    }

    const [body, expPart = "0"] = source.split("e");
    const exponent = Number(expPart);
    const dot = body.indexOf(".");
    const decimals = dot === -1 ? 0 : body.length - dot - 1;
    const digits = body.replace(".", "") || "0";
    let numerator = BigInt(digits) * sign;
    let denominator = 10n ** BigInt(decimals);

    if (exponent > 0) numerator *= 10n ** BigInt(exponent);
    if (exponent < 0) denominator *= 10n ** BigInt(-exponent);
    return rat(numerator, denominator);
  }

  function ratToString(value) {
    return value.d === 1n ? value.n.toString() : `${value.n.toString()}/${value.d.toString()}`;
  }

  function ratAdd(a, b) {
    return rat(a.n * b.d + b.n * a.d, a.d * b.d);
  }

  function ratSub(a, b) {
    return rat(a.n * b.d - b.n * a.d, a.d * b.d);
  }

  function ratMul(a, b) {
    return rat(a.n * b.n, a.d * b.d);
  }

  function ratDiv(a, b) {
    if (b.n === 0n) throw new Error("Division by zero.");
    return rat(a.n * b.d, a.d * b.n);
  }

  function ratNeg(a) {
    return rat(-a.n, a.d);
  }

  function ratPow(a, b) {
    if (b.d !== 1n) return null;
    const exp = b.n;
    if (absBig(exp) > 12n) return null;
    const positive = exp >= 0n ? exp : -exp;
    let num = a.n ** positive;
    let den = a.d ** positive;
    if (exp < 0n) {
      const t = num;
      num = den;
      den = t;
    }
    return rat(num, den);
  }

  function isNumber(node) {
    return node.type === "number";
  }

  function isZero(node) {
    return isNumber(node) && node.value.n === 0n;
  }

  function isOne(node) {
    return isNumber(node) && node.value.n === node.value.d;
  }

  function isMinusOne(node) {
    return isNumber(node) && node.value.n === -node.value.d;
  }

  function sameAst(a, b) {
    return astKey(a) === astKey(b);
  }

  function astKey(node) {
    switch (node.type) {
      case "number":
        return `n:${ratToString(node.value)}`;
      case "symbol":
        return `s:${node.name}`;
      case "const":
        return `c:${node.name}`;
      case "unary":
        return `u:${node.op}(${astKey(node.arg)})`;
      case "binary":
        return `b:${node.op}(${astKey(node.left)},${astKey(node.right)})`;
      case "call":
        return `f:${node.name}(${node.args.map(astKey).join(",")})`;
      default:
        throw new Error(`Unknown AST node ${node.type}.`);
    }
  }

  function simplify(node) {
    switch (node.type) {
      case "number":
      case "symbol":
      case "const":
        return node;
      case "unary": {
        const arg = simplify(node.arg);
        if (node.op === "neg") {
          if (isNumber(arg)) return numberNode(ratNeg(arg.value));
          if (arg.type === "unary" && arg.op === "neg") return arg.arg;
          return unary("neg", arg);
        }
        return unary(node.op, arg);
      }
      case "binary":
        return simplifyBinary(node.op, simplify(node.left), simplify(node.right));
      case "call": {
        const args = node.args.map(simplify);
        if (node.name === "exp" && isZero(args[0])) return numberNode(rat(1n));
        if (node.name === "log" && args.length === 1 && isOne(args[0])) return numberNode(rat(0n));
        if (node.name === "sqrt" && (isZero(args[0]) || isOne(args[0]))) return args[0];
        if (node.name === "sin" && isZero(args[0])) return numberNode(rat(0n));
        if (node.name === "cos" && isZero(args[0])) return numberNode(rat(1n));
        if (node.name === "tan" && isZero(args[0])) return numberNode(rat(0n));
        if (node.name === "sqr") return simplify(binary("^", args[0], numberNode(rat(2n))));
        if (node.name === "half") return simplify(binary("/", args[0], numberNode(rat(2n))));
        if (node.name === "minus") return simplify(unary("neg", args[0]));
        if (node.name === "inv") return simplify(binary("/", numberNode(rat(1n)), args[0]));
        return callNode(node.name, args);
      }
      default:
        throw new Error(`Cannot simplify node ${node.type}.`);
    }
  }

  function simplifyBinary(op, left, right) {
    if (isNumber(left) && isNumber(right)) {
      if (op === "+") return numberNode(ratAdd(left.value, right.value));
      if (op === "-") return numberNode(ratSub(left.value, right.value));
      if (op === "*") return numberNode(ratMul(left.value, right.value));
      if (op === "/" && right.value.n !== 0n) return numberNode(ratDiv(left.value, right.value));
      if (op === "^") {
        const folded = ratPow(left.value, right.value);
        if (folded) return numberNode(folded);
      }
    }

    if (op === "+") {
      if (isZero(left)) return right;
      if (isZero(right)) return left;
    }

    if (op === "-") {
      if (isZero(right)) return left;
      if (isZero(left)) return unary("neg", right);
      if (sameAst(left, right)) return numberNode(rat(0n));
    }

    if (op === "*") {
      if (isZero(left) || isZero(right)) return numberNode(rat(0n));
      if (isOne(left)) return right;
      if (isOne(right)) return left;
      if (isMinusOne(left)) return simplify(unary("neg", right));
      if (isMinusOne(right)) return simplify(unary("neg", left));
    }

    if (op === "/") {
      if (isZero(left)) return numberNode(rat(0n));
      if (isOne(right)) return left;
      if (sameAst(left, right)) return numberNode(rat(1n));
    }

    if (op === "^") {
      if (isZero(right)) return numberNode(rat(1n));
      if (isOne(right)) return left;
      if (isOne(left)) return numberNode(rat(1n));
    }

    return binary(op, left, right);
  }

  function printAst(node, parentPrecedence = 0, side = "") {
    const own = astPrecedence(node);
    let text;

    switch (node.type) {
      case "number":
        text = ratToString(node.value);
        break;
      case "symbol":
        text = node.name;
        break;
      case "const":
        text = node.name === "goldenratio" ? "GoldenRatio" : node.name;
        break;
      case "unary":
        text = `-${printAst(node.arg, own, "right")}`;
        break;
      case "binary":
        text = `${printAst(node.left, own, "left")} ${node.op} ${printAst(node.right, own, "right")}`;
        break;
      case "call":
        text = `${displayFunctionName(node.name)}[${node.args.map((arg) => printAst(arg)).join(", ")}]`;
        break;
      default:
        text = "";
    }

    const needsParens =
      own < parentPrecedence ||
      (side === "right" && node.type === "binary" && ["-", "/", "^"].includes(node.op) && own === parentPrecedence);
    return needsParens ? `(${text})` : text;
  }

  function astPrecedence(node) {
    if (node.type === "binary") {
      if (node.op === "+" || node.op === "-") return 1;
      if (node.op === "*" || node.op === "/") return 2;
      if (node.op === "^") return 3;
    }
    if (node.type === "unary") return 4;
    return 5;
  }

  function displayFunctionName(name) {
    const names = {
      exp: "Exp",
      log: "Log",
      sin: "Sin",
      cos: "Cos",
      tan: "Tan",
      cot: "Cot",
      sec: "Sec",
      csc: "Csc",
      asin: "ArcSin",
      acos: "ArcCos",
      atan: "ArcTan",
      asinh: "ArcSinh",
      acosh: "ArcCosh",
      atanh: "ArcTanh",
      asec: "ArcSec",
      acsc: "ArcCsc",
      acot: "ArcCot",
      sqrt: "Sqrt",
      abs: "Abs",
      avg: "Avg",
      hypot: "Hypot",
      logistic: "LogisticSigmoid",
      eml: "EML",
    };
    return names[name] || name;
  }

  function leaf(value) {
    return { type: "leaf", value };
  }

  function E(left, right) {
    return { type: "eml", left, right };
  }

  const ONE = leaf("1");

  function emlExp(z) {
    return E(z, ONE);
  }

  function emlLog(z) {
    return E(ONE, emlExp(E(ONE, z)));
  }

  function emlZero() {
    return emlLog(ONE);
  }

  function emlSub(a, b) {
    return E(emlLog(a), emlExp(b));
  }

  function emlNeg(z) {
    return emlSub(emlZero(), z);
  }

  function emlAdd(a, b) {
    return emlSub(a, emlNeg(b));
  }

  function emlInv(z) {
    return emlExp(emlNeg(emlLog(z)));
  }

  function emlMul(a, b) {
    return emlExp(emlAdd(emlLog(a), emlLog(b)));
  }

  function emlDiv(a, b) {
    return emlMul(a, emlInv(b));
  }

  function emlPow(a, b) {
    return emlExp(emlMul(b, emlLog(a)));
  }

  function emlInt(value) {
    if (value === 0n) return emlZero();
    if (value === 1n) return ONE;
    if (value < 0n) return emlNeg(emlInt(-value));

    let acc = null;
    let term = ONE;
    let k = value;
    while (k > 0n) {
      if ((k & 1n) === 1n) {
        acc = acc ? emlAdd(acc, term) : term;
      }
      term = emlAdd(term, term);
      k >>= 1n;
    }
    return acc || emlZero();
  }

  function emlRational(value) {
    if (value.d === 1n) return emlInt(value.n);
    const absNum = value.n < 0n ? -value.n : value.n;
    const positive = emlMul(emlInt(absNum), emlInv(emlInt(value.d)));
    return value.n < 0n ? emlNeg(positive) : positive;
  }

  function emlConstE() {
    return emlExp(ONE);
  }

  function emlConstI() {
    const minusOne = emlNeg(ONE);
    const two = emlInt(2n);
    return emlNeg(emlExp(emlDiv(emlLog(minusOne), two)));
  }

  function emlConstPi() {
    const i = emlConstI();
    return emlMul(i, emlLog(emlNeg(ONE)));
  }

  function emlConstGoldenRatio() {
    const sqrtFive = emlPow(emlInt(5n), emlRational(rat(1n, 2n)));
    return emlDiv(emlAdd(ONE, sqrtFive), emlInt(2n));
  }

  function compileToEml(node) {
    switch (node.type) {
      case "number":
        return emlRational(node.value);
      case "symbol":
        return leaf(node.name);
      case "const":
        if (node.name === "e") return emlConstE();
        if (node.name === "i") return emlConstI();
        if (node.name === "pi") return emlConstPi();
        if (node.name === "goldenratio") return emlConstGoldenRatio();
        throw new Error(`Unknown constant ${node.name}.`);
      case "unary":
        if (node.op === "neg") return emlNeg(compileToEml(node.arg));
        throw new Error(`Unknown unary operator ${node.op}.`);
      case "binary": {
        const left = compileToEml(node.left);
        const right = compileToEml(node.right);
        if (node.op === "+") return emlAdd(left, right);
        if (node.op === "-") return emlSub(left, right);
        if (node.op === "*") return emlMul(left, right);
        if (node.op === "/") return emlDiv(left, right);
        if (node.op === "^") return emlPow(left, right);
        throw new Error(`Unknown binary operator ${node.op}.`);
      }
      case "call":
        return compileCall(node);
      default:
        throw new Error(`Cannot compile node ${node.type}.`);
    }
  }

  function compileCall(node) {
    const args = node.args.map(compileToEml);
    const arity = args.length;
    const z = args[0];
    const i = () => emlConstI();
    const two = () => emlInt(2n);
    const half = () => emlRational(rat(1n, 2n));

    function need(count) {
      if (arity !== count) throw new SyntaxError(`${displayFunctionName(node.name)} needs ${count} argument(s).`);
    }

    switch (node.name) {
      case "eml":
        need(2);
        return E(args[0], args[1]);
      case "exp":
        need(1);
        return emlExp(z);
      case "log":
        if (arity === 1) return emlLog(z);
        if (arity === 2) return emlDiv(emlLog(args[1]), emlLog(args[0]));
        throw new SyntaxError("Log needs 1 argument, or base and value.");
      case "sqrt":
        need(1);
        return emlPow(z, half());
      case "sqr":
        need(1);
        return emlPow(z, two());
      case "half":
        need(1);
        return emlDiv(z, two());
      case "minus":
        need(1);
        return emlNeg(z);
      case "inv":
        need(1);
        return emlInv(z);
      case "abs":
        need(1);
        return emlPow(emlPow(z, two()), half());
      case "sin": {
        need(1);
        const iz = emlMul(i(), z);
        return emlDiv(emlSub(emlExp(iz), emlExp(emlNeg(iz))), emlMul(two(), i()));
      }
      case "cos": {
        need(1);
        const iz = emlMul(i(), z);
        return emlDiv(emlAdd(emlExp(iz), emlExp(emlNeg(iz))), two());
      }
      case "tan":
        need(1);
        return emlDiv(compileCall(callNode("sin", [node.args[0]])), compileCall(callNode("cos", [node.args[0]])));
      case "cot":
        need(1);
        return emlDiv(compileCall(callNode("cos", [node.args[0]])), compileCall(callNode("sin", [node.args[0]])));
      case "sec":
        need(1);
        return emlInv(compileCall(callNode("cos", [node.args[0]])));
      case "csc":
        need(1);
        return emlInv(compileCall(callNode("sin", [node.args[0]])));
      default:
        return compileCallExtended(node, args, arity, z);
    }
  }

  function compileCallExtended(node, args, arity, z) {
    const i = () => emlConstI();
    const two = () => emlInt(2n);
    const half = () => emlRational(rat(1n, 2n));

    function need(count) {
      if (arity !== count) throw new SyntaxError(`${displayFunctionName(node.name)} needs ${count} argument(s).`);
    }

    switch (node.name) {
      case "sinh":
        need(1);
        return emlDiv(emlSub(emlExp(z), emlExp(emlNeg(z))), two());
      case "cosh":
        need(1);
        return emlDiv(emlAdd(emlExp(z), emlExp(emlNeg(z))), two());
      case "tanh":
        need(1);
        return emlDiv(compileCall(callNode("sinh", [node.args[0]])), compileCall(callNode("cosh", [node.args[0]])));
      case "asin": {
        need(1);
        const inner = emlAdd(emlMul(emlNeg(i()), z), emlPow(emlSub(ONE, emlPow(z, two())), half()));
        return emlMul(i(), emlLog(inner));
      }
      case "acos": {
        need(1);
        const inner = emlAdd(z, emlMul(emlPow(emlSub(z, ONE), half()), emlPow(emlAdd(z, ONE), half())));
        return emlMul(i(), emlLog(inner));
      }
      case "atan": {
        need(1);
        const numerator = emlAdd(emlNeg(i()), z);
        const denominator = emlSub(emlNeg(i()), z);
        return emlMul(emlNeg(emlDiv(i(), two())), emlLog(emlDiv(numerator, denominator)));
      }
      case "asinh":
        need(1);
        return emlLog(emlAdd(z, emlPow(emlAdd(emlPow(z, two()), ONE), half())));
      case "acosh":
        need(1);
        return emlLog(emlAdd(z, emlMul(emlPow(emlAdd(z, ONE), half()), emlPow(emlSub(z, ONE), half()))));
      case "atanh":
        need(1);
        return emlMul(half(), emlLog(emlDiv(emlAdd(ONE, z), emlSub(ONE, z))));
      case "asec":
        need(1);
        return compileCall(callNode("acos", [binary("/", numberNode(rat(1n)), node.args[0])]));
      case "acsc":
        need(1);
        return compileCall(callNode("asin", [binary("/", numberNode(rat(1n)), node.args[0])]));
      case "acot":
        need(1);
        return compileCall(callNode("atan", [binary("/", numberNode(rat(1n)), node.args[0])]));
      case "avg":
        need(2);
        return emlDiv(emlAdd(args[0], args[1]), two());
      case "hypot":
        need(2);
        return emlPow(emlAdd(emlPow(args[0], two()), emlPow(args[1], two())), half());
      case "logistic":
        need(1);
        return emlMul(half(), emlAdd(ONE, compileCall(callNode("tanh", [binary("/", node.args[0], numberNode(rat(2n)))]))));
      default:
        throw new Error(`Unsupported function ${node.name}.`);
    }
  }

  function emlKey(node) {
    return node.type === "leaf" ? `L:${node.value}` : `E:${emlKey(node.left)}:${emlKey(node.right)}`;
  }

  function emlSize(node) {
    return node.type === "leaf" ? 1 : 1 + emlSize(node.left) + emlSize(node.right);
  }

  function emlMetrics(node) {
    if (node.type === "leaf") {
      return { nodes: 0, leaves: 1, depth: 0, chars: node.value.length };
    }
    const left = emlMetrics(node.left);
    const right = emlMetrics(node.right);
    return {
      nodes: 1 + left.nodes + right.nodes,
      leaves: left.leaves + right.leaves,
      depth: 1 + Math.max(left.depth, right.depth),
      chars: 0,
    };
  }

  function formatEml(node, alias = false) {
    if (node.type === "leaf") return node.value;
    const op = alias ? "E" : "EML";
    return `${op}[${formatEml(node.left, alias)},${formatEml(node.right, alias)}]`;
  }

  function formatShortEml(root, alias = false) {
    const counts = new Map();
    const nodes = new Map();
    walk(root);

    function walk(node) {
      const key = emlKey(node);
      counts.set(key, (counts.get(key) || 0) + 1);
      nodes.set(key, node);
      if (node.type === "eml") {
        walk(node.left);
        walk(node.right);
      }
    }

    const candidates = new Set(
      Array.from(counts.entries())
        .filter(([key, count]) => count > 1 && emlSize(nodes.get(key)) >= 5)
        .map(([key]) => key)
    );

    const names = new Map();
    const bindings = [];
    const op = alias ? "E" : "EML";

    function emit(node, isRoot = false) {
      const key = emlKey(node);
      if (!isRoot && candidates.has(key)) {
        if (!names.has(key)) {
          const expr = emitRaw(node);
          const name = `t${bindings.length + 1}`;
          names.set(key, name);
          bindings.push({ name, expr });
        }
        return names.get(key);
      }
      return emitRaw(node);
    }

    function emitRaw(node) {
      if (node.type === "leaf") return node.value;
      return `${op}[${emit(node.left)},${emit(node.right)}]`;
    }

    const main = emit(root, true);
    if (bindings.length === 0) return main;
    return `${bindings.map((item) => `let ${item.name} = ${item.expr};`).join("\n")}\n${main}`;
  }

  function toRpn(node) {
    if (node.type === "leaf") return [node.value];
    return [...toRpn(node.left), ...toRpn(node.right), "E"];
  }

  function compileExpression(source, options = {}) {
    const parsed = parseFormula(source);
    const normalized = options.optimize === false ? parsed : simplify(parsed);
    const eml = compileToEml(normalized);
    const metrics = emlMetrics(eml);
    const pure = formatEml(eml, Boolean(options.alias));
    metrics.chars = pure.length;
    return {
      parsed,
      normalized,
      normalizedText: printAst(normalized),
      eml,
      metrics,
      pure,
      short: formatShortEml(eml, Boolean(options.alias)),
      rpn: toRpn(eml).join(" "),
    };
  }

  function initBrowser() {
    const form = document.getElementById("compiler-form");
    const input = document.getElementById("formula-input");
    const optimize = document.getElementById("optimize-input");
    const alias = document.getElementById("alias-input");
    const status = document.getElementById("status-line");
    const nodes = document.getElementById("metric-nodes");
    const depth = document.getElementById("metric-depth");
    const leaves = document.getElementById("metric-leaves");
    const normalizedOutput = document.getElementById("normalized-output");
    const shortOutput = document.getElementById("short-output");
    const pureOutput = document.getElementById("pure-output");
    const rpnOutput = document.getElementById("rpn-output");
    const treePreview = document.getElementById("tree-preview");

    function run() {
      try {
        const result = compileExpression(input.value, {
          optimize: optimize.checked,
          alias: alias.checked,
        });
        normalizedOutput.textContent = result.normalizedText;
        shortOutput.textContent = result.short;
        pureOutput.textContent = result.pure;
        rpnOutput.textContent = result.rpn;
        nodes.textContent = result.metrics.nodes.toString();
        depth.textContent = result.metrics.depth.toString();
        leaves.textContent = result.metrics.leaves.toString();
        setStatus(`Compiled ${result.metrics.nodes} EML node(s).`, false);
        renderTree(result.eml, treePreview);
      } catch (error) {
        setStatus(error.message, true);
      }
    }

    function setStatus(message, isError) {
      status.textContent = message;
      status.classList.toggle("error", isError);
      status.classList.remove("flash");
      window.requestAnimationFrame(() => {
        status.classList.add("flash");
      });
    }

    form.addEventListener("submit", (event) => {
      event.preventDefault();
      run();
    });
    optimize.addEventListener("change", run);
    alias.addEventListener("change", run);
    input.addEventListener("input", debounce(run, 240));

    document.getElementById("examples").addEventListener("click", (event) => {
      const button = event.target.closest("[data-example]");
      if (!button) return;
      input.value = button.dataset.example;
      run();
    });

    document.querySelectorAll("[data-copy]").forEach((button) => {
      button.addEventListener("click", async () => {
        const target = document.getElementById(button.dataset.copy);
        await copyText(target.textContent);
        button.textContent = "Copied";
        window.setTimeout(() => {
          button.textContent = "Copy";
        }, 900);
      });
    });

    run();
  }

  function debounce(fn, delay) {
    let timer = 0;
    return function (...args) {
      clearTimeout(timer);
      timer = setTimeout(() => fn.apply(this, args), delay);
    };
  }

  async function copyText(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      await navigator.clipboard.writeText(text);
      return;
    }
    const area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.style.position = "fixed";
    area.style.left = "-9999px";
    document.body.append(area);
    area.select();
    document.execCommand("copy");
    area.remove();
  }

  function renderTree(root, container) {
    container.replaceChildren(buildTreeNode(root, 0, 4));
  }

  function buildTreeNode(node, depth, maxDepth) {
    const wrap = document.createElement("div");
    wrap.className = "tree-node";

    const token = document.createElement("span");
    token.className = node.type === "eml" ? "tree-token operator" : "tree-token";
    token.textContent = node.type === "eml" ? "EML" : node.value;
    wrap.append(token);

    if (node.type === "eml") {
      if (depth >= maxDepth) {
        const clipped = document.createElement("span");
        clipped.className = "tree-clipped";
        clipped.textContent = "clipped";
        wrap.append(clipped);
      } else {
        const children = document.createElement("div");
        children.className = "tree-children";
        children.append(buildTreeNode(node.left, depth + 1, maxDepth));
        children.append(buildTreeNode(node.right, depth + 1, maxDepth));
        wrap.append(children);
      }
    }

    if (depth === 0) {
      const rootWrap = document.createElement("div");
      rootWrap.className = "tree";
      rootWrap.append(wrap);
      return rootWrap;
    }

    return wrap;
  }

  const api = {
    tokenize,
    Parser,
    parseFormula,
    simplify,
    printAst,
    compileToEml,
    compileExpression,
    formatEml,
    formatShortEml,
    toRpn,
    emlMetrics,
  };

  if (typeof module !== "undefined" && module.exports) {
    module.exports = api;
  }

  if (typeof window !== "undefined") {
    window.EMLRemapper = api;
    window.addEventListener("DOMContentLoaded", initBrowser);
  }

})();
