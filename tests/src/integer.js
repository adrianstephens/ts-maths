"use strict";
//-----------------------------------------------------------------------------
// integer
//-----------------------------------------------------------------------------
Object.defineProperty(exports, "__esModule", { value: true });
exports.integer = void 0;
exports.integer = {
    from(n) {
        if (!Number.isInteger(n))
            throw new Error(`Not an integer: ${n}`);
        return n;
    },
    is(n) {
        return Number.isInteger(n);
    },
    almost(x, epsilon = Number.EPSILON) {
        return Math.abs(x - Math.round(x)) < epsilon;
    },
    shift(x, n) {
        return (n > 0 ? x << n : x >> -n);
    },
    add(a, b) {
        return a + b;
    },
    sub(a, b) {
        return a - b;
    },
    mul(a, b) {
        return a * b;
    },
    div(a, b) {
        return Math.trunc(a / b);
    },
    neg(n) {
        return -n;
    },
    abs(n) {
        return Math.abs(n);
    },
    sign(n) {
        return Math.sign(n);
    },
    ipow(x, n) {
        return x ** n;
    },
    rpow(x, n, d) {
        return this.root(this.ipow(x, n), d);
    },
    sqrt(n) {
        if (n < 0)
            throw new Error("Negative number");
        let x = n;
        for (let y = (x + 1) >> 1; y < x;)
            [x, y] = [y, (x + n / x) >> 1];
        return x;
    },
    root(n, r) {
        if (n < 0 && (r & 1) === 0)
            throw new Error('integer.root: negative numbers not allowed with even denominator');
        let x = n;
        const r1 = r - 1;
        for (let y = (r1 * x + n / Math.pow(x, r1)) / r; y < x;)
            [x, y] = [y, (r1 * x + n / Math.pow(x, r1)) / r];
        return Math.round(x);
    },
    modPow(ibase, iexp, mod) {
        let result = 1;
        for (let exp = iexp, base = ibase; exp; exp >>= 1) {
            base %= mod;
            if (exp & 1)
                result = (result * base) % mod;
            base *= base;
        }
        return result;
    },
    gcd(...values) {
        let a = 0;
        for (let b of values) {
            if (a) {
                while (b)
                    [a, b] = [b, a % b];
            }
            else {
                a = b;
            }
        }
        return a;
    },
    extendedGcd(a, b) {
        let s0 = 1, s = 0, t0 = 0, t = 1;
        while (b) {
            const q = this.div(a, b);
            [a, b, s0, t0, s, t] = [b, a - b * q, s, t, s0 - s * q, t0 - t * q];
        }
        // a = gcd, and s0 * a + t0 * b = r0
        return { g: a, x: s0, y: t0 };
    },
    lcm(...x) {
        return x.reduce((a, b) => (a / exports.integer.gcd(a, b)) * b, 1);
    },
};
exports.default = exports.integer;
//# sourceMappingURL=integer.js.map