"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.big = void 0;
exports.isBigInt = isBigInt;
const real_1 = require("./real");
//-----------------------------------------------------------------------------
// bigint
//-----------------------------------------------------------------------------
function isBigInt(x) {
    return typeof x === 'bigint';
}
class _Big {
    value;
    constructor(value) {
        this.value = value;
    }
    dup() { return (0, exports.big)(this.value); }
    neg() { return (0, exports.big)(-this.value); }
    scale(b) { return (0, exports.big)(this.value * BigInt(b)); }
    add(b) { return (0, exports.big)(this.value + b.value); }
    sub(b) { return (0, exports.big)(this.value - b.value); }
    mul(b) { return (0, exports.big)(this.value * b.value); }
    div(b) { return (0, exports.big)(this.value / b.value); }
    mag() { return Number(exports.big.abs(this.value)); }
    from(n) { return (0, exports.big)(BigInt(n)); }
    ipow(n) { return (0, exports.big)(this.value ** BigInt(n)); }
    rpow(n, d) { return (0, exports.big)(exports.big.rpow(this.value, n, d)); }
    npow(n) { return (0, exports.big)(exports.big.npow(this.value, n)); }
    sqrt() { return (0, exports.big)(exports.big.root(this.value, 2)); }
    abs() { return (0, exports.big)(exports.big.abs(this.value)); }
    sign() { return exports.big.sign(this.value); }
    recip() { return (0, exports.big)(1n / this.value); }
    divmod(b) { const q = this.value / b.value; this.value -= q * b.value; return Number(q); }
    lt(b) { return this.value < b.value; }
    eq(b) { return this.value === b.value; }
    valueOf() { return Number(this.value); }
    toString() { return this.value.toString(); }
}
class extentB {
    min;
    max;
    static fromCentreExtent(centre, size) {
        const half = size / 2n;
        return new extentB(centre - half, centre + half);
    }
    static from(items) {
        let ext; // = new extentT<T>;
        for (const i of items) {
            if (!ext)
                ext = new extentB(i, i);
            else
                ext.add(i);
        }
        return ext;
    }
    constructor(min, max) {
        this.min = min;
        this.max = max;
    }
    extent() {
        return this.max - this.min;
    }
    centre() {
        return (this.min + this.max) / 2n;
    }
    add(p) {
        this.min = exports.big.min(this.min, p);
        this.max = exports.big.max(this.max, p);
    }
    combine(b) {
        this.min = exports.big.min(this.min, b.min);
        this.max = exports.big.max(this.max, b.max);
    }
    encompasses(b) {
        return this.min <= b.min && this.max >= b.max;
    }
    overlaps(b) {
        return this.min <= b.max && this.max >= b.min;
    }
    contains(p) {
        return this.min <= p && this.max >= p;
    }
    clamp(p) {
        return exports.big.min(exports.big.max(p, this.min), this.max);
    }
}
exports.big = Object.assign(function (value) {
    return new _Big(value);
}, {
    func(_name, _args) { return undefined; },
    variable(_name) { return undefined; },
    from(n) { return BigInt(n); },
    dup(a) { return a; },
    neg(a) { return -a; },
    scale(a, b) { return a * BigInt(b); },
    add(a, b) { return a + b; },
    sub(a, b) { return a - b; },
    mul(a, b) { return a * b; },
    div(a, b) { return a / b; },
    ipow(a, b) { return a ** BigInt(b); },
    rpow(x, n, d) { return exports.big.root(x ** BigInt(n), d); },
    pow(a, b) { return a ** b; },
    eq(a, b) { return a === b; },
    lt(a, b) { return a < b; },
}, {
    is(x) { return typeof x === 'bigint'; },
    shift(x, n) { return n > 0 ? x << n : x >> -n; },
    sign(a) { return a === 0n ? 0 : a < 0n ? -1 : 1; },
    abs(a) { return a < 0n ? -a : a; },
    max(...values) { return values.reduce((a, b) => a < b ? b : a); },
    min(...values) { return values.reduce((a, b) => a < b ? a : b); },
    gcd(...values) {
        let a = 0n;
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
        let s0 = 1n, s = 0n, t0 = 0n, t = 1n;
        while (b) {
            const q = a / b;
            [a, b, s0, t0, s, t] = [b, a - b * q, s, t, s0 - s * q, t0 - t * q];
        }
        // a = gcd, and s0 * a + t0 * b = r0
        return { g: a, x: s0, y: t0 };
    },
    lcm(...x) {
        return x.reduce((a, b) => (a / exports.big.gcd(a, b)) * b, 1n);
    },
    divToReal(a, b) {
        if (b === 0n)
            return Infinity;
        if (a === 0n)
            return 0;
        const aa = exports.big.abs(a);
        const bb = exports.big.abs(b);
        // If both fit in safe integer range, do the fast exact conversion
        const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
        if (aa <= MAX_SAFE && bb <= MAX_SAFE)
            return Number(a) / Number(b);
        const shiftA = Math.max(0, highestSetB(aa) - 52);
        const shiftB = Math.max(0, highestSetB(bb) - 52);
        const result = Number(aa >> BigInt(shiftA)) / Number(bb >> BigInt(shiftB)) * Math.pow(2, shiftA - shiftB);
        return (a < 0n) !== (b < 0n) ? -result : result;
    },
    modPow(base, exp, mod) {
        let result = 1n;
        while (exp) {
            base %= mod;
            if (exp & 1n)
                result = (result * base) % mod;
            base *= base;
            exp >>= 1n;
        }
        return result;
    },
    npow(x, n) {
        const d = real_1.real.denominator(n, 100);
        if (d === 0)
            throw new Error('Big.npow: exponent not representable as rational');
        return exports.big.root(x ** BigInt(n * d), d);
    },
    random(bits) {
        let seed = (((Date.now() & 0xffffffff) ^ 0x9e3779b9) >>> 0) || 1;
        const rng32 = () => {
            let x = seed >>> 0;
            x ^= (x << 13) >>> 0;
            x ^= x >>> 17;
            x ^= (x << 5) >>> 0;
            return seed = x >>> 0;
        };
        let r = 0n;
        for (let i = bits >> 5; i--;)
            r = (r << 32n) | BigInt(rng32());
        const rem = bits & 31;
        if (rem)
            r = (r << BigInt(rem)) | BigInt(rng32() & ((1 << rem) - 1));
        return r;
    },
    root(x, b) {
        if (b === 1)
            return x;
        if (x === 0n || b < 1)
            return 0n;
        if ((b & 1) === 0 && x < 0n)
            throw new Error('Big.root: negative numbers not allowed with even denominator');
        const resultBits = Math.floor(highestSetB(x) / b);
        let k = 16;
        let y = BigInt(Math.floor(Math.pow(Number(x >> BigInt((resultBits - k) * b)), 1 / b)));
        const b1 = BigInt(b - 1);
        const bb = BigInt(b);
        while (k * 2 < resultBits) {
            const _xk = x >> BigInt((resultBits - k) * b - k);
            y = (((b1 * y) << BigInt(k)) + _xk / (y ** b1)) / bb;
            k <<= 1;
        }
        // Final refinement at full precision
        y = y << BigInt(resultBits - k);
        y = (b1 * y + x / (y ** b1)) / bb;
        while ((y ** bb) > x)
            --y;
        return x < 0n ? -y : y;
    },
    log2(n, bits = 32n) {
        if (n <= 0n)
            throw new Error("n must be positive");
        let log = BigInt(highestSetB(n));
        let x = this.shift(n, bits - log); // normalize to fixed-point [1, 2)
        for (let i = 0; i < bits; i++) {
            log <<= 1n;
            x = (x * x) >> bits;
            if (x >= (2n << BigInt(bits))) {
                ++log;
                x >>= 1n;
            }
        }
        return log;
    },
    extent: extentB
});
exports.big.prototype = _Big.prototype;
exports.default = exports.big;
function highestSetB(x) {
    let s = 0;
    let k = 0;
    for (let t = x >> 1024n; t; t >>= BigInt(s)) {
        s = 1024 << k++;
        x = t;
    }
    if (k) {
        while (--k) {
            const b = x >> BigInt(512 << k);
            if (b) {
                s += 512 << k;
                x = b;
            }
        }
    }
    const b = Math.floor(Math.log2(Number(x)));
    return 1n << BigInt(b) <= x ? s + b : s + b - 1;
}
//# sourceMappingURL=big.js.map