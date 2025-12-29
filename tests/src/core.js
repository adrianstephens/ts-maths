"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isInstance = isInstance;
exports.hasStatic = hasStatic;
exports.arrayOf = arrayOf;
exports.Type = Type;
exports.hasop = hasop;
exports.hasopT = hasopT;
exports.has0 = has0;
exports.has = has;
exports.hasFree = hasFree;
exports.isScalar = isScalar;
exports.isScalarExt = isScalarExt;
exports.asScalarT = asScalarT;
exports.compare = compare;
exports.sign = sign;
exports.lazySlice = lazySlice;
function isInstance(x, i) {
    return x instanceof i || x.prototype === i.prototype;
}
function hasStatic(x, f) {
    const c = x.constructor;
    if (f in c)
        return c[f];
    return undefined;
}
function arrayOf(arr, g) {
    for (const i in arr)
        return g(arr[i]);
    return false;
}
function Type(operators) {
    return class Ops {
        value;
        constructor(value) {
            this.value = value;
        }
        from(n) { return new Ops(operators.from(n)); }
        dup() { return new Ops(operators.dup(this.value)); }
        neg() { return new Ops(operators.neg(this.value)); }
        scale(b) { return new Ops(operators.scale(this.value, b)); }
        add(b) { return new Ops(operators.add(this.value, b.value)); }
        sub(b) { return new Ops(operators.sub(this.value, b.value)); }
        mul(b) { return new Ops(operators.mul(this.value, b.value)); }
        div(b) { return new Ops(operators.div(this.value, b.value)); }
        mag() { return typeof this.value === 'number' ? Math.abs(this.value) : 0; }
        // scalar
        abs() { if (hasFree('abs')(operators))
            return new Ops(operators.abs(this.value)); return this; }
        ;
        sign() { const z = operators.from(0); return operators.eq(this.value, z) ? 0 : operators.lt(this.value, z) ? -1 : 1; }
        eq(b) { return operators.eq(this.value, b.value); }
        lt(b) { return operators.lt(this.value, b.value); }
        //power
        sqrt() { return new Ops(operators.rpow(this.value, 1, 2)); }
        recip() { return new Ops(operators.ipow(this.value, -1)); }
        ipow(n) { return new Ops(operators.ipow(this.value, n)); }
        rpow(n, d) { return new Ops(operators.rpow(this.value, n, d)); }
        npow(n) { return new Ops(operators.pow(this.value, operators.from(n))); }
        //ext
        //		divmod(b: Ops):		number | bigint;
        toString() { return String(this.value); }
        valueOf() { return Number(this.value); }
    };
}
function hasop(f) {
    return (x) => f in x;
}
function hasopT(f) {
    return (x) => f in x;
}
;
;
;
;
function has0(f) {
    return (x) => f in x;
}
function has(f) {
    return (x) => f in x;
}
function hasFree(f) {
    return (x) => f in x;
}
function isScalar(x) {
    return 'lt' in x;
}
function isScalarExt(x) {
    return 'rpow' in x && 'divmod' in x;
}
function asScalarT(from, x) {
    if (typeof x === 'number')
        return from.from(x);
    if (x instanceof from.constructor)
        return x;
    return from.from(+x);
}
function compare(a, b) {
    return a < b ? -1 : a > b ? 1 : 0;
}
function sign(a) {
    return a < 0 ? -1 : a > 0 ? 1 : 0;
}
function* lazySlice(arr, start, end) {
    const len = arr.length;
    start = start === undefined ? 0
        : start < 0 ? Math.max(len + start, 0)
            : Math.min(start, len);
    end = end === undefined ? len
        : end < 0 ? Math.max(len + start, 0)
            : Math.min(end, len);
    for (let i = start; i < end; i++)
        yield arr[i];
}
//# sourceMappingURL=core.js.map