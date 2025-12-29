"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/no-unused-vars */
const test_1 = require("./test");
const complex_1 = __importDefault(require("../dist/complex"));
const string_1 = require("../dist/string");
(0, test_1.test)('complex arithmetic', () => {
    const a = (0, complex_1.default)(3, 4);
    const b = (0, complex_1.default)(2, 3);
    (0, test_1.expect)(a.add(b)).toEqual((0, complex_1.default)(5, 7));
    (0, test_1.expect)(a.sub(b)).toEqual((0, complex_1.default)(1, 1));
    (0, test_1.expect)(a.mul(b)).toEqual((0, complex_1.default)(-6, 17));
    (0, test_1.expect)((0, complex_1.default)(-6, 17).div(b)).toEqual(a);
    //	expect(a.mod(b)).toEqual(complex(3, 4));
    const eqn = (0, string_1.parse)(complex_1.default, '3 + 4i');
    (0, test_1.expect)(eqn).toEqual((0, complex_1.default)(3, 4));
});
(0, test_1.test)('complex intrinsic functions', () => {
    const a = (0, complex_1.default)(1, 2);
    // exp
    (0, test_1.expect)(complex_1.default.exp(a)).toBeCloseTo((0, complex_1.default)(-1.1312043837568135, 2.4717266720048188), 1e-12);
    // ln
    (0, test_1.expect)(complex_1.default.ln(a)).toBeCloseTo((0, complex_1.default)(0.8047189562170501, 1.1071487177940904), 1e-12);
    // sqrt
    (0, test_1.expect)(complex_1.default.sqrt(a)).toBeCloseTo((0, complex_1.default)(1.272019649514069, 0.7861513777574233), 1e-12);
    // sin
    (0, test_1.expect)(complex_1.default.sin(a)).toBeCloseTo((0, complex_1.default)(3.165778513216168, 1.9596010414216063), 1e-12);
    // cos
    (0, test_1.expect)(complex_1.default.cos(a)).toBeCloseTo((0, complex_1.default)(2.0327230070196656, -3.0518977991518), 1e-12);
    // tan
    (0, test_1.expect)(complex_1.default.tan(a)).toBeCloseTo((0, complex_1.default)(0.0338128260798967, 1.0147936161466335), 1e-12);
    // sinh
    (0, test_1.expect)(complex_1.default.sinh(a)).toBeCloseTo((0, complex_1.default)(-0.4890562590412937, 1.4031192506220405), 1e-12);
    // cosh
    (0, test_1.expect)(complex_1.default.cosh(a)).toBeCloseTo((0, complex_1.default)(-0.6421481247155201, 1.0686074213827783), 1e-12);
    // tanh
    (0, test_1.expect)(complex_1.default.tanh(a)).toBeCloseTo((0, complex_1.default)(1.16673625724092, -0.243458201185725), 1e-10);
});
//# sourceMappingURL=test-complex.js.map